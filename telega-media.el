;;; telega-media.el --- Media support for telega  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Jul 10 15:20:09 2018
;; Keywords:

;; telega is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; telega is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with telega.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Code to work with media in telega:
;;  - Download/upload files to cloud
;;  - Thumbnails
;;  - Stickers
;;  - Animations
;;  - Web pages
;;  etc

;;; Code:
(require 'telega-core)
(require 'telega-tdlib)

(declare-function telega-root-view--update "telega-root" (on-update-prop &rest args))
(declare-function telega-chat-title "telega-chat" (chat &optional fmt-type no-badges))

(declare-function telega-msg-redisplay "telega-msg" (msg))

(declare-function telega-image-view-file "telega-modes" (tl-file &optional for-msg))


;;; Files downloading/uploading
(defun telega-file--ensure (file)
  "Ensure FILE is in `telega--files'.
Return FILE.
As side-effect might update root view, if current root view is \"Files\"."
  (when telega-debug
    (cl-assert file))
  (plist-put file :telega-file-recency (telega-time-seconds))
  (puthash (plist-get file :id) file telega--files)

  (telega-root-view--update :on-file-update file)
  file)

(defun telega-file-get (file-id &optional locally)
  "Return file associated with FILE-ID."
  (or (gethash file-id telega--files)
      (unless locally
        (telega-file--ensure (telega--getFile file-id)))))

(defun telega-file--renew (place prop)
  "Renew file value at PLACE and PROP."
  (when-let* ((ppfile (plist-get place prop))
              (file-id (plist-get ppfile :id))
              (file (or (gethash file-id telega--files)
                        (telega-file--ensure ppfile))))
    (plist-put place prop file)
    file))

(defun telega-file--add-update-callback (file-id update-callback)
  "Ensure FILE-ID is monitored with UPDATE-CALLBACK."
  (declare (indent 1))
  (cl-assert update-callback)
  (let ((cb-list (gethash file-id telega--files-updates)))
    (unless (memq update-callback cb-list)
      (puthash file-id (cons update-callback cb-list)
               telega--files-updates))))

(defun telega-file--del-update-callback (file-id update-callback)
  "Delete UPDATE-CALLBACK from callbacks for FILE-ID callbacks."
  (declare (indent 1))
  (cl-assert update-callback)
  (let ((left-cbs (delq update-callback
                        (gethash file-id telega--files-updates))))
    (if left-cbs
        (puthash file-id left-cbs telega--files-updates)
      (remhash file-id telega--files-updates))
    (telega-debug "%s %d: del CB, left callbacks %d"
                  (propertize "FILE-UPDATE" 'face 'bold)
                  file-id (length left-cbs))))

(defun telega-file--update (file &optional omit-throttle-p)
  "FILE has been updated, call any pending callbacks."
  (let* ((file-id (plist-get file :id))
         (old-file (gethash file-id telega--files))
         (throttle-p
          ;; NOTE: Throttle number of update callbacks calls
          ;; Throttle only if `:downloaded_size'/`:uploaded_size'
          ;; property advances more then 1/100s part of the file size
          ;; See https://github.com/zevlg/telega.el/issues/164
          (and (not omit-throttle-p)
               (or (and (telega-file--uploading-p file)
                        (telega-file--uploading-p old-file)
                        (< (- (telega-file--uploading-progress file)
                              (telega-file--uploading-progress old-file))
                           0.01))
                   (and (telega-file--downloading-p file)
                        (telega-file--downloading-p old-file)
                        (< (- (telega-file--downloading-progress file)
                              (telega-file--downloading-progress old-file))
                           0.01))))))
    (unless throttle-p
      ;; Keep number of download tries
      (unless (or (telega-file--downloaded-p file)
                  (telega-file--downloading-p file))
        (when-let ((dtries (plist-get old-file :download-tries)))
          (plist-put file :download-tries dtries)))

      (telega-file--ensure file)

      (let* ((callbacks (gethash file-id telega--files-updates))
             (left-cbs (cl-loop for cb in callbacks
                                when (funcall cb file)
                                collect cb)))
        (telega-debug "%s %S started with %d callbacks, left %d callbacks%s"
                      (propertize "FILE-UPDATE" 'face 'bold)
                      file-id (length callbacks) (length left-cbs)
                      (if omit-throttle-p " (forced)" ""))
        (if left-cbs
            (puthash file-id left-cbs telega--files-updates)
          (remhash file-id telega--files-updates))

        (when (and (not (telega-file--downloaded-p old-file))
                   (telega-file--downloaded-p file))
          (run-hook-with-args 'telega-file-downloaded-hook file))
        ))))

(cl-defun telega-file--download (file &key priority offset limit
                                      update-callback)
  "Download file denoted by FILE-ID.
PRIORITY - (1-32) the higher the PRIORITY, the earlier the file
will be downloaded. (default=1)
Run UPDATE-CALLBACK every time FILE gets updated.
To cancel downloading use `telega-file--cancel-download', it will
remove the UPDATE-CALLBACK as well.
OFFSET and LIMIT specifies file part to download."
  (declare (indent 1))
  ;; - If file already downloaded, then just call the callback
  ;; - If file already downloading, then just install the callback
  ;; - If file can be downloaded, then start downloading file and
  ;;   install callback after file started downloading
  (let* ((file-id (plist-get file :id))
         (dfile (or (telega-file-get file-id 'locally) file)))
    (cond ((telega-file--downloaded-p dfile)
           (when update-callback
             (funcall update-callback dfile)))

          ((telega-file--downloading-p dfile)
           (when update-callback
             (telega-file--add-update-callback file-id
               (lambda (file)
                 (funcall update-callback file)
                 (telega-file--downloading-p file)))))

          ((> (or (plist-get dfile :download-tries) 0) 3)
           ;; NOTE: workaround TDLib issue, that file which can't be
           ;; downloaded is not marked with `can_be_downloaded:false'.
           ;; We try 3 times to start downloading file, then mark this
           ;; file as non-downloadable
           )

          ((telega-file--can-download-p dfile)
           (when update-callback
             (telega-file--add-update-callback file-id
               (lambda (file)
                 (funcall update-callback file)
                 (telega-file--downloading-p file))))

           (plist-put dfile :download-tries
                      (1+ (or (plist-get dfile :download-tries) 0)))

           ;; NOTE: Mark file as being downloading before calling
           ;; `telega--downloadFile', so subsequent calls to
           ;; `telega-file--download' won't call to
           ;; `telega--downloadFile' multiple times
           (plist-put (plist-get dfile :local) :is_downloading_active t)
           (telega--downloadFile file-id
             :priority priority
             :offset offset
             :limit limit
             :callback #'ignore)))))

(defun telega-file--cancel-download (file &optional sync-p)
  "Cancel downloading a FILE.
If SYNC-P is specified, wait for file being canceled to download."
  (telega--cancelDownloadFile file nil (unless sync-p #'ignore)))

;; NOTE: `telega--downloadFile' downloads data in pretty random order
;; Use `telega-file--split-to-parts' to split file to parts for
;; sequentual downloading.
(defun telega-file--split-to-parts (file chunk-size &optional from to)
  "Split FILE by CHUNK-SIZE and return parts list."
  (let ((from (or from 0))
        (to (or to (telega-file--size file)))
        (parts nil))
    (while (< from to)
      (when (> (+ from chunk-size) to)
        (setq chunk-size (- to from)))
      (push (cons from chunk-size) parts)
      (setq from (+ from chunk-size)))
    (nreverse parts)))

(cl-defun telega-file--download-incrementally (file parts
                                                    &key (priority 32)
                                                    update-callback
                                                    internal-update-callback)
  "Download file incrementally by CHUNK-SIZE."
  (declare (indent 2))
  (when (and update-callback (not internal-update-callback))
    (setq internal-update-callback
          (lambda (dfile)
            (when (or (telega-file--downloading-p dfile)
                      (telega-file--downloaded-p dfile))
              ;; Ignore start/stop downloading flickering while
              ;; fetching parts of the file
              (funcall update-callback dfile))
            (not (telega-file--downloaded-p dfile))))
    (telega-file--add-update-callback
        (plist-get file :id) internal-update-callback))

  (let ((file-id (plist-get file :id))
        (file-part (car parts))
        (other-parts (cdr parts)))
    (telega--downloadFile file-id
      :priority priority
      :offset (car file-part)
      :limit (cdr file-part)
      :sync-p t
      :callback
      (lambda (dfile)
        (if (telega--tl-error-p dfile)
            (progn
              (telega-file--del-update-callback file-id internal-update-callback)
              ;; NOTE: file is not yet updated properly, so we use
              ;; `getFile' to update it
              (let ((nfile (telega--getFile file-id)))
                (telega-file--ensure nfile)
                (funcall update-callback nfile)))

          (funcall update-callback dfile 'chunk-done)
          (when other-parts
            (telega-file--download-incrementally dfile other-parts
              :update-callback update-callback
              :internal-update-callback internal-update-callback))))
      )))

(cl-defun telega-file--upload (filename &key file-type priority
                                        update-callback)
  "Upload FILENAME to the cloud.
Return file object, obtained from `telega--preliminaryUploadFile'."
  (declare (indent 1))
  (let ((ufile (telega--preliminaryUploadFile (expand-file-name filename)
                 :file-type file-type
                 :priority priority)))
    (cond ((telega--tl-error-p ufile)
           (error "telega: %s" (plist-get ufile :message)))
          ((telega-file--uploaded-p ufile)
           (when update-callback
             (funcall update-callback ufile)))
          (update-callback
           (telega-file--add-update-callback (plist-get ufile :id)
             (lambda (file)
               (funcall update-callback file)
               (telega-file--uploading-p file)))))
    ufile))


;;; Photos
(defmacro telega-thumbnail--get (type thumbnails)
  "Get thumbnail of TYPE from list of THUMBNAILS.
Thumbnail TYPE and its sizes:
\"s\"  box   100x100
\"m\"  box   320x320
\"x\"  box   800x800
\"y\"  box   1280x1280
\"w\"  box   2560x2560
\"a\"  crop  160x160
\"b\"  crop  320x320
\"c\"  crop  640x640
\"d\"  crop  1280x1280"
  `(cl-find ,type ,thumbnails :test 'string= :key (telega--tl-prop :type)))

(defun telega-photo--highres (photo)
  "Return thumbnail of highest resolution for the PHOTO.
Return thumbnail that can be downloaded."
  (or (cl-some (lambda (tn)
                 (let ((tn-file (telega-file--renew tn :photo)))
                   (when (or (telega-file--downloaded-p tn-file)
                             (telega-file--can-download-p tn-file))
                     tn)))
               ;; From highest res to lower
               (reverse (plist-get photo :sizes)))

      ;; Fallback to the very first thumbnail
      (aref (plist-get photo :sizes) 0)))

(defun telega-photo--thumb (photo)
  "While downloading best photo, get small thumbnail for the PHOTO."
  (let ((photo-sizes (plist-get photo :sizes)))
    (or (cl-some (lambda (tn)
                   (when (telega-file--downloaded-p
                          (telega-file--renew tn :photo))
                     tn))
                 photo-sizes)
        (cl-some (lambda (tn)
                   (when (telega-file--downloading-p
                          (telega-file--renew tn :photo))
                     tn))
                 photo-sizes)
        (cl-some (lambda (tn)
                   (when (telega-file--can-download-p
                          (telega-file--renew tn :photo))
                     tn))
                 photo-sizes)
        )))

(defun telega-photo--best (photo &optional limits)
  "Select best thumbnail from PHOTO suiting LIMITS.
By default LIMITS is `telega-photo-size-limits'."
  (unless limits
    (setq limits telega-photo-size-limits))

  (let ((lim-tw (telega-chars-xwidth (nth 2 limits)))
        (lim-th (telega-chars-xheight (nth 3 limits)))
        ret)
    (seq-doseq (thumb (plist-get photo :sizes))
      (let* ((thumb-file (telega-file--renew thumb :photo))
             (tw (telega-tl-get0 thumb :width))
             (th (telega-tl-get0 thumb :height)))
        ;; NOTE: By default (not ret) use any downloadable file, even
        ;; if size does not fits
        ;; Select sizes larger then limits, because downscaling works
        ;; betten then upscaling
        (when (and (or (telega-file--downloaded-p thumb-file)
                       (and (telega-file--can-download-p thumb-file)
                            (not (telega-file--downloaded-p
                                  (plist-get ret :photo)))))

                   (or (not ret)
                       (and (> tw lim-tw)
                            (> th lim-th))
                       ;; NOTE: prefer thumbs with `:progressive_sizes'
                       (and (= tw lim-tw)
                            (= tw lim-tw)
                            (not (seq-empty-p
                                  (plist-get thumb :progressive_sizes)))
                            (seq-empty-p (plist-get ret :progressive_sizes)))))
          (setq ret thumb
                lim-tw tw
                lim-th th))))

    (or ret
        ;; Fallback to the very first thumbnail
        (aref (plist-get photo :sizes) 0))))

(defun telega-photo--open (photo &optional for-msg)
  "Download highres PHOTO asynchronously and open it as a file.
If FOR-MSG is non-nil, then FOR-MSG is message containing PHOTO."
  (let* ((hr (telega-photo--highres photo))
         (hr-file (telega-file--renew hr :photo)))
    (telega-file--download hr-file
      :priority 32
      :update-callback
      (lambda (tl-file)
        (when for-msg
          (telega-msg-redisplay for-msg))
        (when (telega-file--downloaded-p tl-file)
          (when (telega--tl-get for-msg :content :is_secret)
            (telega--openMessageContent for-msg))
          (if (memq 'photo telega-open-message-as-file)
              (telega-open-file (telega-file--path tl-file) for-msg)
            (telega-image-view-file tl-file for-msg)))))))


(defun telega-image-supported-file-p (filename &optional error-if-unsupported)
  "Same as `image-supported-file-p'.
Trigger an error if ERROR-IF-UNSUPPORTED is specified and FILENAME is
not natively supported."
  (or (funcall (if (fboundp 'image-supported-file-p)
                   'image-supported-file-p
                 'image-type-from-file-name)
               filename)
      (and error-if-unsupported
           (error "telega: \"%s\" image's format is unsupported"
                  filename))))

(defun telega-image--telega-text (img &optional slice-num)
  "Return text version for image IMG and its slice SLICE-NUM.
Return nil if `:telega-text' is not specified in IMG."
  (let ((tt (plist-get (cdr img) :telega-text)))
    (cond ((null tt) nil)
          ((and (stringp tt) (string-empty-p tt)) nil)
          ((stringp tt) tt)
          ((listp tt)
           (if slice-num
               (progn
                 (cl-assert (> (length tt) slice-num))
                 (nth slice-num tt))
             (mapconcat 'identity tt "\n")))
          (t (cl-assert nil nil "Invalid value for :telega-text=%S" tt)))))

(defun telega-media--cheight-for-limits (width height limits)
  "Calculate cheight for image of WIDTHxHEIGHT size fitting into LIMITS."
  (let* ((width (or width (nth 0 limits)))
         (height (or height (nth 1 limits)))
         (ratio (min (/ (float (telega-chars-xwidth (nth 2 limits))) width)
                     (/ (float (telega-chars-xheight (nth 3 limits))) height))))
    (if (< ratio 1.0)
        (telega-chars-in-height (floor (* height ratio)))

      (let ((cheight (telega-chars-in-height height)))
        (if (< cheight (nth 1 limits))
            (nth 1 limits)
          (cl-assert (<= cheight (nth 3 limits)))
          cheight))
      )))

(defun telega-media--progress-svg (file width height cheight)
  "Generate svg showing downloading progress for FILE."
  (let ((svg (telega-svg-create (if (telega-zerop width) 100 width)
                                (if (telega-zerop height) 100 height))))
    (telega-svg-progress svg (telega-file--downloading-progress file) t)
    (telega-svg-image svg
      :scale 1.0
      :height (telega-ch-height cheight)
      :telega-nslices cheight
      :ascent 'center)))

(defsubst telega-photo--progress-svg (photo cheight)
  "Generate svg for the PHOTO."
  (telega-media--progress-svg
   (telega-file--renew photo :photo)
   (telega-tl-get0 photo :width)
   (telega-tl-get0 photo :height)
   cheight))

(defun telega-media--create-image (file width height &optional cheight
                                        progressive-sizes)
  "Create image to display FILE.
WIDTH and HEIGHT specifies size of the FILE's image.
CHEIGHT is the height in chars to use (default=1).
PROGRESSIVE-SIZES specifies list of jpeg's progressive file sizes."
  (unless cheight
    (setq cheight 1))
  (let* ((local-file (plist-get file :local))
         (partial-size
          (when (and (not (seq-empty-p progressive-sizes))
                     (telega-file--downloading-p file)
                     (zerop (telega-tl-get0 local-file :download_offset))
                     (>= (telega-tl-get0 local-file :downloaded_prefix_size)
                         (seq-first progressive-sizes)))
            (cl-find (telega-tl-get0 local-file :downloaded_prefix_size)
                     (seq-reverse progressive-sizes) :test #'>=)))
         (image-filename
          (cond ((telega-file--downloaded-p file)
                 (plist-get local-file :path))
                (partial-size
                 ;; NOTE: Handle case when file is partially
                 ;; downloaded and some progressive size is
                 ;; reached. In this case create temporary image file
                 ;; writing corresponding progress bytes into it and
                 ;; displaying it
                 (let* ((tl-filepath (plist-get local-file :path))
                        (tmp-fname (expand-file-name
                                    (format "%s-%d.%s"
                                            (file-name-base tl-filepath)
                                            partial-size
                                            (file-name-extension tl-filepath))
                                    telega-temp-dir))
                       (coding-system-for-write 'binary))
                   (unless (file-exists-p tmp-fname)
                     (telega-debug "Creating progressive img: %d / %S -> %s"
                                   (telega-file--downloaded-size file)
                                   progressive-sizes
                                   tmp-fname)
                     (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally tl-filepath)
                       (write-region 1 (+ 1 partial-size) tmp-fname nil 'quiet)))
                   tmp-fname)))))
    (if image-filename
        (telega-create-image
            (if (string-empty-p image-filename)
                (telega-etc-file "non-existing.jpg")
              image-filename)
            nil nil
          :height (telega-ch-height cheight)
          :telega-nslices cheight
          :scale 1.0
          :ascent 'center)
      (telega-media--progress-svg file width height cheight))))

(defun telega-minithumb--create-image (minithumb cheight)
  "Create image and use MINITHUMB minithumbnail as data."
  (telega-create-image
      (base64-decode-string (plist-get minithumb :data))
      (if (and (fboundp 'image-transforms-p)
               (funcall 'image-transforms-p))
          'jpeg
        (when (fboundp 'imagemagick-types)
          'imagemagick))
      t
    :height (telega-ch-height cheight)
    :telega-nslices cheight
    :scale 1.0
    :ascent 'center))

(defun telega-thumb--create-image (thumb &optional _file cheight)
  "Create image for the thumbnail THUMB.
THUMB could be `photoSize' or `thumbnail'.
CHEIGHT is the height in chars (default=1)."
  (telega-media--create-image
   (let ((thumb-tl-type (telega--tl-type thumb)))
     (if (eq thumb-tl-type 'photoSize)
         (telega-file--renew thumb :photo)
       (cl-assert (eq thumb-tl-type 'thumbnail))
       (telega-file--renew thumb :file)))
   (telega-tl-get0 thumb :width)
   (telega-tl-get0 thumb :height)
   cheight
   (append (plist-get thumb :progressive_sizes) nil)))

(defun telega-thumb--create-image-one-line (thumb &optional file)
  "Create image for thumbnail (photoSize) for one line use."
  (telega-thumb--create-image thumb file 1))

(defun telega-thumb--create-image-two-lines (thumb &optional file)
  "Create image for thumbnail (photoSize) for two lines use."
  (telega-thumb--create-image thumb file 2))

(defun telega-thumb--create-image-three-lines (thumb &optional file)
  "Create image for thumbnail (photoSize) for three lines use."
  (telega-thumb--create-image thumb file 3))

(defun telega-thumb--create-image-as-is (thumb &optional file)
  "Create image for thumbnail THUMB (photoSize) with size as is."
  (telega-thumb--create-image
   thumb file (telega-chars-in-height (plist-get thumb :height))))

(defun telega-thumb-or-minithumb--create-image (tl-obj &optional _file
                                                       custom-thumb
                                                       custom-minithumb)
  "Create image fol TL-OBJ that has :thumbnail and/or :minithumbnail prop."
  (let* ((thumb (or custom-thumb (plist-get tl-obj :thumbnail)))
         (thumb-cheight (telega-media--cheight-for-limits
                         (telega-tl-get0 thumb :width)
                         (telega-tl-get0 thumb :height)
                         telega-thumbnail-size-limits))
         (thumb-file (telega-file--renew thumb :file))
         (minithumb (or custom-minithumb (plist-get tl-obj :minithumbnail))))
    (cond ((telega-file--downloaded-p thumb-file)
           (telega-thumb--create-image
            thumb thumb-file thumb-cheight))
          (minithumb
           (telega-minithumb--create-image
            minithumb thumb-cheight))
          (t
           (telega-thumb--create-image
            thumb thumb-file thumb-cheight)))))

(defvar telega-preview--create-svg-one-line-function nil
  "Bind this to alter `telega-photo-preview--create-image-one-line' and
`telega-video-preview--create-image-one-line' behaviour.")

(defvar telega-preview--inhibit-cached-preview nil
  "Bind to non-nil to inhibit cached preview image in
`telega-photo-preview--create-image-one-line' and
`telega-video-preview--create-image-one-line'.")

(defun telega-photo-preview--create-image-one-line (photo &optional for-chat)
  "Return one line preview image for the PHOTO.
Return nil if preview image is unavailable."
  (when (and telega-use-images
             (telega-chat-match-p for-chat telega-use-one-line-preview-for))
    (let* ((create-svg-fun (or telega-preview--create-svg-one-line-function
                               #'telega-photo-preview--create-svg-one-line))
           (best (telega-photo--best photo '(1 1 1 1)))
           (best-file (plist-get best :photo))
           (minithumb (plist-get photo :minithumbnail))
           (cached-preview (unless telega-preview--inhibit-cached-preview
                             (plist-get photo :telega-preview-1)))
           (preview-new
            (cond ((and (telega-file--downloaded-p best-file)
                        (not (eq 'best (car cached-preview))))
                   (cons 'best
                         (funcall create-svg-fun
                                  (telega-file--path best-file)
                                  nil
                                  (telega-tl-get0 best :width)
                                  (telega-tl-get0 best :height))))
                  (cached-preview
                   cached-preview)
                  (minithumb
                   (cons 'mini
                         (funcall create-svg-fun
                                  (base64-decode-string
                                   (plist-get minithumb :data))
                                  t
                                  (telega-tl-get0 minithumb :width)
                                  (telega-tl-get0 minithumb :height)))))))
      (plist-put photo :telega-preview-1 preview-new)
      (cdr preview-new))))

(defun telega-video-preview--create-image-one-line (video &optional for-chat)
  "Return one line preview for the VIDEO.
Return nil if preview image is unavailable."
  (when (and telega-use-images
             (telega-chat-match-p for-chat telega-use-one-line-preview-for))
    (let* ((create-svg-fun (or telega-preview--create-svg-one-line-function
                               #'telega-video-preview--create-svg-one-line))
           (thumb (plist-get video :thumbnail))
           (thumb-file (plist-get thumb :file))
           (minithumb (plist-get video :minithumbnail))
           (cached-preview (unless telega-preview--inhibit-cached-preview
                             (plist-get video :telega-preview-1)))
           (preview-new
            (cond ((and thumb
                        (memq (telega--tl-type (plist-get thumb :format))
                              '(thumbnailFormatJpeg thumbnailFormatPng))
                        (telega-file--downloaded-p thumb-file)
                        (not (eq 'best (car cached-preview))))
                   (cons 'best
                         (funcall create-svg-fun
                                  (telega-file--path thumb-file)
                                  nil
                                  (telega-tl-get0 thumb :width)
                                  (telega-tl-get0 thumb :height))))
                  (cached-preview
                   cached-preview)
                  (minithumb
                   (cons 'mini
                         (funcall create-svg-fun
                                  (base64-decode-string
                                   (plist-get minithumb :data))
                                  t
                                  (telega-tl-get0 minithumb :width)
                                  (telega-tl-get0 minithumb :height)))))))
      (plist-put video :telega-preview-1 preview-new)
      (cdr preview-new))))

(defun telega-audio--create-image (audio &optional file)
  "Function to create image for AUDIO album cover."
  (telega-thumb-or-minithumb--create-image
   audio file
   (plist-get audio :album_cover_thumbnail)
   (plist-get audio :album_cover_minithumbnail)))

(defun telega-video--create-image (video &optional file)
  "Create image to preview VIDEO content."
  (if (not telega-use-svg-base-uri)
      (telega-thumb-or-minithumb--create-image video file)

    ;; SVG's `:base-uri' is available
    (let* ((thumb (plist-get video :thumbnail))
           (thumb-file (telega-file--renew thumb :file))
           (minithumb (plist-get video :minithumbnail))
           (v-width (telega-tl-get0 video :width))
           (v-height (telega-tl-get0 video :height))
           (cheight (telega-media--cheight-for-limits
                     v-width v-height telega-video-size-limits))
           (svg (telega-svg-create v-width v-height))
           (base-uri-fname ""))
      (cond ((and (memq (telega--tl-type (plist-get thumb :format))
                        '(thumbnailFormatJpeg thumbnailFormatPng))
                  (telega-file--downloaded-p thumb-file))
             (setq base-uri-fname (telega-file--path thumb-file))
             (telega-svg-embed-image-fitting
              svg base-uri-fname nil
              (telega-tl-get0 thumb :width)
              (telega-tl-get0 thumb :height)))

            (minithumb
             (telega-svg-embed-image-fitting
              svg (base64-decode-string (plist-get minithumb :data)) t
              (telega-tl-get0 minithumb :width)
              (telega-tl-get0 minithumb :height))))

      (telega-svg-white-play-triangle-in-circle svg)
      (telega-svg-image svg
        :scale 1.0
        :ascent 'center
        :height (telega-ch-height cheight)
        :telega-nslices cheight
        :base-uri base-uri-fname))))

(defun telega-media--image-update (obj-spec file &optional cache-prop)
  "Called to update the image contents for the OBJ-SPEC.
OBJ-SPEC is cons of object and create image function.
Create image function accepts two arguments - object and FILE.
Return updated image, cached or created with create image function.

CACHE-PROP specifies property name to cache image at OBJ-SPEC.
Default is `:telega-image'."
  (let ((cached-image (plist-get (car obj-spec) (or cache-prop :telega-image)))
        (simage (funcall (cdr obj-spec) (car obj-spec) file)))
    ;; NOTE: Sometimes `create' function returns nil results
    ;; Probably, because Emacs has no access to the image file while
    ;; trying to convert sticker from webp to png
    (when (and telega-use-images (not simage))
      (error "telega: [BUG] Image create (%S %S) -> nil"
             (car obj-spec) file))

    (unless (equal cached-image simage)
      ;; Update the image
      (if cached-image
          (setcdr cached-image (cdr simage))
        (setq cached-image simage))

      ;; NOTE: We call `image-flush' because only filename in
      ;; the image spec can be changed (during animation for
      ;; example), and image caching won't notice this because
      ;; `(sxhash cached-image)' and `(sxhash simage)' might
      ;; return the same!
      ;;
      ;; We do it under `ignore-errors' to avoid any image related errors
      ;; see https://github.com/zevlg/telega.el/issues/349
      ;; and https://t.me/emacs_telega/33101
      (when telega-use-images
        (ignore-errors (image-flush cached-image)))

      (plist-put (car obj-spec) (or cache-prop :telega-image) cached-image))
    cached-image))

(defun telega-media--image (obj-spec file-spec &optional force-update cache-prop)
  "Return image for media object specified by OBJ-SPEC.
File is specified with FILE-SPEC.
CACHE-PROP specifies property name to cache image at OBJ-SPEC.
Default is `:telega-image'."
  (let ((cached-image (plist-get (car obj-spec) (or cache-prop :telega-image))))
    (when (or force-update (not cached-image))
      (let ((media-file (telega-file--renew (car file-spec) (cdr file-spec))))
        ;; First time image is created or update is forced
        (setq cached-image
              (telega-media--image-update obj-spec media-file cache-prop))

        ;; Possibly initiate file downloading
        (when (and telega-use-images
                   (or (telega-file--need-download-p media-file)
                       (telega-file--downloading-p media-file)))
          (telega-file--download media-file
            :update-callback
            (lambda (dfile)
              (when (telega-file--downloaded-p dfile)
                (telega-media--image-update obj-spec dfile cache-prop)
                (force-window-update)))))))
    cached-image))

(defun telega-media--obj-spec (obj create-image-fun &rest obj-plist)
  "Return media object spec for the object OBJ."
  (declare (indent 2))
  (let ((obj-spec (nconc (list :object obj :create-image-fun create-image-fun)
                         obj-plist)))
    (unless (plist-get obj-spec :cache-prop)
      (plist-put obj-spec :cache-prop
                 (intern (format ":telega-image-%S"
                                 (plist-get obj-spec :cheight)))))
    obj-spec))

(cl-defun telega-media--image-updateNEW (obj-spec &optional no-window-update)
  "Update media image for the OBJ-SPEC.
OBJ-SPEC is a plist.
Pass non-nil NO-WINDOW-UPDATE to ommit call to `force-window-update'."
  (let* ((obj (plist-get obj-spec :object))
         (cache-prop (plist-get obj-spec :cache-prop))
         (cached-image (plist-get obj cache-prop))
         (simage (funcall (plist-get obj-spec :create-image-fun) obj-spec)))
    ;; NOTE: Sometimes `create' function returns nil results
    ;; Probably, because Emacs has no access to the image file while
    ;; trying to convert sticker from webp to png
    (when (and telega-use-images (not simage))
      (error "telega: [BUG] Create image (%S %S) -> nil"
             (plist-get obj-spec :create-image-fun) obj))

    (unless (equal cached-image simage)
      ;; NOTE: We call `image-flush' because only filename in
      ;; the image spec can be changed (during animation for
      ;; example), and image caching won't notice this because
      ;; `(sxhash cached-image)' and `(sxhash simage)' might
      ;; return the same!
      ;;
      ;; We do it under `ignore-errors' to avoid any image related errors
      ;; see https://github.com/zevlg/telega.el/issues/349
      ;; and https://t.me/emacs_telega/33101
      (when telega-use-images
        (ignore-errors (image-flush cached-image)))

      ;; Update the image
      (if cached-image
          (setcdr cached-image (cdr simage))
        (setq cached-image simage))

      (plist-put obj cache-prop cached-image)

      (unless no-window-update
        (force-window-update)))
    cached-image))

(defun telega-media--imageNEW (obj create-image-fun &rest obj-plist)
  "Create cached image for the OBJ."
  (declare (indent 2))
  (let* ((obj-spec (apply #'telega-media--obj-spec obj create-image-fun
                          obj-plist))
         (cached-image (plist-get obj (plist-get obj-spec :cache-prop))))
    (when (not cached-image)
      (setq cached-image
            (telega-media--image-updateNEW obj-spec 'no-window-update)))
    cached-image))

(defun telega-photo--image (photo limits)
  "Return best suitable image for the PHOTO."
  (let* ((best (telega-photo--best photo limits))
         (cheight (telega-media--cheight-for-limits
                   (telega-tl-get0 best :width)
                   (telega-tl-get0 best :height)
                   limits))
         (create-image-fun
          (progn
            (cl-assert (> cheight 0))
            (cl-assert (<= cheight (nth 3 limits)))
            (lambda (_photoignored &optional _fileignored)
              ;; 1) FILE downloaded, show photo
              ;; 2) Thumbnail is downloaded, use it
              ;; 2.5) Minithumbnail is available, use it
              ;; 3) FILE downloading, fallback to progress svg
              (or (let ((best-file (telega-file--renew best :photo)))
                    (when (telega-file--downloaded-p best-file)
                      (telega-thumb--create-image best best-file cheight)))
                  (let* ((thumb (telega-photo--thumb photo))
                         (thumb-file (telega-file--renew thumb :photo)))
                    (when (telega-file--downloaded-p thumb-file)
                      (telega-thumb--create-image thumb thumb-file cheight)))
                  (when-let ((minithumb (plist-get photo :minithumbnail)))
                    (telega-minithumb--create-image minithumb cheight))
                  (telega-photo--progress-svg best cheight))))))

    (telega-media--image
     (cons photo create-image-fun)
     (cons best :photo)
     'force-update)))

(defun telega-avatar-text-simple (sender wchars)
  "Create textual avatar for the SENDER (chat or user).
WCHARS is number of chars in width used for the avatar.
To be used as `telega-avatar-text-function'."
  (let ((title (telega-msg-sender-title sender)))
    (concat "(" (substring title 0 1) ")"
            (when (> wchars 3)
              (make-string (- wchars 3) ?\s)))))

(defun telega-avatar-text-composed (sender wchars)
  "Return avatar text as text with composed `telega-symbol-circle' char.
To be used as `telega-avatar-text-function'."
  (let ((title (telega-msg-sender-title sender)))
    (concat (propertize (compose-chars (aref telega-symbol-circle 0)
                                       (aref title 0))
                        'face (telega-msg-sender-title-faces sender))
            (make-string wchars ?\s))))

(defun telega-avatar--create-image (sender file &optional cheight addon-function)
  "Create SENDER (char or user) avatar image.
CHEIGHT specifies avatar height in chars, default is 2."
  ;; NOTE:
  ;; - For CHEIGHT==1 align avatar at vertical center
  ;; - For CHEIGHT==2 make svg height to be 3 chars, so if font size
  ;;   is increased, there will be no gap between two slices
  (unless cheight (setq cheight 2))
  (let* ((base-dir (telega-directory-base-uri telega-database-dir))
         (photofile (telega-file--path file))
         (factors (alist-get cheight telega-avatar-factors-alist))
         (cfactor (or (car factors) 0.9))
         (mfactor (or (cdr factors) 0.1))
         (xh (telega-chars-xheight cheight))
         (margin (* mfactor xh))
         (ch (* cfactor xh))
         (cfull (floor (+ ch margin)))
         (aw-chars (telega-chars-in-width ch))
         (svg-xw (telega-chars-xwidth aw-chars))
         (svg-xh (cond ((= cheight 1) cfull)
                       ((= cheight 2) (+ cfull (telega-chars-xheight 1)))
                       (t xh)))
         (svg (telega-svg-create svg-xw svg-xh)))
    (if (telega-file-exists-p photofile)
        (let ((img-type (telega-image-supported-file-p photofile))
              (clip (telega-svg-clip-path svg "clip")))
          (svg-circle clip (/ svg-xw 2) (/ cfull 2) (/ ch 2))
          (telega-svg-embed svg (list (file-relative-name photofile base-dir)
                                      base-dir)
                            (format "image/%S" img-type)
                            nil
                            :x (/ (- svg-xw ch) 2) :y (/ margin 2)
                            :width ch :height ch
                            :clip-path "url(#clip)"))

      ;; Draw initials
      (let* ((telega-palette-context 'avatar)
             (palette (telega-msg-sender-palette sender))
             (c1 (telega-color-name-as-hex-2digits
                  (or (telega-palette-attr palette :background) "gray75")))
             (c2 (telega-color-name-as-hex-2digits
                  (or (telega-palette-attr palette :foreground) "gray25"))))
        (svg-gradient svg "cgrad" 'linear (list (cons 0 c1) (cons ch c2))))
      (svg-circle svg (/ svg-xw 2) (/ cfull 2) (/ ch 2) :gradient "cgrad")
      (let ((font-size (/ ch 2)))
        (svg-text svg (telega-msg-sender-initials sender)
                  :font-size font-size
                  :font-weight "bold"
                  :fill "white"
                  :font-family "monospace"
                  :x "50%"
                  :text-anchor "middle" ; makes text horizontally centered
                  ;; XXX: Insane y calculation
                  :y (+ (/ font-size 3) (/ cfull 2))
                  )))

    ;; XXX: Apply additional function, used by `telega-patrons-mode'
    ;; Also used to outline currently speaking users in voice chats
    (when addon-function
      (funcall addon-function svg (list (/ svg-xw 2) (/ cfull 2) (/ ch 2))))

    (telega-svg-image svg
      :scale 1.0
      :width (telega-cw-width aw-chars)
      :ascent 'center
      :mask 'heuristic
      :base-uri (expand-file-name "dummy" base-dir)
      ;; Correct text for tty-only avatar display
      :telega-text
      (cons (let ((ava-text (funcall telega-avatar-text-function
                                     sender aw-chars)))
              (if (> (length ava-text) aw-chars)
                  (substring ava-text 0 aw-chars)
                ava-text))
            (mapcar (lambda (_ignore)
                      (make-string aw-chars ?\u00A0))
                    (make-list (1- cheight) 'not-used))))
    ))

(defun telega-avatar--create-image-one-line (sender file)
  "Create SENDER (chat or user) avatar image for one line use."
  (telega-avatar--create-image sender file 1))

(defun telega-avatar--create-image-three-lines (sender file)
  "Create SENDER (chat or user) avatar image for three lines use."
  (telega-avatar--create-image sender file 3))

(defun telega-msg-sender-avatar-image (msg-sender
                                       &optional create-image-fun
                                       force-update cache-prop)
  "Create avatar image for the MSG-SENDER.
By default CREATE-IMAGE-FUN is `telega-avatar--create-image'."
  (cl-assert msg-sender)
  (telega-media--image
   (cons msg-sender (or create-image-fun #'telega-avatar--create-image))
   (if (telega-user-p msg-sender)
       (cons (plist-get msg-sender :profile_photo) :small) ;user
     (cl-assert (telega-chat-p msg-sender))
     (cons (plist-get msg-sender :photo) :small)) ;chat
   force-update cache-prop))

(defun telega-msg-sender-avatar-image-one-line (msg-sender
                                                &optional create-image-fun
                                                force-update cache-prop)
  "Create one-line avatar for the MSG-SENDER.
By default CREATE-IMAGE-FUN is `telega-avatar--create-image-one-line'."
  (telega-msg-sender-avatar-image
   msg-sender (or create-image-fun #'telega-avatar--create-image-one-line)
   force-update (or cache-prop :telega-avatar-1)))

(defun telega-msg-sender-avatar-image-three-lines (msg-sender
                                                   &optional create-image-fun
                                                   force-update cache-prop)
  "Create three lines avatar for the MSG-SENDER.
By default CREATE-IMAGE-FUN is `telega-avatar--create-image-three-lines'."
  (telega-msg-sender-avatar-image
   msg-sender (or create-image-fun #'telega-avatar--create-image-three-lines)
   force-update (or cache-prop :telega-avatar-3)))

(defun telega-chat-photo-info--create-image (obj-spec)
  "Function to create image for chatPhotoInfo object spec OBJ-SPEC."
  (let* ((photo-info (plist-get obj-spec :object))
         (cheight (or (plist-get obj-spec :cheight) 2))
         (small-file (telega-file--renew photo-info :small))
         (big-file (telega-file--renew photo-info :big))
         files-to-download)

    (unless (telega-file--downloaded-p small-file)
      (setq files-to-download (list small-file)))
    (when (and (> cheight 3) (telega-file--downloaded-p big-file))
      (setq files-to-download (cons big-file files-to-download)))

    ;; Start downloading small/big files if needed
    (seq-doseq (file files-to-download)
      (unless (telega-file--downloading-p file)
        (telega-file--download file
          :priority 32
          :update-callback
          (lambda (dfile)
            (when (telega-file--downloaded-p dfile)
              (telega-media--image-updateNEW obj-spec))))))

    (cond ((telega-file--downloaded-p big-file)
           (telega-media--create-image small-file 640 640 cheight))
          ((telega-file--downloaded-p small-file)
           (telega-media--create-image small-file 160 160 cheight))
          ((plist-get photo-info :minithumbnail)
           (telega-minithumb--create-image
            (plist-get photo-info :minithumbnail) cheight))
          (t
           ;; TODO: Fallback to svg rendering
           ))))

(defun telega-chat-photo-info--image (chat-photo-info
                                      &optional cheight force-update)
  "Create image for chatPhotoInfo TL structure."
  (let* ((cheight (or cheight 2))
         (create-image-fun
          (lambda (_photoignored &optional _fileignored)
            (let ((small-file (plist-get chat-photo-info :small)))
              (cond ((telega-file--downloaded-p small-file)
                     ;; From TDLib docs: @small A small (160x160)
                     ;; chat photo variant in JPEG format.
                     (telega-media--create-image small-file 160 160 cheight))
                    ((plist-get chat-photo-info :minithumbnail)
                     (telega-minithumb--create-image
                      (plist-get chat-photo-info :minithumbnail) cheight))
                    (t
                     ;; TODO: Fallback to svg rendering
                     ))))))
    (telega-media--image
     (cons chat-photo-info create-image-fun)
     (cons chat-photo-info :small)
     force-update
     (intern (format ":telega-%d-lines" cheight)))))

(defun telega-chat-photo-info-image-one-line (chat-photo-info
                                              &optional force-update)
  "Create image for chatPhotoInfo TL structure."
  (telega-chat-photo-info--image chat-photo-info 1 force-update))


;; Venue/Map support
(defvar telega-venue-colors-alist
  '(("building/medical"    . "#43b3f4")  ; light blue?
    ("building/gym"        . "#43b3f4")  ; light blue?
    ("arts_entertainment"  . "#af52de")  ; purple
    ("travel/bedandbreakfast" . "#9987ff")
    ("travel/hotel"        . "#9987ff")
    ("travel/hostel"       . "#9987ff")
    ("travel/resort"       . "#9987ff")
    ("building"            . "#6e81b2")
    ("education"           . "#a57348")
    ("event"               . "#959595")
    ("food"                . "#ff9500")  ; orange
    ("education/cafeteria" . "#ff9500")  ; orange
    ("nightlife"           . "#af52de")  ; purple
    ("travel/hotel_bar"    . "#af52de")  ; purple
    ("parks_outdoors"      . "#6cc138")  ; green
    ("shops"               . "#ffb300")
    ("travel"              . "#1c9fff")
    ("work"                . "#ad7854")
    ("home"                . "#00aeef")))

(defun telega-venue--type-color (venue)
  "Return color for VENUE."
  (when (equal "foursquare" (plist-get venue :provider))
    (let ((venue-type (plist-get venue :type)))
      (or (alist-get venue-type telega-venue-colors-alist nil nil #'equal)
          (alist-get (string-trim-right (file-name-directory venue-type) "/")
                     telega-venue-colors-alist nil nil #'equal)
          ;; Return random color
          (nth 0 (alist-get :background (telega-palette-by-color-id
                                         (mod (sxhash venue-type) 7))))
          ))))

(defun telega-venue--type-image-filename (venue)
  "Return filename for the VENUE type."
  (when (equal "foursquare" (plist-get venue :provider))
    (concat (expand-file-name (plist-get venue :type)
                              (expand-file-name "4sq" telega-temp-dir))
            ".png")))

(defun telega-venue--type-image-download (venue &optional callback)
  "Asynchronously download file for VENUE's type.
CALLBACK is called with two args - venue itself and downloaded filename."
  (declare (indent 1))
  (when (equal "foursquare" (plist-get venue :provider))
    (url-retrieve (format "https://ss3.4sqi.net/img/categories_v2/%s_88.png"
                          (plist-get venue :type))
                  (lambda (status &optional _cbargs)
                    (unless (plist-get status :error)
                      (let ((img-filename
                             (telega-venue--type-image-filename venue))
                            (coding-system-for-write 'binary)
                            (buf (current-buffer)))
                        (mkdir (file-name-directory img-filename) t)
                        (with-temp-buffer
                          (url-insert buf)
                          (write-region nil nil img-filename nil 'quiet))
                        (kill-buffer buf)
                        (when callback
                          (funcall callback venue img-filename))))))
    ))

;; map - plist with props
;; Input props:
;;  `:width', `:height' - size of the map image
;;  `:location'         - Location for the map to display
;;  `:zoom'             - Zoom for the map to display
;;  `:scale'            - Scale for the map to display
;;  `:my-location'      - My location, to display me on the map
;;  `:sender'           - Map sender for which location is displayed
;;  `:user-locations'   - List of users locations. Each element is cons, where car is a user/chat and cdr is its location
;;  `:msg'              - Message where map is displayed
;; Runtime props:
;;  `:map-get-extra'    - Extra param for TDLib request.
;;  `:map-photo'        - Map's thumbnail photo
;;  `:map-location'     - Location of the map's thumbnail
;;  `:map-zoom'         - Zoom of the map's thumbnail
;;  `:map-scale'        - Scale of the map's thumbnail
;;  `:map-my-location'  - My location displayed on the map's thumbnail
;;  `:map-sender'       - Sender displayed on the map's thumbnail
;;  `:map-user-locations' - Other user locations on map's thumbnail
(defvar telega-map--update-distance-pixels 2)
(defconst telega-map--download-priority 32
  "Download priority for the map thumbnail files.")

(defun telega-map--sender-photo-file (sender)
  "Return small profile photo for the message SENDER."
  (if (telega-user-p sender)
      (telega--tl-get sender :profile_photo :small)
    (cl-assert (telega-chat-p sender))
    (telega--tl-get sender :photo :small)))

(defun telega-map--loc-close-enough-p (map loc1 loc2 &optional distance-px)
  "Return non-nil if LOC1 and LOC2 is close enough on MAP."
  (unless distance-px
    (setq distance-px telega-map--update-distance-pixels))

  (< (telega-map--distance-pixels
      (telega-location-distance loc1 loc2)
      (or (plist-get map :map-location) (plist-get map :location))
      (or (plist-get map :map-zoom) (plist-get map :zoom)))
     distance-px))

(defun telega-map--user-loc-close-enough-p (map ul1 ul2 &optional distance-px)
  "Return non-nil if user locations are close enough on MAP."
  (and (eq (car ul1) (car ul2))
       (telega-map--loc-close-enough-p map (cdr ul1) (cdr ul2) distance-px)))

(defun telega-map--need-update-map-p (map)
  "Return non-nil if MAP's photo need to be updated."
  (cl-assert (plist-get map :location))
  ;; NOTE: map image is too complex, always update it
  t)
  ;; (or (and (not (plist-get map :map-photo))
  ;;          (not (plist-get map :get-map-extra)))
  ;;     (not (plist-get map :map-location))
  ;;     (not (eq (plist-get map :scale) (plist-get map :map-scale)))
  ;;     (not (eq (plist-get map :zoom) (plist-get map :map-zoom)))
  ;;     (not (eq (plist-get map :sender) (plist-get map :map-sender)))
  ;;     ;; Location moved?
  ;;     (not (telega-map--loc-close-enough-p
  ;;           map (plist-get map :location) (plist-get map :map-location)
  ;;           ;; If displaying user's location, then update map
  ;;           ;; thumbnail only if user moves significantly
  ;;           (when (plist-get map :sender)
  ;;             (/ (plist-get map :height) 4))
  ;;           ))
  ;;     ;; Me moved?
  ;;     (let ((my-loc (plist-get map :my-location))
  ;;           (map-my-loc (plist-get map :map-my-location)))
  ;;       (or (and my-loc (not map-my-loc))
  ;;           (and (not my-loc) map-my-loc)
  ;;           (and my-loc map-my-loc
  ;;                (not (telega-map--loc-close-enough-p map my-loc map-my-loc)))))
  ;;     ;; Some other user moved?
  ;;     (not (eq (length (plist-get map :user-locations))
  ;;              (length (plist-get map :map-user-locations))))
  ;;     (memq t (cl-mapcar (lambda (ul1 ul2)
  ;;                          (not (telega-map--user-loc-close-enough-p
  ;;                                map ul1 ul2)))
  ;;                        (plist-get map :user-locations)
  ;;                        (plist-get map :map-user-locations)))
  ;;     ))

(defun telega-map--location-coords (map width height loc)
  "Return x and y coordinates as cons cell for the location LOC."
  (let* ((map-loc (plist-get map :map-location))
         (loc-off
          (telega-location-distance map-loc loc 'components))
         (x (+ (/ width 2)
               (telega-map--distance-pixels
                (cdr loc-off) loc (plist-get map :map-zoom))))
         (y (+ (/ height 2)
               (telega-map--distance-pixels
                (car loc-off) loc (plist-get map :map-zoom)))))
    (cons x y)))

(defun telega-map--svg-draw-scale-ruler (map svg)
  "Draw scale ruler for the MAP."
  (let* ((zoom (plist-get map :map-zoom))
         ;; Use 100 meters on zoom=17 as ruler size
         (ruler-meters
          (cond ((> zoom 17)
                 (/ 100.0 (ash 1 (- zoom 17))))
                ((< zoom 17)
                 (* 100 (ash 1 (- 17 zoom))))
                (t 100.0)))
         (ruler-w
          (telega-map--distance-pixels
           ruler-meters (or (plist-get map :map-location)
                            (plist-get map :location))
           zoom))
         (h (telega-svg-height svg))
         (font-size 26))
    (svg-text svg (if (> ruler-meters 1000)
                      (telega-i18n "lng_action_proximity_distance_km"
                        :count (/ ruler-meters 1000.0))
                    (telega-i18n "lng_action_proximity_distance_m"
                      :count (round ruler-meters)))
              :font-size font-size
              :fill-color "currentColor"
              :opacity "0.75"
              :x 10 :y (- h font-size))
    (svg-line svg 10 (- h 10) (+ 10 ruler-w) (- h 10)
              :opacity "0.75"
              :stroke-width 4
              :stroke-color "currentColor")))

(defun telega-map--svg-draw-weather (svg weather)
  "Draw weather on the map."
  (let ((emoji (telega-tl-str weather :emoji))
        (font-size 26))
    (svg-text svg (format "%s\ufe0f %d°"
                          emoji (telega-tl-get0 weather :temperature))
              :font-weight "bold"
              :font-size font-size
              :fill-color "currentColor"
              :opacity "0.75"
              :x 10 :y (+ 10 font-size))))

(defun telega-map--svg-draw-pin (svg x y w h &optional fg-color bg-color)
  "Draw a location pin pointing into X, Y point."
  (let ((w2 (/ w 2.0))
        (w4 (/ w 4.0))
        (fg-color (or fg-color "black"))
        (bg-color (or bg-color "white"))
        (head-radius (+ w w)))
    (svg-circle svg x (- y (- h head-radius)) head-radius
                :fill-color fg-color)
    (svg-circle svg (- x (/ head-radius 4.0)) (- y (- h (/ head-radius 2.0)))
                (/ head-radius 4.0) :fill-color bg-color)
    (svg-rectangle svg (- x w2) (- y (- h head-radius head-radius w2))
                   w (- h head-radius head-radius w)
                   :fill-color fg-color :rx w4)
    (svg-polygon svg (list (cons (- x w2) (- y w w))
                           (cons (+ x w2) (- y w w))
                           (cons (+ x w2) (- y w2))
                           (cons x        y)
                           (cons (- x w2) (- y w2)))
                 :fill-color fg-color)
    ))

(cl-defun telega-map--svg-draw-circle-pin (svg x y r color
                                               &key point-width with-shadow-p
                                               (opacity "0.75"))
  "Draw circle pointing into x, y.
Return center of the circle.
Return nil if circle is not visible inside SVG."
  (let ((width (telega-svg-width svg))
        (height (telega-svg-height svg))
        (r4 (or point-width (/ r 4.0))))
    (when (and (< (- r) x (+ r width))
               (< (- r) y (+ r height)))
      (when with-shadow-p
        ;; (telega-svg-append-shadow-filter svg "shadow" "5" "0")
        (telega-svg-append-glow-filter svg "glow"))
      (apply #'svg-polygon svg (list (cons x y)
                             (cons (+ x r4 1) (- y r4 1))
                             (cons (- x r4 1) (- y r4 1)))
                   :opacity opacity
                   :fill-color color
                   (when with-shadow-p
                     (list :filter "url(#glow)")))
      (apply #'svg-circle svg x (- y r r4) r
             :fill-color color
             :opacity opacity
             (when with-shadow-p
               (list :filter "url(#glow)")))
      (cons x (- y r r4)))))

(cl-defun telega-map--svg-draw-image-pin (svg x y color image-filename
                                              &key r border with-shadow-p
                                              opacity)
  "Into SVG draw circle pin pointing to X and Y with IMAGE-FILENAME inside."
  (let* ((base-dir (telega-directory-base-uri telega-database-dir))
         (r (or r (telega-chars-xheight 0.75)))
         (point-w (/ r 4.0))
         (border (or border 0))
         (cxy (telega-map--svg-draw-circle-pin
               svg x y r color
               :point-width point-w
               :with-shadow-p with-shadow-p
               :opacity opacity)))
    (when cxy
      (cl-assert (< border r))
      (let* ((cx (car cxy))
             (cy (cdr cxy))
             (img-type (telega-image-supported-file-p image-filename))
             (clip-name (make-temp-name "user-clip"))
             (clip (telega-svg-clip-path svg clip-name)))
        (svg-circle clip cx cy (- r border))
        (telega-svg-embed svg (list (file-relative-name image-filename base-dir)
                                    base-dir)
                          (format "image/%S" img-type) nil
                          :x (- cx r) :y (- cy r)
                          :width (+ r r) :height (+ r r)
                          :clip-path (format "url(#%s)" clip-name)
                          :opacity (or opacity "1.0")))
      t)))

(cl-defun telega-map--svg-draw-sender-pin (map svg sender &key loc live-loc)
  "Embed SENDER to the map svg image.
LOC or LIVE-LOC must be specified unless SENDER is a map sender."
  (let* ((width (telega-svg-width svg))
         (height (telega-svg-height svg))
         (msg (plist-get map :msg))
         (map-sender-p (eq sender (and msg (telega-msg-sender msg))))
         (inactive-p (when map-sender-p
                       (let ((live-for (telega-msg-location-live-for msg)))
                         (or (not live-for) (< (car live-for) 0)))))
         (live-loc (or live-loc
                       (unless loc
                         (cl-assert map-sender-p)
                         (telega--tl-get msg :content :location))))
         (loc (or loc (plist-get live-loc :location)))
         (xy (telega-map--location-coords map width height loc))
         (x (car xy))
         (y (cdr xy))
         (y-off (if map-sender-p
                    (telega-chars-xheight 0.1)
                  0))
         (palette
          (telega-msg-sender-palette sender))
         (color
          (telega-color-name-as-hex-2digits
           (or (telega-palette-attr palette :foreground) "white"))))

    ;; Emphasize horizontal accuracy
    (let ((hacc-meters (telega-tl-get0 loc :horizontal_accuracy)))
      (unless (or inactive-p (zerop hacc-meters))
        (let ((hacc-w (telega-map--distance-pixels
                       hacc-meters loc (plist-get map :map-zoom))))
          (when (> hacc-w 4)
            (telega-svg-gradient
             svg "haccgrad" 'radial
             (list (list 0 color :opacity 0.0)
                   (list 90 color :opacity 0.05)
                   (list 100 color :opacity 0.25)))
            (svg-circle svg x y hacc-w
                        :gradient "haccgrad")))))

    ;; User's direction heading 1-360, 0 if unknown
    (let ((heading (telega-tl-get0 live-loc :heading)))
      (unless (zerop heading)
        (let* ((w2 x)
               (h2 y)
               (angle1 (* float-pi (/ (- (+ heading 200)) 180.0)))
               (angle2 (* float-pi (/ (- (+ heading 160)) 180.0)))
               (h-dx1 (* 100 (sin angle1)))
               (h-dy1 (* 100 (cos angle1)))
               (h-dx2 (* 100 (sin angle2)))
               (h-dy2 (* 100 (cos angle2)))
               (hclip (telega-svg-clip-path svg "headclip")))
          (telega-svg-path hclip (format "M %d %d L %f %f L %f %f Z"
                                         w2 h2 (+ w2 h-dx1) (+ h2 h-dy1)
                                         (+ w2 h-dx2) (+ h2 h-dy2)))
          (telega-svg-gradient
           svg "headgrad" 'radial
           (list (list 0 (telega-color-name-as-hex-2digits
                          (face-foreground 'telega-blue))
                       :opacity 0.9)
                 ;; (list 50 (telega-color-name-as-hex-2digits
                 ;;           (face-foreground 'telega-blue))
                 ;;       :opacity 0.5)
                 (list 100 (telega-color-name-as-hex-2digits
                            (face-foreground 'telega-blue))
                       :opacity 0.0)))
          (svg-circle svg w2 h2 (telega-chars-xheight 1.25)
                      :gradient "headgrad"
                      :clip-path "url(#headclip)")
          )))

    (when map-sender-p
      ;; Always show dot for map sender
      (when (< y-off 3) (setq y-off 4))
      (svg-circle svg x y (+ 2 y-off)
                  :stroke-width 4
                  :stroke-color "white"
                  :fill-color color))

    (when-let* ((sender-photo (telega-map--sender-photo-file sender))
                ((telega-file--downloaded-p sender-photo)))
      (telega-map--svg-draw-image-pin svg x (- y y-off 4) (or "white" color)
                                      (telega-file--path sender-photo)
                                      :border y-off
                                      :with-shadow-p map-sender-p
                                      :opacity (when inactive-p
                                                 "0.6")))
    ))

(defun telega-map--svg-draw-venue (map svg venue)
  "Embed VENUE to the MAP's SVG.
Return non-nil if VENUE has been embeded."
  (let* ((bg-color (or (telega-venue--type-color venue) "#999999"))
         (map-loc (plist-get map :map-location))
         (v-loc (plist-get venue :location))
         (loc-off
          (telega-location-distance map-loc v-loc 'components))
         (width (telega-svg-width svg))
         (height (telega-svg-height svg))
         (v-x (+ (/ width 2)
                 (telega-map--distance-pixels
                  (cdr loc-off) v-loc (plist-get map :zoom))))
         (v-y (+ (/ height 2)
                 (telega-map--distance-pixels
                  (car loc-off) v-loc (plist-get map :zoom))))
         (y-off (telega-chars-xheight 0.1))
         (v-filename (telega-venue--type-image-filename venue)))
    (svg-circle svg v-x v-y y-off :fill-color bg-color)
    (when (file-exists-p v-filename)
      (telega-map--svg-draw-image-pin svg v-x (- v-y y-off 2)
                                      bg-color v-filename
                                      :with-shadow-p t
                                      :opacity "1.0"))
    t))

(cl-defun telega-map--svg-draw-location-pin (map svg loc &key r with-shadow-p)
  "Draw a location pin."
  (let* ((width (telega-svg-width svg))
         (height (telega-svg-height svg))
         (r (or r (telega-chars-xheight 0.75)))
         (point-w (/ r 4.0))
         (bg-color (telega-color-name-as-hex-2digits
                    (or (face-background 'telega-location-pin)
                        "RoyalBlue2")))
         (fg-color (telega-color-name-as-hex-2digits
                    (or (face-foreground 'telega-location-pin)
                        "white")))
         (xy (telega-map--location-coords map width height loc))
         (x (car xy))
         (y (cdr xy))
         (y-off (telega-chars-xheight 0.1)))
    (svg-circle svg x y y-off :fill-color bg-color)
    (telega-map--svg-draw-circle-pin
     svg x (- y y-off 2) r bg-color
     :point-width point-w
     :with-shadow-p with-shadow-p
     :opacity "1.0")
    (telega-map--svg-draw-pin svg x (- y y-off (* 2 y-off) point-w 2)
                              y-off (- (* 2 r) (* 4 y-off))
                              fg-color bg-color)
    ))

(defun telega-map--create-image-func (obj-spec)
  "Create map image for location MAP."
  (let* ((base-dir (telega-directory-base-uri telega-database-dir))
         (map (plist-get obj-spec :object))
         (map-photo (telega-file--renew map :map-photo))
         (map-sender (when-let ((msg (plist-get map :msg)))
                       (telega-msg-sender msg)))
         (width (or (plist-get map :width) 800))
         (height (or (plist-get map :height) 400))
         (svg (telega-svg-create width height)))
    (cl-assert (and (integerp width) (integerp height)))
    ;; NOTE: If location moved significantly, then fetch new map thumbnail
    (when (and (not (plist-get map :get-map-extra))
               (or (not map-photo)
                   (not (plist-get map :map-location))
                   (not (plist-get map :map-zoom))
                   (not (eq (plist-get map :zoom) (plist-get map :map-zoom)))
                   (> (telega-map--distance-pixels
                       (telega-location-distance (plist-get map :location)
                                                 (plist-get map :map-location))
                       (plist-get map :map-location) (plist-get map :map-zoom))
                      (/ width 6))))
      (plist-put map :map-zoom (plist-get map :zoom))
      (plist-put map :map-scale (plist-get map :scale))
      (plist-put map :map-location (plist-get map :location))
      (plist-put map :get-map-extra t)
      (telega--getMapThumbnailFile
          (plist-get map :location)
          (or (plist-get map :zoom) telega-location-zoom)
          width height
          (or (plist-get map :scale) telega-location-scale)
          (when-let ((msg (plist-get map :msg)))
            (telega-msg-chat msg))
        (lambda (map-file)
          (plist-put map :map-photo map-file)
          (telega-file--download map-file
            :priority telega-map--download-priority
            :update-callback
            (lambda (mfile)
              (when (telega-file--downloaded-p mfile)
                (plist-put map :get-map-extra nil)
                (telega-media--image-updateNEW obj-spec))

              ;; ARGUABLE: redisplay message?
              ;; (when-let ((msg (plist-get map :msg)))
              ;;   (telega-msg-redisplay msg))
              )
            )))
      ;; Update weather info as well
      (when telega-location-show-weather
        (telega--getCurrentWeather (plist-get map :location)
          (lambda (weather)
            (plist-put map :map-weather weather)
            (telega-media--image-updateNEW obj-spec))))
      )

    (if (and (telega-file--downloaded-p map-photo)
             (telega-file-exists-p (telega-file--path map-photo)) )
        (telega-svg-embed svg (list (file-relative-name
                                     (telega-file--path map-photo) base-dir)
                                    base-dir)
                          "image/png" nil
                          :x 0 :y 0 :width width :height height)
      (svg-rectangle svg 0 0 width height
                     :fill-color (telega-color-name-as-hex-2digits
                                  (or (face-foreground 'telega-shadow)
                                      "gray50"))))

    ;; Display live locations for other users
    (plist-put map :map-user-locations (plist-get map :user-locations))
    (seq-doseq (ul (plist-get map :user-locations))
      (unless (or (eq (car ul) map-sender)
                  (and telega-location-show-me
                       telega-my-location
                       (not (telega-me-p map-sender))))
        (telega-map--svg-draw-sender-pin map svg (car ul) (cdr ul))))

    ;; Display me
    (when (and telega-location-show-me
               telega-my-location
               (not (telega-me-p map-sender)))
      (telega-map--svg-draw-sender-pin map svg (telega-user-me)
                                       :loc telega-my-location))

    (let ((msg (plist-get map :msg)))
      (cond ((and msg (telega-msg-match-p msg '(type LiveLocation)))
             (telega-map--svg-draw-sender-pin map svg map-sender))

            ((and msg (telega-msg-match-p msg '(type Venue)))
             ;; Display Venue label
             (when-let* ((venue (telega--tl-get msg :content :venue))
                         (vt-filename (telega-venue--type-image-filename venue)))
               (when (and (not (file-exists-p vt-filename))
                          (not (plist-get map :get-venue-type)))
                 ;; Need to download
                 (plist-put map :get-venue-type
                            (telega-venue--type-image-download venue
                              (lambda (_venue _imgfile)
                                (plist-put map :get-venue-type nil)
                                (telega-media--image-updateNEW obj-spec)))))
               (telega-map--svg-draw-venue map svg venue)))

            (t
             ;; Location and pageBlockMap
             (telega-map--svg-draw-location-pin
              map svg (plist-get map :location)))
            ))

    ;; Display "Loading..." if updating map thumbnail
    (when (plist-get map :get-map-extra)
      (let ((font-size (telega-chars-xheight 1)))
        (svg-text svg (telega-i18n "telega_loading")
                  :font-size font-size
;                  :font-family "monospace"
                  :fill-color "currentColor"
                  :opacity "0.5"
                  :x "50%" :y font-size
                  :text-anchor "middle")))

    ;; Finally display scale ruler and weather
    (when telega-location-show-scale-ruler
      (telega-map--svg-draw-scale-ruler map svg))
    (when-let ((map-weather (plist-get map :map-weather)))
      (telega-map--svg-draw-weather svg map-weather))

    (telega-svg-image svg
      :scale 1.0
      :max-height (telega-ch-height (plist-get obj-spec :cheight))
      :width width
      :ascent 'center
      :base-uri (expand-file-name "dummy" base-dir))))

(defun telega-map--image-obj-spec (map &optional cheight)
  "Return image object spec for the MAP."
  (cl-assert (and (integerp (plist-get map :width))
                  (integerp (plist-get map :height))))
  (unless cheight
    (setq cheight
          (telega-media--cheight-for-limits
           (plist-get map :width)
           (plist-get map :height)
           (list (cdr telega-location-size) (car telega-location-size)
                 (cdr telega-location-size) (car telega-location-size)))))

  (telega-media--obj-spec map #'telega-map--create-image-func
    :cheight cheight))

(cl-defun telega-map--image (map &key cheight)
  "Return image for the MAP object."
  (declare (indent 1))
  (let ((obj-spec (telega-map--image-obj-spec map cheight)))
    (or (unless (telega-map--need-update-map-p map)
          (plist-get map (plist-get obj-spec :cache-prop)))
        (telega-media--image-updateNEW obj-spec))))

;; See
;; https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Resolution_and_Scale
(defun telega-map--distance-pixels (meters loc zoom)
  "Convert METERS distance at LOC to the pixels distance at ZOOM level."
  (let ((lat (plist-get loc :latitude)))
    (round (/ meters
              (/ (* 156543.03 (cos (degrees-to-radians lat)))
                 (expt 2 zoom))))))

(defun telega-map--zoom (map step)
  "Change zoom for the MAP by STEP.
Return non-nil if zoom has been changed."
  (let* ((old-zoom (plist-get map :zoom))
         (new-zoom (+ old-zoom step)))
    (cond ((< new-zoom 13)
           (setq new-zoom 13))
          ((> new-zoom 20)
           (setq new-zoom 20)))
    (plist-put map :zoom new-zoom)

    (let ((ret (not (= old-zoom new-zoom))))
      (when ret
        (plist-put map :get-map-extra nil)
        (telega-media--image-updateNEW (telega-map--image-obj-spec map)))
      ret)))

(defun telega-msg-for-map-interactive ()
  (when-let* ((mevents (append (this-command-keys) nil))
              (ev-key (or (assq 'wheel-up mevents)
                          (assq 'wheel-down mevents)
                          (assq 'down-mouse-1 mevents)))
              (ev-start (cadr ev-key))
              (ev-point (posn-point ev-start)))
    (telega-msg-at ev-point)))

(defun telega-msg-map-zoom-in (msg)
  "Zoom in map location for the message MSG."
  (interactive (list (telega-msg-for-map-interactive)))
  (when-let* ((map (plist-get msg :telega-map))
              ((telega-map--zoom map 1)))
    (telega-msg-redisplay msg)))

(defun telega-msg-map-zoom-out (msg)
  "Zoom in map location for the message MSG."
  (interactive (list (telega-msg-at
                      (posn-point (event-start last-command-event)))))
  (when-let* ((map (plist-get msg :telega-map))
              ((telega-map--zoom map -1)))
    (telega-msg-redisplay msg)))

(defun telega-map--button-release-event-p (event)
  (and (consp event) (symbolp (car event))
       (or (memq 'click (get (car event) 'event-symbol-elements))
           (memq 'drag (get (car event) 'event-symbol-elements)))))

(defun telega-msg-map-drag (msg event)
  "Handle last drag event."
  (interactive (list (telega-msg-at
                      (posn-point (event-start last-command-event)))
                     last-command-event))

  (track-mouse
    (while (not (telega-map--button-release-event-p (setq event (read-event))))
      ;; TODO: Read drag events until mouse button is released
      (message "telega: TODO, drag map %S" (posn-object-x-y (event-start event)))
      )))


;;; TODO: Chat Themes
(defun telega-chat-theme--create-svg (_theme &optional _cheight)
  "Create svg for chat THEME."
;;   (let ((cheight (or cheight 6))
;; ;        (svg (telega-svg-create width height))
;;         )
;;     ;; TODO: draw theme

;;     svg)
  )

(defun telega-chat-theme--create-image (theme)
  "Create image for the chat THEME."
  (let ((base-dir (telega-directory-base-uri telega-database-dir))
        (svg (telega-chat-theme--create-svg theme)))
    (telega-svg-image svg
      :scale 1.0
      :width (alist-get 'width (nth 1 svg))
      :height (alist-get 'height (nth 1 svg))
      :ascent 'center
      :base-uri (expand-file-name "dummy" base-dir))))


;;; Media layout
(defmacro telega-ins--side-by-side (delim &rest forms)
  `(seq-doseq (str (apply #'seq-mapn (lambda (&rest strings)
                                       (mapconcat #'identity strings ,delim))
                          (list
                           ,@(mapcar (lambda (form)
                                       `(split-string
                                         (telega-ins--as-string ,form) "\n"))
                                     forms))
                          ))
     (telega-ins str "\n")))

(defun telega-media-layout--min-limit (&rest limits)
  "Choose smallest limit across LIMITS."

  )

;; ref: tdesktop/Telegram/SourceFiles/ui/grouped_layout.cpp
(defun telega-media-layout--ratio (w h)
  (/ (float w) h))

(defun telega-media-layout--proportion (w h)
  (let ((ratio (telega-media-layout--ratio w h)))
    (cond ((> ratio 1.2) 'w)
          ((< ratio 0.8) 'n)
          (t 'q))))

(defun telega-media-layout--for-images (sizes)
  "Return layout for the list of the photo SIZES.
Return list of rows."
  (let ((n (length sizes)))
    (cond ((= 1 n)
           )
          )))

(provide 'telega-media)

;;; telega-media.el ends here

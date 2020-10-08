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

(declare-function telega-chat-color "telega-chat" (chat))
(declare-function telega-chat-title "telega-chat" (chat &optional with-username))

(declare-function telega-msg-redisplay "telega-msg" (msg))

(declare-function telega-image-view-file "telega-modes" (tl-file &optional for-msg))


;;; Files downloading/uploading
(defun telega-file-get (file-id)
  "Return file associated with FILE-ID."
  (or (gethash file-id telega--files)
      (telega-file--ensure (telega--getFile file-id))))

(defun telega-file--renew (place prop)
  "Renew file value at PLACE and PROP."
  (when-let* ((ppfile (plist-get place prop))
              (file-id (plist-get ppfile :id))
              (file (or (gethash file-id telega--files)
                        (telega-file--ensure ppfile))))
    (plist-put place prop file)
    file))

(defun telega-file--update (file)
  "FILE has been updated, call any pending callbacks."
  (telega-file--ensure file)

  ;; Run update callbacks
  (let* ((callbacks (gethash (plist-get file :id) telega--files-updates))
         (left-cbs (cl-loop for cb in callbacks
                            when (funcall cb file)
                            collect cb)))
    (telega-debug "%s %S started with %d callbacks, left %d callbacks"
                  (propertize "FILE-UPDATE" 'face 'bold)
                  (plist-get file :id) (length callbacks) (length left-cbs))
    (puthash (plist-get file :id) left-cbs telega--files-updates)))

(defun telega-file--callback-wrap (callback check-fun)
  "Wrapper for CALLBACK.
Removes callback in case downloading is canceled or completed."
  (when callback
    (lambda (file)
      (funcall callback file)
      (funcall check-fun file))))

(defun telega-file--download (file &optional priority callback)
  "Download file denoted by FILE-ID.
PRIORITY - (1-32) the higher the PRIORITY, the earlier the file
will be downloaded. (default=1)
Run CALLBACK every time FILE gets updated.
To cancel downloading use `telega--cancelDownloadFile', it will
remove the callback as well."
  (declare (indent 2))
  ;; - If file already downloaded, then just call the callback
  ;; - If file already downloading, then just install the callback
  ;; - If file can be downloaded, then start downloading file and
  ;;   install callback after file started downloading
  (let* ((file-id (plist-get file :id))
         (dfile (telega-file-get file-id))
         (cbwrap (telega-file--callback-wrap
                  callback 'telega-file--downloading-p)))
    (cond ((telega-file--downloaded-p dfile)
           (when cbwrap
             (funcall cbwrap dfile)))

          ((telega-file--downloading-p dfile)
           (when cbwrap
             (let ((cb-list (gethash file-id telega--files-updates)))
               (puthash file-id (cons cbwrap cb-list)
                        telega--files-updates))))

          ((telega-file--can-download-p dfile)
           (telega--downloadFile file-id priority
             (lambda (downfile)
               ;; NOTE: updateFile may arrive without setting
               ;; telega-file--downloaded-p or telega-file--downloading-p
               ;; to non-nil
               (telega-file--update downfile)
               (when (and cbwrap
                          (or (telega-file--downloaded-p downfile)
                              (telega-file--downloading-p downfile)))
                 (telega-file--download downfile priority callback))))))
    ))

(defun telega-file--upload-internal (file &optional callback)
  "Monitor FILE uploading progress by installing CALLBACK."
  (declare (indent 1))
  (let* ((file-id (plist-get file :id))
         (cbwrap (telega-file--callback-wrap
                  callback 'telega-file--uploading-p)))
    (if (telega-file--uploaded-p file)
        (when cbwrap
          (funcall cbwrap file))

      (when cbwrap
        (let ((cb-list (gethash file-id telega--files-updates)))
          (puthash file-id (cons cbwrap cb-list)
                   telega--files-updates))))
    file))

(defun telega-file--upload (filename &optional file-type priority callback)
  "Upload FILENAME to the cloud.
Return file object, obtained from `telega--uploadFile'."
  (declare (indent 3))
  (let ((file (telega--uploadFile
               (expand-file-name filename) file-type priority)))
    (telega-file--upload-internal file callback)
    file))

(defun telega-file--used-in-msg (msg)
  "Return File object associated with MSG.
Return nil if no File object is associated with the message."
  (let* ((content (plist-get msg :content))
         (file (cl-case (telega--tl-type content)
                 (messageDocument
                  (telega--tl-get content :document :document))
                 (messageAudio
                  (telega--tl-get content :audio :audio))
                 (messageVideo
                  (telega--tl-get content :video :video))
                 (messageVoiceNote
                  (telega--tl-get content :voice_note :voice))
                 (messageVideoNote
                  (telega--tl-get content :video_note :video))
                 ;; TODO: add other message types
                 )))
    (or (gethash (plist-get file :id) telega--files)
        file)))


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
  (cl-some (lambda (tn)
             (let ((tn-file (telega-file--renew tn :photo)))
               (when (or (telega-file--downloaded-p tn-file)
                         (telega-file--can-download-p tn-file))
                 tn)))
           ;; From highest res to lower
           (reverse (plist-get photo :sizes))))

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

  (let ((lim-xwidth (telega-chars-xwidth (nth 2 limits)))
        (lim-xheight (telega-chars-xheight (nth 3 limits)))
        ret)
    ;; NOTE: `reverse' is used to start from highes sizes
    (seq-doseq (thumb (reverse (plist-get photo :sizes)))
      (let* ((thumb-file (telega-file--renew thumb :photo))
             (tw (plist-get thumb :width))
             (th (plist-get thumb :height)))
        ;; NOTE: By default (not ret) use any downloadable file, even
        ;; if size does not fits
        ;; Select sizes larger then limits, because downscaling works
        ;; betten then upscaling
        (when (and (or (telega-file--downloaded-p thumb-file)
                       (telega-file--can-download-p thumb-file))
                   (or (not ret)
                       (and (>= tw lim-xwidth)
                            (>= th lim-xheight))
                       ))
          (setq ret thumb))))
    ret))

(defun telega-photo--open (photo &optional for-msg)
  "Download highres PHOTO asynchronously and open it as a file.
If FOR-MSG is non-nil, then FOR-MSG is message containing PHOTO."
  (let* ((hr (telega-photo--highres photo))
         (hr-file (telega-file--renew hr :photo)))
    (telega-file--download hr-file 32
      (lambda (tl-file)
        (when for-msg
          (telega-msg-redisplay for-msg))
        (when (telega-file--downloaded-p tl-file)
          (when (telega--tl-get for-msg :content :is_secret)
            (telega--openMessageContent for-msg))
          (telega-image-view-file tl-file for-msg))))))


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
  (let ((ratio (min (/ (float (telega-chars-xwidth (nth 2 limits))) width)
                    (/ (float (telega-chars-xheight (nth 3 limits))) height))))
    (if (< ratio 1.0)
        (telega-chars-in-height (floor (* height ratio)))

      (let ((cheight (telega-chars-in-height height)))
        (if (< cheight (nth 1 limits))
            (nth 1 limits)
          (cl-assert (<= cheight (nth 3 limits)))
          cheight))
      )))

(defun telega-media--cwidth-xmargin (width height char-height &optional _max-cwidth)
  "Calculate width in chars and margins X pixels.
MAX-CWIDTH is maximum width in chars.
Return cons cell, where car is width in char and cdr is margin value."
  ;; NOTE: handle case where WIDTH or HEIGHT can be zero
  (let* ((pix-h (telega-chars-xheight char-height))
         (pix-w (if (zerop height)
                    0
                  (* (/ (float width) height) pix-h)))
         (cw (telega-chars-in-width pix-w))
         (xmargin (/ (- (telega-chars-xwidth cw) pix-w) 2)))
;    (cl-assert (> cw 0))
    (cons cw (floor xmargin))))

(defun telega-media--progress-svg (file width height cheight)
  "Generate svg showing downloading progress for FILE."
  (let* ((xh (telega-chars-xheight cheight))
         (cwidth-xmargin (telega-media--cwidth-xmargin
                          (if (zerop width) xh width)
                          (if (zerop height) xh height) cheight))
         (w-chars (car cwidth-xmargin))
         (xw (telega-chars-xwidth w-chars))
         (svg (telega-svg-create xw xh))
         (progress (telega-file--downloading-progress file)))
    (telega-svg-progress svg progress)
    (svg-image svg :scale 1.0
               :width xw :height xh
               :ascent 'center
               :mask 'heuristic
               ;; text of correct width
               :telega-text (make-string w-chars ?X))))

(defsubst telega-photo--progress-svg (photo cheight)
  "Generate svg for the PHOTO."
  (telega-media--progress-svg
   (telega-file--renew photo :photo)
   (plist-get photo :width)
   (plist-get photo :height)
   cheight))

(defun telega-media--create-image (file width height &optional cheight)
  "Create image to display FILE.
WIDTH and HEIGHT specifies size of the FILE's image.
CHEIGHT is the height in chars to use (default=1)."
  (unless cheight
    (setq cheight 1))
  (if (telega-file--downloaded-p file)
      (let ((cw-xmargin (telega-media--cwidth-xmargin width height cheight))
            (image-filename (telega--tl-get file :local :path)))
        (create-image (if (string-empty-p image-filename)
                          (telega-etc-file "non-existing.jpg")
                        image-filename)
                      (when (fboundp 'imagemagick-types) 'imagemagick) nil
                      :height (telega-chars-xheight cheight)
                      :scale 1.0
                      :ascent 'center
                      :margin (cons (cdr cw-xmargin) 0)
                      :telega-text (make-string (car cw-xmargin) ?X)))

    (telega-media--progress-svg file width height cheight)))

(defun telega-minithumb--create-image (minithumb cheight)
  "Create image and use MINITHUMB minithumbnail as data."
  (let* ((xwidth (plist-get minithumb :width))
         (xheight (plist-get minithumb :height))
         (cwidth-xmargin (telega-media--cwidth-xmargin xwidth xheight cheight)))
    (create-image (base64-decode-string (plist-get minithumb :data))
                  (if (and (fboundp 'image-transforms-p)
                           (funcall 'image-transforms-p))
                      'jpeg
                    (when (fboundp 'imagemagick-types)
                      'imagemagick))
                  t
                  :height (telega-chars-xheight cheight)
                  :scale 1.0
                  :ascent 'center
                  :margin (cons (cdr cwidth-xmargin) 0)
                  :telega-text (make-string (car cwidth-xmargin) ?X))))

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
   (plist-get thumb :width)
   (plist-get thumb :height)
   cheight))

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
                         (plist-get thumb :width)
                         (plist-get thumb :height)
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

(defun telega-thumb-or-minithumb--create-image-one-line (_tl-obj &optional _file)
  "Same as `telega-thumb-or-minithumb--create-image' but for one line."
  ;; TODO: create squared (with round corners) version of the image
  ;; suitable for one-line use
  )

(defun telega-audio--create-image (audio &optional file)
  "Function to create image for AUDIO album cover."
  (telega-thumb-or-minithumb--create-image
   audio file
   (plist-get audio :album_cover_thumbnail)
   (plist-get audio :album_cover_minithumbnail)))

(defun telega-media--image-update (obj-spec file &optional cache-prop)
  "Called to update the image contents for the OBJ-SPEC.
OBJ-SPEC is cons of object and create image function.
Create image function accepts two arguments - object and FILE.
Return updated image, cached or created with create image function.

CACHE-PROP specifies property name to cache image at OBJ-SPEC.
Default is `:telega-image'."
  (let ((cached-image (plist-get (car obj-spec) (or cache-prop :telega-image)))
        (simage (funcall (cdr obj-spec) (car obj-spec) file)))
    (unless (equal cached-image simage)
      ;; Update the image
      (if cached-image
          (setcdr cached-image (cdr simage))
        (setq cached-image simage))
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

        ;; Possible initiate file downloading
        (when (or (telega-file--need-download-p media-file)
                  (telega-file--downloading-p media-file))
          (telega-file--download media-file nil
            (lambda (dfile)
              (cl-assert (plist-get (car obj-spec) (or cache-prop :telega-image)))
              (telega-media--image-update obj-spec dfile cache-prop)
              (force-window-update))))))
    cached-image))

(defun telega-photo--image (photo limits)
  "Return best suitable image for the PHOTO."
  (let* ((best (telega-photo--best photo limits))
         (cheight (telega-media--cheight-for-limits
                   (plist-get best :width)
                   (plist-get best :height)
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
              (let ((best-file (telega-file--renew best :photo)))
                (if (telega-file--downloaded-p best-file)
                    (telega-thumb--create-image best best-file cheight)
                  (let* ((thumb (telega-photo--thumb photo))
                         (thumb-file (telega-file--renew thumb :photo)))
                    (if (telega-file--downloaded-p thumb-file)
                        (telega-thumb--create-image thumb thumb-file cheight)
                      (if-let ((minithumb (plist-get photo :minithumbnail)))
                          (telega-minithumb--create-image minithumb cheight)
                        (telega-photo--progress-svg best cheight))))))))))

    (telega-media--image
     (cons photo create-image-fun)
     (cons best :photo)
     'force-update)))

(defun telega-avatar--create-image (chat-or-user file &optional cheight)
  "Create image for CHAT-OR-USER avatar.
CHEIGHT specifies avatar height in chars, default is 2."
  ;; NOTE:
  ;; - For CHEIGHT==1 align avatar at vertical center
  ;; - For CHEIGHT==2 make svg height to be 3 chars, so if font size
  ;;   is increased, there will be no gap between two slices
  (unless cheight (setq cheight 2))
  (let* ((photofile (telega--tl-get file :local :path))
         (factors (alist-get cheight telega-avatar-factors-alist))
         (cfactor (or (car factors) 0.9))
         (mfactor (or (cdr factors) 0.1))
         (xh (telega-chars-xheight cheight))
         (margin (* mfactor xh))
         (ch (* cfactor xh))
         (cfull (+ ch margin))
         (aw-chars (telega-chars-in-width ch))
         (aw-chars-3 (if (> aw-chars 3) (- aw-chars 3) 0))
         (svg-xw (telega-chars-xwidth aw-chars))
         (svg-xh (cond ((= cheight 1) cfull)
                       ((= cheight 2) (+ cfull (telega-chars-xheight 1)))
                       (t xh)))
         (svg (telega-svg-create svg-xw svg-xh))
         (name (if (eq (telega--tl-type chat-or-user) 'user)
                   (telega-user--name chat-or-user)
                 (telega-chat-title chat-or-user))))
    (if (telega-file-exists-p photofile)
        (let ((img-type (image-type-from-file-name photofile))
              (clip (telega-svg-clip-path svg "clip")))
          (svg-circle clip (/ svg-xw 2) (/ cfull 2) (/ ch 2))
          (svg-embed svg photofile
                     (format "image/%S" img-type)
                     nil
                     :x (/ (- svg-xw ch) 2) :y (/ margin 2)
                     :width ch :height ch
                     :clip-path "url(#clip)"))

      ;; Draw initials
      (let ((fsz (/ ch 2))
            (colors (if (eq (telega--tl-type chat-or-user) 'user)
                        (telega-user-color chat-or-user)
                      (telega-chat-color chat-or-user))))
        (svg-gradient svg "cgrad" 'linear
                      (list (cons 0 (telega-color-name-as-hex-2digits
                                     (or (nth 1 colors) "gray75")))
                            (cons ch (telega-color-name-as-hex-2digits
                                      (or (nth 0 colors) "gray25")))))
        (svg-circle svg (/ svg-xw 2) (/ cfull 2) (/ ch 2) :gradient "cgrad")
        (svg-text svg (substring name 0 1)
                  :font-size (/ ch 2)
                  :font-weight "bold"
                  :fill "white"
                  :font-family "monospace"
                  ;; XXX insane X/Y calculation
                  :x (- (/ svg-xw 2) (/ fsz 3))
                  :y (+ (/ fsz 3) (/ cfull 2)))))

    (telega-svg-image svg :scale 1.0
                      :width svg-xw :height svg-xh
                      :ascent 'center
                      :mask 'heuristic
                      ;; Correct text for tty-only avatar display
                      :telega-text
                      (cons (concat "(" (substring name 0 1) ")"
                                    (make-string aw-chars-3 ?\u00A0))
                            (mapcar (lambda (_ignore)
                                      (make-string (+ 3 aw-chars-3) ?\u00A0))
                                    (make-list (1- cheight) 'not-used))))
    ))

(defun telega-avatar--create-image-one-line (chat-or-user file)
  "Avatar creator for one line use."
  (telega-avatar--create-image chat-or-user file 1))

(defun telega-symbol-emojify (emoji &optional image-file)
  "Attach `display' property with emoji svg to EMOJI string.
Typical usage is to emojify `telega-symbol-XXX' values.
Like (telega-symbol-emojify telega-symbol-pin).
Optionally IMAGE-FILE could be used."
  (let ((image (if image-file
                   (create-image image-file nil nil
                                 :scale 1.0 :ascent 'center
                                 :mask 'heuristic
                                 :width (telega-chars-xwidth
                                         (string-width emoji)))
                 (telega-emoji-create-svg emoji))))
    (add-text-properties 0 (length emoji)
                         (list 'rear-nonsticky '(display)
                               'display image)
                         emoji)
    emoji))


;; Location
(defun telega-map--create-image (map &optional _file)
  "Create map image for location MAP."
  (let* ((map-photo (telega-file--renew map :photo))
         (map-photofile (when map-photo
                          (telega--tl-get map-photo :local :path)))
         (_map-loc (plist-get map :map-location))
         (_user-loc (plist-get map :user-location))
         (width (plist-get map :width))
         (height (plist-get map :height))
         (svg (telega-svg-create width height)))
    (if (and (telega-file--downloaded-p map-photo)
             (telega-file-exists-p map-photofile))
        (svg-embed svg map-photofile "image/png" nil
                   :x 0 :y 0 :width width :height height)
      (svg-rectangle svg 0 0 width height
                     :fill-color (apply 'color-rgb-to-hex
                                        (color-name-to-rgb
                                         (face-foreground 'shadow)))))

    ;; Show user's avatar
    ;; TODO: calculate avatar possition according to
    ;;       map-loc/user-loc, they can differ
    ;;
    ;; TODO: show other users close enough to `:user-id'
    (when-let* ((user-id (plist-get map :user-id))
                (user (unless (zerop user-id)
                        (telega-user--get user-id)))
                (user-photo (when user
                              (telega--tl-get user :profile_photo :small))))
      (when (telega-file--downloaded-p user-photo)
        (let* ((photofile (telega--tl-get user-photo :local :path))
               (img-type (image-type-from-file-name photofile))
               (clip (telega-svg-clip-path svg "user-clip"))
               (sz (/ (plist-get map :height) 8))
               (sz2 (/ sz 2)))
          (svg-circle clip (+ (/ width 2) sz2) (- (/ height 2) sz2) sz2)
          (svg-polygon clip (list (cons (/ width 2) (/ height 2))
                                  (cons (+ (/ width 2) (/ sz2 4))
                                        (- (/ height 2) sz2))
                                  (cons (+ (/ width 2) sz2)
                                        (- (/ height 2) (/ sz2 4)))))
          (svg-embed svg photofile (format "image/%S" img-type) nil
                     :x (/ width 2) :y (- (/ height 2) sz)
                     :width sz :height sz
                     :clip-path "url(#user-clip)"))))

    (svg-circle svg (/ width 2) (/ height 2) 8
                :stroke-width 4
                :stroke-color "white"
                :fill-color (face-foreground 'telega-blue))

    (svg-image svg :scale 1.0
               :width width :height height
               :ascent 'center)))

(defun telega-map--get-thumbnail-file (map loc &optional msg)
  "Request MAP image at LOC location for MSG.
Update `:svg-image' when new image is received."
  (telega--getMapThumbnailFile
      loc (plist-get map :zoom)
      (plist-get map :width) (plist-get map :height)
      (plist-get map :scale) (when msg (telega-msg-chat msg))
    (lambda (map-file)
      (plist-put map :map-location loc)
      (plist-put map :photo map-file)

      (telega-file--download map-file 32
        (lambda (mfile)
          (when (telega-file--downloaded-p mfile)
            (let ((svg-image (plist-get map :svg-image))
                  (new-image (telega-map--create-image map mfile)))
              (setcdr svg-image (cdr new-image))
              (force-window-update)))
          (when msg
            (telega-msg-redisplay msg))
          )))))

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
    (not (= old-zoom new-zoom))))

(provide 'telega-media)

;;; telega-media.el ends here

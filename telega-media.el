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
(require 'telega-server)

(defvar telega-emoji-svg-images nil
  "Cache of SVG images for emoji.
Alist with elements in form (emoji . image)")

;;; Files downloading/uploading
(defun telega--getFile (file-id &optional callback)
  (declare (indent 1))
  (telega-server--call
   (list :@type "getFile"
         :file_id file-id)
   callback))

(defun telega--downloadFile (file-id &optional priority)
  "Asynchronously downloads a file by its FILE-ID from the cloud.
`telega--on-updateFile' will be called to notify about the
download progress and successful completion of the download.
PRIORITY is integer in range 1-32 (higher downloads faster), default is 1."
  ;; We use `telega-server--send' instead of `telega-server--call' to
  ;; not block waiting for result.  We catch the result with
  ;; `telega--on-file'
  (telega-server--send
   (list :@type "downloadFile"
         :file_id file-id
         :priority (or priority 1))))

(defun telega--cancelDownloadFile (file-id &optional only-if-pending)
  "Stop downloading the file denoted by FILE-ID.
If ONLY-IF-PENDING is non-nil then stop downloading only if it
hasn't been started, i.e. request hasn't been sent to server."
  (telega-server--send
   (list :@type "cancelDownloadFile"
         :file_id file-id
         :only_if_pending (or only-if-pending :false)))

  (let ((callbacks (gethash file-id telega--downloadings)))
    (unwind-protect
        (telega-file--run-callbacks callbacks (telega--getFile file-id))
      (remhash file-id telega--downloadings))))

(defun telega--deleteFile (file-id)
  "Delete file from cache."
  (telega-server--send
   (list :@type "deleteFile"
         :file_id file-id)))

(defun telega--on-file (file)
  "FILE has been updated."
  (let* ((file-id (plist-get file :id))
         (dl-callbacks (gethash file-id telega--downloadings))
         (ul-callbacks (gethash file-id telega--uploadings)))
    (unwind-protect
        (progn
          (telega-file--run-callbacks dl-callbacks file)
          (telega-file--run-callbacks ul-callbacks file))

      (when (telega-file--downloaded-p file)
        (when (and telega-debug (gethash file-id telega--downloadings))
          (let ((msg (format "Downloading completed: %s (size=%s)"
                             (telega-short-filename
                              (telega--tl-get file :local :path))
                             (file-size-human-readable
                              (telega--tl-get file :local :downloaded_size)))))
            (telega-debug msg)
            (message msg)))
        (remhash file-id telega--downloadings))

      (when (telega-file--uploaded-p file)
        ;; Check for `telega-file--downloaded-p' so path info is available
        (when (and telega-debug (telega-file--downloaded-p file))
          (let ((msg (format "Uploading completed: %s (size=%s)"
                             (telega-short-filename
                              (telega--tl-get file :local :path))
                             (file-size-human-readable
                              (telega--tl-get file :local :downloaded_size)))))
            (telega-debug msg)
            (message msg)))
        (remhash file-id telega--uploadings))
      )))

(defun telega--on-updateFile (event)
  "File has been updated, call all the associated hooks."
  (telega--on-file (plist-get event :file)))

(defun telega-file--update-place (file place prop)
  (plist-put place prop file))

(defsubst telega-file--downloaded-p (file)
  "Return non-nil if FILE has been downloaded."
  (telega--tl-get file :local :is_downloading_completed))

(defsubst telega-file--downloading-p (file)
  "Return non-nil if FILE is downloading right now."
  (telega--tl-get file :local :is_downloading_active))

(defsubst telega-file--downloading-progress (file)
  "Return progress of file downloading as float from 0 to 1."
  ;; NOTE: fsize is 0 if unknown, in this case esize is approximate
  ;; size
  (let ((fsize (plist-get file :size))
        (esize (plist-get file :expected_size))
        (dsize (telega--tl-get file :local :downloaded_size)))
    (color-clamp (/ (float dsize) (if (zerop fsize) esize fsize)))))

(defsubst telega-file--need-download-p (file)
  (and (telega--tl-get file :local :can_be_downloaded)
       (not (telega-file--downloaded-p file))))
;       (not (telega-file--downloading-p file))))

(defun telega-file--run-callbacks (callbacks file)
  "Run CALLBACKS on FILE update."
  (dolist (cb-with-args callbacks)
    (apply (car cb-with-args) file (cdr cb-with-args))))

(defun telega-file--download-monitor-progress (file-id cb &rest cb-args)
  "Start monitoring downloading progress for FILE-ID.
CB and CB-ARGS denotes callback to call.
First argument to callback is file, and only then CB-ARGS are supplied."
  (declare (indent 1))
  (let ((callbacks (gethash file-id telega--downloadings))
        (new-callback (cons cb cb-args)))
    (unless (member new-callback callbacks)
      (puthash file-id (append callbacks (list new-callback))
               telega--downloadings))
    ))

(defun telega-file--download-monitoring (place prop &optional priority
                                               &rest callback-spec)
  "Download file denoted by PLACE and PROP.
PLACE is the plist where its PROP is a file to download.
File is monitored so PLACE's PROP is updated on file updates."
  ;; - If file already downloaded, then just call the callback
  ;; - If file already downloading, then just install the callback
  ;; - If file can be downloaded, install the callback and download
  ;;   the file
  (let* ((file (plist-get place prop))
         (file-id (plist-get file :id)))
    (cond ((telega-file--downloaded-p file)
           (when callback-spec
             (apply (car callback-spec) file (cdr callback-spec))))

          ((telega-file--downloading-p file)
           (when callback-spec
             (apply 'telega-file--download-monitor-progress
                    file-id callback-spec)))

          ((telega--tl-get file :local :can_be_downloaded)
           (telega-file--download-monitor-progress
               file-id 'telega-file--update-place place prop)
           (when callback-spec
             (apply 'telega-file--download-monitor-progress
                    file-id callback-spec))
           (telega--downloadFile file-id priority)))))

(defun telega--uploadFile (filename &optional file-type priority)
  "Asynchronously upload file denoted by FILENAME.
FILE-TYPE is one of `photo', `animation', etc
PRIORITY is same as for `telega-file--download'."
  (telega-server--call
   (list :@type "uploadFile"
         :file (list :@type "inputFileLocal" :path filename)
         :file_type (list :@type (format "fileType%S" (or file-type 'Unknown)))
         :priority (or priority 1))))

(defun telega--cancelUploadFile (file-id)
  "Stop uploading file denoted by FILE-ID."
  (telega-server--send
   (list :@type "cancelUploadFile"
         :file_id file-id))

  (let ((callbacks (gethash file-id telega--uploadings)))
    (unwind-protect
        (telega-file--run-callbacks callbacks (telega--getFile file-id))
      (remhash file-id telega--uploadings))))

(defsubst telega-file--uploaded-p (file)
  "Return non-nil if FILE has been uploaded."
  (telega--tl-get file :remote :is_uploading_completed))

(defsubst telega-file--uploading-p (file)
  "Return non-nil if FILE is uploading right now."
  (telega--tl-get file :remote :is_uploading_active))

(defun telega-file--upload-monitor-progress (file-id cb &rest cb-args)
  "Start monitoring uploading progress for FILE-ID.
CB and CB-ARGS denotes callback to call.
First argument to callback is file, and only then CB-ARGS are supplied."
  (let ((callbacks (gethash file-id telega--uploadings))
        (new-callback (cons cb cb-args)))
    (unless (member new-callback callbacks)
      (puthash file-id (append callbacks (list new-callback))
               telega--uploadings))))

(defun telega-file--upload-monitoring (place prop &rest callback-spec)
  "Upload file denoted by PLACE and PROP.
PLACE is the plist where its PROP is a file to download.
File is monitored so PLACE's PROP is updated on file updates."
  (let* ((file (plist-get place prop))
         (file-id (plist-get file :id)))
    (when (plist-get :can_be_downloaded)
      (telega-file--upload-monitor-progress
       file-id 'telega-file--update-place place prop)
      (when callback-spec
        (apply 'telega-file--upload-monitor-progress
               file-id callback-spec))
      (telega-file--upload file-id))))


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

(defun telega-photo--lowres (photo)
  "Return lowres thumbnail for the PHOTO.
Lowres is always goes first."
  (aref (plist-get photo :sizes) 0))

(defun telega-photo--highres (photo)
  "Return thumbnail of highest resolution for the PHOTO."
  (let ((photo-sizes (plist-get photo :sizes)))
    (aref photo-sizes (1- (length photo-sizes)))))

(defun telega-photo--best (photo &optional fill-column)
  "Select best thumbnail size for the PHOTO.
If FILL-COLUMN is specified, then select best thumbnail to fit
into FILL-COLUMN."
  ;; NOTE: `reverse' is used to start from highes sizes
  (let ((photo-sizes (reverse (plist-get photo :sizes))))
    (or (cl-some (lambda (tn)
                   (and (telega-file--downloaded-p (plist-get tn :photo)) tn))
                 photo-sizes)
        (cl-some (lambda (tn)
                   (and (telega-file--downloading-p (plist-get tn :photo)) tn))
                 photo-sizes)
        (if (not fill-column)
            (aref photo-sizes 0)

          ;; Choose best suiting fill-column
          (let ((xwidth (telega-chars-width fill-column))
                (best (aref photo-sizes 0)))
            (dolist (tn photo-sizes)
              (when (< (abs (- (plist-get tn :width) xwidth))
                       (abs (- (plist-get best :width) xwidth)))
                (setq best tn)))
            best)))))

(defun telega-photo-file-format (file &optional one-line-p &rest image-props)
  "Create propertized text displaying image at PATH.
If ONE-LINE-P is non-nil then create image for inlining.
IMAGE-PROPS are passed directly to `create-image'."
  (let* ((file-path (telega--tl-get file :local :path))
         (photo-img (apply #'create-image file-path 'imagemagick nil
                           :scale 1 image-props))
         (xframe (telega-x-frame))
         (img-char-width (if xframe
                             (round (car (image-size photo-img nil xframe)))
                           0)))
    (when one-line-p
      (plist-put (cdr photo-img) :ascent 'center)
      (when xframe
        (plist-put (cdr photo-img) :height (frame-char-height xframe))))

    (propertize
     (concat (substring (concat "<Photo" (make-string img-char-width ?X))
                        0 img-char-width)
             ">")
     'display photo-img)))

(defun telega-fmt--photo (photo &optional one-line-p)
  "Format best thumbnail for the PHOTO."
  (let* ((thumb (telega-photo--best photo))
         (thumb-file (plist-get thumb :photo))
         (xframe (telega-x-frame)))
    (apply #'telega-thumbnail-file-format
           thumb-file
           one-line-p
           telega-msg-photo-props)))


;;; Stickers

;;; Animations

;;; Web pages
(defun telega-web-page-format (web-page &optional prefix)
  "Format WEB-PAGE.
Prefix every line with PREFIX."
  (cl-assert web-page)

  (unless prefix (setq prefix ""))
  (concat
   (let ((title (plist-get web-page :title)))
     (unless (string-empty-p title)
       (telega-fmt-eval
        `(,prefix (identity :fill left
                            :fill-prefix ,prefix
                            :fill-column ,(- fill-column 10)
                            :face bold)
                  "\n")
        title)))
   (let ((desc (plist-get web-page :description)))
     (unless (string-empty-p desc)
       (telega-fmt-eval
        `(,prefix (identity :fill left
                            :fill-prefix ,prefix
                            :fill-column ,(- fill-column 10))
                  "\n")
        desc)))
   (let ((photo (plist-get web-page :description)))
     (when photo
       (concat prefix
               (telega-thumbnail-format (plist-get web-page :photo))
               "\n")))
   (cl-case (intern (plist-get web-page :type))
     (photo
      ;; no-op, already displayed above
      )
     (article
      ;; nothing to display
      )
     (t (concat prefix "<unsupported webPage:"
                (plist-get web-page :type) ">"
                "\n")))
   ))


;;; Auto-downloading media
(defun telega-media--autodownload-on-chat (chat)
  "Autodownload CHAT's avatar."
  (let* ((photo (plist-get user :photo))
         (photo-file (plist-get photo :small)))
    (when (and (telega-file--need-download-p photo-file)
               (not (telega-file--downloading-p photo-file)))
      (telega-file--download-monitoring photo :small 32))))

(defun telega-media--autodownload-on-user (user)
  "Autodownload USER's profile avatar."
  (let* ((photo (plist-get user :profile_photo))
         (photo-file (plist-get photo :small)))
    (when (and (telega-file--need-download-p photo-file)
               (not (telega-file--downloading-p photo-file)))
      (telega-file--download-monitoring photo :small 32))))

(defun telega-media--autodownload-on-msg (msg disable-notification)
  "Autodownload media in MSG according to `telega-auto-download'.
Always download \"s\" type (for one-line reply/edit formatting).
Downloads highres photos according to `telega-auto-download'."
  (let ((chat (telega-chat-get (plist-get msg :chat_id)))
        (content (plist-get msg :content)))
    (cl-case (telega--tl-type content)
      (messagePhoto
       (let* ((photo (plist-get content :photo))
              (lowres (telega-photo--lowres photo))
              (lowres-file (plist-get lowres :photo))
              (highres (telega-photo--highres photo))
              (highres-file (plist-get highres :photo)))

         ;; Always download lowres files
         (cl-assert lowres-file)
         (when (telega-file--need-download-p lowres-file)
           (telega-debug "Autodownload LOWRES: %S" lowres-file)
           (telega-file--download-monitoring lowres :photo 32))

         (cl-assert highres-file)
         (when (and (telega-file--need-download-p highres-file)
                    (telega-filter-chats
                     (alist-get 'photos telega-auto-download) (list chat)))
           (telega-debug "Autodownload HIGH-RES: %S" highres-file)
           (telega-file--download-monitoring highres :photo 5))))
      ;; TODO
      (messageVideo
       )
      (messageDocument
       )
      )))

;;;###autoload
(defun telega-media-auto-download-mode (&optional arg)
  "Toggle automatic media download for incoming messages.
With positive ARG - enables automatic downloads, otherwise disables.
To customize automatic downloads, use `telega-auto-download'."
  (interactive "p")
  (if (> arg 0)
      (progn
        (add-hook 'telega-user-update-hook 'telega-media--autodownload-on-user)
        (add-hook 'telega-chat-created-hook 'telega-media--autodownload-on-chat)
        (add-hook 'telega-chat-pre-message-hook 'telega-media--autodownload-on-msg))

    (remove-hook 'telega-chat-pre-message-hook 'telega-media--autodownload-on-msg)
    (remove-hook 'telega-chat-created-hook 'telega-media--autodownload-on-chat)
    (remove-hook 'telega-user-update-hook 'telega-media--autodownload-on-user)))


;; Avatars
(defun telega-media--cwidth-xmargin (width height char-height)
  "Calculate width in chars and margin X pixels.
Return cons cell, where car is width in char and cdr is margin value."
  (let* ((pix-h (* (frame-char-height) char-height))
         (pix-w (* (/ (float width) height) pix-h))
         (cw (telega-chars-in-width pix-w))
         (xmargin (/ (- (telega-chars-width cw) pix-w) 2)))
    (cl-assert (> cw 0))
    (cons cw (floor xmargin))))

(defun telega-thumb--create-image (thumb &optional file cheight)
  "Create image for the thumbnail THUMB.
CHEIGHT is the height in chars (default=1)."
  (unless file
    (setq file (plist-get thumb :photo)))
  (unless cheight
    (setq cheight 1))
  (let ((cwidth-xmargin (telega-media--cwidth-xmargin
                         (plist-get thumb :width)
                         (plist-get thumb :height)
                         cheight)))
    (create-image (telega--tl-get file :local :path)
                  'imagemagick nil
                  :height (* cheight (frame-char-height (telega-x-frame)))
                  :scale 1.0
                  :ascent 'center
                  :margin (cons (cdr cwidth-xmargin) 0)
                  :telega-text (make-string (car cwidth-xmargin) ?X))))

(defun telega-thumb--create-image-one-line (thumb &optional file)
  "Create image for thumbnail (photoSize) for one line use."
  (telega-thumb--create-image thumb file 1))

(defun telega-thumb--create-image-as-is (thumb &optional file)
  "Create image for thumbnail THUMB (photoSize) with size as is."
  (telega-thumb--create-image
   thumb file (telega-chars-in-height (plist-get thumb :height))))

(defun telega-media--image-update (obj-spec file)
  "Called to update the image contents for the OBJ-SPEC.
OBJ-SPEC is cons of object and create image function.
Create image function accepts two arguments - object and FILE.
Return updated image, cached or created with create image function."
  (let ((cached-image (plist-get (car obj-spec) :telega-image))
        (simage (funcall (cdr obj-spec) (car obj-spec) file)))
    (unless (equal cached-image simage)
      ;; Update the image
      (if cached-image
          (setcdr cached-image (cdr simage))
        (setq cached-image simage))
      (plist-put (car obj-spec) :telega-image cached-image))
    cached-image))

(defun telega-media--image-download-monitor (file obj-spec)
  (cl-assert (plist-get (car obj-spec) :telega-image))
  (telega-media--image-update obj-spec file)
  (force-window-update))

(defun telega-media--image (obj-spec file-spec &optional force-update)
  "Return image for media object specified by OBJ-SPEC.
File is specified with FILE-SPEC."
  (let ((cached-image (plist-get (car obj-spec) :telega-image)))
    (when (or force-update (not cached-image))
      (let ((media-file (plist-get (car file-spec) (cdr file-spec))))
        ;; First time image is created or update is forced
        (setq cached-image
              (telega-media--image-update obj-spec media-file))

        ;; Possible initiate file downloading
        (when (or (telega-file--need-download-p media-file)
                  (telega-file--downloading-p media-file))
          (telega-file--download-monitoring
           (car file-spec) (cdr file-spec) nil
           'telega-media--image-download-monitor obj-spec))))
    cached-image))

(defun telega-avatar--create-image (chat-or-user file)
  "Create image for CHAT-OR-USER avatar."
  (let* ((photofile (telega--tl-get file :local :path))
         (cfactor (or (car telega-avatar-factors) 0.9))
         (mfactor (or (cdr telega-avatar-factors) 0.1))
         (xh (* 2 (frame-char-height (telega-x-frame))))
         (margin (* mfactor xh))
         (ch (* cfactor xh))
         (cfull (+ ch margin))
         (aw-chars (telega-chars-in-width cfull))
         (xw (telega-chars-width aw-chars))
         (svg (svg-create xw xh)))
    (if (telega-file-exists-p photofile)
        (let ((file-ext (downcase (file-name-extension photofile)))
              (clip (telega-svg-clip-path svg "clip")))
          (svg-circle clip (/ xw 2) (/ cfull 2) (/ ch 2))
          (svg-embed svg photofile
                     (if (string= file-ext "png") "image/png" "image/jpeg")
                     nil
                     :x (/ (- xw ch) 2) :y (/ margin 2)
                     :width ch :height ch
                     :clip-path "url(#clip)"))

      ;; Draw initials
      (let ((fsz (/ ch 2))
            color name)
        (if (eq (telega--tl-type chat-or-user) 'user)
            (setq color (telega-user-color chat-or-user)
                  name (telega-user--name chat-or-user))
          (setq color (telega-chat-color chat-or-user)
                name (telega-chat-title chat-or-user)))

        (svg-gradient svg "cgrad" 'linear
                      (list (cons 0 (cadr color)) (cons ch (caddr color))))
        (svg-circle svg (/ xw 2) (/ cfull 2) (/ ch 2) :gradient "cgrad")
        (svg-text svg (substring name 0 1)
                  :font-size (/ ch 2)
                  :font-weight "bold"
                  :fill "white"
                  :font-family "monospace"
                  ;; XXX insane X/Y calculation
                  :x (- (/ xw 2) (/ fsz 3))
                  :y (+ (/ fsz 3) (/ cfull 2)))))

    (svg-image svg :scale 1.0
               :ascent 'center
               :mask 'heuristic
               :width xw :height xh
               ;; text of correct width
               :telega-text (make-string aw-chars ?X))))

(defun telega-media--emoji-image (emoji)
  "Create svg image for the EMOJI."
  (let ((image (assoc emoji telega-emoji-svg-images)))
    (unless image
      (let* ((xframe (telega-x-frame))
             (xh (frame-char-height xframe))
             (font-size (- xh (/ xh 4)))
             (aw-chars (telega-chars-in-width font-size))
             (xw (telega-chars-width aw-chars))
             (svg (svg-create xw xh)))
        (svg-text svg (substring emoji 0 1)
                  :font-size font-size
                  :x 0 :y font-size)
        (setq image (svg-image svg :scale 1.0
                               :ascent 'center
                               :mask 'heuristic
                               :width xw :height xh
                               :telega-text (make-string aw-chars ?E)))))
    image))

(defun telega-symbol-emojify (emoji)
  "Attach `display' property with emoji svg to EMOJI string.
Typical usage is to emojify `telega-symbol-XXX' values.
Like (telega-symbol-emojify telega-symbol-pin).
EMOJY must be single char string."
  (cl-assert (= (length emoji) 1))
  (add-text-properties 0 1 (list 'rear-nonsticky '(display)
                                 'display (telega-media--emoji-image emoji))
                       emoji))

(provide 'telega-media)

;;; telega-media.el ends here

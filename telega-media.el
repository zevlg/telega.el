;;; telega-media.el --- Media support for telega

;; Copyright (C) 2018 by Zajcev Evgeny.

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


;;; Files downloading/uploading
(defun telega--getFile (file-id)
  (telega-server--call
   (list :@type "getFile" :file_id file-id)))

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

(defun telega-file--run-callbacks (callbacks file)
  "Run CALLBACKS on FILE update."
  (cl-dolist (cb-with-args callbacks)
    (apply (car cb-with-args) file (cdr cb-with-args))))

(defun telega-file--download-monitor-progress (file-id cb &rest cb-args)
  "Start monitoring downloading progress for FILE-ID.
CB and CB-ARGS denotes callback to call.
First argument to callback is file, and only then CB-ARGS are supplied."
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
  (let* ((file (plist-get place prop))
         (file-id (plist-get file :id)))
    (when (plist-get (plist-get file :local) :can_be_downloaded)
      (telega-file--download-monitor-progress
       file-id 'telega-file--update-place place prop)
      (when callback-spec
        (apply 'telega-file--download-monitor-progress
               file-id callback-spec))
      (telega--downloadFile file-id priority))))

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
          (let ((xwidth (telega-pixel-width fill-column))
                (best (aref photo-sizes 0)))
            (cl-dolist (tn photo-sizes)
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
(defmacro telega-media--need-download-p (file)
  `(and ,file
        (not (telega-file--downloaded-p ,file))
        (not (telega-file--downloading-p ,file))))

(defun telega-media--autodownload-on-user (user)
  "Autodownload USER's profile avatar."
  (let* ((photo (plist-get user :profile_photo))
         (photo-file (plist-get photo :small)))
    (when (telega-media--need-download-p photo-file)
      (telega-file--download-monitoring photo :small 32))))

(defun telega-media--autodownload-on-msg (msg disable-notification)
  "Autodownload media in MSG according to `telega-auto-download'.
Always download \"s\" type (for one-line reply/edit formatting).
Downloads highres photos according to `telega-auto-download'."
  (let ((chat (telega-chat--get (plist-get msg :chat_id)))
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
         (when (telega-media--need-download-p lowres-file)
           (telega-debug "Autodownload LOWRES: %S" lowres-file)
           (telega-file--download-monitoring lowres :photo 32))

         (cl-assert highres-file)
         (when (and (telega-media--need-download-p highres-file)
                    (telega-filter-chats
                     (cdr (assq 'photos telega-auto-download)) (list chat)))
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
      (add-hook 'telega-chat-pre-message-hook 'telega-media--autodownload-on-msg)
    (remove-hook 'telega-chat-pre-message-hook 'telega-media--autodownload-on-msg)))

(provide 'telega-media)

;;; telega-media.el ends here

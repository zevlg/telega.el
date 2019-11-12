;;; telega-sticker.el --- Stickers for the telega  -*- lexical-binding:t -*-

;; Copyright (C) 2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Feb  8 21:08:24 2019
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

;;

;;; Code:
(require 'seq)                          ;seq-doseq

(require 'telega-core)
(require 'telega-util)
(require 'telega-media)

;; shutup compiler
(defvar ido-matches)
(defvar ivy--index)
(defvar ivy--old-cands)

(declare-function telega-chatbuf-sticker-insert "telega-chat" (sticker))
(declare-function telega-chatbuf-animation-insert "telega-chat" (animation))


(defvar telega-help-win--emoji nil
  "Emoji for which help window is displayed.")
(make-variable-buffer-local 'telega-help-win--emoji)
(defvar telega-help-win--stickerset nil
  "Stickerset for which help window is displayed.")
(make-variable-buffer-local 'telega-help-win--stickerset)

(defvar telega-sticker--use-thumbnail nil
  "Bind this variable to non-nil to use thumbnail instead of image.
Thumbnail is a smaller (and faster) version of sticker image.")

(defvar telega-sticker-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "f") 'telega-sticker-toggle-favorite)
    (define-key map (kbd "*") 'telega-sticker-toggle-favorite)
    (define-key map (kbd "i") 'telega-sticker-help)
    (define-key map (kbd "h") 'telega-sticker-help)
    map))

(define-button-type 'telega-sticker
  :supertype 'telega
  :inserter 'telega-ins--sticker-image
;  'read-only t
  'keymap telega-sticker-button-map)

(defun telega-sticker-at (&optional pos)
  "Retur sticker at POS."
  (let ((button (button-at (or pos (point)))))
    (when (and button (eq (button-type button) 'telega-sticker))
      (button-get button :value))))

(defsubst telega-sticker--file (sticker)
  "Return STICKER's file."
  (telega-file--renew sticker :sticker))

(defsubst telega-sticker--thumb-file (sticker)
  "Return STICKER's thumbnail file."
  (let ((thumb (plist-get sticker :thumbnail)))
    (telega-file--renew thumb :photo)))

(defsubst telega-sticker-favorite-p (sticker)
  "Return non-nil if STICKER is in favorite list."
  (memq (telega--tl-get sticker :sticker :id) telega--stickers-favorite))

(defun telega-sticker--download (sticker)
  "Ensure STICKER data is downloaded."
  (let ((sticker-file (telega-sticker--file sticker))
        (thumb-file (telega-sticker--thumb-file sticker)))
    (when (telega-file--need-download-p thumb-file)
      (telega-file--download thumb-file 6))
    (unless telega-sticker--use-thumbnail
      (when (telega-file--need-download-p sticker-file)
        (telega-file--download sticker-file 2)))
    ))

(defun telega-stickerset--download (sset)
  "Ensure sticker set SSET data is downloaded."
  (mapc 'telega-sticker--download (plist-get sset :stickers)))

(defun telega--getStickerSet (set-id &optional callback)
  (telega-server--call
   (list :@type "getStickerSet"
         :set_id set-id)
   callback))

(defun telega-stickerset--ensure (sset)
  "Ensure sticker set SSET is put into `telega--stickersets'."
  (setf (alist-get (plist-get sset :id) telega--stickersets nil nil 'equal)
        sset)
  (when telega-sticker-set-download
    (telega-stickerset--download sset))
  sset)

(defun telega-stickerset-get (set-id &optional async-p)
  (let ((sset (cdr (assoc set-id telega--stickersets))))
    (unless sset
      (if async-p
          (telega--getStickerSet set-id 'telega-stickerset--ensure)

        (setq sset (telega--getStickerSet set-id))
        (telega-stickerset--ensure sset)))
    sset))

(defun telega-stickerset-installed-p (sset)
  "Return non-nil if sticker set SSET is installed."
  (member (plist-get sset :id) telega--stickersets-installed-ids))

(defun telega--on-updateInstalledStickerSets (event)
  "The list of installed sticker sets was updated."
  (if (plist-get event :is_masks)
      (telega-debug "TODO: `telega--on-updateInstalledStickerSets' is_mask=True")

    (setq telega--stickersets-installed-ids
          (append (plist-get event :sticker_set_ids) nil))
    ;; Asynchronously fetch sticker sets
    ;; NOTE: this is too heavy, when `telega-emoji-use-images' is
    ;; enabled, avoid doing so at start time
    ;; (dolist (set-id telega--stickersets-installed-ids)
    ;;   (telega-stickerset-get set-id 'async))
    ))

(defun telega--on-updateTrendingStickerSets (event)
  "The list of trending sticker sets was updated or some of them were viewed."
  (let ((ssets-info (telega--tl-get event :sticker_sets :sets)))
    (setq telega--stickersets-trending
          (append ssets-info nil))))

(defun telega--on-updateRecentStickers (event)
  "Recent stickers has been updated."
  ;; NOTE: attached recent stickers are not supported
  (unless (plist-get event :is_attached)
    (setq telega--stickers-recent
          (append (plist-get event :sticker_ids) nil))
    ;; Asynchronously download corresponding files
;    (mapc 'telega--downloadFile telega--stickers-recent)
    ))

(defun telega--on-updateFavoriteStickers (event)
  "Favorite stickers has been updated."
  (setq telega--stickers-favorite
        (append (plist-get event :sticker_ids) nil))
  ;; Asynchronously download corresponding files
;  (mapc 'telega--downloadFile telega--stickers-favorite)
  )

;; TODO: generalize this on telega-server level
(defun telega-stickers--async-call (call-params &optional callback)
  "Perform call to telega-server with CALL-PARAMS.
Then `:stickers' property is extracted from the reply and passed
either to CALLBACK or returned."
  (let ((reply (telega-server--call
                call-params
                (when callback
                  (lambda (server-reply)
                    (funcall callback
                             (append (plist-get server-reply :stickers) nil)))))))
    (if callback
        reply
      (append (plist-get reply :stickers) nil))))
  
(defun telega--getStickers (emoji &optional limit callback)
  "Return installed stickers that correspond to a given EMOJI.
LIMIT defaults to 20."
  (declare (indent 2))
  (telega-stickers--async-call
   (list :@type "getStickers"
         :emoji emoji
         :limit (or limit 20))
   callback))

(defun telega--searchStickers (emoji &optional limit callback)
  "Search for the public stickers that correspond to a given EMOJI.
LIMIT defaults to 20."
  (declare (indent 2))
  (telega-stickers--async-call
   (list :@type "searchStickers"
         :emoji emoji
         :limit (or limit 20))
   callback))

(defun telega--getInstalledStickerSets (&optional masks-p)
  "Returns a list of installed sticker sets."
  (cl-assert (not masks-p) t "installed masks not yet supported")

  (let ((reply (telega-server--call
                (list :@type "getInstalledStickerSets"
                      :is_masks (or masks-p :false)))))
    (append (plist-get reply :sets) nil)))

(defun telega--changeStickerSet (stickerset install-p &optional archive-p)
  "Install/Uninstall STICKERSET."
  (telega-server--call
   (list :@type "changeStickerSet"
         :set_id (plist-get stickerset :id)
         :is_installed (or install-p :false)
         :is_archived (or archive-p :false))))

(defun telega--getTrendingStickerSets ()
  "Returns a list of trending sticker sets."
  (let ((reply (telega-server--call
                (list :@type "getTrendingStickerSets"))))
    (append (plist-get reply :sets) nil)))

(defun telega--getAttachedStickerSets (file-id)
  "Return sticker sets attached to the FILE-ID.
Photo and Video files have attached sticker sets."
  (telega-server--call
   (list :@type "getAttachedStickerSets"
         :file_id file-id)))

(defun telega--searchStickerSet (name)
  "Search for sticker set by NAME."
  (telega-server--call
   (list :@type "searchStickerSet"
         :name name)))

(defun telega--searchInstalledStickerSets (query &optional masks-p limit)
  "Searches for installed sticker sets by QUERY."
  (telega-server--call
   (list :@type "searchInstalledStickerSets"
         :is_masks (or masks-p :false)
         :query query
         :limit (or limit 20))))

(defun telega--searchStickerSets (query)
  "Searches for ordinary sticker sets by looking for specified QUERY."
  (let ((reply (telega-server--call
                (list :@type "searchStickerSets"
                      :query query))))
    (append (plist-get reply :sets) nil)))

(defun telega--viewTrendingStickerSets (set-id &rest other-ids)
  (telega-server--call
   (list :@type "viewTrendingStickerSets"
         :sticker_set_ids (apply 'vector set-id other-ids))))

(defun telega--getRecentStickers (&optional attached-p callback)
  "Returns a list of recently used stickers.
Pass non-nil ATTACHED-P to return only stickers attached to photos/videos."
  (declare (indent 1))
  (telega-stickers--async-call
   (list :@type "getRecentStickers"
         :is_attached (or attached-p :false))
   callback))

(defun telega--getFavoriteStickers (&optional callback)
  "Return favorite stickers."
  (declare (indent 0))
  (telega-stickers--async-call
   (list :@type "getFavoriteStickers")
   callback))

(defun telega--addFavoriteSticker (sticker-input-file &optional callback)
  "Add STICKER-INPUT-FILE on top of favorite stickers."
  (telega-server--call
   (list :@type "addFavoriteSticker"
         :sticker sticker-input-file)
   callback))

(defun telega--removeFavoriteSticker (sticker-input-file &optional callback)
  (telega-server--call
   (list :@type "removeFavoriteSticker"
         :sticker sticker-input-file)
   callback))

(defun telega-sticker-toggle-favorite (sticker)
  "Toggle sticker as favorite."
  (interactive (list (telega-sticker-at)))
  (funcall (if (telega-sticker-favorite-p sticker)
               'telega--removeFavoriteSticker
             'telega--addFavoriteSticker)
           (list :@type "inputFileId"
                 :id (telega--tl-get sticker :sticker :id))

           (lambda (_ignoredreply)
             ;; Update corresponding sticker image
             (telega-media--image-update
              (cons sticker 'telega-sticker--create-image)
              (cons sticker :sticker))
             (force-window-update))))

(defun telega--getStickerEmojis (sticker-input-file)
  (telega-server--call
   (list :@type "getStickerEmojis"
         :sticker sticker-input-file)))

(defun telega-sticker--progress-svg (sticker)
  "Generate svg for STICKER showing download progress."
  (let* ((emoji (telega-tl-str sticker :emoji 'no-props))
         (xh (telega-chars-xheight (car telega-sticker-size)))
         (w-chars (telega-chars-in-width xh))
         (xw (telega-chars-xwidth w-chars))
         (image-properties
          (list :scale 1.0
                :width xw :height xh
                :ascent 'center
                :mask 'heuristic
                ;; text of correct width
                :telega-text
                (make-string
                 (or (car (plist-get sticker :telega-image-cwidth-xmargin))
                     w-chars)
                 ?X))))
    (if (display-graphic-p)
        (let ((svg (svg-create xw xh)))
          (let ((font-size (/ xh 2)))
            (svg-text svg (if (string-empty-p emoji)
                              "?"
                            (substring emoji 0 1))
                      :font-size font-size
                      :font-weight "bold"
                      :fill "white"
                      :font-family "monospace"
                      :x (/ font-size 2)
                      :y (+ font-size (/ font-size 3))))
          (telega-svg-progress svg (telega-file--downloading-progress
                                    (telega-sticker--file sticker)))
          (apply #'svg-image svg image-properties))
      (cons 'dummy-image image-properties))))

(defun telega-sticker--create-image (sticker &optional _ignoredfile)
  "Return image for the STICKER."
  ;; Three cases:
  ;;   1) Sticker downloaded
  ;;      Just show (and cache) sticker image
  ;;
  ;;   2) Thumbnail is downloaded, while sticker still downloading
  ;;      Show thumbnail (caching temporary), waiting for sticker to
  ;;      be downloaded
  ;;
  ;;   3) Thumbnail and sticker downloading
  ;;      Fallback to `telega-sticker--progress-svg', waiting for
  ;;      thumbnail or sticker to be downloaded
  (let* ((sfile (telega-sticker--file sticker))
         (tfile (telega-sticker--thumb-file sticker))
         (filename (or (and (or telega-sticker--use-thumbnail
                                (plist-get sticker :is_animated))
                            (telega-file--downloaded-p tfile) tfile)
                       (and (telega-file--downloaded-p sfile) sfile)
                       (and (telega-file--downloaded-p tfile) tfile)))
         (cwidth-xmargin (plist-get sticker :telega-image-cwidth-xmargin)))
    (unless cwidth-xmargin
      (setq cwidth-xmargin (telega-media--cwidth-xmargin
                            (plist-get sticker :width)
                            (plist-get sticker :height)
                            (car telega-sticker-size)))
      (plist-put sticker :telega-image-cwidth-xmargin cwidth-xmargin))

    (if filename
        (apply 'create-image (telega--tl-get filename :local :path)
               'imagemagick nil
               :height (telega-chars-xheight (car telega-sticker-size))
               ;; NOTE: do not use max-width setting, it will slow
               ;; down displaying stickers
;               :max-width (* (telega-chars-xwidth 1) (cdr telega-sticker-size))
               :scale 1.0
               :ascent 'center
               :margin (cons (cdr cwidth-xmargin) 0)
               :telega-text (make-string (car cwidth-xmargin) ?X)
               (when (telega-sticker-favorite-p sticker)
                 (list :background telega-sticker-favorite-background)))

      ;; Fallback to svg
      (telega-sticker--progress-svg sticker))))

(defun telega-ins--sticker-image (sticker &optional slices-p)
  "Inserter for the STICKER.
If SLICES-P is non-nil, then insert STICKER using slices."
  (if (or (not telega-use-images)
          (not (display-graphic-p)))
      (telega-ins "<STICKER " (telega-tl-str sticker :emoji) ">")

    (let ((simage (telega-media--image
                   (cons sticker 'telega-sticker--create-image)
                   (if (or telega-sticker--use-thumbnail
                           (plist-get sticker :is_animated))
                       (cons (plist-get sticker :thumbnail) :photo)
                     (cons sticker :sticker)))))
      (if slices-p
          (telega-ins--image-slices simage)
        (telega-ins--image simage)))))

(defun telega-ins--stickerset-change-button (sset)
  (telega-ins--button (if (telega-stickerset-installed-p sset)
                          ;; I18N: XXX
                          "Uninstall"
                        "Install")
    :value sset
    'action 'telega-button--stickerset-change-action))

(defun telega-button--stickerset-change-action (button)
  (let ((sset (button-get button :value)))
    (telega--changeStickerSet sset (not (telega-stickerset-installed-p sset)))
    (telega-save-cursor
      (telega-button--change button
        (telega-ins--stickerset-change-button sset)))))


(defun telega-sticker--choosen-action (button)
  "Execute action when sticker BUTTON is pressed."
  (cl-assert telega--chat)
  (cl-assert (eq major-mode 'help-mode))
  (let ((sticker (telega-sticker-at button))
        (thw-emoji telega-help-win--emoji)
        (chat telega--chat))
    ;; NOTE: Kill help win before modifying chatbuffer, because it
    ;; recovers window configuration on kill
    (quit-window 'kill-buffer)

    (with-telega-chatbuf chat
      ;; Substitute emoji with sticker, in case help win has
      ;; `telega-help-win--emoji' set
      (when thw-emoji
        (let* ((input (telega-chatbuf-input-string))
               (emoji (and (> (length input) 0) (substring input 0 1))))
          (unless (string= emoji thw-emoji)
            (error "Emoji changed %s -> %s" thw-emoji emoji))
          (goto-char telega-chatbuf--input-marker)
          (delete-char 1)))
      (telega-chatbuf-sticker-insert sticker))))

(defun telega-ins--sticker-list (stickers &optional addon-inserter)
  "Insert STICKERS list int current buffer."
  (declare (indent 1))
  (seq-doseq (sticker stickers)
    ;; (when (> (telega-current-column) (- telega-chat-fill-column 10))
    ;;   (telega-ins "\n"))
    (telega-button--insert 'telega-sticker sticker
      'help-echo (let ((emoji (telega-tl-str sticker :emoji 'no-props)))
                   (concat "Emoji: " emoji " " (telega-emoji-name emoji)))
      'action 'telega-sticker--choosen-action)
    (when addon-inserter
      (funcall addon-inserter sticker))
    (redisplay)
    ))

(defun telega-describe-stickerset (sset &optional for-chat)
  "Describe the sticker set.
SSET can be either `sticker' or `stickerSetInfo'."
  (let ((stickers (or (plist-get sset :stickers) (plist-get sset :covers))))
    (with-telega-help-win "*Telegram Sticker Set*"
      (visual-line-mode 1)
      (setq telega--chat for-chat)
      (setq telega-help-win--stickerset sset)

      (telega-ins "Title: " (telega-tl-str sset :title))
      (when (plist-get sset :is_official)
        (telega-ins telega-symbol-verified))
      (telega-ins " ")
      (telega-ins--stickerset-change-button sset)
      (telega-ins "\n")
      (telega-ins "Link:  ")
      (let ((link (concat (or (plist-get telega--options :t_me_url)
                              "https://t.me/")
                          "addstickers/" (plist-get sset :name))))
        (telega-ins--raw-button (telega-link-props 'url link)
          (telega-ins link))
        (telega-ins "\n"))
      (when telega-debug
        (telega-ins-fmt "Get: (telega-stickerset-get \"%s\")\n"
          (plist-get sset :id)))
      (telega-ins-fmt "%s: %d\n"
        (cond ((plist-get sset :is_animated) "Animated Stickers")
              ((plist-get sset :is_masks) "Masks")
              (t "Stickers"))
        (length stickers))
      (redisplay)
      (telega-ins--sticker-list stickers
        (when telega-sticker-set-show-emoji
          (lambda (sticker)          
            (telega-ins (telega-tl-str sticker :emoji) "  ")))))))

(defun telega-sticker-help (sticker)
  "Describe sticker set for STICKER."
  (interactive (list (telega-sticker-at (point))))
  (telega-describe-stickerset
   (telega-stickerset-get (plist-get sticker :set_id))))

(defun telega-sticker-list--gen-ins-callback (show-loading-p)
  "Generate callback to be used as callback.
Insert list of stickers at MARKER position.
Functions to be used with:
`telega--getStickers', `telega--getFavoriteStickers',
`telega--getRecentStickers' or `telega--searchStickerSets'"
  (let ((marker (point-marker)))
    (when show-loading-p
      (telega-ins "Loading...\n"))

    (lambda (stickers)
      (let ((marker-buf (marker-buffer marker)))
        (when (buffer-live-p marker-buf)
          (with-current-buffer marker-buf
            (telega-save-excursion
              (let ((inhibit-read-only t))
                (goto-char marker)
                (when show-loading-p
                  (delete-region marker (point-at-eol)))
                (telega-ins--sticker-list stickers)))))))))

(defun telega-sticker-choose-favorite-or-recent (for-chat)
  "Choose recent sticker FOR-CHAT."
  (interactive (list telega-chatbuf--chat))
  (cl-assert for-chat)
  (let ((help-window-select t))
    (with-telega-help-win "*Telegram Stickers*"
      (visual-line-mode 1)
      (setq telega--chat for-chat)

      ;; NOTE: use callbacks for async stickers loading
      (telega-ins "Favorite:\n")
      (telega--getFavoriteStickers
        (telega-sticker-list--gen-ins-callback 'loading))
      (telega-ins "\nRecent:\n")
      (telega--getRecentStickers nil
        (telega-sticker-list--gen-ins-callback 'loading)))))

(defun telega-sticker-choose-emoji (emoji for-chat)
  "Choose sticker by EMOJI FOR-CHAT."
  (let ((help-window-select t))
    (with-telega-help-win "*Telegram Stickers*"
      (visual-line-mode 1)
      (setq telega--chat for-chat)
      (setq telega-help-win--emoji emoji)

      ;; NOTE: use callbacks for async stickers loading
      (telega-ins "Installed:\n")
      (telega--getStickers emoji nil
        (telega-sticker-list--gen-ins-callback 'loading))

      (telega-ins "\nPublic:\n")
      (telega--searchStickers emoji nil
        (telega-sticker-list--gen-ins-callback 'loading))
    )))

(defun telega-stickerset--minibuf-post-command ()
  "Function to complete stickerset for `completion-in-region-function'."
  (cl-assert (eq major-mode 'minibuffer-inactive-mode))
  ;; NOTE:
  ;;  - Avoid help window selection by binding `help-window-select'
  ;;  - Avoid smart packages (such as shackle) to handle help window by
  ;;    binding `display-buffer-alist'
  (let* ((help-window-select nil)
         (display-buffer-alist nil)     ;XXX
         (start (minibuffer-prompt-end))
         (end (point))
         (str (cond ((memq 'ido-exhibit post-command-hook)
                     ;; ido used
                     ;;(and (boundp 'ido-cur-item) ido-matches)
                     (caar ido-matches))
                    ((memq 'ivy--queue-exhibit post-command-hook)
                     ;; ivy used
                     (nth ivy--index ivy--old-cands))
                    (t (buffer-substring start end))))
         (comp (car (all-completions str telega-minibuffer--choices)))
         (sset (cadr (assoc comp telega-minibuffer--choices)))
         (tss-buffer (get-buffer "*Telegram Sticker Set*")))

    (when (and sset
               (or (not (buffer-live-p tss-buffer))
                   (not (with-current-buffer tss-buffer
                          (and (eq telega-minibuffer--chat telega--chat)
                               (eq sset telega-help-win--stickerset))))))
      (let ((telega-sticker--use-thumbnail t))
        (telega-describe-stickerset sset telega-minibuffer--chat)
        ;; Remove annoying "Type C-x 1 to delete the help window."
        ;; message
        (message nil)))

    ;; Always pop to buffer, it might be hidden at the moment
    (when (buffer-live-p tss-buffer)
      (temp-buffer-window-show tss-buffer))
    ))

(defun telega-stickerset-completing-read (prompt &optional sticker-sets)
  "Read stickerset completing their names.
If STICKER-SETS is specified, then they are used,
otherwise installed stickersets is used.
Return sticker set."
  (message "Loading stickers, please wait...")
  (let* ((completion-ignore-case t)
         (ssets (or sticker-sets
                    (mapcar 'telega-stickerset-get
                            telega--stickersets-installed-ids)))
         ;; Bindings used in `telega-stickerset-completing-read'
         (telega-minibuffer--chat telega-chatbuf--chat)
         (telega-minibuffer--choices
          (mapcar (lambda (sset)
                    (list (plist-get sset :name) sset))
                  ssets))
         (sset-name
          (minibuffer-with-setup-hook
              (lambda ()
                (add-hook 'post-command-hook
                          'telega-stickerset--minibuf-post-command t t))
            (funcall telega-completing-read-function
                     prompt telega-minibuffer--choices nil t))))
    (cadr (assoc sset-name telega-minibuffer--choices))
  ))

(defun telega-stickerset-choose (sset)
  "Interactive choose stickerset."
  (interactive (list (telega-stickerset-completing-read "Sticker set: ")))
  (let ((tss-buffer (get-buffer "*Telegram Sticker Set*")))
    (if (and (buffer-live-p tss-buffer)
             (with-current-buffer tss-buffer
               (and (eq telega-help-win--stickerset sset)
                    (eq telega--chat telega-chatbuf--chat))))
        (select-window
         (temp-buffer-window-show tss-buffer))

      (let ((help-window-select t))
        (telega-describe-stickerset sset telega-chatbuf--chat)))))

(defun telega-stickerset-search (query)
  "Search interactively for sticker matching QUERY."
  (interactive "sStickerSet query: ")
  (let ((sticker-sets (telega--searchStickerSets query)))
    (unless sticker-sets
      (user-error "No sticker set found for: %s" query))

    (telega-stickerset-choose
     (if (> (length sticker-sets) 1)
         ;; Multiple sticker sets
         (telega-stickerset-completing-read
          "Sticker set: " sticker-sets)
       ;; Single sticker set
       (car sticker-sets)))))

(defun telega-stickerset-trends ()
  "Show trending stickers."
  (interactive)
  (let ((sticker-sets (telega--getTrendingStickerSets)))
    (unless sticker-sets
      (user-error "No trending sticker sets"))

    (telega-stickerset-choose
     (telega-stickerset-completing-read
      "Trending sticker set: " sticker-sets))))


;;; Animations
(define-button-type 'telega-animation
  :supertype 'telega
  :inserter 'telega-ins--animation-image)

(defun telega-animation-at (&optional pos)
  "Retur sticker at POS."
  (let ((button (button-at (or pos (point)))))
    (when (and button (eq (button-type button) 'telega-animation))
      (button-get button :value))))

(defsubst telega-animation--file (animation)
  "Return ANIMATIONS's file."
  (telega-file--renew animation :animation))

(defalias 'telega-animation--thumb-file 'telega-sticker--thumb-file)

(defun telega-animation--download (animation)
  "Ensure media content for ANIMATION has been downloaded."
  (let ((animation-file (telega-animation--file animation))
        (thumb-file (telega-animation--thumb-file animation)))
    (when (telega-file--need-download-p thumb-file)
      (telega-file--download thumb-file 5))
    (when telega-animation-download-saved
      (when (telega-file--need-download-p animation-file)
        (telega-file--download animation-file 1)))
    ))

(defun telega--on-updateSavedAnimations (event)
  "List of saved animations has been updated."
  (setq telega--animations-saved
        (append (plist-get event :animation_ids) nil))
  ;; Asynchronously download corresponding files
  (when telega-animation-download-saved
    (mapc 'telega--downloadFile telega--animations-saved)))

(defun telega--getSavedAnimations ()
  "Return list of saved animations."
  (let* ((reply (telega-server--call
                 (list :@type "getSavedAnimations")))
         (anims (append (plist-get reply :animations) nil)))
    ;; Start downloading animations
    (when telega-animation-download-saved
      (mapc 'telega-animation--download anims))
    anims))

(defun telega--addSavedAnimation (input-file)
  "Manually adds a new animation to the list of saved animations."
  (telega-server--send
   (list :@type "addSavedAnimation"
         :animation input-file)))

(defun telega--removeSavedAnimation (input-file)
  "Removes an animation from the list of saved animations."
  (telega-server--send
   (list :@type "removeSavedAnimation"
         :animation input-file)))

(defun telega-animation--progress-svg (animation)
  "Generate svg for STICKER showing download progress."
  (let* ((xh (telega-chars-xheight telega-animation-height))
         (w-chars (telega-chars-in-width xh))
         (xw (telega-chars-xwidth w-chars))
         (image-properties
          (list :scale 1.0
                :width xw :height xh
                :ascent 'center
                :mask 'heuristic
                ;; text of correct width
                :telega-text
                (make-string
                 (or (car (plist-get animation :telega-image-cwidth-xmargin))
                     w-chars)
                 ?X))))
    (if (display-graphic-p)
        (let ((svg (svg-create xw xh)))
          (telega-svg-progress svg (telega-file--downloading-progress
                                    (telega-animation--file animation)))
          (apply #'svg-image svg image-properties))
      (cons 'dummy-image image-properties))))

(defun telega-animation--create-image (animation &optional _fileignored)
  "Return image for the ANIMATION."
  ;; Cases:
  ;;   1) Next inline animation frame (set in
  ;;   `telega-msg-animation--callback') is available - display it
  ;;
  ;;   2) Thumbnail is downloaded, while animation still downloading
  ;;      Show thumbnail (caching temporary), waiting for animation to
  ;;      be downloaded
  ;;
  ;;   3) Thumbnail and animation downloading
  ;;      Fallback to svg loading image
  (let* ((anim-frame-filename
          (plist-get animation :telega-ffplay-frame-filename))
         (tfile (telega-animation--thumb-file animation))
         (cwidth-xmargin (plist-get animation :telega-image-cwidth-xmargin)))
    (unless cwidth-xmargin
      (setq cwidth-xmargin (telega-media--cwidth-xmargin
                            (plist-get animation :width)
                            (plist-get animation :height)
                            telega-animation-height))
      (plist-put animation :telega-image-cwidth-xmargin cwidth-xmargin))

    (let ((img-props
           (list :height (telega-chars-xheight telega-animation-height)
                 :scale 1.0
                 :ascent 'center
                 :margin (cons (cdr cwidth-xmargin) 0)
                 :telega-text (make-string (car cwidth-xmargin) ?X))))
      (cond (anim-frame-filename
             ;; Remove this prop, because file is about to be deleted
             (plist-put animation :telega-ffplay-frame-filename nil)
             (if (display-graphic-p)
                 (apply 'create-image
                        (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally anim-frame-filename)
                          (buffer-string))
                        'imagemagick t img-props)
               (cons 'dummy-image image-props)))

            ((telega-file--downloaded-p tfile)
             (if (display-graphic-p)
                 (apply 'create-image
                        (telega--tl-get tfile :local :path)
                        'imagemagick nil img-props)
               (cons 'dummy-image image-properties)))

            (t (telega-animation--progress-svg animation))))))

(defun telega-ins--animation-image (animation &optional slices-p)
  "Inserter for the ANIMATION.
If SLICES-P is non-nil, then insert ANIMATION using slices."
  (let ((aimage (telega-media--image
                 (cons animation 'telega-animation--create-image)
                 (cons (plist-get animation :thumbnail) :photo)
                 'force-update)))
    (if slices-p
        (telega-ins--image-slices aimage)
      (telega-ins--image aimage))))

(defun telega-animation--choosen-action (button)
  "Execute action when animation BUTTON is pressed."
  (cl-assert telega--chat)
  (cl-assert (eq major-mode 'help-mode))
  (let ((animation (telega-animation-at button))
        (chat telega--chat))
    ;; NOTE: Kill help win before modifying chatbuffer, because it
    ;; recovers window configuration on kill
    (quit-window 'kill-buffer)

    (with-telega-chatbuf chat
      (telega-chatbuf-animation-insert animation))))

(defun telega-animation--ffplay-callback (_proc filename anim)
  "Ffplay callback to animate ANIM."
  (plist-put anim :telega-ffplay-frame-filename filename)
  (telega-media--image-update
   (cons anim 'telega-animation--create-image) nil)
  (force-window-update)
  )

(defun telega-animation--gen-sensor-func (anim)
  "Return sensor function to animate ANIM when entered."
  (cl-assert anim)
  (lambda (_window _oldpos dir)
    (when telega-animation-play-inline
      (if (eq dir 'entered)
          (telega-file--download (telega-file--renew anim :animation) 32
            (lambda (file)
              (when (telega-file--downloaded-p file)
                (telega-ffplay-to-png (telega--tl-get file :local :path)
                    nil 'telega-animation--ffplay-callback anim))))

        ;; dir == left
        (telega--cancelDownloadFile (telega--tl-get anim :animation :id))))))

(defun telega-ins--animation (anim &rest props)
  "Inserter for animation ANIM in help buffer.
PROPS are additional properties to the animation button."
  (declare (indent 1))
  (apply 'telega-button--insert 'telega-animation anim
         'action 'telega-animation--choosen-action
         ;; NOTE: use different sensor functions, so all animations
         ;; can be places next to each other and still sensor
         ;; detection will work properly
         'cursor-sensor-functions
         (list (telega-animation--gen-sensor-func anim))
         props))

(defun telega-animation-choose (for-chat animations &optional window-select)
  "Choose one of the ANIMATIONS for insertation into FOR-CHAT."
  (cl-assert for-chat)
  (let ((help-window-select window-select))
    (with-telega-help-win "*Telegram Animations*"
      (visual-line-mode 1)
      (setq telega--chat for-chat)

      (mapc 'telega-ins--animation animations))))

(defun telega-animation-choose-saved (for-chat)
  "Choose saved animation FOR-CHAT."
  (interactive (list telega-chatbuf--chat))
  (telega-animation-choose for-chat (telega--getSavedAnimations) t))

(provide 'telega-sticker)

;;; telega-sticker.el ends here

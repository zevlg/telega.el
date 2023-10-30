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
(require 'telega-tdlib)
(require 'telega-util)
(require 'telega-media)
(require 'telega-ffplay)

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

(defun telega-sticker-static-p (sticker)
  "Return non-nil if sticker is a static sticker."
  (eq 'stickerFormatWebp (telega--tl-type (plist-get sticker :format))))

(defsubst telega-sticker--file (sticker)
  "Return STICKER's file."
  (telega-file--renew sticker :sticker))

(defsubst telega-sticker--thumb-file (sticker)
  "Return STICKER's thumbnail file."
  ;; NOTE: return only for non-animated thumbnails, otherwise
  ;; minithumbnail will be used
  (when-let ((thumb (plist-get sticker :thumbnail)))
    (when (memq (telega--tl-type (plist-get thumb :format))
                '(thumbnailFormatJpeg thumbnailFormatPng thumbnailFormatWebp))
      (telega-file--renew thumb :file))))

(defsubst telega-sticker-favorite-p (sticker)
  "Return non-nil if STICKER is in favorite list."
  (memq (telega--tl-get sticker :sticker :id) telega--stickers-favorite))

(defun telega-sticker--download (sticker)
  "Ensure STICKER data is downloaded.
Download only if `telega-use-images' is non-nil."
  (let ((sticker-file (telega-sticker--file sticker))
        (thumb-file (telega-sticker--thumb-file sticker)))
    (when telega-use-images
      (when (telega-file--need-download-p thumb-file)
        (telega-file--download thumb-file 6))
      (unless telega-sticker--use-thumbnail
        (when (telega-file--need-download-p sticker-file)
          (telega-file--download sticker-file 2))))
    ))

(defsubst telega-sticker-emoji (sticker &optional no-properties)
  "Return STICKER's emoji or empty string."
  (or (telega-tl-str sticker :emoji no-properties) ""))

(defun telega-sticker-size (sticker)
  "Return size suitable to display a STICKER."
  (or (plist-get sticker :telega-sticker-size) telega-sticker-size))

(defun telega-stickerset--download (sset)
  "Ensure sticker set SSET data is downloaded."
  (seq-doseq (sticker (plist-get sset :stickers))
    (telega-sticker--download sticker)))

(defun telega-stickerset--ensure (sset)
  "Ensure sticker set SSET is put into `telega--stickersets'."
  (setf (alist-get (plist-get sset :id) telega--stickersets nil nil 'equal)
        sset)
  (when telega-sticker-set-download
    (telega-stickerset--download sset))
  sset)

(defun telega-stickerset-get (set-id &optional locally-p callback)
  "Get stickerset by SET-ID.
If LOCALLY-P is non-nil, then do not perform request to telega-server.
If CALLBACK is specified and stickerset is not yet fetched, then
fetch stickerset asynchronously and call the CALLBACK function
with one argument - stickerset."
  (declare (indent 2))
  (let ((sset (cdr (assoc set-id telega--stickersets))))
    (if (or locally-p sset)
        (if callback
            (funcall callback sset)
          sset)
      (telega--getStickerSet set-id callback))))

(defun telega-stickerset-installed-p (sset)
  "Return non-nil if sticker set SSET is installed."
  (or (member (plist-get sset :id) telega--stickersets-installed-ids)
      (cl-find (plist-get sset :id) telega--stickersets-custom-emojis
               :key (telega--tl-prop :id) :test #'equal)))

(defun telega-sticker--dice-get (dice-value &optional locally-p callback)
  "Return sticker for the DICE-VALUE.
Return nil, if sticker is not found.
CALLBACK is called without arguments"
  (declare (indent 2))
  (when-let ((dice-sset-name
              (plist-get telega--options :animated_dice_sticker_set_name)))
    (let ((sset (cl-find dice-sset-name
                         telega--stickersets-system
                         :key (telega--tl-prop :name)
                         :test #'equal)))
      (if (or locally-p sset)
          (cl-find (format "%d\uFE0F⃣" dice-value)
                   (plist-get sset :stickers)
                   :key #'telega-sticker-emoji
                   :test #'equal)

        (telega--searchStickerSet dice-sset-name
          (lambda (dice-sset)
            (cl-assert (equal (plist-get dice-sset :name) dice-sset-name))
            (setq telega--stickersets-system
                  (push dice-sset telega--stickersets-system))
            (when callback
              (funcall callback))))
        ))))

(defun telega-sticker--emoji-get (emoji)
  "Get Animated Sticker for emoji."
  ;; NOTE: EMOJI itself or sticker's emoji could be a fe0f emoji
  (when (telega-emoji-fe0f-p emoji)
    (setq emoji (substring emoji 0 1)))
  (cl-find emoji (plist-get (telega-stickerset-get
                                telega--animated-emojis-stickerset-id 'locally)
                            :stickers)
           :test #'equal
           :key (lambda (obj)
                  (let ((sticker-emoji (telega-tl-str obj :emoji)))
                    (if (telega-emoji-fe0f-p sticker-emoji)
                        (substring sticker-emoji 0 1)
                      sticker-emoji)))))

(defun telega-sticker-toggle-favorite (sticker)
  "Toggle sticker as favorite."
  (interactive (list (telega-sticker-at)))
  (funcall (if (telega-sticker-favorite-p sticker)
               #'telega--removeFavoriteSticker
             #'telega--addFavoriteSticker)
           (list :@type "inputFileId"
                 :id (telega--tl-get sticker :sticker :id))

           (lambda (_ignoredreply)
             ;; Update corresponding sticker image
             (telega-media--image-update
              (cons sticker 'telega-sticker--create-image)
              (cons sticker :sticker))
             (force-window-update))))

(defun telega-sticker--svg-outline-path (svg tl-path factor &rest args)
  "Draw closed TL-PATH into SVG."
  (let* ((x-factor (car factor))
         (y-factor (cdr factor))
         (tl-path-commands (plist-get tl-path :commands))
         (tl-cmd1 (aref tl-path-commands 0))
         (tl-move-to (cl-ecase (telega--tl-type tl-cmd1)
                       (vectorPathCommandCubicBezierCurve
                        (plist-get tl-cmd1 :start_control_point))
                       (vectorPathCommandLine
                        (plist-get tl-cmd1 :end_point))))
         (svg-path-commands
          (mapconcat
           (lambda (path-cmd)
             (cl-ecase (telega--tl-type path-cmd)
               (vectorPathCommandCubicBezierCurve
                (let ((p1 (plist-get path-cmd :start_control_point))
                      (p2 (plist-get path-cmd :end_control_point))
                      (p3 (plist-get path-cmd :end_point)))
                  (format "C%f,%f,%f,%f,%f,%f"
                          (* x-factor (plist-get p1 :x))
                          (* y-factor (plist-get p1 :y))
                          (* x-factor (plist-get p2 :x))
                          (* y-factor (plist-get p2 :y))
                          (* x-factor (plist-get p3 :x))
                          (* y-factor (plist-get p3 :y)))))
               (vectorPathCommandLine
                (let ((end-point (plist-get path-cmd :end_point)))
                  (format "L%f,%f"
                          (* x-factor (plist-get end-point :x))
                          (* y-factor (plist-get end-point :y)))))))
           tl-path-commands "\n")))
    (apply #'telega-svg-path svg
           (concat (format "M%f,%f\n"
                           (* x-factor (plist-get tl-move-to :x))
                           (* y-factor (plist-get tl-move-to :y)))
                   svg-path-commands "Z")
           args)))

(defun telega-sticker--svg-outline (svg sticker &rest args)
  "Draw STICKER outline path to the SVG."
  (declare (indent 2))
  (cl-assert (plist-get sticker :outline))
  (let ((factor (cons (/ (float (telega-svg-width svg))
                         (plist-get sticker :width))
                      (/ (float (telega-svg-height svg))
                         (plist-get sticker :height)))))
    (seq-doseq (outline-path (plist-get sticker :outline))
      (apply #'telega-sticker--svg-outline-path
             svg outline-path factor args))))

(defun telega-sticker--progress-svg (sticker)
  "Generate svg for STICKER showing download progress."
  (let* ((emoji (telega-sticker-emoji sticker 'no-props))
         (sticker-size (telega-sticker-size sticker))
         (xh (telega-chars-xheight (car sticker-size)))
         (w-chars (telega-chars-in-width xh))
         (xw (telega-chars-xwidth w-chars))
         (svg (telega-svg-create xw xh))
         (font-size (/ xh 2)))
    (if (plist-get sticker :outline)
        (telega-sticker--svg-outline svg sticker
          :fill (telega-color-name-as-hex-2digits
                 (or (face-foreground 'telega-shadow) "gray50")))

      ;; draw emoji and progress circle
      (svg-text svg (if (string-empty-p emoji)
                        "?"
                      (substring emoji 0 1))
                :font-size font-size
                :font-weight "bold"
                :fill "white"
                :font-family "monospace"
                :x (/ font-size 2)
                :y (+ font-size (/ font-size 3)))
      (telega-svg-progress svg (telega-file--downloading-progress
                                (telega-sticker--file sticker))))
    (telega-svg-image svg :scale 1.0
                      :width xw :height xh
                      :ascent 'center
                      :mask 'heuristic
                      ;; text of correct width
                      :telega-text
                      (make-string
                       (or (car (plist-get sticker :telega-image-cwidth-xmargin))
                           w-chars)
                       ?X))
    ))

(defvar telega-sticker--convert-cmd
  (if (or telega-use-docker
          (executable-find "dwebp")
          (not (telega-ffplay-has-decoder-p "webp")))
      "dwebp -nofancy -mt -o %p %w"
    "ffmpeg -v quiet -i %w %p")
  "Command to convert WEBP file to PNG file.
%p - png filename
%w - webp filename.")

(defun telega-sticker--webp-to-png (webp-filename)
  "Convert FILENAME in webp format to png.
Return path to png file."
  (let ((png-filename (concat (file-name-sans-extension webp-filename)
                              "_telega.png")))
    (unless (file-exists-p png-filename)
      (let ((convert-cmd
             (telega-docker-exec-cmd
               (format-spec telega-sticker--convert-cmd
                            (format-spec-make ?p png-filename
                                              ?w webp-filename))
               'try-host-cmd-first nil 'no-error)))
        (cond (convert-cmd
               (telega-debug "WEBP -> PNG: %s" convert-cmd)
               (shell-command-to-string convert-cmd))
              ((image-type-available-p 'webp)
               ;; NOTE: If `webp' image is supported by Emacs, use it.
               ;; We use webp->png converter by default, because it
               ;; gives better results, then Emacsen `webp' images.
               (setq png-filename webp-filename))
              (t
               (telega-help-message 'no-dwebp-binary
                   "Can't find `%s' binary.  `webp' system package not installed?"
                 (car (split-string telega-sticker--convert-cmd)))))))

    (when (file-exists-p png-filename)
      png-filename)))

(defun telega-sticker--create-image (sticker &optional _ignoredfile)
  "Return image for the STICKER."
  ;; Three cases:
  ;;   1) Sticker downloaded (in case if static)
  ;;      Just show (and cache) sticker image
  ;;
  ;;   2) Thumbnail is downloaded, while sticker still downloading
  ;;      Show thumbnail (caching temporary), waiting for sticker to
  ;;      be downloaded
  ;;
  ;;   3) Thumbnail and sticker downloading
  ;;      Fallback to `telega-sticker--progress-svg', waiting for
  ;;      thumbnail or sticker to be downloaded
  (let* ((sticker-size (telega-sticker-size sticker))
         (sfile (telega-sticker--file sticker))
         (tfile (telega-sticker--thumb-file sticker))
         (filename (or (and (or telega-sticker--use-thumbnail
                                (not (telega-sticker-static-p sticker)))
                            (telega-file--downloaded-p tfile) tfile)
                       (and (telega-sticker-static-p sticker)
                            (telega-file--downloaded-p sfile) sfile)
                       (and (telega-file--downloaded-p tfile) tfile)))
         (img-file (or (plist-get sticker :telega-ffplay-frame-filename)
                       (when-let ((fn (telega--tl-get filename :local :path)))
                         (if (or (fboundp 'imagemagick-types)
                                 (image-type-available-p 'webp)
                                 (not (equal (file-name-extension fn) "webp")))
                             fn
                           (telega-sticker--webp-to-png fn)))))
         (cwidth-xmargin (telega-media--cwidth-xmargin
                          (plist-get sticker :width)
                          (plist-get sticker :height)
                          (car sticker-size)))
         (custom-create-image-function
          (plist-get sticker :telega-create-image-function))
         (img
          (cond (custom-create-image-function
                 ;; NOTE: Custom emoji stickers has custom create
                 ;; image function
                 (funcall custom-create-image-function sticker img-file))
                (img-file
                 (apply #'telega-create-image img-file
                        (when (fboundp 'imagemagick-types) 'imagemagick) nil
                        :height (telega-chars-xheight (car sticker-size))
                        ;; NOTE: do not use max-width setting, it will slow
                        ;; down displaying stickers
                        ;; :max-width (* (telega-chars-xwidth 1)
                        ;;               (cdr sticker-size))
                        :scale 1.0 :ascent 'center
                                        ; :mask 'heuristic
                        :margin (cons (cdr cwidth-xmargin) 0)
                        :telega-text (make-string (car cwidth-xmargin) ?X)
                        (when (telega-sticker-favorite-p sticker)
                          (list :background telega-sticker-favorite-background))))
                (t
                 ;; Fallback to svg
                 (telega-sticker--progress-svg sticker)))))
    img))

(defun telega-sticker--create-image-one-line (sticker &optional file)
  "Create image for one-line STICKER usage."
  (let ((telega-sticker-size (cons 1 (cdr telega-sticker-size))))
    (telega-sticker--create-image sticker file)))

(defun telega-sticker--image (sticker &optional image-create-fun cache-prop)
  "Return image for the STICKER."
  (telega-media--image
   (cons sticker (or image-create-fun #'telega-sticker--create-image))
   (if (or (and telega-sticker--use-thumbnail
                (plist-get sticker :thumbnail))
           (not (telega-sticker-static-p sticker)))
       (cons (plist-get sticker :thumbnail) :file)
     (cons sticker :sticker))
   nil cache-prop))

(defun telega-ins--sticker-image (sticker &optional slices-p)
  "Inserter for the STICKER.
If SLICES-P is non-nil, then insert STICKER using slices."
  (if (or (not telega-use-images)
          (not (display-graphic-p (telega-x-frame))))
      (telega-ins "<STICKER\u00A0" (telega-sticker-emoji sticker) ">")

    (funcall (if slices-p #'telega-ins--image-slices #'telega-ins--image)
             (telega-sticker--image sticker))))

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


(defun telega-sticker--chosen-action (button)
  "Execute action when sticker BUTTON is pressed."
  (cl-assert (eq major-mode 'help-mode))
  (let ((sticker (telega-sticker-at button))
        (custom-action (button-get button :action))
        (thw-emoji telega-help-win--emoji)
        (chat telega--chat))
    (cl-assert (or telega--chat custom-action))
    ;; NOTE: Kill help win before modifying chatbuffer, because it
    ;; recovers window configuration on kill
    (quit-window 'kill-buffer)

    (if custom-action
        (funcall custom-action sticker)

      ;; NOTE: Default action is to insert selected sticker into
      ;; chatbuf
      (with-telega-chatbuf chat
        ;; Substitute emoji with sticker, in case help win has
        ;; `telega-help-win--emoji' set
        (when thw-emoji
          (let* ((start (save-excursion (telega-emoji-backward) (point)))
                 (end (point))
                 (emoji (buffer-substring start end)))
            (unless (string= emoji thw-emoji)
              (error "Emoji changed %s -> %s" thw-emoji emoji))
            (delete-region start end)))
        (if (telega-custom-emoji-sticker-p sticker)
            (telega-chatbuf-custom-emoji-insert sticker thw-emoji)
          (telega-chatbuf-sticker-insert sticker))))))

(cl-defun telega-ins--sticker-list (stickers &key column custom-action)
  "Insert STICKERS list int current buffer.
COLUMN specifies column to fill stickers into.  By default
`fill-column' is used.
CUSTOM-ACTION - function accepting single argument (a sticker) to be
called when some sticker is selected."
  (declare (indent 1))
  (seq-doseq (sticker stickers)
    (telega-ins-prefix (unless (bolp) "\n")
      (telega-button--insert 'telega-sticker sticker
        'help-echo (let ((emoji (telega-sticker-emoji sticker)))
                     (concat "Emoji: " emoji " " (telega-emoji-name emoji)))
        'action #'telega-sticker--chosen-action
        :action custom-action
        'cursor-sensor-functions
        (when (and (not (telega-sticker-static-p sticker))
                   telega-sticker-animated-play)
          (list (telega-sticker--gen-sensor-func sticker)))
        )
      (when telega-sticker-set-show-emoji
        (telega-ins (telega-sticker-emoji sticker) "  "))

      ;; NOTE: `telega-ins-prefix' will insert "\n" if next sexp
      ;; evaluates to non-nil
      (> (current-column) (or column (current-fill-column))))
    ;; (redisplay)
    ))

(defun telega-describe-stickerset (sset &optional for-chat)
  "Describe the sticker set.
SSET can be either `sticker' or `stickerSetInfo'."
  (with-telega-help-win "*Telegram Sticker Set*"
    (setq telega--chat for-chat)
    (setq telega-help-win--stickerset sset)

    (telega-ins "Title: " (telega-tl-str sset :title))
    (when (plist-get sset :is_official)
      (telega-ins (telega-symbol 'verified)))
    (telega-ins " ")
    (telega-ins--stickerset-change-button sset)
    (telega-ins "\n")
    (telega-ins "Link:  ")
    (let ((link (concat (or (plist-get telega--options :t_me_url)
                            "https://t.me/")
                        "addstickers/" (plist-get sset :name))))
      (telega-ins--raw-button (telega-link-props 'url link 'face 'telega-link)
        (telega-ins link))
      (telega-ins "\n"))
    (when telega-debug
      (telega-ins-fmt "Get: (telega-stickerset-get \"%s\")\n"
        (plist-get sset :id)))

    ;; NOTE: In case SSET is "stickerSetInfo" fetch real sticker set
    ;; and insert all the stickers
    (let ((sticker-list-ins
           (lambda (sticker-set)
             (let ((stickers (plist-get sticker-set :stickers)))
               (telega-ins-fmt "%s: %d\n"
                 (cl-ecase (telega--tl-type (plist-get sticker-set :sticker_type))
                   (stickerTypeMask "Masks")
                   (stickerTypeCustomEmoji "Custom Emoji Stickers")
                   (stickerTypeRegular
                    (cl-ecase (telega--tl-type
                               (plist-get sticker-set :sticker_format))
                      (stickerFormatTgs  "Animated Stickers")
                      (stickerFormatWebm "Video Stickers")
                      (stickerFormatWebp "Stickers"))))
                 (length stickers))
               (telega-ins--sticker-list stickers))))
          (sset-id (plist-get sset :id)))
      (when (eq (telega--tl-type sset) 'stickerSetInfo)
        (setq sset (telega-stickerset-get sset-id 'locally)))
      ;; Now SSET is always "stickerSet"
      (if sset
          (funcall sticker-list-ins sset)

        ;; Set `telega--help-win-param', so callback won't insert
        ;; stickers list of stickerset changes during request to
        ;; telega-server.
        (setq telega--help-win-param sset-id)

        (telega-stickerset-get sset-id nil
          (telega--gen-ins-continuation-callback 'loading
            sticker-list-ins
            sset-id))))))

(defun telega-sticker-help (sticker)
  "Describe sticker set for STICKER."
  (interactive (list (telega-sticker-at (point))))
  (telega-describe-stickerset
   (telega-stickerset-get (plist-get sticker :set_id))))

(defun telega-sticker-choose-favorite-or-recent (for-chat)
  "Choose recent sticker FOR-CHAT."
  (interactive (list telega-chatbuf--chat))
  (cl-assert for-chat)
  (let ((help-window-select t))
    (with-telega-help-win "*Telegram Stickers*"
      (setq telega--chat for-chat)

      ;; NOTE: use callbacks for async stickers loading
      (telega-ins (telega-i18n "lng_mac_touchbar_favorite_stickers") ":\n")
      (telega--getFavoriteStickers
        (telega--gen-ins-continuation-callback 'loading
          #'telega-ins--sticker-list))
      (telega-ins "\n")

      (telega-ins "\n" (telega-i18n "lng_recent_stickers") ":\n")
      (telega--getRecentStickers nil
        (telega--gen-ins-continuation-callback 'loading
          #'telega-ins--sticker-list)))))

(defun telega-sticker-choose-emoji (emoji for-chat &optional custom-emojis-only)
  "Choose sticker by EMOJI FOR-CHAT.
If CUSTOM-EMOJIS-ONLY is specified, then list only custom emoji
stickers for the EMOJI."
  (let ((help-window-select t))
    (with-telega-help-win "*Telegram Stickers*"
      (setq telega--chat for-chat)
      (setq telega-help-win--emoji emoji)

      ;; NOTE: use callbacks for async stickers loading
      (telega-ins-fmt "Custom Emoji for %s:\n" emoji)
      (telega--getStickers emoji
        :chat for-chat
        :tl-sticker-type '(:@type "stickerTypeCustomEmoji")
        :callback (telega--gen-ins-continuation-callback 'loading
                    (lambda (stickers &rest args)
                      (apply #'telega-ins--sticker-list
                             (mapcar #'telega-custom-emoji-from-sticker
                                     stickers)
                             args))))

      (unless custom-emojis-only
        (telega-ins "\n")
        (telega-ins "\nInstalled Stickers:\n")
        (telega--getStickers emoji
          :callback (telega--gen-ins-continuation-callback 'loading
                      #'telega-ins--sticker-list))

        (telega-ins "\n")
        (telega-ins "\nPublic Stickers:\n")
        (telega--searchStickers emoji
          :callback
          (telega--gen-ins-continuation-callback 'loading
            #'telega-ins--sticker-list)))
    )))

(defun telega-stickerset--minibuf-post-command ()
  "Function to complete stickerset for `completion-in-region-function'."
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
         (comp (when str (car (all-completions str telega-minibuffer--choices))))
         (sset-id (when comp (cadr (assoc comp telega-minibuffer--choices))))
         (sset (when sset-id
                 (or (cl-find sset-id telega--stickersets-installed
                              :test 'equal
                              :key (telega--tl-prop :id))
                     (telega-stickerset-get sset-id))))
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

(defun telega-stickerset-title (sset &optional raw-p)
  "Return title for the sticker set SSET.
If RAW-P is specified, then do not hack first char for helm."
  (concat (when telega-sticker-set-show-cover
            (when-let ((cover (car (append (or (plist-get sset :covers)
                                               (plist-get sset :stickers))
                                           nil))))
              ;; NOTE: helm uses 'display property of the first
              ;; character and expects it to be string, so we add
              ;; zero-width space in there
              (concat (unless raw-p "\u200B")
                      (propertize
                       (telega-sticker-emoji cover)
                       'display (telega-sticker--image
                                 cover #'telega-sticker--create-image-one-line
                                 :telega-sticker-cover-1)))))
          (or (telega-tl-str sset :title)
              (telega-tl-str sset :name))))

(defun telega-stickerset-completing-read (prompt &optional sticker-sets)
  "Read stickerset completing their names.
If STICKER-SETS is specified, then they are used,
otherwise installed stickersets is used.
Return sticker set."
  (message "Loading stickers, please wait...")
  (let* ((completion-ignore-case t)
         (ssets (or sticker-sets
                    telega--stickersets-installed
                    ;; NOTE: "getInstalledStickerSets" could generate
                    ;; "updateInstalledStickerSets" event, we don't
                    ;; want to handle it, since we are already getting
                    ;; list of installed stickersets
                    (let ((telega-server--inhibit-events
                           (cons "updateInstalledStickerSets"
                                 telega-server--inhibit-events)))
                      (setq telega--stickersets-installed
                            (telega--getInstalledStickerSets))
                      (setq telega--stickersets-installed-ids
                            (mapcar (telega--tl-prop :id)
                                    telega--stickersets-installed))
                      telega--stickersets-installed)
                    (user-error "No installed sticker sets")))
         ;; Bindings used in `telega-stickerset-completing-read'
         (telega-minibuffer--chat telega-chatbuf--chat)
         (telega-minibuffer--choices
          (mapcar (lambda (sset)
                    (list (telega-stickerset-title sset) (plist-get sset :id)))
                  ssets))
         (sset-name
          (minibuffer-with-setup-hook
              (lambda ()
                (add-hook 'post-command-hook
                          'telega-stickerset--minibuf-post-command t t))
            (funcall telega-completing-read-function
                     prompt telega-minibuffer--choices nil t))))

    (cl-find (cadr (assoc sset-name telega-minibuffer--choices)) ssets
             :test #'equal
             :key (telega--tl-prop :id))
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

(defun telega-stickerset-trends (&optional premium-p)
  "Show trending stickers.
If prefix argument is specified, then show trends in Premium stickers."
  (interactive "P")
  (let ((sticker-sets (or (when premium-p
                            telega--stickersets-trending-premium)
                          (telega--getTrendingStickerSets))))
    (unless sticker-sets
      (user-error "No trending sticker sets"))

    (telega-stickerset-choose
     (telega-stickerset-completing-read
      "Trending sticker set: " sticker-sets))))

(defun telega-sticker--animate-callback (_proc frame sticker &optional for-msg)
  "Callback for inline animated sticker playback."
  (plist-put sticker :telega-ffplay-frame-filename (cdr frame))
  ;; NOTE: for some animated stickers (not having thumbnail or
  ;; outline) image update can fail when all frames are player
  ;; (i.e. (cdr frame) is nil)
  (ignore-errors
    (telega-media--image-update
     (cons sticker 'telega-sticker--create-image) nil))
  (if for-msg
      (telega-msg-redisplay for-msg)

    ;; NOTE: just redisplay the image, not redisplaying full message
    (force-window-update)))

(defun telega-sticker--animate-to-png (sticker-file xheight callback
                                                    &rest callback-args)
  "Animate animated sticker to series of PNG files."
  (declare (indent 2))
  (cl-assert (telega-file--downloaded-p sticker-file))

  ;; Kill previously running animations/ffplay if any
;  (telega-ffplay-stop)

  ;; XXX: heavy heuristics to detect HiDPI displays on MacOS
  ;; If resulting PNG is too small and DPI is high, then consider HiDPI
  ;; mode is in use
  ;; png height = 76 * 4 = 306 is still generates pretty fast on modern PC
  ;; See https://t.me/emacs_telega/25785
  (when (and (< xheight 76)
             (> (round (/ (display-pixel-height) (/ (display-mm-height) 25.4)))
                96))
    (setq xheight (* xheight 4)))

  (let* ((prefix (telega-temp-name "png-sticker-anim"))
         (shell-cmd
          (if telega-use-docker
              (telega-docker-exec-cmd
                (format "sh -c \"gunzip -c '%s' | tgs2png -s 0x%d - | telega-server -E %s\""
                        (telega--tl-get sticker-file :local :path)
                        xheight  prefix))
            (format
             "gunzip -c '%s' | %s -s 0x%d - | %s -E %s"
             (telega--tl-get sticker-file :local :path)
             (or (executable-find "tgs2png")
                 (error "tgs2png not found in `exec-path', \
Install from https://github.com/zevlg/tgs2png"))
             xheight (telega-server--process-command) prefix)))
         (process-adaptive-read-buffering nil) ;no buffering please
         (proc (start-process-shell-command
                "sticker-animate"
                (generate-new-buffer-name " *tgs2png telega*")
                shell-cmd)))
    (telega-debug "Running sticker-animate: %s" shell-cmd)

    (set-process-plist proc (list :prefix prefix
                                  :nframes -1
                                  :frames nil
                                  :callback callback
                                  :callback-args callback-args))
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc 'telega-ffplay--png-sentinel)
    (set-process-filter proc 'telega-ffplay--png-filter)
    proc))

(defun telega-sticker--animate (sticker &optional for-msg)
  "Start animating animated STICKER."
  (cl-assert (not (telega-sticker-static-p sticker)))
  (telega-file--download (plist-get sticker :sticker) 32
    (lambda (file)
      (telega-file--renew sticker :sticker)
      (when (telega-file--downloaded-p file)
        (cl-ecase (telega--tl-type (plist-get sticker :format))
          (stickerFormatTgs
           ;; NOTE: do not start animation if already started
           (unless (plist-get sticker :telega-ffplay-frame-filename)
             (telega-sticker--animate-to-png file
                 (telega-chars-xheight (car (telega-sticker-size sticker)))
               #'telega-sticker--animate-callback sticker for-msg)))

          (stickerFormatWebm
           (telega-ffplay-to-png (telega--tl-get file :local :path)
               (format "-pix_fmt rgba -vf scale=-2:%d -an"
                       (telega-chars-xheight
                        (car (telega-sticker-size sticker))))
             (list #'telega-sticker--animate-callback sticker for-msg)
             ;; NOTE: We use "libvpx-vp9" to decode WEBM with alpha
             ;; channel
             :vcodec "libvpx-vp9"))))
      )))

(defun telega-sticker--gen-sensor-func (sticker)
  "Return sensor function to animate STICKER when entered."
  (cl-assert sticker)
  (lambda (_window _oldpos dir)
    (when (and (not (telega-sticker-static-p sticker))
               telega-sticker-animated-play)
      (if (eq dir 'entered)
          (telega-sticker--animate sticker)
        ;; dir == left
        (telega--cancelDownloadFile (plist-get sticker :sticker))))))


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

(defun telega-animation--progress-svg (animation)
  "Generate svg for STICKER showing download progress."
  (let* ((xh (telega-chars-xheight telega-animation-height))
         (w-chars (telega-chars-in-width xh))
         (xw (telega-chars-xwidth w-chars))
         (svg (telega-svg-create xw xh)))
    (telega-svg-progress svg (telega-file--downloading-progress
                              (telega-animation--file animation)))
    (telega-svg-image
     svg :scale 1.0
     :width xw :height xh
     :ascent 'center
     :mask 'heuristic
     ;; text of correct width
     :telega-text
     (make-string
      (or (car (plist-get animation :telega-image-cwidth-xmargin))
          w-chars)
      ?X))
    ))

(defun telega-animation--create-image (animation &optional _fileignored)
  "Return image for the ANIMATION."
  ;; Cases:
  ;;   1) Next inline animation frame (set in
  ;;   `telega-animation--ffplay-callback') is available - display it
  ;;
  ;;   2) Thumbnail is downloaded, while animation still downloading
  ;;      Show thumbnail (caching temporary), waiting for animation to
  ;;      be downloaded
  ;;
  ;;   2.5) Animation/thumbnail is downloading, minithumbnail is
  ;;        available - display it
  ;;
  ;;   3) Thumbnail and animation downloading
  ;;      Fallback to svg loading image
  (let* ((anim-frame-filename
          (plist-get animation :telega-ffplay-frame-filename))
         (minithumb (plist-get animation :minithumbnail))
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
    (cond ((and anim-frame-filename
                ;; NOTE: Check file for existance, to avoid errors in
                ;; `insert-file-contents-literally' if file does not
                ;; exists for some strange reason
                (file-exists-p anim-frame-filename))
           ;; Remove this prop, because file is about to be deleted
           (plist-put animation :telega-ffplay-frame-filename nil)
           (apply #'telega-create-image
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally anim-frame-filename)
                    (buffer-string))
                  (when (fboundp 'imagemagick-types) 'imagemagick) t img-props))

          ((telega-file--downloaded-p tfile)
           (apply #'telega-create-image
                  (telega--tl-get tfile :local :path)
                  (when (fboundp 'imagemagick-types) 'imagemagick) nil img-props))

          (minithumb
           (telega-minithumb--create-image minithumb telega-animation-height))

          (t (telega-animation--progress-svg animation))))))

(defun telega-ins--animation-image (animation &optional slices-p)
  "Inserter for the ANIMATION.
If SLICES-P is non-nil, then insert ANIMATION using slices."
  (let ((aimage (telega-media--image
                 (cons animation 'telega-animation--create-image)
                 (cons (plist-get animation :thumbnail) :file))))
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

(defun telega-animation--gen-sensor-func (anim)
  "Return sensor function to animate ANIM when entered."
  (cl-assert anim)
  (lambda (_window _oldpos dir)
    (when telega-animation-play-inline
      (if (eq dir 'entered)
          (telega-file--download (telega-file--renew anim :animation) 32
            (lambda (file)
              (when (telega-file--downloaded-p file)
                (telega-ffplay-to-png (telega--tl-get file :local :path) "-an"
                  (list #'telega-animation--ffplay-callback anim)))))

        ;; dir == left
        (telega--cancelDownloadFile (plist-get anim :animation))))))

(defun telega-animation-choose (for-chat animations &optional window-select)
  "Choose one of the ANIMATIONS for insertation into FOR-CHAT."
  (cl-assert for-chat)
  (let ((help-window-select window-select))
    (with-telega-help-win "*Telegram Animations*"
      (setq telega--chat for-chat)

      (seq-doseq (anim animations)
        (telega-ins-prefix (unless (bolp) "\n")
          (telega-button--insert 'telega-animation anim
            'action #'telega-animation--choosen-action
            ;; NOTE: use different sensor functions, so all animations
            ;; can be places next to each other and still sensor
            ;; detection will work properly
            'cursor-sensor-functions
            (list (telega-animation--gen-sensor-func anim)))
          (> (current-column) (current-fill-column))))
      )))

(defun telega-animation-choose-saved (for-chat)
  "Choose saved animation FOR-CHAT."
  (interactive (list telega-chatbuf--chat))
  (telega-animation-choose for-chat (telega--getSavedAnimations) t))

(defun telega-animation-play-inline-p (animation)
  "Return non-nil if ANIMATION should be played inside Emacs.
Its behavior is controlled by `telega-animation-play-inline' and
`telega-open-message-as-file' custom options."
  (unless (memq 'animation telega-open-message-as-file)
    (if (numberp telega-animation-play-inline)
        (>= telega-animation-play-inline
            (or (plist-get animation :duration) 0))
      telega-animation-play-inline)))

(provide 'telega-sticker)

;;; telega-sticker.el ends here

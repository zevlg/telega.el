;;; telega-emoji.el --- Emoji support for telega  -*- lexical-binding: t -*-

;; Copyright (C) 2022 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Aug 26 14:03:57 2022
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
(require 'telega-core)

(declare-function telega-root-view--update "telega-root" (on-update-prop &rest args))
(declare-function telega-msg-redisplay "telega-chat" (msg &optional node))
(declare-function telega-chat--update "telega-tdlib-events" (chat &rest events))

(defvar telega-emoji-alist nil)
(defvar telega-emoji-candidates nil)
(defvar telega-emoji-candidate-max-length 0)
(defvar telega-emoji-string-max-length 0)
(defvar telega-emoji-svg-images nil
  "Cache of SVG images for emojis of one char height.
Alist with elements in form (emoji . image)")

(defun telega-emoji-init ()
  "Initialize emojis."
  (unless telega-emoji-alist
    (setq telega-emoji-alist
          (nconc (with-temp-buffer
                   (insert-file-contents (telega-etc-file "emojis.alist"))
                   (goto-char (point-min))
                   (read (current-buffer)))
                 telega-emoji-custom-alist))
    (setq telega-emoji-candidates (mapcar 'car telega-emoji-alist))
    (setq telega-emoji-candidate-max-length
          (apply 'max (mapcar 'length telega-emoji-candidates)))
    (setq telega-emoji-string-max-length
          (apply 'max (mapcar 'length (mapcar 'cdr telega-emoji-alist))))))

(defun telega-emoji-name (emoji)
  "Find EMOJI name."
  (telega-emoji-init)
  (car (cl-find emoji telega-emoji-alist :test 'string= :key 'cdr)))

(defun telega-emoji-p (string)
  "Return non-nil if STRING represents an emoji."
  (or (telega-emoji-name string)
      (telega-emoji-fe0f-p string)))

(defun telega-emoji-backward ()
  "Move point backward by a single longest emoji."
  (let ((start (point)))
    (backward-char telega-emoji-string-max-length)
    (while (and (< (point) start)
                (not (telega-emoji-p (buffer-substring (point) start))))
      (forward-char 1))))

(defun telega-emoji--image-cache-key (emoji)
  "Return cache key for the EMOJI."
  (if (plist-get emoji :is-premium)
      (concat emoji "###IS_PREMIUM")
    emoji))

(defun telega-emoji--image-cache-get (emoji xheight)
  "Get EMOJI from `telega-emoji-svg-images'.
Also checks that height of the cached image equals to XHEIGHT."
  (when-let ((cached-image (cdr (assoc (telega-emoji--image-cache-key emoji)
                                       telega-emoji-svg-images))))
    (when (eq xheight (plist-get (cdr cached-image) :height))
      cached-image)))

(defun telega-emoji--image-cache-put (emoji image)
  "Put EMOJI IMAGE into `telega-emoji-svg-images' cache."
  (let* ((cache-key (telega-emoji--image-cache-key emoji))
         (cached-image (assoc emoji telega-emoji-svg-images)))
    (if cached-image
        (setcdr cached-image image)
      (setq telega-emoji-svg-images
            (cons (cons cache-key image) telega-emoji-svg-images)))))

(defun telega-emoji-create-svg (emoji &optional cheight)
  "Create svg image for the EMOJI.
If EMOJI has non-nil `:is-premium' text property, then add Telegram
Premium logo at the right bottom corner.
CHEIGHT is height for the svg in characters, default=1."
  (let* ((emoji-cheight (or cheight 1))
         (use-cache-p (and (= 1 (length emoji)) (= emoji-cheight 1)))
         (xh (telega-chars-xheight emoji-cheight))
         (image (when use-cache-p
                  (telega-emoji--image-cache-get emoji xh))))
    (unless image
      (let* ((font-xh (min xh (telega-chars-xwidth (* 2 emoji-cheight))))
             (font-size (- font-xh (/ font-xh 5)))
             (font-y (if (> font-xh xh)
                         font-size
                       (cl-assert (>= xh font-xh))
                       (+ font-size (/ (- xh font-xh) 4))))
             (aw-chars (* (or (telega-emoji-svg-width emoji) (length emoji))
                          (telega-chars-in-width font-xh)))
             (xw (telega-chars-xwidth aw-chars))
             (svg (telega-svg-create xw xh))
             ;; NOTE: if EMOJI width matches final width, then use
             ;; EMOJI itself as telega-text
             (telega-text (if (= (string-width emoji) aw-chars)
                              emoji
                            (make-string aw-chars ?E))))
        ;; NOTE: special case librsvg does not handles well - labels
        ;; such as 1️⃣, for such cases `telega-emoji-svg-width' returns 1
        (if (telega-emoji-keycap-p emoji)
            (progn
              (setq telega-text (compose-chars (aref emoji 0) ?⃣))
              (svg-text svg "⃣"
                        :font-family telega-emoji-font-family
                        :font-size font-size
                        :x 0 :y font-y)
              (svg-text svg (substring emoji 0 1)
                        :font-family telega-emoji-font-family
                        :font-size font-size
                        :x 0 :y font-y))
          (svg-text svg emoji
                    :font-family telega-emoji-font-family
                    :font-size font-size
                    :x 0 :y font-y))

        (when (get-text-property 0 :is-premium emoji)
          (let ((wh-size (round (/ xh 3))))
            (telega-svg-premium-logo svg wh-size
              :transform (format "translate(%f, %f)"
                                 (- xw wh-size) (- xh wh-size)))))

        (setq image (telega-svg-image svg :scale 1.0
                                      :width xw :height xh
                                      :ascent 'center
                                      :mask 'heuristic
                                      :telega-text telega-text)))
      (when use-cache-p
        (telega-emoji--image-cache-put emoji image)))
    image))

(defun telega-emoji-has-zero-joiner-p (emoji)
  "Return non-nil if EMOJI has ZWJ char inside."
  (string-match-p (regexp-quote "\U0000200D") emoji))

(defun telega-emoji-fitz-p (emoji)
  "Return non-nil if EMOJI uses Fitzpatrick's modifier."
  (and (= (length emoji) 2)
       (memq (aref emoji 1) '(?\🏻 ?\🏼 ?\🏽 ?\🏾 ?\🏿))))

(defun telega-emoji-flag-p (emoji)
  "Return non-nil if EMOJI is a flag."
  (and (= (length emoji) 2)
       (>= (aref emoji 0) ?\🇦)
       (>= (aref emoji 1) ?\🇦)
       (<= (aref emoji 0) ?\🇿)
       (<= (aref emoji 1) ?\🇿)))

(defun telega-emoji-fe0f-p (emoji)
  "Return non-nil if EMOJI ends with \ufe0f."
  (and (= (length emoji) 2)
       (= (aref emoji 1) (aref "\ufe0f" 0))))

(defun telega-emoji-keycap-p (emoji)
  "Return non-nil if EMOJI is a keycap emoji."
  (or (and (= (length emoji) 3) (string-suffix-p "\ufe0f⃣" emoji))
      (and (= (length emoji) 2) (string-suffix-p "⃣" emoji))))

(defun telega-emoji-svg-width (emoji)
  (if (or (telega-emoji-fitz-p emoji)
          (telega-emoji-flag-p emoji)
          (telega-emoji-fe0f-p emoji)
          (telega-emoji-has-zero-joiner-p emoji)
          (telega-emoji-keycap-p emoji))
      1
    nil))


;;; Custom Emojis
(defun telega-custom-emoji-get (custom-emoji-id)
  "Return cached custom emoji by CUSTOM-EMOJI-ID."
  (gethash custom-emoji-id telega--custom-emoji-stickers))

(defun telega-custom-emoji-id (sticker)
  "Return custom emoji id for the sticker of the CustomEmoji type."
  (telega--tl-get sticker :full_type :custom_emoji_id))

(defun telega-custom-emoji--ensure (sticker)
  "Ensure custom emoji STICKER is in the custom emojis cache."
  (when-let ((ce-id (telega-custom-emoji-id sticker)))
    ;; NOTE: Use custom `telega-sticker-size' to fit them into 1 char
    ;; height
    (plist-put sticker :telega-sticker-size
               (cons 1 (/ (cdr telega-sticker-size)
                          (car telega-sticker-size))))
    (plist-put sticker :telega-create-image-function
               #'telega-custom-emoji--create-image)
    (puthash ce-id sticker telega--custom-emoji-stickers)
    sticker))

(defun telega-custom-emoji-from-sticker (sticker)
  "Conver STICKER to a custom emoji sticker."
  (or (telega-custom-emoji-get (telega-custom-emoji-id sticker))
      (telega-custom-emoji--ensure sticker)))

(defun telega-custom-emoji-sticker-p (sticker)
  "Return non-nil if STICKER is a custom emoji sticker.
Actually return STICKER's full type info."
  (let ((full-type (plist-get sticker :full_type)))
    (when (eq 'stickerFullTypeCustomEmoji (telega--tl-type full-type))
      full-type)))

(defun telega-custom-emoji--create-image (sticker img-file)
  "Create image for the custom emoji using corresponding STICKER."
  (cond
   ((not img-file)
    (telega-emoji-create-svg (telega-sticker-emoji sticker) 1))
   ((equal "webp" (file-name-extension img-file))
    (telega-create-image
     img-file (when (fboundp 'imagemagick-types) 'imagemagick) nil
     :height (- (telega-chars-xheight 1) 2)
     :scale 1.0 :ascent 'center
     :heuristic-mask t))
   (t
    ;; Embed IMG-FILE into 2x1 svg with transparent background
    (let* ((w (telega-chars-xwidth 2))
           ;; NOTE: give 2 pixels gap for the height
           (h (- (min w (telega-chars-xheight 1)) 2))
           (base-dir (telega-directory-base-uri telega-database-dir))
           (svg (telega-svg-create w h))
           (img-size h)
           (img-x (/ (- w img-size) 2.0))
            ;; NOTE: Colorize themed emoji with the color of the
            ;; Telegram Premium badge
           (mask (when (telega--tl-get sticker :full_type :needs_repainting)
                   (let ((node (dom-node 'mask `((id . "mask")
                                                 (x . 0)
                                                 (y . 0)
                                                 (width . ,w)
                                                 (height . ,h)))))
                     (svg--def svg node)
                     node))))
        (telega-svg-embed (or mask svg)
                          (list (file-relative-name img-file base-dir)
                                base-dir)
                          (format "image/%S"
                                  (telega-image-supported-file-p img-file))
                          nil
                          :x img-x :y 0 :width img-size :height img-size)
        (when mask
          (svg-rectangle svg 0 0 100 100
                         :fill-color (face-foreground 'telega-blue)
                         :mask "url(#mask)"))
        (telega-svg-image svg :scale 1.0 :width w :height h :ascent 'center
                          :base-uri (expand-file-name "dummy" base-dir))))))

(defun telega-custom-emoji--ids-for-msg (msg)
  "Return a list of custom emoji ids extracted from the message MSG."
  (let ((content (plist-get msg :content)))
    (seq-uniq
     (delq nil
           (nconc
            ;; Custom emojis from message's text
            (when-let ((fmt-text (or (plist-get content :text)
                                     (plist-get content :caption))))
              (mapcar (lambda (entity)
                        (let ((entity-type (plist-get entity :type)))
                          (when (eq 'textEntityTypeCustomEmoji
                                    (telega--tl-type entity-type))
                            (plist-get entity-type :custom_emoji_id))))
                      (plist-get fmt-text :entities)))

            ;; Custom emojis from message's reactions
            (mapcar (lambda (reaction)
                      (let ((reaction-type (plist-get reaction :type)))
                        (when (eq (telega--tl-type reaction-type)
                                  'reactionTypeCustomEmoji)
                          (plist-get reaction-type :custom_emoji_id))))
                    (telega--tl-get msg :interaction_info :reactions))

            ;; Custom emojis for special messages
            (cl-case (telega--tl-type content)
              (messageForumTopicCreated
               (list (telega--tl-get content :icon :custom_emoji_id)))
              (messageForumTopicEdited
               (when (plist-get content :edit_icon_custom_emoji_id)
                 (list (plist-get content :icon_custom_emoji_id)))))
            )))))

(defun telega-msg--custom-emojis-fetch (msg)
  "Asynchronously fetch custom emojis for the message MSG.
Redisplay message when custom emojis are loaded.
Do not fetch custom emojis for ignored messages."
  (when-let ((not-ignored-p (telega-msg-match-p msg '(not ignored)))
             (custom-emoji-ids
              (seq-remove #'telega-custom-emoji-get
                          (telega-custom-emoji--ids-for-msg msg))))
    ;; NOTE: Fetch only uncached custom emojis
    (telega--getCustomEmojiStickers custom-emoji-ids
      (lambda (stickers)
        (seq-doseq (sticker stickers)
          (telega-custom-emoji--ensure sticker))
        (telega-msg-redisplay msg)

        ;; NOTE: possible autoplay custom emojis
        (when telega-autoplay-mode
          (telega-autoplay-custom-emojis msg))

        ;; NOTE: message update might affect rootview
        (telega-root-view--update :on-message-update msg)))))

(defun telega-custom-emojis-trends ()
  "Show trending custom emojis."
  (interactive)
  (let ((sticker-sets (telega--getTrendingStickerSets
                       :tl-sticker-type '(:@type "stickerTypeCustomEmoji"))))
    (unless sticker-sets
      (user-error "No trending custom emojis"))

    (telega-stickerset-choose
     (telega-stickerset-completing-read
      "Custom Emojis set: " sticker-sets))))


;;; Emoji status
(defun telega-emoji-statuses-to-stickers (tl-emoji-statuses &optional callback)
  "Asynchronously convert TL-EMOJI-STATUSES to a list of custom emoji stickers."
  (declare (indent 1))
  (let ((known-stickers nil)
        (unknown-ids nil))
    (seq-doseq (es tl-emoji-statuses)
      (let* ((ce-id (plist-get es :custom_emoji_id))
             (ce-sticker (telega-custom-emoji-get ce-id)))
        (if ce-sticker
            (setq known-stickers (cons ce-sticker known-stickers))
          (setq unknown-ids (cons ce-id unknown-ids)))))

    (cond ((and (not callback) (not unknown-ids))
           known-stickers)
          ((not callback)
           (nconc known-stickers
                  (mapcar #'telega-custom-emoji--ensure
                          (telega--getCustomEmojiStickers unknown-ids))))
          (t
           (telega--getCustomEmojiStickers unknown-ids
             (lambda (stickers)
               (funcall callback
                        (nconc known-stickers
                               (mapcar #'telega-custom-emoji--ensure
                                       stickers)))))))
    ))

(defun telega-emoji-status--image (emoji-status &optional _ignoredfile)
  "Return image for the user's EMOJI-STATUS."
  (let* ((custom-emoji-id (plist-get emoji-status :custom_emoji_id))
         (sticker (telega-custom-emoji-get custom-emoji-id))
         image)
    (if sticker
        (setq image (telega-sticker--image sticker))

      ;; Use Premium Logo as emoji status while downloading actual sticker
      (setq image (telega-etc-file-create-image "symbols/premium.svg" 2))
      (telega--getCustomEmojiStickers (list custom-emoji-id)
        (lambda (stickers)
          (cl-assert (= 1 (length stickers)))
          (let ((custom-emoji (car stickers)))
            (telega-custom-emoji--ensure custom-emoji)
            ;; NOTE: image could be nil if `telega-use-images' is nil
            (setq image (telega-sticker--image custom-emoji)))))
      image)))

(defun telega-emoji-status--animate (emoji-status)
  "Animate EMOJI-STATUS."
  (let* ((custom-emoji-id (plist-get emoji-status :custom_emoji_id))
         (sticker (telega-custom-emoji-get custom-emoji-id)))
    (when (and telega-sticker-animated-play
               (not (telega-sticker-static-p sticker)))
      (telega-sticker--animate sticker))))

(defun telega-emoji-status-from-sticker (sticker)
  "Create emoji status from given STICKER."
  (cl-assert (telega-custom-emoji-sticker-p sticker))
  (list :@type "emojiStatus"
        :custom_emoji_id (telega-custom-emoji-id sticker)))

(defun telega-emoji-status-list--gen-ins-callback (custom-action)
  (let ((stickers-callback (telega--gen-ins-continuation-callback 'loading
                             (lambda (stickers)
                               (telega-ins--sticker-list
                                   stickers :custom-action custom-action)))))
    (lambda (emoji-statuses)
      (telega-emoji-statuses-to-stickers emoji-statuses
        stickers-callback))))

(defun telega-sticker-choose-emoji-status (custom-action)
  "Choose emoji status sticker and execute CUSTOM-ACTION on it."
  (let ((help-window-select t))
    (with-telega-help-win "*Telegram Emoji Status*"
      ;; NOTE: use callbacks for async emoji statuses loading
      (telega-ins "Themed:\n")
      (telega--getThemedEmojiStatuses
       (telega-emoji-status-list--gen-ins-callback custom-action))

      (telega-ins "\nRecent:\n")
      (telega--getRecentEmojiStatuses
       (telega-emoji-status-list--gen-ins-callback custom-action))

      ;; Sticker sets with custom emojis
      (dolist (sset-info telega--stickersets-custom-emojis)
        (cl-assert (eq (telega--tl-type (plist-get sset-info :sticker_type))
                       'stickerTypeCustomEmoji))
        (telega-ins "\n" (telega-stickerset-title sset-info 'raw) ":\n")
        (let ((cb (telega-emoji-status-list--gen-ins-callback custom-action)))
          (telega--getStickerSet (plist-get sset-info :id)
            (lambda (sset)
              (funcall cb (mapcar #'telega-emoji-status-from-sticker
                                  (plist-get sset :stickers)))))))

      (telega-ins "\nDefault:\n")
      (telega--getDefaultEmojiStatuses
       (telega-emoji-status-list--gen-ins-callback custom-action))
      )))


(defun telega-ins--custom-emoji-stickersets (custom-action)
  "Insert custom emoji stickersets to execute CUSTOM-ACTION when choosen."
  (dolist (sset-info telega--stickersets-custom-emojis)
    (cl-assert (eq (telega--tl-type (plist-get sset-info :sticker_type))
                   'stickerTypeCustomEmoji))
    (telega-ins (telega-stickerset-title sset-info 'raw) ":\n")
    (let ((cb (telega--gen-ins-continuation-callback 'loading
                (lambda (stickers)
                  (telega-ins--sticker-list
                      stickers :custom-action custom-action)))))
      (telega-stickerset-get (plist-get sset-info :id) nil
        (lambda (sset)
          (funcall cb (mapcar #'telega-custom-emoji-from-sticker
                              (plist-get sset :stickers))))))
    (telega-ins "\n")))

(defun telega-custom-emoji-choose (custom-action)
  "Choose custom emoji and execute CUSTOM-ACTION on it."
  (let ((help-window-select t))
    (with-telega-help-win "*Telegram Custom Emojis*"
      (telega-ins--custom-emoji-stickersets custom-action)
      )))


;;; Emojis using stickers
(defvar telega-emoji--emojis-sticker-set nil
  "StickerSet used to lookup emojis in it.")

(defun telega-emoji-from-sticker (emoji &optional cheight)
  "Create image "
  (unless cheight
    (setq cheight 1))

  (if-let ((sticker (cl-find emoji (plist-get telega-emoji--emojis-sticker-set
                                              :stickers)
                             :key #'telega-sticker-emoji
                             :test #'equal)))
      (telega-sticker--image
       sticker
       (lambda (sticker-arg &optional file)
         (let ((telega-sticker-size (cons cheight (* 2 cheight))))
           (telega-sticker--create-image sticker-arg file)))
       (intern (format ":telega-emoji-%d" cheight)))

    ;; Fallback to svg version
    (telega-emoji-create-svg emoji cheight)))

(defun telega-emoji-create-image (emoji &optional cheight)
  "Create image for the EMOJI."
  )

(provide 'telega-emoji)

;;; telega-emoji.el ends here

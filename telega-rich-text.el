;;; telega-rich-text.el --- Support for Rich Text  -*- lexical-binding: t -*-

;; Copyright (C) 2026 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Mon Jun 22 10:20:30 2026
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

(declare-function telega-webpage--add-anchor "telega-webpage" (name))


(defvar telega-rich-text--block-quote-p nil
  "Bind to non-nil if inside block quote.")

(defmacro telega-rich-text--ins-block (&rest body)
  `(telega-ins-from-newline
    (prog1
        (progn ,@body)
      (unless (bolp)
        (telega-ins "\n")))))

(defun telega-rich-text--ins-divider (width)
  "Insert delimiter of WIDTH."
  (telega-ins--with-face 'telega-rich-text-strike-through
    (telega-ins (make-string width ?\s)))
  (telega-ins "\n"))

(defun telega-rich-text--ins-bot-command ()
  )

(defun telega-rich-text--link-button-action (button)
  "Action for rich text link buttons."
  (let ((link (button-get button :telega-link)))
    (cl-ecase (car link)
      (:bot_command
       (message "BOT command: %S" (cdr link)))
      (:username
       (message "Open username: %S" (cdr link)))
      (:hashtag
       (message "Open hashtag: %S" (cdr link)))
      (:cashtag
       (message "Open cashtag: %S" (cdr link)))
      (:user_id
       (message "Open user: %S" (cdr link)))
      )))

(defun telega-rich-text-icon--create-image (obj-spec)
  "Create image function for richTextIcon OBJ-SPEC."
  (let* ((rt (plist-get obj-spec :object))
         (cheight (plist-get obj-spec :cheight))
         (doc (plist-get rt :document))
         (doc-file (telega-file--renew doc :document))
         (thumb (plist-get doc :thumbnail))
         (thumb-file (telega-file--renew thumb :photo)))
    ;; 1) DOC-FILE downloaded, show DOC-FILE
    ;; 2) Thumbnail is downloaded, use it
    ;; 3) Download DOC-FILE, fallback to progress svg
    (cond ((telega-file--downloaded-p doc-file)
           (telega-media--create-image doc-file width height cheight))
          ((telega-file--downloaded-p thumb-file)
           (telega-thumb--create-image thumb thumb-file cheight))
          (t
           (telega-file--download doc-file
             :priority 32
             :update-callback
             (lambda (_dfile)
               ;; Update progress
               (telega-media--image-updateNEW obj-spec)
               (force-window-update)))
           (telega-media--progress-svg doc-file width height cheight)))))

(defun telega-rich-text--math-exp-create-image (string &optional cheight)
  "Create an image with math expression STRING."
  (when-let* ((cheight (or cheight 1))
              (preview-backend
               (seq-find (lambda (backend)
                           (seq-every-p #'executable-find 
                                        (plist-get (cdr backend) :programs)))
                         org-preview-latex-process-alist))
              (f-ext (plist-get (cdr preview-backend) :image-output-type))
              (f-name (telega-temp-name "rich-text-math" (concat "." f-ext))))
  (with-temp-buffer
    (org-create-formula-image string f-name nil nil (car preview-backend))
    (telega-create-image f-name nil nil
      :height (telega-ch-height cheight)
      :telega-nslices cheight
      :scale 1.0
      :mask 'heuristic
      :ascent 'center))))

(defun telega-rich-text--ins-rt (rt)
  (when rt
    (cl-case (telega--tl-type rt)
      (richTextPlain
       (telega-ins (telega-tl-str rt :text)))
      (richTextBold
       (telega-ins--with-face 'bold
         (telega-rich-text--ins-rt (plist-get rt :text))))
      (richTextItalic
       (telega-ins--with-face 'italic
         (telega-rich-text--ins-rt (plist-get rt :text))))
      (richTextUnderline
       (telega-ins--with-face 'underline
         (telega-rich-text--ins-rt (plist-get rt :text))))
      (richTextStrikethrough
       (telega-ins--with-face 'telega-webpage-strike-through
         (telega-rich-text--ins-rt (plist-get rt :text))))
      (richTextSpoiler
       (if (plist-get telega-msg--current :telega-text-spoiler-removed)
           (telega-rich-text--ins-rt (plist-get rt :text))
         (telega-ins (with-temp-buffer
                          (telega-rich-text--ins-rt (plist-get rt :text))
                          (translate-region (point-min) (point-max)
                                            telega-spoiler-translation-table)
                          (buffer-string)))))
      (richTextDateTime
       ;; TODO: use `telega-ins--date-time-formatting'
       (telega-ins "<TODO: richTextDateTime>")
       )
      (richTextMention
       (telega-ins--with-props
           (list 'action #'telega-rich-text--link-button-action
                 :telega-link (cons :username (plist-get rt :username)))
         (telega-ins--with-face 'telega-entity-type-mention
           (telega-rich-text--ins-rt (plist-get rt :text)))))
      (richTextHashtag
       (telega-ins--with-props
           (list 'action #'telega-rich-text--link-button-action
                 :telega-link (cons :hashtag (plist-get rt :hashtag)))
         (telega-ins--with-face 'telega-link
           (telega-rich-text--ins-rt (plist-get rt :text)))))
      (richTextCashtag
       (telega-ins--with-props
           (list 'action #'telega-rich-text--link-button-action
                 :telega-link (cons :cashtag (plist-get rt :cashtag)))
         (telega-ins--with-face 'telega-link
           (telega-rich-text--ins-rt (plist-get rt :text)))))
      (richTextBotCommand
       (telega-ins--with-props
           (list 'action #'telega-rich-text--link-button-action
                 :telega-link (cons :bot_command (plist-get rt :bot_command)))
         (telega-ins--with-face 'telega-entity-type-mention
           (telega-rich-text--ins-rt (plist-get rt :text)))))
      (richTextFixed
       (telega-ins--with-face 'telega-webpage-fixed
         (telega-rich-text--ins-rt (plist-get rt :text))))
      (richTextMentionName
       (telega-ins--with-props
           (list 'action #'telega-rich-text--link-button-action
                 :telega-link (cons :user_id (plist-get rt :user_id)))
         (telega-ins--with-face 'telega-entity-type-mention
           (telega-rich-text--ins-rt (plist-get rt :text)))))
      (richTextUrl
       (let ((url (telega-tl-str rt :url)))
         (telega-ins--raw-button
             (list 'action #'telega-button--action
                   :help-echo (format "URL: %s%s" url
                                      (if (plist-get rt :is_cached)
                                          ", has IV"
                                        ""))
                   :value url
                   :action #'telega-browse-url)
           (telega-ins--with-face 'telega-link
             (telega-rich-text--ins-rt (plist-get rt :text))))))
      (richTextEmailAddress
       ;; TODO: use `:email_address'
       (telega-ins--with-face 'telega-link
         (telega-rich-text--ins-rt (plist-get rt :text))))
      (richTextBankCardNumber
       ;; TODO: use `:bank_card_number'
       (telega-ins--with-face 'telega-link
         (telega-rich-text--ins-rt (plist-get rt :text))))
      (richTextSubscript
       (telega-ins--with-props '(display (raise -0.25))
         (telega-ins--with-face '(:height 0.5)
           (telega-rich-text--ins-rt (plist-get rt :text)))))
      (richTextSuperscript
       (telega-ins--with-props '(display (raise 0.75))
         (telega-ins--with-face '(:height 0.5)
           (telega-rich-text--ins-rt (plist-get rt :text)))))
      (richTextMarked
       (telega-ins--with-face 'telega-webpage-marked
         (telega-rich-text--ins-rt (plist-get rt :text))))
      (richTextPhoneNumber
       ;; TODO: use `:phone_number'
       (telega-ins--with-face 'telega-link
         (telega-rich-text--ins-rt (plist-get rt :text))))
      (richTextCustomEmoji
       (telega-ins--image
        (telega-custom-emoji--image rt
          :alt-text (telega-tl-str rt :alternative_text))))
      (richTextIcon
       (let ((cheight (telega-media--cheight-for-limits
                       (plist-get rt :width) (plist-get rt :height)
                       (list 1 1 (nth 2 telega-webpage-photo-size-limits)
                             (nth 3 telega-webpage-photo-size-limits)))))
         (telega-ins--image-slices
             (telega-media--imageNEW rt #'telega-rich-text-icon--create-image
               :cheight cheight))))
      (richTextMathematicalExpression
       (if-let ((mimage (or (plist-get rt :telega-image)
                            (telega-rich-text--math-exp-create-image
                             (telega-tl-str rt :expression)))))
           (progn
             (plist-put rt :telega-image mimage)
             (telega-ins--image-slices mimage))
         (telega-ins (telega-tl-str rt :expression))))
      (richTextReference
       ;; TODO: use `:name'
       (telega-ins--with-face 'telega-link
         (telega-rich-text--ins-rt (plist-get rt :text))))
      (richTextReferenceLink
       ;; TODO: use `:reference_name'
       (let ((ref-url (telega-tl-str rt :url)))
         (telega-ins--raw-button
             (list 'action #'telega-button--action
                   :help-echo (concat "Reference: " ref-url)
                   :value ref-url
                   :action #'telega-browse-url)
           (telega-ins--with-face 'telega-link
             (telega-rich-text--ins-rt (plist-get rt :text))))))
      (richTextAnchor
       (telega-ins--with-props
           (list :telega-anchor (telega-tl-str rt :name)
                 'invisible t)
         (telega-ins " ")))
;       (telega-webpage--add-anchor (telega-tl-str rt :name)))
      (richTextAnchorLink
       ;; TDLib 1.6.2
       (let ((anchor (telega-tl-str rt :anchor_name)))
         (telega-ins--raw-button
             (list 'action #'telega-button--action
                   :help-echo (concat "Anchor: #" anchor)
                   :value anchor
                   :action #'telega-webpage-goto-anchor)
           (telega-ins--with-face 'telega-link
             (telega-rich-text--ins-rt (plist-get rt :text))))))
      (richTexts
       (seq-doseq (rt-item (plist-get rt :texts))
         (telega-rich-text--ins-rt rt-item)))

      (t
       (telega-ins-fmt "<TODO: telega-rich-text--ins-rt for %S>"
         (telega--tl-type rt))))
    t))

(defun telega-rich-text--ins-pb-details (pb)
  "Inserter for `pageBlockDetails' page block PB."
  (let ((open-p (plist-get pb :is_open)))
    (telega-ins--with-face 'telega-shadow
      (telega-ins (telega-symbol (if open-p 'outline-open 'outline-close))
                  " "))
    (telega-rich-text--ins-rt (plist-get pb :header))
    (when open-p
      (seq-doseq (pb-item (plist-get pb :blocks))
        (telega-rich-text--ins-pb pb-item)))
    t))

(defun telega-rich-text--ins-pb (pb &optional _msg)
  "Inserter for the page block PB."
  (when pb
    (cl-case (telega--tl-type pb)
      (pageBlockTitle
       (telega-ins--with-face 'telega-webpage-title
         (telega-rich-text--ins-rt (plist-get pb :title))))
      (pageBlockSubtitle
       (telega-ins--with-face 'telega-webpage-subtitle
         (telega-rich-text--ins-rt (plist-get pb :subtitle))))
      (pageBlockAuthorDate
       (telega-ins--with-face 'telega-shadow
         (telega-ins-prefix "By "
           (when (telega-rich-text--ins-rt (plist-get pb :author))
             (telega-ins " • ")))
         (let ((publish-date (plist-get pb :publish_date)))
           (when (zerop publish-date)
             (setq publish-date (time-to-seconds)))
           (telega-ins--date publish-date 'date-long))))
      (pageBlockHeader
       (telega-ins--with-face 'telega-webpage-header
         (telega-rich-text--ins-rt (plist-get pb :header))))
      (pageBlockSubheader
       (telega-ins--with-face 'telega-webpage-subheader
         (telega-rich-text--ins-rt (plist-get pb :subheader))))
      (pageBlockSectionHeading
       (telega-ins-from-newline
        (telega-ins--with-face `(telega-rich-text-section-heading
                                 (:height ,(nth (1- (plist-get pb :size))
                                                '(1.3 1.15 1.0 0.85 0.7 0.55))))
          (telega-ins--with-props '(line-height (1.25 1.25))
            (telega-rich-text--ins-rt (plist-get pb :text))
            (unless (bolp)
              (telega-ins "\n"))))))
      (pageBlockKicker
       (telega-rich-text--ins-rt (plist-get pb :kicker)))
      (pageBlockParagraph
       (telega-rich-text--ins-block
        (telega-rich-text--ins-rt (plist-get pb :text))))
      (pageBlockPreformatted
       (telega-ins-from-newline
        (telega-ins--with-face 'telega-webpage-preformatted
          (telega-rich-text--ins-rt (plist-get pb :text))
          (unless (bolp)
            (telega-ins "\n")))))
      (pageBlockFooter
       (telega-ins--with-face 'telega-shadow
         (telega-ins (make-string (/ telega-webpage-fill-column 2) ?-) "\n")
         (telega-rich-text--ins-rt (plist-get pb :footer))))
      (pageBlockThinking
       (telega-ins-from-newline
        (telega-ins--with-face 'telega-shadow
          (telega-rich-text--ins-rt (plist-get pb :text)))))
      (pageBlockDivider
       (telega-rich-text--ins-block
        (telega-ins (make-string (/ telega-webpage-fill-column 4) ?\s))
        (telega-rich-text--ins-divider (/ telega-webpage-fill-column 2))))
      (pageBlockMathematicalExpression
       (if-let ((mimage (or (plist-get pb :telega-image)
                            (telega-rich-text--math-exp-create-image
                             (telega-tl-str pb :expression) 2))))
           (progn
             (plist-put pb :telega-image mimage)
             (telega-ins--image-slices mimage))
         (telega-ins (telega-tl-str pb :expression))))
      (pageBlockAnchor
       (telega-webpage--add-anchor (plist-get pb :name)))
      (pageBlockList
       (seq-doseq (pb-item (plist-get pb :items))
         (telega-rich-text--ins-pb pb-item)))
      (pageBlockBlockQuote
       (telega-ins-from-newline
        (telega-ins--line-wrap-prefix (telega-symbol 'vbar-left)
          (let ((telega-rich-text--block-quote-p t))
            (seq-doseq (quote-pb (plist-get pb :blocks))
              (telega-rich-text--ins-pb quote-pb)))
          (when-let ((credit-rt (plist-get pb :credit)))
            (telega-ins-from-newline
             (telega-ins--with-face 'telega-shadow
               (telega-rich-text--ins-rt credit-rt))))
          (unless (bolp)
            (telega-ins "\n")))
        ;; Add extra line space to last newline, so consequent block
        ;; quotes can be recognized
        (unless telega-rich-text--block-quote-p
          (add-text-properties (1- (point)) (point) '(line-height (1 1.25))))
        ))
      (pageBlockPullQuote
       (telega-ins-from-newline
        (telega-rich-text--ins-divider telega-webpage-fill-column)
        (telega-ins "\u00A0\u00A0")
        (telega-ins--with-attrs
            (list :fill 'center
                  :fill-column (- telega-webpage-fill-column 2)
                  :fill-prefix "\u00A0\u00A0")
          (telega-ins--with-face 'telega-rich-text-pull-quote
            (telega-rich-text--ins-rt (plist-get pb :text)))
          (when-let ((credit-rt (plist-get pb :credit)))
            (telega-ins "\n")
            (telega-ins--with-face 'telega-shadow
              (telega-rich-text--ins-rt credit-rt)))))
       (telega-ins-from-newline
        (telega-rich-text--ins-divider telega-webpage-fill-column)))
      (pageBlockAnimation
       (telega-webpage--ins-animation (plist-get pb :animation)))
      (pageBlockAudio
       (telega-rich-text--ins-block
        (telega-ins--audio nil (plist-get pb :audio))
        (telega-ins-from-newline
         (telega-rich-text--ins-pb (plist-get pb :caption)))))
      (pageBlockPhoto
       ;; TODO: `:has_spoiler'
       (telega-ins-from-newline
        (telega-button--insert 'telega (plist-get pb :photo)
          :inserter (lambda (photo)
                      (telega-ins--photo
                       photo nil telega-webpage-photo-size-limits))
          'action (lambda (_button)
                    (if-let ((photo-url (telega-tl-str pb :url)))
                        (telega-browse-url photo-url)
                      (telega-photo--open (plist-get pb :photo)))))
        (telega-ins "\n")
        (telega-rich-text--ins-pb (plist-get pb :caption))))
      (pageBlockVideo
        ;; TODO: `:need_autoplay', `:is_looped', `:has_spoiler'
       (telega-rich-text--ins-block
        (telega-ins--video nil (plist-get pb :video) 'thumbnail)
        (telega-ins-from-newline
         (telega-rich-text--ins-pb (plist-get pb :caption)))))
      (pageBlockVoiceNote
       (telega-ins-from-newline
        (telega-ins--voice-note nil (plist-get pb :voice_note))
        (telega-ins-from-newline
         (telega-rich-text--ins-pb (plist-get pb :caption)))))
      (pageBlockCover
       (telega-rich-text--ins-pb (plist-get pb :cover)))
      (pageBlockEmbedded
       (telega-button--insert 'telega pb
         :inserter (lambda (pb-embedded)
                     (let ((url (telega-tl-str pb-embedded :url))
                           (html (plist-get pb-embedded :html))
                           (poster-photo (plist-get pb-embedded :poster_photo)))
                       (when (telega-ins--with-face '(bold telega-link)
                               (telega-ins url))
                         (telega-ins "\n"))
                       ;; TODO: use `shr' to render html
                       (when (telega-ins--with-face 'telega-webpage-fixed
                               (telega-ins html))
                         (telega-ins "\n"))
                       (when poster-photo
                         (telega-ins--photo
                          poster-photo nil telega-webpage-photo-size-limits))
                       (telega-ins-from-newline
                        (telega-rich-text--ins-pb
                         (plist-get pb-embedded :caption)))
                       ))
         :action (lambda (pb-embedded)
                   (when-let ((url (telega-tl-str pb-embedded :url)))
                     (telega-browse-url url)))))
      (pageBlockEmbeddedPost
       (telega-ins "<TODO: pageBlockEmbeddedPost>\n"))
      (pageBlockCollage
       (seq-doseq (pb-item (plist-get pb :blocks))
         (telega-rich-text--ins-pb pb-item))
       (telega-ins-from-newline
        (telega-rich-text--ins-pb (plist-get pb :caption))))
      (pageBlockSlideshow
       (let ((page-blocks (plist-get pb :blocks)))
         (dotimes (n (length page-blocks))
           (telega-ins-from-newline
            (telega-rich-text--ins-pb (aref page-blocks n))
            (telega-ins-fmt "%d/%d\n" (1+ n) (length page-blocks)))))
       (telega-rich-text--ins-pb (plist-get pb :caption)))
      (pageBlockChatLink
       (telega-ins--with-face 'telega-webpage-chat-link
         (telega-ins (telega-tl-str pb :title) " "
                     "@" (telega-tl-str pb :username) " ")
         (telega-ins--ui-button (telega-i18n "lng_open_link")
           :value (telega-tl-str pb :username)
           :action 'telega-tme-open-username)
         (telega-ins "\n")))
      (pageBlockTable
       (telega-ins-from-newline
        (telega-ins "<TODO: pageBlockTable>\n")
        (telega-ins-from-newline
         (telega-rich-text--ins-rt (plist-get pb :caption)))))
      (pageBlockDetails
       (telega-button--insert 'telega pb
         'action (lambda (button)
                   (let* ((val (button-get button :value))
                          (new-val (plist-put val :is_open
                                              (not (plist-get val :is_open)))))
                     ;; TODO: actually open details
                     (save-excursion
                       (telega-button--update-value button new-val))))
         :inserter #'telega-rich-text--ins-pb-details
         'help-echo "Toggle details")
       (telega-ins-from-newline
        (telega-rich-text--ins-divider telega-webpage-fill-column)))
      (pageBlockRelatedArticles
       (telega-ins-from-newline
        (telega-ins--with-face '(telega-webpage-outline bold)
          (telega-rich-text--ins-rt (plist-get pb :header)))
        (seq-doseq (article-pb (plist-get pb :articles))
          (telega-ins-from-newline
           (telega-button--insert 'telega article-pb
             :inserter 'telega-webpage--ins-related-article
             :action (lambda (_pbignored)
                       (telega-browse-url (telega-tl-str article-pb :url)))
             :help-echo (concat "URL: " (telega-tl-str article-pb :url))))))
       (unless (bolp)
         (telega-ins "\n")))
      (pageBlockMap
       (telega-ins-from-newline
        (telega-ins "<TODO: pageBlockMap>")
        (telega-ins-from-newline
         (telega-rich-text--ins-pb (plist-get pb :caption)))))

      ;; Special page blocks
      (pageBlockListItem
       (telega-ins-from-newline
        (let ((label (or (when (plist-get pb :has_checkbox)
                           (if (plist-get pb :is_checked)
                               (telega-symbol 'checkbox-on)
                             (telega-symbol 'checkbox-off)))
                         (telega-tl-str pb :label)
                         "•")))
          (telega-ins--line-wrap-prefix
              (cons (concat label " ")
                    (concat (make-string (string-width label) ?\s " ") " "))
            (seq-doseq (pb-item (plist-get pb :blocks))
              (telega-rich-text--ins-pb pb-item))))))
      (pageBlockCaption
       (telega-ins--with-face 'telega-shadow
         (telega-rich-text--ins-rt (plist-get pb :text))
         (telega-ins-prefix " • "
           (telega-rich-text--ins-rt (plist-get pb :credit)))))
      )
    t))

(provide 'telega-rich-text)

;;; telega-rich-text.el ends here

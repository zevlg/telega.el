;;; telega-webpage.el --- Webpage viewer  -*- lexical-binding:t -*-

;; Copyright (C) 2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Jan  8 15:27:03 2019
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

;;; Testing:

;; Large page: https://telegram.org/faq
;;

;;; Code:
(require 'cl-lib)
(require 'url-util)
(require 'visual-fill-column)

(require 'telega-server)
(require 'telega-ins)
(require 'telega-media)
(require 'telega-tme)
(require 'telega-customize)

(defvar telega-webpage-history nil
  "History of viewed webpages.")
(defvar telega-webpage-history--index 0
  "Nth element in `telega-webpage-history' we currently active.")
(defvar telega-webpage-history--ignore nil
  "Bind this to non-nil to ignore pushing to the history.")
(defvar telega-webpage-strip-nl nil
  "Bind to non-nil to strip trailing newlines.")

(defvar telega-webpage--url nil
  "URL for the instant view webpage currently viewing.")
(defvar telega-webpage--sitename nil
  "Sitename for the webpage currently viewing.")
(defvar telega-webpage--iv nil
  "Instant view for the current webpage.")
(defvar telega-webpage--anchors nil)
(defvar telega-webpage--slides nil)

(defun telega-webpage--history-push ()
  "Push current webpage instant view into the history."
  (unless telega-webpage-history--ignore
    (cl-assert telega-webpage--iv nil "No current instant view")

    ;; truncate history
    (cond ((and telega-webpage-history
                (string= (cadar telega-webpage-history) telega-webpage--url))
           ;; NOTE: do not push same url twice into history
           (setq telega-webpage-history (cdr telega-webpage-history)))
          ((> (length telega-webpage-history) telega-webpage-history-max)
           (setq telega-webpage-history (butlast telega-webpage-history))))

    (push (list (if (eq major-mode 'telega-webpage-mode) (point) 0)
                telega-webpage--url telega-webpage--sitename
                telega-webpage--iv)
          telega-webpage-history)
    (setq telega-webpage-history--index 0)))

(defun telega-webpage--history-show (n)
  "Show N's instant view from history."
  (cl-assert (and (>= n 0) (< n (length telega-webpage-history))))
  (let ((hiv (nth n telega-webpage-history))
        (telega-webpage-history--ignore t))
    (apply 'telega-webpage--instant-view (cdr hiv))
    (goto-char (car hiv)))
  (setq telega-webpage-history--index n))

(defun telega-webpage-history-next (&optional n)
  "Goto N previous word in history."
  (interactive "p")
  (let ((idx (- telega-webpage-history--index n)))
    (unless (>= idx 0)
      (error "No next webpage in history"))
    (telega-webpage--history-show idx)))

(defun telega-webpage-history-prev (&optional n)
  "Goto N next word in history."
  (interactive "p")
  (let ((idx (+ telega-webpage-history--index n)))
    (unless (< idx (length telega-webpage-history))
      (error "No previous webpage in history"))
    (telega-webpage--history-show idx)))

(defun telega-msg-button--iv-action (button)
  "Open instant view when BUTTON is pressed."
  (let* ((msg (button-get button :value))
         (web-page (telega--tl-get msg :content :web_page)))
    (funcall 'telega-webpage--instant-view (plist-get web-page :url)
             (plist-get web-page :site_name))))

(defun telega--getWebPageInstantView (url &optional partial)
  "Return instant view for the URL.
Return nil if URL is not available for instant view."
  (let ((reply (telega-server--call
                (list :@type "getWebPageInstantView"
                      :url url
                      :force_full (or (not partial) :false)))))
    ;; NOTE: May result in 404 error, return nil in this case
    (and reply
         (eq (telega--tl-type reply) 'webPageInstantView)
         reply)))

(defvar telega-webpage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'telega-webpage-browse-url)
    (define-key map "w" 'telega-webpage-browse-url)
    (define-key map "c" 'telega-webpage-copy-url)
    (define-key map "l" 'telega-webpage-history-prev) ;as in Info
    (define-key map "r" 'telega-webpage-history-next) ;as in Info
    (define-key map "p" 'telega-webpage-history-prev)
    (define-key map "n" 'telega-webpage-history-next)
    (define-key map [?\t] 'telega-button-forward)
    (define-key map [backtab] 'telega-button-backward)
    map))

(define-derived-mode telega-webpage-mode special-mode "Telega-WebPage"
  "The mode for instant viewing webpages in telega.
Keymap:
\\{telega-webpage-mode-map}"
  :group 'telega
  (setq-local nobreak-char-display nil)
  (setq header-line-format telega-webpage-header-line-format)
  (set-buffer-modified-p nil))

(defun telega-webpage-copy-url (url)
  "Copy current webpage URL into clipboard."
  (interactive (list telega-webpage--url))
  (kill-new url)
  (message "Copied \"%s\" into clipboard" url))

(defun telega-webpage-browse-url (url)
  "Browse URL with web browser."
  (interactive (list telega-webpage--url))
  (telega-browse-url url 'in-web-browser))

(defun telega-webpage-goto-anchor (name)
  "Goto at anchor NAME position."
  (let ((anchor (cdr (assoc name telega-webpage--anchors))))
    (unless anchor
      (error (format "Anchor \"#%s\" not found" name)))
    (goto-char anchor)))

(defun telega-webpage-details-toggle (button)
  "Toggle open/close state of the details block."
  (interactive (list (button-at (point))))
  (let ((val (button-get button :value)))
    (telega-button--update-value
     button (plist-put val :is_open (not (plist-get val :is_open)))
     'action 'telega-webpage-details-toggle
     :inserter 'telega-webpage--ins-details
     :help-echo "Toggle details")
    (goto-char button)))

(defun telega-webpage--ins-pb-details (pb)
  "Inserter for `pageBlockDetails' page block PB."
  (let ((open-p (or t (plist-get pb :is_open))) ;XXX always open
        (telega--current-buffer (current-buffer)))
    (telega-ins (funcall (if open-p 'cdr 'car)
                         telega-symbol-webpage-details) " ")
    (telega-webpage--ins-rt (plist-get pb :header))
    (telega-ins "\n")
    (telega-ins--with-face 'telega-webpage-strike-through
      (telega-ins (make-string (/ telega-webpage-fill-column 2) ?\s)))
    (telega-ins "\n")

    (when open-p
      (mapc 'telega-webpage--ins-pb (plist-get pb :page_blocks)))
    ))

(defun telega-webpage-rticon--image (rt limits)
  "Return image representing rich text icon for RT."
  (let* ((doc (plist-get rt :document))
         (width (plist-get rt :width))
         (height (plist-get rt :height))
         (cheight (telega-media--cheight-for-limits width height limits))
         (create-image-fun
          (progn
            (cl-assert (<= cheight (nth 3 limits)))
            (lambda (_rtignored &optional _fileignored)
              ;; 1) FILE downloaded, show FILE
              ;; 2) Thumbnail is downloaded, use it
              ;; 3) FILE downloading, fallback to progress svg
              (let ((doc-file (telega-file--renew doc :document)))
                (if (telega-file--downloaded-p doc-file)
                    (telega-media--create-image doc-file width height cheight)
                  (let* ((thumb (plist-get doc :thumbnail))
                         (thumb-file (telega-file--renew thumb :photo)))
                    (if (telega-file--downloaded-p thumb-file)
                        (telega-thumb--create-image thumb thumb-file cheight)
                      (telega-media--progress-svg
                       doc-file width height cheight)))))))))

    (telega-media--image
     (cons rt create-image-fun)
     (cons doc :document)
     'force-update)))

(defun telega-webpage--add-anchor (name)
  "Add anchor with the NAME to point at POS position."
  (setf (alist-get name telega-webpage--anchors nil nil 'equal)
        (with-current-buffer telega--current-buffer
          (point-marker))))

(defun telega-webpage--ins-rt (rt)
  "Insert RichText RT."
  (cl-ecase (telega--tl-type rt)
    (richTextAnchor
     (telega-webpage--add-anchor (plist-get rt :name))
     (when-let ((anchor-rt (plist-get rt :text)))
       ;; NOTE: compatibility with TDLib 1.6.0
       (telega-webpage--ins-rt anchor-rt)))
    (richTextReference
     ;; TDLib 1.6.2
     (let ((ref-url (plist-get rt :url)))
       (telega-ins--raw-button
           (list 'face 'telega-link
                 'action #'telega-button--action
                 :help-echo (concat "Reference: " ref-url)
                 :value ref-url
                 :action #'telega-browse-url)
         (telega-webpage--ins-rt (plist-get rt :text)))))
    (richTextAnchorLink
     ;; TDLib 1.6.2
     (let ((anchor (plist-get rt :name)))
       (telega-ins--raw-button
           (list 'face 'telega-link
                 'action #'telega-button--action
                 :help-echo (concat "Anchor: #" anchor)
                 :value anchor
                 :action #'telega-webpage-goto-anchor)
       (telega-webpage--ins-rt (plist-get rt :text)))))
    (richTextIcon
     (telega-ins--image-slices
      (telega-webpage-rticon--image rt telega-photo-size-limits)))
    (richTextPlain
     (telega-ins (funcall (if telega-webpage-strip-nl
                              #'telega-strip-newlines
                            #'identity)
                          (telega-tl-str rt :text))))
    (richTexts
     (mapc #'telega-webpage--ins-rt (plist-get rt :texts)))
    (richTextBold
     (telega-ins--with-attrs (list :face 'bold)
       (telega-webpage--ins-rt (plist-get rt :text))))
    (richTextItalic
     (telega-ins--with-attrs (list :face 'italic)
       (telega-webpage--ins-rt (plist-get rt :text))))
    (richTextUnderline
     (telega-ins--with-attrs (list :face 'underline)
       (telega-webpage--ins-rt (plist-get rt :text))))
    (richTextStrikethrough
     (telega-ins--with-attrs (list :face 'telega-webpage-strike-through)
       (telega-webpage--ins-rt (plist-get rt :text))))
    (richTextFixed
     (telega-ins--with-attrs (list :face 'telega-webpage-fixed)
       (telega-webpage--ins-rt (plist-get rt :text))))
    (richTextUrl
     (let ((url (plist-get rt :url)))
       (telega-ins--raw-button
           (list 'face 'telega-link
                 'action #'telega-button--action
                 :help-echo (concat "URL: " url)
                 :value url
                 :action #'telega-browse-url)
         (telega-webpage--ins-rt (plist-get rt :text)))))
    (richTextEmailAddress
     (telega-ins--with-attrs (list :face 'link)
       (telega-webpage--ins-rt (plist-get rt :text))))
    (richTextSubscript
     (telega-ins--with-props '(display (raise -0.25))
       (telega-ins--with-face '(:height 0.5)
         (telega-webpage--ins-rt (plist-get rt :text)))))
    (richTextSuperscript
     (telega-ins--with-props '(display (raise 0.75))
       (telega-ins--with-face '(:height 0.5)
         (telega-webpage--ins-rt (plist-get rt :text)))))
    (richTextMarked
     (telega-ins--with-attrs (list :face 'region)
       (telega-webpage--ins-rt (plist-get rt :text))))
    ))

(defun telega-webpage--ins-related-article (pageblock)
  "Inserter for pageBlockRelatedArticle PAGEBLOCK."
  (let* ((pb-photo (plist-get pageblock :photo))
         (photo-image (when pb-photo
                        (telega-photo--image pb-photo (list 10 3 10 3))))
         (url (plist-get pageblock :url))
         (title (plist-get pageblock :title))
         (author (plist-get pageblock :author))
         (publish-date (plist-get pageblock :publish_date)))
    (telega-ins--with-attrs (list :max telega-webpage-fill-column
                                  :elide t)
      (when photo-image
        (telega-ins--image photo-image 0))
      (telega-ins--with-face 'bold
        (if title
            (telega-ins title)
          (telega-ins url)))
      (telega-ins "\n")
      (when photo-image
        (telega-ins--image photo-image 1))
      (telega-ins--with-face 'shadow
        (if author
            (telega-ins author)
          (telega-ins "Unknown author"))
        (when (zerop publish-date)
          (setq publish-date (time-to-seconds)))
        (telega-ins " • ")
        (telega-ins--date-full publish-date))
      (telega-ins "\n")
      (when photo-image
        (telega-ins--image photo-image 2))
      (telega-ins--with-face 'telega-link
        (telega-ins url)))))

(defun telega-webpage--ins-pb (pb)
  "Insert PageBlock PB for the instant view."
  (cl-ecase (telega--tl-type pb)
    (pageBlockTitle
     (telega-ins--with-face 'telega-webpage-title
       (telega-webpage--ins-rt (plist-get pb :title))))
    (pageBlockSubtitle
     (telega-webpage--ins-rt (plist-get pb :subtitle)))
    (pageBlockAuthorDate
     (telega-ins--with-attrs (list :face 'shadow)
       (telega-ins "By ")
       (telega-webpage--ins-rt (plist-get pb :author))
       (telega-ins " • ")
       (let ((publish-date (plist-get pb :publish_date)))
         (when (zerop publish-date)
           (setq publish-date (time-to-seconds)))
         (telega-ins--date-full publish-date)))
     (telega-ins "\n"))
    (pageBlockHeader
     (telega-ins--with-face 'telega-webpage-header
       (telega-webpage--ins-rt (plist-get pb :header))
       (telega-ins "\n")))
    (pageBlockSubheader
     (telega-ins--with-face 'telega-webpage-subheader
       (telega-webpage--ins-rt (plist-get pb :subheader))
       (telega-ins "\n")))
    (pageBlockParagraph
     (telega-webpage--ins-rt (plist-get pb :text))
     (telega-ins "\n"))
    (pageBlockPreformatted
     (telega-ins--with-face 'telega-webpage-preformatted
       (telega-webpage--ins-rt (plist-get pb :text))
       (telega-ins "\n")))
    (pageBlockFooter
     (telega-ins--with-face 'shadow
       (telega-ins (make-string (/ telega-webpage-fill-column 2) ?-) "\n")
       (telega-webpage--ins-rt (plist-get pb :footer))))
    (pageBlockDivider
     (telega-ins--with-face 'telega-webpage-strike-through
       (telega-ins (make-string telega-webpage-fill-column ?\s))))
    (pageBlockAnchor
     (telega-webpage--add-anchor (plist-get pb :name)))
    (pageBlockListItem
     (telega-ins--labeled (concat " " (plist-get pb :label) " ") nil
       telega-webpage-fill-column
       (mapc 'telega-webpage--ins-pb (plist-get pb :page_blocks))))
    (pageBlockList
     (let ((telega-webpage-strip-nl t))
       (mapc 'telega-webpage--ins-pb (plist-get pb :items))))
    (pageBlockBlockQuote
     (let ((vbar-prefix (propertize telega-symbol-vertical-bar 'face 'bold)))
       (telega-ins vbar-prefix)
       (telega-ins--with-attrs (list :fill 'left
                                     :fill-column telega-webpage-fill-column
                                     :fill-prefix vbar-prefix)
         (telega-webpage--ins-rt (plist-get pb :text)))
       (telega-ins "\n")))
    (pageBlockPullQuote
     (telega-ins "\u00A0\u00A0")
     (telega-ins--with-attrs (list :fill 'center
                                   :fill-column (- telega-webpage-fill-column 2)
                                   :fill-prefix "\u00A0\u00A0")
       (telega-webpage--ins-rt (plist-get pb :text)))
     (telega-ins "\n"))
    (pageBlockAnimation
     (telega-ins "<TODO: pageBlockAnimation>\n"))
    (pageBlockAudio
     (telega-ins "<TODO: pageBlockAudio>\n"))
    (pageBlockPhoto
     (telega-button--insert 'telega (plist-get pb :photo)
       :inserter (lambda (photo)
                   (telega-ins--photo photo nil telega-webpage-photo-size-limits))
       :action 'telega-photo--open)
     (telega-ins "\n")
     (let ((telega-webpage-strip-nl t))
       (telega-webpage--ins-pb (plist-get pb :caption))))
    (pageBlockVideo
     (telega-ins "<TODO: pageBlockVideo>\n"))
    (pageBlockCover
     (telega-webpage--ins-pb (plist-get pb :cover)))
    (pageBlockEmbedded
     (telega-button--insert 'telega pb
       :inserter (lambda (pb-embedded)
                   (let ((url (plist-get pb-embedded :url))
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
                     (telega-ins-prefix "\n"
                       (telega-webpage--ins-pb
                        (plist-get pb-embedded :caption)))
                     ))
       :action (lambda (pb-embedded)
                 (let ((url (plist-get pb-embedded :url)))
                   (unless (string-empty-p url)
                     (telega-browse-url url))))))
    (pageBlockEmbeddedPost
     (telega-ins "<TODO: pageBlockEmbeddedPost>\n"))
    (pageBlockCollage
     (mapc 'telega-webpage--ins-pb (plist-get pb :page_blocks))
     (telega-webpage--ins-pb (plist-get pb :caption)))
    (pageBlockSlideshow
     (telega-ins "<TODO: pageBlockSlideshow>\n"))
    (pageBlockChatLink
     (telega-ins--with-attrs (list :face 'telega-webpage-chat-link)
       (telega-ins (telega-tl-str pb :title) " "
                   "@" (telega-tl-str pb :username) " ")
       (telega-ins--button "Open"
         :value (telega-tl-str pb :username)
         :action 'telega-tme-open-username)))
    (pageBlockCaption
     (telega-ins--with-face 'shadow
       (telega-webpage--ins-rt (plist-get pb :text))
       (telega-ins-prefix " --"
         (telega-webpage--ins-rt (plist-get pb :credit)))))
    (pageBlockDetails
     (telega-webpage--ins-pb-details pb))
    ;; (telega-button--insert 'telega pb
    ;;   'action 'telega-webpage-details-toggle
    ;;   :inserter 'telega-webpage--ins-pb-details
    ;;   :help-echo "Toggle details"))
    (pageBlockTable
     (let ((telega-webpage-strip-nl t))
       (telega-webpage--ins-rt (plist-get pb :caption)))
     (telega-ins "\n")
     (telega-ins "<TODO: pageBlockTable>\n"))
    (pageBlockRelatedArticle
     (telega-button--insert 'telega pb
       :inserter 'telega-webpage--ins-related-article
       :action (lambda (_pbignored)
                 (telega-browse-url (plist-get pb :url)))
       :help-echo (concat "URL: " (plist-get pb :url))))
    (pageBlockRelatedArticles
     (telega-ins--with-face '(telega-msg-heading bold)
       (when (telega-webpage--ins-rt (plist-get pb :header))
         (telega-ins "\n")))
     (mapc 'telega-webpage--ins-pb (plist-get pb :articles)))
    (pageBlockKicker
     (telega-webpage--ins-rt (plist-get pb :kicker)))
    )

  (unless telega-webpage-strip-nl
    (unless (memq (telega--tl-type pb)
                  '(pageBlockAnchor pageBlockCover pageBlockListItem
                                    pageBlockDetails))
      (telega-ins "\n")))
  t)

(defun telega-webpage--ins-pb-nl (pb)
  "Same as `telega-webpage--ins-PageBlock', but also inserts newline at the end."
  (when (telega-webpage--ins-pb pb)
    (telega-ins "\n")))

(defun telega-webpage--instant-view (url &optional sitename instant-view)
  "Instantly view webpage by URL.
If INSTANT-VIEW is non-nil, then its value is already fetched
instant view for the URL."
  (pop-to-buffer-same-window
   (get-buffer-create "*Telega Instant View*"))

  ;; Update current point location into history
  (when (< telega-webpage-history--index (length telega-webpage-history))
    (setcar (nth telega-webpage-history--index telega-webpage-history) (point)))

  (setq telega-webpage--sitename (or sitename
                                     (capitalize
                                      (url-host
                                       (url-generic-parse-url url))))
        telega-webpage--iv (or instant-view
                               (telega--getWebPageInstantView url)
                               (error "Can't instant view the URL: %s" url))
        ;; IV.url can differ from URL, we use IV.url for the correct
        ;; anchors handling. See docs for "td-api.tl:webPageInstantView"
        telega-webpage--url (or (plist-get telega-webpage--iv :url) url)
        telega-webpage--anchors nil)
  (telega-webpage--history-push)

  (let ((buffer-read-only nil)
        (telega--current-buffer (current-buffer)))
    (erase-buffer)
    (mapc 'telega-webpage--ins-pb
          (plist-get telega-webpage--iv :page_blocks))
    (when telega-debug
      (telega-ins-fmt "\n---DEBUG---\n%S" telega-webpage--iv))
    (goto-char (point-min)))

  (unless (eq major-mode 'telega-webpage-mode)
    (telega-webpage-mode)

    (cursor-sensor-mode 1)
    (visual-line-mode 1)
    (setq visual-fill-column-width telega-webpage-fill-column)
    (visual-fill-column-mode 1))

  (let ((anchor (string-trim-left url "[^#]*#?")))
    (unless (string-empty-p anchor)
      (telega-webpage-goto-anchor anchor)))

  (message "Press `%s' to open in web browser"
           (substitute-command-keys "\\[telega-webpage-browse-url]")))

(defun telega-webpage-anchor-url-p (url)
  "Return non-nil if URL is pointing to anchor for current webpage."
  (string-prefix-p
   (concat (string-trim-right telega-webpage--url "#.*") "#")
   url))

(defun telega-browse-url (url &optional in-web-browser)
  "Open the URL.
If URL can be opened directly inside telega, then do it.
Invite links and link to users can be directly opened in telega.
If IN-WEB-BROWSER is non-nil then force opening in web browser."
  (interactive (list (browse-url-url-at-point)
                     current-prefix-arg))
  (when (or in-web-browser
            (not (cond ((string-prefix-p "tg:" url)
                        (telega-tme-open-tg url))
                       ((or (string-prefix-p "https://t.me/" url)
                            (string-prefix-p "http://t.me/" url)
                            (string-prefix-p "https://telegram.me/" url)
                            (string-prefix-p "https://telegram.dog/" url))
                        (telega-tme-open url))
                       ((string-prefix-p "t.me/" url)
                        (telega-tme-open (concat "https://" url)))

                       ((and (derived-mode-p 'telega-webpage-mode)
                             (telega-webpage-anchor-url-p url))
                        ;; Link to local anchor on current webpage
                        (telega-webpage-goto-anchor
                         (string-trim-left url "[^#]+#"))
                        t)

                       (t
                        ;; Try instant view
                        (let ((iv (telega--getWebPageInstantView url)))
                          (when iv
                            (telega-webpage--instant-view url "Telegra.ph" iv)
                            t))))))

    ;; TODO: maybe use webkit x-widget to browse the URL
    (browse-url url)))

(provide 'telega-webpage)

;;; telega-webpage.el ends here

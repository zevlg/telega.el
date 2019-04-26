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

(require 'telega-customize)
(require 'telega-tme)

(defvar telega-webpage-history nil
  "History of viewed webpages.")
(defvar telega-webpage-history--index 0
  "Nth element in `telega-webpage-history' we currently active.")
(defvar telega-webpage-history--ignore nil
  "Bind this to non-nil to ignore pushing to the history.")

(defvar telega-webpage--url nil
  "URL for the instant view webpage currently viewing.")
(defvar telega-webpage--sitename nil
  "Sitename for the webpage currently viewing.")
(defvar telega-webpage--iv nil
  "Instant view for the current webpage.")
(defvar telega-webpage--anchors nil)

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
      (error "No next webpage history"))
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
    (define-key map "\e\t" 'telega-button-backward)
    (define-key map [backtab] 'telega-button-backward)
    map))

(define-derived-mode telega-webpage-mode special-mode "Telega-WebPage"
  "The mode for instant viewing webpages in telega.
Keymap:
\\{telega-webpage-mode-map}"
  :group 'telega
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
    (when anchor
      (goto-char anchor))))

(defun telega-webpage--ins-rt (rt &optional strip-nl)
  "Insert RichText RT.
If STRIP-NL is non-nil then strip leading/trailing newlines."
  (cl-ecase (telega--tl-type rt)
    (richTextPlain
     (telega-ins (funcall (if strip-nl 'telega-strip-newlines 'identity)
                          (plist-get rt :text))))
    (richTexts
     (mapc (lambda (richtext)
             (telega-webpage--ins-rt richtext strip-nl))
           (plist-get rt :texts)))
    (richTextBold
     (telega-ins--with-attrs (list :face 'bold)
       (telega-webpage--ins-rt (plist-get rt :text) strip-nl)))
    (richTextItalic
     (telega-ins--with-attrs (list :face 'italic)
       (telega-webpage--ins-rt (plist-get rt :text) strip-nl)))
    (richTextUnderline
     (telega-ins--with-attrs (list :face 'underline)
       (telega-webpage--ins-rt (plist-get rt :text) strip-nl)))
    (richTextStrikethrough
     (telega-ins--with-attrs (list :face 'telega-webpage-strike-through)
       (telega-webpage--ins-rt (plist-get rt :text) strip-nl)))
    (richTextFixed
     (telega-ins--with-attrs (list :face 'telega-webpage-fixed)
       (telega-webpage--ins-rt (plist-get rt :text) strip-nl)))
    (richTextUrl
     (let* ((url (plist-get rt :url))
            (anchor-prefix (concat telega-webpage--url "#"))
            (anchor (when (string-prefix-p anchor-prefix url)
                      (substring url (length anchor-prefix))))
            (link (telega-ins--as-string
                   (telega-webpage--ins-rt (plist-get rt :text) strip-nl))))
       (telega-debug
        "web page url: url=%s, anchor-prefix=%s, anchor=%s"
        url anchor-prefix anchor)
       ;; NOTE: links to same page jumps to anchor location
       (if anchor
           (telega-ins--button link
             'face 'telega-link
             :value anchor
             :action 'telega-webpage-goto-anchor)
         (telega-ins--button link
           'face 'link
           :help-echo (concat "URL: " url)
           :value url
           :action 'telega-browse-url))))
    (richTextEmailAddress
     (telega-ins--with-attrs (list :face 'link)
       (telega-webpage--ins-rt (plist-get rt :text) strip-nl)))))

(defun telega-webpage--ins-PageBlock (pb)
  "Render PageBlock BLK for the instant view."
  (cl-ecase (telega--tl-type pb)
    (pageBlockTitle
     (telega-ins--with-attrs (list :face 'telega-webpage-title
                                   :fill 'left
                                   :fill-column telega-webpage-fill-column)
       (telega-webpage--ins-rt (plist-get pb :title) 'strip-newlines)))
    (pageBlockSubtitle
     (telega-ins--with-attrs (list :fill 'left
                                   :fill-column telega-webpage-fill-column)
       (telega-webpage--ins-rt (plist-get pb :subtitle))))
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
     (telega-ins--with-attrs
         (list :face 'telega-webpage-header
               :fill 'left
               :fill-column
               (round (/ telega-webpage-fill-column
                         (telega-face-height 'telega-webpage-header))))
       (telega-webpage--ins-rt (plist-get pb :header))
       (telega-ins "\n")))
    (pageBlockSubheader
     (telega-ins--with-attrs
         (list :face 'telega-webpage-subheader
               :fill 'left
               :fill-column
               (round (/ telega-webpage-fill-column
                         (telega-face-height 'telega-webpage-subheader))))
       (telega-webpage--ins-rt (plist-get pb :subheader))
       (telega-ins "\n")))
    (pageBlockParagraph
     (telega-ins--with-attrs (list :fill 'left
                                   :fill-column telega-webpage-fill-column)
       (telega-webpage--ins-rt (plist-get pb :text)))
     (telega-ins "\n"))
    (pageBlockPreformatted
     (telega-ins--with-attrs (list :face 'telega-webpage-preformatted)
       (telega-webpage--ins-rt (plist-get pb :text)))
     (telega-ins "\n"))
    (pageBlockFooter
     (telega-ins "<TODO: pageBlockFooter>"))
    (pageBlockDivider
     (telega-ins--with-attrs (list :align 'center
                                   :fill-column telega-webpage-fill-column)
       (telega-ins (make-string (/ telega-webpage-fill-column 2) ?-))))
    (pageBlockAnchor
     (setf (alist-get (plist-get pb :name) telega-webpage--anchors) (point)))
    (pageBlockList
     (let ((orderedp (plist-get pb :is_ordered))
           (items (plist-get pb :items)))
       (dotimes (ordernum (length items))
         (let ((label (if orderedp (format "%d. " (1+ ordernum)) "• ")))
           (telega-ins--labeled label telega-webpage-fill-column
             (telega-webpage--ins-rt (aref items ordernum))))
         (telega-ins "\n"))))
    (pageBlockBlockQuote
     (telega-ins (propertize telega-symbol-vertical-bar 'face 'bold))
     (telega-ins--with-attrs (list :fill 'left
                                   :fill-prefix (propertize telega-symbol-vertical-bar 'face 'bold)
                                   :fill-column telega-webpage-fill-column)
       (telega-webpage--ins-rt (plist-get pb :text)))
     (telega-ins "\n"))
    (pageBlockPullQuote
     (telega-ins "<TODO: pageBlockPullQuote>"))
    (pageBlockAnimation
     (telega-ins "<TODO: pageBlockAnimation>"))
    (pageBlockAudio
     (telega-ins "<TODO: pageBlockAudio>"))
    (pageBlockPhoto
     (telega-ins--photo (plist-get pb :photo))
     (telega-ins-prefix "\n"
       (telega-ins--with-attrs (list :face 'shadow
                                     :fill 'left
                                     :fill-column telega-webpage-fill-column)
         (telega-webpage--ins-rt (plist-get pb :caption))))
     (telega-ins "\n"))
    (pageBlockVideo
     (telega-ins "<TODO: pageBlockVideo>"))
    (pageBlockCover
     (telega-webpage--ins-PageBlock (plist-get pb :cover)))
    (pageBlockEmbedded
     (telega-ins "<TODO: pageBlockEmbedded>"))
    (pageBlockEmbeddedPost
     (telega-ins "<TODO: pageBlockEmbeddedPost>"))
    (pageBlockCollage
     (telega-ins "<TODO: pageBlockCollage>"))
    (pageBlockSlideshow
     (telega-ins "<TODO: pageBlockSlideshow>"))
    (pageBlockChatLink
     (telega-ins--with-attrs (list :face 'telega-webpage-chat-link)
       (telega-ins (plist-get pb :title) " "
                   "@" (plist-get pb :username) " ")
       (telega-ins--button "Open"
         :value (plist-get pb :username)
         :action 'telega-tme-open-username)))
    )
  (unless (memq (telega--tl-type pb) '(pageBlockAnchor pageBlockCover))
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

  (setq telega-webpage--url url
        telega-webpage--sitename (or sitename
                                     (capitalize
                                      (url-host
                                       (url-generic-parse-url url))))
        telega-webpage--iv (or instant-view
                               (telega--getWebPageInstantView url)
                               (error "Can't instant view the URL: %s" url))
        telega-webpage--anchors nil)
  (telega-webpage--history-push)

  (let ((buffer-read-only nil))
    (erase-buffer)
    (mapc 'telega-webpage--ins-PageBlock
          (plist-get telega-webpage--iv :page_blocks))
    (when telega-debug
      (telega-ins-fmt "\n---DEBUG---\n%S" telega-webpage--iv))
    (goto-char (point-min)))

  (unless (eq major-mode 'telega-webpage-mode)
    (telega-webpage-mode))

  (message "Press `%s' to open in web browser"
           (substitute-command-keys "\\[telega-webpage-browse-url]")))

(defun telega-browse-url (url &optional in-web-browser)
  "Open the URL.
If URL can be opened directly inside telega, then do it.
Invite links and link to users can be directly opened in telega.
If IN-WEB-BROWSER is non-nil then force opening in web browser."
  (when (or in-web-browser
            (not (cond ((string-prefix-p "tg:" url)
                        (telega-tme-open-tg url))
                       ((or (string-prefix-p "https://t.me/" url)
                            (string-prefix-p "https://telegram.me/" url)
                            (string-prefix-p "https://telegram.dog/" url))
                        (telega-tme-open url))
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

;;; telega-webpage.el --- Webpage viewer

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

;;

;;; Code:
(require 'cl-lib)

(define-button-type 'telega-instant-view
  :supertype 'telega
  :inserter 'telega-ins--instant-view
  :button-props-func 'telega-instant-view-button--props
  'action #'telega-instant-view-button--action
  'face 'telega-link)

(defun telega-ins--instant-view (iview)
  "IVIEW is cons of URL and SITENAME."
  (telega-ins "[ " telega-symbol-thunder
              ;; I18N: ???
              " INSTANT VIEW "
              " ]")
  )

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

(defvar telega-webpage--url nil
  "URL for the instant view webpage currently viewing.")
(defvar telega-webpage--sitename nil
  "Sitename for the webpage currently viewing.")
(defvar telega-webpage--anchors nil)

(defcustom telega-webpage-header-line-format
  '(" " (:eval (concat telega-webpage--sitename
                       (and telega-webpage--sitename ": ")
                       telega-webpage--url)))
  "Header line format for instant webpage."
  :type 'list
  :group 'telega)

(defvar telega-webpage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'telega-webpage-browse-url)
    (define-key map "w" 'telega-webpage-browse-url)
    (define-key map "c" 'telega-webpage-copy-url)
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
  (telega-browse-url url))

(defun telega-webpage-maybe-instant-view-url (url)
  "Browse URL with web browser."
  (interactive (list telega-webpage--url))
  (let ((instant-view (telega--getWebPageInstantView url)))
    (if instant-view
        (telega-webpage--instant-view url nil instant-view)
      (telega-webpage-browse-url url))))

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
           :action 'telega-webpage-maybe-instant-view-url))))
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
     (telega-webpage--ins-rt (plist-get pb :subtitle)))
    (pageBlockAuthorDate
     (telega-ins "By ")
     (telega-webpage--ins-rt (plist-get pb :author))
     (telega-ins " • ")
     (let ((publish-date (plist-get pb :publish_date)))
       (when (zerop publish-date)
         (setq publish-date (time-to-seconds)))
       (telega-ins--date-full publish-date))
     (telega-ins "\n"))
    (pageBlockHeader
     (telega-ins--with-attrs
         (list :face 'telega-webpage-header
               :fill 'left
               :fill-column
               (round (/ telega-webpage-fill-column
                         (telega-face-height 'telega-webpage-header))))
       (telega-webpage--ins-rt (plist-get pb :header))))
    (pageBlockSubheader
     (telega-ins--with-attrs
         (list :face 'telega-webpage-subheader
               :fill 'left
               :fill-column
               (round (/ telega-webpage-fill-column
                         (telega-face-height 'telega-webpage-subheader))))
       (telega-webpage--ins-rt (plist-get pb :subheader))))
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
     (setq telega-webpage--anchors
           (put-alist (plist-get pb :name) (point)
                      telega-webpage--anchors)))
    (pageBlockList
     (let ((orderedp (plist-get pb :is_ordered))
           (items (plist-get pb :items)))
       (dotimes (ordernum (length items))
         (let ((label (if orderedp (format "%d. " (1+ ordernum)) "• ")))
           (telega-ins--labeled label telega-webpage-fill-column
             (telega-webpage--ins-rt (aref items ordernum))))
         (telega-ins "\n"))))
    (pageBlockBlockQuote
     (telega-ins telega-symbol-vertical-bar)
     (telega-ins--with-attrs (list :fill 'left
                                   :fill-prefix telega-symbol-vertical-bar
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
     (telega-ins--with-attrs (list :face 'shadow)
       (telega-webpage--ins-rt (plist-get pb :caption))))
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
       (telega-ins--button "[Open]"
         :value (telega-chat-by-username (plist-get pb :username))
         :action 'telega-chat--pop-to-buffer)))
    )
  (unless (memq (telega--tl-type pb) '(pageBlockAnchor pageBlockCover))
    (telega-ins "\n")))

(defun telega-webpage--instant-view (url &optional sitename instant-view)
  "Instantly view webpage by URL.
If INSTANT-VIEW is non-nil, then its value is already fetched
instant view for the URL."
  (pop-to-buffer-same-window
   (get-buffer-create "*Telega Instant View*"))

  (setq telega-webpage--url url
        telega-webpage--sitename sitename
        telega-webpage--anchors nil)
  (unless instant-view
    (setq instant-view (telega--getWebPageInstantView url)))
  (let ((buffer-read-only nil))
    (erase-buffer)
    (mapc 'telega-webpage--ins-PageBlock
          (plist-get (telega--getWebPageInstantView url) :page_blocks))

    (when telega-debug
      (telega-ins-fmt "\n---DEBUG---\n%S" instant-view))
    (goto-char (point-min)))

  (unless (eq major-mode 'telega-webpage-mode)
    (telega-webpage-mode))

  (message "Press `%s' to open in web browser"
           (substitute-command-keys "\\[telega-webpage-browse-url]")))

(provide 'telega-webpage)

;;; telega-webpage.el ends here

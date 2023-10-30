;;; telega-url-shorten.el --- URL shortening for telega  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2020 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sun Apr  5 12:07:57 2020
;; Package-Requires: ((all-the-icons "4.0.0"))
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

;;; ellit-org:
;; ** /telega-url-shorten.el/ -- Makes urls look nicer
;;
;; Minor mode for chatbuf to show shorter version for some URLs.  For
;; example, with ~telega-url-shorten-mode~ enabled in chatbuf, urls
;; like:
;;
;; #+begin_example
;; https://github.com/zevlg/telega.el/issues/105
;; https://gitlab.com/jessieh/mood-line/issues/6
;; https://www.youtube.com/watch?v=0m2jR6_eMkU
;; https://ru.wikipedia.org/wiki/Душ
;; #+end_example
;;
;; Will look like:
;; [[https://zevlg.github.io/telega/telega-url-shorten.png]]
;;
;; Can be enabled globally in all chats matching
;; ~telega-url-shorten-mode-for~ [[#telega-match-expressions][chat
;; temex]] (see below) with ~(global-telega-url-shorten-mode 1)~ or by
;; adding:
;;
;; #+begin_src emacs-lisp
;; (add-hook 'telega-load-hook 'global-telega-url-shorten-mode)
;; #+end_src
;;
;; Depends on
;; [[https://github.com/domtronn/all-the-icons.el][all-the-icons]]
;; Emacs package.

;; TODO: Use custom emoji stickers from various set of stickeset, such as:
;;  - https://t.me/addemoji/logos_td

;;; Code:
(require 'cl-lib)
(require 'telega)

(require 'all-the-icons)

(defgroup telega-url-shorten nil
  "Customization for `telega-url-shorten' minor mode."
  :prefix "telega-url-shorten"
  :group 'telega-modes)

;;; ellit-org:
;; Customizable options:
;;
;; - {{{user-option(telega-url-shorten-use-images, 2)}}}
(defcustom telega-url-shorten-use-images telega-use-images
  "*Non-nil to use images on graphics display."
  :type 'boolean
  :group 'telega-url-shorten)

;;; ellit-org:
;; - User Option: ~telega-url-shorten-regexps~
;;   {{{vardoc1(telega-url-shorten-regexps)}}}
;;
;;   To change ~:symbol~ or ~:svg-icon~ property for existing url
;;   shortening pattern use something like:
;;   #+begin_src
;;   (plist-put (cdr (assq '<LABEL> telega-url-shorten-regexps))
;;              :<PROP> <VALUE>)
;;   #+end_src
(defcustom telega-url-shorten-regexps
  (list
   `(github-issue
     :regexp "^https?://github.com/\\(.+\\)/issues/\\([0-9]+\\)"
     :symbol ,(all-the-icons-faicon "github")
     :replace "\\1#\\2"
     :svg-icon ("fa-brands/github-octocat.svg" :scale 0.72))
   `(github-repo
     :regexp "^https?://github.com/\\(.+\\)/\\(.*\\)"
     :symbol ,(all-the-icons-faicon "github")
     :replace "\\1/\\2"
     :svg-icon ("fa-brands/github-octocat.svg" :scale 0.72))
  `(gitlab-issue
    :regexp "^https?://gitlab.com/\\(.+\\)/issues/\\([0-9]+\\)"
    :symbol ,(all-the-icons-faicon "gitlab")
    :replace "\\1#\\2"
    :svg-icon ("fa-brands/gitlab-rgb.svg" :scale 0.75))
  `(gitlab-repo
    :regexp "^https?://gitlab.com/\\(.+\\)/\\(.*\\)"
    :symbol ,(all-the-icons-faicon "gitlab")
    :replace "\\1/\\2"
    :svg-icon ("fa-brands/gitlab-rgb.svg" :scale 0.75))
   `(youtube
     :regexp "^https?://www.youtube.com/watch.*[?&]v=\\([^&]+\\).+"
     :symbol ,(all-the-icons-faicon "youtube-play")
     :replace "YouTube#\\1"
     :svg-icon ("fa-brands/youtube-rgb.svg" :scale 0.6))
   `(youtu-be
     :regexp "^https?://youtu.be/\\(.+\\)"
     :symbol ,(all-the-icons-faicon "youtube-play")
     :replace "YouTube#\\1"
     :svg-icon ("fa-brands/youtube-rgb.svg" :scale 0.6))
   `(wikipedia
     :regexp "^https?://\\(\\w+.\\)\\{0,2\\}wikipedia.org/wiki/\\(.+\\)"
     :symbol ,(all-the-icons-faicon "wikipedia-w")
     :replace "wiki#\\2"
     :svg-icon ("fa-brands/wikipedia.svg" :scale 0.85))
   `(instagram
     :regexp "^https?://\\(www\\.\\)?instagram.com/\\(.+\\)"
     :symbol ,(all-the-icons-faicon "instagram")
     :replace "Instagram#\\2"
     :svg-icon ("fa-brands/instagram-rgb.svg" :scale 0.85))
   `(tiktok
     :regexp "^https?://\\(www\\|vm\\)\\.tiktok.com/\\(.+\\)"
     :symbol "♪"
     :replace "TikTok#\\2"
     :svg-icon ("fa-brands/tiktok-rgb.svg" :scale 0.85))
   )
  "Alist of patterns for URL shortening."
  :type 'alist
  :group 'telega-url-shorten)

;;; ellit-org:
;; - {{{user-option(telega-url-shorten-mode-for, 2)}}}
(defcustom telega-url-shorten-mode-for 'all
  "*Chat filter for `global-telega-url-shorten-mode'.
`global-telega-url-shorten-mode' enables urls shortening only for
chats matching this chat filter."
  :type 'list
  :group 'telega-url-shorten)

(defun telega-url-shorten--svg-icon (icon-name &rest props)
  (unless (plist-get props :scale)
    (setq props (plist-put props :scale 1.0)))
  `(image :type svg :file ,(telega-etc-file icon-name)
          :ascent center :height ,(telega-chars-xheight 1)
          ,@props))

(defun telega-url-shorten--e-t-p (old-e-t-p entity text)
  "Change resulting `telega-display' property by shortening URL."
  (let* ((result (funcall old-e-t-p entity text))
         (result-td (plist-get result 'telega-display)))
    (when (and result-td
               (eq 'textEntityTypeUrl
                   (telega--tl-type (plist-get entity :type))))
      (when-let ((pmatch (cdr (cl-find result-td telega-url-shorten-regexps
                                       :test (lambda (res pattern)
                                               (string-match
                                                (plist-get pattern :regexp)
                                                res))
                                       :key #'cdr))))
        (plist-put result 'telega-display-by 'telega-url-shorten)
        (plist-put result 'telega-display
                   (concat (propertize
                            (plist-get pmatch :symbol)
                            'display (when (and telega-use-images
                                                telega-url-shorten-use-images
                                                (plist-get pmatch :svg-icon))
                                       (apply #'telega-url-shorten--svg-icon
                                              (plist-get pmatch :svg-icon))))
                           (replace-match (plist-get pmatch :replace)
                                          t nil result-td)))))
    result))

;;;###autoload
(define-minor-mode telega-url-shorten-mode
  "Toggle URLs shortening mode."
  :init-value nil :group 'telega-modes
  (if telega-url-shorten-mode
      (advice-add 'telega--entity-to-properties
                  :around #'telega-url-shorten--e-t-p)
    (advice-remove 'telega--entity-to-properties
                   #'telega-url-shorten--e-t-p)))

(defun telega-url-shorten-mode--maybe (&optional arg)
  (when (telega-chat-match-p telega-chatbuf--chat telega-url-shorten-mode-for)
    (telega-url-shorten-mode arg)))

;;;###autoload
(define-minor-mode global-telega-url-shorten-mode
  "Global mode to shorten the URLs."
  :init-value nil :global t :group 'telega-modes
  (if global-telega-url-shorten-mode
      (progn
        (add-hook 'telega-chat-mode-hook 'telega-url-shorten-mode--maybe)
        (dolist (buf (telega-chat-buffers))
          (with-current-buffer buf
            (telega-url-shorten-mode--maybe 1))))

    (remove-hook 'telega-chat-mode-hook 'telega-url-shorten-mode--maybe)
    (dolist (buf (telega-chat-buffers))
      (with-current-buffer buf
        (telega-url-shorten-mode -1)))))

(provide 'telega-url-shorten)

;;; telega-url-shorten.el ends here

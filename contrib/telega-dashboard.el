;;; telega-dashboard.el --- Important telega chats in Emacs dashboard.  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2020 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Mon Dec 21 00:02:44 2020
;; Package-Requires: ((dashboard "1.8.0"))
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
;; ** /telega-dashboard.el/ -- Important telega chats in the Emacs dashboard
;; 
;; Display important telega chats in the Emacs dashboard.  Enable it with:
;;
;; #+begin_src emacs-lisp
;; (require 'telega-dashboard)
;; (add-to-list 'dashboard-items '(telega-chats . 5))
;; #+end_src
;;
;; Screenshot of =telega-dashboard= in action:
;; [[https://zevlg.github.io/telega/telega-dashboard.png]]
;; 
;; Customizable options:
;; - {{{user-option(telega-dashboard-chat-filter, 2)}}}
;; - {{{user-option(telega-dashboard-chat-inserter, 2)}}}

;;; Code:
(require 'telega)
(require 'dashboard)

(add-to-list 'dashboard-item-generators
             '(telega-chats . telega-dashboard-chats-insert))
(add-to-list 'dashboard-item-shortcuts
             '(telega-chats . "t"))

(defvar telega-dashboard-chat-filter '(or mention (and unread unmuted))
  "Chat Filter used to filter chats to display in the Emacs dashboard.")
(defvar telega-dashboard-chat-inserter #'telega-ins--chat-full
  "Inserter for the chat button in the Emacs dashboard.")

(defvar telega-dashboard--cached-icon nil
  "Cached telega logo image.")

(defun telega-dashboard-chats-insert (list-size)
  "Add at most LIST-SIZE important telega chats."
  (when (and (display-graphic-p)
             dashboard-set-heading-icons)
    ;; Insert telega logo icon
    (unless telega-dashboard--cached-icon
      (setq telega-dashboard--cached-icon
            (find-image
             (list (list :type (when (fboundp 'imagemagick-types) 'imagemagick)
                         :file "etc/telega-logo.png"
                         :ascent 'center :mask 'heuristic
                         :height (window-line-height))
                   (list :type 'svg :file "etc/telega-logo.svg"
                         :ascent 'center
                         :background (face-attribute 'default :background)
                         :height (window-line-height))
                   (list :type 'xpm :file "etc/telega-logo.xpm"
                         :ascent 'center)))))
    (insert (propertize telega-symbol-telegram
                        'display telega-dashboard--cached-icon)
            " "))
  ;; Avoid extra spaces insertation in case icons are used
  (let ((dashboard-set-heading-icons nil))
    (dashboard-insert-heading
     "Telega Chats:" (dashboard-get-shortcut 'telega-chats)))

  (if (not (telega-server-live-p))
      (insert (propertize "\n    --- telega not running ---" 'face 'error))

    (let ((ichats (seq-take (telega-filter-chats
                             telega--ordered-chats telega-dashboard-chat-filter)
                            list-size)))
      (if (not ichats)
          (insert (propertize "\n    --- No important chats ---"
                              'face 'dashboard-no-items-face))
        (dolist (chat ichats)
          (insert "\n    ")
          (let ((telega-root-fill-column (- dashboard-banner-length 4)))
            (telega-button--insert 'telega-chat chat
              :inserter telega-dashboard-chat-inserter)))))
    (dashboard-insert-shortcut
     (dashboard-get-shortcut 'telega-chats) "Telega Chats:")))

(provide 'telega-dashboard)

;;; telega-dashboard.el ends here

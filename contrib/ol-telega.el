;;; ol-telega.el --- Links to telega chats and messages -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sat Mar 28 11:15:21 2020
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
;; ** /ol-telega.el/ -- Org mode links to telegram chats and messages
;;
;; Installs "telega" links to Org mode.
;;
;; "telega" link can point to a chat, a message or content of a
;; message.
;;
;; Creating links to a message content is very useful in conjuction
;; with [[#telega-edit-file-mode][Edit File Mode]], so you can store
;; your Org mode files in Telegram Cloud and create links to them in
;; Roam manner.

;;; Code:
(require 'cl-lib)
(require 'org)
;; Emacs26 has `org-store-link-props' instead of `org-link-store-props'
(eval-when-compile
  (when (and (not (fboundp 'org-link-store-props))
             (fboundp 'org-store-link-props))
    (defalias 'org-link-store-props 'org-store-link-props)))

(require 'telega)

(defun org-telega-follow-link (link)
  "Follow a telegram LINK to chat or message."
  (telega-tme-open-tg (concat "tg:telega:" link)))

(defun org-telega-store-link ()
  "Store a link to a telegram chat or message.
It could be link to a chat, message or to content opened from a
message, file or photo."
  (let* ((msg
          (or (telega-msg-at (point))
              (and telega-edit-file-mode
                   telega--help-win-param)))
         (msg-open-p
          (when msg
            (or telega-edit-file-mode
                (and (eq 'messageDocument
                         (telega--tl-type (plist-get msg :content)))
                     (y-or-n-p "Store link to a message's file?")))))
         (chat-or-msg
          (or msg telega-chatbuf--chat (telega-chat-at (point))))
         (link
          (when chat-or-msg
            (apply #'telega-tme-internal-link-to chat-or-msg
                   (when msg-open-p
                     '(:open_content "")))))
         ;; NOTE: strip leading "tg:"
         (org-link (when link (substring link 3))))
    (when org-link
      (org-link-store-props
       :type "telega" :link org-link
       :description
       (concat (telega-symbol 'telegram)
               (if (telega-chat-p chat-or-msg)
                   (telega-chat-title-with-brackets chat-or-msg)
                 (cl-assert (telega-msg-p chat-or-msg))
                 (telega-ins--as-string
                  (telega-ins--msg-sender
                   (telega-msg-sender chat-or-msg) 'short)
                  (telega-ins ": ")
                  (telega-ins--with-attrs
                      (list :max 20 :align 'left :elide t)
                    (telega-ins--content-one-line chat-or-msg))))))
      org-link)))

(defun org-telega-complete-link ()
  "Completing link to a chat."
  (let ((chat (telega-completing-read-chat "Chat: ")))
    ;; NOTE: strip leading "tg:"
    (concat (substring (telega-tme-internal-link-to chat) 3))))

(org-link-set-parameters "telega"
                         :follow #'org-telega-follow-link
                         :store #'org-telega-store-link
                         :complete #'org-telega-complete-link)

(provide 'ol-telega)

;;; ol-telega.el ends here

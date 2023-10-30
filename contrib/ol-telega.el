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
;; 
;; Customizable options:
;;
;; - {{{user-option(org-telega-chat-link-format, 2)}}}
;; - {{{user-option(org-telega-msg-link-format, 2)}}}
;; - {{{user-option(org-telega-file-link-format, 2)}}}

;;; Code:
(require 'cl-lib)
(require 'org)
(require 'format-spec)
;; Emacs26 has `org-store-link-props' instead of `org-link-store-props'
(eval-when-compile
  (when (and (not (fboundp 'org-link-store-props))
             (fboundp 'org-store-link-props))
    (defalias 'org-link-store-props 'org-store-link-props)))

(require 'telega)

(defcustom org-telega-chat-link-format
  (concat telega-symbol-telegram "%t")
  "Description format for link to a chat.
Format spec:
%u - chat username if any, %t otherwise.
%t - chat title with brackets."
  :group 'org-link
  :type 'string)

(defcustom org-telega-msg-link-format
  (concat telega-symbol-telegram
          "%u" telega-symbol-sender-and-text-delim " %>.32m")
  "Description format for link to a message.
Format spec:
%u - user username if any, %t otherwise.
%t - user title without brackets.
%m - one line message body."
  :group 'org-link
  :type 'string)

(defcustom org-telega-file-link-format
  (concat telega-symbol-telegram telega-symbol-attachment
          "%u" telega-symbol-sender-and-text-delim " %>.32m")
  "Description format for link to a message to open message's media file.
Format spec:
%u - user username if any, %t otherwise.
%t - user title without brackets.
%m - one line message body."
  :group 'org-link
  :type 'string)

(defun org-telega-follow-link (link)
  "Follow a telegram LINK to chat or message."
  (telega-tme-open-tg (concat "tg:telega:" link)))

(defun org-telega-store-link ()
  "Store a link to a telegram chat or message.
It could be link to a chat, message or to content opened from a
message, file or photo."
  (let* ((msg
          (or (telega-msg-at (point))
              (when telega-edit-file-mode
                telega--help-win-param)
              (when (derived-mode-p 'telega-image-mode)
                telega-image--message)))
         (msg-open-p
          (when msg
            (or telega-edit-file-mode
                (derived-mode-p 'telega-image-mode)
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
       (if (telega-chat-p chat-or-msg)
           ;; Link to a chat
           (let ((chat-title (telega-msg-sender-title chat-or-msg
                               :with-brackets-p t)))
             (format-spec org-telega-chat-link-format
                          (format-spec-make
                           ?u (or (telega-msg-sender-username chat-or-msg
                                                              'with-@)
                                  chat-title)
                           ?t chat-title)))

         ;; Link to a message or file
         (let* ((sender (telega-msg-sender msg))
                (sender-title (telega-msg-sender-title sender
                                :with-badges-p nil)))
           (format-spec (if msg-open-p
                            org-telega-file-link-format
                          org-telega-msg-link-format)
                        (format-spec-make
                         ?u (or (telega-msg-sender-username sender 'with-@)
                                sender-title)
                         ?t sender-title
                         ?m (telega-ins--as-string
                             (telega-ins--content-one-line chat-or-msg)))))))
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

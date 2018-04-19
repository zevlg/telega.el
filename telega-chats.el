;;; telega-chats.el --- Chats support for telegram

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Apr 19 19:59:51 2018
;; Keywords: 

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(defmacro telega-chat--get (event)
  `(gethash (plist-get ,event :chat_id) (telega-context-chats telega-ctx)))

(defun telega-chat--brackets (chat)
  (let ((chat-type (plist-get chat :type)))
    (case (intern (plist-get chat-type :@type))
      (chatTypePrivate (cons "|" "|"))
      (chatTypeBasicGroup (cons "(" ")"))
      (chatTypeSupergroup
       (if (eq (plist-get chat-type :is_channel) :json-false)
           (cons "<" ">")
         (cons "[" "]")))
      (chatTypeSecret (cons "#" "#"))
      (t (cons "(" ")")))))

(defun telega-chat--title (chat)
  "Return title for the CHAT."
  (let ((chat-type (plist-get chat :type))
        (title (plist-get chat :title)))
    (if (string-empty-p title)
        (ecase (intern (plist-get chat-type :@type))
          (chatTypePrivate
           (telega-user--title
            (telega-user--get (plist-get chat-type :user_id)))))

      title)))

(defun telega-chat--order (chat) (plist-get chat :order))

(defun telega-chat--reorder (chat order)
  (plist-put chat :order order)
  (cl-sort (telega-context-ordered-chats telega-ctx)
           'string< :key 'telega-chat--order)
  (telega-root--chat-reorder chat))

(defun telega-chat--new (chat)
  "Create new CHAT."
  (puthash (plist-get chat :id) chat (telega-context-chats telega-ctx))
  (telega-root--chat-new chat)

  (push chat (telega-context-ordered-chats telega-ctx))
  (telega-chat--reorder chat (telega-chat--order chat)))
  
(defun telega--on-updateNewChat (event)
  "New chat has been loaded or created."
  (telega-chat--new (plist-get event :chat)))

(defun telega--on-updateChatTitle (event)
  (let ((chat (telega-chat--get event)))
    (plist-put chat :title (plist-get event :title))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatOrder (event)
  (let ((chat (telega-chat--get event)))
    (telega-chat--reorder chat (plist-get event :order))))

(defun telega--on-updateChatIsPinned (event)
  (let ((chat (telega-chat--get event)))
    (plist-put chat :is_pinned (plist-get event :is_pinned))
    (telega-chat--reorder chat (plist-get event :order))))

(defun telega--on-updateChatUnreadMentionCount (event)
  (let ((chat (telega-chat--get event)))
    (plist-put chat :unread_mention_count (plist-get event :unread_mention_count))
    (telega-root--chat-update chat)))

(defalias 'telega--on-updateChatUnreadMentionRead 'telega--on-updateChatUnreadMentionCount)

(defun telega-chat--on-getChats (result)
  "Ensure chats from RESULT exists, and continue fetching chats."
  (let ((chat_ids (plist-get result :chat_ids)))
    (telega-debug "on-getChats: %s" (plist-get result :chat_ids))
    (mapc (lambda (chat_id)
            (unless (gethash chat_id (telega-context-chats telega-ctx))
              (telega-chat--new
               (telega-server--call
                `(:@type "getChat" :chat_id ,chat_id)))))
          chat_ids)

    (unless (zerop (length (plist-get result :chat_ids)))
      ;; Continue fetching chats
      (telega-chat--getChatList))))

(defun telega-chat--getChatList ()
  "Retreive all chats from the server."
  (let* ((last-chat (car (telega-context-ordered-chats telega-ctx)))
         (offset-order (or (and last-chat (plist-get last-chat :order)) "9223372036854775807"))
         (offset-chatid (or (and last-chat (plist-get last-chat :id)) 0)))
    
    (telega-server--call
     `(:@type "getChats"
       :offset_order ,offset-order
       :offset_chat_id ,offset-chatid
       :limit 1000000)
     #'telega-chat--on-getChats)))

(provide 'telega-chats)

;;; telega-chats.el ends here

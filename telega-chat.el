;;; telega-chat.el --- Chat mode for telega

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
(require 'telega-core)

(declare-function telega-root--chat-update "telega-root" (chat))

(defvar telega-chat-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-buffer-map)
    (define-key map (kbd "i") 'telega-chat-info)
    (define-key map (kbd "h") 'telega-chat-info)
    (define-key map (kbd "n") 'telega-chat-notify-toggle)
    (define-key map (kbd "DEL") 'telega-chat-delete)
    map)
  "The key map for telega chat buttons.")

(define-button-type 'telega-chat
  :format '("[" (telega-chat--title
                 :min 25 :max 25
                 :align left :align-char ?\s
                 :elide t :elide-trail 0)
            "]\n")
  'keymap telega-chat-button-map
  'action 'telega-chat-activate
  'face nil)

(defsubst telega-chat--ensure (chat)
  "Ensure CHAT resides in `telega--chats' and `telega--ordered-chats'."
  (let ((chat-id (plist-get chat :id)))
    (unless (gethash chat-id telega--chats)
      (puthash chat-id chat telega--chats)
      (push chat telega--ordered-chats))))

(defun telega-chat--get (chat-id)
  "Get chat by its CHAT-ID."
  (let ((chat (gethash chat-id telega--chats)))
    (unless chat
      (setq chat (telega-server--call
                  `(:@type "getChat" :chat_id ,chat-id)))
      (assert user nil "getChat timed out chat_id=%d" chat-id)
      (telega-chat--ensure chat))
    chat))

(defun telega-chat--private-user (chat)
  "For private CHAT return corresponding user."
  (telega-user--get (plist-get (plist-get chat :type) :user_id)))

(defun telega-chat--me ()
  "Chat with myself, a.k.a Saved Messages."
  ;; TODO
  )

(defun telega-chat--type (chat)
  "Return type of the CHAT.
Types are: `private', `secret', `bot', `basicgroup', `supergroup' or `channel'."
  (let* ((chat-type (plist-get chat :type))
         (type-sym (intern (downcase (substring (plist-get chat-type :@type) 8)))))
    (cond ((and (eq type-sym 'supergroup)
                (telega--tl-bool chat-type :is_channel))
           'channel)
          ((and (eq type-sym 'private)
                (telega-user--bot-p (telega-chat--private-user chat)))
           'bot)
          (t type-sym))))

(defun telega-chat--order (chat)
  (plist-get chat :order))

(defun telega-chat--title (chat)
  "Return title for the CHAT."
  (let ((title (plist-get chat :title)))
    (if (string-empty-p title)
        (ecase (telega-chat--type chat)
          (private
           (telega-user--title (telega-chat--private-user chat))))
      title)))

(defun telega-chat--reorder (chat order)
  (plist-put chat :order order)
  (cl-sort telega--ordered-chats 'string< :key 'telega-chat--order)
  (telega-root--chat-reorder chat))

(defun telega-chat--new (chat)
  "Create new CHAT."
  (telega-chat--ensure chat)
  (telega-root--chat-update chat)
  (telega-chat--reorder chat (telega-chat--order chat)))

(defun telega-chat--visible-p (chat)
  "Return non-nil if CHAT is visible by means of active filters."
  (telega-filter--test chat (cons 'all (car telega--filters))))

(defun telega--on-updateNewChat (event)
  "New chat has been loaded or created."
  (telega-chat--new (plist-get event :chat)))

(defun telega--on-updateChatTitle (event)
  (let ((chat (telega-chat--get (plist-get event :chat_id))))
    (plist-put chat :title (plist-get event :title))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatOrder (event)
  (let ((chat (telega-chat--get (plist-get event :chat_id))))
    (telega-chat--reorder chat (plist-get event :order))))

(defun telega--on-updateChatIsPinned (event)
  (let ((chat (telega-chat--get (plist-get event :chat_id))))
    (plist-put chat :is_pinned (plist-get event :is_pinned))
    (telega-chat--reorder chat (plist-get event :order))))

(defun telega--on-updateChatUnreadMentionCount (event)
  (let ((chat (telega-chat--get (plist-get event :chat_id))))
    (plist-put chat :unread_mention_count
               (plist-get event :unread_mention_count))
    (telega-root--chat-update chat)))

(defalias 'telega--on-updateChatUnreadMentionRead
  'telega--on-updateChatUnreadMentionCount)

(defun telega-chat--on-getChats (result)
  "Ensure chats from RESULT exists, and continue fetching chats."
  (let ((chat_ids (plist-get result :chat_ids)))
    (telega-debug "on-getChats: %s" chat_ids)
    (mapc #'telega-chat--get chat_ids)

    (if (> (length chat_ids) 0)
        ;; Continue fetching chats
        (telega-chat--getChats)
      ;; All chats has been fetched
      (run-hooks 'telega-chats-fetched-hook))))

(defun telega-chat--getChats ()
  "Retreive all chats from the server in async manner."
  (let* ((last-chat (car telega--ordered-chats))
         (offset-order (or (and last-chat (plist-get last-chat :order))
                           "9223372036854775807"))
         (offset-chatid (or (and last-chat (plist-get last-chat :id)) 0)))
    (telega-server--call
     `(:@type "getChats"
              :offset_order ,offset-order
              :offset_chat_id ,offset-chatid
              :limit 1000000)
     #'telega-chat--on-getChats)))

(defun telega-chat--getGroupsInCommon (with-user)
  "Return groups in common WITH-USER."
  (let ((groups-in-common
         (telega-server--call
          `(:@type "getGroupsInCommon"
                   :user_id ,(plist-get with-user :id)
                   :offset_chat_id 0
                   :limit ,(plist-get (telega-user--full-info with-user)
                                      :group_in_common_count)))))
    (mapcar #'telega-chat--get (plist-get groups-in-common :chat_ids))))

(defun telega-chats--kill-em-all ()
  "Kill all chat buffers."
  (message "TODO: `telega-chats--kill-em-all'")
  )

(defun telega-chats--unread (chats)
  "Return total number of unread messages in CHATS."
  (apply #'+ (mapcar (telega--tl-prop :unread_count) chats)))

(defun telega-chats--unread-mentions (chats)
  "Return total number of unread mentions in CHATS."
  (apply #'+ (mapcar (telega--tl-prop :unread_mention_count) chats)))

;;;###autoload
(defun telega-chat-info (chat)
  "Show info about CHAT at point."
  (interactive
   (list (let ((button (button-at (point))))
           (and button (button-get button :value)))))
  (unless chat
    (error "No chat at point"))

  (with-help-window (format " *ChatInfo: %s*" (telega-chat--title chat))
    (set-buffer standard-output)
    (insert (format "Title: %s\n" (telega-chat--title chat)))
    (insert (format "Type: %S (%d)\n" (telega-chat--type chat) (plist-get chat :id)))
;    (insert "Notifications: %s\n" TODO
    (insert "\n")
    (ecase (telega-chat--type chat)
      (private 
       (telega-user-info--insert (telega-chat--private-user chat))))

    ;; TODO: view shared media as thumbnails
    ))

(provide 'telega-chat)

;;; telega-chat.el ends here

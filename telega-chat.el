;;; telega-chat.el --- Chat mode for telega

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Apr 19 19:59:51 2018
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

(defcustom telega-chat-button-width 28
  "*Width for the chat buttons."
  :type 'integer
  :group 'telega)

(declare-function telega-root--chat-update "telega-root" (chat))

(defvar telega-chat-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "i") 'telega-chat-info)
    (define-key map (kbd "h") 'telega-chat-info)
    (define-key map (kbd "DEL") 'telega-chat-delete)
    map)
  "The key map for telega chat buttons.")

(define-button-type 'telega-chat
  :supertype 'telega
  :format #'telega-chat-button--formatter
  'keymap telega-chat-button-map
  'action #'telega-chat-button--action)

(defun telega-chat-button--formatter (chat)
  "Formatter for the CHAT button."
  (let ((title (telega-chat--title chat))
        (unread (plist-get chat :unread_count))
        (mentions (plist-get chat :unread_mention_count))
        (pinned-p (telega--tl-bool chat :is_pinned))
        (muted-p (telega-chat--muted-p chat))
        (umwidth 7))
    `("[" (,title
           :min ,(- telega-chat-button-width umwidth)
           :max ,(- telega-chat-button-width umwidth)
           :align left :align-char ?\s :elide t)
      ((,(unless (zerop unread)
           (propertize (number-to-string unread)
                       'face (if muted-p
                                 'telega-muted-count
                               'telega-unmuted-count)))
        ,(unless (zerop mentions)
           (propertize (format "@%d" mentions) 'face 'telega-mention-count)))
       :min ,umwidth :max ,umwidth :elide t :align right)
      "]"
      ,(when pinned-p
         telega-pin-string)
      "\n")))

(defun telega-chat-button--action (button)
  "Action to take when chat BUTTON is pressed."
  (message "TODO: `telega-chat-button--action'")
  )

(defsubst telega-chat--ensure (chat)
  "Ensure CHAT resides in `telega--chats' and `telega--ordered-chats'."
  (let ((chat-id (plist-get chat :id)))
    (unless (gethash chat-id telega--chats)
      (puthash chat-id chat telega--chats)
      (push chat telega--ordered-chats))))

(defun telega-chat--get (chat-id &optional offline-p)
  "Get chat by its CHAT-ID.
If OFFLINE-P is non-nil then do not request the telegram-server."
  (let ((chat (gethash chat-id telega--chats)))
    (when (and (not chat) (not offline-p))
      (setq chat (telega-server--call
                  `(:@type "getChat" :chat_id ,chat-id)))
      (assert chat nil "getChat timed out chat_id=%d" chat-id)
      (telega-chat--ensure chat))
    chat))

(defun telega-chat--info (chat)
  "Return info structure for the CHAT.
It could be user, secretChat, basicGroup or supergroup."
  (let ((chat-type (plist-get chat :type)))
    (ecase (telega--tl-type chat-type)
      (chatTypePrivate
       (telega--info 'user (plist-get chat-type :user_id)))
      (chatTypeSecret
       (telega--info 'secretChat (plist-get chat-type :secret_chat_id)))
      (chatTypeBasicGroup
       (telega--info 'basicGroup (plist-get chat-type :basic_group_id)))
      (chatTypeSupergroup
       (telega--info 'supergroup (plist-get chat-type :supergroup_id))))))
(defalias 'telega-chat--user 'telega-chat--info)
(defalias 'telega-chat--secretchat 'telega-chat--info)
(defalias 'telega-chat--basicgroup 'telega-chat--info)
(defalias 'telega-chat--supergroup 'telega-chat--info)

(defun telega-chat--me ()
  "Chat with myself, a.k.a Saved Messages."
  ;; NOTE: Saved Messages has same id as me user
  (telega-chat--get (plist-get (telega-user--me) :id)))

(defun telega-chat--type (chat &optional no-interpret)
  "Return type of the CHAT.
Types are: `private', `secret', `bot', `basicgroup', `supergroup' or `channel'.
If NO-INTERPRET is specified, then return only `private',
`basicgroup' and `supergroup' without interpretation them to bots
or channels."
  (let* ((chat-type (plist-get chat :type))
         (type-sym (intern (downcase (substring (plist-get chat-type :@type) 8)))))
    (cond ((and (not no-interpret)
                (eq type-sym 'supergroup)
                (telega--tl-bool chat-type :is_channel))
           'channel)
          ((and (not no-interpret)
                (eq type-sym 'private)
                (telega-user--bot-p (telega-chat--user chat)))
           'bot)
          (t type-sym))))

(defsubst telega-chat--order (chat)
  (plist-get chat :order))

(defsubst telega-chat--muted-p (chat)
  "Return non-nil if CHAT is muted."
  (> (plist-get (plist-get chat :notification_settings) :mute_for) 0))

(defun telega-chat--title (chat)
  "Return title for the CHAT."
  (let ((title (plist-get chat :title)))
    (if (string-empty-p title)
        (ecase (telega-chat--type chat)
          (private
           (telega-user--title (telega-chat--user chat))))
      title)))

(defun telega-chat--reorder (chat order)
  (plist-put chat :order order)
  (setq telega--ordered-chats
        (cl-sort telega--ordered-chats 'string> :key 'telega-chat--order))
  (telega-root--chat-reorder chat))

(defun telega-chat--new (chat)
  "Create new CHAT."
  (telega-chat--ensure chat)
  (telega-root--chat-update chat)
  (telega-chat--reorder chat (telega-chat--order chat)))

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
    (telega-chat--reorder chat (plist-get event :order))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatReadInbox (event)
  (let ((chat (telega-chat--get (plist-get event :chat_id))))
    (plist-put chat :last_read_inbox_message_id
               (plist-get event :last_read_inbox_message_id))
    (plist-put chat :unread_count
               (plist-get event :unread_count))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatReadOutbox (event)
  (let ((chat (telega-chat--get (plist-get event :chat_id))))
    (plist-put chat :last_read_outbox_message_id
               (plist-get event :last_read_outbox_message_id))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatUnreadMentionCount (event)
  (let ((chat (telega-chat--get (plist-get event :chat_id))))
    (plist-put chat :unread_mention_count
               (plist-get event :unread_mention_count))
    (telega-root--chat-update chat)))

(defun telega--on-updateMessageMentionRead (event)
  (telega--on-updateChatUnreadMentionCount event)
  ;; TODO: might be workout with message of `:message_id' as well
  )

(defun telega--on-updateChatReplyMarkup (event)
  (let ((chat (telega-chat--get (plist-get event :chat_id))))
    (plist-put chat :reply_markup_message_id
               (plist-get event :reply_markup_message_id))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatLastMessage (event)
  (let ((chat (telega-chat--get (plist-get event :chat_id))))
    (plist-put chat :last_message
               (plist-get event :last_message))
    (telega-chat--reorder chat (plist-get event :order))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatDraftMessage (event)
  (let ((chat (telega-chat--get (plist-get event :chat_id))))
    (telega-debug "TODO: `telega--on-updateChatDraftMessage' handle draft message")
    (telega-chat--reorder chat (plist-get event :order))
    (telega-root--chat-update chat)))

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
  (let* ((last-chat (car (last telega--ordered-chats)))
         (offset-order (or (plist-get last-chat :order) "9223372036854775807"))
         (offset-chatid (or (plist-get last-chat :id) 0)))
    (telega-server--call
     `(:@type "getChats"
              :offset_order ,offset-order
              :offset_chat_id ,offset-chatid
              :limit 1000)
     #'telega-chat--on-getChats)))

(defun telega-chat--getPinnedMessage (chat)
  "Get pinned message for the CHAT, if any."
  (when (and (eq (telega-chat--type chat) 'supergroup)
             (not (zerop (plist-get
                          (telega--full-info (telega-chat--supergroup chat))
                          :pinned_message_id))))
    (telega-server--call
     `(:@type "getChatPinnedMessage" :chat_id ,(plist-get chat :id)))))


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

(defun telega-chat-info (chat)
  "Show info about CHAT at point."
  (interactive
   (list (let ((button (button-at (point))))
           (and button (button-get button :value)))))
  (unless chat
    (error "No chat at point"))

  (with-help-window " *Telegram Chat Info*"
    (set-buffer standard-output)
    (insert (format "Title: %s\n" (telega-chat--title chat)))
    (insert (format "Id: %d Type: %S Order: %s\n"
                    (plist-get chat :id) (telega-chat--type chat)
                    (telega-chat--order chat)))
    (let ((not-cfg (plist-get chat :notification_settings)))
      (insert (format "Notifications: %s\n"
                      (if (zerop (plist-get not-cfg :mute_for))
                          (concat "enabled"
                                  (if (telega--tl-bool not-cfg :show_preview)
                                      " with preview"
                                    ""))
                        "disabled"))))

    (insert "\n")
    (telega-info--insert (plist-get chat :type) chat)
    ))


;;; Chat Buffer
(defgroup telega-chat nil
  "Customization for telega-chat-mode"
  :prefix "telega-chat-"
  :group 'telega)

(define-derived-mode telega-chat-mode nil "Telega-Chat"
  "The mode for telega chat buffer.
Keymap:
\\{telega-chat-mode-map}"
  :group 'telega-chat

  ;; TODO
  )

(provide 'telega-chat)

;;; telega-chat.el ends here

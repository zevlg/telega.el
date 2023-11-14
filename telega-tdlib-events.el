;;; telega-tdlib-events.el --- Handle events from TDLib  -*- lexical-binding: t -*-

;; Copyright (C) 2020 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sun May 24 20:36:43 2020
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
(require 'telega-tdlib)
(require 'telega-root)
(require 'telega-chat)
(require 'telega-inline)
(require 'telega-topic)
(require 'telega-story)

(defvar tracking-buffers nil)
(declare-function telega--authorization-ready "telega")

(defvar telega-tdlib-min-version)
(defvar telega-tdlib-max-version)


(defun telega-chat--update (chat &rest dirtiness)
  "Something changed in CHAT, button needs to be updated.
DIRTINESS specifies additional CHAT dirtiness."
  (let ((chat-dirtiness (nconc dirtiness (plist-get chat :telega-dirtiness))))
    (telega-debug "IN: `telega-chat--update': %s dirtiness: %S"
                  (telega-chat-title chat) chat-dirtiness)

    (when dirtiness
      (plist-put chat :telega-dirtiness chat-dirtiness)
      (when (memq 'reorder dirtiness)
        (setq telega--ordered-chats (delq chat telega--ordered-chats))
        (telega--ordered-chats-insert chat))))

  ;; Update root ewocs, filters and chatbuf
  (telega-root-view--update :on-chat-update chat)
  (telega-filters--chat-update chat)
  (with-telega-chatbuf chat
    (telega-chatbuf--chat-update))

  (telega-describe-chat--maybe-redisplay chat)

  (run-hook-with-args 'telega-chat-update-hook chat)

  ;; Finally chat has been updated
  (plist-put chat :telega-dirtiness nil))

(defun telega-chat--mark-dirty (chat &optional event)
  "Mark CHAT as dirty by EVENT."
  (unless (memq chat telega--dirty-chats)
    (setq telega--dirty-chats (cons chat telega--dirty-chats)))
  (plist-put chat :telega-dirtiness
             (cons (plist-get event :@type)
                   (plist-get chat :telega-dirtiness)))

  ;; If there are more then 50 chats are dirty, then force update
  (when (> (length telega--dirty-chats) 50)
    (telega-chats-dirty--update)
    (telega-filters--redisplay)

    ;; NOTE: Timers are not run when Emacs is under heavy load, so
    ;; force status animations as well if timer is running
    (when telega-status--timer
      (telega-status--animate))
    ))

(defun telega-chats-dirty--update ()
  "Update dirty chats."
  (let* ((dirty-chats (prog1 telega--dirty-chats
                        (setq telega--dirty-chats nil)))
         (reorder-chats
          (cl-remove-if-not #'telega-chat-order-dirty-p dirty-chats)))
    ;; NOTE: To reorder REORDER-CHATS we fist remove all of them from
    ;; the ordered chat list and then insert them one by one, so
    ;; chat's dirty orders won't affect insertion point
    (dolist (rc reorder-chats)
      (setq telega--ordered-chats (delq rc telega--ordered-chats)))
    (dolist (rc reorder-chats)
      (telega--ordered-chats-insert rc))

    (dolist (dc dirty-chats)
      (telega-chat--update dc))))


(defun telega--on-ok (_event)
  "On ok result from command function call."
  ;; no-op
  )

;; User updates
(defun telega-user--update (user event)
  "USER has been updated, do something about this."
  (telega-root-view--update :on-user-update user)
  (telega-describe-user--maybe-redisplay (plist-get user :id))
  (telega-describe-contact--maybe-redisplay (plist-get user :id))

  ;; Update corresponding private chat as well
  (when-let ((chat (telega-chat-get (plist-get user :id) 'offline)))
    (telega-chat--mark-dirty chat event))

  (run-hook-with-args 'telega-user-update-hook user))

(defun telega--on-updateUser (event)
  "Some user info has has been changed."
  (let ((user (plist-get event :user)))
    (telega--info-update user)
    (telega-user--update user event)))

(defun telega--on-updateUserStatus (event)
  "User status has been changed."
  (let* ((user-id (plist-get event :user_id))
         (user (telega-user-get user-id))
         (status (plist-get event :status)))
    (plist-put user :status status)
    ;; NOTE: For online status, set special USER property with value
    ;; of time last seen online
    (when (eq (telega--tl-type status) 'userStatusOnline)
      (plist-put user :telega-last-online (telega-time-seconds)))

    ;; NOTE: do not track online status changes on me
    (unless (telega-me-p user)
      (telega-user--update user event))
    ))

(defun telega--on-updateChatAction (event)
  "Some message sender has actions on chat."
  (let* ((chat-id (plist-get event :chat_id))
         (msg-thread-id (plist-get event :message_thread_id))
         (action-id (cons chat-id msg-thread-id))
         (chat-actions (gethash action-id telega--actions))
         (sender (plist-get event :sender_id))
         (user-action (assoc sender chat-actions))
         (action (plist-get event :action))
         (cancel-p (eq (telega--tl-type action) 'chatActionCancel)))
    (cond (cancel-p
           (let ((new-chat-actions (assoc-delete-all sender chat-actions)))
             (if new-chat-actions
                 (puthash action-id new-chat-actions telega--actions)
               (remhash action-id telega--actions))))
          (user-action
           (setcdr user-action action))
          (t (puthash action-id (cons (cons sender action) chat-actions)
                      telega--actions)))

    (let ((chat (telega-chat-get chat-id)))
      (telega-chat--mark-dirty chat event)

      (with-telega-chatbuf chat
        ;; If action by me, update `telega-chatbuf--my-action' as well
        (when (telega-me-p (telega-msg-sender sender))
          (setq telega-chatbuf--my-action (unless cancel-p action)))))
    ))

(defun telega--on-updateUserFullInfo (event)
  (let ((user-id (plist-get event :user_id))
        (ufi (cdr (assq 'user telega--full-info))))
    (puthash user-id (plist-get event :user_full_info) ufi)

    ;; Possibly update `telega--blocked-user-ids-alist', keeping
    ;; `telega--blocked-user-ids-alist' in sync with update events
    ;; see https://github.com/tdlib/td/issues/1669

    ;; 1. First remove user from all block lists
    (dolist (blocked-user-list telega--blocked-user-ids-alist)
      (when (memq user-id (cdr blocked-user-list))
        (setcdr blocked-user-list
                (delq user-id (cdr blocked-user-list)))))

    ;; 2. Then add it to the block list from event
    (when-let* ((block-list (telega--tl-get event :user_full_info :block_list))
                (blocked-user-list
                 (assoc block-list telega--blocked-user-ids-alist)))
      (unless (memq user-id (cdr blocked-user-list))
        (setcdr blocked-user-list
                (append (cdr blocked-user-list) (list user-id)))))

    (telega-user--update (telega-user-get user-id) event)))


;; Chat updates
(defun telega--on-updateNewChat (event)
  "New chat has been loaded or created."
  (let ((chat (telega-chat--ensure (plist-get event :chat))))
    (telega-chat--mark-dirty chat event)

    ;; Asynchronously fetch all topics
    (when (telega-chat-match-p chat 'is-forum)
      (telega-chat--topics-fetch chat))

    (run-hook-with-args 'telega-chat-created-hook chat)))

(defun telega--on-updateChatPhoto (event)
  "Chat's photo has been updated."
  (let ((chat (telega-chat-get (plist-get event :chat_id)))
        (photo (plist-get event :photo)))
    (plist-put chat :photo photo)

    ;; XXX remove cached avatars
    (plist-put chat :telega-image nil)
    (plist-put chat :telega-avatar-1 nil)
    (plist-put chat :telega-avatar-3 nil)
    (plist-put chat :telega-avatar-vc-1 nil)
    (plist-put chat :telega-avatar-vc-speaking-1 nil)

    (telega-chat--mark-dirty chat event)
    ))

(defun telega--on-updateChatPermissions (event)
  "Chat's permissions was changed."
  (let ((chat (telega-chat-get (plist-get event :chat_id))))
    (plist-put chat :permissions (plist-get event :permissions))

    (telega-chat--mark-dirty chat event)
    ))

(defun telega--on-updateChatNotificationSettings (event)
  "Notification settings has been changed in chat."
  (let ((chat (telega-chat-get (plist-get event :chat_id))))
    (plist-put chat :notification_settings
               (plist-get event :notification_settings))

    (telega-chat--mark-dirty chat event)

    (telega-root-view--update :on-notifications-update)
    (telega-describe-notifications--maybe-redisplay)
    ))

(defun telega--on-updateChatTitle (event)
  "EVENT arrives when title of a chat was changed."
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :title (plist-get event :title))

    (telega-chat--mark-dirty chat event)

    (with-telega-chatbuf chat
      (rename-buffer (telega-chatbuf--name chat)))
    ))

(defun telega--on-updateChatPosition (event)
  (let* ((chat (telega-chat-get (plist-get event :chat_id) 'offline))
         (new-pos (plist-get event :position))
         (chat-positions
          (cl-delete (plist-get new-pos :list) (plist-get chat :positions)
                     :key (telega--tl-prop :list) :test #'equal)))
    (plist-put chat :positions (vconcat chat-positions (list new-pos)))

    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateChatMessageSender (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (plist-put chat :message_sender_id
               (plist-get event :message_sender_id))
    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateChatHasProtectedContent (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (plist-put chat :has_protected_content
               (plist-get event :has_protected_content))
    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateChatIsTranslatable (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (plist-put chat :is_translatable
               (plist-get event :is_translatable))
    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateChatReadInbox (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline))
        (unread-count (plist-get event :unread_count)))
    (cl-assert chat)
    (plist-put chat :last_read_inbox_message_id
               (plist-get event :last_read_inbox_message_id))
    (plist-put chat :unread_count unread-count)

    (telega-chat--mark-dirty chat event)

    (with-telega-chatbuf chat
      ;; NOTE: if all messages are read (in another telegram client),
      ;; then remove the chatbuf from tracking
      (when (and (zerop unread-count)
                 (member (buffer-name) tracking-buffers))
        (tracking-remove-buffer (current-buffer))))))

(defun telega--on-updateChatReadOutbox (event)
  (let* ((chat (telega-chat-get (plist-get event :chat_id) 'offline))
         (old-read-outbox-msgid (plist-get chat :last_read_outbox_message_id)))
    (cl-assert chat)
    (plist-put chat :last_read_outbox_message_id
               (plist-get event :last_read_outbox_message_id))

    (telega-chat--mark-dirty chat event)

    (with-telega-chatbuf chat
      (telega-chatbuf--read-outbox old-read-outbox-msgid))))

(defun telega--on-updateChatUnreadMentionCount (event &optional chat)
  (unless chat
    (setq chat (telega-chat-get (plist-get event :chat_id) 'offline)))

  (cl-assert chat)
  (plist-put chat :unread_mention_count
             (plist-get event :unread_mention_count))

  (telega-chat--mark-dirty chat event))

(defmacro with-telega--msg-update-event (event bindings &rest body)
  (declare (indent 2))
  (let ((chat-id-sym (gensym "chat-id"))
        (msg-id-sym (gensym "msg-id")))
    `(let* ((,chat-id-sym (plist-get ,event :chat_id))
            (,msg-id-sym (plist-get ,event :message_id))
            (,(nth 0 bindings) (telega-chat-get ,chat-id-sym 'offline))
            (,(nth 2 bindings) (with-telega-chatbuf ,(nth 0 bindings)
                                 (telega-chatbuf--node-by-msg-id ,msg-id-sym)))
            (,(nth 1 bindings) (if ,(nth 2 bindings)
                                   (ewoc-data ,(nth 2 bindings))
                                 (gethash (cons ,chat-id-sym ,msg-id-sym)
                                          telega--cached-messages))))
       ,@body)))

(defun telega--on-updateMessageMentionRead (event)
  (with-telega--msg-update-event event (chat msg node)
    (cl-assert chat)
    (telega--on-updateChatUnreadMentionCount event chat)

    ;; Update message's `:contains_unread_mention' as well
    ;; This requires message redisplay, since message could outline
    ;; unread mention
    (plist-put msg :contains_unread_mention nil)
    (when node
      (with-telega-chatbuf chat
        (telega-chatbuf--redisplay-node node)))))

(defun telega--on-updateChatDefaultDisableNotification (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :default_disable_notification
               (plist-get event :default_disable_notification))
    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateChatLastMessage (event)
  (let* ((chat-id (plist-get event :chat_id))
         (chat (telega-chat-get chat-id 'offline))
         (event-last-msg (plist-get event :last_message)))
    (cl-assert chat)
    ;; NOTE: If chat's last message is already in the message's
    ;; cache, use it, because various logic (such as ignoring
    ;; predicated) might be already applied to the message
    (when event-last-msg
      (when-let ((cached-last-msg
                  (gethash (cons chat-id (plist-get event-last-msg :id))
                           telega--cached-messages)))
        (setq event-last-msg cached-last-msg)))

    (plist-put chat :last_message event-last-msg)
    (plist-put chat :positions (plist-get event :positions))

    ;; NOTE: `:last_message' is unset when gap is created in the chat
    ;; This case is handled in the `telega-chatbuf--last-msg-loaded-p'
    ;; See https://github.com/tdlib/td/issues/896
    ;;
    ;; Gap can be also created if last message in the chat is deleted.
    ;; TDLib might take some time to update chat's last message.
    (with-telega-chatbuf chat
      (telega-chatbuf--history-state-delete :newer-loaded))

    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateChatIsMarkedAsUnread (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :is_marked_as_unread
               (plist-get event :is_marked_as_unread))

    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateChatBlockList (event)
  "Chat/User has been blocked/unblocked."
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :block_list
               (plist-get event :block_list))

    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateChatOnlineMemberCount (event)
  "The number of online group members has changed.
NOTE: we store the number as custom chat property, to use it later."
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :x-online-count
               (plist-get event :online_member_count))

    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateForumTopicInfo (event)
  (let* ((new-info (plist-get event :info))
         (chat (telega-chat-get (plist-get event :chat_id) 'offline))
         (topic (telega-topic-get chat (plist-get new-info :message_thread_id))))
    (plist-put topic :info new-info)

    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateChatDraftMessage (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline))
        (draft-msg (plist-get event :draft_message)))
    (cl-assert chat)
    (plist-put chat :draft_message draft-msg)

    ;; Generate fake events with position updates
    (seq-doseq (pos (plist-get event :positions))
      (telega--on-updateChatPosition (list :chat_id (plist-get event :chat_id)
                                           :position pos)))

    (telega-chat--mark-dirty chat event)

    ;; Update chat's input to the text in DRAFT-MSG
    (with-telega-chatbuf chat
      (telega-chatbuf--input-draft-update))
    ))

(defun telega--on-updateChatHasScheduledMessages (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :has_scheduled_messages
               (plist-get event :has_scheduled_messages))

    (telega-chat--mark-dirty chat event)

    ;; NOTE: `telega-chatbuf--name' uses `:has_scheduled_messages', so
    ;; rename the buffer
    (with-telega-chatbuf chat
      (rename-buffer (telega-chatbuf--name chat)))))

(defun telega--on-updateChatActionBar (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :action_bar (plist-get event :action_bar))

    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateChatTheme (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :theme_name (plist-get event :theme_name))
    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateChatMessageAutoDeleteTime (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id))))
    (cl-assert chat)
    (plist-put chat :message_auto_delete_time
               (plist-get event :message_auto_delete_time))

    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateSecretChat (event)
  (let ((secretchat (plist-get event :secret_chat)))
    (telega--info-update secretchat)

    ;; update corresponding secret chat button
    (when-let ((chat (cl-find secretchat
                              (telega-filter-chats
                               telega--ordered-chats '(type secret))
                              :test 'eq :key #'telega-chat--info)))
      (telega-chat--mark-dirty chat event))))

(defun telega--on-blocked-senders-load (reply)
  ;; NOTE:
  ;;  - first element of the REPLY is a block-list
  ;;  - second element of the REPLY is a total number of blocked
  ;;    senders in a block-lis
  ;;  - rest is the message senders ids
  (let* ((block-list (car reply))
         (_total-senders (cadr reply))
         (senders (cddr reply))
         (blocked-user-ids
          (alist-get block-list telega--blocked-user-ids-alist)))
    (when senders
      (cl-incf (car blocked-user-ids) (length senders))
      (setcdr blocked-user-ids
              (mapcar (telega--tl-prop :id)
                      (cl-remove-if-not #'telega-user-p senders)))

      ;; Continue fetching blocked users
      (telega--getBlockedMessageSenders block-list (car blocked-user-ids)
        #'telega--on-blocked-senders-load)
      )))

(defun telega--on-initial-chats-load (tl-ok)
  "Process initially loaded chats, or continue loading chats."
  (if (not (telega--tl-error-p tl-ok))
      (progn
        ;; Continue fetching chats
        (telega--loadChats (list :@type "chatListMain")
          #'telega--on-initial-chats-load))

    ;; All chats has been fetched
    (telega-status--set nil "")

    ;; Check `:last_message' of initially fetched chats for client
    ;; side messages ignoring.  Also trigger reordering, since
    ;; ignoring last message might affect chat order, see
    ;; `contrib/telega-adblock.el'
    (dolist (chat telega--ordered-chats)
      (when-let ((last-message (plist-get chat :last_message)))
        (when (telega-msg-run-ignore-predicates last-message 'last-msg)
          (telega-chat--update chat 'reorder))

        ;; Also fetch dependend message and custom emojis for the last
        ;; message, because last message might be displayed in the
        ;; rootbuf
        (when (telega-msg-match-p last-message
                '(type PinMessage GameScore PaymentSuccessful
                       ForumTopicEdited ForumTopicIsClosedToggled))
          (telega-msg--replied-message-fetch last-message))

        (when telega-use-images
          (telega-msg--custom-emojis-fetch last-message))
        ))

    (run-hooks 'telega-chats-fetched-hook)))

(defun telega--on-updateChatReplyMarkup (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :reply_markup_message_id
               (plist-get event :reply_markup_message_id))

    (with-telega-chatbuf chat
      (telega-chatbuf--reply-markup-message-fetch))))

(defun telega--on-updateChatVideoChat (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :video_chat (plist-get event :video_chat))

    (telega-chat--mark-dirty chat event)

    (with-telega-chatbuf chat
      (telega-chatbuf--video-chat-fetch))))

(defun telega--on-updateGroupCall (event)
  (let ((new-group-call (plist-get event :group_call)))
    (telega-group-call--ensure new-group-call)

    (when-let ((chat (telega-group-call-get-chat (plist-get new-group-call :id))))
      (telega-chat--mark-dirty chat event))
    ))

(defun telega--on-updateGroupCallParticipant (event)
  (let ((group-call-id (plist-get event :group_call_id))
        (_call-user (plist-get event :participant)))
    (when-let ((chat (telega-group-call-get-chat group-call-id)))
      (with-telega-chatbuf chat
        ;; TODO: video-chats
        ;; update `telega-chatbuf--group-call-users'
        )
      )))


;; Chat filters
(defun telega--on-updateChatFolders (event)
  "List of chat filters has been updated."
  ;; NOTE: collect folders with changed names and update all chats in
  ;; that folders.  Because folder name might be displayed along the
  ;; side with chat's title in the rootbuf
  (let* ((new-tdlib-folders (append (plist-get event :chat_folders) nil))
         (new-names (seq-difference (telega-folder-names new-tdlib-folders)
                                    (telega-folder-names))))
    (setq telega-tdlib--chat-folders new-tdlib-folders)

    (dolist (folder-name new-names)
      (dolist (fchat (telega-filter-chats
                      telega--ordered-chats `(folder ,folder-name)))
        (telega-chat--mark-dirty fchat)))
    (telega-chats-dirty--update)

    ;; Update custom filters ewoc
    (with-telega-root-buffer
      (telega-save-cursor
        (telega-filters--refresh))

      ;; If folders view is active, then redisplay it, see
      ;; https://t.me/emacs_telega/36581
      (when (eq (car telega-root--view) #'telega-view-folders)
        (telega-view-folders))

      (run-hooks 'telega-root-update-hook))
    ))


;; Messages updates
(defun telega-message--update (msg)
  "Message MSG has been updated."
  (when (plist-get msg :is_pinned)
    (when-let ((chat (telega-msg-chat msg 'offline)))
      (plist-put chat :telega-pinned-message
                 (cons msg
                       (cl-remove msg (plist-get chat :telega-pinned-messages)
                                  :test #'telega-msg-id=)))))

  (telega-root-view--update :on-message-update msg)
  )

(defalias 'telega--on-message 'ignore)

(defun telega--on-updateNewMessage (event)
  "A new message was received; can also be an outgoing message."
  (let* ((new-msg (plist-get event :message))
         (chat (telega-msg-chat new-msg)))
    ;; NOTE: We always set `:ignored-p' property to not trigger
    ;; `telega-msg-run-ignore-predicates' once again when this message
    ;; is inserted into chatbuf
    (if (telega-msg-run-ignore-predicates new-msg 'last-msg)
        (progn
          ;; NOTE: In case ignored message contains mention, we mark
          ;; all chat mentions as read if there is no other mentions.
          ;; See https://github.com/zevlg/telega.el/issues/314
          (when (and (plist-get new-msg :contains_unread_mention)
                     (eq 1 (plist-get chat :unread_mention_count)))
            (telega--readAllChatMentions chat))
          ;; NOTE: If all messages in the chat are read and ignored
          ;; message arives, automatically read it
          ;; See https://github.com/zevlg/telega.el/issues/381
          (when-let ((last-message (plist-get chat :last_message)))
            (when (<= (plist-get last-message :id)
                      (plist-get chat :last_read_inbox_message_id))
            (telega--viewMessages chat (list new-msg)
              :source '(:@type "messageSourceChatHistory")
              :force t))))

      (plist-put new-msg :ignored-p nil))

    (run-hook-with-args 'telega-chat-pre-message-hook new-msg)

    (with-telega-chatbuf chat
      (telega-msg-cache new-msg)

      ;; NOTE: `:last_message' could be already updated in the chat
      ;; with the id of the NEW-MSG, so check for it
      (when (telega-chatbuf--append-new-message-p new-msg)
        (when-let ((node (telega-chatbuf--insert-messages
                          (list new-msg) 'append-new)))
          (when (and (telega-chatbuf-match-p telega-use-tracking-for)
                     (not (telega-msg-match-p new-msg 'ignored))
                     (not (plist-get new-msg :is_outgoing))
                     (not (telega-msg-seen-p new-msg telega-chatbuf--chat)))
            (tracking-add-buffer (current-buffer) '(telega-tracking)))

          ;; If message is visible in some window, then mark it as read
          ;; see https://github.com/zevlg/telega.el/issues/4
          (when (telega-chatbuf--msg-observable-p new-msg node)
            (telega-chatbuf--msg-view new-msg))
          )))
    ;; NOTE: Trigger `telega-chat-post-message-hook' for outgoing
    ;; messages, only when message is successfully sent
    ;; See https://t.me/emacs_telega/25615
    (unless (plist-get new-msg :sending_state)
      (run-hook-with-args 'telega-chat-post-message-hook new-msg))))

(defun telega--on-updateMessageSendSucceeded (event)
  "Message has been successfully sent to server.
Message id could be updated on this update."
  (let* ((new-msg (plist-get event :message))
         (chat-id (plist-get new-msg :chat_id))
         (new-id (plist-get new-msg :id))
         (old-id (plist-get event :old_message_id)))
    ;; Actualize cached message
    (remhash (cons chat-id old-id) telega--cached-messages)
    (puthash (cons chat-id new-id) new-msg telega--cached-messages)

    (with-telega-chatbuf (telega-msg-chat new-msg)
      ;; NOTE: Actualize message position according to NEW-ID
      ;; Optimization: search old message's node from last node
      (let ((node (ewoc-nth telega-chatbuf--ewoc -1)))
        (while (and node (not (= old-id (plist-get (ewoc-data node) :id))))
          (setq node (ewoc-prev telega-chatbuf--ewoc node)))

        (when node
          (let ((before-node (ewoc-next telega-chatbuf--ewoc node)))
            ;; Search the node to insert new message before
            (while (and before-node
                        (> new-id (plist-get (ewoc-data before-node) :id)))
              (setq before-node (ewoc-next telega-chatbuf--ewoc before-node)))

            (run-hook-with-args 'telega-chatbuf-pre-msg-insert-hook new-msg)
            (ewoc-set-data node new-msg)
            (telega-ewoc--move-node telega-chatbuf--ewoc node before-node
                                    'save-point)
            (when before-node
              ;; NOTE: need to redisplay next to newly created
              ;; node, in case `telega-chat-group-messages-for' is
              ;; used, see https://github.com/zevlg/telega.el/issues/159
              (ewoc-invalidate telega-chatbuf--ewoc before-node))

            ;; Automatically view all outgoing messages
            (when (plist-get new-msg :is_outgoing)
              (telega-chatbuf--msg-view new-msg))

            (run-hook-with-args 'telega-chatbuf-post-msg-insert-hook new-msg)
            ))))
    (unless (plist-get new-msg :sending_state)
      ;; NOTE: In case outgoing message is done via bot, we need to
      ;; update recently used inline bots list
      (when-let ((outgoing-p (plist-get new-msg :is_outgoing))
                 (via-bot-id (plist-get new-msg :via_bot_user_id)))
        (unless (zerop via-bot-id)
          (telega--recent-inline-bots-fetch)))

      ;; NOTE: Handle update for the favorite messages storage creation
      (when (and (eq chat-id (plist-get telega--favorite-messages-storage-message :chat_id))
                 (eq old-id (plist-get telega--favorite-messages-storage-message :id)))
        (setq telega--favorite-messages-storage-message new-msg)
        (message "telega: Storage for the favorite messages has been created"))

      (run-hook-with-args 'telega-chat-post-message-hook new-msg))))

(defun telega--on-updateMessageSendFailed (event)
  "Message failed to send."
  ;; NOTE: Triggered for example if trying to send bad picture.
  ;; `telega--on-updateMessageSendSucceeded' updates the message
  ;; content with new(failed) state
  (telega--on-updateMessageSendSucceeded event)

  (let ((err (plist-get event :error)))
    (message "telega: Failed to send message: %d %s"
             (plist-get err :code) (telega-tl-str err :message))
    ))

(defun telega--on-updateMessageContent (event)
  "Content of the message has been changed."
  (let ((new-content (plist-get event :new_content)))
    ;; NOTE: for "messagePoll" update check if there any option with
    ;; `:is_being_chosen' set to non-nil.
    ;; If so, then just ignore this update, waiting for real update
    ;; chosing some poll option may fail with "REVOTE_NOT_ALLOWED" error
    (unless (and (eq (telega--tl-type new-content) 'messagePoll)
                 (cl-some (telega--tl-prop :is_being_chosen)
                          (telega--tl-get new-content :poll :options)))
      (with-telega--msg-update-event event (chat msg node)
        (plist-put msg :content new-content)
        (when node
          (with-telega-chatbuf chat
            (run-hook-with-args 'telega-chatbuf-pre-msg-update-hook msg)
            (telega-chatbuf--redisplay-node node)
            (run-hook-with-args 'telega-chatbuf-post-msg-update-hook msg)
            ))))))

(defun telega--on-updateMessageEdited (event)
  "Edited date of the message specified by EVENT has been changed."
  (with-telega--msg-update-event event (chat msg node)
    (plist-put msg :edit_date (plist-get event :edit_date))
    (plist-put msg :reply_markup (plist-get event :reply_markup))
    (with-telega-chatbuf chat
      (when node
        (telega-chatbuf--redisplay-node node))

      ;; In case user has active aux with a message from EVENT,
      ;; then redisplay aux as well.
      ;; See https://t.me/emacs_telega/7243
      (when-let ((aux-msg (plist-get telega-chatbuf--aux-plist :aux-msg)))
        (when (telega-msg-id= msg aux-msg)
          (plist-put telega-chatbuf--aux-plist :aux-msg msg)
          (telega-chatbuf--chat-update "aux-plist")))
      )))

(defun telega--on-updateMessageIsPinned (event)
  "Message has ben pinned or unpinned."
  (with-telega--msg-update-event event (chat msg node)
    (plist-put msg :is_pinned (plist-get event :is_pinned))
    (with-telega-chatbuf chat
      (when node
        (telega-chatbuf--redisplay-node node))
      (telega-chatbuf--pinned-messages-fetch))

    ;; TODO:
    ;(telega-chat--update-pinned-message chat nil old-pinned-message-id)
    (telega-chat--mark-dirty chat event)
    ))

(defun telega--on-updateMessageInteractionInfo (event)
  "Message interaction info has been changed."
  (with-telega--msg-update-event event (chat msg node)
    (plist-put msg :interaction_info (plist-get event :interaction_info))
    (with-telega-chatbuf chat
      (when node
        (telega-chatbuf--redisplay-node node)

        ;; NOTE: reactions might be updated and custom emojis needs to
        ;; be downloaded.
        (telega-msg--custom-emojis-fetch msg))

      (when-let ((thread-msg (telega-chatbuf--thread-msg)))
        (when (= (plist-get event :message_id) (plist-get thread-msg :id))
          (telega-chatbuf--chat-update "thread")))
      )))

(defun telega--on-updateMessageContentOpened (event)
  "The message content was opened.
Updates voice note messages to \"listened\", video note messages
to \"viewed\" and starts the TTL timer for self-destructing
messages."
  (with-telega--msg-update-event event (chat msg node)
    (when-let ((content (plist-get msg :content)))
      (cl-case (telega--tl-type content)
        (messageVoiceNote
         (plist-put content :is_listened t))
        (messageVideoNote
         (plist-put content :is_viewed t))
        ;; TODO: Check self-destruct type and probably start self-destruct
        ;; timer
        ;; ((messagePhoto messageVideo)
        (t
         ;; Nothing to update
         (setq node nil))))
    (when node
      (with-telega-chatbuf chat
        (telega-chatbuf--redisplay-node node)))))

(defun telega--on-updateDeleteMessages (event)
  "Some messages has been deleted from chat."
  (let ((chat-id (plist-get event :chat_id)))
    ;; NOTE: Always delete message from `telega--cached-messages' even
    ;; if `:from_cache' is nil.  Both `:is_permanent' and
    ;; `:from_cache' could be nil for some private channels we are not
    ;; member of
    (seq-doseq (msg-id (plist-get event :message_ids))
      (remhash (cons chat-id msg-id) telega--cached-messages))

    (when (plist-get event :is_permanent)
      (with-telega-chatbuf (telega-chat-get chat-id)
        (seq-doseq (msg-id (plist-get event :message_ids))
          (when-let ((node (telega-chatbuf--node-by-msg-id msg-id))
                     (msg (ewoc--node-data node)))
            (plist-put msg :telega-is-deleted-message t)
            (if (telega-chat-match-p (telega-msg-chat msg)
                                     telega-chat-show-deleted-messages-for)
                (telega-chatbuf--redisplay-node node)

              ;; NOTE: need to redisplay next to deleted node, in case
              ;; `telega-chat-group-messages-for' is used
              ;; See https://github.com/zevlg/telega.el/issues/159
              (let ((next-node (ewoc-next telega-chatbuf--ewoc node)))
                (ewoc-delete telega-chatbuf--ewoc node)
                (when next-node
                  (telega-chatbuf--redisplay-node next-node))))

            ;; TODO: 1.7.0 pinned
            (when (plist-get msg :is_pinned)
              (telega-chatbuf--pinned-messages-fetch))
            ))))))


;; Call updates
(defun telega--on-updateCall (event)
  "Called when some call data has been updated."
  (let* ((call (plist-get event :call))
         (state (plist-get call :state))
         (call-id (plist-get call :id))
         (old-call (telega-voip--by-id call-id)))
    (setf (alist-get call-id telega-voip--alist) call)

    ;; Update active call value
    (when (eq call-id (plist-get telega-voip--active-call :id))
      (setq telega-voip--active-call call))

    (cl-case (telega--tl-type state)
      (callStatePending
       (unless old-call
         (if (plist-get call :is_outgoing)
             (run-hook-with-args 'telega-call-outgoing-hook call)
           (run-hook-with-args 'telega-call-incoming-hook call)))

       ;; * If no active calls and CALL is outgoing, then make it
       ;;   active
       ;; * If there is active call and `telega-voip-busy-if-active' is
       ;;   non-nil then discard all other incoming calls
       (if (plist-get call :is_outgoing)
           (unless telega-voip--active-call
             (setq telega-voip--active-call call))

         (when (and telega-voip-busy-if-active
                    telega-voip--active-call
                    (not (eq call telega-voip--active-call)))
           (telega--discardCall call-id)))

       (when (and telega-voip-help-echo
                  (not telega-voip--active-call)
                  (eq call (telega-voip--incoming-call)))
         (let ((prefix (when (eq (telega-root--buffer) (window-buffer))
                         "\\<telega-root-mode-map>")))
           (message "telega: Press `%s' to answer, `%s' to decline"
                    (substitute-command-keys
                     (concat prefix "\\[telega-voip-accept]"))
                    (substitute-command-keys
                     (concat prefix "\\[telega-voip-discard]"))))))

      (callStateReady
       (unless (eq call telega-voip--active-call)
         (error "Another call became Ready, while having active call"))

       (run-hook-with-args 'telega-call-ready-hook call)

       (let ((start
              (list :@command "start"
                    :server_config (plist-get state :config)
                    :is_outgoing (or (plist-get call :is_outgoing) :false)
                    :encryption_key (plist-get state :encryption_key)
                    :allow_p2p (or telega-voip-allow-p2p :false)
                    :max_layer (telega--tl-get state :protocol :max_layer)
                    :endpoints (plist-get state :connections))))
         (when telega-voip-logfile
           (telega-server--send
            (list :@command "config"
                  :log-file-path telega-voip-logfile) "voip"))

         (telega-server--send start "voip"))

       (when telega-voip-help-echo
         (message "telega: Press `%s' to hang up"
                  (substitute-command-keys
                   (concat (when (eq (telega-root--buffer) (window-buffer))
                             "\\<telega-root-mode-map>")
                           "\\[telega-voip-discard]")))))

      (callStateError
       (let ((err (plist-get state :error))
             (user (telega-user-get (plist-get call :user_id))))
         (message "Error[%d] calling %s: %s" (plist-get err :code)
                  (telega-msg-sender-title user
                    :with-avatar-p t
                    :with-username-p t)
                  (plist-get err :message))))

      (callStateDiscarded
       (let ((discard (plist-get state :reason))
             (user (telega-user-get (plist-get call :user_id))))
         (message "Call %s discarded: %s" (telega-msg-sender-title user
                                            :with-avatar-p t
                                            :with-username-p t)
                  (substring (plist-get discard :@type) 17))))
      )

    ;; Delete call from the list, if call is ended
    (when (memq (telega--tl-type state) '(callStateError callStateDiscarded))
      (unwind-protect
          (run-hook-with-args 'telega-call-end-hook call)
        (when (eq telega-voip--active-call call)
          (telega-server--send (list :@command "stop") "voip")
          (setq telega-voip--active-call nil))
        (setq telega-voip--alist (assq-delete-all call-id telega-voip--alist))))

    ;; Update user
    (telega-user--update (telega-user-get (plist-get call :user_id)) event)

    ;; Update aux status
    (telega-root-aux-redisplay 'telega-ins--voip-active-call)
    ))

;; Stickers updates
(defun telega--on-updateInstalledStickerSets (event)
  "The list of installed sticker sets was updated."
  (cl-case (telega--tl-type (plist-get event :sticker_type))
    (stickerTypeMask
     (telega-debug "TODO: `telega--on-updateInstalledStickerSets' for masks"))

    (stickerTypeRegular
     (setq telega--stickersets-installed-ids
           (append (plist-get event :sticker_set_ids) nil))

     ;; NOTE: Refresh `telega--stickersets-installed' on next call to
     ;; `telega-stickerset-completing-read'
     (setq telega--stickersets-installed nil)

     ;; Asynchronously update value for `telega--stickersets-installed'
     ;; and download covers for these sticker sets
     ;; (telega--getInstalledStickerSets
     ;;   (lambda (ssets)
     ;;     (setq telega--stickersets-installed ssets)
     ;;     (dolist (sset ssets)
     ;;       (mapc #'telega-sticker--download (plist-get sset :covers)))))
     )

    (stickerTypeCustomEmoji
     (telega--getInstalledStickerSets
         :tl-sticker-type '(:@type "stickerTypeCustomEmoji")
         :callback
         (lambda (ssets)
           (setq telega--stickersets-custom-emojis ssets))))
    ))

(defun telega--on-updateTrendingStickerSets (event)
  "The list of trending sticker sets was updated or some of them were viewed."
  (let* ((trending-sset (plist-get event :sticker_sets))
         (ssets-info (plist-get trending-sset :sets)))
    (set (if (plist-get trending-sset :is_premium)
             'telega--stickersets-trending-premium
           'telega--stickersets-trending)
         (append ssets-info nil))))

(defun telega--on-updateRecentStickers (event)
  "Recent stickers has been updated."
  ;; NOTE: attached recent stickers are not supported
  (unless (plist-get event :is_attached)
    (setq telega--stickers-recent
          (append (plist-get event :sticker_ids) nil))
    ;; Asynchronously download corresponding files
;    (mapc 'telega--downloadFile telega--stickers-recent)
    ))

(defun telega--on-updateFavoriteStickers (event)
  "Favorite stickers has been updated."
  (setq telega--stickers-favorite
        (append (plist-get event :sticker_ids) nil))
  ;; Asynchronously download corresponding files
;  (mapc 'telega--downloadFile telega--stickers-favorite)
  )

(defun telega--on-updateSavedAnimations (event)
  "List of saved animations has been updated."
  (setq telega--animations-saved
        (append (plist-get event :animation_ids) nil))
  ;; Asynchronously download corresponding files
  (when telega-animation-download-saved
    (mapc 'telega--downloadFile telega--animations-saved)))

(defun telega--on-updateChatThemes (event)
  "List of chat themes has been updated."
  (setq telega--chat-themes
        (append (plist-get event :chat_themes) nil))
  )

;; since TDLib 1.6.3
(defun telega--on-updateStickerSet (event)
  (telega-stickerset--ensure (plist-get event :sticker_set)))


(defun telega--on-updateBasicGroup (event)
  (let ((basicgroup (plist-get event :basic_group)))
    (telega--info-update basicgroup)

    (when telega--sort-criteria
      (when-let ((chat (cl-find basicgroup telega--ordered-chats
                                :test 'eq :key #'telega-chat--info)))
        (telega-chat--mark-dirty chat event))

      ;; TODO: chatbuf might need to be updated, status might be
      ;; changed due to someone removed me from basic group
      )))

(defun telega--on-updateBasicGroupFullInfo (event)
  (let ((ufi (cdr (assq 'basicGroup telega--full-info))))
    (puthash (plist-get event :basic_group_id)
             (plist-get event :basic_group_full_info) ufi)))

(defun telega--on-updateSupergroup (event)
  "Handle supergroup update EVENT."
  (let* ((supergroup (plist-get event :supergroup))
         (old-supergroup (telega--info 'supergroup (plist-get supergroup :id)
                                       'locally))
         (old-my-status (plist-get old-supergroup :status))
         (me-was-owner (and old-my-status
                            (eq 'chatMemberStatusCreator
                                (telega--tl-type old-my-status)))))
    (telega--info-update supergroup)

    (when-let ((chat (cl-find supergroup telega--ordered-chats
                              :test 'eq :key 'telega-chat--supergroup)))
      ;; If :is_forum state is toggled, then update topics as well
      (unless (eq (plist-get old-supergroup :is_forum)
                  (plist-get supergroup :is_forum))
        (telega-chat--topics-fetch chat))

      ;; NOTE: notify if someone transferred ownership to me
      (when (and (not me-was-owner)
                 (telega-chat-match-p chat 'me-is-owner))
        (message "telega: me is now owner of the %s"
                 (telega-ins--as-string
                  (telega-ins--msg-sender chat
                    :with-avatar-p t
                    :with-username-p t
                    :with-brackets-p t))))

      (telega-chat--mark-dirty chat event)

      ;; NOTE: Chatbuf prompt might be affected as well by "status"
      ;; change, see `telega-chatbuf--unblock-start-join'
      (with-telega-chatbuf chat
        (unless (equal old-my-status (plist-get supergroup :status))
          (telega-chatbuf--footer-update))
        (telega-chatbuf--prompt-update)))

    ))

(defun telega--on-updateSupergroupFullInfo (event)
  (let* ((supergroup-id (plist-get event :supergroup_id))
         (supergroup-fi (plist-get event :supergroup_full_info))
         (fi-table (cdr (assq 'supergroup telega--full-info)))
         (old-supergroup-fi (gethash supergroup-id fi-table)))
    (puthash supergroup-id supergroup-fi fi-table)

    ;; NOTE: if slow delay expiration changes, then save timestamp of
    ;; this update event, so we could calculate time left in slow mode
    ;; before expiration
    (plist-put supergroup-fi :telega-update-event-timestamp
               (unless (zerop (plist-get
                               supergroup-fi :slow_mode_delay_expires_in))
                 (float-time)))

    ;; Check number of the admins has been changed, it might be not up
    ;; to date, see https://github.com/tdlib/td/issues/1040
    (when-let ((chat (telega-chat-get
                      (string-to-number (format "-100%d" supergroup-id))
                      'offline)))
      ;; TODO: Might affect root's buffer view
      ;; NOTE: chatbuf might need to be updated, since for example
      ;; pinned message might change
      (telega-chat--mark-dirty chat event)

      (with-telega-chatbuf chat
        (unless (equal (plist-get supergroup-fi :administrator_count)
                       (length telega-chatbuf--administrators))
          (telega-chatbuf--admins-fetch))

        (unless (equal (plist-get old-supergroup-fi :has_pinned_stories)
                       (plist-get supergroup-fi :has_pinned_stories))
          (telega-chatbuf--pinned-stories-fetch)))
      )))

(defun telega--on-updateUnreadMessageCount (event)
  "Number of unread messages has changed."
  (let ((chat-list (plist-get event :chat_list)))
    (when (or (null chat-list)
              (equal chat-list (list :@type "chatListMain")))
      (setq telega--unread-message-count (cddr event)))))

(defun telega--on-updateUnreadChatCount (event)
  "Number of unread/unmuted chats has been changed."
  (let ((chat-list (plist-get event :chat_list)))
    (when (or (null chat-list)
              (equal chat-list (list :@type "chatListMain")))
      (setq telega--unread-chat-count (cddr event)))))

(defun telega--on-updateUsersNearby (event)
  "Handle EVENT with update for users nearby chats."
  (seq-doseq (nb-chat (plist-get event :users_nearby))
    (telega-chat-nearby--ensure nb-chat)

    (when-let ((chat (telega-chat-get (plist-get nb-chat :chat_id) 'offline)))
      (telega-chat--mark-dirty chat event))
    ))

(defun telega--on-updateConnectionState (event)
  "Update telega connection state using EVENT."
  (let* ((conn-state (telega--tl-get event :state :@type))
         (status (substring conn-state 15)))
    (setq telega--conn-state (intern status))
    (telega-status--set status)

    (run-hooks 'telega-connection-state-hook)))

(defun telega--on-updateTermsOfService (event)
  "New terms of service must be accepted by the user."
  (let* ((tos-id (telega-tl-str event :terms_of_service_id))
         (tos (plist-get event :terms_of_service))
         (tos-text (telega-tl-str tos :text)))
    (with-help-window "*Telegram Terms of Service*"
      (telega-ins tos-text))
    ;; TODO: check for (plist-get tos :min_user_age)
    (if (and (not (yes-or-no-p
                   "Accept updated Telegram ToS? "))
             (not (yes-or-no-p
                   "Your Telegram account will be deleted, maybe accept ToS? ")))
        (if (telega-read-im-sure-p
             "Delete your Telegram account by declining Telegram ToS?")
            (telega-server--send
             (list :@type "deleteAccount"
                   :reason "Decline ToS update"))
          ;; Query ToS acceptance once again
          (telega--on-updateTermsOfService event))

      (telega--acceptTermsOfService tos-id
        (lambda (_reply)
          (message "telega: updated Telegram ToS has been accepted"))))
    ))

(defun telega--on-updateOption (event)
  "Proceed with option update from telega server using EVENT."
  (let* ((option (intern (concat ":" (plist-get event :name))))
         (opt-val (plist-get event :value))
         (value (plist-get opt-val :value)))
    ;; TDLib 1.6.9 has `optionValueInteger' as int64, represented as
    ;; string
    (when (and (eq 'optionValueInteger (telega--tl-type opt-val))
               (stringp value))
      (setq value (string-to-number value)))

    (setq telega--options
          (plist-put telega--options option value))

    (when (and (eq option :unix_time) value)
      ;; Mark unix-time as need to be updated when Emacs gets idle
      ;; time, it will be updated on next call to
      ;; `telega-server--idle-timer-function'
      (setq telega-tdlib--unix-time
            (plist-put telega-tdlib--unix-time :need-update t)))

    (when (and (eq option :my_id) value)
      (setq telega--me-id value))
    (when (and (eq option :replies_bot_chat_id) value)
      (setq telega--replies-id value))

    ;; Fetch Sticker Set with animated emojis
    (when (and (eq option :animated_emoji_sticker_set_name) value)
      (telega--searchStickerSet value
        (lambda (sset)
          (setq telega--animated-emojis-stickerset-id (plist-get sset :id))
          (telega-stickerset--ensure sset))))

    ;; NOTE: use language pack suggested by server as default
    ;; translate-to language, if not set
    (when (and (eq option :suggested_language_pack_id) value
               (not telega-translate-to-language-by-default))
      (setq telega-translate-to-language-by-default value))

    (when (and (eq option :is_location_visible) value)
      (if telega-my-location
          (telega--setLocation telega-my-location)

        (warn (concat "telega: Option `:is_location_visible' is set, "
                      "but `telega-my-location' is nil"))))

    (when (and (eq option :version) value)
      ;; Validate TDLib version
      (when-let ((version-error-msg
                  (cond ((version< (plist-get telega--options :version)
                                   telega-tdlib-min-version)
                         (format "TDLib version=%s < %s (min required)\n\
Please upgrade TDLib and recompile `telega-server'"
                                 (plist-get telega--options :version)
                                 telega-tdlib-min-version))
                        ((and telega-tdlib-max-version
                              (version< telega-tdlib-max-version
                                        (plist-get telega--options :version)))
                         (format "TDLib version=%s > %s (max required)\n\
Please downgrade TDLib and recompile `telega-server'"
                                 (plist-get telega--options :version)
                                 telega-tdlib-max-version)))))
        (with-telega-root-buffer
          (save-excursion
            (telega-ins--with-face 'error
              (telega-ins version-error-msg))))
        ;; NOTE: rootbuf might not be visible if `M-x telega RET' is
        ;; started with prefix argument, so notify user with warning
        ;; as well
        (warn version-error-msg)
        ;; Finally stop processing events from telega-server
        (telega-server-kill)
        (error version-error-msg)
        ))))

(defun telega--on-updateAuthorizationState (event)
  "Proceed with user authorization state change using EVENT."
  (let* ((state (plist-get event :authorization_state))
         (stype (plist-get state :@type)))
    (setq telega--auth-state (substring stype 18))
    (telega-status--set (concat "Auth " telega--auth-state))
    (cl-ecase (intern stype)
      (authorizationStateWaitTdlibParameters
       ;; Tune permissions for docker's /dev/snd, /dev/video*
       (when-let ((devices-chown-cmd
                   (telega-docker-exec-cmd
                    "chmod -R o+rw /dev/snd /dev/video0" nil
                    "-u 0" 'no-error)))
         (telega-debug "docker RUN: %s" devices-chown-cmd)
         (shell-command-to-string devices-chown-cmd))

       ;; NOTE: Setup proxies.  Only `telega-proxies' setup enables
       ;; some proxy.  See
       ;; https://github.com/zevlg/telega.el/issues/233
       (telega--disableProxy)
       (dolist (proxy telega-proxies)
         (telega--addProxy proxy))

       (telega--setTdlibParameters))

      (authorizationStateWaitPhoneNumber
       (if (and (not telega--relogin-with-phone-number)
                telega-use-images
                (or (executable-find "qrencode")
                    ;; NOTE: docker image has "qrencode" tool
                    telega-use-docker))
           (progn
             ;; NOTE: transition to
             ;; "authorizationStateWaitOtherDeviceConfirmation" state
             ;; takes some time, so display QR code dialog first, then
             ;; wait for QR code itself
             (telega--requestQrCodeAuthentication)
             (telega-qr-code--show nil))

         (setq telega--relogin-with-phone-number nil)
         (telega--setAuthenticationPhoneNumber
          (read-string "Telega phone number: " "+"))))

      (authorizationStateWaitCode
       (let ((code (read-string "Telega login code: ")))
         (telega--checkAuthenticationCode code)))

      (authorizationStateWaitOtherDeviceConfirmation
       (telega-qr-code--show (plist-get state :link)))

      (authorizationStateWaitRegistration
       (let* ((names (split-string (read-from-minibuffer "Your Name: ") " "))
              (first-name (car names))
              (last-name (mapconcat 'identity (cdr names) " ")))
         (telega--registerUser first-name last-name)))

      (authorizationStateWaitPassword
       (let* ((hint (plist-get state :password_hint))
              (pass (password-read
                     (concat "Telegram password"
                             (if (string-empty-p hint)
                                 ""
                               (format "(hint='%s')" hint))
                             ": "))))
         (telega--checkAuthenticationPassword pass)))


      (authorizationStateReady
       ;; Hide previously possibly shown QR code auth dialog
       (telega-qr-code--hide)

       ;; TDLib is now ready to answer queries
       (telega--authorization-ready))

      (authorizationStateLoggingOut
       )

      (authorizationStateClosing
       )

      (authorizationStateClosed
       (telega-server-kill)))))

(defun telega--on-updateServiceNotification (event)
  "Handle service notification EVENT from the server."
  (let ((help-window-select t))
    (with-telega-help-win "*Telega Service Notification*"
      ;; NOTE: use `telega-ins--content' in hope that only `:content'
      ;; property is used
      (telega-ins--with-attrs (list :fill 'center
                                    :fill-column telega-chat-fill-column)
        (telega-ins--content event))
      (when (string-prefix-p "AUTH_KEY_DROP_" (plist-get event :type))
        (telega-ins "\n")
        (telega-ins--button "Cancel"
          'action (lambda (_ignored)
                    (quit-window)))
        (telega-ins " ")
        (telega-ins--button "Logout"
          'action (lambda (_ignored)
                    (when (yes-or-no-p "Destroy all local data? ")
                      (telega-server--send (list :@type "destroy")))))))))

(defun telega--on-updateFile (event)
  "File has been updated, call all the associated hooks."
  (telega-file--update (plist-get event :file))

  ;; Update "Files" root view as well
  (telega-root-view--update :on-file-update (plist-get event :file)))

(defun telega--on-updateScopeNotificationSettings (event)
  "Handle `updateScopeNotificationSettings' EVENT."
  (let ((scope-type (telega--tl-get event :scope :@type)))
    (setf (alist-get scope-type telega--scope-notification-alist
                     nil nil #'string=)
          (plist-get event :notification_settings))

    (telega-root-view--update :on-notifications-update)

    (telega-describe-notifications--maybe-redisplay)
    ))

(defun telega--on-updateDiceEmojis (event)
  (setq telega--dice-emojis
        (mapcar #'telega--desurrogate-apply (plist-get event :emojis))))

(defun telega--on-updateAnimatedEmojiMessageClicked (event)
  "Animated emoji message has been clicked on by other side."
  (with-telega--msg-update-event event (chat msg node)
    (when (and telega-sticker-animated-play
               node
               (with-telega-chatbuf chat
                 (telega-chatbuf--msg-observable-p msg node)))
      (let ((fs-sticker (plist-get event :sticker)))
        (plist-put msg :telega-sticker-fullscreen fs-sticker)
        (telega-sticker--animate fs-sticker msg)

        (with-telega-chatbuf chat
          (telega-chatbuf--set-action "WatchingAnimations"))
        ))))

(defun telega--on-updateSuggestedActions (event)
  (let ((added-actions (append (plist-get event :added_actions) nil))
        (removed-actions (append (plist-get event :removed_actions) nil)))
    (setq telega--suggested-actions
          (append (seq-difference telega--suggested-actions removed-actions
                                  #'equal)
                  added-actions))))

(defun telega--on-updateHavePendingNotifications (_event)
  ;; We define this to avoid
  ;; "TODO: define `telega--on-updateHavePendingNotifications'"
  ;; messages in the *telega-debug*
  )


;;; Reactions
(defun telega--on-updateActiveEmojiReactions (event)
  (setq telega-emoji-reaction-list
        (mapcar #'telega--desurrogate-apply (plist-get event :emojis))))

(defun telega--on-updateDefaultReactionType (event)
  (setq telega-default-reaction-type
        (plist-get event :reaction_type)))

(defun telega--on-updateChatAvailableReactions (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :available_reactions
               (plist-get event :available_reactions))))

(defun telega--on-updateChatUnreadReactionCount (event &optional chat)
  (unless chat
    (setq chat (telega-chat-get (plist-get event :chat_id) 'offline)))

  (cl-assert chat)
  (plist-put chat :unread_reaction_count
             (plist-get event :unread_reaction_count))

  (telega-chat--mark-dirty chat event))

(defun telega--on-updateMessageUnreadReactions (event)
  (with-telega--msg-update-event event (chat msg node)
    (cl-assert chat)
    (telega--on-updateChatUnreadReactionCount event chat)

    (plist-put msg :unread_reactions (plist-get event :unread_reactions))
    (when node
      (with-telega-chatbuf chat
        (telega-chatbuf--redisplay-node node)))
    ))

(defun telega--on-updateAddChatMembersPrivacyForbidden (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id)))
        (users (mapcar #'telega-user-get (plist-get event :user_ids))))
    (plist-put chat :telega-add-member-forbidden-users users)
    (with-telega-chatbuf chat
      (telega-chatbuf--footer-update))
    ))

;; Stories
(defun telega--on-updateStory (event)
  (telega-story--ensure (plist-get event :story)))

(defun telega--on-updateStoryDeleted (event)
  "A story became inaccessible."
  (let* ((chat-id (plist-get event :story_sender_chat_id))
         (story-id (plist-get event :story_id))
         (story (telega-story-get chat-id story-id 'offline)))
    (unless story
      (setq story (list :sender_chat_id chat-id
                        :id story-id)))

    (setf (telega-story-deleted-p story) t)
    (telega-story--ensure story)))

(defun telega--on-updateChatActiveStories (event)
  "The list of active stories posted by a specific chat has changed."
  (let* ((active-stories (plist-get event :active_stories))
         (chat (telega-chat-get (plist-get active-stories :chat_id) 'offline)))
    (setf (telega-chat--active-stories chat) active-stories)

    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateStoryListChatCount (event)
  "Number of chats in a story list has changed."
  (let ((story-list (plist-get event :story_list))
        (chat-count (plist-get event :chat_count)))

    (plist-put telega--story-list-chat-count
               (cl-ecase (telega--tl-type story-list)
                 (storyListMain 'main)
                 (storyListArchive 'archive))
               chat-count)
    ))

(defun telega--on-updateAccentColors (event)
  "The list of supported accent colors has changed."
  (setq telega--accent-colors-alist nil)
  (seq-doseq (color (plist-get event :colors))
    (setf (alist-get (plist-get color :id) telega--accent-colors-alist)
          color))

  (setq telega--accent-colors-available-ids
        (plist-get event :available_accent_color_ids)))

(provide 'telega-tdlib-events)

;;; telega-tdlib-events.el ends here

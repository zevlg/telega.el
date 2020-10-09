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

(defvar tracking-buffers nil)
(declare-function telega--authorization-ready "telega")


(defun telega--event-reorders-chat-p (event)
  "Return non-nil if EVENT causes CHAT reorder."
  (let ((event-type (plist-get event :@type)))
    (or (member event-type
                '("updateNewChat" "updateChatPosition"
                  "updateChatLastMessage" "updateChatDraftMessage"

                  ;; Special fake event used by telega to force chat
                  ;; reordering
                  "telegaChatReorder"
                  ))
        (and telega--sort-criteria
             (cl-some (lambda (criteria-sym)
                        (member event-type
                                (get criteria-sym :telega-order-events)))
                      telega--sort-criteria)))))

(defun telega-chat--update (chat &rest events)
  "Something changed in CHAT, button needs to be updated.
If FOR-REORDER is non-nil, then CHAT's node is ok, just update filters."
  (telega-debug "IN: `telega-chat--update': %s" (telega-chat-title chat))

  ;; Reordering requires? Reorder CHAT by removing and then adding it
  ;; again at correct place
  (when (cl-some #'telega--event-reorders-chat-p events)
    (plist-put chat :telega-need-reorder-p t)
    (setq telega--ordered-chats (delq chat telega--ordered-chats))
    (telega--ordered-chats-insert chat))

  ;; Update root ewocs and filters
  (telega-root-view--update :on-chat-update chat events)
  (telega-filters--chat-update chat)
  (plist-put chat :telega-need-reorder-p nil)

  (telega-describe-chat--maybe-redisplay chat)

  (run-hook-with-args 'telega-chat-update-hook chat))

(defun telega-chat--mark-dirty (chat &rest events)
  "Mark CHAT as dirty by EVENTS."
  (let ((dc (assq chat telega--dirty-chats)))
    (if dc
        (setcdr (last dc) events)
      (setq telega--dirty-chats
            (cons (cons chat events) telega--dirty-chats)))))

(defun telega-chats-dirty--update ()
  "Update dirty chats."
  (dolist (dirty-chat (prog1 telega--dirty-chats
                        (setq telega--dirty-chats nil)))
    (apply #'telega-chat--update dirty-chat)))


(defun telega--on-ok (_event)
  "On ok result from command function call."
  ;; no-op
  )

;; User updates
(defun telega-user--update (user)
  "USER has been updated, do something about this."
  (telega-root-view--update :on-user-update user)
  (telega-describe-user--maybe-redisplay (plist-get user :id))

  (run-hook-with-args 'telega-user-update-hook user))

(defun telega--on-updateUser (event)
  "Some user info has has been changed."
  (let ((user (plist-get event :user)))
    (telega--info-update user)

    (telega-user--update user)

    ;; Update corresponding private chat button
    (when-let ((chat (telega-chat-get (plist-get user :id) 'offline)))
      (telega-chat--mark-dirty chat event))))

(defun telega--on-updateUserStatus (event)
  "User status has been changed."
  (let* ((user-id (plist-get event :user_id))
         (user (telega-user--get user-id))
         (status (plist-get event :status)))
    (plist-put user :status status)
    ;; NOTE: For online status, set special USER property with value
    ;; of time last seen online
    (when (eq (telega--tl-type status) 'userStatusOnline)
      (plist-put user :telega-last-online (telega-time-seconds)))

    ;; NOTE: do not track me online status changes
    (unless (telega-me-p user)
      (telega-user--update user)

      (when-let ((chat (telega-chat-get user-id 'offline)))
        (with-telega-chatbuf chat
          (telega-chatbuf-mode-line-update))
        (telega-chat--mark-dirty chat event)))
    ))

(defun telega--on-updateUserChatAction (event)
  "Some user has actions on chat."
  (let* ((chat-id (plist-get event :chat_id))
         (chat-actions (gethash chat-id telega--actions))
         (user-id (plist-get event :user_id))
         (user-action (assq user-id chat-actions))
         (action (plist-get event :action))
         (cancel-p (eq (telega--tl-type action) 'chatActionCancel)))
    (cond (cancel-p
           (puthash chat-id (assq-delete-all user-id chat-actions)
                    telega--actions))
          (user-action
           (setcdr user-action action))
          (t (puthash chat-id (cons (cons user-id action) chat-actions)
                      telega--actions)))

    (let ((chat (telega-chat-get chat-id)))
      (telega-chat--mark-dirty chat event)

      (with-telega-chatbuf chat
        ;; If action by me, update `telega-chatbuf--my-action' as well
        (when (eq user-id telega--me-id)
          (setq telega-chatbuf--my-action (unless cancel-p action)))
        (telega-chatbuf--footer-redisplay)))
    ))

(defun telega--on-updateUserFullInfo (event)
  (let ((user-id (plist-get event :user_id))
        (ufi (cdr (assq 'user telega--full-info))))
    (puthash user-id (plist-get event :user_full_info) ufi)

    (telega-user--update (telega-user--get user-id))

    ;; Might affect root's buffer view
    ;; because for example `:is_blocked' might be used
    (when-let ((chat (telega-chat-get user-id 'offline)))
      (telega-chat--mark-dirty chat event)

      ;; Might affect chatbuf prompt as well to display/remove
      ;; [UNBLOCK] ujs prompt button
      (with-telega-chatbuf chat
        (telega-chatbuf--prompt-update))
      )))


;; Chat updates
(defun telega--on-updateNewChat (event)
  "New chat has been loaded or created."
  (let ((chat (telega-chat--ensure (plist-get event :chat))))
    (telega-chat--mark-dirty chat event)

    (run-hook-with-args 'telega-chat-created-hook chat)))

(defun telega--on-updateChatPhoto (event)
  "Chat's photo has been updated."
  (let ((chat (telega-chat-get (plist-get event :chat_id)))
        (photo (plist-get event :photo)))
    (plist-put chat :photo photo)
    (plist-put chat :telega-image nil)

    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateChatNotificationSettings (event)
  "Notification settings has been changed in chat."
  (let ((chat (telega-chat-get (plist-get event :chat_id))))
    (plist-put chat :notification_settings
               (plist-get event :notification_settings))

    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateChatTitle (event)
  "EVENT arrives when title of a chat was changed."
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :title (plist-get event :title))

    (telega-chat--mark-dirty chat event)

    (with-telega-chatbuf chat
      (rename-buffer (telega-chatbuf--name chat)))
    ))

(defun telega--on-updateChatOrder (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :order (plist-get event :order))

    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateChatPosition (event)
  (let* ((chat (telega-chat-get (plist-get event :chat_id) 'offline))
         (new-pos (plist-get event :position))
         (old-pos (cl-find (plist-get new-pos :list) (plist-get chat :positions)
                           :key (telega--tl-prop :list) :test #'equal)))
    (if old-pos
        ;; If new order is \"0\", then remove this position, otherwise
        ;; modify it inplace
        (if (equal "0" (plist-get new-pos :order))
            (plist-put chat :positions
                       (vconcat (seq-remove
                                 (apply-partially #'eq old-pos)
                                 (plist-get chat :positions))))
          (plist-put old-pos :order (plist-get new-pos :order))
          (plist-put old-pos :is_pinned (plist-get new-pos :is_pinned)))
      (plist-put chat :positions (vconcat (plist-get chat :positions)
                                          (list new-pos))))

    (telega-chat--mark-dirty chat event)
  ))

(defun telega--on-updateChatReadInbox (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline))
        (unread-count (plist-get event :unread_count)))
    (cl-assert chat)
    (plist-put chat :last_read_inbox_message_id
               (plist-get event :last_read_inbox_message_id))
    (plist-put chat :unread_count unread-count)

    (telega-chat--mark-dirty chat event)

    ;; NOTE: unread_count affects modeline and footer
    (with-telega-chatbuf chat
      ;; NOTE: if all messages are read (in another telegram client),
      ;; then remove the chatbuf from tracking
      (when (and (zerop unread-count)
                 (member (buffer-name) tracking-buffers))
        (tracking-remove-buffer (current-buffer)))

      (telega-chatbuf-mode-line-update)
      (telega-chatbuf--footer-redisplay))))

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

  (telega-chat--mark-dirty chat event)

  ;; NOTE: unread_mention_count affects modeline and footer
  (with-telega-chatbuf chat
    (telega-chatbuf-mode-line-update)
    (telega-chatbuf--footer-redisplay)))

(defun telega--on-updateMessageMentionRead (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (telega--on-updateChatUnreadMentionCount event chat)

    ;; Update message's `:contains_unread_mention' as well
    ;; This requires message redisplay, since message could outline
    ;; unread mention
    (with-telega-chatbuf chat
      (cl-destructuring-bind (msg node)
          (telega-chatbuf--msg (plist-get event :message_id) 'with-node)
        (plist-put msg :contains_unread_mention nil)
        (when node
          (telega-chatbuf--redisplay-node node))))
    ))

(defun telega--on-updateChatDefaultDisableNotification (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :default_disable_notification
               (plist-get event :default_disable_notification))
    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateChatLastMessage (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :last_message (plist-get event :last_message))
    (plist-put chat :positions (plist-get event :positions))
    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateChatIsMarkedAsUnread (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :is_marked_as_unread
               (plist-get event :is_marked_as_unread))

    (telega-chat--mark-dirty chat event)))

(defun telega--on-updateChatOnlineMemberCount (event)
  "The number of online group members has changed.
NOTE: we store the number as custom chat property, to use it later."
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :x-online-count
               (plist-get event :online_member_count))

    (telega-chat--mark-dirty chat event)

    ;; NOTE: this affects the modeline
    (with-telega-chatbuf chat
      (telega-chatbuf-mode-line-update))
    ))

(defun telega--on-updateChatPinnedMessage (event)
  "The chat pinned message was changed."
  (let* ((chat (telega-chat-get (plist-get event :chat_id) 'offline))
         (old-pinned-message-id (plist-get chat :pinned_message_id)))
    (cl-assert chat)
    (plist-put chat :pinned_message_id
               (plist-get event :pinned_message_id))

    (telega-chat--update-pinned-message chat nil old-pinned-message-id)
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
      (telega-chatbuf--input-draft draft-msg))
    ))

(defun telega--on-updateChatChatList (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline))
        (chat-list (plist-get event :chat_list)))
    (cl-assert chat)
    (plist-put chat :chat_list chat-list)

    (telega-chat--mark-dirty chat event)))

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

    (telega-chat--mark-dirty chat event)

    (with-telega-chatbuf chat
      (telega-chatbuf--footer-redisplay))))

(defun telega--on-updateSecretChat (event)
  (let ((secretchat (plist-get event :secret_chat)))
    (telega--info-update secretchat)

    ;; update corresponding secret chat button
    (when-let ((chat (cl-find secretchat
                              (telega-filter-chats
                               telega--ordered-chats '(type secret))
                              :test 'eq :key #'telega-chat--info)))
      (telega-chat--mark-dirty chat event))))

(defun telega--on-initial-chats-fetch (chats)
  "Ensure chats from RESULT exists, and continue fetching chats."
  (if (> (length chats) 0)
      ;; Continue fetching chats, redisplaying custom filters
      (telega--getChats (car (last chats)) (list :@type "chatListMain")
        #'telega--on-initial-chats-fetch)

    ;; All chats has been fetched
    (telega-status--set nil "")
    (run-hooks 'telega-chats-fetched-hook)))

(defun telega--on-updateChatReplyMarkup (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :reply_markup_message_id
               (plist-get event :reply_markup_message_id))

    (telega-chat--update-reply-markup-message chat)))


;; Chat filters
(defun telega--on-updateChatFilters (event)
  "List of chat filters has been updated."
  (setq telega-tdlib--chat-filters
        (append (plist-get event :chat_filters) nil))

  ;; Update custom filters ewoc
  (with-telega-root-buffer
    (telega-save-cursor
      (telega-filters--refresh))

    (run-hooks 'telega-root-update-hook))
  )


;; Messages updates
(defun telega-message--update (msg)
  "Message MSG has been updated."
  (when-let ((chat (telega-msg-chat msg 'offline)))
    (when (eq (plist-get chat :pinned_message_id)
              (plist-get msg :id))
      (plist-put chat :telega-pinned-message msg)))

  (telega-root-view--update :on-message-update msg)
  )

(defalias 'telega--on-message 'ignore)

(defun telega--on-updateNewMessage (event)
  "A new message was received; can also be an outgoing message."
  (let ((new-msg (plist-get event :message)))
    (run-hook-with-args 'telega-chat-pre-message-hook new-msg)

    (with-telega-chatbuf (telega-msg-chat new-msg)
      (telega-chatbuf--cache-msg new-msg)

      ;; NOTE: `:last_message' could be already updated in the chat
      ;; with the id of the NEW-MSG, so check for it
      (when (or (telega-chatbuf--last-msg-loaded-p)
                (eq (telega--tl-get telega-chatbuf--chat :last_message :id)
                    (plist-get new-msg :id)))
        (when-let ((node (telega-chatbuf--append-messages (list new-msg))))
          (when (and (telega-chat-match-p
                      telega-chatbuf--chat telega-use-tracking-for)
                     (not (plist-get new-msg :is_outgoing))
                     (not (telega-msg-seen-p new-msg telega-chatbuf--chat)))
            (tracking-add-buffer (current-buffer)))

          ;; If message is visibible in some window, then mark it as read
          ;; see https://github.com/zevlg/telega.el/issues/4
          (when (telega-msg-observable-p new-msg telega-chatbuf--chat node)
            (telega--viewMessages telega-chatbuf--chat (list new-msg)))
          )))
    (run-hook-with-args 'telega-chat-post-message-hook new-msg)))

(defun telega--on-updateMessageSendSucceeded (event)
  "Message has been successfully sent to server.
Message id could be updated on this update."
  (let* ((new-msg (plist-get event :message))
         (new-id (plist-get new-msg :id))
         (old-id (plist-get event :old_message_id)))
    (with-telega-chatbuf (telega-msg-chat new-msg)
      (remhash old-id telega-chatbuf--messages)
      (puthash new-id new-msg telega-chatbuf--messages)

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

            (ewoc-delete telega-chatbuf--ewoc node)
            (with-telega-deferred-events
              (if before-node
                  ;; NOTE: need to redisplay next to newly created
                  ;; node, in case `telega-chat-group-messages-for' is
                  ;; used, see https://github.com/zevlg/telega.el/issues/159
                  (progn
                    (ewoc-enter-before telega-chatbuf--ewoc before-node new-msg)
                    (ewoc-invalidate telega-chatbuf--ewoc before-node))
                (ewoc-enter-last telega-chatbuf--ewoc new-msg)))))))))

(defun telega--on-updateMessageSendFailed (event)
  "Message failed to send."
  ;; NOTE: Triggered for example if trying to send bad picture.
  ;; `telega--on-updateMessageSendSucceeded' updates the message
  ;; content with new(failed) state
  (telega--on-updateMessageSendSucceeded event)

  (let ((err-code (plist-get event :error_code))
        (err-msg (plist-get event :error_message)))
    (message "telega: Failed to send message: %d %s" err-code err-msg)
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
      (with-telega-chatbuf (telega-chat-get (plist-get event :chat_id))
        (cl-destructuring-bind (msg node)
            (telega-chatbuf--msg (plist-get event :message_id) 'with-node)
          (plist-put msg :content new-content)
          (when node
            (telega-chatbuf--redisplay-node node)))))))

(defun telega--on-updateMessageEdited (event)
  "Edited date of the message specified by EVENT has been changed."
  (with-telega-chatbuf (telega-chat-get (plist-get event :chat_id))
    (cl-destructuring-bind (msg node)
        (telega-chatbuf--msg (plist-get event :message_id) 'with-node)
      (plist-put msg :edit_date (plist-get event :edit_date))
      (plist-put msg :reply_markup (plist-get event :reply_markup))
      (when node
        (telega-chatbuf--redisplay-node node))

      ;; In case user has active aux-button with message from EVENT,
      ;; then redisplay aux as well, see
      ;; https://t.me/emacs_telega/7243
      (let ((aux-msg (or (telega-chatbuf--replying-msg)
                         (telega-chatbuf--editing-msg))))
        (when (and aux-msg (eq (plist-get msg :id) (plist-get aux-msg :id)))
          (cl-assert (not (button-get telega-chatbuf--aux-button 'invisible)))
          (telega-save-excursion
            (telega-button--update-value
             telega-chatbuf--aux-button msg
             :inserter (button-get telega-chatbuf--aux-button :inserter)))))
      )))

(defun telega--on-updateMessageInteractionInfo (event)
  "Message interaction info has been changed."
  (with-telega-chatbuf (telega-chat-get (plist-get event :chat_id))
    (cl-destructuring-bind (msg node)
        (telega-chatbuf--msg (plist-get event :message_id) 'with-node)
      (plist-put msg :interaction_info (plist-get event :interaction_info))
      (when node
        (telega-chatbuf--redisplay-node node)))))

(defun telega--on-updateMessageContentOpened (event)
  "The message content was opened.
Updates voice note messages to \"listened\", video note messages
to \"viewed\" and starts the TTL timer for self-destructing
messages."
  (with-telega-chatbuf (telega-chat-get (plist-get event :chat_id))
    (cl-destructuring-bind (msg node)
        (telega-chatbuf--msg (plist-get event :message_id) 'with-node)
      (let ((content (plist-get msg :content)))
        (cl-case (telega--tl-type content)
          (messageVoiceNote
           (plist-put content :is_listened t)
           (when node
             (telega-chatbuf--redisplay-node node)))
          (messageVideoNote
           (plist-put content :is_viewed t)
           (when node
             (telega-chatbuf--redisplay-node node))))))))

(defun telega--on-updateDeleteMessages (event)
  "Some messages has been deleted from chat."
  (with-telega-chatbuf (telega-chat-get (plist-get event :chat_id))
    (let ((from-cache-p (plist-get event :from_cache))
          (permanent-p (plist-get event :is_permanent)))
      (seq-doseq (msg-id (plist-get event :message_ids))
        (when from-cache-p
          (remhash msg-id telega-chatbuf--messages))
        (when permanent-p
          (remhash msg-id telega-chatbuf--messages)
          (when-let ((node (telega-chatbuf--node-by-msg-id msg-id))
                     (msg (ewoc--node-data node)))
            (plist-put msg :telega-is-deleted-message t)
            (if (telega-chat-match-p (telega-msg-chat msg)
                                     telega-chat-show-deleted-messages-for)
                (telega-msg-redisplay msg node)

              ;; NOTE: need to redisplay next to deleted node, in case
              ;; `telega-chat-group-messages-for' is used
              ;; See https://github.com/zevlg/telega.el/issues/159
              (let ((next-node (ewoc-next telega-chatbuf--ewoc node)))
                (ewoc-delete telega-chatbuf--ewoc node)
                (when next-node
                  (ewoc-invalidate telega-chatbuf--ewoc next-node)))))
          (when (eq msg-id (plist-get telega-chatbuf--chat :pinned_message_id))
            (telega-chat--update-pinned-message telega-chatbuf--chat 'offline)))
        ))))


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
             (user (telega-user--get (plist-get call :user_id))))
         (message "Error[%d] calling %s: %s" (plist-get err :code)
                  (telega-user--name user) (plist-get err :message))))

      (callStateDiscarded
       (let ((discad (plist-get state :reason))
             (user (telega-user--get (plist-get call :user_id))))
         (message "Call %s discaded: %s" (telega-user--name user)
                  (substring (plist-get discad :@type) 17))))
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
    (telega-user--update (telega-user--get (plist-get call :user_id)))

    ;; Update corresponding chat button
    (when-let ((chat (telega-chat-get (plist-get call :user_id) 'offline)))
      (telega-chat--mark-dirty chat event))

    ;; Update aux status
    (telega-voip--aux-status
     (or telega-voip--active-call (telega-voip--incoming-call)))
    ))

;; Stickers updates
(defun telega--on-updateInstalledStickerSets (event)
  "The list of installed sticker sets was updated."
  (if (plist-get event :is_masks)
      (telega-debug "TODO: `telega--on-updateInstalledStickerSets' is_mask=True")

    (setq telega--stickersets-installed-ids
          (append (plist-get event :sticker_set_ids) nil))

    ;; Asynchronously update value for `telega--stickersets-installed'
    ;; and download covers for these sticker sets
    ;; (telega--getInstalledStickerSets nil
    ;;   (lambda (ssets)
    ;;     (setq telega--stickersets-installed ssets)
    ;;     (dolist (sset ssets)
    ;;       (mapc #'telega-sticker--download (plist-get sset :covers)))))
    ))

(defun telega--on-updateTrendingStickerSets (event)
  "The list of trending sticker sets was updated or some of them were viewed."
  (let ((ssets-info (telega--tl-get event :sticker_sets :sets)))
    (setq telega--stickersets-trending
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
         (old-my-status
          (plist-get (telega--info 'supergroup (plist-get supergroup :id)
                                   'locally) :status))
         (me-was-owner (and old-my-status (telega--tl-type old-my-status)
                            'chatMemberStatusCreator)))
    (telega--info-update supergroup)

    (when-let ((chat (cl-find supergroup telega--ordered-chats
                              :test 'eq :key 'telega-chat--supergroup)))
      ;; NOTE: notify if someone transferred ownership to me
      (when (and (not me-was-owner)
                 (telega-chat-match-p chat 'me-is-owner))
        (message "telega: me is now owner of the %s"
                 (telega-chat-title-with-brackets chat " ")))

      (when telega--sort-criteria
        (telega-chat--mark-dirty chat event))

      ;; NOTE: Chatbuf prompt might be affected as well by "status"
      ;; change, see `telega-chatbuf--unblock-start-join'
      (with-telega-chatbuf chat
        (telega-chatbuf--prompt-update)))
    ))

(defun telega--on-updateSupergroupFullInfo (event)
  (let ((ufi (cdr (assq 'supergroup telega--full-info))))
    (puthash (plist-get event :supergroup_id)
             (plist-get event :supergroup_full_info) ufi)
    ;; Might affect root's buffer view
    ;; TODO: chatbuf might need to be updated, since for example
    ;; pinned message might change

    ;; Check number of the admins has been changed, it might be not up
    ;; to date, see https://github.com/tdlib/td/issues/1040
    (when-let ((chat (telega-chat-get
                      (string-to-number
                       (format "-100%d" (plist-get event :supergroup_id)))
                      'offline)))
      (with-telega-chatbuf chat
        (unless (equal (plist-get ufi :administrator_count)
                       (length telega-chatbuf--administrators))
          (telega-chat--update-administrators chat))))
    ))

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

    (when (and (eq option :is_location_visible) value)
      (if telega-my-location
          (telega--setLocation telega-my-location)

        (warn (concat "telega: Option `:is_location_visible' is set, "
                      "but `telega-my-location' is nil"))))))

(defun telega--on-updateAuthorizationState (event)
  "Proceed with user authorization state change using EVENT."
  (let* ((state (plist-get event :authorization_state))
         (stype (plist-get state :@type)))
    (setq telega--auth-state (substring stype 18))
    (telega-status--set (concat "Auth " telega--auth-state))
    (cl-ecase (intern stype)
      (authorizationStateWaitTdlibParameters
       (telega--setTdlibParameters))

      (authorizationStateWaitEncryptionKey
       (telega--checkDatabaseEncryptionKey)
       ;; List of proxies, since tdlib 1.3.0
       ;; Do it after checkDatabaseEncryptionKey,
       ;; See https://github.com/tdlib/td/issues/456
       (dolist (proxy telega-proxies)
         (telega--addProxy proxy))
       )

      (authorizationStateWaitPhoneNumber
       (let ((phone (read-string "Telega phone number: " "+")))
         (telega--setAuthenticationPhoneNumber phone)))

      (authorizationStateWaitCode
       (let ((code (read-string "Telega login code: ")))
         (telega--checkAuthenticationCode code)))

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
  (telega-file--update (plist-get event :file)))

(defun telega--on-updateScopeNotificationSettings (event)
  "Handle `updateScopeNotificationSettings' EVENT."
  (let* ((scope (plist-get event :scope))
         (scope-key (cl-ecase (telega--tl-type scope)
                      (notificationSettingsScopePrivateChats 'private)
                      (notificationSettingsScopeGroupChats 'group)
                      (notificationSettingsScopeChannelChats 'channel))))
    (setf (alist-get scope-key telega--scope-notification-alist)
          (plist-get event :notification_settings))))

;; since TDLib 1.6.3
(defun telega--on-updateDiceEmojis (event)
  (setq telega--dice-emojis
        (mapcar #'telega--desurrogate-apply (plist-get event :emojis))))

(provide 'telega-tdlib-events)

;;; telega-tdlib-events.el ends here

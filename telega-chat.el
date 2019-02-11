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
(require 'cl-lib)
(require 'ring)
(require 'telega-core)
(require 'telega-msg)
(require 'telega-voip)                  ;telega-voip-call
(require 'telega-notifications)

(eval-when-compile (require 'cl))       ;defsetf

(declare-function tracking-add-buffer "tracking" (buffer &optional faces))
(declare-function tracking-remove-buffer "tracking" (buffer))

(declare-function telega-root--chat-update "telega-root"
                  (chat &optional for-reorder))
(declare-function telega-root--chat-reorder "telega-root" (chat))

(defun telega-chat--set-uaprops (chat uaprops)
  "Set CHAT's user application properties to UAPROPS."
  (plist-put chat :uaprops uaprops)
  (let ((client-data (if uaprops (prin1-to-string uaprops) "")))
    (plist-put chat :client_data client-data)
    (telega-server--send
     (list :@type "setChatClientData"
           :chat_id (plist-get chat :id)
           :client_data client-data))))

(defmacro telega-chat-uaprop-del (chat uaprop-name)
  "Delete custom CHAT property named UAPROP-NAME."
  `(telega-chat--set-uaprops
    (cl--plist-remove (plist-get ,chat :uaprops) ,uaprop-name)))

(defmacro telega-chat-uaprop (chat uaprop-name)
  "Return value for CHAT's custom property with name UAPROP-NAME."
  `(plist-get (plist-get ,chat :uaprops) ,uaprop-name))

(defsetf telega-chat-uaprop (chat uaprop-name) (value)
  `(telega-chat--set-uaprops
    ,chat (plist-put (plist-get ,chat :uaprops) ,uaprop-name ,value)))

(defsubst telega-chat--order (chat)
  (string-to-number
   (or (telega-chat-uaprop chat :order) (plist-get chat :order))))

(defsubst telega--ordered-chats-insert (chat)
  "Insert CHAT into `telega--ordered-chats' according to CHAT's order."
  (let ((place telega--ordered-chats))
    (if (or (null place)
            (> (telega-chat--order chat)
               (telega-chat--order (car place))))
        (setq telega--ordered-chats (push chat telega--ordered-chats))

      (while (and (not (< (telega-chat--order (car place))
                          (telega-chat--order chat)))
                  (cdr place)
                  (not (< (telega-chat--order (cadr place))
                          (telega-chat--order chat))))
        (setq place (cdr place)))
      (cl-assert place)
      (setcdr place (cons chat (cdr place))))
    telega--ordered-chats))

(defsubst telega-chat--ensure (chat)
  "Ensure CHAT resides in `telega--chats' and `telega--ordered-chats'.
Return chat from `telega--chats'."
  (let ((chat-id (plist-get chat :id)))
    (or (gethash chat-id telega--chats)
        (prog1
            chat

          (puthash chat-id chat telega--chats)
          ;; Place chat in correct place inside `telega--ordered-chats'
          (telega--ordered-chats-insert chat)

          ;; parse :client_data as plist, we use it to store
          ;; additional chat properties (user application properties)
          (let ((client-data (plist-get chat :client_data)))
            (unless (string-empty-p client-data)
              (ignore-errors
                (plist-put chat :uaprops (car (read-from-string client-data))))))

          ;; Assign the chat some color (used to draw avatars and
          ;; highlight users in chat)
          ;; We use custom chat property, so it is saved between restarts
          ;; list of three elements - (LIGHT-COLOR COLOR DARK-COLOR)
          (unless (telega-chat-uaprop chat :color)
            (let ((col (telega-color-random)))
              (setf (telega-chat-uaprop chat :color)
                    (list (telega-color-gradient col 'light)
                          col
                          (telega-color-gradient col)))))
          ))))

(defun telega-chat-get (chat-id &optional offline-p)
  "Get chat by its CHAT-ID.
If OFFLINE-P is non-nil then do not request the telegram-server."
  (let ((chat (gethash chat-id telega--chats)))
    (when (and (not chat) (not offline-p))
      (setq chat (telega-server--call
                  (list :@type "getChat"
                        :chat_id chat-id)))
      (cl-assert chat nil "getChat timed out chat_id=%d" chat-id)
      (telega-chat--ensure chat))
    chat))

(defsubst telega-chats-list-get (tl-obj-Chats)
  "Return chats list of TL-OBJ-CHATS represeting `Chats' object."
  (mapcar #'telega-chat-get (plist-get tl-obj-Chats :chat_ids)))

(defun telega-chat-by-username (username)
  "Find chat by its USERNAME."
  (cl-find username telega--ordered-chats
           :test 'string=
           :key (lambda (chat)
                  (plist-get (telega-chat--info chat) :username))))

(defun telega--joinChatByInviteLink (invite-link)
  "Return new chat by its INVITE-LINK.
Return nil if can't join the chat."
  (telega-server--call
   (list :@type "joinChatByInviteLink"
         :invite_link invite-link)))

(defun telega--checkChatInviteLink (invite-link)
  "Check invitation link INVITE-LINK."
  (telega-server--call
   (list :@type "checkChatInviteLink"
         :invite_link invite-link)))

(defun telega--joinChat (chat)
  "Adds a new member to a CHAT."
  (telega-server--send
   (list :@type "joinChat" :chat_id (plist-get chat :id))))

(defun telega--leaveChat (chat)
  "Removes current user from CHAT members."
  (telega-server--send
   (list :@type "leaveChat" :chat_id (plist-get chat :id))))

(defun telega--deleteChatHistory (chat &optional remove-from-list)
  "Deletes all messages in the CHAT only for the user.
Cannot be used in channels and public supergroups."
  (telega-server--send
   (list :@type "deleteChatHistory"
         :chat_id (plist-get chat :id)
         :remove_from_chat_list (or remove-from-list :false))))

(defun telega--setChatTitle (chat title)
  "Changes the CHAT title to TITLE."
  (telega-server--send
   (list :@type "setChatTitle"
         :chat_id (plist-get chat :id)
         :title title)))

(defun telega-chat--info (chat)
  "Return info structure for the CHAT.
It could be user, secretChat, basicGroup or supergroup."
  (let ((chat-type (plist-get chat :type)))
    (cl-ecase (telega--tl-type chat-type)
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

(defun telega-chat-me ()
  "Chat with myself, a.k.a Saved Messages."
  ;; NOTE: Saved Messages has same id as me user
  (telega-chat-get telega--me-id 'offline))

(defun telega-chat--type (chat &optional no-interpret)
  "Return type of the CHAT.
Types are: `private', `secret', `bot', `basicgroup', `supergroup' or `channel'.
If NO-INTERPRET is specified, then return only `private',
`secret', `basicgroup' and `supergroup' without interpretation
them to bots or channels."
  (let* ((chat-type (plist-get chat :type))
         (type-sym (intern (downcase (substring (plist-get chat-type :@type) 8)))))
    (cond ((and (not no-interpret)
                (eq type-sym 'supergroup)
                (plist-get chat-type :is_channel))
           'channel)
          ((and (not no-interpret)
                (eq type-sym 'private)
                (telega-user--bot-p (telega-chat--user chat)))
           'bot)
          (t type-sym))))

(defsubst telega-chat-username (chat)
  "Return CHAT's username.
Return nil if no username is assigned to CHAT."
  (let ((username (plist-get (telega-chat--info chat) :username)))
    (unless (or (null username) (string-empty-p username))
      username)))

(defun telega-chat--secret-p (chat)
  "Return non-nil if CHAT is secret."
  (eq (telega-chat--type chat 'no-interpret) 'secret))

(defun telega-chat--public-p (chat &optional chat-type)
  "Return non-nil if CHAT is public.
Public chats are only chats with non-empty username.
CHAT-TYPE is either `private', `supergroup' or `any'.
`supergroup' type also includes channels.
By default CHAT-TYPE is `any'."
  (and (or (eq (or chat-type 'any) 'any)
           (eq (telega-chat--type chat 'no-interpret) chat-type))
       (telega-chat-username chat)))

(defsubst telega-chat--muted-p (chat)
  "Return non-nil if CHAT is muted."
  (> (telega-chat-notification-setting chat :mute_for) 0))

(defun telega-chat-title (chat &optional with-username)
  "Return title for the CHAT.
If WITH-USERNAME is specified, append trailing username for this chat."
  (let ((title (plist-get chat :title)))
    (when (string-empty-p title)
      (setq title (cl-ecase (telega-chat--type chat)
                    (private
                     (telega-user--name (telega-chat--user chat) 'name)))))
    (when (and (eq chat (telega-chat-me)) telega-chat-me-custom-title)
      (setq title telega-chat-me-custom-title))
    (when with-username
      (let ((username (telega-chat-username chat)))
        (when username
          (setq title (concat title " @" username)))))
    title))

(defun telega-chat--reorder (chat order)
  ;; NOTE: order=nil if reordering with custom ORDER
  (when order
    (plist-put chat :order order))
  ;; Reorder CHAT by removing and then adding it again at correct place
  (setq telega--ordered-chats (delq chat telega--ordered-chats))
  (telega--ordered-chats-insert chat)
  (telega-root--chat-reorder chat))

(defun telega--on-updateNewChat (event)
  "New chat has been loaded or created."
  (let ((chat (plist-get event :chat)))
    (telega-chat--ensure chat)
    (telega-root--chat-new chat)))

(defun telega--setChatNotificationSettings (chat &rest settings)
  "Set CHAT's notification settings to NOT-CFG."
  (declare (indent 1))
  (let ((not-cfg (plist-get chat :notification_settings))
        (request (list :@type "chatNotificationSettings")))
    (cl-loop for (prop-name value) on (append not-cfg settings)
             by 'cddr
             do (setq request (plist-put request prop-name (or value :false))))
    (telega-server--call
     (list :@type "setChatNotificationSettings"
           :chat_id (plist-get chat :id)
           :notification_settings request))))

(defun telega--on-updateChatNotificationSettings (event)
  "Notification settings has been changed in chat."
  (let ((chat (telega-chat-get (plist-get event :chat_id))))
    (plist-put chat :notification_settings
               (plist-get event :notification_settings))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatTitle (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline))
        (new-title (plist-get event :title)))
    (cl-assert chat)
    (with-telega-chatbuf chat
      (rename-buffer (telega-chatbuf--name chat new-title)))

    (plist-put chat :title new-title)
    (telega-root--chat-update chat)))

(defun telega--on-updateChatOrder (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (telega-chat--reorder chat (plist-get event :order))
    ;; NOTE: reorder might affect `telega--filtered-chats' and custom
    ;; filters, so update the chat
    (telega-root--chat-update chat 'for-reorder)))

(defun telega--on-updateChatIsPinned (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :is_pinned (plist-get event :is_pinned))
    (telega-chat--reorder chat (plist-get event :order))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatReadInbox (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline))
        (unread-count (plist-get event :unread_count)))
    (cl-assert chat)
    (plist-put chat :last_read_inbox_message_id
               (plist-get event :last_read_inbox_message_id))
    (plist-put chat :unread_count unread-count)

    ;; NOTE: if all messages are read (in another telegram client) and
    ;; tracking is enabled, then remove the buffer from tracking
    (when (and telega-use-tracking (zerop unread-count))
      (with-telega-chatbuf chat
        (tracking-remove-buffer (current-buffer))))

    (telega-root--chat-update chat)))

(defun telega--on-updateChatReadOutbox (event)
  (let* ((chat (telega-chat-get (plist-get event :chat_id) 'offline))
         (old-read-outbox-msgid (plist-get chat :last_read_outbox_message_id)))
    (cl-assert chat)
    (plist-put chat :last_read_outbox_message_id
               (plist-get event :last_read_outbox_message_id))
    (with-telega-chatbuf chat
      (telega-chatbuf--read-outbox old-read-outbox-msgid))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatUnreadMentionCount (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :unread_mention_count
               (plist-get event :unread_mention_count))
    (telega-root--chat-update chat)))

(defun telega--on-updateMessageMentionRead (event)
  (telega--on-updateChatUnreadMentionCount event)
  ;; TODO: might be workout with message of `:message_id' as well
  )

(defun telega--on-updateChatReplyMarkup (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :reply_markup_message_id
               (plist-get event :reply_markup_message_id))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatLastMessage (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :last_message
               (plist-get event :last_message))
    (unless (string= (plist-get event :order) (plist-get chat :order))
      (telega-chat--reorder chat (plist-get event :order)))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatDraftMessage (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline))
        (draft-msg (plist-get event :draft_message)))
    (cl-assert chat)
    (plist-put chat :draft_message draft-msg)
    (unless (string= (plist-get event :order) (plist-get chat :order))
      (telega-chat--reorder chat (plist-get event :order)))
    (telega-root--chat-update chat)

    ;; If chat has no active input, then update it to the draft
    (with-telega-chatbuf chat
      (unless (telega-chatbuf-has-input-p)
        (let ((reply-msg-id (plist-get draft-msg :reply_to_message_id)))
          (when (and reply-msg-id (not (zerop reply-msg-id)))
            (telega-msg-reply
             (telega-msg--get (plist-get chat :id) reply-msg-id)))

          (save-excursion
            (goto-char telega-chatbuf--input-marker)
            (telega-ins--text
             (telega--tl-get draft-msg :input_message_text :text))))))
    ))

(defun telega--on-updateChatIsMarkedAsUnread (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :is_marked_as_unread
               (plist-get event :is_marked_as_unread))
    (telega-root--chat-update chat)))

(defun telega-chat--on-getChats (result)
  "Ensure chats from RESULT exists, and continue fetching chats."
  (let ((chat-ids (plist-get result :chat_ids)))
    (telega-debug "on-getChats: %s" chat-ids)
    (mapc #'telega-chat--ensure (mapcar #'telega-chat-get chat-ids))

    (if (> (length chat-ids) 0)
        ;; Redisplay the root's custom filters and then
        ;; Continue fetching chats
        (let ((telega-filters--inhibit-redisplay nil))
          (telega-filters--redisplay)
          (telega--getChats))

      ;; All chats has been fetched

      ;; TODO: some chats remains with order="0", i.e. known chats, do
      ;; not proceed with chats that once was used, such as basic
      ;; groups upgraded to supergroups, closed secret chats, etc.  We
      ;; might want to remove them from `telega--ordered-chats' list
      ;; for faster processing, but keep it in chats hash
      (setq telega-filters--inhibit-redisplay nil)
      (telega-filters--redisplay)
      (telega-status--set nil "")       ;reset aux status

      (run-hooks 'telega-chats-fetched-hook))))

(defun telega--getChats ()
  "Retreive all chats from the server in async manner."
  ;; Do not update filters on every chat fetched, update them at the end
  (setq telega-filters--inhibit-redisplay t)

  (let* ((last-chat (car (last telega--ordered-chats)))
         (offset-order (or (plist-get last-chat :order) "9223372036854775807"))
         (offset-chatid (or (plist-get last-chat :id) 0)))
    (telega-server--call
     (list :@type "getChats"
           :offset_order offset-order
           :offset_chat_id offset-chatid
           :limit 1000)
     #'telega-chat--on-getChats)))

(defun telega--getChatPinnedMessage (chat)
  "Get pinned message for the CHAT, if any."
  (when (and (eq (telega-chat--type chat 'raw) 'supergroup)
             (not (zerop (plist-get
                          (telega--full-info (telega-chat--supergroup chat))
                          :pinned_message_id))))
    (telega-server--call
     (list :@type "getChatPinnedMessage"
           :chat_id (plist-get chat :id)))))

(defun telega--getCreatedPublicChats ()
  "Return list of public chats created by the user."
  (telega-chats-list-get
   (telega-server--call
    (list :@type "getCreatedPublicChats"))))

(defun telega-chats--kill-em-all ()
  "Kill all chat buffers."
  (dolist (cbuf telega--chat-buffers)
    (kill-buffer cbuf)))

(defun telega-chats-top (category)
  "Return list of top chats used by CATEGORY.
CATEGORY is one of `Users', `Bots', `Groups', `Channels',
`InlineBots', `Calls'"
  (let ((top (assq category telega--top-chats))
        (currts (time-to-seconds (current-time))))
    (when (> currts (+ (or (cadr top) 0) 60))
      ;; XXX update only if last fetch is older then 60 seconds
      (let* ((cattype (list :@type (concat "topChatCategory"
                                           (symbol-name category))))
             (cl (telega-server--call
                  (list :@type "getTopChats"
                        :category cattype
                        :limit 30))))
        (setq top (list category (time-to-seconds (current-time))
                        (telega-chats-list-get cl)))
        (setf (alist-get category telega--top-chats) top)))
    (caddr top)))

(defun telega--sendChatAction (chat action)
  "Send ACTION on CHAT."
  (cl-assert (member action '("Typing" "RecordingVideo" "UploadingVideo"
                              "RecordingVoiceNote" "UploadingVoiceNote"
                              "UploadingPhoto" "UploadingDocument"
                              "ChoosingLocation" "ChoosingContact"
                              "StartPlayingGame" "RecordingVideoNote"
                              "UploadingVideoNote" "Cancel")))
  (telega-server--send
   (list :@type "sendChatAction"
         :chat_id (plist-get chat :id)
         :action (list :@type (concat "chatAction" action)))))

(defun telega--createPrivateChat (user)
  "Create private chat with USER.
Return newly created chat."
  (telega-chat-get
   (plist-get
    (telega-server--call
     (list :@type "createPrivateChat"
           :user_id (plist-get user :id))) :id)))

(defun telega--createNewSecretChat (user)
  "Create secret chat with USER.
Return newly created chat."
  (telega-chat-get
   (plist-get
    (telega-server--call
     (list :@type "createNewSecretChat"
           :user_id (plist-get user :id))) :id)))

(defun telega--closeSecretChat (secretchat)
  "Close SECRETCHAT."
  (telega-server--call
   (list :@type "closeSecretChat"
         :secret_chat_id (plist-get secretchat :id))))

(defun telega--viewMessages (chat messages &optional force)
  "Mark CHAT's MESSAGES as read.
Use non-nil value for FORCE, if messages in closed chats should
be marked as read."
  (telega-server--send
   (list :@type "viewMessages"
         :chat_id (plist-get chat :id)
         :message_ids (cl-map 'vector (telega--tl-prop :id) messages)
         :force_read (or force :false))))

(defun telega--toggleChatIsPinned (chat)
  "Toggle pin state of the CHAT."
  (telega-server--send
   (list :@type "toggleChatIsPinned"
         :chat_id (plist-get chat :id)
         :is_pinned (if (plist-get chat :is_pinned) :false t))))

(defun telega--toggleChatIsMarkedAsUnread (chat)
  "Toggle marked as read state of the CHAT."
  (telega-server--send
   (list :@type "toggleChatIsMarkedAsUnread"
         :chat_id (plist-get chat :id)
         :is_marked_as_unread
         (if (plist-get chat :is_marked_as_unread) :false t))))

(defun telega--readAllChatMentions (chat)
  "Read all mentions in CHAT."
  (telega-server--send
   (list :@type "readAllChatMentions" :chat_id (plist-get chat :id))))

(defun telega--openChat (chat)
  "Mark CHAT as opened."
  (telega-server--send
   (list :@type "openChat"
         :chat_id (plist-get chat :id))))

(defun telega--closeChat (chat)
  "Mark CHAT as closed."
  (telega-server--send
   (list :@type "closeChat"
         :chat_id (plist-get chat :id))))

(defun telega--searchPublicChat (username)
  "Search public chat with USERNAME."
  (telega-server--call
   (list :@type "searchPublicChat"
         :username username)))

(defun telega--searchPublicChats (query &optional callback)
  "Search public chats by looking for specified QUERY.
Return nil if QUERY is less then 5 chars.
If CALLBACK is specified, then do async call and run CALLBACK
with list of chats received."
  (unless (< (length query) 5)
    (let ((ret (telega-server--call
                (list :@type "searchPublicChats"
                      :query query)
                (and callback
                     `(lambda (reply)
                        (funcall ',callback (telega-chats-list-get reply)))))))
      (if callback
          ret
        (telega-chats-list-get ret)))))

(defun telega--searchChats (query &optional limit)
  "Search already known chats by QUERY."
  (telega-chats-list-get
   (telega-server--call
    (list :@type "searchChats"
          :query query
          :limit (or limit 200)))))

(defun telega--searchChatsOnServer (query &optional limit)
  "Search already known chats on server by QUERY."
  (telega-server--call
   (list :@type "searchChatsOnServer"
         :query query
         :limit (or limit 200))))


;;; Chat buttons in root buffer
(defvar telega-chat-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "i") 'telega-describe-chat)
    (define-key map (kbd "h") 'telega-describe-chat)
    (define-key map (kbd "a") 'telega-chat-add-member)
    (define-key map (kbd "o") 'telega-chat-custom-order)
    (define-key map (kbd "r") 'telega-chat-toggle-read)
    (define-key map (kbd "d") 'telega-chat-delete)
    (define-key map (kbd "C-c p") 'telega-chat-pin)
    (define-key map (kbd "P") 'telega-chat-pin)
    (define-key map (kbd "C") 'telega-chat-call)
    (define-key map (kbd "DEL") 'telega-chat-delete)
    map)
  "The key map for telega chat buttons.")

(define-button-type 'telega-chat
  :supertype 'telega
  :inserter telega-inserter-for-chat-button
  'keymap telega-chat-button-map
  'action #'telega-chat-button--action)

(defun telega-chat-button--action (button)
  "Action to take when chat BUTTON is pressed."
  (telega-chat--pop-to-buffer (button-get button :value)))

(defsubst telega-chat--pp (chat)
  "Pretty printer for CHAT button."
  (telega-button--insert 'telega-chat chat)
  (insert "\n"))

(defun telega-chat-known--pp (chat)
  "Pretty printer for known CHAT button."
  ;; Insert only visible chat buttons
  ;; See https://github.com/zevlg/telega.el/issues/3
  (let ((visible-p (and (telega-filter-chats nil (list chat))
                        (if telega-search-query
                            (memq chat telega--search-chats)
                          t))))
    (when visible-p
      (telega-chat--pp chat))))

(defun telega-chat-global--pp (chat)
  "Display CHAT found in global public chats search."
  (let* ((telega-chat-button-width (+ telega-chat-button-width
                                     (/ telega-chat-button-width 2)))
         (telega-filters--inhibit-list '(has-order))
         (visible-p (telega-filter-chats nil (list chat))))
    (when visible-p
      (telega-chat--pp chat))))

(defun telega-chat--pop-to-buffer (chat)
  "Pop to CHAT's buffer."
  (pop-to-buffer (telega-chatbuf--get-create chat)
                 telega-chat--display-buffer-action))

(defun telega-chat-at-point ()
  "Return current chat at point."
  (let ((button (button-at (point))))
    (when (and button (eq (button-type button) 'telega-chat))
      (button-get button :value))))

(defun telega-chat-pin (chat)
  "Toggle chat's pin state at point."
  (interactive (list (telega-chat-at-point)))
  (telega--toggleChatIsPinned chat))

(defun telega--addChatMembers (chat users)
  "Add new members to the CHAT.
CHAT must be supergroup or channel."
  (telega-server--send
   (list :@type "addChatMembers"
         :chat_id (plist-get chat :id)
         :user_ids (cl-map 'vector (telega--tl-prop :id) users))))

(defun telega-chat-add-member (chat user &optional forward-limit)
  "Add MEMBER to the CHAT."
  (interactive (list (or telega-chatbuf--chat
                         (telega-chat-at-point))
                     (telega-completing-read-user "User: ")))
  (telega-server--send
   (list :@type "addChatMember"
         :chat_id (plist-get chat :id)
         :user_id (plist-get user :id)
         :forward_limit (or forward-limit 300))))

(defun telega-chat-custom-order (chat order)
  "For the CHAT (un)set custom ORDER."
  (interactive (let ((chat (telega-chat-at-point)))
                 (list chat
                       (read-string "Custom Order [empty to unset]: "
                                    (number-to-string
                                     (telega-chat--order chat))))))
  (if (string-empty-p order)
      (setq order nil)

    (unless (numberp (read order))
      (error "Invalid order, must contain only digits")))

  (setf (telega-chat-uaprop chat :order) order)
  (telega-chat--reorder chat nil)
  (telega-root--chat-update chat))

(defun telega--setChatMemberStatus (chat user status)
  "Change the STATUS of a CHAT USER, needs appropriate privileges.
STATUS is one of: "
  (telega-server--send
   (list :@type "setChatMemberStatus"
         :chat_id (plist-get chat :id)
         :user_id (plist-get user :id)
         :status status)))

(defun telega-chat-call (chat)
  "Call to the user associated with the given private CHAT."
  (interactive (list (telega-chat-at-point)))

  ;; NOTE: If calling to secret chat, then use ordinary private chat
  ;; for calling
  (when (telega-chat--secret-p chat)
    (setq chat (telega-chat-get
                (plist-get (telega-chat--info chat) :user_id))))

  (unless (eq (telega-chat--type chat 'no-interpret) 'private)
    (error "Can call only to users"))
  (let* ((user (telega-chat--user chat))
         (full-info (telega--full-info user)))
    (when (plist-get full-info :has_private_calls)
      (error "%s can't be called due to their privacy settings"
             (telega-user--name user)))
    (unless (plist-get full-info :can_be_called)
      (error "%s can't be called" (telega-user--name user)))

    (telega-voip-call user)))

(defun telega-chat-share-my-contact (chat)
  "Share my contact info with CHAT."
  (message "TODO: `telega-chat-share-contact'."))

(defun telega-describe-chat (chat)
  "Show info about chat at point."
  (interactive (list (telega-chat-at-point)))
  (with-telega-help-win "*Telegram Chat Info*"
    ;; We use buffer local `telega-chatbuf--chat' to keep track on
    ;; chat
    (setq telega-chatbuf--chat chat)

    (telega-ins (capitalize (symbol-name (telega-chat--type chat))) ": ")
    (telega-ins--with-face
        (list :foreground (if (eq (frame-parameter nil 'background-mode) 'light)
                              (nth 2 (telega-chat-uaprop chat :color))
                            (nth 0 (telega-chat-uaprop chat :color))))
      (telega-ins (telega-chat-title chat 'with-username)))
    (telega-ins " ")
    (telega-ins--button "Open"
      :value chat
      :action 'telega-chat--pop-to-buffer)
    (telega-ins "\n")
    (telega-ins-fmt "Id: %d\n" (plist-get chat :id))
    (when (telega-chat--public-p chat)
      (let ((link (concat (or (plist-get telega--options :t_me_url)
                              "https://t.me/")
                          (plist-get (telega-chat--supergroup chat) :username))))
        (insert "Link: ")
        (apply 'insert-text-button link (telega-link-props 'url link 'link))
        (insert "\n")))
    (when telega-debug
      (telega-ins-fmt "Order: %S\n" (telega-chat--order chat)))

    (let ((default-mute-for
            (telega-chat-notification-setting chat :mute_for 'default))
          (default-show-preview
            (telega-chat-notification-setting chat :show_preview 'default))
          (mute-for (telega-chat-notification-setting chat :mute_for))
          (show-preview (telega-chat-notification-setting chat :show_preview)))
      (telega-ins-fmt "Notifications: ")
      (let* ((unmuted-p (zerop mute-for))
             (change-args (if unmuted-p
                              (cons :false 599634793)
                            (cons t 0))))
        (telega-ins--button (if unmuted-p
                                telega-symbol-heavy-checkmark
                              "  ")
          :value chat
          :action `(lambda (chat)
                     (telega--setChatNotificationSettings chat
                       :use_default_mute_for ,(car change-args)
                       :mute_for ,(cdr change-args))
                     (telega-save-cursor
                       (telega-describe-chat chat))))
        (when unmuted-p
          (telega-ins ", Preview: ")
          (telega-ins--with-face 'telega-box
            (telega-ins (if show-preview
                            telega-symbol-heavy-checkmark
                          "  "))))
        (telega-ins "\n")))

    (insert "\n")
    (telega-info--insert (plist-get chat :type) chat)

    (when telega-debug
      (telega-ins "\n---DEBUG---\n")
      (telega-ins-fmt "Chat: %S\n" chat)
      (telega-ins-fmt "Info: %S" (telega-chat--info chat)))
    ))

(defun telega-chat-with (name)
  "Start chatting with peer matching NAME."
  (interactive
   (let ((completion-ignore-case t))
     (list (funcall telega-completing-read-function
                    "Chat with: " (telega-completing-titles) nil t))))

  (let ((chat (cl-find name telega--ordered-chats
                       :test (lambda (needname chat)
                               (string= (telega-chat-title chat 'with-username)
                                        needname)))))
    (unless chat
      (let ((user (cl-find name (hash-table-values (cdr (assq 'user telega--info)))
                           :test (lambda (needname user)
                                   (string= (telega-user--name user) needname)))))
        (setq chat (telega--createPrivateChat user))))

    (telega-chat--pop-to-buffer chat)))

(defun telega-chat-join-by-link (link)
  "Join chat by invitation LINK."
  (interactive "sInvite link: ")
  (telega-chat--pop-to-buffer
   (or (telega--joinChatByInviteLink link)
       (error "Can't join chat: %s"
              (plist-get telega-server--last-error :message)))))

(defun telega-chat-toggle-read (chat)
  "Toggle chat as read/unread."
  (interactive (list (telega-chat-at-point)))
  (let ((unread-count (plist-get chat :unread_count))
        (marked-unread-p (plist-get chat :is_marked_as_unread)))
    (if (or (> unread-count 0) marked-unread-p)
        (progn
          ;; Toggle chat as readed
          (when marked-unread-p
            (telega--toggleChatIsMarkedAsUnread chat))
          (when (> unread-count 0)
            (telega--viewMessages
             chat (list (plist-get chat :last_message)) t))
          ;; NOTE: reading messages can change mentions count, so
          ;; force all mentions are read
          (telega--readAllChatMentions chat))

      ;; Toggle chat is unread
      (unless marked-unread-p
        (telega--toggleChatIsMarkedAsUnread chat))
      )))

(defun telega-chats-filtered-toggle-read (&optional force)
  "Apply `telega-chat-toggle-read' to all currently filtered chats."
  (interactive
   (list (y-or-n-p (format "Toggle read for %d chats? "
                           (length telega--filtered-chats)))))
  (mapc 'telega-chat-toggle-read telega--filtered-chats))

(defun telega-chat-delete (chat)
  "Delete CHAT."
  (interactive (list (and (y-or-n-p "This action cannot be undone. Delete chat? ")
                          (telega-chat-at-point))))

  (when chat
    (let ((chat-type (telega-chat--type chat)))
      (cond ((eq chat-type 'secret)
             (telega--closeSecretChat (telega-chat--info chat)))
            ((not (eq chat-type 'private))
             (telega--leaveChat chat)))

      ;; NOTE: `telega--deleteChatHistory' Cannot be used in channels
      ;; and public supergroups
      (unless (or (eq (telega-chat--type chat) 'channel)
                  (telega-chat--public-p chat 'supergroup))
        (telega--deleteChatHistory chat t)))

    ;; Kill corresponding chat buffer
    (with-telega-chatbuf chat
      (kill-buffer (current-buffer)))))

(defun telega-chats-filtered-delete (&optional force)
  "Apply `telega-chat-delete' to all currently filtered chats.
Do it only if FORCE is non-nil."
  (interactive
   (list (y-or-n-p (format "This action cannot be undone. Delete %d chats? "
                           (length telega--filtered-chats)))))
  (when force
    (mapc 'telega-chat-delete telega--filtered-chats)))


;;; Chat Buffer
(defgroup telega-chat nil
  "Customization for telega-chat-mode"
  :prefix "telega-chat-"
  :group 'telega)

(defvar telega-chat-mode-hook nil
  "Hook run when telega chat buffer is created.")

(defvar telega-chat-mode-map
  (let ((map (make-sparse-keymap)))
    ;; C-M-[ - cancels edit/reply
    (define-key map (kbd "\e\e") 'telega-chatbuf-cancel-aux)
    (define-key map (kbd "C-M-c") 'telega-chatbuf-cancel-aux)

    (define-key map (kbd "C-c C-a") 'telega-chatbuf-attach)
    (define-key map (kbd "C-c C-f") 'telega-chatbuf-attach-file)
    (define-key map (kbd "C-c C-v") 'telega-chatbuf-attach-clipboard)
    (define-key map (kbd "C-c ?") 'telega-describe-chatbuf)

    (define-key map (kbd "RET") 'telega-chatbuf-input-send)
    (define-key map (kbd "M-p") 'telega-chatbuf-input-prev)
    (define-key map (kbd "M-n") 'telega-chatbuf-input-next)
    (define-key map (kbd "M-r") 'telega-chatbuf-input-search)

    ;; jumping around links
    (define-key map (kbd "TAB") 'telega-chat-complete-or-next-link)
    (define-key map (kbd "<backtab>") 'telega-chat-prev-link)
    map))

(defvar telega-chatbuf--chat nil
  "Telega chat for the current chat buffer.")
(make-variable-buffer-local 'telega-chatbuf--chat)

(defvar telega-chatbuf--ewoc nil
  "Ewoc for for the current chat buffer.")
(make-variable-buffer-local 'telega-chatbuf--ewoc)

(defvar telega-chatbuf--messages nil
  "Local cache for the messages.")
(make-variable-buffer-local 'telega-chatbuf--messages)

(defvar telega-chatbuf--input-ring nil
  "The chat input history ring.")
(make-variable-buffer-local 'telega-chatbuf--input-ring)

(defvar telega-chatbuf--input-idx nil
  "The index to the current item in the chat input ring.")
(make-variable-buffer-local 'telega-chatbuf--input-idx)

(defvar telega-chatbuf--input-pending-p nil
  "Non-nil if last input is not yet commited.")
(make-variable-buffer-local 'telega-chatbuf--input-pending-p)

(defvar telega-chatbuf--input-marker nil)
(make-variable-buffer-local 'telega-chatbuf--input-marker)

(defvar telega-chatbuf--isearch-input nil)
(make-variable-buffer-local 'telega-chatbuf--isearch-input)

(defvar telega-chatbuf--output-marker nil)
(make-variable-buffer-local 'telega-chatbuf--output-marker)

(defvar telega-chatbuf--aux-button nil
  "Button that display reply/edit/fwd message above input.")
(make-variable-buffer-local 'telega-chatbuf--aux-button)

(defvar telega-chatbuf--prompt-button nil "Input prompt button.")
(make-variable-buffer-local 'telega-chatbuf--prompt-button)

(defvar telega-chatbuf--history-loading nil
  "Non-nil if history has been requested.
Actual value is `:@extra` value of the call to load history.")
(make-variable-buffer-local 'telega-chatbuf--history-loading)

(define-button-type 'telega-prompt
  :supertype 'telega
  :inserter 'telega-ins
  'face 'telega-chat-prompt
  'rear-nonsticky t
  'front-sticky nil
  'read-only t
;  'cursor-intangible t
  'field 'telega-prompt)

(define-button-type 'telega-prompt-aux
  :supertype 'telega
  :inserter 'telega-ins
  'read-only t
  'cursor-intangible t
  'field 'telega-prompt)

(defun telega-chatbuf--footer ()
  "Generate string to be used as ewoc's footer."
  (let ((actions (gethash (plist-get telega-chatbuf--chat :id)
                          telega--actions)))
    (telega-ins--as-string
     (telega-ins--with-props '(read-only t rear-nonsticky t front-sticky nil)
       (telega-ins telega-symbol-underline-bar)
       (telega-ins--with-attrs (list :min (- telega-chat-fill-column 2)
                                     :max (- telega-chat-fill-column 2)
                                     :align 'left
                                     :align-symbol telega-symbol-underline-bar
                                     :elide t
                                     :elide-trail (/ telega-chat-fill-column 2))
         (when actions
           (telega-ins "(")
           (telega-ins--actions actions)
           (telega-ins ")")))
       (telega-ins telega-symbol-underline-bar)
       (telega-ins "\n")))))

(define-derived-mode telega-chat-mode nil "Telega-Chat"
  "The mode for telega chat buffer.
Keymap:
\\{telega-chat-mode-map}"
  :group 'telega-chat
  (setq telega-chatbuf--messages (make-hash-table :test 'eq)
        telega-chatbuf--input-ring (make-ring telega-chat-input-ring-size)
        telega-chatbuf--input-idx nil
        telega-chatbuf--input-pending-p nil
        telega-chatbuf--history-loading nil)

  (erase-buffer)
  (setq-local window-point-insertion-type t)
  (setq-local next-line-add-newlines nil)
  (cursor-sensor-mode 1)
  (cursor-intangible-mode 1)

  (setq telega-chatbuf--ewoc
        (ewoc-create 'telega-msg--pp nil
                     (telega-chatbuf--footer) t))
  (goto-char (point-max))

  (setq telega-chatbuf--aux-button
        (telega-button--insert 'telega-prompt-aux "no-value"
          'invisible t))
  (setq telega-chatbuf--prompt-button
        (telega-button--insert 'telega-prompt telega-chat-input-prompt))

  (setq telega-chatbuf--input-marker (point-marker))

  (add-hook 'window-scroll-functions 'telega-chatbuf-scroll nil t)
  (add-hook 'post-command-hook 'telega-chatbuf--post-command nil t)
  (add-hook 'isearch-mode-hook 'telega-chatbuf--input-isearch-setup nil t)
  (add-hook 'kill-buffer-hook 'telega-chatbuf--killed nil t)

  (setq telega--chat-buffers
        (pushnew (current-buffer) telega--chat-buffers)))

(defun telega-describe-chatbuf ()
  "Show info about chat."
  (interactive)
  (telega-describe-chat telega-chatbuf--chat))

(defun telega-chatbuf--killed ()
  "Called when chat buffer is killed."
  (ignore-errors
    ;; See https://github.com/zevlg/telega.el/issues/12
    (telega--closeChat telega-chatbuf--chat))

  (setq telega--chat-buffers
        (delq (current-buffer) telega--chat-buffers))

  ;; Closing chat may affect filtering, see `opened' filter
  (telega-root--chat-update telega-chatbuf--chat))

(defun telega-chatbuf--input-isearch-setup ()
  "Setup chat buffer to use isearch to search in chat input history."
  (when telega-chatbuf--isearch-input
    ;; TODO: setup isearch, see. `comint-history-isearch-setup'
    )
  )

(defun telega-chatbuf-scroll (window display-start)
  "If at the beginning then request for history messages.
Also mark messages as read with `viewMessages'."
  (with-current-buffer (window-buffer window)
    ;; If at the beginning of chatbuffer then request for the history
    (when (= display-start 1)
      (telega-chat--load-history telega-chatbuf--chat))

    ;; Mark some messages as read
    (unless (zerop (plist-get telega-chatbuf--chat :unread_count))
      (save-excursion
        (goto-char display-start)
        (telega--viewMessages
         telega-chatbuf--chat
         (telega-chatbuf--visible-messages window))))
    ))

(defsubst telega-chat--my-action (chat)
  "Return my current action in chatbuffer."
  (assq telega--me-id (gethash (plist-get chat :id) telega--actions)))

(defsubst telega-chatbuf-has-input-p ()
  "Return non-nil if chatbuf has some input."
  (< telega-chatbuf--input-marker (point-max)))

(defsubst telega-chatbuf-input-string ()
  "Return non-nil if chatbuf has some input."
  (buffer-substring telega-chatbuf--input-marker (point-max)))

(defun telega-chatbuf--editing-msg ()
  "Return message currently editing."
  (and (eq (button-get telega-chatbuf--aux-button :inserter)
           'telega-ins--aux-edit-inline)
       (button-get telega-chatbuf--aux-button :value)))

(defun telega-chatbuf--forwarding-msg ()
  "Return message currently forwarding."
  (and (eq (button-get telega-chatbuf--aux-button :inserter)
           'telega-ins--aux-fwd-inline)
       (button-get telega-chatbuf--aux-button :value)))

(defun telega-chatbuf--replying-msg ()
  "Return message currently editing."
  (and (eq (button-get telega-chatbuf--aux-button :inserter)
           'telega-ins--aux-reply-inline)
       (button-get telega-chatbuf--aux-button :value)))

(defun telega-chatbuf--post-command ()
  "Chabuf `post-command-hook' funcion."
  ;; - Check that all atachements are valid (starting/ending chars are
  ;;   ok) and remove invalid attachements
  (let ((attach (telega--region-by-text-prop
                 telega-chatbuf--input-marker 'telega-attach)))
    (while attach
      (if (and (get-text-property (car attach) 'attach-open-bracket)
               (get-text-property (1- (cdr attach)) 'attach-close-bracket))
          ;; Valid
          (setq attach (telega--region-by-text-prop
                        (cdr attach) 'telega-attach))

        ;; Invalid attachement, remove it from input
        (delete-region (car attach) (cdr attach))
        (setq attach (telega--region-by-text-prop
                      (car attach) 'telega-attach)))))

  ;; - If point moves inside prompt, move it at the beginning of
  ;;   input.  However inhibit this behaviour in case main prompt is
  ;;   invisible, prompt is invisible if we are not member of the
  ;;   group and [JOIN] button is shown
  (when (and (not (button-get telega-chatbuf--prompt-button 'invisible))
             (>= (point) telega-chatbuf--prompt-button)
             (< (point) telega-chatbuf--input-marker))
    (goto-char telega-chatbuf--input-marker))

  ;; - Finally, when input is probably changed by above operations,
  ;;   update chat's action after command execution.
  (let ((input-p (telega-chatbuf-has-input-p))
        (my-action (telega-chat--my-action telega-chatbuf--chat)))
    (cond ((and (not my-action) input-p)
           (telega--sendChatAction telega-chatbuf--chat "Typing"))
          ((and my-action (not input-p))
           (telega--sendChatAction telega-chatbuf--chat "Cancel")))

    ;; - If there is active draft_message and input is empty then
    ;;   clear the draf
    (when (and (plist-get telega-chatbuf--chat :draft_message)
               (not input-p))
      (telega--setChatDraftMessage telega-chatbuf--chat)))
  )

(defmacro with-telega-chatbuf (chat &rest body)
  "Execute BODY setting current buffer to chat buffer of CHAT.
Executes BODY only if chat buffer already exists.
If there is no corresponding buffer, then do nothing.
Inhibits read-only flag."
  (declare (indent 1))
  (let ((bufsym (cl-gensym)))
    `(let ((,bufsym (cl-find ,chat telega--chat-buffers
                             :test (lambda (val buf)
                                     (with-current-buffer buf
                                       (eq telega-chatbuf--chat val))))))
       (when (buffer-live-p ,bufsym)
         (with-current-buffer ,bufsym
           (let ((inhibit-read-only t))
             ,@body))))))

(defmacro with-telega-chatbuf-action (action &rest body)
  "Execute BODY setting current action to ACTION.
Recover previous active action after BODY execution."
  (declare (indent 1))
  (let ((actsym (gensym "action")))
    `(let ((,actsym (caddr (telega-chat--my-action telega-chatbuf--chat))))
       (telega--sendChatAction telega-chatbuf--chat ,action)
       (unwind-protect
           (progn ,@body)
         (telega--sendChatAction telega-chatbuf--chat
                                 (or (and ,actsym (substring ,actsym 10))
                                     "Cancel"))))))

(defun telega-chatbuf--name (chat &optional title)
  "Return name for the CHAT buffer.
If TITLE is specified, use it instead of chat's title."
  (format "Telega-%S%s: %s" (telega-chat--type chat)
          (telega--desurrogate-apply
           (let ((un (plist-get (telega-chat--info chat) :username)))
            (concat (if (string-empty-p un) "" "@") un)))
          (telega--desurrogate-apply
           (or title (telega-chat-title chat)))))

(defun telega-chatbuf--join (chat)
  "[JOIN] button has been pressed."
  (cl-assert (eq chat telega-chatbuf--chat))
  (telega--joinChat chat)

  ;; reset the prompt
  (let ((inhibit-read-only t))
    (button-put telega-chatbuf--prompt-button
                'invisible nil)
    (goto-char telega-chatbuf--prompt-button)
    (delete-region (point-at-bol) telega-chatbuf--prompt-button)
    (goto-char telega-chatbuf--input-marker)))

(defun telega-chatbuf--get-create (chat)
  "Get or create chat buffer for the CHAT."
  (let ((bufname (telega-chatbuf--name chat)))
    (or (get-buffer bufname)

        (with-current-buffer (generate-new-buffer bufname)
          (telega-chat-mode)
          (setq telega-chatbuf--chat chat)

          ;; If me is not member of this chat, then show [JOIN] button
          ;; instead of the prompt
          (unless (telega-filter-chats 'me-is-member (list chat))
            (let ((inhibit-read-only t))
              (button-put telega-chatbuf--prompt-button 'invisible t)
              (goto-char telega-chatbuf--prompt-button)
              (save-excursion
                (telega-ins--button "JOIN"
                  :value chat :action 'telega-chatbuf--join))))

          (telega--openChat chat)
          ;; Insert last message if any
          (let ((last-msg (plist-get chat :last_message)))
            (when last-msg
              (telega-chatbuf--enter-youngest-msg last-msg t)))
          ;; Start loading chat's history
          (telega-chat--load-history chat)

          ;; Openning chat may affect filtering, see `opened' filter
          (telega-root--chat-update chat)

          (current-buffer)))))

(defun telega-chatbuf--oldest-msg ()
  "Return oldest message in the current chat buffer."
  (let ((node (ewoc-nth telega-chatbuf--ewoc 0)))
    (when node
      (ewoc-data node))))

(defun telega-chatbuf--youngest-msg ()
  "Return youngest message in the current chat buffer."
  (let ((node (ewoc-nth telega-chatbuf--ewoc -1)))
    (when node
      (ewoc-data node))))

(defun telega-chat--modeline-buffer-identification (chat)
  "Return `mode-line-buffer-identification' for the CHAT buffer."
  ;; TODO: Display number of mentions that are unread and not visible
  ;; at the moment
  )

(defun telega-chatbuf-input-prev (n)
  "Goto N previous items in chat input history."
  (interactive "p")
  (let ((input (telega-chatbuf-input-string)))
    (unless telega-chatbuf--input-pending-p
      (setq telega-chatbuf--input-pending-p t)
      (ring-insert telega-chatbuf--input-ring input))

    (if telega-chatbuf--input-idx
        (cl-incf telega-chatbuf--input-idx n)
      (setq telega-chatbuf--input-idx n))
    (cond ((< telega-chatbuf--input-idx 0)
           (setq telega-chatbuf--input-idx 0))
          ((> telega-chatbuf--input-idx
              (ring-length telega-chatbuf--input-ring))
           (setq telega-chatbuf--input-idx
                 (ring-length telega-chatbuf--input-ring))))

    (delete-region telega-chatbuf--input-marker (point-max))
    (insert (ring-ref telega-chatbuf--input-ring telega-chatbuf--input-idx))))

(defun telega-chatbuf-input-next (n)
  "Goto next N's item in chat input history."
  (interactive "p")
  (when telega-chatbuf--input-idx
    (telega-chatbuf-input-prev (- n))))

(defun telega-chatbuf-input-search (&optional regex)
  "Search for REGEX in chat input history."
  (interactive)
  ;; TODO:
  ;;  incrementaly search for history
  ;;   see comint-history-isearch-setup
  ;; (let ((elem (cl-find regex (cddr telega-chatbuf--input-ring)
  ;;                      :test #'string-match)))
  ;;   (
  )

(defun telega-chatbuf--enter-oldest-msg (msg)
  "Insert message MSG as oldest message in chatbuffer."
  (run-hook-with-args 'telega-chat-pre-message-hook msg t)

  (with-telega-chatbuf (telega-msg-chat msg)
    (save-excursion
      (run-hook-with-args 'telega-chat-before-oldest-msg-hook msg)

      (let ((onode (ewoc-enter-first telega-chatbuf--ewoc msg)))
        ;; NOTE: Inserting oldest node might affect how message next
        ;; to it is formatted, so redisplay it
        (cl-assert onode)
        (let ((nnode (ewoc-next telega-chatbuf--ewoc onode)))
          (when nnode
            (ewoc-invalidate telega-chatbuf--ewoc nnode)))))))

(defun telega-chatbuf--enter-youngest-msg (msg &optional disable-notification)
  "Insert newly arrived message MSG as youngest into chatbuffer.
If DISABLE-NOTIFICATION is non-nil, then do not trigger
notification for this message.
Return newly inserted message button."
  (run-hook-with-args 'telega-chat-pre-message-hook msg disable-notification)

  (unwind-protect
      (with-telega-chatbuf (telega-msg-chat msg)
        (telega-save-excursion
          (run-hook-with-args 'telega-chat-before-youngest-msg-hook msg)
          (ewoc-enter-last telega-chatbuf--ewoc msg)

          (when (and telega-use-tracking (not disable-notification))
            (tracking-add-buffer (current-buffer)))
          ))

    (run-hook-with-args 'telega-chat-message-hook msg disable-notification)))

(defmacro telega-chatbuf--node-by-msg-id (msg-id)
  "In current chatbuffer find message button with MSG-ID."
  `(telega-ewoc--find-node
    telega-chatbuf--ewoc
    (lambda (msg) (= ,msg-id (plist-get msg :id)))))

(defun telega-chatbuf--visible-messages (window)
  "Return list of messages visible in chat buffer WINDOW."
  (let ((footer (ewoc--footer telega-chatbuf--ewoc))
        (node (ewoc-locate telega-chatbuf--ewoc (window-start window)))
        (messages nil))
    (while (and node (not (eq node footer)))
      (if (not (pos-visible-in-window-p (ewoc-location node) window))
          (setq node footer)            ; done

        (setq messages (push (ewoc-data node) messages))
        (setq node (ewoc-next telega-chatbuf--ewoc node))))
    messages))

(defun telega-chatbuf--prompt-reset ()
  "Reset prompt to initial state in chat buffer."
  (let ((inhibit-read-only t)
        (prompt-invisible-p
         (button-get telega-chatbuf--prompt-button 'invisible)))
    (telega-save-excursion
      (unless (button-get telega-chatbuf--aux-button 'invisible)
        (telega-button--update-value
         telega-chatbuf--aux-button "no-value"
         :inserter 'telega-ins
         'invisible t))

      (telega-button--update-value
       telega-chatbuf--prompt-button telega-chat-input-prompt
       'invisible prompt-invisible-p))))

(defun telega-chatbuf--read-outbox (old-last-read-outbox-msgid)
  "Redisplay chat messages affected by read-outbox change.
OLD-LAST-READ-OUTBOX-MSGID is old value for chat's `:last_read_outbox_message_id'."
  (let ((node (ewoc--footer telega-chatbuf--ewoc)))
    (telega-save-cursor
      (while (and (setq node (ewoc-prev telega-chatbuf--ewoc node))
                  (>= old-last-read-outbox-msgid
                      (plist-get (ewoc-data node) :id)))
        (ewoc-invalidate telega-chatbuf--ewoc node)))))

(defun telega--on-updateNewMessage (event)
  "A new message was received; can also be an outgoing message."
  (let* ((new-msg (plist-get event :message))
         (button (telega-chatbuf--enter-youngest-msg
                  new-msg (plist-get event :disable_notification))))

    ;; If message is visibible in some window, then mark it as read
    ;; see https://github.com/zevlg/telega.el/issues/4
    (when (telega-button--observable-p button)
      (telega--viewMessages
       (telega-chat-get (plist-get new-msg :chat_id))
       (list new-msg)))))

(defun telega--on-updateMessageSendSucceeded (event)
  "Message has been successfully sent to server.
Message id could be updated on this update."
  (let* ((new-msg (plist-get event :message))
         (new-id (plist-get new-msg :chat_id))
         (old-id (plist-get event :old_message_id)))
    (with-telega-chatbuf (telega-chat-get new-id)
      (remhash old-id telega-chatbuf--messages)
      (puthash new-id new-msg telega-chatbuf--messages)

      (let ((node (telega-chatbuf--node-by-msg-id old-id)))
        (cl-assert node nil (format "Can't find message id=%d" old-id))
        (setf (ewoc--node-data node) new-msg)
        (ewoc-invalidate telega-chatbuf--ewoc node)))))

(defun telega--on-updateMessageContent (event)
  "Content of the message has been changed."
  (with-telega-chatbuf (telega-chat-get (plist-get event :chat_id))
    (let ((node (telega-chatbuf--node-by-msg-id
                 (plist-get event :message_id))))
      (when node
        (plist-put (ewoc--node-data node)
                   :content (plist-get event :new_content))
        (ewoc-invalidate telega-chatbuf--ewoc node)))))

(defun telega--on-updateMessageEdited (event)
  "Edited date of the message specified by EVENT has been changed."
  (with-telega-chatbuf (telega-chat-get (plist-get event :chat_id))
    (let ((node (telega-chatbuf--node-by-msg-id
                 (plist-get event :message_id))))
      (when node
        (plist-put (ewoc--node-data node)
                   :edit_date (plist-get event :edit_date))
        (plist-put (ewoc--node-data node)
                   :reply_markup (plist-get event :reply_markup))
        (ewoc-invalidate telega-chatbuf--ewoc node)))))

(defun telega--on-updateMessageViews (event)
  "Number of message views has been updated."
  (with-telega-chatbuf (telega-chat-get (plist-get event :chat_id))
    (let ((node (telega-chatbuf--node-by-msg-id
                 (plist-get event :message_id))))
      (when node
        (plist-put (ewoc--node-data node)
                   :views (plist-get event :views))
        (ewoc-invalidate telega-chatbuf--ewoc node)))))

(defun telega--on-updateMessageSendFailed (event)
  "Message failed to send."
  ;; Triggered for example if trying to send bad picture
  (telega-debug "TODO: `telega--on-updateMessageSendFailed'.")
  )

(defun telega--on-updateDeleteMessages (event)
  "Some messages has been deleted from chat."
  (with-telega-chatbuf (telega-chat-get (plist-get event :chat_id))
    (let ((from-cache-p (plist-get event :from_cache))
          (permanent-p (plist-get event :is_permanent)))
      (seq-doseq (msg-id (plist-get event :message_ids))
        (when from-cache-p
          (remhash msg-id telega-chatbuf--messages))
        (when permanent-p
          (let ((node (telega-chatbuf--node-by-msg-id msg-id)))
            (when node
              (ewoc-delete telega-chatbuf--ewoc node))))
        ))))

(defun telega--on-updateUserChatAction (event)
  "Some user has actions on chat."
  (let* ((chat-id (plist-get event :chat_id))
         (acts-alist (gethash chat-id telega--actions))
         (user-id (plist-get event :user_id))
         (current-action (assq user-id acts-alist))
         (action (plist-get event :action)))
    (cl-case (telega--tl-type action)
      (chatActionCancel
       (puthash chat-id (assq-delete-all user-id acts-alist) telega--actions))
      (t (if current-action
             (setcdr current-action action)
           (puthash chat-id (list (cons user-id action)) telega--actions))))

    (let ((chat (telega-chat-get chat-id)))
      (telega-root--chat-update chat)
      (with-telega-chatbuf chat
        (telega-save-excursion
          (telega-ewoc--set-footer
           telega-chatbuf--ewoc (telega-chatbuf--footer)))))
    ))

(defun telega-chat--load-history (chat &optional from-msg-id offset limit
                                       callback)
  "Load and insert CHAT's history.
If FROM-MSG-ID is specified, then cancel last history load and
start loading messages from FROM-MSG-ID.
OFFSET and LIMIT are passed directly to `getChatHistory'.
CALLBACK is called after history has been loaded."
  (declare (indent 4))
  (with-telega-chatbuf chat
    (when (and from-msg-id telega-chatbuf--history-loading)
      ;; Cancel currently active history load
      (telega-server--callback-put telega-chatbuf--history-loading 'ignore))

    (unless telega-chatbuf--history-loading
      (unless from-msg-id
        (setq from-msg-id (plist-get (telega-chatbuf--oldest-msg) :id)
              offset 0))
      (unless from-msg-id
        (setq from-msg-id (plist-get (plist-get chat :last_message) :id)
              offset -1))

      (when from-msg-id
        ;; Asynchronously load chat history
        (setq telega-chatbuf--history-loading
              (telega-server--call
               (list :@type "getChatHistory"
                     :chat_id (plist-get chat :id)
                     :from_message_id from-msg-id
                     :offset offset
                     :limit (or limit telega-chat-history-limit))
               ;; The callback
               `(lambda (history)
                  (with-telega-chatbuf (telega-chat-get ,(plist-get chat :id))
                    (mapc #'telega-chatbuf--enter-oldest-msg
                          (plist-get history :messages))
                    (setq telega-chatbuf--history-loading nil)
                    (when ,callback
                      (funcall ',callback))))))))))

;; DEPRECATED
(defun telega-chat-send-msg (chat text &optional markdown reply-to-msg
                                  reply-markup notify from-background)
  "Send message to the CHAT.
REPLY-TO-MSG-ID - Id of the message to reply to.
Pass non-nil NOTIFY to generate notification for this message.
Pass non-nil FROM-BACKGROUND if message sent from background."
  (let ((tl-msg (list :@type "sendMessage"
                      :chat_id (plist-get chat :id)
                      :disable_notification (or (not notify) :false)
                      :input_message_content
                      (telega-msg--input-content text markdown))))
    (when reply-to-msg
      (setq tl-msg (plist-put tl-msg :reply_to_message_id
                              (plist-get reply-to-msg :id))))
    (when from-background
      (setq tl-msg (plist-put tl-msg :from_background t)))
    (telega-server--send tl-msg)))

;; DEPRECATED
(defun telega-chat-edit-msg (chat text &optional markdown edit-msg-id reply-markup)
  "Message MSG has been edited."
  (let ((tl-msg (list :@type "editMessageText"
                      :chat_id (plist-get chat :id)
                      :message_id edit-msg-id
                      :input_message_content
                      (telega-msg--input-content text markdown))))
    (telega-server--send tl-msg)))

(defun telega-chatbuf-cancel-aux ()
  "Cancel current aux prompt."
  (interactive)
  (telega-chatbuf--prompt-reset))

(defun telega-chatbuf--help-cancel-keys (what)
  "Show help about canceling reply/edit/fwd in echo area."
  (let ((cancel-keys (where-is-internal
                      'telega-chatbuf-cancel-aux telega-chat-mode-map)))
    (message "%s to cancel %s"
             (mapconcat #'key-description cancel-keys ", ") what)))

(defun telega--sendMessage (chat imc &optional reply-to-msg disable-notify
                                 from-background reply-markup)
  "Send the message content represented by IMC to CHAT."
  (let ((tsm (list :@type "sendMessage"
                   :chat_id (plist-get chat :id)
                   :disable_notification (or disable-notify :false)
                   :input_message_content imc)))
    (when reply-to-msg
      (setq tsm (plist-put tsm :reply_to_message_id
                           (plist-get reply-to-msg :id))))
    (when from-background
      (setq tsm (plist-put tsm :from_background t)))
    (telega-server--send tsm)))

(defun telega--sendMessageAlbum (chat imcs &optional reply-to-msg disable-notify
                                      from-background)
  "Send IMCS as media album."
  (let ((tsm (list :@type "sendMessageAlbum"
                   :chat_id (plist-get chat :id)
                   :disable_notification (or disable-notify :false)
                   :input_message_contents (cl-map 'vector 'identity imcs))))
    (when reply-to-msg
      (setq tsm (plist-put tsm :reply_to_message_id
                           (plist-get reply-to-msg :id))))
    (when from-background
      (setq tsm (plist-put tsm :from_background t)))
    (telega-server--send tsm)))

(defun telega--forwardMessages (chat from-chat messages &optional disable-notify
                                     from-background as-album)
  "Forward MESSAGES FROM-CHAT into CHAT."
  (telega-server--send
   (list :@type "forwardMessages"
         :chat_id (plist-get chat :id)
         :from_chat_id (plist-get from-chat :id)
         :message_ids (cl-map 'vector (telega--tl-prop :id) messages)
         :disable_notification (or disable-notify :false)
         :from_background (or from-background :false)
         :as_album (or as-album :false))))

(defun telega--forwardMessage (chat msg &optional disable-notify from-background)
  "Forward single message MSG to CHAT."
  (telega--forwardMessages
   chat (telega-msg-chat msg) (list msg) disable-notify from-background))

(defun telega--editMessageText (chat msg imc &optional reply-markup)
  (telega-server--send
   (list :@type "editMessageText"
         :chat_id (plist-get chat :id)
         :message_id (plist-get msg :id)
         :input_message_content imc)))

(defun telega--editMessageMedia (chat msg imc &optional reply-markup)
  (telega-server--send
   (list :@type "editMessageMedia"
         :chat_id (plist-get chat :id)
         :message_id (plist-get msg :id)
         :input_message_content imc)))

(defun telega--editMessageCaption (chat msg caption &optional reply-markup)
  (telega-server--send
   (list :@type "editMessageCaption"
         :chat_id (plist-get chat :id)
         :message_id (plist-get msg :id)
         :caption caption)))

(defun telega-chatbuf--input-imcs (markdown)
  "Convert input to input message contents list."
  (let ((attaches (telega--split-by-text-prop
                   (telega-chatbuf-input-string) 'telega-attach))
        result)
    (while attaches
      (let* ((text (car attaches))
             (attach (get-text-property 0 'telega-attach text)))
        (if (not attach)
            ;; Simple text
            (progn
              ;; Check the limit first
              (when (> (length text)
                       (plist-get telega--options :message_text_length_max))
                (error "Message length exceedes %d limit"
                       (plist-get telega--options :message_text_length_max)))

              (push (list :@type "inputMessageText"
                          :text (telega--formattedText text markdown)
                          :clear_draft t)
                    result))

          ;; Some real attachement
          ;; If attachement followed by plain text, then it might be a
          ;; caption for the attachement, in this case add caption to
          ;; attachement
          (when (and (memq (telega--tl-type attach)
                           (list 'inputMessageAnimation 'inputMessageAudio
                                 'inputMessageDocument 'inputMessagePhoto
                                 'inputMessageVideo 'inputMessageVoiceNote))
                     (cadr attaches)
                     (not (get-text-property 0 'telega-attach (cadr attaches))))
            ;; NOTE: there is caption limit in telegram
            ;; Attach the caption
            (when (> (length (cadr attaches))
                     (plist-get telega--options :message_caption_length_max))
              (error "Caption exceedes %d limit"
                     (plist-get telega--options :message_caption_length_max)))

            (let ((cap (telega--formattedText (cadr attaches) markdown)))
              (setq attach (plist-put attach :caption cap)))
            (setq attaches (cdr attaches)))
          (push attach result)))

      (setq attaches (cdr attaches)))
    (nreverse result)))

(defun telega-chatbuf-input-send (prefix-arg)
  "Send current input to the chat.
With PREFIX-ARG, inverses `telega-chat-use-markdown-formatting' setting."
  (interactive "P")
  ;; Send the input
  (let ((input (telega-chatbuf-input-string))
        (imcs (telega-chatbuf--input-imcs
               (if telega-chat-use-markdown-formatting
                   (not prefix-arg)
                 prefix-arg)))
        (replying-msg (telega-chatbuf--replying-msg))
        (editing-msg (telega-chatbuf--editing-msg))
        (forwarding-msg (telega-chatbuf--forwarding-msg)))
    (if editing-msg
        (let ((edit-mc (plist-get editing-msg :content))
              (imc (car imcs)))
          (when (> (length imcs) 1)
            (error "Multiple input messages while edit"))
          (cond ((and (eq (telega--tl-type imc) 'inputMessageText)
                      (eq (telega--tl-type edit-mc) 'messageText))
                 (telega--editMessageText
                  telega-chatbuf--chat editing-msg imc))

                ((eq (telega--tl-type imc) 'inputMessageText)
                 (telega--editMessageCaption
                  telega-chatbuf--chat editing-msg (plist-get imc :text)))

                (t
                 (telega--editMessageMedia
                  telega-chatbuf--chat editing-msg imc))))

      ;; If all IMCS are photos and videos then send them as ablum
      ;; otherwise send IMCS as separate messages
      ;; NOTE: cl-every returns `t' on empty list
      (if (and imcs (cl-every (lambda (imc)
                                (memq (telega--tl-type imc)
                                      '(inputMessagePhoto inputMessageVideo)))
                              imcs))
          (telega--sendMessageAlbum telega-chatbuf--chat imcs replying-msg)
        (cl-dolist (imc imcs)
          (telega--sendMessage telega-chatbuf--chat imc replying-msg))))

    (when forwarding-msg
      (telega--forwardMessage telega-chatbuf--chat forwarding-msg))

    ;; Recover prompt to initial state
    (delete-region telega-chatbuf--input-marker (point-max))
    (telega-chatbuf--prompt-reset)

    ;; Save input to history
    (unless (string-empty-p input)
      (when telega-chatbuf--input-pending-p
        (ring-remove telega-chatbuf--input-ring 0)
        (setq telega-chatbuf--input-pending-p nil))

      (ring-insert telega-chatbuf--input-ring input)
      (setq telega-chatbuf--input-idx nil
            telega-chatbuf--input-pending-p nil))))

(defun telega-chatbuf-input-insert (imc)
  "Insert input content defined by IMC into current input."
  (when (get-text-property (point) 'telega-attach)
    (telega-ins " "))
  ;; NOTE: Put special properties `attach-open-bracket' and
  ;; `attach-close-bracket' to be used by
  ;; `telega-chatbuf--post-command' to determine if part of
  ;; attachement is deleted by `delete-char' or `backward-delete'
  (telega-ins--with-props `(telega-attach ,imc face telega-chat-input-attachment)
    (telega-ins--with-props '(cursor-intangible t)
      (telega-ins--with-props '(attach-open-bracket t)
        (telega-ins (car telega-symbol-attach-brackets)))
      (telega-ins--input-content-one-line imc)
      (telega-ins (cdr telega-symbol-attach-brackets)))
    (telega-ins--with-props '(attach-close-bracket t rear-nonsticky t)
      (telega-ins " "))))

(defun telega-chatbuf-attach-location (location)
  "Attach location to the current input."
  (interactive (list (with-telega-chatbuf-action "ChoosingLocation"
                       (read-string "Location: "))))
  (let ((loc (mapcar 'string-to-number (split-string location ","))))
    (unless (and (numberp (car loc)) (numberp (cadr loc)))
      (error "Invalid location `%s', use: <LAT>,<LONG> format" location))

    (telega-chatbuf-input-insert
     (list :@type "inputMessageLocation"
           :location (list :@type "Location"
                           :latitude (car loc)
                           :longitude (cadr loc))))))

(defun telega-chatbuf-attach-contact (contact)
  "Attach USER contact to the current input."
  (interactive
   (with-telega-chatbuf-action "ChoosingContact"
     (let* ((contacts (telega--getContacts))
            (names-alist (mapcar (lambda (user)
                                   (cons (telega-user--name user 'full)
                                         user))
                                 contacts))
            (name (funcall telega-completing-read-function
                           "Contact: " (mapcar 'car names-alist) nil t))
            (user (cdr (assoc name names-alist))))
       (assert user)
       (list
        (list :@type "Contact"
              :phone_number (concat "+" (plist-get user :phone_number))
              :first_name (plist-get user :first_name)
              :last_name (plist-get user :last_name)
              :vcard ""
              :user_id (plist-get user :id))))))

    (telega-chatbuf-input-insert
     (list :@type "inputMessageContact"
           :contact contact)))

(defun telega-chatbuf-attach-file (file-name)
  "Attach FILE as document to the current input."
  (interactive (list (read-file-name "Attach file: ")))
  (setq file-name (expand-file-name file-name))
  (telega-chatbuf-input-insert
   (list :@type "inputMessageDocument"
         :document (list :@type "inputFileLocal"
                         :path file-name))))

(defun telega-chatbuf-attach-photo (file-name)
  "Attach FILE as document to the current input."
  (interactive (list (read-file-name "Photo: ")))
  (setq file-name (expand-file-name file-name))
  (telega-chatbuf-input-insert
   (list :@type "inputMessagePhoto"
          :photo (list :@type "inputFileLocal"
                       :path file-name))))

(defun telega-chatbuf-attach-video (file-name)
  "Attach FILE as document to the current input."
  (interactive (list (read-file-name "Photo: ")))
  (setq file-name (expand-file-name file-name))
  (telega-chatbuf-input-insert
   (list :@type "inputMessageVideo"
          :video (list :@type "inputFileLocal"
                       :path file-name))))

(defun telega-chatbuf-attach-note-video (file-name)
  "Attach FILE as document to the current input."
  (interactive (list (read-file-name "Video Note: ")))
  (setq file-name (expand-file-name file-name))
  (telega-chatbuf-input-insert
   (list :@type "inputMessageVideoNote"
          :video_note (list :@type "inputFileLocal"
                            :path file-name))))

(defun telega-chatbuf-attach-note-voice (file-name)
  "Attach FILE as document to the current input."
  (interactive (list (read-file-name "Voice Note: ")))
  (setq file-name (expand-file-name file-name))
  (telega-chatbuf-input-insert
   (list :@type "inputMessageVoiceNote"
          :voice_note (list :@type "inputFileLocal"
                            :path file-name))))

(defun telega-chatbuf--attach-tmp-photo (tmpfile &optional doc-p)
  "Attach temporary photo file TMPFILE.
If DOC-P is non-nil, then attach it as document."
  (let* ((preview (and (> (frame-char-height) 1)
                       (create-image tmpfile 'imagemagick nil
                                     :scale 1.0 :ascent 'center
                                     :height (frame-char-height))))
         (tlfile (telega--uploadFile
                  tmpfile (if doc-p 'Document 'Photo) 16)))

    ;; Delete the tmpfile once it has been uploaded to the cloud
    (telega-file--upload-monitor-progress
     (plist-get tlfile :id) (lambda (tlfile filepath)
                              (when (telega-file--uploaded-p tlfile)
                                (delete-file filepath)))
     tmpfile)

    (telega-chatbuf-input-insert
     (list :@type (if doc-p "inputMessageDocument" "inputMessagePhoto")
           (if doc-p :document :photo)
           ;; NOTE: 'telega-preview used in `telega-ins--input-file'
           ;; to insert document/photo preview
           (list :@type (propertize "inputFileId" 'telega-preview preview)
                 :id (plist-get tlfile :id))))
    ))

(defun telega-chatbuf-attach-clipboard (doc-p)
  "Send clipboard image to the chat.
If DOC-P prefix arg as given, then send it as document."
  (interactive "P")
  (let* ((selection-coding-system 'no-conversion) ;for rawdata
         (imgdata (or (gui-get-selection 'CLIPBOARD 'image/png)
                      (error "No image in CLIPBOARD")))
         (temporary-file-directory telega-temp-dir)
         (tmpfile (telega-temp-name "clipboard" ".png"))
         (coding-system-for-write 'binary))
    (write-region imgdata nil tmpfile nil 'quiet)
    (telega-chatbuf--attach-tmp-photo tmpfile doc-p)))

(defun telega-chatbuf-attach-screenshot (frame-only)
  "Attach screenshot to the input.
If prefix arg is given, then take screenshot only of current emacs frame."
  (interactive "P")
  (let* ((temporary-file-directory telega-temp-dir)
         (tmpfile (telega-temp-name "screenshot" ".png")))
    (call-process (or (executable-find "import")
                      (error "Utility `import' (imagemagick) not found"))
                  nil nil nil
                  "-silent"             ;no beep
                  "-window" (if frame-only
                                (frame-parameter nil 'window-id)
                              "root")
                  tmpfile)
    (telega-chatbuf--attach-tmp-photo tmpfile)))

(defun telega-chatbuf-attach-member (user)
  "Add USER to the chat members."
  (interactive (list (telega-completing-read-user "Add user: ")))
  (telega-chat-add-member telega-chatbuf--chat user))

(defun telega-chatbuf-attach (attach-type attach-value)
  "Attach something into message."
  (interactive
   (list (funcall telega-completing-read-function
                  "Attach type: " (nconc
                                   (list "photo" "video"
                                         "note-video" "note-voice"
                                         "file" "location"
                                         "poll" "contact"
                                         "screenshot" "member")
                                   (when (gui-get-selection 'CLIPBOARD 'image/png)
                                     (list "clipboard")))
                  nil t)
         nil))
  (let ((cmd (symbol-function
              (intern (concat "telega-chatbuf-attach-" attach-type)))))
    (assert (commandp cmd))
    (if attach-value
        (funcall cmd attach-value)
      (call-interactively cmd))))

(defun telega--setChatDraftMessage (chat &optional draft-msg)
  "Set CHAT's draft message to DRAFT-MSG.
If DRAFT-MSG is ommited, then clear draft message."
  (telega-server--send
   (nconc (list :@type "setChatDraftMessage"
                :chat_id (plist-get chat :id))
          (when draft-msg
            (list :draft_message draft-msg)))))

(defun telega-chatbuf--switch-out ()
  "Called when switching from chat buffer."
  (when (telega-chatbuf-has-input-p)
    (let ((input (telega-chatbuf-input-string)))
      (when (telega-chat--my-action telega-chatbuf--chat)
        (telega--sendChatAction telega-chatbuf--chat "Cancel"))

      (telega--setChatDraftMessage
       telega-chatbuf--chat
       (list :@type "draftMessage"
             :reply_to_message_id
             (or (plist-get (telega-chatbuf--replying-msg) :id) 0)
             :input_message_text
             (list :@type "inputMessageText"
                   :text (telega--formattedText input)))))))

(defun telega-chatbuf--switch-in ()
  "Called when switching to chat buffer."
  )

;; Message commands
(defun telega-msg-reply (msg)
  "Start replying to MSG."
  (interactive (list (telega-msg-at-point)))

  (with-telega-chatbuf (telega-msg-chat msg)
    (telega-button--update-value
     telega-chatbuf--aux-button msg
     :inserter 'telega-ins--aux-reply-inline
     'invisible nil)

    (telega-button--update-value
     telega-chatbuf--prompt-button telega-chat-reply-prompt)
    (goto-char (point-max))

    (telega-chatbuf--help-cancel-keys "reply")))

(defun telega-msg-edit (msg)
  "Start editing the MSG."
  (interactive (list (telega-msg-at-point)))

  (unless (plist-get msg :can_be_edited)
    (error "Message can't be edited"))

  (with-telega-chatbuf (telega-msg-chat msg)
    (telega-button--update-value
     telega-chatbuf--aux-button msg
     :inserter 'telega-ins--aux-edit-inline
     'invisible nil)

    (telega-button--update-value
     telega-chatbuf--prompt-button telega-chat-edit-prompt)

    ;; Replace any input text with edited message
    (delete-region telega-chatbuf--input-marker (point-max))
    (goto-char (point-max))

    ;; Insert message's text or attachement caption
    (let ((content (plist-get msg :content)))
      (telega-ins--text (or (plist-get content :text)
                            (plist-get content :caption))))

    (telega-chatbuf--help-cancel-keys "edit")))

(defun telega-msg-forward (msg chat)
  "Forward message to chat."
  (interactive (list (telega-msg-at-point)
                     (telega-completing-read-chat "Forward to chat: ")))

  (unless (plist-get msg :can_be_forwarded)
    (error "Message can't be forwarded"))

  (with-current-buffer (telega-chat--pop-to-buffer chat)
    (let ((inhibit-read-only t))
      (telega-button--update-value
       telega-chatbuf--aux-button msg
       :inserter 'telega-ins--aux-fwd-inline
       'invisible nil)

      (goto-char (point-max))
      (telega-button--update-value
       telega-chatbuf--prompt-button telega-chat-fwd-prompt)

      (telega-chatbuf--help-cancel-keys "forward"))
  ))

(defun telega-msg-delete (msg &optional revoke)
  "Delete message MSG.
With prefix arg delete only for yourself."
  (interactive (list (button-get (button-at (point)) :value)
                     (not current-prefix-arg)))

  (when (y-or-n-p (concat (if revoke "Revoke" "Kill") " the message? "))
    (telega--deleteMessages
     (plist-get msg :chat_id) (vector (plist-get msg :id)) revoke)))

(defun telega-chat-complete-username ()
  "Complete username at point."
  (interactive)
  (error "Username completion not yet implemented"))

(defun telega-chat-next-link (n)
  (interactive "p")
  (error "`telega-chat-next-link' not yet implemented"))

(defun telega-chat-prev-link (n)
  (interactive "p")
  (telega-chat-next-link (- n)))

(defun telega-chat-complete-or-next-link ()
  "Complete username at point, or jump to next link."
  (interactive)
  (if (<= telega-chatbuf--input-marker (point))
      (call-interactively 'telega-chat-complete-username)
    (call-interactively 'telega-chat-next-link)))

(defun telega-chat-generate-invite-link (chat-id)
  "Generate invite link for chat with CHAT-ID.
If called interactively then copy generated link into the kill ring."
  (interactive (list (plist-get telega-chatbuf--chat :id)))

  (let ((link (telega-server--call
               (list :@type "generateChatInviteLink"
                     :chat_id chat-id))))
    (when (called-interactively-p 'interactive)
      (kill-new (plist-get link :invite_link))
      (message "Invite link: %s (copied into kill ring)"
               (plist-get link :invite_link)))
    link))

(defun telega-chat-msg-observable-p (chat msg-id)
  "Return non-nil if MSG is observable in CHAT."
  (with-telega-chatbuf chat
    (let ((node (telega-chatbuf--node-by-msg-id msg-id)))
      (when node
        (telega-button--observable-p (ewoc-location node))))))

(defun telega-chat--goto-msg0 (chat-id msg-id &optional highlight)
  "In chat denoted by CHAT-ID goto message denoted by MSG-ID.
Return non-nil on success."
  (with-telega-chatbuf (telega-chat-get chat-id)
    (let ((node (telega-chatbuf--node-by-msg-id msg-id)))
      (when node
        (ewoc-goto-node telega-chatbuf--ewoc node)
        (when highlight
          (message "TODO: animate message highlighting"))
        t))))

(defun telega-chat--goto-msg (chat msg-id &optional highlight)
  "In CHAT goto message denoted by MSG-ID.
If HIGHLIGHT is non-nil then highlight with fading background color."
  (with-current-buffer (telega-chat--pop-to-buffer chat)
    (let ((chat-id (plist-get chat :id)))
      (unless (telega-chat--goto-msg0 chat-id msg-id highlight)
        ;; Not found, need to fetch history
        (telega-ewoc--clean telega-chatbuf--ewoc)
        (telega-chat--load-history chat msg-id -10 20
          `(lambda ()
             (telega-chat--goto-msg0 ,chat-id ,msg-id ,highlight)))))))

(provide 'telega-chat)

;;; telega-chat.el ends here

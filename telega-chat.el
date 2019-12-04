;;; telega-chat.el --- Chat mode for telega  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2019 by Zajcev Evgeny.

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
(require 'subr-x)
(require 'ring)
(require 'url-util)
(require 'seq)

(require 'telega-core)
(require 'telega-tdlib)
(require 'telega-msg)
(require 'telega-ins)
(require 'telega-voip)                  ;telega-voip-call
(require 'telega-notifications)
(require 'telega-sticker)
(require 'telega-company)

(eval-when-compile
  (require 'rx)
  (require 'pcase))                     ;`pcase-let*' and `rx'

;; shutup compiler
(declare-function company-complete-common "company")
(declare-function company-begin-backend "company" (backend &optional callback))

(declare-function tracking-add-buffer "tracking" (buffer &optional faces))
(declare-function tracking-remove-buffer "tracking" (buffer))

(declare-function telega-root--chat-update "telega-root"
                  (chat &optional for-reorder))
(declare-function telega-root--chat-reorder "telega-root" (chat))
(declare-function telega-root--chat-new "telega-root" (chat))
(declare-function telega-status--set "telega-root" (conn-status &optional aux-status raw))

(defvar telega-filters--inhibit-redisplay)
(defvar telega-filters--inhibit-list)
(declare-function telega-filters--redisplay "telega-filter")
(declare-function telega-filter--test "telega-filter" (chat fspec))
(declare-function telega-filter-chats "telega-filter" (filter-spec chats-list))
(declare-function telega-filter-default-p "telega-filter" (&optional filter))

;;; Chatbuf vars
(defvar telega-chatbuf--ewoc nil
  "Ewoc for for the current chat buffer.")
(make-variable-buffer-local 'telega-chatbuf--ewoc)

(defvar telega-chatbuf--input-ring nil
  "The chat input history ring.")
(make-variable-buffer-local 'telega-chatbuf--input-ring)

(defvar telega-chatbuf--input-idx nil
  "The index to the current item in the chat input ring.")
(make-variable-buffer-local 'telega-chatbuf--input-idx)

(defvar telega-chatbuf--input-pending nil
  "Non-nil if last input is not yet commited.
Real value is the pending input string.")
(make-variable-buffer-local 'telega-chatbuf--input-pending)

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

(defvar telega-chatbuf--history-state nil
  "State of the history loading.
Could be `basicgroup', `loaded' or `nil'.")
(make-variable-buffer-local 'telega-chatbuf--history-state)

(defvar telega-chatbuf--voice-msg nil
  "Active (playing/paused) voice note message for the current chat.")
(make-variable-buffer-local 'telega-chatbuf--voice-msg)

(defvar telega-chatbuf--my-action nil
  "My current action in chat buffer.")
(make-variable-buffer-local 'telega-chatbuf--my-action)

;; Special variable used to set `telega-chatbuf--chat'
;; inside `telega-chat-mode'
(defvar telega-chat--preparing-buffer-for)


(defun telega-chat--set-uaprops (chat uaprops)
  "Set CHAT's user application properties to UAPROPS."
  (plist-put chat :uaprops uaprops)
  (let ((client-data (if uaprops (prin1-to-string uaprops) "")))
    (plist-put chat :client_data client-data)
    (telega-server--send
     (list :@type "setChatClientData"
           :chat_id (plist-get chat :id)
           :client_data client-data))))

(defun telega-chat-color (chat)
  "Return color list associated with CHAT.
If there is no CHAT color, then generate new and assign it to CHAT."
  (or (telega-chat-uaprop chat :color)
      (setf (telega-chat-uaprop chat :color)
            (telega-color-tripple (telega-color-random)))))

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
          ;; NOTE: plist might contain strings with surropagated
          ;; pairs, so `telega-tl-str' is used, see
          ;; https://github.com/zevlg/telega.el/issues/94
          (let ((client-data (telega-tl-str chat :client_data)))
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

(defun telega-chat-by-username (username)
  "Find chat by its USERNAME."
  (cl-find username telega--ordered-chats
           :test 'string=
           :key (lambda (chat)
                  (telega-tl-str (telega-chat--info chat) :username))))

(defun telega--joinChatByInviteLink (invite-link)
  "Return new chat by its INVITE-LINK.
Return nil if can't join the chat."
  (let ((chat (telega-server--call
               (list :@type "joinChatByInviteLink"
                     :invite_link invite-link))))
    (telega-chat-get (plist-get chat :id))))

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

(defun telega-chat-me ()
  "Chat with myself, a.k.a Saved Messages."
  ;; NOTE: Saved Messages has same id as me user
  (telega-chat-get telega--me-id 'offline))

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
                (telega-user-bot-p (telega-chat--user chat)))
           'bot)
          (t type-sym))))

(defsubst telega-chat-bot-p (chat)
  "Return non-nil if CHAT is the chat with bot."
  (eq (telega-chat--type chat) 'bot))

(defun telega-chat-private-p (chat &optional include-bots-p)
  "Return non-nil if CHAT is private.
If INCLUDE-BOTS-P is non-nil, then return non-nil also for bots."
  (eq (telega-chat--type chat include-bots-p) 'private))

(defun telega-chat-secret-p (chat)
  "Return non-nil if CHAT is secret."
  (eq (telega-chat--type chat 'no-interpret) 'secret))

(defun telega-chat-public-p (chat &optional chat-type)
  "Return non-nil if CHAT is public.
Public chats are only chats with non-empty username.
CHAT-TYPE is either `private', `supergroup' or `any'.
`supergroup' type also includes channels.
By default CHAT-TYPE is `any'."
  (and (or (eq (or chat-type 'any) 'any)
           (eq (telega-chat--type chat 'no-interpret) chat-type))
       (telega-chat-username chat)))

(defun telega-chat-muted-p (chat)
  "Return non-nil if CHAT is muted."
  (> (telega-chat-notification-setting chat :mute_for) 0))

(defun telega-chat-user (chat &optional include-bots-p)
  "For private CHAT return corresponding user.
If CHAT is not private, return nil.
If INCLUDE-BOTS-P is non-nil, return corresponding bot user."
  (when (telega-chat-private-p chat include-bots-p)
    (telega-user--get (telega--tl-get chat :type :user_id))))

(defun telega-chat-title (chat &optional with-username)
  "Return title for the CHAT.
If WITH-USERNAME is specified, append trailing username for this chat."
  (let ((title (telega-tl-str chat :title)))
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

(defun telega-chat-brackets (chat)
  "Return CHAT's brackets from `telega-chat-button-brackets'."
  (cdr (seq-find (lambda (bspec)
                   (telega-filter--test chat (car bspec)))
                 telega-chat-button-brackets)))

(defun telega-chat--reorder (chat order)
  "Reorder CHAT in `telega--ordered-chats' according to ORDER."
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
    (telega-root--chat-new chat)

    (run-hook-with-args 'telega-chat-created-hook chat)))

(defun telega--on-updateChatPhoto (event)
  "Chat's photo has been updated."
  (let ((chat (telega-chat-get (plist-get event :chat_id)))
        (photo (plist-get event :photo)))
    (plist-put chat :photo photo)

    (plist-put chat :telega-image nil)
    ;; TODO:
    ;;  - redisplay chat in rootbuf
    ;;  - redisplay chat's input in case icon is used in input
    ;;  - redisplay *Telegram Chat Info* in case it is shown for the
    ;;    CHAT
    ))

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

    ;; NOTE: unread_count affects modeline and footer
    (with-telega-chatbuf chat
      ;; NOTE: if all messages are read (in another telegram client) and
      ;; tracking is enabled, then remove the buffer from tracking
      (when (and telega-use-tracking (zerop unread-count))
        (tracking-remove-buffer (current-buffer)))

      (telega-chatbuf-mode-line-update)
      (telega-chatbuf--footer-redisplay))

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

    ;; NOTE: unread_mention_count affects modeline and footer
    (with-telega-chatbuf chat
      (telega-chatbuf-mode-line-update)
      (telega-chatbuf--footer-redisplay))

    (telega-root--chat-update chat)))

(defun telega--on-updateMessageMentionRead (event)
  (telega--on-updateChatUnreadMentionCount event)
  ;; TODO: might be some action needed on `:message_id' as well
  )

(defun telega-chat--update-reply-markup-message (chat &optional offline-p)
  "Asynchronously load reply markup message for CHAT.
Pass non-nil OFFLINE-P argument to avoid any async requests."
  (let ((reply-markup-msg-id (plist-get chat :reply_markup_message_id))
        (reply-markup-msg (telega-chat-reply-markup-msg chat 'offline)))
    (if (or (zerop reply-markup-msg-id) reply-markup-msg offline-p)
        (with-telega-chatbuf chat
          (telega-chatbuf--footer-redisplay))

      ;; Async load reply markup message
      (telega-chat-reply-markup-msg chat nil
        (lambda (rm-message)
          (with-telega-chatbuf chat
            (telega-chatbuf--cache-msg
             (or rm-message
                 (list :id reply-markup-msg-id
                       :telega-is-deleted-message t))))
          (telega-chat--update-reply-markup-message chat 'offline))))))

(defun telega--on-updateChatReplyMarkup (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :reply_markup_message_id
               (plist-get event :reply_markup_message_id))

    (telega-chat--update-reply-markup-message chat)))

(defun telega--on-updateChatLastMessage (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :last_message
               (plist-get event :last_message))
    (unless (string= (plist-get event :order) (plist-get chat :order))
      (telega-chat--reorder chat (plist-get event :order)))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatIsMarkedAsUnread (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :is_marked_as_unread
               (plist-get event :is_marked_as_unread))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatOnlineMemberCount (event)
  "The number of online group members has changed.
NOTE: we store the number as custom chat property, to use it later."
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline)))
    (cl-assert chat)
    (plist-put chat :x-online-count
               (plist-get event :online_member_count))

    ;; NOTE: this affects the modeline
    (with-telega-chatbuf chat
      (telega-chatbuf-mode-line-update))

    ;; ARGUABLE: root buffer chat inserter might use the counter
    ; (telega-root--chat-update chat)
    ))

(defun telega-chat--update-pinned-message (chat &optional offline-p
                                                old-pin-msg-id)
  "Asynchronously load pinned message for CHAT.
Pass non-nil OFFLINE-P argument to avoid any async requests.
OLD-PIN-MSG-ID is the id of the previously pinned message."
  (let ((pin-msg-id (plist-get chat :pinned_message_id))
        (pin-msg (telega-chat-pinned-msg chat 'locally)))
    (if (or (zerop pin-msg-id) pin-msg offline-p)
        (progn
          (with-telega-chatbuf chat
            (when-let ((old-pin (and old-pin-msg-id
                                     (not (zerop old-pin-msg-id))
                                     (telega-chatbuf--msg
                                      old-pin-msg-id 'with-node))))
              (apply 'telega-msg-redisplay old-pin))
            (when pin-msg
              (telega-msg-redisplay pin-msg))

            (telega-chatbuf-mode-line-update)
            (telega-chatbuf--footer-redisplay))
          (telega-root--chat-update chat))

      ;; Async load pinned message
      (telega-chat-pinned-msg chat nil
        (lambda (pinned-message)
          (with-telega-chatbuf chat
            (telega-chatbuf--cache-msg
             (or pinned-message
                 (list :id pin-msg-id
                       :telega-is-deleted-message t))))
          (telega-chat--update-pinned-message chat 'offline))))))

(defun telega--on-updateChatPinnedMessage (event)
  "The chat pinned message was changed."
  (let* ((chat (telega-chat-get (plist-get event :chat_id) 'offline))
         (old-pinned-message-id (plist-get chat :pinned_message_id)))
    (cl-assert chat)
    (plist-put chat :pinned_message_id
               (plist-get event :pinned_message_id))

    (telega-chat--update-pinned-message chat nil old-pinned-message-id)))

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

(defun telega--getChatPinnedMessage (chat &optional callback)
  "Get pinned message for the CHAT, if any."
  (unless (zerop (plist-get chat :pinned_message_id))
    (telega-server--call
     (list :@type "getChatPinnedMessage"
           :chat_id (plist-get chat :id))
     callback)))

(defsubst telega-chats-list-get (tl-obj-chats)
  "Return chats list of TL-OBJ-CHATS represeting `Chats' object."
  (mapcar #'telega-chat-get (plist-get tl-obj-chats :chat_ids)))

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
        (setq top (list (time-to-seconds (current-time))
                        (telega-chats-list-get cl)))
        (setf (alist-get category telega--top-chats) top)))
    (caddr top)))

(defun telega--sendChatAction (chat action)
  "Send ACTION on CHAT."
  (telega-server--send
   (list :@type "sendChatAction"
         :chat_id (plist-get chat :id)
         :action action)))

(defun telega--createPrivateChat (user)
  "Create private chat with USER.
Return newly created chat."
  (telega-chat-get
   (plist-get
    (telega-server--call
     (list :@type "createPrivateChat"
           :user_id (plist-get user :id))) :id)))

(defun telega--viewMessages (chat messages &optional force)
  "Mark CHAT's MESSAGES as read.
Use non-nil value for FORCE, if messages in closed chats should
be marked as read."
  (telega-server--send
   (list :@type "viewMessages"
         :chat_id (plist-get chat :id)
         :message_ids (cl-map 'vector (telega--tl-prop :id) messages)
         :force_read (if force t :false))))

(defun telega-chatbuf--view-visible-messages (window &optional display-start force)
  "View all visible messages in chatbuf's window."
  (when (or (> (plist-get telega-chatbuf--chat :unread_count) 0)
            (< (plist-get telega-chatbuf--chat :last_read_inbox_message_id)
               (or (telega--tl-get telega-chatbuf--chat :last_message :id) 0)))
    (when-let ((visible-messages (telega-chatbuf--visible-messages window display-start)))
      (telega--viewMessages telega-chatbuf--chat visible-messages force))))

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

(defun telega--searchPublicChat (username &optional callback)
  "Search public chat with USERNAME.
If CALLBACK is specified, call it with one argument - CHAT."
  (declare (indent 1))
  (let ((ret (telega-server--call
              (list :@type "searchPublicChat"
                    :username username)
              (when callback
                (lambda (reply)
                  (when reply
                    (funcall
                     callback (telega-chat-get (plist-get reply :id)))))))))
    (if callback
        ret
      (when ret
        (telega-chat-get (plist-get ret :id))))))

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
  (telega-chats-list-get
   (telega-server--call
    (list :@type "searchChatsOnServer"
          :query query
          :limit (or limit 200)))))

(defun telega--getGroupsInCommon (with-user &optional limit)
  "Return list of common chats WITH-USER.
LIMIT - number of chats to get (default=100)"
  (telega-chats-list-get
   (telega-server--call
    (list :@type "getGroupsInCommon"
          :user_id (plist-get with-user :id)
          :offset_chat_id 0
          :limit (or limit 100)))))


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
    (define-key map (kbd "L") 'telega-chat-custom-label)
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
  (unless (= (char-before) ?\n)
    (insert "\n")))

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
  "Pop to CHAT's buffer.
Uses `telega-chat--display-buffer-action' as action in `pop-to-buffer.'"
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
  "Add USER to the CHAT."
  (interactive (list (or telega-chatbuf--chat
                         telega--chat
                         (telega-chat-at-point))
                     (telega-completing-read-user "Add member: ")))
  (cl-assert user)
  (telega-server--send
   (list :@type "addChatMember"
         :chat_id (plist-get chat :id)
         :user_id (plist-get user :id)
         :forward_limit (or forward-limit 100))))

(defun telega-chat-set-title (chat title)
  "Set CHAT's title to TITLE."
  (interactive (let ((chat (or telega-chatbuf--chat
                               (telega-chat-at-point))))
                 (list chat
                       (read-string "New title: " (telega-chat-title chat)))))
  (telega--setChatTitle chat title))

(defun telega-chat-custom-order (chat order)
  "For the CHAT (un)set custom ORDER."
  (interactive (let ((chat (telega-chat-at-point)))
                 (list chat
                       (read-string "Custom Order [empty to unset]: "
                                    (telega-chat--order chat 'as-str)))))
  (if (string-empty-p order)
      (setq order nil)

    (unless (numberp (read order))
      (error "Invalid order, must contain only digits")))

  (setf (telega-chat-uaprop chat :order) order)
  (telega-chat--reorder chat nil)
  (telega-root--chat-update chat))

(defun telega-chat-custom-label (chat label)
  "For CHAT (un)set custom LABEL."
  (interactive (let* ((chat (telega-chat-at-point))
                      (chat-label (telega-chat-uaprop chat :label)))
                 ;; NOTE: If chat already has label, then use
                 ;; `read-string' to change label, so empty name can
                 ;; be used to unset the label
                 (list chat
                       (if chat-label
                           (read-string "Custom label [empty to unset]: "
                                        chat-label)
                       (funcall telega-completing-read-function
                                "Custom label: " (telega-custom-labels))))))
  (when (string-empty-p label)
    (setq label nil))

  (setf (telega-chat-uaprop chat :label) label)
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
  (when (telega-chat-secret-p chat)
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
  (interactive (list telega-chatbuf--chat))
  (when chat
    (let* ((me (telega-user-me))
           (me-contact
            (list :@type "Contact"
                  :phone_number (concat "+" (plist-get me :phone_number))
                  :first_name (telega-tl-str me :first_name)
                  :last_name (telega-tl-str me :last_name)
                  :vcard ""
                  :user_id (plist-get me :id))))
      (telega--sendMessage chat (list :@type "inputMessageContact"
                                      :contact me-contact)))))

(defun telega-describe-chat (chat)
  "Show info about chat at point."
  (interactive (list (telega-chat-at-point)))
  (with-telega-help-win "*Telegram Chat Info*"
    (setq telega--chat chat)

    (let ((chat-ava (telega-chat-avatar-image chat)))
      (telega-ins--image chat-ava 0)
      (telega-ins--with-face
          (let ((color (telega-chat-color chat))
                (lightp (eq (frame-parameter nil 'background-mode) 'light)))
            (list :foreground (nth (if lightp 2 0) color)))
        (telega-ins (telega-chat-title chat 'with-username)))
      (telega-ins "\n")
      (telega-ins--image chat-ava 1)
      (telega-ins (capitalize (symbol-name (telega-chat--type chat))) " ")
      (telega-ins--button "Open"
        :value chat
        :action 'telega-chat--pop-to-buffer)
      (when (telega--tl-get chat :permissions :can_invite_users)
        (telega-ins " ")
        (telega-ins--button "Add Member"
          'action (lambda (_ignored)
                    (telega-chat-add-member
                     chat (telega-completing-read-user "Add member: ")))))
      (telega-ins "\n"))

    (telega-ins-fmt "Id: %d\n" (plist-get chat :id))
    (when (telega-chat-public-p chat)
      (let ((link (concat (or (plist-get telega--options :t_me_url)
                              "https://t.me/")
                          (telega-tl-str (telega-chat--supergroup chat) :username))))
        (insert "Link: ")
        (apply 'insert-text-button link (telega-link-props 'url link 'link))
        (insert "\n")))
    (telega-ins "Order")
    (when (telega-chat-uaprop chat :order)
      (telega-ins " (" (propertize "custom" 'face 'shadow) ")"))
    (telega-ins ": " (telega-chat--order chat 'as-str) "\n")

    (let* (;; (default-mute-for
           ;;   (telega-chat-notification-setting chat :mute_for 'default))
           ;; (default-show-preview
           ;;   (telega-chat-notification-setting chat :show_preview 'default))
           (not-cfg (plist-get chat :notification_settings))
           (unmuted-p (not (telega-chat-muted-p chat)))
           (show-preview (telega-chat-notification-setting chat :show_preview)))
      (telega-ins "Notifications ("
                  (propertize (if (plist-get not-cfg :use_default_mute_for)
                                  "default" "custom")
                              'face 'shadow)
                  "): ")

      (telega-ins--button (if unmuted-p
                              telega-symbol-heavy-checkmark
                            "  ")
        :value chat
        :action (lambda (chat)
                  (telega--setChatNotificationSettings chat
                    :use_default_mute_for nil
                    :mute_for (if unmuted-p 599634793 0))
                  (telega-save-cursor
                    (telega-describe-chat chat))))
      (when unmuted-p
        (telega-ins ", Preview ("
                    (propertize (if (plist-get not-cfg :use_default_show_preview)
                                    "default" "custom")
                                'face 'shadow)
                    "): ")
        (telega-ins--with-face 'telega-box
          (telega-ins (if show-preview
                          telega-symbol-heavy-checkmark
                        "  "))))
      ;; Reseting custom notification settings
      (unless (and (plist-get not-cfg :use_default_mute_for)
                   (plist-get not-cfg :use_default_show_preview))
        (telega-ins " ")
        (telega-ins--button "Reset"
          :value chat
          :action (lambda (chat)
                    (telega--setChatNotificationSettings chat
                      :use_default_mute_for t
                      :use_default_show_preview t)
                    (telega-save-cursor
                      (telega-describe-chat chat)))))
      (telega-ins "\n"))

    (telega-ins "\n")
    (cl-ecase (telega-chat--type chat 'no-interpret)
      (private
       (telega-ins "--- User Info ---\n")
       (telega-info--insert-user
        (telega-chat--info chat) chat
        (lambda () (telega-describe-chat chat))))
      (secret
       (telega-ins "--- SecretChat Info ---\n")
       (telega-info--insert-secretchat
        (telega-chat--info chat) chat))
      (basicgroup
       (telega-ins "--- BasicGroup Info ---\n")
       (telega-info--insert-basicgroup
        (telega-chat--info chat) chat))
      (supergroup
       (telega-ins "--- SuperGroup Info ---\n")
       (telega-info--insert-supergroup
        (telega-chat--info chat) chat)))

    (when telega-debug
      (telega-ins "\n---DEBUG---\n")
      (telega-ins (propertize "Chat: " 'face 'bold)
                  (format "%S" chat) "\n")
      (telega-ins (propertize "Info: " 'face 'bold)
                  (format "%S" (telega-chat--info chat))))
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
  (interactive "sJoin chat by invite link: ")
  (telega-chat--pop-to-buffer (telega--joinChatByInviteLink link)))

(defun telega-chat-toggle-read (chat)
  "Toggle chat as read/unread."
  (interactive (list (telega-chat-at-point)))
  (let ((unread-count (plist-get chat :unread_count))
        (unread-mentions-count (plist-get chat :unread_mention_count))
        (marked-unread-p (plist-get chat :is_marked_as_unread)))
    (if (or (> unread-count 0) (> unread-mentions-count 0) marked-unread-p)
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

(defun telega-chats-filtered-toggle-read (&optional _force)
  "Apply `telega-chat-toggle-read' to all currently filtered chats."
  (interactive
   (list (y-or-n-p (format "Toggle read for %d chats? "
                           (length telega--filtered-chats)))))
  ;; NOTE: If no filter is applied, ask once more time
  (when (or (not (telega-filter-default-p))
            (y-or-n-p "No filtering applied, toggle anyway? "))
    (mapc 'telega-chat-toggle-read telega--filtered-chats)))

(defun telega-chat-delete (chat &optional leave-p)
  "Delete CHAT.
If LEAVE-P is non-nil, then just leave the chat.
Leaving chat does not removes chat from chat list."
  (interactive (list (telega-chat-at-point) nil))
  (when (and chat
             (yes-or-no-p
              (format "This action cannot be undone. Delete \"%s\" chat? "
                      (telega-chat-title chat))))
    (let ((chat-type (telega-chat--type chat)))
      (cl-case chat-type
        (secret (telega--closeSecretChat (telega-chat--info chat)))
        ((private bot) 'no-op)
        (t (telega--leaveChat chat)))

      ;; NOTE: `telega--deleteChatHistory' Cannot be used in channels
      ;; and public supergroups
      (unless (or (eq chat-type 'channel)
                  (telega-chat-public-p chat 'supergroup)
                  leave-p)
        (telega--deleteChatHistory chat t))

      (when-let ((chat-user (telega-chat-user chat 'include-bots)))
        (when (yes-or-no-p
               (format "Block \"%s\" user as well? " (telega-user--name chat-user)))
          (telega--blockUser chat-user))))

    ;; Kill corresponding chat buffer
    (with-telega-chatbuf chat
      (kill-buffer (current-buffer)))))

(defun telega-chat-create (title &rest users)
  "Create new chat with TITLE and USERS."
  (interactive
   (cons (read-string "Chat title: ")
         (let ((users nil) (done nil))
           (while (not done)
             (condition-case nil
                 (setq users
                       (cons (telega-completing-read-user
                              (format "Add user (C-g when done)%s: "
                                      (if users
                                          (concat " [" (mapconcat 'telega-user--name users ",") "]")
                                        "")))
                              users))
               (quit (setq done t))))
           users)))

  (telega-server--call
   (list :@type "createNewBasicGroupChat"
         :user_ids (cl-map 'vector (telega--tl-prop :id) users)
         :title title)
   (lambda (newchat)
     ;; NOTE: Chat might change while created, so renew its value
     ;; using `telega-chat-get'
     (telega-chat--pop-to-buffer
      (telega-chat-get (plist-get newchat :id))))))

(defun telega-chat-upgrade-to-supergroup (chat)
  "Upgrade basic group CHAT from basicgroup to supergroup."
  (interactive (list (or telega-chatbuf--chat
                         (telega-chat-at-point))))
  (telega-server--call
   (list :@type "upgradeBasicGroupChatToSupergroupChat"
         :chat_id (plist-get chat :id))
   (lambda (newchat)
     ;; NOTE: Chat might change while upgrading, so renew its value
     ;; using `telega-chat-get'
     (telega-chat--pop-to-buffer
      (telega-chat-get (plist-get newchat :id))))))

(defun telega-chat-description (chat descr)
  "Update CHAT's description."
  (interactive (let* ((chat (or telega-chatbuf--chat (telega-chat-at-point)))
                      (full-info (telega--full-info (telega-chat--info chat))))
                 (list chat
                       (read-string "Description: "
                                    (telega-tl-str full-info :description)))))
  (telega--setChatDescription chat descr))

(defun telega-chats-filtered-delete (&optional force)
  "Apply `telega-chat-delete' to all currently filtered chats.
Do it only if FORCE is non-nil."
  (interactive
   (list (yes-or-no-p (format "This action cannot be undone. Delete %d chats? "
                              (length telega--filtered-chats)))))
  (when force
    (mapc 'telega-chat-delete telega--filtered-chats)))

(defun telega-saved-messages (arg)
  "Switch to SavedMessages chat buffer.
If SavedMessages chat is not opened, then open it.
If prefix ARG is specified, then keep the point, otherwise goto
end of the buffer."
  (interactive "P")
  (telega-chat--pop-to-buffer (telega-chat-me))
  (unless arg
    (goto-char (point-max))))

(defun telega-switch-buffer (buffer)
  "Interactive switch to chat BUFFER."
  (interactive
   (list (funcall telega-completing-read-function
                  "Telega chat: "
                  (mapcar 'telega-chatbuf--name
                          (cl-sort
                           (mapcar 'telega-chatbuf--chat telega--chat-buffers)
                           (car telega-chat-switch-buffer-sort-by)
                           :key (cdr telega-chat-switch-buffer-sort-by)))
                  nil t)))
  (switch-to-buffer buffer))

(defun telega-chat-reply-markup-msg (chat &optional offline-p callback)
  "Return reply markup for the CHAT.
If OFFLINE-P is non-nil, then do not perform any requests to telega-server.
If CALLBACK is specified, then get reply markup asynchronously."
  (declare (indent 2))
  (let ((reply-markup-msg-id (plist-get chat :reply_markup_message_id)))
    (unless (zerop reply-markup-msg-id)
      (telega-msg--get (plist-get chat :id) reply-markup-msg-id offline-p
        callback))))

(defun telega-chat-pinned-msg (chat &optional offline-p callback)
  "Return pinned message for the CHAT.
If OFFLINE-P is non-nil, then do not perform any requests to telega-server."
  (declare (indent 2))
  (let ((pin-msg-id (plist-get chat :pinned_message_id)))
    (unless (zerop pin-msg-id)
      (telega-msg--get (plist-get chat :id) pin-msg-id offline-p callback))))


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
    (define-key map (kbd "C-c C-r") 'telega-chatbuf-filter-related)
    (define-key map (kbd "C-c C-s") 'telega-chatbuf-filter-search)
    (define-key map (kbd "C-c C-c") 'telega-chatbuf-filter-cancel)

    (define-key map (kbd "RET") 'telega-chatbuf-input-send)
    (define-key map (kbd "M-p") 'telega-chatbuf-input-prev)
    (define-key map (kbd "M-n") 'telega-chatbuf-input-next)
    (define-key map (kbd "M-r") 'telega-chatbuf-input-search)
    (define-key map (kbd "M-g <") 'telega-chatbuf-history-beginning)
    (define-key map (kbd "M-g >") 'telega-chatbuf-read-all)
    (define-key map (kbd "M-g r") 'telega-chatbuf-read-all)
    (define-key map (kbd "M-g m") 'telega-chatbuf-next-mention)

    ;; jumping around links
    (define-key map (kbd "TAB") 'telega-chat-complete-or-next-link)
    (define-key map (kbd "<backtab>") 'telega-chat-prev-link)
    map))

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

(defun telega-ins--voice-msg-status (msg)
  "Insert voice message MSG status.
Used in chatbuf footer."
  (let* ((proc (plist-get msg :telega-ffplay-proc))
         (proc-status (and (process-live-p proc) (process-status proc)))
         (played (and proc-status (plist-get (process-plist proc) :progress)))
         (sender (telega-msg-sender msg)))
    (when played ;(memq proc-status '(run stop))
      (telega-ins "[")
      (telega-ins--raw-button
          (list 'action (lambda (_button)
                          (if (eq proc-status 'run)
                              (telega-ffplay-pause proc)
                            (telega-ffplay-resume proc)))
                'face nil)
        (if (eq proc-status 'run)
            (telega-ins telega-symbol-pause)
          (telega-ins telega-symbol-play))
        (telega-ins " ")
        (if sender
            (telega-ins (telega-user--name sender))
          (telega-ins (telega-chat-title (telega-msg-chat msg)))))
      (telega-ins " ")
      (telega-ins--button "stop"
        'action (lambda (_ignored)
                  (telega-ffplay-stop)))
      (telega-ins "]"))))

(defsubst telega-chatbuf--nth-msg (n)
  "Return N's oldest message in the current chat buffer."
  (let ((node (ewoc-nth telega-chatbuf--ewoc n)))
    (when node
      (ewoc-data node))))

(defmacro telega-chatbuf--first-msg ()
  "Return first message inserted in chat buffer."
  `(telega-chatbuf--nth-msg 0))

(defmacro telega-chatbuf--last-msg ()
  "Return last message inserted in chat buffer."
  `(telega-chatbuf--nth-msg -1))

(defsubst telega-chatbuf--last-msg-loaded-p (&optional for-msg)
  "Return non-nil if `:last_message' of the chat is loaded.
FOR-MSG can be optionally specified, and used instead of yongest message."
  (let ((last-msg-id
         (or (telega--tl-get telega-chatbuf--chat :last_message :id) 0)))
    (or (<= last-msg-id (or (plist-get (telega-chatbuf--last-msg) :id) 0))
        (and for-msg (= last-msg-id (plist-get for-msg :id))))))

(defun telega-chatbuf--footer ()
  "Generate string to be used as ewoc's footer."
  ;; --(actions part)---------------[additional status]--
  ;; 📌 Pinned Message
  ;; [ REPLY-MARKUP] [ BUTTONS ]
  ;; >>>
  (let* ((column (+ telega-chat-fill-column 10 1))
         (column1 (/ column 2))
         (column2 (- column column1))
         (fill-symbol (if (and telega-chatbuf--ewoc
                               (telega-chatbuf--last-msg-loaded-p))
                          telega-symbol-underline-bar
                        "."))
         ;; NOTE: `telega-ins--as-string' uses temporary buffer, so
         ;; prepare everything we need before
         (actions (gethash (plist-get telega-chatbuf--chat :id)
                           telega--actions))
         (chat telega-chatbuf--chat)
         (history-loading-p telega-chatbuf--history-loading)
         (voice-msg telega-chatbuf--voice-msg))
    (telega-ins--as-string
     (telega-ins--with-props '(read-only t rear-nonsticky t front-sticky nil)
       ;; Chat action part
       (telega-ins fill-symbol)
       (telega-ins--with-attrs (list :min (- column1 2)
                                     :max (- column1 2)
                                     :align 'left
                                     :align-symbol fill-symbol
                                     :elide t
                                     :elide-trail (/ column1 2))
         (when actions
           (telega-ins "(")
           (telega-ins--actions actions)
           (telega-ins ")")))
       (telega-ins fill-symbol)

       ;; Chat's additional info part
       (telega-ins fill-symbol)
       (telega-ins--with-attrs (list :min (- column2 2)
                                     :max (- column2 2)
                                     :align 'right
                                     :align-symbol fill-symbol
                                     :elide t
                                     :elide-trail (/ column2 2))
         (cond (history-loading-p
                (telega-ins "[history loading]"))
               (voice-msg
                (telega-ins--voice-msg-status voice-msg))
               ))
       (telega-ins fill-symbol)
       (telega-ins "\n")

       ;; Pinned message
       (when-let ((pin-msg
                   (and telega-chat-show-pinned-message
                        (not (zerop (plist-get chat :pinned_message_id)))
                        (telega-chat-pinned-msg chat 'offline))))
         (unless (plist-get pin-msg :telega-is-deleted-message)
           ;; NOTE: Using 'telega-msg button makes point behave
           ;; strange, so drop to `telega' button type
           (telega-button--insert 'telega pin-msg
             :inserter 'telega-ins--chat-pin-msg-one-line
             :action 'telega-msg-goto-highlight)
           (telega-ins "\n")))

       ;; Reply markup
       (when-let ((markup-msg
                   (and (not (zerop (plist-get chat :reply_markup_message_id)))
                        (telega-chat-reply-markup-msg chat 'offline))))
         (unless (plist-get markup-msg :telega-is-deleted-message)
           (telega-ins--reply-markup markup-msg 'force)
           (telega-ins "\n")))
       ))))

(defun telega-chatbuf--footer-redisplay ()
  "Redisplay chatbuf's footer.
Update modeline as well."
  (setq mode-line-process
        (cond (telega-chatbuf--history-loading
               "[history loading]")
              (telega-chatbuf--voice-msg
               "[voice note]")))
  (force-mode-line-update)

  (telega-save-cursor
    (telega-ewoc--set-footer
     telega-chatbuf--ewoc (telega-chatbuf--footer))))

(defun telega-chatbuf--check-focus-change ()
  "Check is some messages need to be viewed."
  (cl-assert (eq major-mode 'telega-chat-mode))
  (when (telega-focus-state)
    ;; NOTE: on focus-in view all visible messages, see
    ;; https://github.com/zevlg/telega.el/issues/81
    (when-let ((chat-win (get-buffer-window (current-buffer))))
      (telega-chatbuf--view-visible-messages chat-win))))

(define-derived-mode telega-chat-mode nil "Telega-Chat"
  "The mode for telega chat buffer.

Message bindings (cursor on message):
\\{telega-msg-button-map}
Global chat bindings:
\\{telega-chat-mode-map}"
  :group 'telega-chat
  (setq telega-chatbuf--chat telega-chat--preparing-buffer-for
        telega-chatbuf--messages (make-hash-table :test 'eq)
        telega-chatbuf--input-ring (make-ring telega-chat-input-ring-size)
        telega-chatbuf--input-idx nil
        telega-chatbuf--input-pending nil
        telega-chatbuf--history-loading nil
        telega-chatbuf--inline-query nil
        telega-chatbuf--voice-msg nil
        telega-chatbuf--my-action nil)

  ;; Make usernames with "_" be completable
  (modify-syntax-entry ?\_ "w" telega-chat-mode-syntax-table)

  (erase-buffer)
  (setq-local window-point-insertion-type t)
  (setq-local next-line-add-newlines nil)
  (cursor-sensor-mode 1)
  (cursor-intangible-mode 1)

  (setq telega-chatbuf--ewoc
        (ewoc-create (if telega-debug
                         'telega-msg--pp
                       (telega-ewoc--gen-pp 'telega-msg--pp))
                     nil nil t))
  (goto-char (point-max))

  ;; Use punctuation as "no-value" button content
  ;; See https://github.com/zevlg/telega.el/issues/45
  (setq telega-chatbuf--aux-button
        (telega-button--insert 'telega-prompt-aux "!aa!"
          'invisible t))
  (setq telega-chatbuf--prompt-button
        (telega-button--insert 'telega-prompt telega-chat-input-prompt))

  (setq telega-chatbuf--input-marker (point-marker))

  (add-hook 'window-scroll-functions 'telega-chatbuf-scroll nil t)
  (add-hook 'post-command-hook 'telega-chatbuf--post-command nil t)
  (add-hook 'kill-buffer-hook 'telega-chatbuf--killed nil t)
  (when (boundp 'after-focus-change-function)
    (add-function :after (local 'after-focus-change-function)
                  'telega-chatbuf--check-focus-change))

  (setq telega--chat-buffers
        (cl-pushnew (current-buffer) telega--chat-buffers)))

(defun telega-describe-chatbuf ()
  "Show info about chat."
  (interactive)
  (telega-describe-chat telega-chatbuf--chat))

(defun telega-chatbuf-scroll (window display-start)
  "If at the beginning then request for history messages.
Also mark messages as read with `viewMessages'."
  (with-current-buffer (window-buffer window)
    ;; If point moves near the beginning of chatbuf, then
    ;; request for the previous history
    ;; In case if there is less then 1000 chars, then wait while point
    ;; moves to the beginning of the buffer
    (when (and (< display-start 1000) (> (point-max) 1000))
      (telega-chatbuf--load-older-history))

    ;; If point moves near the end of the chatbuf, then request for
    ;; newer history
    (when (and (> display-start (- (point-max) 2000))
               (telega-chatbuf--need-newer-history-p))
      (telega-chatbuf--load-newer-history))

    ;; Mark some messages as read
    ;; Scroll might be triggered in closed chat, so force viewMessages
    (telega-chatbuf--view-visible-messages window display-start 'force)
    ))

(defun telega-chatbuf--set-action (action)
  "Set my chatbuf action to ACTION"
  (when (stringp action)
    (cl-assert (member action '("Typing" "RecordingVideo" "UploadingVideo"
                                "RecordingVoiceNote" "UploadingVoiceNote"
                                "UploadingPhoto" "UploadingDocument"
                                "ChoosingLocation" "ChoosingContact"
                                "StartPlayingGame" "RecordingVideoNote"
                                "UploadingVideoNote" "Cancel")))
    (setq action (list :@type (concat "chatAction" action))))
  ;; NOTE: special case is for `chatActionUploadingVideoNote', it
  ;; might have additional `:progress' argument.  In this case, pass
  ;; it directly as list to `telega-chatbuf--set-action'

  (unless (equal telega-chatbuf--my-action action)
    (let ((cancel-p (eq (telega--tl-type action) 'chatActionCancel)))
      (setq telega-chatbuf--my-action (unless cancel-p action)))

    ;; Update it on server as well
    (telega--sendChatAction telega-chatbuf--chat action)))

(defmacro with-telega-chatbuf-action (action &rest body)
  "Execute BODY setting current action to ACTION.
Recover previous active action after BODY execution."
  (declare (indent 1))
  (let ((actsym (gensym "action")))
    `(let ((,actsym (plist-get telega-chatbuf--my-action :@type)))
       (telega-chatbuf--set-action ,action)
       (unwind-protect
           (progn ,@body)
         (telega-chatbuf--set-action
          (or (and ,actsym (substring ,actsym 10))
              "Cancel"))))))

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
  "Return message currently replying."
  (and (eq (button-get telega-chatbuf--aux-button :inserter)
           'telega-ins--aux-reply-inline)
       (button-get telega-chatbuf--aux-button :value)))

(defun telega-chatbuf--post-command ()
  "Chabuf `post-command-hook' function."
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

  ;; - If at the beginning of chatbuf then request for the history
  ;;   same as in telega-chatbuf-scroll
  (when (= (point) (point-min))
    (telega-chatbuf--load-older-history))

  ;; - If at the bottom of the chatbuf and newer history is not yet
  ;;   loaded, then load it.  Same as in `telega-chatbuf-scroll'
  ;;   Do not load newer history if prompt is active (reply, or forward)
  (when (and (= (point) (point-max))
             (telega-chatbuf--need-newer-history-p))
    (telega-chatbuf--load-newer-history))

  ;; - Finally, when input is probably changed by above operations,
  ;;   update chat's action after command execution.
  (let ((input-p (telega-chatbuf-has-input-p)))
    (cond ((and (not telega-chatbuf--my-action) input-p)
           (telega-chatbuf--set-action "Typing"))
          ((and telega-chatbuf--my-action (not input-p))
           (telega-chatbuf--set-action "Cancel")))

    ;; - If there is active draft_message and input is empty then
    ;;   clear the draf
    (when (and (plist-get telega-chatbuf--chat :draft_message)
               (not input-p))
      (telega--setChatDraftMessage telega-chatbuf--chat)))
  )

(defun telega-chatbuf--name (chat &optional title)
  "Return name for the CHAT buffer.
If TITLE is specified, use it instead of chat's title."
  (let ((brackets (telega-chat-brackets chat)))
    (substring-no-properties
     (concat telega-symbol-telegram
             (or (car brackets) "[")
             (or title (telega-chat-title chat))
             (when-let ((un (telega-tl-str (telega-chat--info chat) :username)))
               (unless (string-empty-p un)
                 (concat "@" un)))
             (or (cadr brackets) "]")))))

(defun telega-chatbuf--join (chat)
  "[JOIN] button has been pressed."
  (cl-assert (eq chat telega-chatbuf--chat))
  (cl-assert (memq (telega-chat--type chat) '(bot channel basicgroup supergroup)))
  (if (eq (telega-chat--type chat) 'bot)
      (telega--sendMessage
       chat (list :@type "inputMessageText"
                  :text (telega--formattedText "/start")))
    ;; join the chat
    (telega--joinChat chat))

  ;; reset the prompt
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (button-put telega-chatbuf--prompt-button
                'invisible nil)
    (goto-char telega-chatbuf--prompt-button)
    (delete-region (point-at-bol) telega-chatbuf--prompt-button)
    (goto-char telega-chatbuf--input-marker)))

(defun telega-chatbuf--prompt-reset ()
  "Reset prompt to initial state in chat buffer."
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        (prompt-invisible-p
         (button-get telega-chatbuf--prompt-button 'invisible)))
    (telega-save-excursion
      (unless (button-get telega-chatbuf--aux-button 'invisible)
        (telega-button--update-value
         telega-chatbuf--aux-button "!aa!"
         :inserter 'telega-ins
         'invisible t))

      (telega-button--update-value
       telega-chatbuf--prompt-button telega-chat-input-prompt
       'invisible prompt-invisible-p))))

(defun telega-chatbuf--input-draft (draft-msg &optional force)
  "Update chatbuf's input to display draft message DRAFT-MSG.
If FORCE is specified, then set input draft unconditionally,
otherwise set draft only if current input is also draft."
  (let ((reply-msg-id (plist-get draft-msg :reply_to_message_id)))
    (if (and reply-msg-id (not (zerop reply-msg-id)))
        (telega-msg-reply
         (telega-msg--get (plist-get telega-chatbuf--chat :id) reply-msg-id))
      ;; Reset only if replying, but `:reply_to_message_id' is not
      ;; specified, otherwise keep the aux, for example editing
      (when (telega-chatbuf--replying-msg)
        (telega-chatbuf--prompt-reset)))

    ;; NOTE: update draft only if current chatbuf input is marked as
    ;; draft, otherwise draft update may change current input
    (when (or force (telega-chatbuf--input-draft-p))
      (telega-save-cursor
        (telega-chatbuf--input-delete)
        (goto-char telega-chatbuf--input-marker)
        (telega-ins--with-props '(:draft-input-p t)
          (telega-ins--text
           (telega--tl-get draft-msg :input_message_text :text)))))))

(defun telega-chatbuf--get-create (chat)
  "Get or create chat buffer for the CHAT."
  (let ((bufname (telega-chatbuf--name chat)))
    (or (get-buffer bufname)

        (with-current-buffer (generate-new-buffer bufname)
          (let ((telega-chat--preparing-buffer-for chat))
            (telega-chat-mode))
          (telega-chatbuf--footer-redisplay)
          (telega-chatbuf-mode-line-update)

          ;; Asynchronously update pinned message, if any
          (unless (zerop (plist-get chat :pinned_message_id))
            (telega-chat--update-pinned-message chat))
          ;; Asynchronously update reply markup message
          (unless (zerop (plist-get chat :reply_markup_message_id))
            (telega-chat--update-reply-markup-message chat))

          ;; If me is not member of this chat, then show [JOIN/START]
          ;; button instead of the prompt
          ;;  - For channels/groups show JOIN button
          ;;  - For bots show START button
          (unless (telega-filter-chats 'me-is-member (list chat))
            (let ((inhibit-read-only t)
                  (buffer-undo-list t)
                  (chat-type (telega-chat--type chat)))
              (when (memq chat-type '(bot channel basicgroup supergroup))
                (button-put telega-chatbuf--prompt-button 'invisible t)
                (goto-char telega-chatbuf--prompt-button)
                (save-excursion
                  (telega-ins--button (if (eq chat-type 'bot) "START" "JOIN")
                    :value chat :action 'telega-chatbuf--join)))))

          ;; Show the draft message if any, see
          ;; https://github.com/zevlg/telega.el/issues/80
          (when-let ((draft-msg (plist-get chat :draft_message)))
            (telega-chatbuf--input-draft draft-msg 'force))

          ;; Start from last read message
          ;; see https://github.com/zevlg/telega.el/issues/48
          (let ((last-read-msg-id (plist-get chat :last_read_inbox_message_id)))
            (telega-chat--load-history
                chat last-read-msg-id (- (/ telega-chat-history-limit 2)) nil
              (lambda ()
                (telega-chat--goto-msg0 chat last-read-msg-id)
                (condition-case nil
                    (telega-button-forward 1 'telega-msg)
                  (user-error
                   ;; No more buttons, jump to input
                   (goto-char (point-max))))
                ;; NOTE: view all visible messages, see
                ;; https://t.me/emacs_telega/4731
                (when-let ((chat-win (get-buffer-window (current-buffer))))
                  (telega-chatbuf--view-visible-messages chat-win)))))

          ;; Openning chat may affect filtering, see `opened' filter
          (telega-root--chat-update chat)

          (current-buffer)))))

(defun telega-chatbuf--need-newer-history-p ()
  "Return non-nil if newer history can be loaded."
  (and (not (telega-chatbuf--last-msg-loaded-p))
       (button-get telega-chatbuf--aux-button 'invisible)))

(defun telega-chatbuf-mode-line-unread ()
  "Format unread/mentions string for chat buffer modeline."
  (let* ((unread-count (plist-get telega-chatbuf--chat :unread_count))
         (mention-count (plist-get telega-chatbuf--chat :unread_mention_count))
         (brackets (or (> unread-count 0) (> mention-count 0))))
    (concat
     (when brackets " (")
     (when (> unread-count 0)
       (propertize (concat "unread:" (number-to-string unread-count))
                   'face 'bold
                   'local-map
                   (eval-when-compile
                     (make-mode-line-mouse-map 'mouse-1 'telega-chatbuf-read-all))
                   'mouse-face 'mode-line-highlight
                   'help-echo "mouse-1: Read all messages"))
     (when (> mention-count 0)
       (concat
        (when (> unread-count 0) " ")
        (propertize (concat "@" (number-to-string mention-count))
                    'face 'telega-mention-count
                    'local-map
                    (eval-when-compile
                      (make-mode-line-mouse-map 'mouse-1 'telega-chatbuf-next-mention))
                    'mouse-face 'mode-line-highlight
                    'help-echo "mouse-1: Goto next mention")))
     (when brackets ")"))))

(defun telega-chatbuf-mode-line-members (&optional use-icons-p)
  "Format members string for chat buffer modeline.
If ICONS-P is non-nil, then use icons for members count."
  (let ((member-count
         (or (plist-get (telega-chat--info telega-chatbuf--chat) :member_count) 0))
        (online-count (or (plist-get telega-chatbuf--chat :x-online-count) 0)))
    (unless (zerop member-count)
      (concat " [" (number-to-string member-count)
              (if use-icons-p
                  (propertize telega-symbol-contact 'face 'shadow)
                " members")
              (unless (zerop online-count)
                (concat ", " (number-to-string online-count)
                        (if use-icons-p
                            telega-symbol-online-status
                          " online")))
              "]"))))

(defun telega-chatbuf-mode-line-pinned-msg (&optional max-width)
  "Format pinned message string for chat buffer modeline."
  (when-let ((pin-msg (telega-chat-pinned-msg telega-chatbuf--chat 'locally)))
    (unless (plist-get pin-msg :telega-is-deleted-message)
      (telega-ins--as-string
       (telega-ins " [")
       (telega-ins--with-attrs
           (list :max (or max-width 15) :align 'left :elide t)
         (telega-ins--with-props
             (list 'local-map (eval-when-compile
                                (make-mode-line-mouse-map
                                 'mouse-1 'telega-chatbuf-goto-pin-message))
                   'mouse-face 'mode-line-highlight
                   'help-echo "mouse-1: Goto pinned message")
           (telega-ins telega-symbol-pin)
           (let ((telega-emoji-use-images nil))
             ;; NOTE: avoid using images for emojis, because modeline
             ;; height might differ from default height, and modeline
             ;; will increase its height
             (telega-ins--content-one-line pin-msg))))
       (telega-ins "]")))))

(defun telega-chatbuf-mode-line-update ()
  "Update `mode-line-buffer-identification' for the CHAT buffer."
  (setq mode-line-buffer-identification
        (list (propertized-buffer-identification "%b")
              ;; Online status
              (when (and (eq (telega-chat--type telega-chatbuf--chat) 'private)
                         (not (telega-me-p telega-chatbuf--chat))
                         (string= "Online"
                                  (telega-user--seen
                                   (telega-chat--user telega-chatbuf--chat))))
                telega-symbol-online-status)
              (format-mode-line telega-chat-mode-line-format nil nil
                                (current-buffer))))
  (force-mode-line-update))

(defun telega-chatbuf--input-idx-valid-p (idx)
  "Return non-nil if input history position IDX is valid."
  (and (>= idx 0) (< idx (ring-length telega-chatbuf--input-ring))))

(defun telega-chatbuf-input-goto (idx)
  "Put input history item of the absolute history position IDX."
  ;; Save any pending input
  (unless telega-chatbuf--input-idx
    (setq telega-chatbuf--input-pending (telega-chatbuf-input-string)))

  (setq telega-chatbuf--input-idx idx)
  (telega-chatbuf--input-delete)
  (goto-char (point-max))

  (if (and idx (not (ring-empty-p telega-chatbuf--input-ring)))
      (insert (ring-ref telega-chatbuf--input-ring idx))

    ;; Restore pending input
    (when telega-chatbuf--input-pending
      (insert telega-chatbuf--input-pending))))

(defun telega-chatbuf-input-restore ()
  "Restore pending input."
  (when telega-chatbuf--input-idx
    (telega-chatbuf--input-delete)
    (when telega-chatbuf--input-pending
      (goto-char (point-max))
      (insert telega-chatbuf--input-pending))
    (setq telega-chatbuf--input-idx nil)))

(defun telega-chatbuf-input-prev (n)
  "Goto N previous items in chat input history."
  (interactive "p")
  (let ((idx (if telega-chatbuf--input-idx
                 (+ telega-chatbuf--input-idx n)
               (1- n))))
    ;; clamp IDX
    (cond ((< idx 0)
           (setq idx nil)) ;; restory pending input
          ((>= idx (ring-length telega-chatbuf--input-ring))
           (setq idx (1- (ring-length telega-chatbuf--input-ring)))))
    (telega-chatbuf-input-goto idx)))

(defun telega-chatbuf-input-next (n)
  "Goto next N's item in chat input history."
  (interactive "p")
  (when telega-chatbuf--input-idx
    (telega-chatbuf-input-prev (- n))))

(defun telega-chatbuf-input-match (regexp forward-p)
  "Move point to previous Goto previous match."
  (let ((found (funcall (if forward-p 're-search-forward 're-search-backward)
                        regexp
                        (if forward-p (point-max) telega-chatbuf--input-marker)
                        t)))
    (unless found
      (let* ((step (if forward-p -1 1))
             (idx (if telega-chatbuf--input-idx
                      (+ telega-chatbuf--input-idx step)
                    0)))
        (while (and (telega-chatbuf--input-idx-valid-p idx)
                    (not (string-match
                          regexp (ring-ref telega-chatbuf--input-ring idx))))
          (cl-incf idx step))
        (when (telega-chatbuf--input-idx-valid-p idx)
          (telega-chatbuf-input-goto idx)
          (when forward-p
            (goto-char telega-chatbuf--input-marker))
          (telega-chatbuf-input-match regexp forward-p))))))

(defun telega-chatbuf--minibuf-post-command ()
  "Function to search chatbuf history input."
  (cl-assert (eq major-mode 'minibuffer-inactive-mode))
  (let* ((start (minibuffer-prompt-end))
         (end (point))
         (regexp (buffer-substring start end)))
    (unless (string= telega-minibuffer--string regexp)
      (setq telega-minibuffer--string regexp)
      (with-telega-chatbuf telega-minibuffer--chat
        (telega-chatbuf-input-restore)
        (telega-chatbuf-input-match regexp nil)))))

(defun telega-chatbuf--input-search-prev (&optional forward-p)
  "For `C-r' in minibuffer."
  (interactive)
  (cl-assert (eq major-mode 'minibuffer-inactive-mode))
  (let ((regexp telega-minibuffer--string))
    (with-telega-chatbuf telega-minibuffer--chat
      (telega-chatbuf-input-match regexp forward-p))))

(defun telega-chatbuf--input-search-next ()
  "For `C-s' in minibuffer."
  (interactive)
  (telega-chatbuf--input-search-prev 'forward))

(defun telega-chatbuf--input-search-cancel ()
  (interactive)
  (cl-assert (eq major-mode 'minibuffer-inactive-mode))
  (delete-region (minibuffer-prompt-end) (point))
  (exit-minibuffer))

(defvar telega-chatbuf--input-search-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") 'telega-chatbuf--input-search-cancel)
    (define-key map (kbd "C-r") 'telega-chatbuf--input-search-prev)
    (define-key map (kbd "C-s") 'telega-chatbuf--input-search-next)
    map))

(defun telega-chatbuf-input-search ()
  "Search for REGEX in chat input history."
  (interactive)
  (let* ((saved-input-idx telega-chatbuf--input-idx)
         (telega-minibuffer--string "")
         (telega-minibuffer--chat telega-chatbuf--chat)
         (regexp (minibuffer-with-setup-hook
                     (lambda ()
                       (add-hook 'post-command-hook
                                 'telega-chatbuf--minibuf-post-command t t))
                   (read-from-minibuffer "History input search: " nil
                                         telega-chatbuf--input-search-map))))
    (when (string-empty-p regexp)
      ;; Restore input if canceled
      (telega-chatbuf-input-goto saved-input-idx))
    ))

(defun telega-chatbuf--redisplay-node (node)
  "Redisplay NODE in chatbuffer.
Try to keep point at its position."
  (let* ((chat-win (get-buffer-window))
         (wstart (and chat-win (window-start chat-win)))
         (pos (point))
         (msg-button (button-at pos)))
    (unwind-protect
        ;; NOTE: we save cursor position only if point is currently
        ;; inside the message we are redisplaying, otherwise simple
        ;; `telega-save-excursion' will work
        (if (and msg-button
                 (eq (button-get msg-button :value)
                     (ewoc--node-data node))
                 (>= pos (button-start msg-button))
                 (<= pos (button-end msg-button)))
            (telega-save-cursor
              (ewoc-invalidate telega-chatbuf--ewoc node))
          (telega-save-excursion
            (ewoc-invalidate telega-chatbuf--ewoc node)))

      (when chat-win
        (set-window-start chat-win wstart 'noforce)))))

(defun telega-chatbuf--prepend-messages (messages)
  "Insert MESSAGES at the beginning of the chat buffer.
First message in MESSAGE will be first message at the beginning."
  (with-telega-deferred-events
    (let ((node (ewoc--header telega-chatbuf--ewoc)))
      (seq-doseq (msg messages)
        (unless (telega-msg-ignored-p msg)
          (setq node (ewoc-enter-after telega-chatbuf--ewoc node msg)))))))

(defun telega-chatbuf--append-messages (messages)
  "Insert MESSAGES at the end of the chat buffer."
  (with-telega-deferred-events
    (seq-doseq (msg messages)
      (unless (telega-msg-ignored-p msg)
        (ewoc-enter-last telega-chatbuf--ewoc msg)))))

(defun telega-chatbuf--append-message (msg)
  "Insert message MSG as last in chat buffer.
Return newly created ewoc node."
  (unless (telega-msg-ignored-p msg)
    (with-telega-deferred-events
      ;; Track the uploading progress
      ;; see: https://github.com/zevlg/telega.el/issues/60
      (telega-msg--track-file-uploading-progress msg)
      (ewoc-enter-last telega-chatbuf--ewoc msg))))

(defun telega-chatbuf--node-by-msg-id (msg-id)
  "In current chatbuffer find message button with MSG-ID."
  ;; TODO: maybe do binary search on buffer position (getting message
  ;; as `telega-msg-at'), since message ids grows monotonically
  ;; Or maybe search from last node
  (telega-ewoc--find telega-chatbuf--ewoc msg-id '= (telega--tl-prop :id)))

(defun telega-chatbuf--msg (msg-id &optional with-node)
  "In current chatbuf return message by MSG-ID.
If WITH-NODE is non-nil then return also corresponding ewoc node.
Return nil if message not found.
Return message if WITH-NODE is nil.
Return list, where first element is the message and second is the
ewoc node if WITH-NODE is non-nil."
  (let* ((cached-msg (gethash msg-id telega-chatbuf--messages))
         (node (if (or (null cached-msg) with-node)
                   (telega-chatbuf--node-by-msg-id msg-id)))
         (msg (or cached-msg (when node (ewoc--node-data node)))))
    (cl-assert (or (null cached-msg) (null msg) (eq msg cached-msg)))
    (if with-node
        (list msg node)
      msg)))

(defun telega-chatbuf--next-voice-msg (msg)
  "Search for voice message next to MSG.
Return `nil' if there is no such message."
  (with-telega-chatbuf (telega-msg-chat msg)
    (let* ((mnode (telega-chatbuf--node-by-msg-id (plist-get msg :id)))
           (mnode1 (ewoc-next telega-chatbuf--ewoc mnode))
           (nnode (and mnode1
                       (telega-ewoc--find-if
                        telega-chatbuf--ewoc
                        (lambda (content)
                          (eq (telega--tl-type content) 'messageVoiceNote))
                        (telega--tl-prop :content)
                        mnode1))))
      (when nnode
        (ewoc--node-data nnode)))))

(defun telega-chatbuf--visible-messages (window &optional start-point)
  "Return list of messages visible in chat buffer WINDOW.
If START-POINT is specified, then start from this point.
Otherwise start from WINDOW's `window-start'."
  (save-excursion
    (goto-char (or start-point (window-start window)))
    (let (msg messages)
      (while (and (setq msg (telega-msg-at (point)))
                  (pos-visible-in-window-p (point) window))
        (push msg messages)
        (telega-button-forward 1))
      messages)))

(defun telega-chatbuf--read-outbox (old-last-read-outbox-msgid)
  "Redisplay chat messages affected by read-outbox change.
OLD-LAST-READ-OUTBOX-MSGID is old value for chat's `:last_read_outbox_message_id'."
  (let ((node (ewoc--footer telega-chatbuf--ewoc)))
    (while (and (setq node (ewoc-prev telega-chatbuf--ewoc node))
                (< old-last-read-outbox-msgid
                   (plist-get (ewoc-data node) :id)))
      (telega-chatbuf--redisplay-node node))))

(defalias 'telega--on-message 'ignore)

(defun telega--on-updateNewMessage (event)
  "A new message was received; can also be an outgoing message."
  (let ((new-msg (plist-get event :message)))
    (run-hook-with-args 'telega-chat-pre-message-hook new-msg)

    (with-telega-chatbuf (telega-msg-chat new-msg)
      (telega-chatbuf--cache-msg new-msg)

      (when (telega-chatbuf--last-msg-loaded-p new-msg)
        (let ((node (telega-chatbuf--append-message new-msg)))
          (when node
            (when (and telega-use-tracking
                       (not (plist-get new-msg :is_outgoing))
                       (not (telega-msg-seen-p new-msg telega-chatbuf--chat)))
              (tracking-add-buffer (current-buffer)))

            ;; If message is visibible in some window, then mark it as read
            ;; see https://github.com/zevlg/telega.el/issues/4
            (when (telega-msg-observable-p new-msg telega-chatbuf--chat node)
              (telega--viewMessages telega-chatbuf--chat (list new-msg)))))))

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
                  (ewoc-enter-before telega-chatbuf--ewoc before-node new-msg)
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
  (with-telega-chatbuf (telega-chat-get (plist-get event :chat_id))
    (cl-destructuring-bind (msg node)
        (telega-chatbuf--msg (plist-get event :message_id) 'with-node)
      (when msg
        (plist-put msg :content (plist-get event :new_content))
        (when node
          (telega-chatbuf--redisplay-node node))))))

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
      (when-let ((aux-msg (or (telega-chatbuf--replying-msg)
                              (telega-chatbuf--editing-msg)
                              (telega-chatbuf--forwarding-msg))))
        (when (eq (plist-get msg :id) (plist-get aux-msg :id))
          (cl-assert (not (button-get telega-chatbuf--aux-button 'invisible)))
          (telega-save-excursion
            (telega-button--update-value
             telega-chatbuf--aux-button msg
             :inserter (button-get telega-chatbuf--aux-button :inserter)))))
      )))

(defun telega--on-updateMessageViews (event)
  "Number of message views has been updated."
  (with-telega-chatbuf (telega-chat-get (plist-get event :chat_id))
    (cl-destructuring-bind (msg node)
        (telega-chatbuf--msg (plist-get event :message_id) 'with-node)
      (plist-put msg :views (plist-get event :views))
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
            (if telega-chat-show-deleted-messages
                (telega-msg-redisplay msg node)
              (ewoc-delete telega-chatbuf--ewoc node)))
          (when (eq msg-id (plist-get telega-chatbuf--chat :pinned_message_id))
            (telega-chat--update-pinned-message telega-chatbuf--chat 'offline)))
        ))))

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
      (telega-root--chat-update chat)
      (with-telega-chatbuf chat
        ;; If action by me, update `telega-chatbuf--my-action' as well
        (when (eq user-id telega--me-id)
          (setq telega-chatbuf--my-action (unless cancel-p action)))

        (telega-chatbuf--footer-redisplay)))
    ))

(defun telega--getChatHistory (chat from-msg-id offset
                                    &optional limit only-local callback)
  "Returns messages in a chat.
The messages are returned in a reverse chronological order."
  (declare (indent 5))
  (telega-server--call
   (list :@type "getChatHistory"
         :chat_id (plist-get chat :id)
         :from_message_id from-msg-id
         :offset offset
         :limit (or limit telega-chat-history-limit)
         :only_local (or only-local :false))
   callback))

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
      (telega-server--callback-put telega-chatbuf--history-loading 'ignore)
      (setq telega-chatbuf--history-loading nil))

    (unless telega-chatbuf--history-loading
      (unless from-msg-id
        (setq from-msg-id (plist-get (telega-chatbuf--first-msg) :id)
              offset 0))
      (unless from-msg-id
        (setq from-msg-id (plist-get (plist-get chat :last_message) :id)
              offset -1))

      (when from-msg-id
        ;; Asynchronously load chat history
        (setq telega-chatbuf--history-loading
              (telega--getChatHistory
                  chat from-msg-id offset
                  (or limit telega-chat-history-limit) nil
               ;; The callback
               (lambda (history)
                 (with-telega-chatbuf chat
                   (telega-save-excursion
                     (telega-chatbuf--prepend-messages
                      (nreverse (plist-get history :messages))))
                   (setq telega-chatbuf--history-loading nil)
                   (telega-chatbuf--footer-redisplay)
                   (when callback
                     (funcall callback))))))
        (telega-chatbuf--footer-redisplay)
        ))))

(defun telega-chatbuf--load-older-history ()
  "In chat buffer load older messages."
  (telega-chat--load-history telega-chatbuf--chat))

(defun telega-chatbuf--load-newer-history ()
  "In chat buffer load newer messages."
  (with-telega-chatbuf telega-chatbuf--chat
    (unless (or telega-chatbuf--history-loading
                (not (telega-chatbuf--last-msg)))
      (let ((chat telega-chatbuf--chat)
            (from-msg-id (plist-get (telega-chatbuf--last-msg) :id)))
        (setq telega-chatbuf--history-loading
              (telega--getChatHistory
                  chat from-msg-id (- 1 telega-chat-history-limit)
                  telega-chat-history-limit nil
               ;; The callback
               (lambda (history)
                 (let ((rmsgs (append
                               (nreverse (plist-get history :messages)) nil)))
                   ;; Strip messages till FROM-MSG-ID
                   (while (and rmsgs
                               (<= (plist-get (car rmsgs) :id) from-msg-id))
                     (setq rmsgs (cdr rmsgs)))
                   (with-telega-chatbuf chat
                     (telega-save-cursor
                       (telega-chatbuf--append-messages rmsgs)
                       (setq telega-chatbuf--history-loading nil)
                       (telega-chatbuf--footer-redisplay)))))))
        (telega-chatbuf--footer-redisplay)
        ))))

(defun telega-chatbuf-cancel-aux (&optional arg)
  "Cancel current aux prompt.
If prefix ARG is giver, also delete input."
  (interactive "P")
  (telega-chatbuf--prompt-reset)
  (when arg
    (telega-chatbuf--input-delete)))

(defun telega-chatbuf--help-cancel-keys (what)
  "Show help about canceling reply/edit/fwd in echo area."
  (let ((cancel-keys (where-is-internal
                      'telega-chatbuf-cancel-aux telega-chat-mode-map)))
    (message "%s to cancel %s"
             (mapconcat #'key-description cancel-keys ", ") what)))

(defun telega--sendMessage (chat imc &optional reply-to-msg disable-notify
                                 from-background reply-markup callback)
  "Send the message content represented by IMC to CHAT.
If CALLBACK is specified, then call it with one argument - new
message uppon message is created."
  ;; We catch new message with `telega--on-updateNewMessage', so
  ;; ignore result returned from `sendMessage'
  (telega-server--call
   (nconc (list :@type "sendMessage"
                :chat_id (plist-get chat :id)
                :disable_notification (or disable-notify :false)
                :input_message_content imc)
          (when reply-to-msg
            (list :reply_to_message_id (plist-get reply-to-msg :id)))
          (when from-background
            (list :from_background t))
          (when reply-markup
            (list :reply_markup reply-markup)))
   (or callback 'ignore)))

(defun telega--sendMessageAlbum (chat imcs &optional reply-to-msg disable-notify
                                      from-background callback)
  "Send IMCS as media album.
If CALLBACK is specified, then call it with one argument - new
message uppon message is created."
  (let ((tsm (list :@type "sendMessageAlbum"
                   :chat_id (plist-get chat :id)
                   :disable_notification (or disable-notify :false)
                   :input_message_contents (apply 'vector imcs))))
    (when reply-to-msg
      (setq tsm (plist-put tsm :reply_to_message_id
                           (plist-get reply-to-msg :id))))
    (when from-background
      (setq tsm (plist-put tsm :from_background t)))
    (telega-server--call tsm (or callback 'ignore))))

(defun telega--sendInlineQueryResultMessage (chat imc &optional reply-to-msg
                                                  disable-notify from-background
                                                  )
  "Send IMC as inline query result from bot.
If CALLBACK is specified, then call it with one argument - new
message uppon message is created."
  (telega-server--send
   (nconc (list :@type "sendInlineQueryResultMessage"
                :chat_id (plist-get chat :id)
                :disable_notification (or disable-notify :false)
                :query_id (plist-get imc :query-id)
                :result_id (plist-get imc :result-id))
          (when reply-to-msg
            (list :reply_to_message_id (plist-get reply-to-msg :id)))
          (when from-background
            (list :from_background t))
          (when (plist-get imc :hide-via-bot)
            (list :hide_via_bot t)))))

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
   (nconc (list :@type "editMessageText"
                :chat_id (plist-get chat :id)
                :message_id (plist-get msg :id)
                :input_message_content imc)
          (when reply-markup
            (list :reply_markup reply-markup)))))

(defun telega--editMessageMedia (chat msg imc &optional reply-markup)
  (telega-server--send
   (nconc (list :@type "editMessageMedia"
                :chat_id (plist-get chat :id)
                :message_id (plist-get msg :id)
                :input_message_content imc)
          (when reply-markup
            (list :reply_markup reply-markup)))))

(defun telega--editMessageCaption (chat msg caption &optional reply-markup)
  (telega-server--send
   (nconc (list :@type "editMessageCaption"
                :chat_id (plist-get chat :id)
                :message_id (plist-get msg :id)
                :caption caption)
          (when reply-markup
            (list :reply_markup reply-markup)))))

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

(defun telega-chatbuf-input-send (arg)
  "Send current input to the chat.
Prefix ARG, inverses `telega-chat-use-markdown-formatting' setting."
  (interactive "P")
  ;; Send the input
  (let ((input (telega-chatbuf-input-string))
        (imcs (telega-chatbuf--input-imcs
               (if telega-chat-use-markdown-formatting
                   (not arg)
                 arg)))
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

      ;; If all IMCS are photos and videos then send them as album
      ;; otherwise send IMCS as separate messages
      ;; NOTE: cl-every returns `t' on empty list
      (if (and (> (length imcs) 1)
               (cl-every (lambda (imc)
                           (memq (telega--tl-type imc)
                                 '(inputMessagePhoto inputMessageVideo)))
                         imcs))
          (telega--sendMessageAlbum telega-chatbuf--chat imcs replying-msg)
        (dolist (imc imcs)
          (if (eq (telega--tl-type imc) 'telegaInlineQuery)
              (telega--sendInlineQueryResultMessage
               telega-chatbuf--chat imc replying-msg)
            (telega--sendMessage telega-chatbuf--chat imc replying-msg)))))

    (when forwarding-msg
      (telega--forwardMessage telega-chatbuf--chat forwarding-msg))

    ;; Recover prompt to initial state
    (telega-chatbuf--input-delete)
    (telega-chatbuf--prompt-reset)

    ;; Save input to history
    (unless (string-empty-p input)
      (ring-insert telega-chatbuf--input-ring input)
      (setq telega-chatbuf--input-idx nil
            telega-chatbuf--input-pending nil))))

(defun telega-chatbuf-input-insert (imc)
  "Insert input content defined by IMC into current input.
IMC might be a plain string or attachement specification."
  ;; Check that point is in input area, otherwise move to the end
  (when (< (point) telega-chatbuf--input-marker)
    (goto-char (point-max)))

  (when (get-text-property (point) 'telega-attach)
    (telega-ins " "))
  (if (stringp imc)
      (telega-ins imc)
    ;; NOTE: Put special properties `attach-open-bracket' and
    ;; `attach-close-bracket' to be used by
    ;; `telega-chatbuf--post-command' to determine if part of
    ;; attachement is deleted by `delete-char' or `backward-delete'
    (telega-ins--with-props
        `(telega-attach ,imc face telega-chat-input-attachment)
      (telega-ins--with-props '(cursor-intangible t)
        (when (telega-ins-prefix
                  (propertize (car telega-symbol-attach-brackets)
                              'attach-open-bracket t)
                (telega-ins--input-content-one-line imc))
          (telega-ins (cdr telega-symbol-attach-brackets))))
      (telega-ins--with-props '(attach-close-bracket t rear-nonsticky t)
        (telega-ins " ")))))

(defun telega-chatbuf-input-has-attaches-p ()
  "Return non-nil if chatbuf's input has some attaches."
  (let ((attaches (telega--split-by-text-prop
                   (telega-chatbuf-input-string) 'telega-attach)))
    (not (and (= (length attaches) 1)
              (not (get-text-property 0 'telega-attach (car attaches)))))))

(defun telega-chatbuf-history-beginning ()
  "Jump to the chat creation beginning."
  ;; See https://github.com/tdlib/td/issues/195
  (interactive)
  (telega-ewoc--clean telega-chatbuf--ewoc)
  (telega-chat--load-history
      telega-chatbuf--chat 10 (- telega-chat-history-limit))
  (goto-char (point-min)))

(defun telega-chatbuf-read-all ()
  "Read all messages in chat buffer."
  (interactive)
  (unless (telega-chatbuf--last-msg-loaded-p)
    ;; Need to load most recent history
    (telega-ewoc--clean telega-chatbuf--ewoc)
    (telega-chatbuf--load-older-history))

  (goto-char (point-max)))

(defun telega-chatbuf-next-mention ()
  "Goto next mention in chat buffer."
  (interactive)
  ;; TODO:
  ;; - check `:unread_mention_count' for zerop
  ;; - searchChatMessages with :filter searchMessagesFilterUnreadMention
  ;; - Goto first found message
  (user-error "`telega-chatbuf-next-mention' not yet implemented")
  )

(defun telega-chatbuf-goto-pin-message ()
  "Goto pinned message for the chatbuffer."
  (interactive)
  (let ((pinned-msg-id (plist-get telega-chatbuf--chat :pinned_message_id)))
    (unless (zerop pinned-msg-id)
      (telega-chat--goto-msg telega-chatbuf--chat pinned-msg-id 'highlight))))

;;; Attaching stuff to the input
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
       (cl-assert user)
       (list
        (list :@type "Contact"
              :phone_number (concat "+" (plist-get user :phone_number))
              :first_name (telega-tl-str user :first_name)
              :last_name (telega-tl-str user :last_name)
              :vcard ""
              :user_id (plist-get user :id))))))

    (telega-chatbuf-input-insert
     (list :@type "inputMessageContact"
           :contact contact)))

(defun telega-chatbuf--gen-input-file (filename &optional file-type preview-p)
  "Generate InputFile using FILENAME.
If PREVIEW-P is non-nil, then generate preview image."
  (setq filename (expand-file-name filename))
  (let ((preview (when (and preview-p (> (telega-chars-xheight 1) 1))
                   (create-image filename
                                 (when (fboundp 'imagemagick-types) 'imagemagick)
                                 nil
                                 :scale 1.0 :ascent 'center
                                 :height (telega-chars-xheight 1))))
        (ifile (if telega-chat-upload-attaches-ahead
                   (let ((ufile (telega-file--upload filename file-type 16)))
                     (list "inputFileId" :id (plist-get ufile :id)))
                 (list "inputFileLocal" :path filename))))
    (nconc (list :@type (propertize (car ifile) 'telega-preview preview))
           (cdr ifile))))

(defun telega-chatbuf-attach-file (filename &optional preview-p)
  "Attach FILE as document to the current input."
  (interactive (list (read-file-name "Attach file: ")))
  (let ((ifile (telega-chatbuf--gen-input-file filename 'Document preview-p)))
    (telega-chatbuf-input-insert
     (list :@type "inputMessageDocument"
           :document ifile))))

(defun telega-chatbuf-attach-photo (filename)
  "Attach FILENAME as photo to the current input."
  (interactive (list (read-file-name "Photo: ")))
  (let ((ifile (telega-chatbuf--gen-input-file filename 'Photo t))
        (img-size (image-size
                   (create-image filename (when (fboundp 'imagemagick-types) 'imagemagick) nil :scale 1.0) t)))
    (telega-chatbuf-input-insert
     (list :@type "inputMessagePhoto"
           :photo ifile
           :width (car img-size)
           :height (cdr img-size)))))

(defun telega-chatbuf-attach-video (filename)
  "Attach FILENAME as video to the current input."
  (interactive (list (read-file-name "Video: ")))
  (let ((ifile (telega-chatbuf--gen-input-file filename 'Video)))
    (telega-chatbuf-input-insert
     (list :@type "inputMessageVideo"
           :video ifile))))

(defun telega-chatbuf-attach-audio (filename)
  "Attach FILENAME as audio to the current input."
  (interactive (list (read-file-name "Audio: ")))
  (let ((ifile (telega-chatbuf--gen-input-file filename 'Audio)))
    (telega-chatbuf-input-insert
     (list :@type "inputMessageAudio"
           :audio ifile))))

(defun telega-chatbuf-attach-note-video (filename)
  "Attach FILE as document to the current input."
  (interactive (list (read-file-name "Video Note: ")))
  ;; TODO: start video note generation process
  ;; see https://github.com/tdlib/td/issues/126
  (let ((ifile (telega-chatbuf--gen-input-file filename 'VideoNote)))
    (telega-chatbuf-input-insert
     (list :@type "inputMessageVideoNote"
           :video_note ifile))))

(defun telega-chatbuf-attach-note-voice (filename)
  "Attach FILE as document to the current input."
  (interactive (list (read-file-name "Voice Note: ")))
  ;; TODO: start voice note generation process
  ;; see https://github.com/tdlib/td/issues/126
  (let ((ifile (telega-chatbuf--gen-input-file filename 'VoiceNote)))
    (telega-chatbuf-input-insert
     (list :@type "inputMessageVoiceNote"
           :voice_note ifile))))

(defun telega-chatbuf--attach-tmp-photo (tmpfile &optional doc-p)
  "Attach temporary photo file TMPFILE.
If DOC-P is non-nil, then attach it as document."
  (if doc-p
      (telega-chatbuf-attach-file tmpfile t)
    (telega-chatbuf-attach-photo tmpfile)))

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

(defun telega-chatbuf-attach-screenshot (&optional n chat)
  "Attach screenshot to the input.
If numeric prefix arg is given, then take screenshot in N seconds.
If `C-u' prefix arg is given, then take screenshot of the screen area.
Multiple `C-u' increases delay before taking screenshot of the area."
  (interactive (list (or current-prefix-arg 1) telega-chatbuf--chat))

  ;; NOTE: use float N value as special, to make screenshot of the
  ;; area, `log' returns float
  (when (listp n)
    (setq n (log (car n) 4)))

  (if (> n 0)
      (progn
        (message "Telega: taking screenshot in %d seconds" n)
        (run-with-timer 1 nil 'telega-chatbuf-attach-screenshot (1- n) chat))

    ;; Make a screenshot
    (message nil)
    (let* ((temporary-file-directory telega-temp-dir)
           (tmpfile (telega-temp-name "screenshot" ".png")))
      (funcall telega-screenshot-function tmpfile (floatp n))

      ;; Switch back to chat buffer in case it has been changed
      (telega-chat--pop-to-buffer chat)
      (telega-chatbuf--attach-tmp-photo tmpfile))))

(defun telega-chatbuf-attach-member (user)
  "Add USER to the chat members."
  (interactive (list (telega-completing-read-user "Add member: ")))
  (cl-assert user)
  (telega-chat-add-member telega-chatbuf--chat user))

(defun telega-chatbuf-sticker-insert (sticker)
  "Attach STICKER to the input."
  (let ((thumb (plist-get sticker :thumbnail))
        (preview (telega-sticker--create-image sticker)))
    ;; Scale down preview to single char
    (plist-put (cdr preview) :scale (/ 1.0 (car telega-sticker-size)))

    (telega-chatbuf-input-insert
     (list :@type "inputMessageSticker"
           :width (plist-get sticker :width)
           :height (plist-get sticker :height)
           ;; Use remote thumbnail and sticker files
           :thumbnail (list :@type "inputThumbnail"
                            :width (plist-get thumb :width)
                            :height (plist-get thumb :height)
                            :thumbnail (list :@type "inputFileId"
                                             :id (telega--tl-get thumb :photo :id)))
           ;; NOTE: 'telega-preview used in `telega-ins--input-file'
           ;; to insert document/photo/sticker preview
           :sticker (list :@type (propertize "inputFileId" 'telega-preview preview)
                          :id (telega--tl-get sticker :sticker :id))
           ))
    ))

(defun telega-chatbuf-animation-insert (animation)
  "Attach ANIMATION to the input."
  (let ((thumb (plist-get animation :thumbnail))
        (preview (telega-animation--create-image animation)))
    ;; Scale down preview to single char
    (plist-put (cdr preview) :scale (/ 1.0 telega-animation-height))

    (telega-chatbuf-input-insert
     (list :@type "inputMessageAnimation"
           :width (plist-get animation :width)
           :height (plist-get animation :height)
           :duration (plist-get animation :duration)
           ;; Use remote thumbnail and animation files
           :thumbnail (list :@type "inputThumbnail"
                            :width (plist-get thumb :width)
                            :height (plist-get thumb :height)
                            :thumbnail (list :@type "inputFileId"
                                             :id (telega--tl-get thumb :photo :id)))
           ;; NOTE: 'telega-preview used in `telega-ins--input-file'
           ;; to insert document/photo/sticker/animation preview
           :animation (list :@type (propertize "inputFileId"
                                               'telega-preview preview)
                            :id (telega--tl-get animation :animation :id))
           ))
    ))

(defun telega-chatbuf-attach-sticker-by-emoji ()
  "If chatbuf has single emoji input, then popup stickers win.
Intended to be added to `post-command-hook' in chat buffer.
Or to be called directly.
Return non-nil if input has single emoji."
  (interactive)

  (telega-emoji-init)
  (let ((input (telega-chatbuf-input-string)))
    (when (and (= (length input) 1)
               (cl-member input telega-emoji-alist
                          :key 'cdr :test 'string=))
      ;; NOTE: Do nothing in case sticker's help win is exists and
      ;; have same emoji
      (let ((buf (get-buffer "*Telegram Stickers*")))
        (when (or (called-interactively-p 'interactive)
                  (not (buffer-live-p buf))
                  (not (with-current-buffer buf
                         (string= input telega-help-win--emoji))))
          (telega-sticker-choose-emoji input telega-chatbuf--chat)))
      t)))

(defun telega-chatbuf-attach-sticker (fav-or-recent-p)
  "Attach the sticker.
If prefix argument is specified, then attach recent or favorite sticker.
Otherwise choose sticker from some installed sticker set."
  (interactive "P")
  (if fav-or-recent-p
      (telega-sticker-choose-favorite-or-recent telega-chatbuf--chat)

    ;; Select some stickerset
    (let ((sset (telega-stickerset-completing-read "Sticker set: "))
          (tss-buffer (get-buffer "*Telegram Sticker Set*")))
      (unless (or (buffer-live-p tss-buffer)
                  (not (with-current-buffer tss-buffer
                         (and (eq telega-help-win--stickerset sset)
                              (eq telega--chat telega-chatbuf--chat)))))
        (telega-describe-stickerset sset telega-chatbuf--chat))

      (select-window
       (temp-buffer-window-show tss-buffer)))))

(defun telega-chatbuf-attach-animation (&optional from-file-p)
  "Attach the animation.
If prefix argument is specified, then attach animation from file.
Otherwise choose animation from list of saved animations."
  (interactive "P")
  (if from-file-p
      (let* ((afilename (read-file-name "Animation File: "))
             (ifile (telega-chatbuf--gen-input-file afilename 'Animation)))
        (telega-chatbuf-input-insert
         (list :@type "inputMessageAnimation"
               :animation ifile)))

    (telega-animation-choose-saved telega-chatbuf--chat)))

(defun telega-chatbuf-attach-gif ()
  "Attach animation from file."
  (interactive)
  (telega-chatbuf-attach-animation 'from-file))

(defun telega-chatbuf-attach-inline-bot-query (&optional no-empty-search)
  "Popup results with inline bot query.
Intended to be added to `post-command-hook' in chat buffer.
Or to be called directly.
Return non-nil if input has inline bot query.
If NO-EMPTY-SEARCH is non-nil, then do not perform empty query search."
  (interactive)
  (let ((input (telega-chatbuf-input-string)))
    (when (string-match "^@\\([^ ]+\\)[ \t]+\\(.*\\)" input)
      (let* ((username (match-string 1 input))
             (query (match-string 2 input))
             (uchat (telega--searchPublicChat username))
             (bot-user (and uchat (eq (telega-chat--type uchat) 'bot)
                            (telega-chat--user uchat)))
             (bot (plist-get bot-user :type))
             (inline-help (telega-tl-str bot :inline_query_placeholder)))
        (when (plist-get bot :is_inline)
          ;; Start querying the bot
          (unless (and (string-empty-p query) no-empty-search)
            (telega-inline-bot-query bot-user query telega-chatbuf--chat))

          ;; Display the inline help
          (when (string-empty-p query)
            (telega-momentary-display
             (propertize inline-help 'face 'shadow)))
          t)))))

(defun telega-chatbuf-attach-poll (question &rest options)
  "Attach poll to the input.
QUESTION - Title of the poll.
OPTIONS - List of strings representing poll options."
  (interactive (let ((ilist (list (read-string "Poll title: ")))
                     (optidx 1) opt)
                 (while (not (string-empty-p
                              (setq opt (read-string
                                         (format "Option %d): " optidx)))))
                   (setq ilist (nconc ilist (list opt)))
                   (cl-incf optidx))
                 ilist))
  (telega-chatbuf-input-insert
   (list :@type "inputMessagePoll"
         :question question
         :options (apply 'vector options))))

(defun telega-chatbuf-attach (attach-type attach-value)
  "Attach something into message.
Prefix argument is available for next attachements:
  screenshot - Takes numeric prefix argument to delay taking
               screenshot.
  sticker    - Takes C-u prefix argument to attach favorite/recent
               sticker.
  clipboard  - Available only if image is in the clipboard.
               Takes C-u prefix argument to attach clipboard as
               document.
  animation  - Takes C-u prefix ta attach file as animation."
  (interactive
   (list (funcall telega-completing-read-function
                  "Attach type: "
                  (nconc (list "photo" "audio" "video" "gif"
                               "note-video" "note-voice"
                               "file" "location"
                               "poll" "contact"
                               "screenshot" "member"
                               "sticker" "animation")
                         ;; Avoid any "Selection owner couldn't convert"
                         ;; errors
                         (when (ignore-errors
                                 (gui-get-selection 'CLIPBOARD 'image/png))
                           (list "clipboard")))
                  nil t)
         nil))
  (let ((cmd (symbol-function
              (intern (concat "telega-chatbuf-attach-" attach-type)))))
    (cl-assert (commandp cmd))
    (if attach-value
        (funcall cmd attach-value)
      (call-interactively cmd))))

(defun telega-photo-send (file chat)
  "Prepare FILE to be sent as photo to CHAT."
  (interactive (list (buffer-file-name)
                     (telega-completing-read-chat "Send photo to chat: ")))

  (cl-assert chat)
  (with-current-buffer (telega-chat--pop-to-buffer chat)
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (goto-char (point-max))
      (telega-chatbuf-attach-photo file))))

(defun telega-file-send (file chat &optional as-photo-p)
  "Prepare FILE to be sent as document or photo to CHAT.
If prefix argument is used, then always send as a file.
Otherwise for `image-mode' major-mode, send file as photo."
  (interactive
   (let ((send-photo-p (and (not current-prefix-arg)
                            (eq major-mode 'image-mode))))
     (list (buffer-file-name)
           (telega-completing-read-chat
            (format "Send %s to chat: " (if send-photo-p "PHOTO" "FILE")))
           send-photo-p)))

  (cl-assert chat)
  (with-current-buffer (telega-chat--pop-to-buffer chat)
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (goto-char (point-max))
      (if as-photo-p
          (telega-chatbuf-attach-photo file)
        (telega-chatbuf-attach-file file)))))

(defun telega--on-updateChatDraftMessage (event)
  (let ((chat (telega-chat-get (plist-get event :chat_id) 'offline))
        (draft-msg (plist-get event :draft_message)))
    (cl-assert chat)
    (plist-put chat :draft_message draft-msg)
    (unless (string= (plist-get event :order) (plist-get chat :order))
      (telega-chat--reorder chat (plist-get event :order)))
    (telega-root--chat-update chat)

    ;; Update chat's input to the text in DRAFT-MSG
    (with-telega-chatbuf chat
      (telega-chatbuf--input-draft draft-msg))
    ))

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
  (telega-debug "Switch %s: %s" (propertize "OUT" 'face 'bold)
                (buffer-name))
  (telega--closeChat telega-chatbuf--chat)

  (when (telega-chatbuf-has-input-p)
    (when telega-chatbuf--my-action
      (telega-chatbuf--set-action "Cancel"))

    ;; NOTE: Set draft only if there is no attaches in the input,
    ;; otherwise setting/updating draft will screw up text properties
    (unless (telega-chatbuf-input-has-attaches-p)
      (telega--setChatDraftMessage
       telega-chatbuf--chat
       (list :@type "draftMessage"
             :reply_to_message_id
             (or (plist-get (telega-chatbuf--replying-msg) :id) 0)
             :input_message_text
             (list :@type "inputMessageText"
                   :text (telega--formattedText
                          (telega-chatbuf-input-string))))))))

(defun telega-chatbuf--switch-in ()
  "Called when switching to chat buffer."
  (telega-debug "Switch %s: %s" (propertize "IN" 'face 'bold)
                (buffer-name))
  (telega--openChat telega-chatbuf--chat))

(defun telega-chatbuf--killed ()
  "Called when chat buffer is killed."
  ;; Cancel any active action and actualizes the draft
  ;; see https://t.me/emacs_telega/6708
  ;; Also closes chat
  (ignore-errors
    (telega-chatbuf--switch-out))

  ;; Stop any voice notes, see
  ;; https://github.com/zevlg/telega.el/issues/49
  (when telega-chatbuf--voice-msg
    (telega-ffplay-stop))

  (setq telega--chat-buffers
        (delq (current-buffer) telega--chat-buffers))

  ;; NOTE: chatbuffer might be left from other telega start, so it
  ;; will throw "Ewoc node not found" error - ignore it
  (ignore-errors
    (telega-root--chat-update telega-chatbuf--chat)))

;;; Message commands
(defun telega-msg-redisplay (msg &optional node)
  "Redisplay the message MSG.
NODE is already calculated ewoc NODE, or nil."
  (interactive (list (telega-msg-at (point))))

  (with-telega-chatbuf (telega-msg-chat msg)
    ;; Redisplay footer in case active voice note is redisplayed
    (when (eq msg telega-chatbuf--voice-msg)
      (telega-chatbuf--footer-redisplay))

    (when-let ((msg-node (or node (telega-ewoc--find-by-data
                                   telega-chatbuf--ewoc msg))))
      (telega-chatbuf--redisplay-node msg-node))))

(defun telega-msg-activate-voice-note (msg &optional for-chat)
  "Activate voice note MSG FOR-CHAT.
MSG can be nil in case there is no active voice message."
  (with-telega-chatbuf (or for-chat (telega-msg-chat msg))
    (setq telega-chatbuf--voice-msg msg)
    (telega-chatbuf--footer-redisplay)))

(defun telega-msg-reply (msg)
  "Start replying to MSG."
  (interactive (list (telega-msg-at (point))))

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
  (interactive (list (telega-msg-at (point))))

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
                            (plist-get content :caption))
                        'as-markdown))

    (telega-chatbuf--help-cancel-keys "edit")))

(defun telega-msg-forward (msg chat)
  "Forward message to chat."
  (interactive (list (telega-msg-at (point))
                     (telega-completing-read-chat "Forward to chat: ")))

  (cl-assert chat)
  (unless (plist-get msg :can_be_forwarded)
    (error "Message can't be forwarded"))

  (with-current-buffer (telega-chat--pop-to-buffer chat)
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
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

  (cl-assert (eq (telega--tl-type msg) 'message))
  (if (plist-get msg :telega-is-deleted-message)
      ;; NOTE: `d' is pressed on deleted message
      ;; (`telega-chat-show-deleted-messages' is non-nil)
      ;; Generate pseudo-event to delete the message
      (let ((telega-chat-show-deleted-messages nil))
        (telega--on-updateDeleteMessages
         (list :chat_id (plist-get msg :chat_id)
               :is_permanent t
               :message_ids (vector (plist-get msg :id)))))

    (when (y-or-n-p (concat (if revoke "Revoke" "Kill") " the message? "))
      (telega--deleteMessages
       (plist-get msg :chat_id) (list (plist-get msg :id)) revoke)

      (if telega-chat-show-deleted-messages
          (telega-help-message 'telega-chat-show-deleted-messages 'double-delete
            "Press %s once again to hide deleted message"
            (substitute-command-keys (format "\\[%S]" this-command)))
        (telega-help-message 'telega-chat-show-deleted-messages 'show-deleted
          "JFYI see `telega-chat-show-deleted-messages'")))))

(defun telega-chat-complete ()
  "Complete thing at chat input."
  (interactive)
  (or (call-interactively 'telega-chatbuf-attach-sticker-by-emoji)
      (when (and (boundp 'company-mode) company-mode)
        (cond ((telega-company-grab-username)
               (company-begin-backend 'telega-company-username)
               (company-complete-common)
               t)
              ((telega-company-grab-emoji)
               (company-begin-backend telega-emoji-company-backend)
               (company-complete-common)
               t)
              ((telega-company-grab-hashtag)
               (company-begin-backend 'telega-company-hashtag)
               (company-complete-common)
               t)
              ((telega-company-grab-botcmd)
               (company-begin-backend 'telega-company-botcmd)
               (company-complete-common)
               t)
              ))
      (call-interactively 'telega-chatbuf-attach-inline-bot-query)
      ;; TODO: add other completions
      ))

(defun telega-chat-next-link (n)
  (interactive "p")
  ;; TODO: maybe be more smarter about links
  (telega-button-forward n))

(defun telega-chat-prev-link (n)
  (interactive "p")
  (telega-chat-next-link (- n)))

(defun telega-chat-complete-or-next-link ()
  "Complete username at point, or jump to next link."
  (interactive)
  (if (<= telega-chatbuf--input-marker (point))
      (call-interactively 'telega-chat-complete)
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

(defun telega-chat--goto-msg0 (chat msg-id &optional highlight)
  "In chat denoted by CHAT-ID goto message denoted by MSG-ID.
Return non-nil on success."
  (with-telega-chatbuf chat
    (let ((node (telega-chatbuf--node-by-msg-id msg-id)))
      (when node
        (ewoc-goto-node telega-chatbuf--ewoc node)
        (when highlight
          (let ((msgb (button-at (point))))
            (with-no-warnings
              (pulse-momentary-highlight-region
               (button-start msgb) (button-end msgb)))))
        t))))

(defun telega-chat--goto-msg (chat msg-id &optional highlight)
  "In CHAT goto message denoted by MSG-ID.
If HIGHLIGHT is non-nil then highlight with fading background color."
  (with-current-buffer (telega-chat--pop-to-buffer chat)
    (unless (telega-chat--goto-msg0 chat msg-id highlight)
      ;; Not found, need to fetch history
      (telega-ewoc--clean telega-chatbuf--ewoc)
      (telega-chat--load-history
          chat msg-id (- (/ telega-chat-history-limit 2)) nil
        (lambda ()
          (telega-chat--goto-msg0 chat msg-id highlight))))))

(defun telega-chat-avatar-image (chat)
  "Return avatar for the USER."
  (let ((photo (plist-get chat :photo)))
    (telega-media--image
     (cons chat 'telega-avatar--create-image)
     (cons photo :small))))

(defun telega-chat-dnd-dispatcher (uri action)
  "DND open function for telega.
If called outside chat buffer, then fallback to default DND behaviour."
  (if (not (eq major-mode 'telega-chat-mode))
      (telega-chat-dnd-fallback uri action)
    (pcase-let* ((`(,proto ,content) (split-string uri "://"))
                 (real-name (thread-first content
                              (url-unhex-string)
                              (decode-coding-string 'utf-8))))
      (pcase proto
        ("file"
         (let ((doc-p (if (image-type-from-file-name real-name)
                          (y-or-n-p "Send this as a file?")
                        t)))
           (if doc-p
               (telega-chatbuf-attach-file real-name)
             (telega-chatbuf-attach-photo real-name))))
        (_
         (goto-char (point-max))
         (insert (concat proto "://" real-name)))))))

(defun telega-chat-dnd-fallback (uri action)
  "DND fallback function."
  (let ((dnd-protocol-alist
         (rassq-delete-all
          'telega-chat-dnd-dispatcher
          (copy-alist dnd-protocol-alist))))
    (dnd-handle-one-url nil action uri)))

;;; Filtering messages in chat buffer
(defun telega-chatbuf-filter-related ()
  "Show only related (to message at point) messages.
See https://t.me/designers/44"
  (user-error "`telega-chatbuf-filter-related' NOT yet implemented"))

(defun telega-chatbuf-filter-search (_query)
  "Search messages in chatbuffer by QUERY."
  (interactive "sSearch by query: ")
  (user-error "`telega-chatbuf-filter-search' NOT yet implemented"))

(defun telega-chatbuf-filter-cancel ()
  "Cancel any message filtering."
  (user-error "`telega-chatbuf-filter-cancel' NOT yet implemented"))


;; Chat Event Log
(defun telega-chatevent-log-filter (&rest filters)
  "Return chat event log filter.
FILTERS are:
`:message_edits', `:message_deletions', `:message_pins',
`:member_joins', `:member_leaves', `:member_invites',
`:member_promotions', `:member_restrictions', `:info_changes',
`:setting_changes'."
  (apply 'nconc (list :@type "chatEventLogFilters")
         (mapcar (lambda (filter) (list filter t)) filters)))

(provide 'telega-chat)

;; Install DND dispatcher for telega at load time
(let ((re (rx bol (or "file" "https" "http" "ftp"))))
  (cl-pushnew (cons re 'telega-chat-dnd-dispatcher) dnd-protocol-alist))

;;; telega-chat.el ends here

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

;;; ellit-org: commentary
;;
;; Chatbuf is a Emacs buffer showing some Telegram chat.  Chatbuf
;; consists of a list of chat messages and an input for your messages
;; to send.

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
(require 'telega-i18n)
(require 'telega-tme)
(require 'telega-sort)

(eval-when-compile
  (require 'rx)
  (require 'pcase))                     ;`pcase-let*' and `rx'

;; shutup compiler
(declare-function company-complete-common "company")
(declare-function company-begin-backend "company" (backend &optional callback))

(declare-function tracking-add-buffer "tracking" (buffer &optional faces))
(declare-function tracking-remove-buffer "tracking" (buffer))

(declare-function telega--on-updateDeleteMessages "telega-tdlib-events" (event))
(declare-function telega-chat--update "telega-tdlib-events" (chat &rest events))

(declare-function telega-chat-match-p "telega-filter" (chat chat-filter))
(declare-function telega-filter-chats "telega-filter" (chat-list &optional chat-filter))
(declare-function telega-filter-default-p "telega-filter" (&optional filter))

(declare-function telega--check-buffer-switch "telega-root")

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
Could be `loaded' or `nil'.")
(make-variable-buffer-local 'telega-chatbuf--history-state)

(defvar telega-chatbuf--voice-msg nil
  "Active (playing/paused) voice note message for the current chat.")
(make-variable-buffer-local 'telega-chatbuf--voice-msg)

(defvar telega-chatbuf--my-action nil
  "My current action in chat buffer.")
(make-variable-buffer-local 'telega-chatbuf--my-action)

(defvar telega-chatbuf--refresh-point nil
  "Non-nil if point needs refreshing on buffer switch in.
Could be either `t', then move point to next unread message.
Or point value to move point to.")
(make-variable-buffer-local 'telega-chatbuf--refresh-point)

(defvar telega-chatbuf--filter nil
  "Active messages filter in the chatbuf.
List in form:
\\(NAME FUN-OR-TDLIB-FILTER-NAME QUERY SENDER TOTAL-COUNT\\)")
(make-variable-buffer-local 'telega-chatbuf--filter)

(defvar telega-chatbuf--messages-ring nil
  "History of messages jumps.
Used for `M-g x' command.")
(make-variable-buffer-local 'telega-chatbuf--messages-ring)

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
  "Return colors pair associated with CHAT.
If there is no CHAT color, then generate new and assign it to CHAT."
  (let ((colors (telega-chat-uaprop chat :color)))
    (when (= (length colors) 3)
      ;; NOTE: Colors from telega<0.6.12, regenerate them
      (setq colors nil))

    (or colors
        (setf (telega-chat-uaprop chat :color)
              (or (when-let ((cc (cl-find chat telega-rainbow-color-custom-for
                                          :test #'telega-chat-match-p
                                          :key #'car)))
                    (list (cdr cc) (cdr cc)))
                  (let ((cid (telega-chat-title chat " ")))
                    (list (funcall telega-rainbow-color-function cid 'light)
                          (funcall telega-rainbow-color-function cid 'dark))))))
    ))

(defsubst telega--ordered-chats-insert (chat)
  "Insert CHAT into `telega--ordered-chats' according active sorter."
  (let ((place telega--ordered-chats))
    (if (or (null place)
            (telega-chat> chat (car place)))
        (setq telega--ordered-chats (push chat telega--ordered-chats))

      (while (and (telega-chat> (car place) chat)
                  (cdr place)
                  (telega-chat> (cadr place) chat))
        (setq place (cdr place)))
      (cl-assert place)
      (setcdr place (cons chat (cdr place))))
    telega--ordered-chats))

(defun telega-chat--ensure (chat)
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
          (when-let ((client-data (telega-tl-str chat :client_data)))
            (ignore-errors
              (plist-put chat :uaprops (car (read-from-string client-data)))))
          ))))

(defun telega-chat-get (chat-id &optional offline-p)
  "Get chat by its CHAT-ID.
If OFFLINE-P is non-nil then do not request the telegram-server."
  (let ((chat (gethash chat-id telega--chats)))
    (when (and (not chat) (not offline-p))
      (setq chat (telega--getChat chat-id))
      (cl-assert chat nil "getChat timed out chat_id=%d" chat-id)
      (telega-chat--ensure chat))
    chat))

(defun telega-chat-by-username (username)
  "Find chat by its USERNAME."
  (cl-find username telega--ordered-chats
           :test #'string=
           :key #'telega-chat-username))

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
(defalias 'telega-chat--secretchat 'telega-chat--info)
(defalias 'telega-chat--basicgroup 'telega-chat--info)
(defalias 'telega-chat--supergroup 'telega-chat--info)

(defun telega-chat--user (chat)
  "Return user associated with private or secret CHAT.
Return nil if CHAT is not private or secret."
  (let ((chat-type (plist-get chat :type)))
    (when (memq (telega--tl-type chat-type) '(chatTypePrivate chatTypeSecret))
      (telega--info 'user (plist-get chat-type :user_id)))))

;;; ellit-org: chatbuf
;; ** Chat types
;;
;; Every chat has a type.  Type is one of:
;; - private :: Private chat with telegram user
;; - secret :: Secret chat with telegram user
;; - bot :: Chat with telegram bot
;; - basicgroup :: Small chat group, could be upgraded to supergroup
;; - supergroup :: Chat group with all the chat possibilities
;; - channel :: Supergroup with unlimited members, where only admins can post messags
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

(defun telega-chat-channel-p (chat)
  "Return non-nil if CHAT is channel."
  (eq (telega-chat--type chat) 'channel))

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

(defun telega-chat-admin-get (chat user)
  "Return \"chatAdministrator\" structure for the USER.
Return nil if USER not administrator in the CHAT.
Works only for chats with active chatbuffer and fetched
administrators list."
  (with-telega-chatbuf chat
    (cl-find (plist-get user :id) telega-chatbuf--administrators
             :key (telega--tl-prop :user_id))))

(defun telega-chat-title (chat &optional with-username-delim)
  "Return title for the CHAT.
If WITH-USERNAME-DELIM is specified, append username to the title
delimiting with WITH-USERNAME-DELIM."
  (let* ((telega-emoji-use-images telega-chat-title-emoji-use-images)
         (chat-me-p (telega-me-p chat))
         (title (or (when chat-me-p
                      (if (stringp telega-chat-me-custom-title)
                          telega-chat-me-custom-title
                        ;; I18N: saved_messages -> Saved Messages
                        (telega-i18n "saved_messages")))
                    (telega-tl-str chat :title)
                    (progn
                      (cl-assert (telega-chat--user chat))
                      (telega-user--name (telega-chat--user chat) 'name)))))
    (when with-username-delim
      (when-let ((username (telega-chat-username chat)))
        (setq title (concat title (if (stringp with-username-delim)
                                      with-username-delim
                                    " ")
                            "@" username))))

    (if (and chat-me-p (functionp telega-chat-me-custom-title))
        (funcall telega-chat-me-custom-title title)
      title)))

(defun telega-chat-brackets (chat)
  "Return CHAT's brackets from `telega-chat-button-brackets'."
  (cdr (seq-find (lambda (bspec)
                   (telega-chat-match-p chat (car bspec)))
                 telega-chat-button-brackets)))

(defun telega-chat-title-with-brackets (chat &optional with-username-delim)
  "Return CHAT title surrounded with chat brackets."
  (let ((brackets (telega-chat-brackets chat)))
    (concat (or (car brackets) "[")
            (telega-chat-title chat with-username-delim)
            (or (cadr brackets) "]"))))

(defun telega--setChatNotificationSettings (chat &rest settings)
  "Set CHAT's notification settings to NOT-CFG."
  (declare (indent 1))
  (let ((not-cfg (plist-get chat :notification_settings))
        (request (list :@type "chatNotificationSettings")))
    (telega--tl-dolist ((prop-name value) (append not-cfg settings))
      (setq request (plist-put request prop-name (or value :false))))
    (telega-server--call
     (list :@type "setChatNotificationSettings"
           :chat_id (plist-get chat :id)
           :notification_settings request))))

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
                       :chat_id (plist-get chat :id)
                       :telega-is-deleted-message t))))
          (telega-chat--update-reply-markup-message chat 'offline))))))

(defun telega-chat--update-administrators (chat)
  "Asynchronously update CHAT's `telega-chatbuf--administrators'."
  ;; NOTE: admin right is required to to get admins list in channels
  (unless (telega-chat-channel-p chat)
    (telega--getChatAdministrators chat
      (lambda (admins)
        (with-telega-chatbuf chat
          (setq telega-chatbuf--administrators admins))))))

(defun telega-chat--update-pinned-message (chat &optional offline-p
                                                old-pin-msg-id)
  "Asynchronously load pinned message for CHAT.
Pass non-nil OFFLINE-P argument to avoid any async requests.
OLD-PIN-MSG-ID is the id of the previously pinned message."
  (when old-pin-msg-id
    (plist-put chat :telega-pinned-message nil))

  (let ((pin-msg (telega-chat-pinned-msg chat 'locally)))
    (if (or pin-msg (zerop (plist-get chat :pinned_message_id)) offline-p)
        (progn
          (with-telega-chatbuf chat
            (when (and old-pin-msg-id
                       (not (zerop old-pin-msg-id)))
              (cl-destructuring-bind (_old-pin-msg old-pin-node)
                  (telega-chatbuf--msg old-pin-msg-id 'with-node)
                (when old-pin-node
                  (telega-chatbuf--redisplay-node old-pin-node))))
            (when pin-msg
              (telega-msg-redisplay pin-msg))

            (telega-chatbuf-mode-line-update)
            (telega-chatbuf--footer-redisplay))
          (telega-chat--update chat))

      ;; Async load pinned message
      (telega-chat-pinned-msg chat nil
        (lambda (pinned-message)
          (cl-assert pinned-message)
          (telega-chat--update-pinned-message chat 'offline))))))

(defun telega-chats--kill-em-all ()
  "Kill all chat buffers."
  (dolist (cbuf (telega-chat-buffers))
    (kill-buffer cbuf)))

(defun telega-chats-top (category)
  "Return list of top chats used by CATEGORY.
CATEGORY is one of `Users', `Bots', `Groups', `Channels',
`InlineBots', `Calls', `ForwardChats'."
  (let ((top (assq category telega--top-chats))
        (currts (time-to-seconds (current-time))))
    (when (> currts (+ (or (cadr top) 0) 60))
      ;; XXX update only if last fetch is older then 60 seconds
      (setq top (list (time-to-seconds (current-time))
                      (telega--getTopChats (symbol-name category))))
      (setf (alist-get category telega--top-chats) top))
    (caddr top)))


;;; Chat buttons in root buffer
(defvar telega-chat-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "i") 'telega-describe-chat)
    (define-key map (kbd "h") 'telega-describe-chat)
    (define-key map (kbd "a") 'telega-chat-add-member)
    (define-key map (kbd "o") 'telega-chat-set-custom-order)
    (define-key map (kbd "r") 'telega-chat-toggle-read)
    (define-key map (kbd "d") 'telega-chat-delete)
    (define-key map (kbd "P") 'telega-chat-pin)
    (define-key map (kbd "^") 'telega-chat-pin)
    (define-key map (kbd "C") 'telega-chat-call)
    (define-key map (kbd "DEL") 'telega-chat-delete)
    map)
  "The key map for telega chat buttons.")

(define-button-type 'telega-chat
  :supertype 'telega
  :inserter telega-inserter-for-chat-button
  :action #'telega-chat--pop-to-buffer
  'keymap telega-chat-button-map)

(defun telega-chat--pop-to-buffer (chat &optional no-history-load)
  "Pop to CHAT's buffer.
NO-HISTORY-LOAD passed directly to `telega-chatbuf--get-create'.
Uses `telega-chat--display-buffer-action' as action in `pop-to-buffer.'
Return chatbuf."
  (prog1
      (pop-to-buffer (telega-chatbuf--get-create chat no-history-load)
                     telega-chat--display-buffer-action)
    ;; Force switch-in for non-interactive buffer switching
    (telega--check-buffer-switch)))

(defun telega-chat-pin (chat)
  "Toggle chat's pin state at point."
  (interactive (list (telega-chat-at (point))))
  (telega--toggleChatIsPinned chat))

(defun telega-chat-add-member (chat user &optional forward-limit)
  "Add USER to the CHAT."
  (interactive (list (or telega-chatbuf--chat
                         telega--chat
                         (telega-chat-at (point)))
                     (telega-completing-read-user "Add member: ")))
  (cl-assert user)
  (telega--addChatMember chat user forward-limit))

(defun telega-chat-remove-member (chat user &optional _ban)
  "Remove USER from the CHAT.
Specify non-nil BAN to ban this user in this CHAT."
  (interactive
   (let ((chat (or telega-chatbuf--chat
                   telega--chat
                   (telega-chat-at (point)))))
     (list chat
           (telega-completing-read-user
               "Remove member: "
             (telega--searchChatMembers chat ""))
           current-prefix-arg)))

  ;; TODO: ban (chatMemberStatusBanned :banned_until_date)
  (telega--setChatMemberStatus
   chat user (list :@type "chatMemberStatusLeft")))

(defun telega-chat-set-title (chat title)
  "Set CHAT's title to TITLE."
  (interactive
   (let ((chat (or telega-chatbuf--chat (telega-chat-at (point)))))
     (list chat (read-string "New title: " (telega-chat-title chat)))))
  (telega--setChatTitle chat title))

(defun telega-chat-set-ttl (chat ttl-seconds)
  "Set TTL setting for secret CHAT to TTL-SECONDS."
  (interactive
   (let ((chat (or telega-chatbuf--chat (telega-chat-at (point)))))
     (list chat (ceiling (read-number "TTL (seconds): ")))))
  (telega--sendChatSetTtlMessage chat ttl-seconds))

(defun telega-chat-set-custom-order (chat order)
  "For the CHAT (un)set custom ORDER."
  (interactive (let ((chat (telega-chat-at (point))))
                 (list chat
                       (read-string "Custom Order [empty to unset]: "
                                    (telega-chat-order chat)))))
  (if (string-empty-p order)
      (setq order nil)

    (unless (numberp (read order))
      (error "Invalid order, must contain only digits")))

  (setf (telega-chat-uaprop chat :order) order)
  ;; NOTE: Update chat with fake event causing chat reorder
  (telega-chat--update chat (list :@type "telegaChatReorder")))

(defun telega-chat-call (chat)
  "Call to the user associated with the given private CHAT."
  (interactive (list (telega-chat-at (point))))

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
    (telega--sendMessage chat (list :@type "inputMessageContact"
                                    :contact (telega-user-as-contact
                                              (telega-user-me))))))

(defun telega-describe-chat--inserter (chat)
  "Inserter for the CHAT description."
  (let ((chat-ava (telega-chat-avatar-image chat)))
    (telega-ins--image chat-ava 0
                       :no-display-if (not telega-chat-show-avatars))
    (telega-ins--with-face
        (let* ((ccolors (telega-chat-color chat))
               (lightp (eq (frame-parameter nil 'background-mode) 'light))
               (foreground (nth (if lightp 0 1) ccolors)))
          (when foreground
            (list :foreground foreground)))
      (telega-ins (telega-chat-title chat 'with-username)))
    (telega-ins "\n")
    (telega-ins--image chat-ava 1
                       :no-display-if (not telega-chat-show-avatars))

    (telega-ins (capitalize (symbol-name (telega-chat--type chat))) " ")
    (telega-ins--button "Open"
      :value chat
      :action 'telega-chat--pop-to-buffer)
    (when (telega-me-p chat)
      (telega-ins " ")
      (telega-ins--button "Set Profile Photo"
        'action (lambda (_ignored)
                  (let ((photo (read-file-name "Profile Photo: " nil nil t)))
                    (telega--setProfilePhoto photo)))))
    (when (telega--tl-get chat :permissions :can_invite_users)
      (telega-ins " ")
      (telega-ins--button "Add Member"
        'action (lambda (_ignored)
                  (telega-chat-add-member
                   chat (telega-completing-read-user "Add member: ")))))
    (when (telega--tl-get chat :permissions :can_change_info)
      (telega-ins " ")
      (telega-ins--button "Set Chat Photo"
        'action (lambda (_ignored)
                  (let ((photo (read-file-name "Chat Photo: " nil nil t)))
                    (telega--setChatPhoto chat photo)))))

    ;; Archive/Unarchive
    (let ((chat-archived-p (telega-chat-match-p chat 'archive)))
      (telega-ins " ")
      (telega-ins--button (if chat-archived-p
                              (telega-i18n "archived_remove")
                            (telega-i18n "archived_add"))
        'action (lambda (_ignored)
                  (telega--addChatToList
                   chat (list :@type (if chat-archived-p
                                         "chatListMain"
                                       "chatListArchive"))))))
    (telega-ins "\n"))

  (telega-ins-fmt "Id: %s\n"
    (if telega-debug
        (format "(telega-chat-get %d)" (plist-get chat :id))
      (format "%d" (plist-get chat :id))))
  (when (telega-chat-public-p chat)
    (let ((link (concat (or (plist-get telega--options :t_me_url)
                            "https://t.me/")
                        (telega-chat-username chat))))
      (insert "Link: ")
      (apply 'insert-text-button link (telega-link-props 'url link 'link))
      (insert "\n")))
  (telega-ins "Internal Link: ")
  (let ((internal-link (telega-tme-internal-link-to chat)))
    (apply 'insert-text-button internal-link
           (telega-link-props 'url internal-link 'link)))
  (telega-ins "\n")

  (telega-ins "Order")
  (when (telega-chat-uaprop chat :order)
    (telega-ins " (" (propertize "custom" 'face 'shadow) ")"))
  (telega-ins ": " (telega-chat-order chat) "\n")

  (let* (;; (default-mute-for
         ;;   (telega-chat-notification-setting chat :mute_for 'default))
         ;; (default-show-preview
         ;;   (telega-chat-notification-setting chat :show_preview 'default))
         (not-cfg (plist-get chat :notification_settings))
         (unmuted-p (not (telega-chat-muted-p chat)))
         (show-preview (telega-chat-notification-setting chat :show_preview)))
    (telega-ins "Default Disable Notification: ")
    (telega-ins--button (if (plist-get chat :default_disable_notification)
                            telega-symbol-heavy-checkmark
                          "  ")
      :value chat
      :action (lambda (chat)
                (telega--toggleChatDefaultDisableNotification
                 chat (not (plist-get chat :default_disable_notification)))))
    (telega-ins "\n")

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
                  :mute_for (if unmuted-p 599634793 0))))
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
                    :use_default_show_preview t))))
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
  )

(defun telega-describe-chat (chat)
  "Show info about chat at point."
  (interactive (list (telega-chat-at (point))))
  (with-telega-help-win "*Telegram Chat Info*"
    (setq telega--chat chat)
    (telega-describe-chat--inserter chat)

    (setq telega--help-win-param chat)
    (setq telega--help-win-inserter #'telega-describe-chat--inserter)
    ))

(defun telega-describe-chat--maybe-redisplay (chat)
  "If CHAT info buffer exists and visible, then redisplay it."
  (telega-help-win--maybe-redisplay "*Telegram Chat Info*" chat))

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
  (interactive (list (telega-chat-at (point))))
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

(defun telega-chats-filtered-kill-chatbuf ()
  "Kill chatbuf for all filtered chats."
  (interactive)
  (let ((filtered-chatbufs
         (cl-remove-if-not
          (lambda (buf)
            (telega-filter-chats (list (telega-chatbuf--chat buf))))
          (telega-chat-buffers))))
    (unless filtered-chatbufs
      (user-error "No chats with chatbuf to kill"))
    (when (and (y-or-n-p (telega-i18n "telega_query_kill_chatbufs"
                           :count (length filtered-chatbufs)))
               ;; NOTE: If no filter is applied, ask once more time
               (or (not (telega-filter-default-p))
                   (y-or-n-p (telega-i18n "telega_query_kill_anyway"))))
      (dolist (buf filtered-chatbufs)
        (kill-buffer buf)))))

(defun telega-chats-filtered-toggle-read (&optional _force)
  "Apply `telega-chat-toggle-read' to all currently filtered chats."
  (interactive
   (list (y-or-n-p (telega-i18n "telega_query_read_chats"
                     :count (length telega--filtered-chats)))))
  ;; NOTE: If no filter is applied, ask once more time
  (when (or (not (telega-filter-default-p))
            (y-or-n-p (telega-i18n "telega_query_read_anyway")))
    (mapc 'telega-chat-toggle-read telega--filtered-chats)))

(defun telega-chat-leave (chat &optional keep-chatbuf)
  "Leave the CHAT."
  (interactive
   (list (or telega-chatbuf--chat (telega-chat-at (point))) nil))

  (cl-case (telega-chat--type chat)
    (secret (telega--closeSecretChat (telega-chat--info chat)))
    ((private bot) 'no-op)
    (t (telega--leaveChat chat)))

  ;; Kill corresponding chat buffer
  (unless keep-chatbuf
    (with-telega-chatbuf chat
      (kill-buffer (current-buffer)))))

(defun telega-chat-delete (chat)
  "Delete CHAT.
Query about everything.  Leaving CHAT, blocking corresponding
user and delete all the chat history.
Use `telega-chat-leave' to just leave the CHAT."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))
  (when (and chat
             (yes-or-no-p
              (concat (telega-i18n "telega_action_cant_undone") ".\n"
                      (telega-i18n "telega_query_delete_chat"
                        :title (telega-chat-title chat)))))
    (telega-chat-leave chat 'keep-chatbuf)
    (setq telega-deleted-chats
          (cl-pushnew chat telega-deleted-chats))

    ;; Block corresponding user, so he could not initiate any incoming
    ;; messages
    (when-let ((chat-user (telega-chat-user chat 'include-bots)))
      (when (yes-or-no-p
             (concat (telega-i18n "blocked_list_confirm_text"
                       :name (telega-user--name chat-user)) " "))
        (telega--blockUser chat-user)))

    ;; NOTE: `telega--deleteChatHistory' cannot be used in channels
    ;; and public supergroups
    (unless (or (telega-chat-channel-p chat)
                (telega-chat-public-p chat 'supergroup))
      (when (telega-read-im-sure-p
             (telega-i18n "telega_query_delete_chat_history"
               :title (telega-chat-title chat)))
        (telega--deleteChatHistory chat t)))

    ;; Kill corresponding chat buffer
    (with-telega-chatbuf chat
      (kill-buffer (current-buffer)))))

(defun telega--chat-create-callback (newchat)
  "Callback used in `telega-chat-create'."
  ;; NOTE: Chat might change while created, so renew its value using
  ;; `telega-chat-get'
  (telega-chat--pop-to-buffer
   (telega-chat-get (plist-get newchat :id))))

(defun telega-chat-create (chat-type)
  "Interactively create new chat of CHAT-TYPE.
CHAT-TYPE is one of \"basicgroup\", \"supergroup\", \"channel\",
\"secret\", \"location-supergroup\", \"location-channel\"."
  (interactive (list (funcall telega-completing-read-function
                              "Chat Type: "
                              (list "basicgroup" "supergroup" "channel" "secret"
                                    "location-supergroup" "location-channel")
                              nil t)))

  (cond ((string= chat-type "basicgroup")
         (let ((title (read-string "Chat Title: "))
               (users (telega-completing-read-user-list "Add users")))
           (telega--createNewBasicGroupChat
            title users #'telega--chat-create-callback)))

        ((string= chat-type "secret")
         (let ((user (telega-completing-read-user "Secret chat with: ")))
           (telega-chat--pop-to-buffer
            (telega--createNewSecretChat user))))

        (t
         ;; Supergroup
         (let ((title (read-string "Chat Title: "))
               (desc (read-string "Chat Description: "))
               (loc (when (or (string= chat-type "location-supergroup")
                              (string= chat-type "location-channel"))
                      (let ((chat-loc (telega-read-location "Chat Location"))
                            (chat-address (read-string "Chat Address: ")))
                        (list :@type "chatLocation"
                              :location (cons :@type (cons "location" chat-loc))
                              :address chat-address)))))

           (telega--createNewSupergroupChat
            title (or (string= chat-type "channel")
                      (string= chat-type "location-channel"))
            desc loc #'telega--chat-create-callback)))))

(defun telega-chat-upgrade-to-supergroup (chat)
  "Upgrade basic group CHAT from basicgroup to supergroup."
  (interactive (list (or telega-chatbuf--chat
                         (telega-chat-at (point)))))
  (telega-server--call
   (list :@type "upgradeBasicGroupChatToSupergroupChat"
         :chat_id (plist-get chat :id))
   #'telega--chat-create-callback))

(defun telega-chat-transfer-ownership (chat)
  "Transfer CHAT's ownership TO-USER."
  (declare (indent 1))
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))

  (let* ((admins (mapcar #'telega-user--get
                         (mapcar (telega--tl-prop :user_id)
                                 (telega--getChatAdministrators chat))))
        (to-user (telega-completing-read-user "To Admin: "
                   (cl-remove-if #'telega-me-p admins))))

    ;; NOTE: check chat ownership can be transferred
    (unless (eq (telega--tl-type (telega--canTransferOwnership))
                'canTransferOwnershipResultOk)
      (user-error (concat
                   (telega-i18n "rights_transfer_check_about"
                     :user (telega-user--name to-user)) "\n"
                     (telega-i18n "rights_transfer_check_session") "\n"
                     (telega-i18n "rights_transfer_check_password") "\n"
                     (telega-i18n "rights_transfer_check_later"))))

    (unless (telega-read-im-sure-p
             (concat (telega-i18n "rights_transfer_about"
                       :group (telega-chat-title chat)
                       :user (telega-user--name to-user))
                     "\n"
                     (telega-i18n "rights_transfer_sure") "?"))
      (user-error "Ownership transfer canceled"))

    (let ((pass (password-read
                 (concat (telega-i18n "rights_transfer_password_description")
                         "\n" "Telegram Password: "))))
      (telega--transferChatOwnership chat to-user pass
        (lambda (result)
          (when (eq (telega--tl-type result) 'ok)
            (message
             (telega-i18n (if (telega-chat-channel-p chat)
                              "rights_transfer_done_channel"
                            "rights_transfer_done_group")
               :user (telega-user--name to-user)))))))
    ))

(defun telega-chat-set-description (chat descr)
  "Update CHAT's description."
  (interactive (let* ((chat (or telega-chatbuf--chat (telega-chat-at (point))))
                      (full-info (telega--full-info (telega-chat--info chat))))
                 (list chat
                       (read-string "Description: "
                                    (telega-tl-str full-info :description)))))
  (telega--setChatDescription chat descr))

(defun telega-chats-filtered-delete (&optional force)
  "Apply `telega-chat-delete' to all currently filtered chats.
Do it only if FORCE is non-nil."
  (interactive (list (yes-or-no-p
                      (format "%s.\nDelete %d chats? "
                              (telega-i18n "telega_action_cant_undone")
                              (length telega--filtered-chats)))))
  (when force
    (mapc #'telega-chat-delete telega--filtered-chats)))

(defun telega-saved-messages (arg)
  "Switch to SavedMessages chat buffer.
If \"Saved Messages\" chat is not opened, then open it.
If `\\[universal-argument]' is specified, then keep the point,
otherwise goto end of the buffer."
  (interactive "P")
  (telega-chat--pop-to-buffer (telega-chat-me))
  (unless arg
    (goto-char (point-max))))

(defun telega-switch-buffer (buffer)
  "Interactively switch to chat BUFFER."
  (interactive
   (list (progn
           (unless telega--chat-buffers-alist
             (user-error "No chatbufs to switch"))
           (funcall
            telega-completing-read-function
            "Telega chat: "
            ;; NOTE: if current buffer is chatbuf, then exclude it
            ;; from the list, because it is strange if it appers first
            ;; in the list
            (mapcar #'telega-chatbuf--name
                    (let ((telega-sort--inhibit-order t))
                      (telega-sort-chats
                       telega-chat-switch-buffer-sort-criteria
                       (delq telega-chatbuf--chat
                             (mapcar #'car telega--chat-buffers-alist)))))
            nil t))))
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
    (if (zerop pin-msg-id)
        (progn
          (plist-put chat :telega-pinned-message nil)
          nil)

      (or (plist-get chat :telega-pinned-message)
          (telega-msg--get (plist-get chat :id) pin-msg-id offline-p
            (lambda (pin-msg)
              (unless pin-msg
                (setq pin-msg (list :id pin-msg-id
                                    :chat_id (plist-get chat :id)
                                    :telega-is-deleted-message t)))
              (plist-put chat :telega-pinned-message pin-msg)
              (when callback
                (funcall callback pin-msg))))))))


;;; Chat Buffer
(defvar telega-chatbuf--origin-recenter-command
  (lookup-key global-map (kbd "C-l"))
  "Command used to run on `C-l'.")

(defvar telega-chat-mode-hook nil
  "Hook run when telega chat buffer is created.")

(defvar telega-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") 'telega-chatbuf-recenter-1)

    ;; C-M-[ - cancels edit/reply
    (define-key map (kbd "\e\e") 'telega-chatbuf-cancel-aux)
    (define-key map (kbd "C-c C-k") 'telega-chatbuf-cancel-aux)
    (define-key map (kbd "C-M-c") 'telega-chatbuf-cancel-aux)
    (define-key map (kbd "C-M-a") 'telega-chatbuf-beginning-of-thing)

    (define-key map (kbd "C-c ?") 'telega-describe-chatbuf)

    (define-key map (kbd "RET") 'telega-chatbuf-input-send)
    (define-key map (kbd "M-p") 'telega-chatbuf-edit-prev)
    (define-key map (kbd "M-n") 'telega-chatbuf-edit-next)
    (define-key map (kbd "M-r") 'telega-chatbuf-input-search)

    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-history-beginning,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-history-beginning, 2)}}}
    (define-key map (kbd "M-g <") 'telega-chatbuf-history-beginning)
    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-read-all,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-read-all, 2)}}}
    (define-key map (kbd "M-g >") 'telega-chatbuf-read-all)
    (define-key map (kbd "M-g r") 'telega-chatbuf-read-all)
    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-next-unread-mention,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-next-unread-mention, 2)}}}
    (define-key map (kbd "M-g m") 'telega-chatbuf-next-unread-mention)
    (define-key map (kbd "M-g @") 'telega-chatbuf-next-unread-mention)
    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-next-unread,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-next-unread, 2)}}}
    (define-key map (kbd "M-g u") 'telega-chatbuf-next-unread)
    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-goto-pin-message,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-goto-pin-message, 2)}}}
    (define-key map (kbd "M-g P") 'telega-chatbuf-goto-pin-message)
    (define-key map (kbd "M-g ^") 'telega-chatbuf-goto-pin-message)
    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-goto-pop-message,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-goto-pop-message, 2)}}}
    (define-key map (kbd "M-g x") 'telega-chatbuf-goto-pop-message)

    ;;; ellit-org: chatbuf-attach-bindings
    ;; - {{{where-is(telega-chatbuf-attach,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-attach,2)}}}
    (define-key map (kbd "C-c C-a") 'telega-chatbuf-attach)
    ;;; ellit-org: chatbuf-attach-bindings
    ;; - {{{where-is(telega-chatbuf-attach-file,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-attach-file,2)}}}
    (define-key map (kbd "C-c C-f") 'telega-chatbuf-attach-file)
    ;;; ellit-org: chatbuf-attach-bindings
    ;; - {{{where-is(telega-chatbuf-attach-clipboard,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-attach-clipboard,2)}}}
    (define-key map (kbd "C-c C-v") 'telega-chatbuf-attach-clipboard)

    ;;; ellit-org: chatbuf-filtering-bindings
    ;; - {{{where-is(telega-chatbuf-filter,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-filter,2)}}}
    (define-key map (kbd "C-c /") 'telega-chatbuf-filter)
    ;;; ellit-org: chatbuf-filtering-bindings
    ;; - {{{where-is(telega-chatbuf-filter-cancel,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-filter-cancel, 2)}}}
    (define-key map (kbd "C-c C-c") 'telega-chatbuf-filter-cancel)
    ;;; ellit-org: chatbuf-filtering-bindings
    ;; - {{{where-is(telega-chatbuf-filter-search,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-filter-search, 2)}}}
    (define-key map (kbd "C-c C-s") 'telega-chatbuf-filter-search)

    ;; jumping around links
    (define-key map (kbd "TAB") 'telega-chatbuf-complete-or-next-link)
    (define-key map (kbd "<backtab>") 'telega-chatbuf-prev-link)
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

(defsubst telega-chatbuf--last-msg-loaded-p ()
  "Return non-nil if `:last_message' of the chat is shown."
  (unless telega-chatbuf--filter
    (let ((last-msg-id
           (or (telega--tl-get telega-chatbuf--chat :last_message :id) 0)))
      (<= last-msg-id (or (plist-get (telega-chatbuf--last-msg) :id) 0)))))

(defun telega-chatbuf--view-msg-at (&optional point force)
  "View message at POINT.
If POINT is ommited, then current point is used.
FORCE - non-nil to force viewing messages in closed chat.
If POINT is not over some message, then view last message."
  (let* ((last-read-msg-id
          (plist-get telega-chatbuf--chat :last_read_inbox_message_id))
         (message (or (telega-msg-at (or point (point)))
                      (telega-chatbuf--last-msg))))
    (when (and message
               (or (plist-get message :contains_unread_mention)
                   (> (plist-get message :id) last-read-msg-id)))
      (telega--viewMessages telega-chatbuf--chat (list message) force))))

(defun telega-chatbuf--footer ()
  "Generate string to be used as ewoc's footer."
  ;; --(actions part)---------------[additional status]--
  ;; [x] Messages Filter: <FILTER> (total: MSG-COUNT)
  ;; [x] Action Bar: [ action ] [ bar ] [ buttons ]
  ;; [ REPLY-MARKUP] buttons
  ;; (AVA)>>>
  (let* ((column (+ telega-chat-fill-column 10 1))
         (column1 (/ column 2))
         (column2 (- column column1))
         (fill-symbol (if (or (null telega-chatbuf--ewoc)
                              telega-chatbuf--filter
                              (and telega-chatbuf--ewoc
                                   (telega-chatbuf--last-msg-loaded-p)))
                          telega-symbol-underline-bar
                        telega-symbol-underline-bar-partial))
         ;; NOTE: `telega-ins--as-string' uses temporary buffer, so
         ;; prepare everything we need before
         (actions (gethash (plist-get telega-chatbuf--chat :id)
                           telega--actions))
         (chat telega-chatbuf--chat)
         (chat-filter telega-chatbuf--filter)
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

       ;; Messages Filter
       (when chat-filter
         (telega-ins--button (propertize "✕" 'face 'bold)
           'action #'telega-chatbuf-filter-cancel)
         (telega-ins " ")
         (telega-ins "Messages Filter: "
                     (propertize (car chat-filter) 'face 'bold))
         (when-let ((sender (nth 3 chat-filter)))
           (telega-ins " by ")
           (telega-ins--raw-button
               (list 'action (lambda (_button)
                               (telega-describe-user sender)))
             (telega-ins (telega-user--name sender))))
         (when-let ((total-count (nth 4 chat-filter)))
           (telega-ins-fmt " (total: %d)" total-count))
         (telega-ins "\n"))
       ;; Action Bar
       (when (telega-ins--chat-action-bar chat)
         (telega-ins "\n"))

       ;; Reply markup
       (when-let ((markup-msg
                   (and (not (zerop (plist-get chat :reply_markup_message_id)))
                        (telega-chat-reply-markup-msg chat 'offline))))
         (unless (plist-get markup-msg :telega-is-deleted-message)
           (telega-ins--labeled (concat telega-symbol-keyboard "\u00A0") nil
             (telega-ins--reply-markup markup-msg 'force))
           (telega-ins "\n")

           (telega-ins--with-attrs (list :min column :max column
                                         :align 'left
                                         :align-symbol fill-symbol))
           (telega-ins "\n")
           ))
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

  ;; NOTE: This keeps point where it is
  (if (< (ewoc-location (ewoc--footer telega-chatbuf--ewoc))
         (point)
         telega-chatbuf--input-marker)
      (telega-save-cursor
        (telega-ewoc--set-footer
         telega-chatbuf--ewoc (telega-chatbuf--footer)))
    (save-excursion
      (telega-ewoc--set-footer
       telega-chatbuf--ewoc (telega-chatbuf--footer)))))

(defun telega-chatbuf--check-focus-change ()
  "Check is some messages need to be viewed."
  (cl-assert (eq major-mode 'telega-chat-mode))
  (when (telega-focus-state)
    ;; NOTE: on focus-in view all visible messages, see
    ;; https://github.com/zevlg/telega.el/issues/81
    (telega-chatbuf--view-msg-at (point))))

(define-derived-mode telega-chat-mode nil "◁Chat"
  "The mode for telega chat buffer.

Message bindings (cursor on message):
\\{telega-msg-button-map}
Global chat bindings:
\\{telega-chat-mode-map}"
  :group 'telega-chat
  (setq telega-chatbuf--chat telega-chat--preparing-buffer-for
        telega-chatbuf--messages (make-hash-table :test 'eq)
        telega-chatbuf--messages-ring (make-ring telega-chat-messages-ring-size)
        telega-chatbuf--input-ring (make-ring telega-chat-input-ring-size)
        telega-chatbuf--input-idx nil
        telega-chatbuf--input-pending nil
        telega-chatbuf--history-loading nil
        telega-chatbuf--inline-query nil
        telega-chatbuf--voice-msg nil
        telega-chatbuf--my-action nil
        telega-chatbuf--administrators nil)

  ;; Make usernames with "_" be completable
  (modify-syntax-entry ?\_ "w" telega-chat-mode-syntax-table)

  (erase-buffer)
  (setq-local nobreak-char-display nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local window-point-insertion-type t)
  (setq-local next-line-add-newlines nil)
  (setq-local scroll-conservatively telega-chat-scroll-scroll-conservatively)
  (cursor-sensor-mode 1)
  (cursor-intangible-mode 1)

  (setq telega-chatbuf--ewoc
        (ewoc-create (telega-ewoc--gen-pp #'telega-msg--pp) nil nil t))
  (goto-char (point-max))

  ;; Use punctuation as "no-value" button content
  ;; See https://github.com/zevlg/telega.el/issues/45
  (setq telega-chatbuf--aux-button
        (telega-button--insert 'telega-prompt-aux "!aa!"
          'invisible t))
  (setq telega-chatbuf--prompt-button
        (telega-button--insert 'telega-prompt ">>>"))
  (telega-chatbuf--prompt-update telega-chat-input-prompt)

  (setq telega-chatbuf--input-marker (point-marker))

  (add-hook 'window-scroll-functions 'telega-chatbuf--window-scroll nil t)
  (add-hook 'post-command-hook 'telega-chatbuf--post-command nil t)
  (add-hook 'kill-buffer-hook 'telega-chatbuf--killed nil t)
  (when (boundp 'after-focus-change-function)
    (add-function :after (local 'after-focus-change-function)
                  'telega-chatbuf--check-focus-change))

  (setq telega--chat-buffers-alist
        (cl-pushnew (cons telega-chatbuf--chat (current-buffer))
                    telega--chat-buffers-alist)))

(defun telega-describe-chatbuf ()
  "Show info about chat."
  (interactive)
  (telega-describe-chat telega-chatbuf--chat))

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
           #'telega-ins--prompt-aux-edit)
       (button-get telega-chatbuf--aux-button :value)))

(defun telega-chatbuf--replying-msg ()
  "Return message currently replying."
  (and (eq (button-get telega-chatbuf--aux-button :inserter)
           #'telega-ins--prompt-aux-reply)
       (button-get telega-chatbuf--aux-button :value)))

(defun telega-chatbuf--window-scroll (window display-start)
  "Mark some messages as read while scrolling."
  (with-current-buffer (window-buffer window)
    (when (not (eq window (selected-window)))
      ;; Mark some messages as read
      ;; Scroll might be triggered in closed chat, so force viewMessages
      (telega-chatbuf--view-msg-at (window-point window) 'force)

      ;; If scrolling in inactive window (with C-M-v) we might need to
      ;; fetch new history if point near the buffer bottom
      (when (and (> display-start (- (point-max) 2000))
                 (not telega-chatbuf--history-loading)
                 (telega-chatbuf--need-newer-history-p))
        (telega-chatbuf--load-newer-history)))
    ))

(defun telega-chatbuf--post-command ()
  "Chabuf `post-command-hook' function."
  ;; Possible view some new message at point
  (telega-chatbuf--view-msg-at (point))

  ;; Check that all atachements are valid (starting/ending chars are
  ;; ok) and remove invalid attachments
  (let ((attach (telega--region-by-text-prop
                 telega-chatbuf--input-marker 'telega-attach)))
    (while attach
      (if (and (get-text-property (car attach) 'attach-open-bracket)
               (get-text-property (1- (cdr attach)) 'attach-close-bracket))
          ;; Valid
          (setq attach (telega--region-by-text-prop
                        (cdr attach) 'telega-attach))

        ;; Invalid attachment, remove it from input
        (delete-region (car attach) (cdr attach))
        (setq attach (telega--region-by-text-prop
                      (car attach) 'telega-attach)))))

  ;; If point moves inside prompt, move it at the beginning of input.
  ;; However inhibit this behaviour in case unblock-start-join button
  ;; is displayed in the prompt
  (when (and (not (button-get telega-chatbuf--prompt-button :usj-prompt-p))
             (>= (point) telega-chatbuf--prompt-button)
             (< (point) telega-chatbuf--input-marker))
    (goto-char telega-chatbuf--input-marker))

  ;; If point moves near the beginning of chatbuf, then request for
  ;; the older history
  (when (and (< (point) 2000)
             (not telega-chatbuf--history-loading)
             (telega-chatbuf--need-older-history-p))
    (telega-chatbuf--load-older-history))

  ;; If point moves near the end of the chatbuf, then request for
  ;; newer history
  ;; NOTE: Do not load newer history if prompt is active (reply or
  ;; edit)
  (when (and (> (point) (- (point-max) 2000))
             (not telega-chatbuf--history-loading)
             (telega-chatbuf--need-newer-history-p))
    (telega-chatbuf--load-newer-history))

  ;; Finally, when input is probably changed by above operations,
  ;; update chat's action after command execution.
  (let ((input-p (telega-chatbuf-has-input-p)))
    (cond ((and (not telega-chatbuf--my-action) input-p)
           (telega-chatbuf--set-action "Typing"))
          ((and telega-chatbuf--my-action (not input-p))
           (telega-chatbuf--set-action "Cancel")))

    ;; If there is active draft_message and input is empty then clear
    ;; the draf
    (when (and (plist-get telega-chatbuf--chat :draft_message)
               (not input-p))
      (telega--setChatDraftMessage telega-chatbuf--chat)))
  )

(defun telega-chatbuf--name (chat)
  "Return uniquified name for the CHAT buffer."
  (let* ((bufname (substring-no-properties
                   (concat telega-symbol-telegram
                           (when (telega-chat-secret-p chat)
                             telega-symbol-lock)
                           (telega-chat-title-with-brackets chat "")
                           (when (plist-get chat :is_pinned)
                             telega-symbol-pin)
                           (when (plist-get chat :has_scheduled_messages)
                             telega-symbol-alarm))))
         (buf (get-buffer bufname)))
    ;; NOTE: Multiple chats could have same BUFNAME, uniquify it by
    ;; adding unique suffix, in case other chat occupies BUFNAME
    ;; See https://github.com/zevlg/telega.el/issues/158
    (if (and (buffer-live-p buf)
             (not (eq chat (telega-chatbuf--chat buf))))
        (concat bufname
                "<"
                (or (telega-chat-username chat)
                    (number-to-string (plist-get chat :id)))
                ">")
      bufname)))

(defun telega-chatbuf--unblock-start-join ()
  "[START] [UNBLOCK] or [JOIN] or button has been pressed."
  (cl-assert (not (telega-chat-secret-p telega-chatbuf--chat)))

  ;; NOTE: do async calls, update chatbuf prompt
  ;; on-updateUserFullInfo, on-updateBasicGroup or on-updateSupergroup
  (if-let ((user (telega-chat-user telega-chatbuf--chat 'include-bots)))
      (progn
        (when (telega-user-blocked-p user 'locally)
          (telega--unblockUser user #'ignore))
        (when (telega-user-bot-p user)
          (telega--sendMessage telega-chatbuf--chat
                               (list :@type "inputMessageText"
                                     :text (telega-string-fmt-text "/start")))))

    (telega--joinChat telega-chatbuf--chat))

;  (telega-chatbuf--prompt-update)
  )

(defun telega-chatbuf--prompt-unblock-start-join ()
  "Return unblock-start-join button to be used in chatbuf prompt."
  (let* ((chat telega-chatbuf--chat)
         (user (telega-chat-user chat 'include-bots))
         (label (cond ((and user (telega-user-blocked-p user 'locally))
                       (if (telega-user-bot-p user)
                           "RESTART BOT"
                         "UNBLOCK"))
                      ((and user (telega-user-bot-p user)
                            (not (telega-chat-match-p chat 'has-last-message)))
                       "START")
                      ((and (not user)
                            (not (telega-chat-match-p chat 'me-is-member)))
                       "JOIN"))))
    (when label
      (telega-ins--as-string
       (telega-ins--button (concat "   " label "   ")
         'action (lambda (_ignored)
                   (telega-chatbuf--unblock-start-join)))))))

(defun telega-chatbuf--prompt-update (&optional prompt)
  "Update chatbuf's prompt to PROMPT.
If PROMPT is ommited, then update prompt only if
unblock-start-join button state changes."
  ;; NOTE: `telega-chatbuf--chat' will be overwritten in
  ;; `telega-ins--as-string', so save it before
  (let* ((chat telega-chatbuf--chat)
         (has-usj-prompt-p
          (button-get telega-chatbuf--prompt-button :usj-prompt-p))
         (usj-prompt (telega-chatbuf--prompt-unblock-start-join))
         (value (when (or prompt (and has-usj-prompt-p (not usj-prompt))
                          (and (not has-usj-prompt-p) usj-prompt))
                  (telega-ins--as-string
                   (when (telega-chat-match-p
                          chat telega-chat-prompt-show-avatar-for)
                     (telega-ins--image
                      (telega-chat-avatar-image-one-line chat)))
                   (telega-ins (or usj-prompt
                                   prompt
                                   telega-chat-input-prompt))))))
    (when value
      (telega-button--update-value telega-chatbuf--prompt-button value
                                   :usj-prompt-p usj-prompt))
    ))

(defun telega-chatbuf--prompt-reset ()
  "Reset prompt to initial state in chat buffer."
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (telega-save-excursion
      (unless (button-get telega-chatbuf--aux-button 'invisible)
        (telega-button--update-value
         telega-chatbuf--aux-button "!aa!"
         :inserter 'telega-ins
         'invisible t))

      (telega-chatbuf--prompt-update telega-chat-input-prompt))))

(defun telega-chatbuf--input-draft (draft-msg &optional force)
  "Update chatbuf's input to display draft message DRAFT-MSG.
If FORCE is specified, then set input draft unconditionally,
otherwise set draft only if chatbuf input is also draft."
  (let ((reply-msg-id (plist-get draft-msg :reply_to_message_id)))
    (if (and reply-msg-id (not (zerop reply-msg-id)))
        (telega-msg-reply
         (telega-msg--get (plist-get telega-chatbuf--chat :id) reply-msg-id))
      ;; Reset only if replying, but `:reply_to_message_id' is not
      ;; specified, otherwise keep the aux, for example editing
      (when (telega-chatbuf--replying-msg)
        (telega-chatbuf--prompt-reset)))

    ;; NOTE: update draft only if current chatbuf input is marked as
    ;; draft (or empty), otherwise draft update may change current
    ;; input
    (when (or force (not (telega-chatbuf-has-input-p))
              (telega-chatbuf--input-draft-p))
      (telega-save-cursor
        (telega-chatbuf--input-delete)
        (goto-char telega-chatbuf--input-marker)
        (telega-ins--with-props '(:draft-input-p t)
          (telega-ins--fmt-text
           (telega--tl-get draft-msg :input_message_text :text)))))))

(defun telega-chatbuf--load-initial-history ()
  "Load initial history in the chatbuf."
  (telega-chatbuf--clean)
  (telega-chat--load-history telega-chatbuf--chat
      (plist-get telega-chatbuf--chat :last_read_inbox_message_id)
      (- (/ telega-chat-history-limit 2))
      telega-chat-history-limit
    (lambda (total-messages)
      (telega-chatbuf-next-unread
        (lambda (button)
          (telega-chatbuf--view-msg-at button)
          (when (eq (telega-msg-at button) (telega-chatbuf--last-msg))
            (goto-char (point-max)))))

      ;; Possible load more history
      (unless (zerop total-messages)
        (when (and (< (point) 2000)
                   (telega-chatbuf--need-older-history-p)))
        (telega-chatbuf--load-older-history))
      )))

(defun telega-chatbuf--get-create (chat &optional no-history-load)
  "Get or create chat buffer for the CHAT.
If NO-HISTORY-LOAD is specified, do not try to load history."
  (let ((bufname (telega-chatbuf--name chat)))
    (or (get-buffer bufname)
        (with-current-buffer (generate-new-buffer bufname)
          (let ((telega-chat--preparing-buffer-for chat))
            (telega-chat-mode))
          (telega-chatbuf--footer-redisplay)
          (telega-chatbuf-mode-line-update)

          ;; Asynchronously update chat administrators
          (telega-chat--update-administrators chat)
          ;; Asynchronously update pinned message, if any
          (unless (zerop (plist-get chat :pinned_message_id))
            (telega-chat--update-pinned-message chat))
          ;; Asynchronously update reply markup message
          (unless (zerop (plist-get chat :reply_markup_message_id))
            (telega-chat--update-reply-markup-message chat))

          ;; Show the draft message if any, see
          ;; https://github.com/zevlg/telega.el/issues/80
          (when-let ((draft-msg (plist-get chat :draft_message)))
            (telega-chatbuf--input-draft draft-msg 'force))

          ;; Start from last read message
          ;; see https://github.com/zevlg/telega.el/issues/48
          (unless no-history-load
            (telega-chatbuf--load-initial-history))

          ;; Openning chat may affect filtering, see `opened' filter
          (telega-chat--update chat)

          (current-buffer)))))

(defun telega-chatbuf--need-older-history-p ()
  "Return non-nil if older history can be loaded."
  (not (eq telega-chatbuf--history-state 'loaded)))

(defun telega-chatbuf--need-newer-history-p ()
  "Return non-nil if newer history can be loaded."
  (and (not (telega-chatbuf--last-msg-loaded-p))
       (button-get telega-chatbuf--aux-button 'invisible)))

(defun telega-chatbuf-mode-line-discuss ()
  "Format [Discuss] button for chat buffer modeline."
  (when (telega-chat-match-p telega-chatbuf--chat 'has-linked-chat)
    (let ((channel-p (telega-chat-channel-p telega-chatbuf--chat)))
      (telega-ins--as-string
       (telega-ins " [")
       (telega-ins--with-props
           (list 'local-map (eval-when-compile
                              (make-mode-line-mouse-map
                               'mouse-1 #'telega-chatbuf-goto-linked-chat))
                 'mouse-face 'mode-line-highlight
                 'help-echo (telega-i18n "telega_chat_modeline_discuss_help"
                              :mouse "mouse-1"))
         (telega-ins telega-symbol-linked)
         (telega-ins-i18n (if channel-p
                              "channel_discuss"
                            "manage_linked_channel")))
       (telega-ins "]")))))

(defun telega-chatbuf-mode-line-unread ()
  "Format unread/mentions string for chat buffer modeline."
  (let* ((unread-count (plist-get telega-chatbuf--chat :unread_count))
         (mention-count (plist-get telega-chatbuf--chat :unread_mention_count))
         (brackets (or (> unread-count 0) (> mention-count 0))))
    (concat
     (when brackets " (")
     (when (> unread-count 0)
       (propertize (telega-i18n "telega_chat_modeline_unread"
                     :unread_count unread-count)
                   'face 'bold
                   'local-map (eval-when-compile
                                (make-mode-line-mouse-map
                                 'mouse-1 #'telega-chatbuf-read-all))
                   'mouse-face 'mode-line-highlight
                   'help-echo (telega-i18n "telega_chat_modeline_unread_help"
                                :mouse "mouse-1")))
     (when (> mention-count 0)
       (concat
        (when (> unread-count 0) " ")
        (propertize (concat "@" (number-to-string mention-count))
                    'face 'telega-mention-count
                    'local-map (eval-when-compile
                                 (make-mode-line-mouse-map
                                  'mouse-1 'telega-chatbuf-next-unread-mention))
                    'mouse-face 'mode-line-highlight
                    'help-echo (telega-i18n "telega_chat_modeline_mention_help"
                                 :mouse "mouse-1"))))
     (when brackets ")"))))

(defun telega-chatbuf-mode-line-marked ()
  "Format string for marked messages in chatbuffer."
  (let ((marked-count (length telega-chatbuf--marked-messages)))
    (unless (zerop marked-count)
      (concat
       " ("
       (propertize (telega-i18n "telega_chat_modeline_marked"
                     :marked_count marked-count)
                   'face 'error
                   'local-map (eval-when-compile
                                (make-mode-line-mouse-map
                                 'mouse-1 'telega-chatbuf-unmark-all))
                   'mouse-face 'mode-line-highlight
                   'help-echo (telega-i18n "telega_chat_modeline_marked_help"
                                :mouse "mouse-1"))
       ")"))))

(defun telega-chatbuf-mode-line-members (&optional use-icons-p)
  "Format members string for chat buffer modeline.
If ICONS-P is non-nil, then use icons for members count."
  (let ((member-count
         (or (plist-get (telega-chat--info telega-chatbuf--chat) :member_count) 0))
        (online-count (or (plist-get telega-chatbuf--chat :x-online-count) 0)))
    (unless (zerop member-count)
      (concat " ["
              (if (not use-icons-p)
                  (telega-i18n "telega_chat_modeline_members"
                    :member_count member-count :count online-count)

                (concat
                 (number-to-string member-count)
                 (propertize telega-symbol-contact 'face 'shadow)
                 (unless (zerop online-count)
                   (concat ", " (number-to-string online-count)
                           telega-symbol-online-status))))
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
                   'help-echo (telega-i18n "telega_chat_modeline_pinned_msg_help"
                                :mouse "mouse-1"))
           (telega-ins telega-symbol-pin)
           (let ((telega-use-images nil)
                 (telega-emoji-use-images nil))
             ;; NOTE: avoid using images for emojis, because modeline
             ;; height might differ from default height, and modeline
             ;; will increase its height
             (telega-ins--content-one-line pin-msg))))
       (telega-ins "]")))))

(defun telega-chatbuf-mode-line-messages-filter ()
  "Format currently applied messages filter."
  (when-let ((chat-filter telega-chatbuf--filter))
    (concat " ["
            (propertize "Filter" 'face 'error)
            ": "
            (propertize (car chat-filter) 'face 'bold)
            (when-let ((sender (nth 3 chat-filter)))
              (concat " by " (telega-user--name sender 'name)))
            "]")))

(defun telega-chatbuf-mode-line-update ()
  "Update `mode-line-buffer-identification' for the CHAT buffer."
  (setq mode-line-buffer-identification
        (list (propertized-buffer-identification "%b")
              ;; Online status
              (when (and (telega-chat-private-p telega-chatbuf--chat)
                         (not (telega-me-p telega-chatbuf--chat))
                         (telega-user-online-p
                          (telega-chat--user telega-chatbuf--chat)))
                telega-symbol-online-status)
              ;; TTL for secret chats
              (when (telega-chat-secret-p telega-chatbuf--chat)
                (let* ((secret (telega-chat--secretchat telega-chatbuf--chat))
                       (ttl (plist-get secret :ttl)))
                  (concat " ("
                          (propertize
                           (concat telega-symbol-lock "TTL: "
                                   (if (zerop ttl)
                                       "Off"
                                     (telega-duration-human-readable ttl 2)))
                           'local-map (eval-when-compile
                                        (make-mode-line-mouse-map
                                         'mouse-1 'telega-chat-set-ttl))
                           'mouse-face 'mode-line-highlight
                           'help-echo "Change Time-To-Live for messages")
                          ")")))

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

(defun telega-chatbuf-edit-next (without-aux &optional backward)
  "Edit message sent next to currently editing.
If WITHOUT-AUX is specified with `\\[universal-argument]', then
instead of editing, just pop previously sent message as input."
  (interactive "P")
  (let* ((edit-msg (telega-chatbuf--editing-msg))
         (last-msg (telega-chatbuf--last-msg))
         (last-sent-msg
          (if (and backward (not edit-msg) (telega-msg-by-me-p last-msg)
                   (plist-get last-msg :can_be_edited))
              last-msg
            (telega-chatbuf--next-msg (or edit-msg last-msg)
                                      (lambda (msg)
                                        (and (telega-msg-by-me-p msg)
                                             (plist-get msg :can_be_edited)))
                                      backward))))
    (if last-sent-msg
        (progn
          (telega-msg-edit last-sent-msg)
          (when without-aux
            (telega-chatbuf-cancel-aux)))

      (if (and edit-msg (not backward))
          (telega-chatbuf-cancel-aux 'delete-input)
        (user-error "Nothing to edit")))))

(defun telega-chatbuf-edit-prev (without-aux)
  "Edit previously sent message."
  (interactive "P")
  (telega-chatbuf-edit-next without-aux 'backward))

(defun telega-chatbuf-beginning-of-thing (&optional arg)
  "Move backward to the beginning of the chat input or message."
  (interactive "p")
  (when (> (point) telega-chatbuf--input-marker)
    (goto-char telega-chatbuf--input-marker)
    (cl-decf arg))

  (when (> arg 0)
    (beginning-of-defun arg)))

(defun telega-chatbuf--redisplay-node (node)
  "Redisplay NODE in chatbuffer.
Try to keep point at its position."
  (let* ((pos (point))
         (msg-button (button-at pos))
         (chat-win (get-buffer-window))
         (chat-win-start (when chat-win
                           (window-start chat-win))))
    (unwind-protect
        (if (and msg-button
                 (eq (button-get msg-button :value)
                     (ewoc--node-data node))
                 (>= pos (button-start msg-button))
                 (<= pos (button-end msg-button)))
            (telega-save-cursor
              (ewoc-invalidate telega-chatbuf--ewoc node))
          (telega-save-excursion
            (ewoc-invalidate telega-chatbuf--ewoc node)))

      ;; NOTE: we keep window position as is, node redisplay might
      ;; change it or shift.
      (when chat-win
        (set-window-point chat-win (point))
        (set-window-start chat-win chat-win-start 'noforce))
      )))

(defun telega-chatbuf--prepend-messages (messages)
  "Insert MESSAGES at the beginning of the chat buffer.
First message in MESSAGE will be first message at the beginning."
  (with-telega-deferred-events
    (let ((node (ewoc--header telega-chatbuf--ewoc)))
      (seq-doseq (msg messages)
        (run-hook-with-args 'telega-chat-insert-message-hook msg)
        ;; Track the uploading progress
        ;; see: https://github.com/zevlg/telega.el/issues/60
        (telega-msg--track-file-uploading-progress msg)
        (remhash (plist-get msg :id) telega-chatbuf--messages)
        (setq node (ewoc-enter-after telega-chatbuf--ewoc node msg)))
      node)))

(defun telega-chatbuf--append-messages (messages)
  "Insert MESSAGES at the end of the chat buffer.
Return last inserted ewoc node."
  (with-telega-deferred-events
    (let (ret)
      (seq-doseq (msg messages)
        (run-hook-with-args 'telega-chat-insert-message-hook msg)
        ;; Track the uploading progress
        ;; see: https://github.com/zevlg/telega.el/issues/60
        (telega-msg--track-file-uploading-progress msg)
        (remhash (plist-get msg :id) telega-chatbuf--messages)
        (setq ret (ewoc-enter-last telega-chatbuf--ewoc msg)))
      ret)))

(defun telega-chatbuf--node-by-msg-id (msg-id)
  "In current chatbuffer find message button with MSG-ID."
  ;; TODO: maybe do binary search on buffer position (getting message
  ;; as `telega-msg-at'), since message ids grows monotonically
  ;; Or maybe search from last node
  ;;
  ;; NOTE: message with MSG-ID might be deleted, in this case return
  ;; first message with higher id
  (telega-ewoc--find telega-chatbuf--ewoc msg-id #'= (telega--tl-prop :id)))

(defun telega-chatbuf--msg (msg-id &optional with-node)
  "In current chatbuf return message by MSG-ID.
If WITH-NODE is non-nil then return also corresponding ewoc node.
Return message if WITH-NODE is nil.
Return list, where first element is the message and second is the
ewoc node if WITH-NODE is non-nil."
  (let* ((pinned-msg (plist-get telega-chatbuf--chat :telega-pinned-message))
         (cached-msg (or (when (eq msg-id (plist-get pinned-msg :id))
                           pinned-msg)
                         (gethash msg-id telega-chatbuf--messages)))
         (node (when (or (null cached-msg) with-node)
                 (telega-chatbuf--node-by-msg-id msg-id)))
         (msg (or cached-msg (when node (ewoc--node-data node)))))
    (cl-assert (or (null cached-msg) (null msg) (eq msg cached-msg)))
    (if with-node
        (list msg node)
      msg)))

(defun telega-chatbuf--cache-msg (msg)
  "Cache MSG in chatbuf's messages cache."
  ;; NOTE: if message with msg's id already in cache or has associated
  ;; ewoc node, then do not override the value
  ;; updateXXX events will update contents of the message
  (unless (telega-chatbuf--msg (plist-get msg :id))
    (if (eq (plist-get msg :id)
            (plist-get telega-chatbuf--chat :pinned_message_id))
        (plist-put telega-chatbuf--chat :telega-pinned-message msg)
      (puthash (plist-get msg :id) msg telega-chatbuf--messages))))

(defun telega-chatbuf--next-msg (msg predicate &optional backward)
  "Return message next to MSG matching PREDICATE.
If BACKWARD is non-nil, then return previous message.
Return nil, if not found."
  (with-telega-chatbuf (telega-msg-chat msg)
    (let* ((mnode (telega-chatbuf--node-by-msg-id (plist-get msg :id)))
           (mnode1 (if backward
                       (ewoc-prev telega-chatbuf--ewoc mnode)
                     (ewoc-next telega-chatbuf--ewoc mnode)))
           (nnode (and mnode1
                       (telega-ewoc--find-if
                        telega-chatbuf--ewoc predicate nil mnode1
                        (if backward #'ewoc--node-prev #'ewoc--node-next)))))
      (when nnode
        (ewoc--node-data nnode)))))

(defun telega-chatbuf--read-outbox (old-last-read-outbox-msgid)
  "Redisplay chat messages affected by read-outbox change.
OLD-LAST-READ-OUTBOX-MSGID is old value for chat's `:last_read_outbox_message_id'."
  (let ((node (ewoc--footer telega-chatbuf--ewoc)))
    (while (and (setq node (ewoc-prev telega-chatbuf--ewoc node))
                (< old-last-read-outbox-msgid
                   (plist-get (ewoc-data node) :id)))
      (telega-chatbuf--redisplay-node node))))

(defun telega-chat--load-history (chat &optional from-msg-id offset limit
                                       callback)
  "Load and insert CHAT's history.
If FROM-MSG-ID is specified, then cancel last history load and
start loading messages from FROM-MSG-ID.
OFFSET and LIMIT are passed directly to `getChatHistory'.
CALLBACK is called after history has been loaded with single
argument - total number of loaded messages."
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
        (let ((history-callback
               (lambda (history)
                 (with-telega-chatbuf chat
                   (telega-save-excursion
                     (telega-chatbuf--prepend-messages
                      (nreverse (plist-get history :messages))))
                   (setq telega-chatbuf--history-loading nil)
                   (when (zerop (length (plist-get history :messages)))
                     (setq telega-chatbuf--history-state 'loaded))
                   (telega-chatbuf--footer-redisplay)
                   (when callback
                     (funcall callback (plist-get history :total_count)))))))
        (setq telega-chatbuf--history-loading
              (if telega-chatbuf--filter
                  (telega--searchChatMessages
                      chat (nth 1 telega-chatbuf--filter)
                      (nth 2 telega-chatbuf--filter)
                      from-msg-id offset limit
                      (nth 3 telega-chatbuf--filter)
                    history-callback)

                (telega--getChatHistory
                    chat from-msg-id offset
                    (or limit telega-chat-history-limit) nil
                  history-callback))))
        (telega-chatbuf--footer-redisplay)
        ))))

(defun telega-chatbuf--load-older-history (&optional callback)
  "In chat buffer load older messages.
CALLBACK if non-nil, then called with total number of loaded messages."
  (if (and telega-chatbuf--filter
           (not (stringp (nth 1 telega-chatbuf--filter))))
      ;; TODO: filtering
      nil
    (telega-chat--load-history telega-chatbuf--chat nil nil nil callback)))

(defun telega-chatbuf--load-newer-history ()
  "In chat buffer load newer messages."
  (if telega-chatbuf--filter
      ;; TODO: filtering
      nil

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
          )))))

(defun telega-chatbuf-cancel-aux (&optional arg)
  "Cancel current aux prompt.
If prefix ARG is giver, also delete input."
  (interactive "P")
  (telega-chatbuf--prompt-reset)
  (when arg
    (telega-chatbuf--input-delete)))

(defun telega-help-message--cancel-aux (what)
  "Show help about canceling reply/edit in echo area."
  (telega-help-message what "%s to cancel %S"
    (telega-keys-description 'telega-chatbuf-cancel-aux telega-chat-mode-map)
    what))

(defsubst telega--forwardMessage (chat msg &rest args)
  "Forward single message MSG to CHAT.
ARGS are passed directly to `telega--forwardMessages'."
  (apply 'telega--forwardMessages chat (telega-msg-chat msg) (list msg) args))

(defun telega-chatbuf--input-imcs (markdown-version)
  "Convert input to input message contents list.
If MARKDOWN-VERSION is specified, then format input as markdown
markup of MARKDOWN-VERSION."
  (cl-assert (memq markdown-version '(nil 0 1 2)))
  (let ((attaches (telega--split-by-text-prop
                   (telega-chatbuf-input-string) 'telega-attach))
        (disable-webpage-preview nil)
        result)
    (while attaches
      (let* ((text (car attaches))
             (attach (get-text-property 0 'telega-attach text)))
        (cond
         ((not attach)
          ;; Simple text
          ;; Check the limit first
          (when (> (length text)
                   (plist-get telega--options :message_text_length_max))
            (error "Message length exceedes %d limit"
                   (plist-get telega--options :message_text_length_max)))

          (push (list :@type "inputMessageText"
                      :text (telega-string-fmt-text text markdown-version)
                      :disable_web_page_preview
                      (if disable-webpage-preview t :false)
                      :clear_draft t)
                result))

         ;; Special attachment to disable web-page preview
         ((eq (telega--tl-type attach) 'telegaDisableWebpagePreview)
          (setq disable-webpage-preview t))

          ;; Some real attachment:
          ;; 1) If attachment followed by plain text, then it might be
          ;; a caption for the attachment, in this case add caption
          ;; to the attachment.
          ;; 2) Special case is for forwarded messages, new caption can
          ;; be supplied for the forwarded message only if forwarded
          ;; message as copy and original caption is removed (`C-u C-u
          ;; f' behaviour)
         (t
          (when (and (or (memq (telega--tl-type attach)
                               (list 'inputMessageAnimation 'inputMessageAudio
                                     'inputMessageDocument 'inputMessagePhoto
                                     'inputMessageVideo 'inputMessageVoiceNote))
                         ;; New caption for the forwarded message?
                         (and (eq (telega--tl-type attach) 'telegaForwardMessage)
                              (plist-get attach :send_copy)
                              (plist-get attach :remove_caption)))
                     (cadr attaches)
                     (not (get-text-property 0 'telega-attach (cadr attaches))))
            ;; NOTE: there is caption limit in telegram
            ;; Attach the caption
            (when (> (length (cadr attaches))
                     (plist-get telega--options :message_caption_length_max))
              (error "Caption exceedes %d limit"
                     (plist-get telega--options :message_caption_length_max)))

            (let ((cap (telega-string-fmt-text (cadr attaches) markdown-version)))
              (setq attach (plist-put attach :caption cap)))
            (setq attaches (cdr attaches)))
          (push attach result))))

      (setq attaches (cdr attaches)))
    (nreverse result)))

(defun telega-chatbuf--input-options (imc)
  "Convert IMC to send options.
Return valid \"messageSendOptions\"."
  (cl-ecase (telega--tl-type imc)
    (telegaScheduledMessage
     (let ((timestamp (plist-get imc :timestamp)))
       (list :@type "messageSendOptions"
             :scheduling_state
             (if timestamp
                 (list :@type "messageSchedulingStateSendAtDate"
                       :send_date timestamp)
               (list :@type "messageSchedulingStateSendWhenOnline")))))

    (telegaDisableNotification
       (list :@type "messageSendOptions"
             :disable_notification (plist-get imc :disable_notification)))
    ))

(defun telega-chatbuf-input-send (&optional markdown-version)
  "Send chatbuf input to the chat.
MARKDOWN-VERSION - version for markdown formatting, by default
`telega-chat-use-markdown-version' is used as MARKDOWN-VERSION.

If called interactively, then `\\[universal-argument]' inverses
value of the `telega-chat-use-markdown-version'.
In case `telega-chat-use-markdown-version' is nil, number of
`\\[universal-argument]' is used as MARKDOWN-VERSION.
In case `telega-chat-use-markdown-version' is no-nil, and `\\[universal-argument]' is specified, then nil MARKDOWN-VERSION is used."
  (interactive (list (if current-prefix-arg
                         (unless telega-chat-use-markdown-version
                           (cond ((equal current-prefix-arg '(4)) 1)
                                 ((equal current-prefix-arg '(16)) 2)
                                 (t 0)))
                       telega-chat-use-markdown-version)))
  ;; Send the input
  (let ((input (telega-chatbuf-input-string))
        (imcs (telega-chatbuf--input-imcs markdown-version))
        (replying-msg (telega-chatbuf--replying-msg))
        (editing-msg (telega-chatbuf--editing-msg))
        (options nil))
    ;; NOTE: if first IMC is telegaScheduledMessage, then schedule all
    ;; the IMCs
    (when (and imcs (eq (telega--tl-type (car imcs)) 'telegaScheduledMessage))
      (setq options (telega-chatbuf--input-options (car imcs)))
      (setq imcs (cdr imcs)))

    (cond
     (editing-msg
      (let ((edit-mc (plist-get editing-msg :content))
            (imc (car imcs)))
        (when (> (length imcs) 1)
          (error "Multiple input messages while edit"))
        (cond ((and ;(eq (telega--tl-type imc) 'inputMessageLocation)
                    (eq (telega--tl-type edit-mc) 'messageLocation))
               (telega--editMessageLiveLocation
                telega-chatbuf--chat editing-msg (plist-get imc :location)))

              ((and (eq (telega--tl-type imc) 'inputMessageText)
                    (eq (telega--tl-type edit-mc) 'messageText))
               (telega--editMessageText
                telega-chatbuf--chat editing-msg imc))

              ((eq (telega--tl-type imc) 'inputMessageText)
               (telega--editMessageCaption
                telega-chatbuf--chat editing-msg (plist-get imc :text)))

              (t
               (telega--editMessageMedia
                telega-chatbuf--chat editing-msg imc)))))

     ;; If all IMCS are photos and videos then send them as album
     ;; otherwise send IMCS as separate messages
     ;; NOTE: cl-every returns `t' on empty list
     ((and (> (length imcs) 1)
           (cl-every (lambda (imc)
                       (memq (telega--tl-type imc)
                             '(inputMessagePhoto inputMessageVideo)))
                     imcs))
      (telega--sendMessageAlbum telega-chatbuf--chat imcs replying-msg options))

     ;; NOTE: If forwarding <= 10 messages with photos/videos
     ;; then combine them into album
     ((and (> (length imcs) 1) (<= (length imcs) 10)
           (cl-every
            (lambda (imc)
              (and (eq (telega--tl-type imc) 'telegaForwardMessage)
                   (memq (telega--tl-type
                          (telega--tl-get imc :message :content))
                         '(messagePhoto messageVideo))))
            imcs)
           ;; TODO
           nil
           )
      ;; TODO: forward as album
      )

     (t (dolist (imc imcs)
          (cl-case (telega--tl-type imc)
            (telegaInlineQuery
             (telega--sendInlineQueryResultMessage
              telega-chatbuf--chat imc replying-msg))

            (telegaForwardMessage
             (let* ((msg (plist-get imc :message))
                    (copy-opts
                     (nconc (list :@type "messageCopyOptions"
                                  :send_copy
                                  (if (plist-get imc :send_copy)
                                      t :false)
                                  :replace_caption
                                  (if (plist-get imc :remove_caption)
                                      t :false))
                            (when-let ((new-cap (plist-get imc :caption)))
                              (list :new_caption new-cap))))
                    (fwd-imc (list :@type "inputMessageForwarded"
                                   :from_chat_id (plist-get msg :chat_id)
                                   :message_id (plist-get msg :id)
                                   :copy_options copy-opts)))
               (telega--sendMessage telega-chatbuf--chat
                                    fwd-imc replying-msg options)
               (when (plist-get imc :unmark-after-sent)
                 (telega-msg-unmark msg))))

            ((telegaScheduledMessage telegaDisableNotification)
             ;; Merge new imc options into existing options
             (telega--tl-dolist ((prop value) (telega-chatbuf--input-options imc))
               (setq options (plist-put options prop value))))

            (t (telega--sendMessage
                telega-chatbuf--chat imc replying-msg options))))))

    ;; Recover prompt to initial state
    (telega-chatbuf--input-delete)
    (telega-chatbuf--prompt-reset)

    ;; Save input to history
    (unless (string-empty-p input)
      (ring-insert telega-chatbuf--input-ring input)
      (setq telega-chatbuf--input-idx nil
            telega-chatbuf--input-pending nil))))

(defun telega-chatbuf-input-insert (imc)
  "Insert input content defined by IMC into chatbuf input.
IMC might be a plain string or attachment specification."
  ;; Check that point is in input area, otherwise move to the end
  (when (< (point) telega-chatbuf--input-marker)
    (goto-char (point-max)))

  (when (and (> (point) telega-chatbuf--input-marker)
             (get-text-property (point) 'telega-attach))
    (telega-ins " "))
  (if (stringp imc)
      (telega-ins imc)
    ;; NOTE: Put special properties `attach-open-bracket' and
    ;; `attach-close-bracket' to be used by
    ;; `telega-chatbuf--post-command' to determine if part of
    ;; attachment is deleted by `delete-char' or `backward-delete'
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

(defun telega-chatbuf--clean ()
  "Remove all messages displayed in chatbuf.
Also reset `telega-chatbuf--filter'."
  (setq telega-chatbuf--filter nil)
  (telega-ewoc--clean telega-chatbuf--ewoc)
  (setq telega-chatbuf--history-state nil))

(defun telega-chatbuf-history-beginning ()
  "Jump to the first message in the chat history."
  ;; See https://github.com/tdlib/td/issues/195
  (interactive)
  (if (eq telega-chatbuf--history-state 'loaded)
      (goto-char (point-min))

    (telega-chatbuf--clean)
    (telega-chat--load-history
        telega-chatbuf--chat 10 (- telega-chat-history-limit) nil
      (lambda (_ignored)
        (setq telega-chatbuf--history-state 'loaded)
        (goto-char (point-min))))))

(defun telega-chatbuf-recenter-1 (arg)
  "Recenter for chatbuf.
Call `(recenter -1)' if point is at prompt, otherwise call `recenter' as-is."
  (interactive "P")
  (if (and (not arg) (<= telega-chatbuf--input-marker (point)))
      (recenter -1)
    (when (commandp telega-chatbuf--origin-recenter-command)
      (call-interactively telega-chatbuf--origin-recenter-command))))

(defun telega-chatbuf-read-all ()
  "Jump to the last message in the chat history and mark all messages as read."
  (interactive)
  (unless (telega-chatbuf--last-msg-loaded-p)
    ;; Need to load most recent history
    (telega-chatbuf--clean)
    (telega-chatbuf--load-older-history
     (lambda (total-mesages)
       (unless (zerop total-mesages)
         (telega-chatbuf--view-msg-at (point))))))

  (goto-char (point-max)))

(defun telega-chatbuf-unmark-all ()
  "Unmark all marked messages in chatbuf."
  (interactive)
  (let ((marked-messages telega-chatbuf--marked-messages))
    (setq telega-chatbuf--marked-messages nil)

    (dolist (msg marked-messages)
      (telega-msg-redisplay msg))
    (telega-chatbuf-mode-line-update)))

(defun telega-chatbuf-next-message (&optional n)
  "Move point to the next message."
  (interactive "p")
  (telega-button-forward (or n 1)))

(defun telega-chatbuf-next-unread (&optional button-callback)
  "Goto next uneard message in chat.
BUTTON-CALLBACK - callback to call with single argument - message
button."
  (declare (indent 0))
  (interactive)
  (telega-chat--goto-msg telega-chatbuf--chat
      (plist-get telega-chatbuf--chat :last_read_inbox_message_id) nil
    (lambda ()
      ;; NOTE:
      ;; - deleted messages can't be marked as read, so point will
      ;;   stuck at deleted messag, so we just skip such messages
      ;; - `telega-button-forward' returns nil if there is no button
      ;;   matching predicate.  In this case just move to the prompt
      (let ((button (telega-button-forward
                        1 (lambda (button)
                            (when-let ((msg (telega-msg-at button)))
                              (not (plist-get msg :telega-is-deleted-message))))
                        'recenter)))
        (if button
            (when button-callback
              (funcall button-callback button))
          (goto-char (point-max)))))))

(defun telega-chatbuf-next-unread-mention ()
  "Goto next unread mention in chat buffer."
  (interactive)

  ;; NOTE:
  ;; - check `:unread_mention_count' for zerop
  ;; - searchChatMessages with :filter searchMessagesFilterUnreadMention
  ;; - Goto first found message
  (when (zerop (plist-get telega-chatbuf--chat :unread_mention_count))
    (user-error "telega: No next unread mention"))

  (let* ((reply
          (telega--searchChatMessages telega-chatbuf--chat
              "searchMessagesFilterUnreadMention" "" 0 0 1))
         (next-unread-mention-msg
          (car (append (plist-get reply :messages) nil))))
    (unless next-unread-mention-msg
      (user-error "telega: Can't fetch next unread mention message"))
    (telega-msg-goto next-unread-mention-msg 'highlight)
    ))

(defun telega-chatbuf-goto-reply-markup-message ()
  "Goto chat's reply markup message."
  (interactive)
  (let ((reply-markup-msg-id (plist-get telega-chatbuf--chat
                                        :reply_markup_message_id)))
    (if (zerop reply-markup-msg-id)
        (user-error "No reply markup message for this chat")
      (telega-chatbuf--goto-msg reply-markup-msg-id 'highlight))))

(defun telega-chatbuf-goto-linked-chat ()
  "Goto chat linked to current chat buffer channel."
  (interactive)
  (let* ((full-info (telega--full-info
                     (telega-chat--info telega-chatbuf--chat)))
         (linked-chat (telega-chat-get
                       (plist-get full-info :linked_chat_id) 'offline)))
    (unless linked-chat
      (user-error "telega: %s has no linked chat"
                  (telega-chat-title telega-chatbuf--chat)))

    (telega-chat--pop-to-buffer linked-chat)))

(defun telega-chatbuf-goto-pin-message ()
  "Goto pinned message for the chatbuffer."
  (interactive)
  (let ((pinned-msg-id (plist-get telega-chatbuf--chat :pinned_message_id)))
    (unless (zerop pinned-msg-id)
      (telega-chat--goto-msg telega-chatbuf--chat pinned-msg-id 'highlight))))

(defun telega-chatbuf-goto-pop-message ()
  "Pop message from `telega-chatbuf--messages-ring' and goto it."
  (interactive)
  (when (ring-empty-p telega-chatbuf--messages-ring)
    (user-error "telega: No messages to pop to"))

  (let ((pop-to-msg (ring-remove telega-chatbuf--messages-ring 0)))
    (message "telega: %d messages left in messages ring"
             (ring-length telega-chatbuf--messages-ring))
    ;; NOTE: by binding `telega-chatbuf--messages-ring' to nil, we
    ;; avoid putting current message into
    ;; `telega-chatbuf--messages-ring'
    (let ((telega-chatbuf--messages-ring nil))
      (telega-msg-goto-highlight pop-to-msg))))

;;; Attaching stuff to the input
(defun telega-chatbuf-attach-location (location &optional live-secs)
  "Attach location to the chatbuf input.
If `\\[universal-argument]' is given, then attach live location."
  (interactive (list (with-telega-chatbuf-action "ChoosingLocation"
                       (if current-prefix-arg
                           (telega-read-live-location "Live Location")
                         (telega-read-location "Location")))
                     (when current-prefix-arg
                       (let* ((choices `(("1 min" . 60)
                                         ("15 min" . ,(* 15 60))
                                         ("1 hour" . ,(* 60 60))
                                         ("8 hours" . ,(* 8 60 60))))
                              (live-for (funcall telega-completing-read-function
                                                 "Live for: "
                                                 (mapcar 'car choices) nil t)))
                         (cdr (assoc live-for choices))))))

  (telega-chatbuf-input-insert
   (nconc (list :@type "inputMessageLocation"
                :location (cons :@type (cons "location" location)))
          (when live-secs
            (list :live_period live-secs)))))

(defun telega-chatbuf-attach-contact (contact)
  "Attach CONTACT user to the chatbuf input."
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
       (list (telega-user-as-contact user)))))

    (telega-chatbuf-input-insert
     (list :@type "inputMessageContact"
           :contact contact)))

(defun telega-chatbuf--gen-input-file (filename &optional file-type
                                                preview-p upload-callback)
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
                   (let ((ufile (telega-file--upload
                                    filename file-type 16 upload-callback)))
                     (list "inputFileId" :id (plist-get ufile :id)))
                 (list "inputFileLocal" :path filename))))
    (nconc (list :@type (propertize (car ifile) 'telega-preview preview))
           (cdr ifile))))

(defun telega-chatbuf-attach-file (filename &optional preview-p)
  "Attach FILENAME as document to the chatbuf input."
  (interactive (list (read-file-name "Attach file: ")))
  (let ((ifile (telega-chatbuf--gen-input-file filename 'Document preview-p)))
    (telega-chatbuf-input-insert
     (list :@type "inputMessageDocument"
           :document ifile))))

(defun telega-chatbuf-attach-photo (filename &optional ttl)
  "Attach FILENAME as photo to the chatbuf input."
  (interactive (list (read-file-name "Photo: ")))
  (let ((ifile (telega-chatbuf--gen-input-file filename 'Photo t))
        (img-size (image-size
                   (create-image filename (when (fboundp 'imagemagick-types)
                                            'imagemagick) nil :scale 1.0)
                   t (telega-x-frame))))
    (telega-chatbuf-input-insert
     (nconc (list :@type "inputMessagePhoto"
                  :photo ifile
                  :width (or (car img-size) 0)
                  :height (or (cdr img-size) 0))
            (when ttl
              (list :ttl ttl))))))

(defun telega-chatbuf-attach-ttl-photo (filename ttl)
  "Attach self destructing photo.
This attachment can be used only in private chats."
  (interactive (list (read-file-name "Photo: ")
                     (read-number "Self desctruct in seconds (0-60): ")))
  (telega-chatbuf-attach-photo filename ttl))

(defun telega-chatbuf-attach-video (filename &optional ttl)
  "Attach FILENAME as video to the chatbuf input."
  (interactive (list (read-file-name "Video: ")))
  (let ((ifile (telega-chatbuf--gen-input-file filename 'Video)))
    (telega-chatbuf-input-insert
     (nconc (list :@type "inputMessageVideo"
                  :video ifile)
            (when ttl
              (list :ttl ttl))))))

(defun telega-chatbuf-attach-ttl-video (filename ttl)
  "Attach self destructing video.
This attachment can be used only in private chats."
  (interactive (list (read-file-name "Video: ")
                     (read-number "Self desctruct in seconds (0-60): ")))
  (telega-chatbuf-attach-video filename ttl))

(defun telega-chatbuf-attach-audio (filename)
  "Attach FILENAME as audio to the chatbuf input."
  (interactive (list (read-file-name "Audio: ")))
  (let ((ifile (telega-chatbuf--gen-input-file filename 'Audio)))
    (telega-chatbuf-input-insert
     (list :@type "inputMessageAudio"
           :audio ifile))))

(defun telega-chatbuf-attach-note-video (filename)
  "Attach FILENAME as (circled) video note to the chatbuf input."
  (interactive (list (read-file-name "Video Note: ")))
  ;; TODO: start video note generation process
  ;; see https://github.com/tdlib/td/issues/126
  (let ((ifile (telega-chatbuf--gen-input-file filename 'VideoNote)))
    (telega-chatbuf-input-insert
     (list :@type "inputMessageVideoNote"
           :video_note ifile))))

(defun telega-chatbuf-attach-note-voice (filename)
  "Attach FILENAME as voice note to the chatbuf input."
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
  "Attach clipboard image to the chatbuf as photo.
If `\\[universal-argument]' is given, then attach clipboard as document."
  (interactive "P")
  (let* ((selection-coding-system 'no-conversion) ;for rawdata
         (temporary-file-directory telega-temp-dir)
         (tmpfile (telega-temp-name "clipboard" ".png"))
         (coding-system-for-write 'binary))
    (if (eq system-type 'darwin)
        (progn
          ;; NOTE: On MacOS, try extracting clipboard using pngpaste
          (unless (executable-find "pngpaste")
            (error "Please install pngpaste to paste images"))
          (unless (= 0 (telega-screenshot-with-pngpaste tmpfile))
            (error "No image in CLIPBOARD")))
      (write-region (or (gui-get-selection 'CLIPBOARD 'image/png)
                        (error "No image in CLIPBOARD"))
                    nil tmpfile nil 'quiet))
    (telega-chatbuf--attach-tmp-photo tmpfile doc-p)))

(defun telega-chatbuf-attach-screenshot (&optional n chat)
  "Attach screenshot to the chatbuf input.
If numeric prefix arg N is given, then take screenshot in N seconds.
If `\\[universal-argument]' is given, then take screenshot of the screen area.
Multiple `\\[universal-argument]' increases delay before taking
screenshot of the area.
Uses `telega-screenshot-function' to take a screenshot."
  (interactive (list (or current-prefix-arg 1) telega-chatbuf--chat))

  ;; NOTE: use float N value as special, to make screenshot of the
  ;; area, `log' returns float
  (when (listp n)
    (setq n (log (car n) 4)))

  (if (and (> n 0)
           ;; NO delays for "pngpaste"
           (not (eq telega-screenshot-function
                    'telega-screenshot-with-pngpaste)))
      (progn
        (message "Telega: taking screenshot in %d seconds" n)
        (run-with-timer 1 nil 'telega-chatbuf-attach-screenshot (1- n) chat))

    ;; Make a screenshot
    (message nil)
    (let* ((temporary-file-directory telega-temp-dir)
           (tmpfile (telega-temp-name "screenshot" ".png")))
      (funcall telega-screenshot-function tmpfile (floatp n))
      (when (file-exists-p tmpfile)
        ;; NOTE: Screenshot successfully taken
        (telega-chat--pop-to-buffer chat)
        (x-focus-frame (window-frame (get-buffer-window)))
        (telega-chatbuf--attach-tmp-photo tmpfile)))))

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
  "Attach a sticker.
If `\\[universal-argument]' is given, then attach recent or
favorite sticker.  Otherwise choose a sticker from installed
sticker sets."
  (interactive "P")
  (if fav-or-recent-p
      (telega-sticker-choose-favorite-or-recent telega-chatbuf--chat)

    ;; Select some stickerset
    (let ((sset (telega-stickerset-completing-read "Sticker set: "))
          (chat telega-chatbuf--chat)
          (tss-buffer (get-buffer "*Telegram Sticker Set*")))
      (when (or (not (buffer-live-p tss-buffer))
                (not (with-current-buffer tss-buffer
                       (and (eq telega-help-win--stickerset sset)
                            (eq telega--chat chat)))))
        (telega-describe-stickerset sset telega-chatbuf--chat))

      (select-window
       (temp-buffer-window-show tss-buffer)))))

(defun telega-chatbuf-attach-animation (&optional from-file-p)
  "Attach an animation.
If `\\[universal-argument]' is given, then attach animation from
file, Otherwise choose animation from list of saved animations."
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

(defun telega-chatbuf-attach-poll (question non-anonymous allow-multiple-answers
                                            &rest options)
  "Attach poll to the chatbuf input.
Can be used only in group chats.
QUESTION - Title of the poll.
NON-ANONYMOUS - Non-nil to create non-anonymous poll.
ALLOW-MULTIPLE-ANSWERS - Non-nil to allow multiple answers.
OPTIONS - List of strings representing poll options."
  (interactive
   (let ((poll-q (read-string
                  (concat (telega-i18n "polls_public")
                          " "
                          (telega-i18n "polls_create_question")
                          ": ")))
         (optidx 1) opt poll-opts)
     (while (not (string-empty-p
                  (setq opt (read-string
                             (format "Option %d): " optidx)))))
       (setq poll-opts (append poll-opts (list opt)))
       (cl-incf optidx))
     (nconc (list poll-q
                  (y-or-n-p
                   (concat (telega-i18n "polls_create_anonymous") "? "))
                  (y-or-n-p
                   (concat (telega-i18n "polls_create_multiple_choice") "? ")))
            poll-opts)))

  (telega-chatbuf-input-insert
   (list :@type "inputMessagePoll"
         :question question
         :is_anonymous (if non-anonymous :false t)
         :type (list :@type "pollTypeRegular"
                     :allow_multiple_answers allow-multiple-answers)
         :options (apply 'vector options))))

(defun telega-chatbuf-attach-scheduled (timestamp)
  "Mark content as scheduled.
Send following message at TIMESTAMP.
If `\\[universal-argument]' is given and chat is private and
online status of the corresponding user is known, then send
message when user gets online."
  (interactive (list (unless current-prefix-arg
                       (telega-read-timestamp "Send time: "))))

  (telega-chatbuf-input-insert
   (list :@type "telegaScheduledMessage"
         :timestamp timestamp))
  (telega-momentary-display
   (propertize (telega-i18n "telega_scheduled_help") 'face 'shadow)))

(defun telega-chatbuf-attach-toggle-disable-notification (disable-p)
  "Toggle disable-notification chat option for the subsequent chatbuf input.
Use this attachment to disable/enable notification on the receiver side."
  (interactive
   (list (not (plist-get telega-chatbuf--chat :default_disable_notification))))

  (telega-chatbuf-input-insert
   (list :@type "telegaDisableNotification"
         :disable_notification disable-p))
  (telega-momentary-display
   (propertize (telega-i18n (if disable-p
                                "telega_disable_notification_help"
                              "telega_enable_notification_help"))
               'face 'shadow)))

(defun telega-chatbuf-attach-disable-webpage-preview ()
  "Disable webpage preview for the following text message."
  (interactive)
  (telega-chatbuf-input-insert
   (list :@type "telegaDisableWebpagePreview"))
  (telega-momentary-display
   (propertize (telega-i18n "telega_disable_webpage_preview_help")
               'face 'shadow)))

(defun telega-chatbuf-attach-dice (emoji)
  "Attach random dice roll message."
  (interactive (list (funcall telega-completing-read-function
                              "Dice Emoji: "
                              telega--dice-emojis nil t)))
  (telega-chatbuf-input-insert
   (list :@type "inputMessageDice"
         :emoji emoji
         :clear_draft t)))

(defun telega-chatbuf-attach-markup (markup-name)
  "Attach text using MARKUP-NAME into chatbuf.
Using this type of attachment it is possible to intermix multiple
markups in the chatbuf input.
Markups are defined in the `telega-chat-markup-functions' user option."
  (interactive (list (funcall telega-completing-read-function
                              "Markup: "
                              (mapcar #'car telega-chat-markup-functions)
                              nil t)))
  (let ((markup-func (cdr (assoc markup-name telega-chat-markup-functions))))
    (telega-chatbuf-input-insert
     (telega-string-as-markup "" markup-name markup-func))
    (backward-char 1)))

(defun telega-chatbuf-attach (attach-type)
  "Attach something to the chatbuf input.
`\\[universal-argument]' is passed directly to the attachment function.
See `telega-chat-attach-commands' for available attachment types."
  (interactive
   (list (funcall telega-completing-read-function
                  "Attachment type: "
                  (mapcar #'car (cl-remove-if-not
                                 (lambda (cmdesc)
                                   (let ((avail-func (cadr cmdesc)))
                                     (or (not avail-func)
                                         (funcall avail-func))))
                                 telega-chat-attach-commands))
                  nil t)))

  (let ((cmd (nth 2 (assoc attach-type telega-chat-attach-commands))))
    (cl-assert (commandp cmd))
    (call-interactively cmd)))

(defun telega-buffer-file-send (file chat &optional as-photo-p)
  "Prepare FILE to be sent as document or photo to CHAT.
If `\\[universal-argument]' is specified, then always send as a file.
Otherwise for `image-mode' major-mode, send file as photo.
If called interactively, then file associated with current buffer
is used as FILE."
  (interactive
   (let ((send-photo-p (and (not current-prefix-arg)
                            (derived-mode-p 'image-mode))))
     (list (or (buffer-file-name)
               (user-error (concat "Can't send current buffer, "
                                   "it does not have corresponding file")))
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
                   :text (telega-string-fmt-text
                          (telega-chatbuf-input-string)))))))

  ;; NOTE: temporary move point out of prompt, so newly incoming
  ;; messages won't get automatically read
  (when (>= (point) telega-chatbuf--input-marker)
    (goto-char (ewoc-location (ewoc--footer telega-chatbuf--ewoc)))
    (setq telega-chatbuf--refresh-point t)))

(defun telega-chatbuf--switch-in ()
  "Called when switching to chat buffer."
  (telega-debug "Switch %s: %s" (propertize "IN" 'face 'bold)
                (buffer-name))
  (telega--openChat telega-chatbuf--chat)

  ;; Recover point position, saved in `telega-chatbuf--switch-out' In
  ;; case point was saved, then jump to last unread message, just as
  ;; if chat was freshly opened.  The only difference is that we jump
  ;; to the prompt only if last unread message is fully viewable
  (when telega-chatbuf--refresh-point
    (let ((rpoint telega-chatbuf--refresh-point))
      (setq telega-chatbuf--refresh-point nil)
      (if (eq rpoint t)
          (telega-chatbuf-next-unread
            (lambda (button)
              (telega-chatbuf--view-msg-at button)
              (when (and (eq (telega-msg-at button) (telega-chatbuf--last-msg))
                         (telega-button--observable-p button))
                (goto-char (point-max)))

              ;; NOTE: If point at bottom half of the window, then
              ;; move it up a bit, so point won't stuck at the bottom
              (unless (pos-visible-in-window-p (point-max))
                (when (> (- (line-number-at-pos)
                            (line-number-at-pos (window-start)))
                         (/ (window-height) 2))
                  (recenter)))))
        (goto-char rpoint))))

  ;; May affect rootbuf sorting if `chatbuf-recency' criteria is used
  (when (memq 'chatbuf-recency telega--sort-criteria)
    (telega-chat--update telega-chatbuf--chat
                         (list :@type "telegaChatReorder")))
  )

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

  (setq telega--chat-buffers-alist
        (assq-delete-all telega-chatbuf--chat telega--chat-buffers-alist))

  ;; NOTE: chatbuffer might be left from other telega start, so it
  ;; will throw "Ewoc node not found" error - ignore it
  (ignore-errors
    (telega-chat--update telega-chatbuf--chat)))

;;;###autoload
(defun telega-chatbuf-input-as-region-advice (orig-region-func start end &rest args)
  "Advice for commands accepting region.
If point is inside telega chatbuf input, then call region command
with input prompt as region."
  (when (and (not (region-active-p))
             (derived-mode-p 'telega-chat-mode)
             (>= (point) telega-chatbuf--input-marker))
    (setq start telega-chatbuf--input-marker
          end (point-max)))
  (apply orig-region-func start end args))

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
     :inserter #'telega-ins--prompt-aux-reply
     'invisible nil)

    (telega-chatbuf--prompt-update telega-chat-reply-prompt)
    (goto-char (point-max))

    (telega-help-message--cancel-aux 'reply)))

(defun telega-msg-edit (msg)
  "Start editing the MSG."
  (interactive (list (telega-msg-at (point))))

  (unless (plist-get msg :can_be_edited)
    (error "Message can't be edited"))

  (with-telega-chatbuf (telega-msg-chat msg)
    ;; Allow editing deleted messages as new one
    ;; See https://github.com/zevlg/telega.el/issues/194
    (if (plist-get msg :telega-is-deleted-message)
        (telega-chatbuf-cancel-aux)

      (telega-button--update-value
       telega-chatbuf--aux-button msg
       :inserter #'telega-ins--prompt-aux-edit
       'invisible nil)

      (telega-chatbuf--prompt-update telega-chat-edit-prompt))

    ;; Replace any input text with edited message
    (delete-region telega-chatbuf--input-marker (point-max))
    (goto-char (point-max))

    ;; Insert message's text or attachment caption
    (let ((content (plist-get msg :content)))
      (telega-ins--fmt-text-as-markdown
       (or (plist-get content :text)
           (plist-get content :caption))))

    (telega-help-message--cancel-aux 'edit)))

(defun telega-chatbuf-attach-fwd-msg (msg &optional send-copy-p rm-cap-p)
  "Attach MSG as foward message into chatbuf's input."
  (telega-chatbuf-input-insert
   (list :@type "telegaForwardMessage"
         :message msg
         :send_copy send-copy-p
         :remove_caption rm-cap-p
         :unmark-after-sent (telega-msg-marked-p msg)))

  (when (and send-copy-p rm-cap-p)
    (telega-momentary-display
     (propertize (telega-i18n "telega_forward_new_caption_help")
                 'face 'shadow)))
  )

(defun telega-msg-forward-marked-or-at-point (&optional send-copy-p rm-cap-p)
  "Forward marked messages or message at point.
If C-u is given, then forward message copy.
If C-u C-u is given, then forward message copy without caption."
  (interactive (list current-prefix-arg
                     (> (prefix-numeric-value current-prefix-arg) 4)))
  (when-let ((messages (or (reverse telega-chatbuf--marked-messages)
                           (when-let ((msg-at-point (telega-msg-at (point))))
                             (list msg-at-point)))))
    (let ((chat (telega-completing-read-chat
                 (concat "Forward"
                         (when send-copy-p " Copy")
                         (when rm-cap-p " NoCap")
                         (when (> (length messages) 1)
                           (format " (%d marked)" (length messages)))
                         " to: "))))
      (telega-chat--pop-to-buffer chat)
      (with-telega-chatbuf chat
        (goto-char (point-max))
        (dolist (msg messages)
          (telega-chatbuf-attach-fwd-msg msg send-copy-p rm-cap-p))))))

(defun telega-msg-delete0 (msg &optional revoke)
  (cl-assert (eq (telega--tl-type msg) 'message))
  (if (plist-get msg :telega-is-deleted-message)
      ;; NOTE: `d' is pressed on deleted message
      ;; (`telega-chat-show-deleted-messages-for' is non-nil)
      ;; Generate pseudo-event to delete the message
      (let ((telega-chat-show-deleted-messages-for nil))
        (telega--on-updateDeleteMessages
         (list :chat_id (plist-get msg :chat_id)
               :is_permanent t
               :message_ids (vector (plist-get msg :id)))))

    (telega--deleteMessages
     (plist-get msg :chat_id) (list (plist-get msg :id)) revoke)))

(defun telega-chatbuf-marked-messages-delete (revoke)
  "Delete marked messages in chatbuf."
  (interactive (not current-prefix-arg))
  (when-let ((marked-messages telega-chatbuf--marked-messages))
    (when (yes-or-no-p (telega-i18n (if revoke
                                        "telega_query_revoke_marked_messages"
                                      "telega_query_kill_marked_messages")
                         :count (length marked-messages)))
      (setq telega-chatbuf--marked-messages nil)
      (dolist (msg marked-messages)
        (telega-msg-delete0 msg revoke))
      (telega-chatbuf-mode-line-update))))

(defun telega-msg-delete-marked-or-at-point (revoke)
  "Deletes some messages.
If some messages are marked, then delete them.
Otherwise delete message at point.
With prefix arg delete only for yourself."
  (interactive (list (not current-prefix-arg)))

  (if telega-chatbuf--marked-messages
      (telega-chatbuf-marked-messages-delete revoke)

    (when-let ((msg (telega-msg-at (point))))
      (if (plist-get msg :telega-is-deleted-message)
          ;; Purge already deleted message
          (telega-msg-delete0 msg)

        (when (y-or-n-p (telega-i18n (if revoke
                                         "telega_query_revoke_message"
                                       "telega_query_kill_message")))
          (telega-msg-delete0 msg revoke)

          (if (telega-chat-match-p telega-chatbuf--chat
                                   telega-chat-show-deleted-messages-for)
              (telega-help-message 'double-delete
                  "Press %s once again to hide deleted message"
                (substitute-command-keys (format "\\[%S]" this-command)))
            (telega-help-message 'show-deleted
                "JFYI see `telega-chat-show-deleted-messages-for'")))))))

(defun telega-chatbuf-complete ()
  "Complete thing at chatbuf input."
  (interactive)
  (or (when (functionp telega-chat-input-complete-function)
        (funcall telega-chat-input-complete-function))
      (call-interactively 'telega-chatbuf-attach-sticker-by-emoji)
      (when (and (boundp 'company-mode) company-mode)
        (when-let ((backend (cond ((telega-company-grab-username)
                                   'telega-company-username)
                                  ((telega-company-grab-emoji)
                                   'telega-emoji-company-backend)
                                  ((telega-company-grab-hashtag)
                                   'telega-company-hashtag)
                                  ((telega-company-grab-botcmd)
                                   'telega-company-botcmd)
                                  )))
          (company-begin-backend backend)
          (company-complete)
          t))
      (call-interactively 'telega-chatbuf-attach-inline-bot-query)
      ;; TODO: add other completions
      ))

(defun telega-chatbuf-next-link (n)
  (interactive "p")
  ;; TODO: maybe be more smarter about links
  (telega-button-forward n
    (lambda (button)
      (not (eq (button-type button) 'telega-prompt)))))

(defun telega-chatbuf-prev-link (n)
  (interactive "p")
  (when (<= telega-chatbuf--input-marker (point))
    (goto-char (ewoc-location (ewoc--footer telega-chatbuf--ewoc))))
  (telega-chatbuf-next-link (- n)))

(defun telega-chatbuf-complete-or-next-link ()
  "Complete username at point, or jump to next link."
  (interactive)
  (if (<= telega-chatbuf--input-marker (point))
      (call-interactively 'telega-chatbuf-complete)
    (call-interactively 'telega-chatbuf-next-link)))

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

(defun telega-chatbuf--goto-msg (msg-id &optional highlight)
  "In chatbuf goto message denoted by MSG-ID.
If HIGHLIGHT is non-nil, then momentary highlight the message.
Return non-nil on success."
  (when-let ((node (telega-chatbuf--node-by-msg-id msg-id)))
    (ewoc-goto-node telega-chatbuf--ewoc node)

    ;; NOTE: node could exists, where button is not!  This is true for
    ;; ignored messages if `telega-ignored-messages-visible' is nil
    (when-let ((msg-button (button-at (point))))
      (telega-button--make-observable msg-button)
      (when highlight
        (cl-assert (eq (button-type msg-button) 'telega-msg))
        (with-no-warnings
          (pulse-momentary-highlight-region
           (button-start msg-button) (button-end msg-button)))))
    t))

(defun telega-chat--goto-msg (chat msg-id &optional highlight callback)
  "In CHAT goto message denoted by MSG-ID.
If HIGHLIGHT is non-nil then highlight with fading background color.
This call is asynchronous, and might require history fetching.
CALLBACK is called after point is moved to the message with MSG-ID."
  (declare (indent 3))
  ;; NOTE: Avoid ellit-org from extracting this by using ";;;" instead
  ;; of ";;"
  ;;; 1. Put message at point into messages ring
  ;;; 2. If message seen in chatbuf, jump to it
  ;;; 3. Otherwise, fetch history containing message and jump to it
  (with-current-buffer (telega-chat--pop-to-buffer chat :no-history)
    (when (ring-p telega-chatbuf--messages-ring)
      (when-let ((msg-at-point (telega-msg-at (point))))
        (ring-insert telega-chatbuf--messages-ring msg-at-point)

        (telega-help-message 'msg-ring-pop "%s to jump back"
          (telega-keys-description
           'telega-chatbuf-goto-pop-message telega-chat-mode-map))
        ))

    ;; NOTE: message with MSG-ID might be already deleted, so load
    ;; history only if:
    ;;   - MSG-ID is less then id of the first message shown in
    ;;     chatbuf and older history might be loaded
    ;;   - MSG-ID is greater then id of the last message shown in
    ;;     chatbuf and newer history might be loaded
    (if (or (telega-chatbuf--goto-msg msg-id highlight)
            (when-let ((first-msg (telega-chatbuf--first-msg))
                       (last-msg (telega-chatbuf--last-msg)))
              (not (or (and (< msg-id (plist-get first-msg :id))
                            (telega-chatbuf--need-older-history-p))
                       (and (> msg-id (plist-get last-msg :id))
                            (telega-chatbuf--need-newer-history-p))))))
        (when callback
          (funcall callback))

      (telega-chatbuf--clean)
      (telega-chat--load-history
          chat msg-id (- (/ telega-chat-history-limit 2)) nil
        (lambda (_ignored)
          (telega-chatbuf--goto-msg msg-id highlight)
          (when callback
            (funcall callback)))))))

(defun telega-chat-avatar-image (chat)
  "Return avatar for the CHAT."
  (let ((photo (plist-get chat :photo)))
    (telega-media--image
     (cons chat 'telega-avatar--create-image)
     (cons photo :small))))

(defun telega-chat-avatar-image-one-line (chat)
  "Return avatar for the CHAT for one line use."
  (let ((photo (plist-get chat :photo)))
    (telega-media--image
     (cons chat 'telega-avatar--create-image-one-line)
     (cons photo :small)
     nil :telega-avatar-1)))

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
         (let ((doc-p (or (not (image-type-from-file-name real-name))
                          (y-or-n-p (telega-i18n "telega_query_dnd_photo_as_file")))))
           (telega-chatbuf--attach-tmp-photo real-name doc-p)))
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


(defconst telega-chat--filters
  (list (list "scheduled" #'telega-chatbuf-filter-scheduled)
        (list "search" "searchMessagesFilterEmpty")
        (list "by-sender" #'telega-chatbuf-filter-by-sender)
        (list "hashtag" #'telega-chatbuf-filter-hashtag)
        (list "photo" "searchMessagesFilterPhoto")
        (list "photo-video" "searchMessagesFilterPhotoAndVideo")
        (list "url" "searchMessagesFilterUrl")
        (list "doc" "searchMessagesFilterDocument")
        (list "file" "searchMessagesFilterDocument")
        (list "gif" "searchMessagesFilterAnimation")
        (list "audio" "searchMessagesFilterAudio")
        (list "video" "searchMessagesFilterVideo")
        (list "voice-note" "searchMessagesFilterVoiceNote")
        (list "video-note" "searchMessagesFilterVideoNote")
        (list "voice-video-note" "searchMessagesFilterVoiceAndVideoNote")
        (list "chat-photo" "searchMessagesFilterChatPhoto")
        (list "call" "searchMessagesFilterCall")
        (list "missed-call" "searchMessagesFilterMissedCall")
        (list "mention" "searchMessagesFilterMention")
        (list "unread-mention" "searchMessagesFilterUnreadMention")
        (list "failed-to-send" "searchMessagesFilterFailedToSend")
        ))

(defun telega-chatbuf-filter (filter-name &optional query by-sender-p)
  "Enable chat messages filtering.
Enables FILTER-NAME filter.
QUERY is only used by \"search\" messages filter.
If `\\[universal-argument]' is specified, then filter messages
sent by sender of the message at point.
Not all filters can filter messages by sender."
  (interactive (let ((fname (funcall telega-completing-read-function
                                     "Chat Messages Filter: "
                                     (mapcar #'car telega-chat--filters)
                                     nil t)))
                 (list fname (if (string= fname "search") ;XXX
                                 (read-string "Search Query: ")
                               "")
                       current-prefix-arg)))

  (let ((msg-filter (assoc filter-name telega-chat--filters)))
    (cond ((null msg-filter)
           ;; NOTE: if point is at some message, then keep this
           ;; message visible, otherwise load initial history
           (if-let ((msg-at-point (telega-msg-at (point))))
               (progn
                 (telega-chatbuf--clean)
                 (telega-chat--goto-msg
                  telega-chatbuf--chat (plist-get msg-at-point :id)))
             (telega-chatbuf--load-initial-history))
           (telega-chatbuf-mode-line-update))

          ((commandp (cadr msg-filter) 'for-interactive)
           (call-interactively (cadr msg-filter)))

          (t
           (cl-assert (member (cadr msg-filter)
                              '("searchMessagesFilterEmpty"
                                "searchMessagesFilterAnimation"
                                "searchMessagesFilterAudio"
                                "searchMessagesFilterDocument"
                                "searchMessagesFilterPhoto"
                                "searchMessagesFilterVideo"
                                "searchMessagesFilterVoiceNote"
                                "searchMessagesFilterPhotoAndVideo"
                                "searchMessagesFilterUrl"
                                "searchMessagesFilterChatPhoto"
                                "searchMessagesFilterCall"
                                "searchMessagesFilterMissedCall"
                                "searchMessagesFilterVideoNote"
                                "searchMessagesFilterVoiceAndVideoNote"
                                "searchMessagesFilterMention"
                                "searchMessagesFilterUnreadMention"
                                "searchMessagesFilterFailedToSend")))
           (let ((sender (when (and by-sender-p
                                    (not (telega-me-p telega-chatbuf--chat))
                                    (not (telega-chat-secret-p
                                          telega-chatbuf--chat)))
                           (telega-completing-read-user "Sent by: "))))
             (telega-chatbuf--clean)
             (setq telega-chatbuf--filter
                   (list (if (string-empty-p query)
                             (nth 0 msg-filter)
                           (concat (nth 0 msg-filter) " \"" query "\""))
                         (nth 1 msg-filter) query sender nil))
             (telega-chatbuf-mode-line-update)

             (telega-chatbuf--load-older-history
              (lambda (total-messages)
                (when telega-chatbuf--filter
                  (setf (nth 4 telega-chatbuf--filter) total-messages)
                  (telega-chatbuf--footer-redisplay))))

             (telega-help-message 'msg-filter-cancel
                 "%s to cancel messages filtering"
               (telega-keys-description
                'telega-chatbuf-filter-cancel telega-chat-mode-map))
             ))
          ))

  (telega-chatbuf--footer-redisplay))

(defun telega-chatbuf-filter-by-sender ()
  "Show only messages send by some user."
  (interactive)
  (telega-chatbuf-filter "search" "" 'by-sender))

(defun telega-chatbuf-filter-hashtag (hashtag &optional by-sender-p)
  "Show only messages marked with HASHTAG."
  (interactive (list (funcall telega-completing-read-function
                              "Hashtag: #" (telega--searchHashtags ""))
                     current-prefix-arg))
  (telega-chatbuf-filter "search" (concat "#" hashtag) by-sender-p))

(defun telega-chatbuf-filter-scheduled ()
  "Show only scheduled messages."
  (interactive)

  (let ((scheduled-messages
         (telega--getChatScheduledMessages telega-chatbuf--chat)))
    (telega-chatbuf--clean)
    (setq telega-chatbuf--filter
          (list "scheduled" nil nil nil (length scheduled-messages)))
    (telega-chatbuf-mode-line-update)

    (telega-chatbuf--append-messages (nreverse scheduled-messages))))

(defun telega-chatbuf-filter-search (&optional query by-sender-p)
  "Interactively search for messages in chatbuf.
If non-nil BY-SENDER-P is specified by `\\[universal-argument]',
then also search by sender."
  (interactive "sSearch Query: \nP")
  (telega-chatbuf-filter "search" query by-sender-p))

(defun telega-chatbuf-filter-cancel (&rest _ignored)
  "Cancel any message filtering.
If point is at some message, then keep point on this message after reseting."
  (interactive)
  (when telega-chatbuf--filter
    (telega-chatbuf-filter nil)))


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

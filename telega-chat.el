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
;; to send.  Press
;; {{{where-is(telega-describe-message,telega-msg-button-map)}}} to
;; get detailed description of the message at point.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'ring)
(require 'url-util)
(require 'seq)
(require 'dired)                        ; dired-get-marked-files

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
(require 'telega-filter)
(require 'telega-modes)

(eval-when-compile
  (require 'rx)
  (require 'pcase))                     ;`pcase-let*' and `rx'

;; shutup compiler
(declare-function company-complete-common "company")
(declare-function company-begin-backend "company" (backend &optional callback))

;; telega-tdlib-events.el depends on telega-chat.el
(declare-function telega--on-updateDeleteMessages "telega-tdlib-events" (event))
(declare-function telega-chat--update "telega-tdlib-events" (chat &rest events))

;; telega-info.el depends on telega-chat.el
(declare-function telega--info "telega-info" (tlobj-type tlobj-id &optional locally-p))
(declare-function telega--full-info "telega-info" (tlobj &optional offline-p _callback))

;; telega-root.el depends on telega-chat.el
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
Could contain `older-loaded' or `newer-loaded' elements.")
(make-variable-buffer-local 'telega-chatbuf--history-state)

(defvar telega-chatbuf--voice-msg nil
  "Active (playing/paused) voice note message for the current chat.")
(make-variable-buffer-local 'telega-chatbuf--voice-msg)

(defvar telega-chatbuf--my-action nil
  "My current action in chat buffer.")
(make-variable-buffer-local 'telega-chatbuf--my-action)

(defvar telega-chatbuf--refresh-point nil
  "Non-nil if point needs refreshing on buffer switch in.
Specifies point position inside chatbuf input prompt.")
(make-variable-buffer-local 'telega-chatbuf--refresh-point)

(defvar telega-chatbuf--msg-filter nil
  "Active messages filter in the chatbuf.
Plist with properties:
- `:title' displayed in  chatbuf footer
- `:tdlib-msg-filter' Could be a function or list representing
  TDLib SearchMessagesFilter.
- `:query' - query argument to `telega--searchChatMessages'
- `:sender' - sender argument to `telega--searchChatMessages'
- `:total-count' - Number of total messages found.")
(make-variable-buffer-local 'telega-chatbuf--msg-filter)

(defvar telega-chatbuf--thread-msg nil
  "MSG that starts a thread in the chatbuf.")
(make-variable-buffer-local 'telega-chatbuf--thread-msg)
(defvar telega-chatbuf--thread-info nil
  "Last thread info received by `telega--getMessageThread'.
Used to determine all older thread history has been loaded.")
(make-variable-buffer-local 'telega-chatbuf--thread-info)

(defvar telega-chatbuf--messages-compact-view nil
  "Non-nil to use compact view for messages in chatbuf.")
(make-variable-buffer-local 'telega-chatbuf--messages-compact-view)
(defvar telega-chatbuf--inhibit-reset-filter-and-thread nil
  "Bind this variable to inhibit
`telega-chatbuf--reset-filter-and-thread' on message goto.")

(defvar telega-chatbuf--messages-pop-ring nil
  "History of messages jumps.
Used for `M-g x' command.")
(make-variable-buffer-local 'telega-chatbuf--messages-pop-ring)

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
If OFFLINE-P is non-nil then do not request the telega-server."
  (let ((chat (gethash chat-id telega--chats)))
    (when (and (not chat) (not offline-p))
      (setq chat (telega--getChat chat-id))
      (cl-assert chat nil "getChat timed out chat_id=%d" chat-id)
      (telega-chat--ensure chat))
    chat))

(defun telega-chat-by-username (username)
  "Find chat by its USERNAME."
  (cl-find username telega--ordered-chats
           :test #'string= :key #'telega-chat-username))

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
                (telega-user-bot-p (telega-chat-user chat 'inc-bots)))
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
    (telega-user-get (telega--tl-get chat :type :user_id))))

(defun telega-chat-admin-get (chat user)
  "Return \"chatAdministrator\" structure for the USER.
Return nil if USER not administrator in the CHAT.
Works only for chats with active chatbuffer and fetched
administrators list."
  (with-telega-chatbuf chat
    (cl-find (plist-get user :id) telega-chatbuf--administrators
             :key (telega--tl-prop :user_id))))

(defun telega-chat-member-my-status (chat)
  "Return my status as Chat Member Status for the CHAT.
Only available for basicgroup and supergroup (including channels)."
  (when (memq (telega-chat--type chat 'raw) '(basicgroup supergroup))
    (plist-get (telega-chat--info chat) :status)))

(defun telega-chat-member-my-permissions (chat)
  "Return my member permissions in the CHAT."
  (let ((perms (copy-sequence (cddr (plist-get chat :permissions)))))
    (when-let ((status (telega-chat-member-my-status chat)))
      (cl-case (telega--tl-type status)
        ((chatMemberStatusCreator chatMemberStatusAdministrator)
         ;; NOTE: Owner of the chat has all the admins privs except
         ;; for `:is_anonymous' which is set separately
         (let ((owner-p (eq 'chatMemberStatusCreator (telega--tl-type status))))
           (dolist (perm-spec telega-chat--admin-permissions)
             (plist-put perms (car perm-spec)
                        (or owner-p (plist-get status (car perm-spec))))))
         (plist-put perms :is_anonymous (plist-get status :is_anonymous)))
        (chatMemberStatusRestricted
         (setq perms (plist-get status :permissions)))))
    perms))

(defun telega-chat-title (chat &optional with-username-delim)
  "Return title for the CHAT.
If WITH-USERNAME-DELIM is specified, append username to the title
delimiting with WITH-USERNAME-DELIM."
  (let* ((telega-emoji-use-images telega-chat-title-emoji-use-images)
         (title (or (when (telega-me-p chat)
                      (telega-i18n "lng_saved_messages"))
                    (when (telega-replies-p chat)
                      (telega-i18n "lng_replies_messages"))
                    (telega-tl-str chat :title)
                    (progn
                      (cl-assert (telega-chat-user chat 'inc-bots))
                      (telega-user-title
                       (telega-chat-user chat 'inc-bots) 'name)))))
    (when with-username-delim
      (when-let ((username (telega-chat-username chat)))
        (setq title (concat title (if (stringp with-username-delim)
                                      with-username-delim
                                    " ")
                            "@" username))))

    (if-let ((cctfun (cdr (cl-find chat telega-chat-title-custom-for
                                   :test #'telega-chat-match-p
                                   :key #'car))))
        (progn
          (cl-assert (functionp cctfun))
          (funcall cctfun title))
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

(defun telega-chat-reply-markup-msg (chat &optional callback)
  "Return reply markup for the CHAT."
  (declare (indent 1))
  (let ((reply-markup-msg-id (plist-get chat :reply_markup_message_id)))
    (unless (zerop reply-markup-msg-id)
      (telega-msg-get chat reply-markup-msg-id callback))))

(defun telega-chatbuf--reply-markup-message-fetch ()
  "Asynchronously load reply markup message for CHAT.
Pass non-nil OFFLINE-P argument to avoid any async requests."
  (let* ((chat telega-chatbuf--chat)
         (reply-markup-msg-id (plist-get chat :reply_markup_message_id)))
    (if (zerop reply-markup-msg-id)
        (telega-chatbuf--footer-update)

      ;; Async load reply markup message
      (telega-chat-reply-markup-msg chat
        (lambda (rm-message &optional offline-p)
          (unless offline-p
            (telega-msg-cache
             (or rm-message
                 ;; deleted message
                 (list :id reply-markup-msg-id
                       :chat_id (plist-get chat :id)
                       :telega-is-deleted-message t))))
          (with-telega-chatbuf chat
            (telega-chatbuf--footer-update)))))))

(defun telega-chatbuf--admins-fetch ()
  "Asynchronously fetch and update `telega-chatbuf--administrators'."
  (cl-assert telega-chatbuf--chat)

  ;; NOTE: fetch admins not frequent as one time in a minute, to avoid
  ;; admins fetch/supergroup full-info update loop, see
  ;; https://github.com/tdlib/td/issues/1284
  ;;
  ;; We store last update time in `admins' entry in
  ;; `telega-chatbuf--fetch-alist'
  ;;
  ;; Also, admin right is required to to get admins list in channels
  (let ((chat telega-chatbuf--chat)
        (last-fetch-time (or (alist-get 'admins telega-chatbuf--fetch-alist) 0))
        (current-time (time-to-seconds)))
    (when (and (> (- current-time last-fetch-time) 60)
               (not (telega-chat-private-p chat 'inc-bots))
               (not (telega-chat-secret-p chat))
               (or (not (telega-chat-channel-p chat))
                   (telega-chat-match-p chat '(me-is-owner or-admin))))
      (setf (alist-get 'admins telega-chatbuf--fetch-alist) current-time)
      (telega--getChatAdministrators chat
        (lambda (admins)
          (with-telega-chatbuf chat
            (setq telega-chatbuf--administrators admins)))))
    ))

(defun telega-chatbuf--pinned-messages-fetch ()
  "Asynchronously fetch pinned messages for chatbuf."
  (let ((chat telega-chatbuf--chat))
    (telega--searchChatMessages telega-chatbuf--chat
        (list :@type "searchMessagesFilterPinned") "" 0 0 nil nil
      (lambda (reply)
        (let ((msgs (plist-get reply :messages)))
          (plist-put chat :telega-pinned-messages (append msgs nil))

          ;; Possible clamp index of pinned message displayed in
          ;; modeline
          (let ((pin-idx (plist-get chat :telega-pinned-message-index)))
            (unless (and pin-idx (< pin-idx (length msgs)))
              (plist-put chat :telega-pinned-message-index 0)))

          (with-telega-chatbuf chat
            (telega-chatbuf--modeline-update)))))))

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
    (define-key map (kbd "P") 'telega-chat-toggle-pin)
    (define-key map (kbd "^") 'telega-chat-toggle-pin)
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

(defun telega-chat-toggle-pin (chat)
  "Toggle chat's pin state at point."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))
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
  (interactive
   (let ((chat (or telega-chatbuf--chat (telega-chat-at (point)))))
     (list chat (read-string "Custom Order [empty to unset]: "
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
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))

  ;; NOTE: If calling to secret chat, then use ordinary private chat
  ;; for calling
  (when (telega-chat-secret-p chat)
    (setq chat (telega-chat-get
                (plist-get (telega-chat--info chat) :user_id))))

  (unless (eq (telega-chat--type chat 'no-interpret) 'private)
    (error "Can call only to users"))
  (let* ((user (telega-chat-user chat 'inc-bots))
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
  (unless chat
    (user-error "`telega-chat-share-my-contact' available only in chatbuf"))
  (telega--sendMessage chat (list :@type "inputMessageContact"
                                  :contact (telega-user-as-contact
                                            (telega-user-me)))))

(defun telega-chat-unpin-all-messages (chat)
  "Unpin all messages in the CHAT."
  (interactive (list telega-chatbuf--chat))
  (telega--unpinAllChatMessages chat))

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
    (when (telega-msg-sender-blocked-p chat)
      (telega-ins--with-face 'error
        (telega-ins " " telega-symbol-blocked "BLOCKED")))
    (telega-ins "\n")
    (telega-ins--image chat-ava 1
                       :no-display-if (not telega-chat-show-avatars))

    (telega-ins (capitalize (symbol-name (telega-chat--type chat))) " ")
    (telega-ins--button "Open"
      :value chat
      :action #'telega-chat--pop-to-buffer)
    (when (telega-me-p chat)
      (telega-ins " ")
      (telega-ins--button "Set Profile Photo"
        'action (lambda (_ignored)
                  (let ((photo (read-file-name "Profile Photo: " nil nil t)))
                    (telega--setProfilePhoto photo)))))
    (when (telega-chat-match-p chat '(my-permission :can_invite_users))
      (telega-ins " ")
      (telega-ins--button "Add Member"
        :value chat
        :action (lambda (to-chat)
                  (telega-chat-add-member
                   to-chat (telega-completing-read-user "Add member: ")))))
    (when (telega-chat-match-p chat '(my-permission :can_change_info))
      (telega-ins " ")
      (telega-ins--button "Set Chat Photo"
        :value chat
        :action (lambda (for-chat)
                  (let ((photo (read-file-name "Chat Photo: " nil nil t)))
                    (telega--setChatPhoto for-chat photo)))))

    ;; Archive/Unarchive
    (telega-ins " ")
    (telega-ins--button (if (telega-chat-match-p chat 'archive)
                            (telega-i18n "lng_archived_remove")
                          (telega-i18n "lng_archived_add"))
      :value chat
      :action #'telega-chat-toggle-archive)
    (telega-ins "\n"))

  (telega-ins-fmt "Id: %s\n"
    (if telega-debug
        (format "(telega-chat-get %d)" (plist-get chat :id))
      (format "%d" (plist-get chat :id))))
  (when (telega-chat-public-p chat)
    (let ((link (concat (or (plist-get telega--options :t_me_url)
                            "https://t.me/")
                        (telega-chat-username chat))))
      (insert "Public Link: ")
      (apply #'insert-text-button link (telega-link-props 'url link 'link))
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

  (telega-ins "Default Disable Notification: ")
  (telega-ins--button (if (plist-get chat :default_disable_notification)
                          telega-symbol-heavy-checkmark
                        telega-symbol-blank-button)
    :value chat
    :action (lambda (for-chat)
              (telega--toggleChatDefaultDisableNotification
               for-chat (not (plist-get chat :default_disable_notification)))))
  (telega-ins "\n")
  (telega-ins--help-message
   (telega-ins "Used when you send a message to the chat.\n"
               "Disables message notification on receiver side.\n"
               "Use `C-c C-a "
               (if (plist-get chat :default_disable_notification)
                   "enable-notification"
                 "disable-notification")
               " RET' in chatbuf prompt to temporary "
               (if (plist-get chat :default_disable_notification)
                   "enable"
                 "disable")
               " notifications on receiver side at message send time."))

  ;; Chat notification settings
  (let* ((notify-cfg (plist-get chat :notification_settings))
         (muted-p (telega-chat-muted-p chat))
         (show-preview-p (telega-chat-notification-setting
                          chat :show_preview))
         (disable-pin-msg-p (telega-chat-notification-setting
                             chat :disable_pinned_message_notifications))
         (disable-mentions-p (telega-chat-notification-setting
                             chat :disable_mention_notifications)))
    (telega-ins
     (propertize (telega-i18n "lng_settings_section_notify") 'face 'bold))
    ;; If any custom setting is enabled, then show [Reset] button
    (unless (cl-every (apply-partially #'plist-get notify-cfg)
                      '(:use_default_mute_for
                        :use_default_sound
                        :use_default_show_preview
                        :use_default_disable_pinned_message_notifications
                        :use_default_disable_mention_notifications))
      (telega-ins " ")
      (telega-ins--button "Reset"
        :value chat
        :action (lambda (chat)
                  (telega--setChatNotificationSettings chat
                    :use_default_mute_for t
                    :use_default_sound t
                    :use_default_show_preview t
                    :use_default_disable_pinned_message_notifications t
                    :use_default_disable_mention_notifications t))))
    (telega-ins "\n")
    (telega-ins--labeled "  " nil
      (telega-ins--button (if (not muted-p)
                              telega-symbol-heavy-checkmark
                            telega-symbol-blank-button)
        :value chat
        :action #'telega-chat-toggle-muted)
      (telega-ins " ")
      (telega-ins-fmt "Enabled (%s)"
        (propertize (if (plist-get notify-cfg :use_default_mute_for)
                        "default" "custom")
                    'face 'shadow))
      (unless muted-p
        (telega-ins " ")
        (telega-ins--button "Mute For"
          :value chat
          :action (lambda (chat)
                    (telega-chat-toggle-muted
                     chat (telega-completing-read-mute-for
                           "Disable notifications for: ")))))
      (telega-ins "\n")

      ;; Show Preview
      (telega-ins--button (if show-preview-p
                              telega-symbol-heavy-checkmark
                            telega-symbol-blank-button)
        :value chat
        :action (lambda (chat)
                  (telega--setChatNotificationSettings chat
                    :use_default_show_preview nil
                    :show_preview (if show-preview-p :false t))))
      (telega-ins " ")
      (telega-ins-fmt "Show Preview (%s)"
        (propertize (if (plist-get notify-cfg :use_default_show_preview)
                        "default" "custom")
                    'face 'shadow))
      (telega-ins "\n")

      (telega-ins--button (if disable-pin-msg-p
                              telega-symbol-heavy-checkmark
                            telega-symbol-blank-button)
        :value chat
        :action (lambda (chat)
                  (telega--setChatNotificationSettings chat
                    :use_default_disable_pinned_message_notifications nil
                    :disable_pinned_message_notifications
                    (if disable-pin-msg-p :false t))))
      (telega-ins " ")
      (telega-ins-fmt "Disable Pinned Message Notification (%s)"
        (propertize (if (plist-get notify-cfg :use_default_disable_pinned_message_notifications)
                        "default" "custom")
                    'face 'shadow))
      (telega-ins "\n")

      (telega-ins--button (if disable-mentions-p
                              telega-symbol-heavy-checkmark
                            telega-symbol-blank-button)
        :value chat
        :action (lambda (chat)
                  (telega--setChatNotificationSettings chat
                    :use_default_disable_mention_notifications nil
                    :disable_mention_notifications
                    (if disable-mentions-p :false t))))
      (telega-ins " ")
      (telega-ins-fmt "Disable Mention Notification (%s)"
        (propertize (if (plist-get notify-cfg :use_default_disable_mention_notifications)
                        "default" "custom")
                    'face 'shadow))
      (telega-ins "\n")
      ))

  ;; Permissions for basicgroup and supergroup
  (when (telega-chat-match-p chat '(type basicgroup supergroup))
    (let ((my-perms (telega-chat-member-my-permissions chat)))
      (telega-ins
       (propertize (telega-i18n "lng_manage_peer_permissions") 'face 'bold) "\n")
      (telega-ins
       (propertize (telega-i18n "lng_rights_default_restrictions_header")
                   'face 'shadow) "\n")
      (telega-ins--labeled "  " nil
        (dolist (perm-spec telega-chat--chat-permisions)
          (let ((perm-value (telega--tl-get chat :permissions (car perm-spec))))
            (if (plist-get my-perms :can_restrict_members)
                (telega-ins--button (if perm-value
                                        telega-symbol-heavy-checkmark
                                      telega-symbol-blank-button)
                  :value chat
                  :action (lambda (chat)
                            (telega--setChatPermissions chat
                              (car perm-spec) (not perm-value))))
              (telega-ins (if perm-value
                              telega-symbol-ballout-check
                            telega-symbol-ballout-empty)))
            (telega-ins " " (telega-i18n (cdr perm-spec)))
            (telega-ins "\n"))))))

  (telega-ins "\n")
  (let ((info-spec
         (assq (telega-chat--type chat 'no-interpret)
               '((private "User" telega-info--insert-user)
                 (secret "SecretChat" telega-info--insert-secretchat)
                 (basicgroup "BasicGroup" telega-info--insert-basicgroup)
                 (supergroup "SuperGroup" telega-info--insert-supergroup)))))
    (cl-assert info-spec)
    (telega-ins--with-face 'bold
      (telega-ins (nth 1 info-spec)))
    (telega-ins "\n")
    (funcall (nth 2 info-spec) (telega-chat--info chat) chat))

  (when telega-debug
    (telega-ins "\n---DEBUG---\n")
    (telega-ins (propertize "Chat: " 'face 'bold)
                (format "%S" chat) "\n")
    (telega-ins (propertize "Info: " 'face 'bold)
                (format "%S" (telega-chat--info chat))))
  )

(defun telega-describe-chat (chat)
  "Show info about chat at point."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))
  (with-telega-help-win "*Telegram Chat Info*"
    (setq telega--chat chat)
    (telega-describe-chat--inserter chat)

    (setq telega--help-win-param chat)
    (setq telega--help-win-inserter #'telega-describe-chat--inserter)
    ))

(defun telega-describe-chat--maybe-redisplay (chat)
  "If CHAT info buffer exists and visible, then redisplay it."
  (telega-help-win--maybe-redisplay "*Telegram Chat Info*" chat))

(defun telega-chat-with (chat-or-user)
  "Start messaging with CHAT-OR-USER."
  (interactive
   (let* ((completion-ignore-case t)
          (completions (telega-completing-titles))
          (title (funcall telega-completing-read-function
                          "Chat with: " (mapcar #'car completions) nil t)))
     (list (cdr (assoc title completions)))))

  (when (telega-user-p chat-or-user)
    (setq chat-or-user (or (telega-chat-get (plist-get chat-or-user :id))
                           (telega--createPrivateChat chat-or-user))))

  (telega-chat--pop-to-buffer chat-or-user))

(defun telega-chat-join-by-link (link)
  "Join chat by invitation LINK."
  (interactive "sJoin chat by invite link: ")
  (telega-chat--pop-to-buffer (telega--joinChatByInviteLink link)))

(defun telega-chat-toggle-muted (chat &optional muted-for)
  "Toggle mute for the CHAT.
If MUTED-FOR is specified, set it as `:mute_for' notification setting."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))
  (telega--setChatNotificationSettings chat
    :use_default_mute_for nil
    :mute_for (or muted-for (if (telega-chat-muted-p chat)
                                0
                              telega-mute-for-ever))))

(defun telega-chat-toggle-archive (chat)
  "Archive or Unarchive CHAT."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))
  (telega--addChatToList
   chat (list :@type (if (telega-chat-match-p chat 'archive)
                         "chatListMain"
                       "chatListArchive"))))

(defun telega-chat-toggle-read (chat)
  "Toggle chat as read/unread."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))
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
             chat (list (plist-get chat :last_message)) 'force))
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
            (telega-chat-match-active-p (telega-chatbuf--chat buf)))
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
    (when (and (telega-chat-private-p chat 'include-bots)
               (not (plist-get chat :is_blocked)))
      (when (yes-or-no-p
             (concat (telega-i18n "lng_blocked_list_confirm_text"
                       :name (telega-chat-title chat)) " "))
        (telega-msg-sender-block chat)))

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

(defun telega-chat-create (chat-type)
  "Interactively create new chat of CHAT-TYPE.
CHAT-TYPE is one of \"basicgroup\", \"supergroup\", \"channel\",
\"secret\", \"location-supergroup\", \"location-channel\".
Return newly created chat."
  (interactive (list (funcall telega-completing-read-function
                              "Chat Type: "
                              (list "basicgroup" "supergroup" "channel" "secret"
                                    "location-supergroup" "location-channel")
                              nil t)))

  (cond ((string= chat-type "basicgroup")
         (let ((title (read-string "Chat Title: "))
               (users (telega-completing-read-user-list "Add users")))
           (telega--createNewBasicGroupChat
            title users #'telega-chat--pop-to-buffer)))

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
            desc loc #'telega-chat--pop-to-buffer)))))

(defun telega-chat-upgrade-to-supergroup (chat)
  "Upgrade basic group CHAT from basicgroup to supergroup."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))
  (telega--upgradeBasicGroupChatToSupergroupChat
   chat #'telega-chat--pop-to-buffer))

(defun telega-chat-transfer-ownership (chat)
  "Transfer CHAT's ownership TO-USER."
  (declare (indent 1))
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))

  (let* ((admins (mapcar #'telega-user-get
                         (mapcar (telega--tl-prop :user_id)
                                 (telega--getChatAdministrators chat))))
        (to-user (telega-completing-read-user "To Admin: "
                   (cl-remove-if #'telega-me-p admins))))

    ;; NOTE: check chat ownership can be transferred
    (unless (eq (telega--tl-type (telega--canTransferOwnership))
                'canTransferOwnershipResultOk)
      (user-error (concat
                   (telega-i18n "lng_rights_transfer_check_about"
                     :user (telega-user--name to-user)) "\n"
                     (telega-i18n "lng_rights_transfer_check_session") "\n"
                     (telega-i18n "lng_rights_transfer_check_password") "\n"
                     (telega-i18n "lng_rights_transfer_check_later"))))

    (unless (telega-read-im-sure-p
             (concat (telega-i18n "lng_rights_transfer_about"
                       :group (telega-chat-title chat)
                       :user (telega-user--name to-user))
                     "\n"
                     (telega-i18n "lng_rights_transfer_sure") "?"))
      (user-error "Ownership transfer canceled"))

    (let ((pass (password-read
                 (concat (telega-i18n "lng_rights_transfer_password_description")
                         "\n" "Telegram Password: "))))
      (telega--transferChatOwnership chat to-user pass
        (lambda (result)
          (when (eq (telega--tl-type result) 'ok)
            (message
             (telega-i18n (if (telega-chat-channel-p chat)
                              "lng_rights_transfer_done_channel"
                            "lng_rights_transfer_done_group")
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
  "Switch to \"Saved Messages\" chat buffer.
If \"Saved Messages\" chat is not opened, then open it.
If `\\[universal-argument]' is specified, then goto prompt otherwise
keep the point, where it is."
  (interactive "P")
  (telega-chat--pop-to-buffer (telega-chat-me))
  (when arg
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

(defun telega-switch-important-chat (chat)
  "Switch to important CHAT if any.
If `\\[universal-argument] is used, then select first chat if
multiple chats are important."
  (interactive
   (list (let ((ichats (telega-filter-chats
                        telega--ordered-chats
                        '(or mention (and unread unmuted)))))
           (cond ((null ichats)
                  (user-error "No important chats"))
                 ((or (= 1 (length ichats)) current-prefix-arg)
                  (car ichats))
                 (t
                  (telega-completing-read-chat "Important Chat: " ichats))))))

  (telega-chat--pop-to-buffer chat))


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

    (define-key map (kbd "RET") 'telega-chatbuf-newline-or-input-send)
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
    ;; - {{{where-is(telega-chatbuf-goto-pinned-message,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-goto-pinned-message, 2)}}}
    (define-key map (kbd "M-g P") 'telega-chatbuf-goto-pinned-message)
    (define-key map (kbd "M-g ^") 'telega-chatbuf-goto-pinned-message)
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
    (define-key map (kbd "C-c C-r") 'telega-chatbuf-filter-search)

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
            (telega-ins (telega-symbol 'pause))
          (telega-ins (telega-symbol 'play)))
        (telega-ins " ")
        (if sender
            (telega-ins (telega-user--name sender))
          (telega-ins (telega-chat-title (telega-msg-chat msg)))))
      (telega-ins " ")
      (telega-ins--button "stop"
        'action (lambda (_ignored)
                  (telega-ffplay-stop)))
      (telega-ins "]"))))

(defun telega-chatbuf--first-msg ()
  "Return first message inserted in chat buffer."
  ;; Find first non-telegaMessage message in the chatbuf
  ;; telegaMessage have :id = -1
  (let ((node (ewoc-nth telega-chatbuf--ewoc 0)))
    (while (and node (< (plist-get (ewoc-data node) :id) 0))
      (setq node (ewoc-next telega-chatbuf--ewoc node)))
    (when node
      (ewoc-data node))))

(defun telega-chatbuf--last-msg ()
  "Return last message inserted in chat buffer."
  ;; Find last non-telegaMessage message in the chatbuf
  ;; telegaMessage have :id = -1
  (let ((node (ewoc-nth telega-chatbuf--ewoc -1)))
    (while (and node (< (plist-get (ewoc-data node) :id) 0))
      (setq node (ewoc-prev telega-chatbuf--ewoc node)))
    (when node
      (ewoc-data node))))

(defun telega-chatbuf--last-message-id ()
  "Return last message id in for the chatbuf.
Takes into account threads."
  (or (telega--tl-get telega-chatbuf--thread-msg
                      :interaction_info :reply_info :last_message_id)
      (telega--tl-get telega-chatbuf--chat :last_message :id)))

(defsubst telega-chatbuf--last-msg-loaded-p ()
  "Return non-nil if chat's last message is shown."
  (or (memq 'newer-loaded telega-chatbuf--history-state)
      (<= (or (telega-chatbuf--last-message-id) 0)
          (or (plist-get (telega-chatbuf--last-msg) :id) 0))))

(defun telega-chatbuf--last-read-inbox-msg-id ()
  "Return last read inbox message id.
Takes into account `telega-chatbuf--thread-msg'."
  ;; NOTE: `:last_read_inbox_message_id'==0 in thread means nothing
  ;; has been read in this thread yet
  (let ((thread-last-read-msg-id
         (telega--tl-get telega-chatbuf--thread-msg :interaction_info
                         :reply_info :last_read_inbox_message_id)))
    (or (when thread-last-read-msg-id
          (if (zerop thread-last-read-msg-id)
              (plist-get telega-chatbuf--thread-msg :id)
            thread-last-read-msg-id))
        (plist-get telega-chatbuf--chat :last_read_inbox_message_id))))

(defun telega-chatbuf--view-msg-at (&optional point force)
  "View message at POINT.
If POINT is ommited, then current point is used.
FORCE - non-nil to force viewing messages in closed chat.
If POINT is not over some message, then view last message."
  (let ((message (or (telega-msg-at (or point (point)))
                     (telega-chatbuf--last-msg))))
    (when (and message
               (or (plist-get message :contains_unread_mention)
                   (> (plist-get message :id)
                      (telega-chatbuf--last-read-inbox-msg-id))))
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
                              (telega-chatbuf--last-msg-loaded-p))
                          telega-symbol-underline-bar
                        telega-symbol-underline-bar-partial))
         ;; NOTE: `telega-ins--as-string' uses temporary buffer, so
         ;; prepare everything we need before
         (actions (gethash (plist-get telega-chatbuf--chat :id)
                           telega--actions))
         (chat telega-chatbuf--chat)
         (msg-filter telega-chatbuf--msg-filter)
         (thread-msg telega-chatbuf--thread-msg)
         (history-loading-p telega-chatbuf--history-loading)
         (compact-view-p telega-chatbuf--messages-compact-view)
         (voice-msg telega-chatbuf--voice-msg))
    (telega-ins--as-string
     (when compact-view-p
       (telega-ins "\n"))
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

       ;; Message thread
       (when thread-msg
         (telega-ins--button (propertize "✕" 'face 'bold)
           'action #'telega-chatbuf-filter-cancel)
         (telega-ins " ")
         (telega-ins--with-attrs (list :max telega-chat-fill-column
                                       :align 'left :elide t)
           (telega-ins "Thread: ")
           (telega-ins--content-one-line thread-msg))
         (telega-ins "\n"))

       ;; Messages Filter
       (when msg-filter
         (telega-ins--button (propertize "✕" 'face 'bold)
           'action #'telega-chatbuf-filter-cancel)
         (telega-ins " ")
         (telega-ins "Messages Filter: "
                     (propertize (plist-get msg-filter :title) 'face 'bold))
         (when-let ((sender (plist-get msg-filter :sender)))
           (telega-ins " by ")
           (telega-ins--raw-button
               (list 'action (lambda (_button)
                               (telega-describe-user sender)))
             (telega-ins (telega-user--name sender))))
         (when-let ((total-count (plist-get msg-filter :total-count)))
           (telega-ins-fmt " (total: %d)" total-count))
         (telega-ins "\n"))
       ;; Action Bar
       (when (telega-ins--chat-action-bar chat)
         (telega-ins "\n"))

       ;; Reply markup
       (when-let ((markup-msg (telega-chat-reply-markup-msg chat)))
         (unless (plist-get markup-msg :telega-is-deleted-message)
           (telega-ins--labeled (concat (telega-symbol 'keyboard) "\u00A0") nil
             (telega-ins--reply-markup markup-msg 'force))
           (telega-ins "\n")

           (telega-ins--with-attrs (list :min column :max column
                                         :align 'left
                                         :align-symbol fill-symbol))
           (telega-ins "\n")
           ))
       ))))

(defun telega-chatbuf--footer-update ()
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
        telega-chatbuf--messages-pop-ring
        (make-ring telega-chat-messages-pop-ring-size)
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

  ;; Process earch line seperately to check for bidi paragraph
  ;; See https://github.com/zevlg/telega.el/issues/45#issuecomment-462160553
  (setq bidi-display-reordering telega-chat-bidi-display-reordering)
  (setq bidi-paragraph-separate-re "^")
  (setq bidi-paragraph-start-re "^")

  (erase-buffer)
  (setq-local nobreak-char-display nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local window-point-insertion-type t)
  (setq-local next-line-add-newlines nil)
  (setq-local next-screen-context-lines 0) ; do not scroll if point at `eobp'
  (setq-local scroll-conservatively telega-chat-scroll-conservatively)
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
  (telega-chatbuf--prompt-update)

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

(defun telega-ins--prompt-aux-msg (title msg &optional with-username)
  "Inserter for MSG in chatbuf aux prompt."
  (cl-assert msg)
  (telega-ins--aux-inline
      (progn
        ;; NOTE: hack to insert [x] button (to cancel aux prompt)
        ;; before title
        (telega-ins--button "✕"
          'action (lambda (_ignored)
                    (telega-chatbuf-cancel-aux))
          :help-echo (lambda (_ignored)
                       (telega-help-message--cancel-aux 'aux-prompt)))
        title)
      'telega-chat-prompt
    (telega-ins--aux-msg-one-line msg with-username)))

(defun telega-ins--prompt-aux-edit (edit-msg)
  "Inserter for EDIT-MSG in chatbuf aux prompt."
  (telega-ins--prompt-aux-msg
   (telega-i18n "lng_context_edit_msg") edit-msg))

(defun telega-chatbuf--editing-msg ()
  "Return message currently editing."
  (and (eq (button-get telega-chatbuf--aux-button :inserter)
           #'telega-ins--prompt-aux-edit)
       (button-get telega-chatbuf--aux-button :value)))

(defun telega-ins--prompt-aux-reply (reply-msg)
  "Inserter for REPLY-MSG in chatbuf aux prompt."
  (telega-ins--prompt-aux-msg
   (telega-i18n "lng_context_reply_msg") reply-msg 'with-username))

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
  ;;
  ;; If AUX part is invisible it goes before real prompt, take this
  ;; into account, in this case prompt looks like:
  ;;   .----- telega-chatbuf--aux-button
  ;;   |    .-- telega-chatbuf--prompt-button
  ;;   v    v
  ;;   [AUX][PROMPT]
  ;;                 ^
  ;;                 `-- telega-chatbuf--input-marker
  (when (and (not (telega-chatbuf--prompt-unblock-start-join-p))
             (>= (point) (if (button-get telega-chatbuf--aux-button 'invisible)
                             telega-chatbuf--aux-button
                           telega-chatbuf--prompt-button))
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
                   (concat (telega-symbol 'telegram)
                           (when (telega-chat-secret-p chat)
                             (telega-symbol 'lock))
                           (telega-chat-title-with-brackets chat "")
                           (when (plist-get chat :is_pinned)
                             (telega-symbol 'pin))
                           (when (plist-get chat :has_scheduled_messages)
                             (telega-symbol 'alarm)))))
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

(defun telega-chatbuf--unblock-start-join-action (&optional _ignored_button)
  "[START] [UNBLOCK] or [JOIN] or button has been pressed."
  (cl-assert (not (telega-chat-secret-p telega-chatbuf--chat)))

  ;; NOTE: do async calls, update chatbuf prompt
  ;; on-updateUserFullInfo, on-updateBasicGroup or on-updateSupergroup
  (if (telega-chat-private-p telega-chatbuf--chat 'include-bots)
      (progn
        (telega-msg-sender-unblock telega-chatbuf--chat)
        (when (telega-chat-bot-p telega-chatbuf--chat)
          (telega--sendBotStartMessage
           (telega-chat-user telega-chatbuf--chat 'inc-bots)
           telega-chatbuf--chat)))

    (telega--joinChat telega-chatbuf--chat)))

(defun telega-chatbuf--unblock-start-join-prompt ()
  "Return unblock-start-join button to be used in chatbuf prompt.
unblock-start-join button is used for prompt if chatbuf is
unknown, i.e. has no positions set."
  (unless (append (plist-get telega-chatbuf--chat :positions) nil)
    (when-let ((label (cond ((plist-get telega-chatbuf--chat :is_blocked)
                             (if (telega-chat-bot-p telega-chatbuf--chat)
                                 "RESTART BOT"
                               "UNBLOCK"))
                            ((telega-chat-bot-p telega-chatbuf--chat)
                             "START")
                            ((and (not (telega-chat-private-p
                                        telega-chatbuf--chat))
                                  (not (telega-chat-secret-p
                                        telega-chatbuf--chat)))
                             "JOIN"))))
      (telega-ins--as-string
       (telega-ins--button (concat "   " label "   ")
         'action #'telega-chatbuf--unblock-start-join-action)))))

(defun telega-chatbuf--prompt-unblock-start-join-p ()
  "Return non-nil if current prompt is unblock-start-join button."
  (button-get telega-chatbuf--prompt-button :usj-prompt-p))

(defun telega-chatbuf--prompt-update ()
  "Update chatbuf's prompt."
  ;; NOTE: `telega-chatbuf--chat' will be overwritten in
  ;; `telega-ins--as-string', so save it before
  (let* ((chat telega-chatbuf--chat)
         (comment-p (and telega-chatbuf--thread-msg
                         (not (telega-chat-match-p chat 'me-is-member))))
         (anonymous-p (and (eq 'supergroup (telega-chat--type chat 'raw))
                           (telega--tl-get (telega-chat--supergroup chat)
                                           :status :is_anonymous)))
         (usj-prompt (unless (or comment-p anonymous-p)
                       (telega-chatbuf--unblock-start-join-prompt)))
         (prompt (concat (when (telega-chat-match-p
                                chat telega-chat-prompt-show-avatar-for)
                           (telega-ins--as-string
                            (telega-ins--image
                             (telega-chat-avatar-image-one-line chat))))
                         (cond (usj-prompt usj-prompt)
                               (anonymous-p
                                telega-chat-input-anonymous-prompt)
                               (comment-p
                                telega-chat-input-comment-prompt)
                               (t
                                telega-chat-input-prompt)))))
    (telega-save-excursion
      (telega-button--update-value
       telega-chatbuf--prompt-button prompt :usj-prompt-p usj-prompt))))

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

      (telega-chatbuf--prompt-update))))

(defun telega-chatbuf--input-draft-update (&optional force)
  "Update chatbuf's input to display draft message.
If FORCE is specified, then set input draft unconditionally,
otherwise set draft only if chatbuf input is also draft."
  (let* ((chat telega-chatbuf--chat)
         (draft-msg (plist-get chat :draft_message))
         (reply-msg-id (plist-get draft-msg :reply_to_message_id)))
    (if (and reply-msg-id (not (zerop reply-msg-id)))
        (telega-msg-get chat reply-msg-id
          (lambda (msg &optional _ignored) (telega-msg-reply msg)))
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
      (telega-chatbuf--last-read-inbox-msg-id)
      (- (/ telega-chat-history-limit 2))
      telega-chat-history-limit
    (lambda (total-messages)
      ;; NOTE: If thread is empty initially, then mark all history
      ;; newer and older is loaded
      (when (zerop total-messages)
        (telega-chatbuf--newer-history-loaded)
        (telega-chatbuf--older-history-loaded)
        (goto-char (point-min)))
      ;; NOTE: if thread loads from the first message, then insert
      ;; thread starter message
      (let ((thread-last-read-msg-id
             (telega--tl-get telega-chatbuf--thread-msg :interaction_info
                             :reply_info :last_read_inbox_message_id)))
        (when (and thread-last-read-msg-id
                  (zerop thread-last-read-msg-id))
          (telega-chatbuf--older-history-loaded)
          (goto-char (point-min))))

      ;; NOTE: When there is pending input, then jump directly to it,
      ;; so user will see his input
      (unless (telega-chatbuf-has-input-p)
        (telega-chatbuf-next-unread
          (lambda (button)
            (telega-chatbuf--view-msg-at button)
            (when (and (eq (telega-msg-at button)
                           (telega-chatbuf--last-msg))
                       (telega-button--observable-p
                        telega-chatbuf--input-marker))
              (goto-char (point-max))))))

      ;; Possible load more history
      (unless (zerop total-messages)
        (when (and (< (point) 2000)
                   (telega-chatbuf--need-older-history-p))
          (telega-chatbuf--load-older-history)))
      )))

(defun telega-chatbuf--get-create (chat &optional no-history-load)
  "Get or create chat buffer for the CHAT.
If NO-HISTORY-LOAD is specified, do not try to load history."
  (let ((bufname (telega-chatbuf--name chat)))
    (or (get-buffer bufname)
        (with-current-buffer (generate-new-buffer bufname)
          (let ((telega-chat--preparing-buffer-for chat))
            (telega-chat-mode))
          (telega-chatbuf--modeline-update)
          (telega-chatbuf--footer-update)
          ;; Show the draft message if any, see
          ;; https://github.com/zevlg/telega.el/issues/80
          (when (plist-get chat :draft_message)
            (telega-chatbuf--input-draft-update 'force))

          ;; Asynchronously fetch some chat info
          (telega-chatbuf--admins-fetch)
          (telega-chatbuf--pinned-messages-fetch)
          (unless (zerop (plist-get chat :reply_markup_message_id))
            (telega-chatbuf--reply-markup-message-fetch))

          ;; Start from last read message
          ;; see https://github.com/zevlg/telega.el/issues/48
          (unless no-history-load
            (telega-chatbuf--load-initial-history))

          ;; Openning chat may affect filtering, see `opened' filter
          (telega-chat--update chat)

          (current-buffer)))))

(defun telega-chatbuf--need-older-history-p ()
  "Return non-nil if older history can be loaded."
  (not (memq 'older-loaded telega-chatbuf--history-state)))

(defun telega-chatbuf--older-history-loaded ()
  "In chatbuf set mark, that all older history has been loaded."
  (when (telega-chatbuf--need-older-history-p)
    (message "loaded all history")
    (setq telega-chatbuf--history-state
          (cl-pushnew 'older-loaded telega-chatbuf--history-state))
    ;; Insert thread starter message if any, because it is not
    ;; included in the results of the `getMessageThreadHistory' call
    (when telega-chatbuf--thread-msg
      (telega-chatbuf--insert-messages
       (list telega-chatbuf--thread-msg
             (telega-msg-create-internal
              telega-chatbuf--chat
              (telega-fmt-text (telega-i18n "lng_replies_discussion_started")
                               '(:@type "textEntityTypeBold"))))
       'prepend))))

(defun telega-chatbuf--newer-history-loaded ()
  "In chatbuf set mark, that all newer history has been loaded."
  (setq telega-chatbuf--history-state
        (cl-pushnew 'newer-loaded telega-chatbuf--history-state)))

(defun telega-chatbuf--need-newer-history-p ()
  "Return non-nil if newer history can be loaded."
  (and (not (telega-chatbuf--last-msg-loaded-p))
       ;; Not editing or replying
       (button-get telega-chatbuf--aux-button 'invisible)
       (not (telega-chatbuf-has-input-p))))

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
         (telega-ins (telega-symbol 'linked))
         (telega-ins-i18n (if channel-p
                              "lng_channel_discuss"
                            "lng_manage_linked_channel")))
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
                 (propertize (telega-symbol 'contact) 'face 'shadow)
                 (unless (zerop online-count)
                   (concat ", " (number-to-string online-count)
                           telega-symbol-online-status))))
              "]"))))

(defun telega-chatbuf-mode-line-pinned-or-thread-msg (&optional max-width)
  "Format pinned or thread starter message string for chat buffer modeline.
If message thread filtering is enabled, use it first."
  (let* ((thread-msg telega-chatbuf--thread-msg)
         (pinned-messages (plist-get telega-chatbuf--chat
                                     :telega-pinned-messages))
         (pinned-msg-idx (plist-get telega-chatbuf--chat
                                    :telega-pinned-message-index))
         (pin-msg (or thread-msg
                      (when pinned-messages
                        (nth pinned-msg-idx pinned-messages)))))
    (when (and pin-msg (not (plist-get pin-msg :telega-is-deleted-message)))
      ;; NOTE: Adjust MAX-WIDTH taking into account length of the
      ;; `telega-symbol-pin'
      (setq max-width (+ (or max-width 15)
                         (length (if thread-msg
                                     "Thread"
                                   (telega-symbol 'pin)))))
      (telega-ins--as-string
       (telega-ins " [")
       (telega-ins--with-attrs
           (list :max max-width :align 'left :elide t)
         (telega-ins--with-props
             (list 'local-map (make-mode-line-mouse-map
                               'mouse-1
                               (if thread-msg
                                   #'telega-chatbuf-goto-thread-message
                                 #'telega-chatbuf-goto-pinned-message))
                   'mouse-face 'mode-line-highlight
                   'help-echo (telega-i18n
                                  (if thread-msg
                                      "telega_chat_modeline_thread_msg_help"
                                    "telega_chat_modeline_pinned_msg_help")
                                :mouse "mouse-1"))
           (if thread-msg
               (telega-ins (propertize "Thread" 'face 'error) ": ")
             (telega-ins (telega-symbol 'pin))
             (when (> (length pinned-messages) 1)
               (telega-ins-fmt "(%d/%d)"
                 (1+ pinned-msg-idx) (length pinned-messages))))
           (let ((telega-use-images nil)
                 (telega-emoji-use-images nil))
             ;; NOTE: avoid using images for emojis, because modeline
             ;; height might differ from default height, and modeline
             ;; will increase its height
             (telega-ins--content-one-line pin-msg))))
       (telega-ins "]")))))

(defun telega-chatbuf-mode-line-messages-filter ()
  "Format currently applied messages filter."
  (when telega-chatbuf--msg-filter
    (concat " ["
            (propertize "Filter" 'face 'error)
            ": "
            (propertize (plist-get telega-chatbuf--msg-filter :title) 'face 'bold)
            (when-let ((sender (plist-get telega-chatbuf--msg-filter :sender)))
              (concat " by " (telega-msg-sender-title sender)))
            "]")))

(defun telega-chatbuf--modeline-update ()
  "Update `mode-line-buffer-identification' for the CHAT buffer."
  ;; NOTE: Avoid images in modeline if mode-line height is less then
  ;; default height
  (let* ((mode-line-smaller-p (< (telega-chars-xheight 1 'mode-line)
                                 (telega-chars-xheight 1 'default)))
         (telega-use-images (unless mode-line-smaller-p
                              telega-use-images))
         (telega-emoji-use-images (unless mode-line-smaller-p
                                    telega-emoji-use-images)))
    (setq mode-line-buffer-identification
          (list (propertized-buffer-identification "%b")
                ;; Online status
                (when (and (telega-chat-private-p telega-chatbuf--chat)
                           (not (telega-me-p telega-chatbuf--chat))
                           (telega-user-online-p
                            (telega-chat-user telega-chatbuf--chat 'inc-bots)))
                  telega-symbol-online-status)
                ;; TTL for secret chats
                (when (telega-chat-secret-p telega-chatbuf--chat)
                  (let* ((secret (telega-chat--secretchat telega-chatbuf--chat))
                         (ttl (plist-get secret :ttl)))
                    (concat " ("
                            (propertize
                             (concat (telega-symbol 'lock) "TTL: "
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
                                  (current-buffer)))))
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
  (let ((idx (if (and telega-chatbuf--input-idx
                      (telega-chatbuf-has-input-p))
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
  (when (and telega-chatbuf--input-idx
             (telega-chatbuf-has-input-p))
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
  (let ((regexp (buffer-substring (minibuffer-prompt-end) (point))))
    (if (string-empty-p regexp)
        (with-telega-chatbuf telega-minibuffer--chat
          (telega-chatbuf-input-restore))

      (unless (string= telega-minibuffer--string regexp)
        (setq telega-minibuffer--string regexp)
        (with-telega-chatbuf telega-minibuffer--chat
          (telega-chatbuf-input-restore)
          (telega-chatbuf-input-match regexp nil))))))

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
  "Cancel input search results."
  (interactive)
  (cl-assert (eq major-mode 'minibuffer-inactive-mode))
  (delete-region (minibuffer-prompt-end) (point))
  (exit-minibuffer))

(defun telega-chatbuf--input-search-accept ()
  "Accept input search results."
  (interactive)
  (cl-assert (eq major-mode 'minibuffer-inactive-mode))
  (exit-minibuffer))

(defun telega-chatbuf--input-search-input-prev (&optional forward-p)
  (interactive)
  (cl-assert (eq major-mode 'minibuffer-inactive-mode))
  (delete-region (minibuffer-prompt-end) (point))

  (let ((prompt-input (with-telega-chatbuf telega-minibuffer--chat
                        (telega-chatbuf-input-prev (if forward-p -1 1))
                        (telega-chatbuf-input-string))))
    (insert prompt-input)))

(defun telega-chatbuf--input-search-input-next ()
  (interactive)
  (telega-chatbuf--input-search-input-prev 'forward))

(defvar telega-chatbuf--input-search-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") 'telega-chatbuf--input-search-cancel)
    (define-key map (kbd "C-r") 'telega-chatbuf--input-search-prev)
    (define-key map (kbd "C-s") 'telega-chatbuf--input-search-next)
    (define-key map (kbd "M-p") 'telega-chatbuf--input-search-input-prev)
    (define-key map (kbd "M-n") 'telega-chatbuf--input-search-input-next)
    (define-key map (kbd "RET") 'telega-chatbuf--input-search-accept)
    map))

(defun telega-chatbuf-input-search ()
  "Search for REGEX in chat input history."
  (interactive)
  ;; Save pending input
  (setq telega-chatbuf--input-idx nil
        telega-chatbuf--input-pending (telega-chatbuf-input-string))

  (let* ((telega-minibuffer--string "")
         (telega-minibuffer--chat telega-chatbuf--chat)
         (regexp (minibuffer-with-setup-hook
                     (lambda ()
                       (add-hook 'post-command-hook
                                 'telega-chatbuf--minibuf-post-command t t))
                   (read-from-minibuffer "History input search: " nil
                                         telega-chatbuf--input-search-map))))
    (when (string-empty-p regexp)
      ;; Restore saved pending input if canceled
      (telega-chatbuf--input-delete)
      (goto-char (point-max))
      (insert telega-chatbuf--input-pending))
    ))

(defun telega-chatbuf-edit-next (without-aux &optional backward)
  "Edit message sent next to currently editing.
If WITHOUT-AUX is specified with `\\[universal-argument]', then
instead of editing, just pop previously sent message as input."
  (interactive "P")
  (let* ((edit-msg (telega-chatbuf--editing-msg))
         (last-msg (telega-chatbuf--last-msg))
         (last-sent-msg
          (if (and backward (not edit-msg)
                   (telega-msg-by-me-p last-msg)
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
  "Edit previously sent message.
If `\\[universal-argument]' is given, then just copy last sent message."
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
  ;; NOTE: MSG-BUTTON could be `nil' if message is ignored and not displayed
  (when-let ((msg-button (button-at (ewoc-location node))))
    (telega-save-window-start (button-start msg-button) (button-end msg-button)
      (if (eq (telega-msg-at (point)) (ewoc--node-data node))
          (telega-save-cursor
            (ewoc-invalidate telega-chatbuf--ewoc node))
        (telega-save-excursion
          (ewoc-invalidate telega-chatbuf--ewoc node)))))

  (let ((chat-win (get-buffer-window)))
    (if (not chat-win)
        (telega-buffer--hack-win-point)

      ;; Redetect cursor sensor
      (set-window-parameter chat-win 'cursor-sensor--last-state nil)
      ;; Inhibit button's help message
      (let ((telega-help-messages nil))
        (cursor-sensor--detect chat-win)))))

(defun telega-chatbuf--insert-messages (messages how)
  "Insert MESSAGES into chatbuf.
HOW could be `prepend' or `append', or `append-new'.
Return last inserted ewoc node."
  (with-telega-deferred-events
    (let* ((use-date-breaks-p
            (telega-chat-match-p telega-chatbuf--chat
                                 telega-chat-use-date-breaks-for))
           (node (if (eq how 'prepend)
                     (ewoc--header telega-chatbuf--ewoc)
                   (ewoc-nth telega-chatbuf--ewoc -1)))
           (saved-point (if (or (eq how 'prepend)
                                (and (eq how 'append-new)
                                     (>= (point) telega-chatbuf--input-marker)))
                            (copy-marker (point) t)
                          (point)))
           ;; State of the current button if prepending
           (chat-win (get-buffer-window (current-buffer)))
           (msg-button (button-at (point)))
           (msg-button-was-observable-p
            (when (and (eq how 'prepend) chat-win msg-button)
              (telega-button--observable-p msg-button)))
           )
      (unwind-protect
          (seq-doseq (msg messages)
            (run-hook-with-args 'telega-chat-insert-message-hook msg)
            ;; Track the uploading progress
            ;; see: https://github.com/zevlg/telega.el/issues/60
            (telega-msg--track-file-uploading-progress msg)

            ;; Ensure cached message (if any) and node data is the same
            ;; object, so message could be modified inplace
            (telega-msg-cache msg 'maybe-update)

            ;; Maybe insert date break, such as
            ;; -----(28 December 2020)-----
            (let ((node-msg (ewoc--node-data node)))
              (when (and use-date-breaks-p
                         (telega-msg-p node-msg)
                         (plist-get node-msg :date)
                         (plist-get msg :date)
                         ;; 3-6 elements are for DAY MONTH YEAR
                         (not (equal (seq-subseq (decode-time
                                                  (plist-get node-msg :date))
                                                 3 6)
                                     (seq-subseq (decode-time
                                                  (plist-get msg :date))
                                                 3 6))))
                (setq node (ewoc-enter-after
                            telega-chatbuf--ewoc node
                            (telega-msg-create-internal
                             telega-chatbuf--chat
                             (telega-fmt-text
                              (telega-ins--as-string
                               (telega-ins--date-full (plist-get msg :date)))
                              '(:@type "textEntityTypeBold")))))))

            (setq node (ewoc-enter-after telega-chatbuf--ewoc node msg)))

        (goto-char saved-point))

      ;; If message at point was visible - keep it visible
      (when (and (memq msg-button-was-observable-p '(full top))
                 (equal msg-button (button-at (point))))
        (telega-button--make-observable msg-button))

      node)))

(defun telega-chatbuf--prepend-messages (messages)
  "Insert MESSAGES at the beginning of the chat buffer.
First message in MESSAGE will be first message at the beginning."
  (telega-chatbuf--insert-messages messages 'prepend))

(defun telega-chatbuf--append-new-message-p (msg)
  "Return non-nil if incoming message MSG should be appended."
  ;; NOTE: `:last_message' could be already updated in the chat
  ;; with the id of the MSG, so check for it
  ;; Also, do not insert new messages while loading history messages,
  ;; see https://github.com/zevlg/telega.el/issues/91
  (when (and (not telega-chatbuf--history-loading)
             (or (telega-chatbuf--last-msg-loaded-p)
                 (eq (telega-chatbuf--last-message-id)
                     (plist-get msg :id))))
    (cond (telega-chatbuf--thread-msg
           (eq (plist-get telega-chatbuf--thread-msg :message_thread_id)
               (plist-get msg :message_thread_id)))
          (telega-chatbuf--msg-filter
           ;; Update history state by side-effect
           (setq telega-chatbuf--history-state
                 (delq 'newer-loaded telega-chatbuf--history-state))
           (telega-chatbuf--footer-update)
           nil)
          (t t))))

(defun telega-chatbuf--node-by-msg-id (msg-id)
  "In current chatbuffer find message button with MSG-ID."
  ;; NOTE: message IDs are monotonically grows from first to the last
  ;; message. If MSG-ID is closer to the first message's id, then to
  ;; the last one, then search from the beginning, otherwise search
  ;; from the end
  (when-let ((first-msg (telega-chatbuf--first-msg))
             (last-msg (telega-chatbuf--last-msg)))
    (when (and (>= msg-id (plist-get first-msg :id))
               (<= msg-id (plist-get last-msg :id)))
      (telega-ewoc--find
       telega-chatbuf--ewoc msg-id #'= (telega--tl-prop :id) nil
       (if (< (- msg-id (plist-get first-msg :id))
              (- (plist-get last-msg :id) msg-id))
           ;; MSG-ID is closer to the beginning
           #'ewoc--node-next
         #'ewoc--node-prev)))))

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
OLD-LAST-READ-OUTBOX-MSGID is old value for chat's
`:last_read_outbox_message_id'."
  (let ((node (ewoc--footer telega-chatbuf--ewoc)))
    (while (and (setq node (ewoc-prev telega-chatbuf--ewoc node))
                (< old-last-read-outbox-msgid
                   (plist-get (ewoc-data node) :id)))
      (when (plist-get (ewoc-data node) :is_outgoing)
        (telega-chatbuf--redisplay-node node)))))

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
        ;; NOTE: Mark newer history is loaded in advance
        (telega-chatbuf--newer-history-loaded)
        (setq from-msg-id (plist-get (plist-get chat :last_message) :id)
              offset -1))

      (when from-msg-id
        ;; Asynchronously load chat history
        (let ((history-callback
               (lambda (history)
                 (with-telega-chatbuf chat
                   ;; NOTE: Message insertation might trigger history
                   ;; loading, thats why
                   ;; `telega-chatbuf--history-loading' is reseted
                   ;; only after all the messages are inserted?
                   (telega-chatbuf--insert-messages
                    (nreverse (plist-get history :messages)) 'prepend)
                   (setq telega-chatbuf--history-loading nil)
                   (when (zerop (length (plist-get history :messages)))
                     (telega-chatbuf--older-history-loaded))
                   (when callback
                     (funcall callback (plist-get history :total_count)))
                   (telega-chatbuf--footer-update)))))
          (setq telega-chatbuf--history-loading
                (cond (telega-chatbuf--msg-filter
                       (telega--searchChatMessages
                           chat (plist-get telega-chatbuf--msg-filter
                                           :tdlib-msg-filter)
                           (plist-get telega-chatbuf--msg-filter :query)
                           from-msg-id offset limit
                           (plist-get telega-chatbuf--msg-filter :sender)
                         history-callback))
                      (telega-chatbuf--thread-msg
                       (telega--getMessageThreadHistory
                           chat telega-chatbuf--thread-msg
                           from-msg-id offset limit
                         history-callback))
                      (t
                       (telega--getChatHistory
                           chat from-msg-id offset
                           (or limit telega-chat-history-limit) nil
                         history-callback)))))
        (telega-chatbuf--footer-update)
        ))))

(defun telega-chatbuf--load-older-history (&optional callback)
  "In chat buffer load older messages.
CALLBACK if non-nil, then called with total number of loaded messages."
  (if (and telega-chatbuf--msg-filter
           (functionp (plist-get telega-chatbuf--msg-filter :tdlib-msg-filter)))
      ;; no-op: chatbuf messages filter is a function
      nil

    (cl-assert (telega-chatbuf--need-older-history-p))
    (telega-chat--load-history telega-chatbuf--chat nil nil nil callback)))

(defun telega-chatbuf--load-newer-history ()
  "In chat buffer load newer messages."
  (if (and telega-chatbuf--msg-filter
           (functionp (plist-get telega-chatbuf--msg-filter :tdlib-msg-filter)))
      ;; no-op: chatbuf messages filter is a function
      nil

    (cl-assert (telega-chatbuf--need-newer-history-p))
    (with-telega-chatbuf telega-chatbuf--chat
      (unless (or telega-chatbuf--history-loading
                  (not (telega-chatbuf--last-msg)))
        (let* ((chat telega-chatbuf--chat)
               (from-msg-id (plist-get (telega-chatbuf--last-msg) :id))
               (history-callback
                (lambda (history)
                  (let ((rmsgs (append
                                (nreverse (plist-get history :messages)) nil)))
                    ;; Strip messages till FROM-MSG-ID
                    (while (and rmsgs
                                (<= (plist-get (car rmsgs) :id) from-msg-id))
                      (setq rmsgs (cdr rmsgs)))
                    (with-telega-chatbuf chat
                      (telega-chatbuf--insert-messages rmsgs 'append)
                      (setq telega-chatbuf--history-loading nil)
                      (unless rmsgs
                        (telega-chatbuf--newer-history-loaded))
                      (telega-chatbuf--footer-update))))))
          (setq telega-chatbuf--history-loading
                (cond (telega-chatbuf--msg-filter
                       (telega--searchChatMessages
                           chat (plist-get telega-chatbuf--msg-filter
                                           :tdlib-msg-filter)
                           (plist-get telega-chatbuf--msg-filter :query)
                           from-msg-id
                           (- 1 telega-chat-history-limit)
                           telega-chat-history-limit
                           (plist-get telega-chatbuf--msg-filter :sender)
                         history-callback))
                      (telega-chatbuf--thread-msg
                       (telega--getMessageThreadHistory
                           chat telega-chatbuf--thread-msg from-msg-id
                           (- 1 telega-chat-history-limit)
                           telega-chat-history-limit
                         history-callback))
                      (t
                       (telega--getChatHistory
                           chat from-msg-id (- 1 telega-chat-history-limit)
                           telega-chat-history-limit nil
                         history-callback))))
          (telega-chatbuf--footer-update)
          )))))

(defun telega-chatbuf-cancel-aux (&optional arg)
  "Cancel current aux prompt.
If prefix ARG is given, also delete input."
  (interactive "P")
  (telega-chatbuf--prompt-reset)
  (when arg
    (telega-chatbuf--input-delete)))

(defun telega-help-message--cancel-aux (what)
  "Show help about canceling reply/edit in echo area."
  (telega-help-message what "%s to cancel %S"
    (telega-keys-description 'telega-chatbuf-cancel-aux telega-chat-mode-map)
    what))

(defun telega-chatbuf--input-imcs (markup-name)
  "Convert input to input message contents list.
MARKUP-NAME names a markup function from
`telega-chat-markup-functions' to be used for input formatting."
  (cl-assert (or (null markup-name)
                 (assoc markup-name telega-chat-markup-functions)))
  (let ((markup-function
         (or (cdr (assoc markup-name telega-chat-markup-functions))
             #'telega-string-fmt-text))
        (attaches (telega--split-by-text-prop
                   (telega-chatbuf-input-string) 'telega-attach))
        (disable-webpage-preview telega-chat-send-disable-webpage-preview)
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
                      :text (funcall markup-function text)
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

            (let ((cap (funcall markup-function (cadr attaches))))
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

(defun telega-chatbuf--input-imc-cancel-upload-ahead (imc)
  "For file used in IMC cancel its ahead uploading."
  (when-let* ((file-prop-alist '((inputMessageDocument  . :document)
                                 (inputMessagePhoto     . :photo)
                                 (inputMessageVideo     . :video)
                                 (inputMessageAudio     . :audio)
                                 (inputMessageVideoNote . :video_note)
                                 (inputMessageVoiceNote . :voice_note)
                                 (inputMessageAnimation . :animation)))
              (file-prop (cdr (assq (telega--tl-type imc) file-prop-alist)))
              (ifile (plist-get imc file-prop))
              (upload-ahead-file
               (get-text-property 0 'telega-upload-ahead-file
                                  (plist-get ifile :@type))))
    (telega--cancelUploadFile upload-ahead-file)))

(defun telega-chatbuf-input-send (markup-name)
  "Send chatbuf input to the chat.
If called interactively, number of `\\[universal-argument]' before
command determines index in `telega-chat-input-markups' of markup to
use.  For example `C-u RET' will use
`(nth 1 telega-chat-input-markups)' markup."
  (interactive (list (if (and current-prefix-arg (listp current-prefix-arg))
                         (nth (round (log (car current-prefix-arg) 4))
                              telega-chat-input-markups)
                       (car telega-chat-input-markups))))

  (let ((input (telega-chatbuf-input-string))
        (imcs (telega-chatbuf--input-imcs markup-name))
        (replying-msg (telega-chatbuf--replying-msg))
        (editing-msg (telega-chatbuf--editing-msg))
        (options nil)
        (send-imcs nil))
    ;; Send the input by traversing IMCS and sending composed
    ;; SEND-IMCS
    (while imcs
      (cond
       (editing-msg
        (when (> (length imcs) 1)
          (user-error "Multiple input messages while edit"))
        (setq send-imcs (seq-take imcs 1))
        (let ((edit-mc (plist-get editing-msg :content))
              (imc (car send-imcs)))
          (cond ((and ;(eq (telega--tl-type imc) 'inputMessageLocation)
                  (eq (telega--tl-type edit-mc) 'messageLocation))
                 (telega--editMessageLiveLocation
                  editing-msg (plist-get imc :location)
                  :sync-p (not telega-chat-send-messages-async)))

                ((and (eq (telega--tl-type imc) 'inputMessageText)
                      (eq (telega--tl-type edit-mc) 'messageText))
                 (telega--editMessageText
                  editing-msg imc
                  :sync-p (not telega-chat-send-messages-async)))

                ((eq (telega--tl-type imc) 'inputMessageText)
                 (telega--editMessageCaption
                  editing-msg (plist-get imc :text)
                  :sync-p (not telega-chat-send-messages-async)))

                (t
                 (telega--editMessageMedia
                  editing-msg imc
                  :sync-p (not telega-chat-send-messages-async))))))

       ;; Messages can be sent as album if:
       ;; - All messages are photos or videos
       ;; - All messages are documents
       ;; - All messages are audio
       ;; NOTE: cl-every returns `t' on empty list
       ;;
       ;; NOTE: maximum 10 messages can be grouped to album
       ;; See https://t.me/emacs_telega/22918
       ((and (> (length imcs) 1)
             (let ((album-types
                    (cl-find (telega--tl-type (car imcs))
                             '((inputMessagePhoto inputMessageVideo)
                               (inputMessageDocument)
                               (inputMessageAudio))
                             :test #'memq)))
               (setq send-imcs
                     (seq-take (seq-take-while
                                (lambda (imc)
                                  (memq (telega--tl-type imc) album-types))
                                imcs)
                               10))))
        (telega--sendMessageAlbum
         telega-chatbuf--chat send-imcs replying-msg options
         :sync-p (not telega-chat-send-messages-async)))

       ;; NOTE: TDLib will automatically group messages to albums when
       ;; forwarding multiple messages.  Message IDS must be in strictly
       ;; increasing order, otherwise TDLib triggers an error
       ((and (> (length imcs) 1)
             (not replying-msg)
             (let ((msg-id 0)
                   (chat-id (telega--tl-get (car imcs) :message :chat_id))
                   (send-copy (plist-get (car imcs) :send_copy))
                   (rm-caption (plist-get (car imcs) :remove_caption)))
               (setq send-imcs
                     (seq-take-while
                      (lambda (imc)
                        (and (eq (telega--tl-type imc) 'telegaForwardMessage)
                             (eq chat-id (telega--tl-get imc :message :chat_id))
                             (not (plist-get imc :caption))
                             (equal send-copy (plist-get imc :send_copy))
                             (equal rm-caption (plist-get imc :remove_caption))
                             ;; Check for strictly increasing ID order
                             (when (> (telega--tl-get imc :message :id) msg-id)
                               (setq msg-id (telega--tl-get imc :message :id)))))
                      imcs))))
        (telega--forwardMessages
         telega-chatbuf--chat
         (telega-msg-chat (plist-get (car send-imcs) :message))
         (mapcar (telega--tl-prop :message) send-imcs) options
         (plist-get (car send-imcs) :send_copy)
         (plist-get (car send-imcs) :remove_caption)
         :sync-p (not telega-chat-send-messages-async)))

     (t
      (setq send-imcs (seq-take imcs 1))
      (let ((imc (car send-imcs)))
        (cl-case (telega--tl-type imc)
          (telegaInlineQuery
           (telega--sendInlineQueryResultMessage
            telega-chatbuf--chat imc replying-msg options
            :sync-p (not telega-chat-send-messages-async)))

          (telegaForwardMessage
           (let* ((msg (plist-get imc :message))
                  (copy-opts
                   (nconc (list :@type "messageCopyOptions"
                                ;; NOTE: force copying if replying
                                ;; to message.  TDLib 1.7.10 can
                                ;; forward copy as reply to a
                                ;; message
                                :send_copy
                                (if (or (plist-get imc :send_copy) replying-msg)
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
             (telega--sendMessage
              telega-chatbuf--chat
              fwd-imc replying-msg options
              :sync-p (not telega-chat-send-messages-async))
             (when (plist-get imc :unmark-after-sent)
               (telega-msg-unmark msg))))

          ((telegaScheduledMessage telegaDisableNotification)
           ;; Merge new imc options into existing options
           (telega--tl-dolist ((prop value) (telega-chatbuf--input-options imc))
             (setq options (plist-put options prop value))))

          (t (telega--sendMessage
              telega-chatbuf--chat imc replying-msg options
              :sync-p (not telega-chat-send-messages-async)))))))

      ;; NOTE: Cancell all file upload ahead, initiated by
      ;; attachements in `send-imcs' See
      ;; https://github.com/tdlib/td/issues/1348#issuecomment-752465634
      ;; NOTE: Currently this does not cancel uploads, as noted in
      ;; https://github.com/tdlib/td/issues/1348#issuecomment-752654650
      (dolist (imc send-imcs)
        (telega-chatbuf--input-imc-cancel-upload-ahead imc))

      ;; Continue traversing, stripping SEND-IMCS from IMCS
      ;; Each cond clause above must set SEND-IMCS
      (cl-assert (> (length send-imcs) 0))
      (setq imcs (last imcs (- (length imcs) (length send-imcs)))
            send-imcs nil))

    ;; Recover prompt to initial state
    (telega-chatbuf--input-delete)
    (telega-chatbuf--prompt-reset)

    ;; Save input to history
    (unless (string-empty-p input)
      (ring-insert telega-chatbuf--input-ring input)
      (setq telega-chatbuf--input-idx nil
            telega-chatbuf--input-pending nil))))

(defun telega-chatbuf-newline-or-input-send ()
  "Insert newline or send chatbuf input.
Behaviour depends on point position and value for
`telega-chat-ret-always-sends-message'."
  (interactive)
  (if (and (not telega-chat-ret-always-sends-message)
           (< (point) (point-max)))
      (call-interactively #'newline)
    (call-interactively #'telega-chatbuf-input-send)))

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

(defun telega-chatbuf--enable-compact-media-view ()
  "Enable compact view for media messages.
For filters from `telega-chat-message-filters-as-media'."
  (setq telega-chatbuf--messages-compact-view t)
  (setq-local telega-inserter-for-msg-button
              #'telega-ins--message-media-compact)
  (setq-local telega-ignored-messages-visible nil))

(defun telega-chatbuf--disable-compact-media-view ()
  "Disable compact view for media messages.
For filters from `telega-chat-message-filters-as-media'."
  (setq telega-chatbuf--messages-compact-view nil)
  (kill-local-variable 'telega-inserter-for-msg-button)
  (kill-local-variable 'telega-ignored-messages-visible))

(defun telega-chatbuf--reset-filter-and-thread (&optional no-redisplay)
  "Reset messages filtering and thread filtering.
Reset `telega-chatbuf--msg-filter' and `telega-chatbuf--thread-msg'."
  (unless telega-chatbuf--inhibit-reset-filter-and-thread
    (let ((update-prompt-p telega-chatbuf--thread-msg))
      (telega-chatbuf--disable-compact-media-view)
      (setq telega-chatbuf--msg-filter nil)
      (setq telega-chatbuf--thread-msg nil)

      (when update-prompt-p
        (telega-chatbuf--prompt-update))

      (unless no-redisplay
        (telega-chatbuf--modeline-update)
        (telega-chatbuf--footer-update))
      )))

(defun telega-chatbuf--clean ()
  "Remove all messages displayed in chatbuf."
  (telega-ewoc--clean telega-chatbuf--ewoc)
  (setq telega-chatbuf--history-state nil))

(defun telega-chatbuf-history-beginning ()
  "Jump to the first message in the chat history."
  ;; See https://github.com/tdlib/td/issues/195
  (interactive)
  (if (not (telega-chatbuf--need-older-history-p))
      (goto-char (point-min))

    (telega-chatbuf--clean)
    (telega-chat--load-history
        telega-chatbuf--chat 10
        ;; NOTE: For `searchChatMessages' limit must be greater than -offset
        (- 1 telega-chat-history-limit) telega-chat-history-limit
      (lambda (_ignored)
        (telega-chatbuf--older-history-loaded)
        (goto-char (point-min))))))

(defun telega-chatbuf-recenter-1 (arg)
  "Recenter for chatbuf.
Call `(recenter -1)' if point is at prompt, otherwise call `recenter' as-is."
  (interactive "P")
  (if (and (not arg) (<= telega-chatbuf--input-marker (point)))
      (recenter -1)
    (when (commandp telega-chatbuf--origin-recenter-command)
      (call-interactively telega-chatbuf--origin-recenter-command))))

(defun telega-chatbuf-read-all (&optional reset-filter-and-thread)
  "Jump to the last message in the chat history and mark all messages as read.
If `\\[universal-argument]' is used, then reset active messages filter."
  (interactive "P")
  ;; NOTE: Load most recent history if last message is not yet loaded
  ;; or if message filtering is enabled and RESET-FILTER-AND-THREAD is
  ;; specified
  (when (or (not (telega-chatbuf--last-msg-loaded-p))
            (and reset-filter-and-thread
                 (or telega-chatbuf--msg-filter
                     telega-chatbuf--thread-msg)))
    (when reset-filter-and-thread
      (telega-chatbuf--reset-filter-and-thread))
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
    (telega-chatbuf--modeline-update)))

(defun telega-chatbuf-next-unread (&optional button-callback)
  "Goto next uneard message in chat.
BUTTON-CALLBACK - callback to call with single argument - message
button."
  (declare (indent 0))
  (interactive)
  (let ((telega-chatbuf--inhibit-reset-filter-and-thread t))
    (telega-chat--goto-msg telega-chatbuf--chat
        (telega-chatbuf--last-read-inbox-msg-id) nil
      (lambda ()
        ;; NOTE:
        ;; - deleted messages can't be marked as read, so point will
        ;;   stuck at deleted messag, so we just skip such messages
        ;; - `telega-button-forward' returns nil if there is no button
        ;;   matching predicate.  In this case just move to the prompt
        (let ((button (telega-button-forward 1
                        (lambda (button)
                          (when-let ((msg (telega-msg-at button)))
                            (and (not (telega-msg-internal-p msg))
                                 (not (plist-get
                                       msg :telega-is-deleted-message)))))
                        'interactive)))
          (if button
              (when button-callback
                (funcall button-callback button))
            (goto-char (point-max))))))))

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
              (list :@type "searchMessagesFilterUnreadMention") "" 0 0 1))
         (next-unread-mention-msg
          (car (append (plist-get reply :messages) nil))))
    (unless next-unread-mention-msg
      (user-error "telega: Can't fetch next unread mention message"))
    (telega-msg-goto next-unread-mention-msg 'highlight)
    ))

(defun telega-chatbuf-goto-reply-markup-message ()
  "Goto chat's reply markup message."
  (interactive)
  (let ((reply-markup-msg
         (telega-chat-reply-markup-msg telega-chatbuf--chat)))
    (unless reply-markup-msg
      (user-error "telega: No reply markup message for this chat"))

    (telega-msg-goto-highlight reply-markup-msg)))

(defun telega-chat-linked-chat (chat)
  "Return linked chat for the CHAT.
Return nil if CHAT has no linked chat."
  (let ((supergroup (telega-chat--info chat)))
    (when (plist-get supergroup :has_linked_chat)
      (telega-chat-get
       (plist-get (telega--full-info supergroup) :linked_chat_id) 'offline))))

(defun telega-chatbuf-goto-linked-chat ()
  "Goto chat linked to current chat buffer channel."
  (interactive)
  (let ((linked-chat (telega-chat-linked-chat telega-chatbuf--chat)))
    (unless linked-chat
      (user-error "telega: %s has no linked chat"
                  (telega-chat-title telega-chatbuf--chat)))

    (telega-chat--pop-to-buffer linked-chat)))

(defun telega-chatbuf-goto-pinned-message ()
  "Goto next pinned message for the chatbuffer."
  (interactive)
  (let* ((pinned-messages (plist-get telega-chatbuf--chat
                                     :telega-pinned-messages))
         (pinned-msg-idx (plist-get telega-chatbuf--chat
                                    :telega-pinned-message-index))
         (pinned-msg (nth pinned-msg-idx pinned-messages)))
    (unless pinned-msg
      (user-error "telega: No pinned messages in this chat"))

    ;; Update the index for next
    ;; `telega-chatbuf-goto-pinned-message'
    (setq pinned-msg-idx (1+ pinned-msg-idx))
    (unless (< pinned-msg-idx (length pinned-messages))
      (setq pinned-msg-idx 0))
    (plist-put telega-chatbuf--chat
               :telega-pinned-message-index pinned-msg-idx)
    (telega-chatbuf--modeline-update)

    (telega-msg-goto-highlight pinned-msg)))

(defun telega-chatbuf-goto-thread-message ()
  "Goto current thread's root message."
  (interactive)
  (unless telega-chatbuf--thread-msg
    (user-error "telega: No thread filtering in chatbuf"))

  (telega-msg-goto-highlight telega-chatbuf--thread-msg))

(defun telega-chatbuf-goto-pop-message ()
  "Pop message from `telega-chatbuf--messages-pop-ring' and goto it."
  (interactive)
  (when (ring-empty-p telega-chatbuf--messages-pop-ring)
    (user-error "telega: No messages to pop to"))

  (let ((pop-to-msg (ring-remove telega-chatbuf--messages-pop-ring 0)))
    (message "telega: %d messages left in messages ring"
             (ring-length telega-chatbuf--messages-pop-ring))
    ;; NOTE: by binding `telega-chatbuf--messages-pop-ring' to nil, we
    ;; avoid putting current message into
    ;; `telega-chatbuf--messages-pop-ring'
    (let ((telega-chatbuf--messages-pop-ring nil))
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
                                                preview-p upload-ahead-callback)
  "Generate InputFile using FILENAME.
If PREVIEW-P is non-nil, then generate preview image.
UPLOAD-AHEAD-CALLBACK is callback for file updates, when uploading
ahead in case `telega-chat-upload-attaches-ahead' is non-nil."
  (setq filename (expand-file-name filename))
  (let ((preview (when (and preview-p (> (telega-chars-xheight 1) 1))
                   (create-image filename
                                 (when (fboundp 'imagemagick-types) 'imagemagick)
                                 nil
                                 :scale 1.0 :ascent 'center
                                 :height (telega-chars-xheight 1))))
        (upload-ahead-file
         (when telega-chat-upload-attaches-ahead
           (telega-file--upload filename file-type 16 upload-ahead-callback))))
    (list :@type (propertize "inputFileLocal"
                             'telega-preview preview
                             'telega-upload-ahead-file upload-ahead-file)
          :path filename)))

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
  "Attach a file as self destructing photo.
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
  "Attach a file as self destructing video.
This attachment can be used only in private chats."
  (interactive (list (read-file-name "Video: ")
                     (read-number "Self desctruct in seconds (0-60): ")))
  (telega-chatbuf-attach-video filename ttl))

(defun telega-chatbuf-attach-audio (filename)
  "Attach FILENAME as audio to the chatbuf input."
  (interactive (list (read-file-name "Audio: ")))
  (let ((ifile (telega-chatbuf--gen-input-file filename 'Audio))
        (metadata (telega-ffplay-get-metadata filename)))
    (telega-chatbuf-input-insert
     (list :@type "inputMessageAudio"
           :audio ifile
           :title (cdr (assoc "title" metadata))
           :performer (cdr (assoc "artist" metadata))
           ))))

(defun telega-chatbuf-attach-video-note (as-file-p)
  "Attach a (circled) video note to the chatbuf input.
If `\\[universal-argument] is given, then attach existing file as
video-note.  Otherwise record video note inplace.
`telega-vvnote-video-cmd' is used to record video notes."
  (interactive "P")
  ;; TODO: start video note generation process
  ;; see https://github.com/tdlib/td/issues/126
  (let* ((filename (if as-file-p
                       (read-file-name "Video Note: ")
                     (telega-vvnote-video--record)))
         (ifile (telega-chatbuf--gen-input-file filename 'VideoNote))
         (frame1 (plist-get telega-vvnote-video--preview :first-frame)))
    (telega-chatbuf-input-insert
     (nconc
      (list :@type "inputMessageVideoNote"
            :duration (round (telega-ffplay-get-duration filename))
            :video_note ifile)
      (when frame1
        `(:thumbnail
          (:@type "inputThumbnail"
                  :thumbnail (:@type "inputFileLocal" :path ,frame1)
                  :width 240
                  :height 240)))))))

(defun telega-chatbuf-attach-voice-note (as-file-p)
  "Attach a voice note to the chatbuf input.
If `\\[universal-argument] is given, then attach existing file as
voice-note.  Otherwise record voice note inplace.
`telega-vvnote-voice-cmd' is used to record voice notes."
  (interactive "P")
  ;; TODO: start voice note generation process
  ;; see https://github.com/tdlib/td/issues/126
  (let* ((filename (if as-file-p
                       (read-file-name "Voice Note: ")
                     (telega-vvnote-voice--record)))
         (ifile (telega-chatbuf--gen-input-file filename 'VoiceNote)))
    (telega-chatbuf-input-insert
     (list :@type "inputMessageVoiceNote"
           :waveform (telega-vvnote--waveform-for-file filename)
           :duration (round (telega-ffplay-get-duration filename))
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
  "Attach a file as animation to the chatbuf input."
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
             (bot-user (and uchat
                            (telega-chat-bot-p uchat)
                            (telega-chat-user uchat 'inc-bots)))
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

(defun telega-chatbuf-attach-poll (question anonymous-p allow-multiple-answers-p
                                            &rest options)
  "Attach poll to the chatbuf input.
Can be used only in group chats.
QUESTION - Title of the poll.
ANONYMOUS-P - Non-nil to create anonymous poll.
ALLOW-MULTIPLE-ANSWERS-P - Non-nil to allow multiple answers.
OPTIONS - List of strings representing poll options."
  (interactive
   (let ((poll-q (read-string
                  (concat (telega-i18n "lng_polls_public")
                          " "
                          (telega-i18n "lng_polls_create_question")
                          ": ")))
         (optidx 1) opt poll-opts)
     (while (not (string-empty-p
                  (setq opt (read-string
                             (format "Option %d): " optidx)))))
       (setq poll-opts (append poll-opts (list opt)))
       (cl-incf optidx))
     (nconc (list poll-q
                  (y-or-n-p
                   (concat (telega-i18n "lng_polls_create_anonymous") "? "))
                  (y-or-n-p
                   (concat (telega-i18n "lng_polls_create_multiple_choice") "? ")))
            poll-opts)))

  (telega-chatbuf-input-insert
   (list :@type "inputMessagePoll"
         :question question
         :is_anonymous (if anonymous-p t :false)
         :type (list :@type "pollTypeRegular"
                     :allow_multiple_answers
                     (if allow-multiple-answers-p t :false))
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
  (when (eobp)
    (telega-momentary-display
     (propertize (telega-i18n "telega_disable_webpage_preview_help")
                 'face 'shadow))))

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
is used as FILE.
If current buffer is dired, then send all marked files."
  (interactive
   (let ((send-photo-p
          (and (not current-prefix-arg) (derived-mode-p 'image-mode)))
         (file
          (or (buffer-file-name)
              (when (eq 'dired-mode major-mode)
                (seq-filter #'file-regular-p (dired-get-marked-files)))
              (user-error (concat "Can't send current buffer, "
                                  "it does not have corresponding file")))))
     (list file
           (telega-completing-read-chat
            (format "Send %s to chat: "
                    (cond (send-photo-p "PHOTO")
                          ((listp file) (format "%d FILES" (length file)))
                          (t "FILE"))))
           send-photo-p)))

  (cl-assert chat)
  (with-current-buffer (telega-chat--pop-to-buffer chat)
    (let ((inhibit-read-only t)
          (buffer-undo-list t)
          (files (if (listp file) file (list file))))
      ;; NOTE: `telega-chatbuf-attach-XX' might do sync calls to
      ;; TDLib, so we protect it from intermetiate TDLib events
      ;; handling with `with-telega-deferred-events'
      (with-telega-deferred-events
        (dolist (file files)
          (goto-char (point-max))
          (if as-photo-p
              (telega-chatbuf-attach-photo file)
            (telega-chatbuf-attach-file file))))
      )))

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
  (when (and (not (get-buffer-window))
             (>= (point) telega-chatbuf--input-marker))
    (setq telega-chatbuf--refresh-point
          (- (point) telega-chatbuf--input-marker))
    (goto-char (ewoc-location (ewoc--footer telega-chatbuf--ewoc)))
    (telega-buffer--hack-win-point))
  )

(defun telega-chatbuf--switch-in ()
  "Called when switching to chat buffer."
  (telega-debug "Switch %s: %s" (propertize "IN" 'face 'bold)
                (buffer-name))
  (telega--openChat telega-chatbuf--chat)

  ;; Recover point position in the prompt, saved in
  ;; `telega-chatbuf--switch-out'. Jump to the last unread message,
  ;; just as if chat was freshly opened.
  (when-let ((rpoint telega-chatbuf--refresh-point))
    (setq telega-chatbuf--refresh-point nil)
    (cond ((eq t rpoint)
           ;; Just refresh point's visual appearance
           (when-let ((button (button-at (point))))
             (telega-button--make-observable button 'force)))
          ((not (telega-msg-at (point)))
           ;; No new messages arrived
           (goto-char (+ telega-chatbuf--input-marker rpoint)))
          (t
           ;; New messages arrived, move to next unread then
           (telega-chatbuf-next-unread
             (lambda (button)
               (telega-chatbuf--view-msg-at button)
               (if (and (eq (telega-msg-at button) (telega-chatbuf--last-msg))
                        (telega-button--observable-p
                         telega-chatbuf--input-marker))
                   (goto-char (+ telega-chatbuf--input-marker rpoint))
                 (telega-button--make-observable button 'force)))))))

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
      (telega-chatbuf--footer-update))

    (when-let ((msg-node (or node (telega-chatbuf--node-by-msg-id
                                   (plist-get msg :id)))))
      (telega-chatbuf--redisplay-node msg-node))))

(defun telega-msg-activate-voice-note (msg &optional for-chat)
  "Activate voice note MSG FOR-CHAT.
MSG can be nil in case there is no active voice message."
  (with-telega-chatbuf (or for-chat (telega-msg-chat msg))
    (setq telega-chatbuf--voice-msg msg)
    (telega-chatbuf--footer-update)))

(defun telega-msg-reply (msg)
  "Start replying to MSG."
  (interactive (list (telega-msg-for-interactive)))

  (with-telega-chatbuf (telega-msg-chat msg)
    (telega-button--update-value
     telega-chatbuf--aux-button msg
     :inserter #'telega-ins--prompt-aux-reply
     'invisible nil)

    (telega-chatbuf--prompt-update)
    (goto-char (point-max))

    (telega-help-message--cancel-aux 'reply)))

(defun telega-msg-edit (msg)
  "Start editing the MSG."
  (interactive (list (telega-msg-for-interactive)))

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

      (telega-chatbuf--prompt-update))

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
If `\\[universal-argument]' is given, then forward message copy.
If `\\[universal-argument]' `\\[universal-argument]' is given,
then forward message copy without caption."
  (interactive (list current-prefix-arg
                     (> (prefix-numeric-value current-prefix-arg) 4)))
  (when-let ((messages (or (reverse telega-chatbuf--marked-messages)
                           (when-let ((msg-at-point (telega-msg-at (point))))
                             (list msg-at-point)))))
    (let ((chat (telega-completing-read-chat
                 (concat (telega-symbol 'forward) "Forward"
                         (when send-copy-p " Copy")
                         (when rm-cap-p " NewCap")
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

    (telega--deleteMessages (list msg) revoke)))

(defun telega-chatbuf-marked-messages-delete (revoke)
  "Delete marked messages in chatbuf.
If `\\[universal-argument]' is specified, then kill
messages (delete for me only), otherwise revoke message (delete
for everyone).
If chatbuf is supergroups, channels or secret chat, then always revoke."
  (interactive (list (or (memq (telega-chat--type telega-chatbuf--chat 'raw)
                               '(supergroup secret))
                         (not current-prefix-arg))))
  (when-let ((marked-messages telega-chatbuf--marked-messages))
    (when (yes-or-no-p (telega-i18n (if revoke
                                        "telega_query_revoke_marked_messages"
                                      "telega_query_kill_marked_messages")
                         :count (length marked-messages)))
      (setq telega-chatbuf--marked-messages nil)
      (dolist (msg marked-messages)
        (telega-msg-delete0 msg revoke))
      (telega-chatbuf--modeline-update))))

(defun telega-msg-delete-at-down-mouse-3 ()
  "Delete message at mouse down point if any."
  (interactive)
  (when (telega-msg-at-down-mouse-3)
    (let ((telega-chatbuf--marked-messages nil))
      (call-interactively #'telega-msg-delete-marked-or-at-point))))

(defun telega-msg-delete-marked-or-at-point (revoke)
  "Deletes some messages.
If some messages are marked, then delete them.  Otherwise delete
message at point.
If `\\[universal-argument]' REVOKE is specified, then kill
messages (delete for me only), otherwise revoke message (delete
for everyone).
REVOKE forced to non-nil for supergroup, channel or a secret chat."
  (interactive (list (or (memq (telega-chat--type telega-chatbuf--chat 'raw)
                               '(supergroup secret))
                         (not current-prefix-arg))))

  (if telega-chatbuf--marked-messages
      (telega-chatbuf-marked-messages-delete revoke)

    (when-let ((msg (telega-msg-for-interactive)))
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
                                   telega-emoji-company-backend)
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
      (and
       ;; Skip internal telega messages, such at (Discussion Started)
       (not (when-let ((msg (telega-msg-at button)))
              (telega-msg-internal-p msg)))
       (not (eq (button-type button) 'telega-prompt))))))

(defun telega-chatbuf-prev-link (n)
  (interactive "p")
  (when (<= telega-chatbuf--input-marker (point))
    (goto-char (ewoc-location (ewoc--footer telega-chatbuf--ewoc))))
  (telega-chatbuf-next-link (- n)))

(defun telega-chatbuf-complete-or-next-link ()
  "Complete username at point, or jump to next link."
  (interactive)
  (if (<= telega-chatbuf--input-marker (point))
      (call-interactively #'telega-chatbuf-complete)
    (call-interactively #'telega-chatbuf-next-link)))

(defun telega-chat-generate-invite-link (chat)
  "Generate invite link for CHAT.
If called interactively then copy generated link into the kill ring."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))

  (let ((link (telega--generateChatInviteLink chat)))
    (when (called-interactively-p 'interactive)
      (kill-new link)
      (message "Invite link: %s (copied into kill ring)" link))
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
      (telega-chatbuf--view-msg-at (point))
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

  ;; 1. Put message at point into messages ring
  ;; 2. If message seen in chatbuf, jump to it
  ;; 3. Otherwise, fetch history containing message and jump to it
  (with-current-buffer (telega-chat--pop-to-buffer chat :no-history)
    (when (ring-p telega-chatbuf--messages-pop-ring)
      (when-let ((msg-at-point (telega-msg-at (point))))
        (ring-insert telega-chatbuf--messages-pop-ring msg-at-point)

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
    (if (or (zerop msg-id)
            (telega-chatbuf--goto-msg msg-id highlight)
            (when-let ((first-msg (telega-chatbuf--first-msg))
                       (last-msg (telega-chatbuf--last-msg)))
              (when (not (or (and (< msg-id (plist-get first-msg :id))
                                  (telega-chatbuf--need-older-history-p))
                             (and (> msg-id (plist-get last-msg :id))
                                  (telega-chatbuf--need-newer-history-p))))
                (cond ((< msg-id (plist-get first-msg :id))
                       (goto-char (point-min)))
                      ((> msg-id (plist-get last-msg :id))
                       (goto-char (point-max)))
                      (t
                       (message "Message(ID=%S) not found in chatbuf" msg-id)

                       ;; NOTE: Move point to the message before
                       ;; deleted message with MSG-ID
                       (goto-char (point-max))
                       (when-let ((prev-node
                                   (telega-ewoc--find
                                    telega-chatbuf--ewoc
                                    msg-id #'> (telega--tl-prop :id)
                                    nil #'ewoc--node-prev)))
                         (ewoc-goto-node telega-chatbuf--ewoc prev-node))))
                t)))
        (when callback
          (funcall callback))

      (telega-chatbuf--reset-filter-and-thread)
      (telega-chatbuf--clean)
      (telega-chat--load-history
          chat msg-id (- (/ telega-chat-history-limit 2)) nil
        (lambda (_ignored)
          (telega-chatbuf--goto-msg msg-id highlight)
          (when callback
            (funcall callback)))))))

(defun telega-chat--goto-thread (chat thread-msg-id
                                      &optional reply-msg-id)
  "Goto reply thread THREAD-MSG."
  (let* ((thread-info (or (telega--getMessageThread chat thread-msg-id)
                          (error "Thread not available")))
         (thread-chat (telega-chat-get
                       (plist-get thread-info :chat_id) 'offline))
         (thread-msg
          (car (last (append (plist-get thread-info :messages) nil)))))
    (with-current-buffer (telega-chat--pop-to-buffer thread-chat :no-history)
      (setq telega-chatbuf--thread-info thread-info)
      (telega-chatbuf-filter-by-thread thread-msg
                                       (when reply-msg-id :no-history))
      (when reply-msg-id
        (let ((telega-chatbuf--inhibit-reset-filter-and-thread t))
          (telega-chat--goto-msg thread-chat reply-msg-id 'highlight))))))

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


(defconst telega-chat--message-filters
  '(("scheduled" . telega-chatbuf-filter-scheduled)
    ("search" . telega-chatbuf-filter-search)
    ("by-sender" . telega-chatbuf-filter-by-sender)
    ("hashtag" . telega-chatbuf-filter-hashtag)
    ("photo" :@type "searchMessagesFilterPhoto")
    ("photo-video" :@type "searchMessagesFilterPhotoAndVideo")
    ("url" :@type "searchMessagesFilterUrl")
    ("doc" :@type "searchMessagesFilterDocument")
    ("file" :@type "searchMessagesFilterDocument")
    ("gif" :@type "searchMessagesFilterAnimation")
    ("audio" :@type "searchMessagesFilterAudio")
    ("video" :@type "searchMessagesFilterVideo")
    ("voice-note" :@type "searchMessagesFilterVoiceNote")
    ("video-note" :@type "searchMessagesFilterVideoNote")
    ("voice-video-note" :@type "searchMessagesFilterVoiceAndVideoNote")
    ("chat-photo" :@type "searchMessagesFilterChatPhoto")
    ("call" :@type "searchMessagesFilterCall")
    ("missed-call" :@type "searchMessagesFilterMissedCall")
    ("mention" :@type "searchMessagesFilterMention")
    ("unread-mention" :@type "searchMessagesFilterUnreadMention")
    ("failed-to-send" :@type "searchMessagesFilterFailedToSend")
    ("pinned" :@type "searchMessagesFilterPinned")))

(defun telega-chatbuf-filter (msg-filter)
  "Enable chat message filtering MSG-FILTER."
  (interactive
   (let* ((filter-name
           (funcall telega-completing-read-function
                    "Chat Messages Filter: "
                    (mapcar #'car telega-chat--message-filters) nil t))
          (tdlib-msg-filter
           (cdr (assoc filter-name telega-chat--message-filters))))
     (list (list :title filter-name
                 :tdlib-msg-filter tdlib-msg-filter))))

  (cl-assert msg-filter)
  (if (commandp (plist-get msg-filter :tdlib-msg-filter) 'for-interactive)
      (call-interactively (plist-get msg-filter :tdlib-msg-filter))

    (cl-assert (listp (plist-get msg-filter :tdlib-msg-filter)))
    (setq telega-chatbuf--msg-filter msg-filter)
    (when (member (plist-get msg-filter :title)
                  telega-chat-message-filters-as-media)
      (telega-chatbuf--enable-compact-media-view))
    (telega-chatbuf--clean)
    (telega-chatbuf--modeline-update)

    (telega-chatbuf--load-older-history
     (lambda (total-messages)
       (when telega-chatbuf--msg-filter
         (plist-put telega-chatbuf--msg-filter :total-count total-messages)
         (telega-chatbuf--footer-update))))

    (telega-help-message 'msg-filter-cancel
        "%s to cancel messages filtering"
      (telega-keys-description
       'telega-chatbuf-filter-cancel telega-chat-mode-map)))

  (telega-chatbuf--footer-update))

(defun telega-chatbuf-filter-by-thread (msg &optional no-history-load)
  "Show only messages for thread starter message MSG.
If NO-HISTORY-LOAD is specified, do not load history."
  ;; NOTE: aparently message id and thread id equals for thread
  ;; starters
  (cl-assert (eq (plist-get msg :id) (plist-get msg :message_thread_id)))
  (setq telega-chatbuf--thread-msg msg)
  (telega-chatbuf--clean)
  (unless no-history-load
    (telega-chatbuf--load-initial-history))

  (telega-chatbuf--modeline-update)
  (telega-chatbuf--footer-update)
  (telega-chatbuf--prompt-update)
  )

(defun telega-chatbuf-filter-search (&optional query by-sender-p)
  "Interactively search for messages in chatbuf.
If `\\[universal-argument]' is given, then search for QUERY sent
by some chat member, member name is queried."
  (interactive "sSearch Query: \nP")
  (let ((by-sender (when by-sender-p
                     (telega-completing-read-chat-member
                      "Sent by: " telega-chatbuf--chat))))
    (telega-chatbuf-filter
     (list :title (format "search \"%s\"" query)
           :tdlib-msg-filter (list :@type "searchMessagesFilterEmpty")
           :query query
           :sender by-sender))

    (when (and query (not (string-empty-p query)))
      (telega-highlight-text (regexp-quote query)))
    ))

(defun telega-chatbuf-filter-by-sender ()
  "Show only messages send by some member, member is queried."
  (interactive)
  (telega-chatbuf-filter-search "" 'by-sender))

(defun telega-chatbuf-filter-hashtag (hashtag &optional by-sender-p)
  "Show only messages marked with HASHTAG.
If `\\[universal-argument]' is given, then search for HASHTAG
sent by some chat member, member name is queried."
  (interactive (list (funcall telega-completing-read-function
                              "Hashtag: #" (telega--searchHashtags ""))
                     current-prefix-arg))
  (telega-chatbuf-filter-search
   (concat (unless (string-prefix-p "#" hashtag) "#") hashtag)
   by-sender-p))

(defun telega-chatbuf-filter-scheduled ()
  "Show only scheduled messages."
  (interactive)

  (let ((scheduled-messages
         (telega--getChatScheduledMessages telega-chatbuf--chat)))
    (telega-chatbuf--reset-filter-and-thread 'no-redisplay)
    (telega-chatbuf--clean)
    (setq telega-chatbuf--msg-filter
          (list :title "scheduled"
                :total-count (length scheduled-messages)))
    (telega-chatbuf--modeline-update)
    (telega-chatbuf--footer-update)

    (telega-chatbuf--insert-messages (nreverse scheduled-messages) 'append)))

(defun telega-chatbuf-filter-cancel (&rest _ignored)
  "Cancel any message filtering.
If point is at some message, then keep point on this message after reseting."
  (interactive)
  (when (or telega-chatbuf--msg-filter
            telega-chatbuf--thread-msg)
    ;; NOTE: if point is at some message, then keep this
    ;; message visible, otherwise load initial history
    (let ((msg-at-point (or (telega-msg-at (point))
                            telega-chatbuf--thread-msg)))
      (telega-chatbuf--reset-filter-and-thread)
      (telega-chatbuf--clean)
      (if msg-at-point
          (telega-chat--goto-msg
              telega-chatbuf--chat (plist-get msg-at-point :id) 'highlight)
        (telega-chatbuf--load-initial-history)))

    ;; Make sure text highlighting is disabled, in case
    ;; `telega-chatbuf-filter-search' filter has been used
    (telega-highlight-text-mode -1)

    (telega-chatbuf--modeline-update)
    (telega-chatbuf--footer-update)))


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

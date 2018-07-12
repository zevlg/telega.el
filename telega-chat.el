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
(require 'ring)
(require 'telega-core)
(require 'telega-msg)

(declare-function telega-root--chat-update "telega-root" (chat))
(declare-function telega-root--chat-reorder "telega-root" (chat))

(defsubst telega-chat--ensure (chat)
  "Ensure CHAT resides in `telega--chats' and `telega--ordered-chats'.
Return chat from `telega--chats'."
  (let ((chat-id (plist-get chat :id)))
    (or (gethash chat-id telega--chats)
        (prog1
            chat
          (puthash chat-id chat telega--chats)
          (push chat telega--ordered-chats)))))

(defun telega-chat--get (chat-id &optional offline-p)
  "Get chat by its CHAT-ID.
If OFFLINE-P is non-nil then do not request the telegram-server."
  (let ((chat (gethash chat-id telega--chats)))
    (when (and (not chat) (not offline-p))
      (setq chat (telega-server--call
                  (list :@type "getChat"
                        :chat_id chat-id)))
      (assert chat nil "getChat timed out chat_id=%d" chat-id)
      (telega-chat--ensure chat))
    chat))

(defun telega-chat--by-invite-link (invite-link)
  "Return new chat by its INVITE-LINK.
Return nil if can't join the chat."
  (telega-server--call
   (list :@type "joinChatByInviteLink"
         :invite_link invite-link)))

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
  (telega-chat--get (plist-get (telega-user--me) :id) 'offline))

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

(defun telega-chat--public-p (chat)
  "Return non-nil if CHAT is public.
Public chats are only supergroups with non-empty username."
  (and (eq (telega-chat--type chat 'no-interpret) 'supergroup)
       (not (string-empty-p (plist-get (telega-chat--supergroup chat) :username)))))

(defsubst telega-chat--order (chat)
  (plist-get chat :order))

(defsubst telega-chat--muted-p (chat)
  "Return non-nil if CHAT is muted."
  (> (plist-get (plist-get chat :notification_settings) :mute_for) 0))

(defun telega-chat--title (chat &optional with-username)
  "Return title for the CHAT.
If WITH-USERNAME is specified, append trailing username for this chat."
  (let ((title (plist-get chat :title)))
    (when (string-empty-p title)
      (setq title (ecase (telega-chat--type chat)
                    (private
                     (telega-user--name (telega-chat--user chat) 'name)))))
    (when with-username
      (let ((un (plist-get (telega-chat--info chat) :username)))
        (when (and un (not (string-empty-p un)))
          (setq title (concat title " @" un)))))
    title))

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
  (let ((chat (telega-chat--get (plist-get event :chat_id) 'offline))
        (new-title (plist-get event :title)))
    (assert chat)
    (with-telega-chat-buffer chat
      (rename-buffer (telega-chat-buffer--name chat new-title)))

    (plist-put chat :title new-title)
    (telega-root--chat-update chat)))

(defun telega--on-updateChatOrder (event)
  (let ((chat (telega-chat--get (plist-get event :chat_id) 'offline)))
    (assert chat)
    (telega-chat--reorder chat (plist-get event :order))))

(defun telega--on-updateChatIsPinned (event)
  (let ((chat (telega-chat--get (plist-get event :chat_id) 'offline)))
    (assert chat)
    (plist-put chat :is_pinned (plist-get event :is_pinned))
    (telega-chat--reorder chat (plist-get event :order))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatReadInbox (event)
  (let ((chat (telega-chat--get (plist-get event :chat_id) 'offline)))
    (assert chat)
    (plist-put chat :last_read_inbox_message_id
               (plist-get event :last_read_inbox_message_id))
    (plist-put chat :unread_count
               (plist-get event :unread_count))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatReadOutbox (event)
  (let* ((chat (telega-chat--get (plist-get event :chat_id) 'offline))
         (old-read-outbox-msgid (plist-get chat :last_read_outbox_message_id)))
    (assert chat)
    (plist-put chat :last_read_outbox_message_id
               (plist-get event :last_read_outbox_message_id))
    (with-telega-chat-buffer chat
      (telega-chat-buffer--read-outbox old-read-outbox-msgid))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatUnreadMentionCount (event)
  (let ((chat (telega-chat--get (plist-get event :chat_id) 'offline)))
    (assert chat)
    (plist-put chat :unread_mention_count
               (plist-get event :unread_mention_count))
    (telega-root--chat-update chat)))

(defun telega--on-updateMessageMentionRead (event)
  (telega--on-updateChatUnreadMentionCount event)
  ;; TODO: might be workout with message of `:message_id' as well
  )

(defun telega--on-updateChatReplyMarkup (event)
  (let ((chat (telega-chat--get (plist-get event :chat_id) 'offline)))
    (assert chat)
    (plist-put chat :reply_markup_message_id
               (plist-get event :reply_markup_message_id))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatLastMessage (event)
  (let ((chat (telega-chat--get (plist-get event :chat_id) 'offline)))
    (assert chat)
    (plist-put chat :last_message
               (plist-get event :last_message))
    (telega-chat--reorder chat (plist-get event :order))
    (telega-root--chat-update chat)))

(defun telega--on-updateChatDraftMessage (event)
  (let ((chat (telega-chat--get (plist-get event :chat_id) 'offline)))
    (assert chat)
    (telega-debug "TODO: `telega--on-updateChatDraftMessage' handle draft message")
    (telega-chat--reorder chat (plist-get event :order))
    (telega-root--chat-update chat)))

(defun telega-chat--on-getChats (result)
  "Ensure chats from RESULT exists, and continue fetching chats."
  (let ((chat-ids (plist-get result :chat_ids)))
    (telega-debug "on-getChats: %s" chat-ids)
    (mapc #'telega-chat--ensure (mapcar #'telega-chat--get chat-ids))

    (if (> (length chat-ids) 0)
        ;; Continue fetching chats
        (telega-chat--getChats)

      ;; All chats has been fetched
      (message "telega: all chats are fetched")
      (run-hooks 'telega-chats-fetched-hook))))

(defun telega-chat--getChats ()
  "Retreive all chats from the server in async manner."
  (let* ((last-chat (car (last telega--ordered-chats)))
         (offset-order (or (plist-get last-chat :order) "9223372036854775807"))
         (offset-chatid (or (plist-get last-chat :id) 0)))
    (telega-server--call
     (list :@type "getChats"
           :offset_order offset-order
           :offset_chat_id offset-chatid
           :limit 1000)
     #'telega-chat--on-getChats)))

(defun telega-chat--getPinnedMessage (chat)
  "Get pinned message for the CHAT, if any."
  (when (and (eq (telega-chat--type chat 'raw) 'supergroup)
             (not (zerop (plist-get
                          (telega--full-info (telega-chat--supergroup chat))
                          :pinned_message_id))))
    (telega-server--call
     (list :@type "getChatPinnedMessage"
           :chat_id (plist-get chat :id)))))

(defun telega-chats--kill-em-all ()
  "Kill all chat buffers."
  (dolist (cbuf telega--chat-buffers)
    (kill-buffer cbuf)))

(defun telega-chats--unread (chats)
  "Return total number of unread messages in CHATS."
  (apply #'+ (mapcar (telega--tl-prop :unread_count) chats)))

(defun telega-chats--unread-mentions (chats)
  "Return total number of unread mentions in CHATS."
  (apply #'+ (mapcar (telega--tl-prop :unread_mention_count) chats)))

(defun telega-chat--send-action (chat action)
  "Send ACTION on CHAT."
  (telega-server--send
   (list :@type "sendChatAction"
         :chat_id (plist-get chat :id)
         :action (list :@type (concat "chatAction"
                                      (capitalize (symbol-name action)))))))

(defun telega-chat--create-private (user)
  "Create private chat with USER.
Return newly created chat."
  (telega-server--call
   (list :@type "createPrivateChat"
         :user_id (plist-get user :id))))

(defun telega-chat--view-messages (chat messages)
  "Mark CHAT's MESSAGES as read."
  (telega-server--send
   (list :@type "viewMessages"
         :chat_id (plist-get chat :id)
         :message_ids
         (cl-map 'vector (lambda (msg) (plist-get msg :id)) messages))))

(defun telega-chat-show-info (chat)
  "Show help buffer with info about CHAT."
  (with-help-window " *Telegram Chat Info*"
    (set-buffer standard-output)
    (insert (format "%s: %s %s\n"
                    (capitalize (symbol-name (telega-chat--type chat)))
                    (telega-chat--title chat)
                    (let ((username (plist-get (telega-chat--info chat) :username)))
                      (if (and username (not (string-empty-p username)))
                          (concat "@" username)
                        ""))))
    (insert (format "Id: %d\n" (plist-get chat :id)))
    (when (telega-chat--public-p chat)
      (insert "Link: https://t.me/"
              (plist-get (telega-chat--supergroup chat) :username) "\n"))
    (when telega-debug
      (insert (format "Order: %s\n" (telega-chat--order chat))))

    (let ((not-cfg (plist-get chat :notification_settings)))
      (insert (format "Notifications: %s\n"
                      (if (zerop (plist-get not-cfg :mute_for))
                          (concat "enabled"
                                  (if (plist-get not-cfg :show_preview)
                                      " with preview"
                                    ""))
                        "disabled"))))

    (insert "\n")
    (telega-info--insert (plist-get chat :type) chat)
    ))

(defun telega-chat-toggle-pin (chat)
  "Toggle pin state of the CHAT."
  (telega-server--send
   (list :@type "toggleChatIsPinned"
         :chat_id (plist-get chat :id)
         :is_pinned (if (plist-get chat :is_pinned) :false t))))


;;; Chat buttons in root buffer
(defcustom telega-chat-button-width 28
  "*Width for the chat buttons."
  :type 'integer
  :group 'telega)

(defvar telega-chat-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "i") 'telega-chat-button-info)
    (define-key map (kbd "h") 'telega-chat-button-info)
    (define-key map (kbd "C-c p") 'telega-chat-button-pin)
    (define-key map (kbd "DEL") 'telega-chat-button-delete)
    map)
  "The key map for telega chat buttons.")

(define-button-type 'telega-chat
  :supertype 'telega
  :format #'telega-chat-button--format
  'keymap telega-chat-button-map
  'action #'telega-chat-button--action)

(defun telega-chat-button--format (chat)
  "Formatter for the CHAT button."
  (let ((title (telega-chat--title chat))
        (unread (plist-get chat :unread_count))
        (mentions (plist-get chat :unread_mention_count))
        (pinned-p (plist-get chat :is_pinned))
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
         telega-symbol-pin)
      ,(when (eq (telega-chat--type chat 'raw) 'secret)
         telega-symbol-lock)
      "\n")))

(defun telega-chat-button--action (button)
  "Action to take when chat BUTTON is pressed."
  (telega-chat--pop-to-buffer (button-get button :value)))

(defun telega-chat--pop-to-buffer (chat)
  "Pop to CHAT's buffer."
  (pop-to-buffer (telega-chat-buffer--get-create chat)))

(defun telega-chat-button-pin (button)
  "Toggle chat's pin state at point."
  (interactive (list (button-at (point))))
  (telega-chat-toggle-pin (button-get button :value)))

(defun telega-chat-button-info (button)
  "Show info about chat at point."
  (interactive (list (button-at (point))))
  (telega-chat-show-info (button-get button :value)))

(defun telega-chat-with (name)
  "Start chatting with peer matching NAME."
  (interactive
   (let ((completion-ignore-case t))
     (list (funcall telega-completing-read-function
                    "Chat with: " (telega-completing-titles) nil t))))

  (let ((chat (cl-find name telega--ordered-chats
                       :test (lambda (needname chat)
                               (string= (telega-chat--title chat 'with-username)
                                        needname)))))
    (unless chat
      (let ((user (cl-find name (hash-table-values (cdr (assq 'user telega--info)))
                           :test (lambda (needname user)
                                   (string= (telega-user--name user) needname)))))
        (setq chat (telega-chat--create-private user))))

    (telega-chat--pop-to-buffer chat)))

(defun telega-chat-join-by-link (link)
  "Join chat by invitation LINK."
  (interactive "sInvite link: ")
  (telega-chat--pop-to-buffer
   (or (telega-chat--by-invite-link link)
       (error "Can't join chat: %s"
              (plist-get telega-server--last-error :message)))))


;;; Chat Buffer, resembles lui.el
(defgroup telega-chat nil
  "Customization for telega-chat-mode"
  :prefix "telega-chat-"
  :group 'telega)

(defvar telega-chat-mode-hook nil
  "Hook run when telega chat buffer is created.")

(defvar telega-chat-mode-map
  (let ((map (make-sparse-keymap)))
    ;; C-M-[ - cancels edit/reply
    (define-key map (kbd "\e\e") 'telega-chat-cancel-edit-reply)
    (define-key map (kbd "C-M-c") 'telega-chat-cancel-edit-reply)

    (define-key map (kbd "C-c C-a") 'telega-chat-insert-media)
    (define-key map (kbd "C-c C-v") 'telega-chat-insert-media-clipboard)
    (define-key map (kbd "C-c C-f") 'telega-chat-insert-file)
    (define-key map (kbd "C-c ?") 'telega-chat-buffer-info)

    (define-key map (kbd "RET") 'telega-chat-send)
    (define-key map (kbd "M-p") 'telega-chat-input-prev)
    (define-key map (kbd "M-n") 'telega-chat-input-next)
    (define-key map (kbd "M-r") 'telega-chat-input-search)

    ;; jumping around links
    (define-key map (kbd "TAB") 'telega-chat-complete-or-next-link)
    (define-key map (kbd "<backtab>") 'telega-chat-prev-link)
    map))

(defvar telega-chatbuf--chat nil
  "Telega chat for the current chat buffer.")
(make-variable-buffer-local 'telega-chatbuf--chat)

(defvar telega-chatbuf--chat-action nil
  "Current action on the chat.")
(make-variable-buffer-local 'telega-chatbuf--chat-action)

(defvar telega-chatbuf--replies nil
  "List of active replies in chat buffer.
Used becase `telega-msg--get' requests the server and it causes
delays/errors when formatting messages with
`:reply_to_message_id'")
(make-variable-buffer-local 'telega-chatbuf--replies)

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

(defvar telega-chatbuf--auxmsg-button nil
  "Button that display reply/edit message above input.")
(make-variable-buffer-local 'telega-chatbuf--auxmsg-button)

(defvar telega-chatbuf--prompt-button nil "Input prompt button.")
(make-variable-buffer-local 'telega-chatbuf--prompt-button)

(defvar telega-chatbuf--history-loading nil "Non-nil if history has been requested.")
(make-variable-buffer-local 'telega-chatbuf--history-loading)

(defvar telega-chatbuf--send-func nil
  "Function to call in order to send message.
Used to reply to messages and edit message.")
(make-variable-buffer-local 'telega-chatbuf--send-func)

(defvar telega-chatbuf--send-args nil
  "Additional arguments to `telega-chatbuf--send-func'.")
(make-variable-buffer-local 'telega-chatbuf--send-args)

(define-button-type 'telega-prompt
  :supertype 'telega
  :format '((identity :face telega-chat-prompt))
  'rear-nonsticky t
  'front-sticky t
  'read-only t
  'cursor-intangible t
  'field 'telega-prompt)

(define-button-type 'telega-prompt-aux
  :supertype 'telega
  :format 'identity
  'read-only t
  'cursor-intangible t
  'field 'telega-prompt)

(define-derived-mode telega-chat-mode nil "Telega-Chat"
  "The mode for telega chat buffer.
Keymap:
\\{telega-chat-mode-map}"
  :group 'telega-chat
  (setq telega-chatbuf--input-ring (make-ring telega-chat-input-ring-size)
        telega-chatbuf--input-idx nil
        telega-chatbuf--input-pending-p nil
        telega-chatbuf--chat-action nil
        telega-chatbuf--history-loading nil
        telega-chatbuf--send-func 'telega-chat-send-msg
        telega-chatbuf--send-args nil
        telega-chatbuf--replies nil)

  (erase-buffer)
  (setq-local window-point-insertion-type t)
  (setq-local next-line-add-newlines nil)

  (cursor-intangible-mode 1)
  ;; Separator for `telega-chatbuf--output-marker'
  (telega-button-insert 'telega-prompt-aux
    :value "\n" 'invisible t)

  (setq telega-chatbuf--auxmsg-button
        (telega-button-insert 'telega-prompt-aux
          :value "no-value" 'invisible t))
  (setq telega-chatbuf--prompt-button
        (telega-button-insert 'telega-prompt
          :value telega-chat-input-prompt))

  ;; Separator for `telega-chatbuf--input-marker'
  (telega-button-insert 'telega-prompt :value " " :format 'identity)

  (setq telega-chatbuf--input-marker (point-marker))
  ;; NOTE: `telega-chatbuf--output-marker' automagically moves when
  ;; buttons inserted at its position
  (setq telega-chatbuf--output-marker (copy-marker 1 t))

  (setq telega--chat-buffers
        (pushnew (current-buffer) telega--chat-buffers))

  (add-hook 'window-scroll-functions 'telega-chat-scroll nil t)
  (add-hook 'post-command-hook 'telega-chat-action-post-command nil t)
  (add-hook 'isearch-mode-hook 'telega-chat-buffer--input-isearch-setup nil t)
  (add-hook 'kill-buffer-hook 'telega-chat-buffer--killed nil t))

(defun telega-chat-buffer-info ()
  "Show info about chat."
  (interactive)
  (telega-chat-show-info telega-chatbuf--chat))

(defun telega-chat-buffer--killed ()
  "Called when chat buffer is killed."
  (ignore-errors
    ;; See https://github.com/zevlg/telega.el/issues/12
    (telega-server--send
     (list :@type "closeChat"
           :chat_id (plist-get telega-chatbuf--chat :id))))

  (setq telega--chat-buffers
        (delq (current-buffer) telega--chat-buffers)))

(defun telega-chat-buffer--input-isearch-setup ()
  "Setup chat buffer to use isearch to search in chat input history."
  (when telega-chatbuf--isearch-input
    ;; TODO: setup isearch, see. `comint-history-isearch-setup'
    )
  )

(defun telega-chat-scroll (window display-start)
  "If at the beginning then request for history messages.
Also mark messages as read with `viewMessages'."
  (with-current-buffer (window-buffer window)
    ;; If at the beginning of chatbuffer then request for the history
    (when (= display-start 1)
      (telega-chat--load-history telega-chatbuf--chat))

    ;; Mark some messages as read
    (unless (zerop (plist-get telega-chatbuf--chat :unread_count))
      (goto-char display-start)
      (telega-chat--view-messages
       telega-chatbuf--chat
       (mapcar (lambda (button) (button-get button :value))
               (telega-chat-buffer--visible-buttons window))))
    ))

(defun telega-chat-action-post-command ()
  "Update chat's action after command execution."
  (let ((input (buffer-substring telega-chatbuf--input-marker (point-max))))
    (cond ((and (not telega-chatbuf--chat-action)
                (not (string-empty-p input)))
           (setq telega-chatbuf--chat-action 'typing)
           (telega-chat--send-action telega-chatbuf--chat 'typing))

          ((and telega-chatbuf--chat-action
                (string-empty-p input))
           (setq telega-chatbuf--chat-action nil)
           (telega-chat--send-action telega-chatbuf--chat 'cancel)))))

(defmacro with-telega-chat-buffer (chat &rest body)
  "Execute BODY setting current buffer to chat buffer of CHAT.
Executes BODY only if chat buffer already exists.
If there is no corresponding buffer, then do nothing.
Inhibits read-only flag."
  (let ((bufsym (cl-gensym)))
    `(let ((,bufsym (cl-find ,chat telega--chat-buffers
                             :test (lambda (val buf)
                                     (with-current-buffer buf
                                       (eq telega-chatbuf--chat val))))))
       (when (buffer-live-p ,bufsym)
         (with-current-buffer ,bufsym
           (let ((inhibit-read-only t))
             ,@body))))))
(put 'with-telega-chat-buffer 'lisp-indent-function 'defun)

(defun telega-chat-buffer--name (chat &optional title)
  "Return name for the CHAT buffer.
If TITLE is specified, use it instead of chat's title."
  (format "Telega-%S%s: %s" (telega-chat--type chat)
          (telega--desurrogate-apply
           (let ((un (plist-get (telega-chat--info chat) :username)))
            (concat (if (string-empty-p un) "" "@") un)))
          (telega--desurrogate-apply
           (or title (telega-chat--title chat)))))

(defun telega-chat-buffer--get-create (chat)
  "Get or create chat buffer for the CHAT."
  (let ((bufname (telega-chat-buffer--name chat)))
    (or (get-buffer bufname)

        (with-current-buffer (generate-new-buffer bufname)
          (telega-chat-mode)

          (setq telega-chatbuf--chat chat)
          (telega-server--send
           (list :@type "openChat"
                 :chat_id (plist-get chat :id)))
          (let ((last-msg (plist-get chat :last_message)))
            (when last-msg
              (telega-chat-buffer--insert-youngest-msg last-msg t)))
          (telega-chat--load-history chat)

          (current-buffer)))))

(defun telega-chat-buffer--oldest-msg (chat)
  "Return oldest message in chat buffer associated with CHAT."
  (with-telega-chat-buffer chat
    (when (> telega-chatbuf--output-marker (point-min))
      (button-get (point-min) :value))))

(defun telega-chat-buffer--youngest-msg (chat)
  "Return youngest message in chat buffer associated with CHAT."
  (with-telega-chat-buffer chat
    (when (> telega-chatbuf--output-marker (point-min))
      (button-get (1- telega-chatbuf--output-marker) :value))))

(defun telega-chat--modeline-buffer-identification (chat)
  "Return `mode-line-buffer-identification' for the CHAT buffer."
  ;; TODO: Display number of mentions that are unread and not visible
  ;; at the moment
  )

(defun telega-chat-input-prev (n)
  "Goto N previous items in chat input history."
  (interactive "p")
  (let ((input (buffer-substring telega-chatbuf--input-marker (point-max))))
    (unless telega-chatbuf--input-pending-p
      (setq telega-chatbuf--input-pending-p t)
      (ring-insert telega-chatbuf--input-ring input))

    (if telega-chatbuf--input-idx
        (incf telega-chatbuf--input-idx n)
      (setq telega-chatbuf--input-idx n))
    (cond ((< telega-chatbuf--input-idx 0)
           (setq telega-chatbuf--input-idx 0))
          ((> telega-chatbuf--input-idx
              (ring-length telega-chatbuf--input-ring))
           (setq telega-chatbuf--input-idx
                 (ring-length telega-chatbuf--input-ring))))

    (delete-region telega-chatbuf--input-marker (point-max))
    (insert (ring-ref telega-chatbuf--input-ring telega-chatbuf--input-idx))))

(defun telega-chat-input-next (n)
  "Goto next N's item in chat input history."
  (interactive "p")
  (when telega-chatbuf--input-idx
    (telega-chat-input-prev (- n))))

(defun telega-chat-input-search (&optional regex)
  "Search for REGEX in chat input history."
  (interactive)
  ;; TODO:
  ;;  incrementaly search for history
  ;;   see comint-history-isearch-setup
  ;; (let ((elem (cl-find regex (cddr telega-chatbuf--input-ring)
  ;;                      :test #'string-match)))
  ;;   (
  )

(defun telega-chat-buffer--insert-oldest-msg (msg)
  "Insert message MSG as oldest message in chatbuffer."
  (with-telega-chat-buffer (telega-msg--chat msg)
    (save-excursion
      (run-hook-with-args 'telega-chat-before-oldest-msg-hook msg)

      (let ((oldest-msg (telega-chat-buffer--oldest-msg telega-chatbuf--chat)))
        ;; NOTE: if OLDEST-MSG is non-nil then corresponding button is
        ;; at `(point-min)'
        (when oldest-msg
          (let ((oldest-button (button-at (point-min))))
            (assert (eq (button-get oldest-button :value) oldest-msg))
            (button-put oldest-button :prev-msg msg)
            (button-put oldest-button
                        :format (telega-msg-button--format oldest-msg msg))
            (telega-button--redisplay oldest-button)))

        (goto-char (point-min))
        (telega-button-insert 'telega-msg
          :value msg
          :format (telega-msg-button--format msg))))))

(defun telega-chat-buffer--insert-youngest-msg (msg &optional disable-notification)
  "Insert newly arrived message MSG as youngest into chatbuffer.
If DISABLE-NOTIFICATION is non-nil, then do not trigger
notification for this message.
Return newly inserted message button."
  (unwind-protect
      (with-telega-chat-buffer (telega-msg--chat msg)
        (telega-save-excursion
          (run-hook-with-args 'telega-chat-before-youngest-msg-hook msg)

          (goto-char telega-chatbuf--output-marker)
          (let ((prev-msg (telega-chat-buffer--youngest-msg telega-chatbuf--chat)))
            (telega-button-insert 'telega-msg
              :value msg
              :prev-msg prev-msg
              :format (telega-msg-button--format msg prev-msg)))))

    (run-hook-with-args 'telega-chat-message-hook msg disable-notification)))

(defun telega-chat-buffer--button-get (msg-id)
  "In current chatbuffer find message button with MSG-ID."
  (goto-char telega-chatbuf--output-marker)
  (cl-block 'button-found
    (telega-button-foreach0 previous-button 'telega-msg (button)
      (when (= msg-id (plist-get (button-get button :value) :id))
        (cl-return-from 'button-found button)))))

(defun telega-chat-buffer--visible-buttons (window)
  "Return list of buttons visible in chatbuffer window."
  (let (buttons)
    (cl-block 'done
      (telega-button-foreach 'telega-msg (button)
        (if (pos-visible-in-window-p button window)
            (setq buttons (push button buttons))
          (cl-return-from 'done buttons))))
    buttons))

(defun telega-chat-buffer--prompt-reset ()
  "Reset prompt to initial state in chat buffer."
  (let ((inhibit-read-only t))
    (telega-save-excursion
      (setq telega-chatbuf--send-func 'telega-chat-send-msg
            telega-chatbuf--send-args nil)

      (unless (button-get telega-chatbuf--auxmsg-button 'invisible)
        (button-put telega-chatbuf--auxmsg-button :value "no-value")
        (button-put telega-chatbuf--auxmsg-button :format 'identity)
        (button-put telega-chatbuf--auxmsg-button 'invisible t))

      (button-put telega-chatbuf--prompt-button
                  :value telega-chat-input-prompt)
      (telega-button--redisplay telega-chatbuf--prompt-button))))

(defun telega-chat-buffer--read-outbox (old-last-read-outbox-msgid)
  "Redisplay chat messages affected by read-outbox change.
OLD-LAST-READ-OUTBOX-MSGID is old value for chat's `:last_read_outbox_message_id'."
  (telega-save-excursion
    (goto-char telega-chatbuf--output-marker)
    (cl-block 'buttons-traverse-done
      (telega-button-foreach0 previous-button 'telega-msg (button)
        (when (>= old-last-read-outbox-msgid
                  (plist-get (button-get button :value) :id))
          (cl-return-from 'buttons-traverse-done))
        (telega-button--redisplay button)))))

(defun telega--on-updateNewMessage (event)
  "A new message was received; can also be an outgoing message."
  (let* ((new-msg (plist-get event :message))
         (button (telega-chat-buffer--insert-youngest-msg
                  new-msg (plist-get event :disable_notification))))

    ;; If message is visibible in some window, then mark it as read
    ;; see https://github.com/zevlg/telega.el/issues/4
    (when (and (markerp button)
               (get-buffer-window (marker-buffer button))
               (pos-visible-in-window-p
                button (get-buffer-window (marker-buffer button))))
      (telega-chat--view-messages
       (telega-chat--get (plist-get new-msg :chat_id))
       (list new-msg)))))

(defun telega--on-updateMessageSendSucceeded (event)
  "Message has been successfully sent to server.
Message id could be updated on this update."
  (let* ((new-msg (plist-get event :message))
         (chat (telega-chat--get (plist-get new-msg :chat_id))))
    (with-telega-chat-buffer chat
      (save-excursion
        (let ((msg-button (telega-chat-buffer--button-get
                           (plist-get event :old_message_id))))
          (assert msg-button nil
                  (format "Can't find message id=%d"
                          (plist-get event :old_message_id)))

          (button-put msg-button :value new-msg)
          (telega-button--redisplay msg-button))))))

(defun telega--on-updateMessageContent (event)
  "Content of the message has been changed."
  (let ((chat (telega-chat--get (plist-get event :chat_id))))
    (with-telega-chat-buffer chat
      (save-excursion
        (let ((msg-button (telega-chat-buffer--button-get
                           (plist-get event :message_id))))
          (when msg-button
            (plist-put (button-get msg-button :value)
                       :content (plist-get event :new_content))
            (telega-button--redisplay msg-button)))))))

(defun telega--on-updateMessageEdited (event)
  "Edited date of the message specified by EVENT has been changed."
  (let ((chat (telega-chat--get (plist-get event :chat_id))))
    (with-telega-chat-buffer chat
      (save-excursion
        (let ((msg-button (telega-chat-buffer--button-get
                           (plist-get event :message_id))))
          (when msg-button
            (plist-put (button-get msg-button :value)
                       :edit_date (plist-get event :edit_date))
            (plist-put (button-get msg-button :value)
                       :reply_markup (plist-get event :reply_markup))
            (button-put msg-button
                        :format (telega-msg-button--format
                                 (button-get msg-button :value)
                                 (button-get msg-button :prev-msg)))
            (telega-button--redisplay msg-button)))))))

(defun telega--on-updateMessageViews (event)
  "Number of message views has been updated."
  (let ((chat (telega-chat--get (plist-get event :chat_id))))
    (with-telega-chat-buffer chat
      (save-excursion
        (let ((msg-button (telega-chat-buffer--button-get
                           (plist-get event :message_id))))
          (when msg-button
            (plist-put (button-get msg-button :value)
                       :views (plist-get event :views))
            (telega-button--redisplay msg-button)))))))

(defun telega--on-updateDeleteMessages (event)
  "Some messages has been deleted from chat."
  (let ((chat (telega-chat--get (plist-get event :chat_id))))
    (with-telega-chat-buffer chat
      (save-excursion
        (when (plist-get event :is_permanent)
          (mapc (lambda (msg-id)
                  (let ((button (telega-chat-buffer--button-get msg-id)))
                    (when button
                      (telega-button-delete button))))
                (plist-get event :message_ids)))))))

(defun telega-chat--load-history (chat)
  "Load and insert CHAT's history."
  (with-telega-chat-buffer chat
    (unless telega-chatbuf--history-loading
      (let ((oldest-msg (telega-chat-buffer--oldest-msg chat))
            (offset 0))
        (unless oldest-msg
          (setq oldest-msg (plist-get chat :last_message)
                offset -1))

        (when oldest-msg
          ;; Asynchronously load chat history
          (setq telega-chatbuf--history-loading t)

          (telega-server--call
           (list :@type "getChatHistory"
                 :chat_id (plist-get chat :id)
                 :from_message_id (plist-get oldest-msg :id)
                 :offset offset
                 :limit telega-chat-history-limit)

           `(lambda (history)
              (with-telega-chat-buffer (telega-chat--get ,(plist-get chat :id))
                (telega-save-excursion
                 (mapc #'telega-chat-buffer--insert-oldest-msg
                       (plist-get history :messages)))
                (setq telega-chatbuf--history-loading nil)))))))))

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
      ;; NOTE: Remember message we reply to, so getting with
      ;; `telega-msg--get' will go without online requests
      (push reply-to-msg telega-chatbuf--replies)
      (setq tl-msg (plist-put tl-msg :reply_to_message_id (plist-get reply-to-msg :id))))
    (when from-background
      (setq tl-msg (plist-put tl-msg :from_background t)))
    (telega-server--send tl-msg)))

(defun telega-chat-edit-msg (chat text &optional markdown edit-msg-id reply-markup)
  "Message MSG has been edited."
  (let ((tl-msg (list :@type "editMessageText"
                      :chat_id (plist-get chat :id)
                      :message_id edit-msg-id
                      :input_message_content
                      (telega-msg--input-content text markdown))))
    (telega-server--send tl-msg)))

(defun telega-chat-cancel-edit-reply ()
  "Cancel current reply or edit prompt."
  (interactive)
  (telega-chat-buffer--prompt-reset))

(defcustom telega-chat-use-markdown-formatting nil
  "*Non-nil to use markdown formatting for outgoing messages."
  :type 'boolean
  :group 'telega)

(defun telega-chat-send (prefix-arg)
  "Send current input to the chat.
With PREFIX-ARG, inverses `telega-chat-use-markdown-formatting' setting."
  (interactive "P")
  (let ((input (buffer-substring telega-chatbuf--input-marker (point-max)))
        (sfunc telega-chatbuf--send-func)
        (sargs telega-chatbuf--send-args))
    ;; Recover prompt to initial state
    (delete-region telega-chatbuf--input-marker (point-max))
    (telega-chat-buffer--prompt-reset)

    (when (string-empty-p input)
      (error "No input"))

    (when telega-chatbuf--input-pending-p
      (ring-remove telega-chatbuf--input-ring 0)
      (setq telega-chatbuf--input-pending-p nil))

    (ring-insert telega-chatbuf--input-ring input)
    (setq telega-chatbuf--input-idx nil
          telega-chatbuf--input-pending-p nil)

    (apply sfunc telega-chatbuf--chat input
           (if telega-chat-use-markdown-formatting
               (not prefix-arg)
             prefix-arg)
           sargs)))

;; Message commands
(defun telega-msg-reply (msg)
  "Start replying to MSG."
  (interactive (list (button-get (button-at (point)) :value)))

  (with-telega-chat-buffer (telega-msg--chat msg)
    (button-put telega-chatbuf--auxmsg-button
                :value msg)
    (button-put telega-chatbuf--auxmsg-button
                :format 'telega-msg-button--format-aux-reply)
    (button-put telega-chatbuf--auxmsg-button
                'invisible nil)
    (telega-button--redisplay telega-chatbuf--auxmsg-button)

    (button-put telega-chatbuf--prompt-button
                :value telega-chat-reply-prompt)
    (telega-button--redisplay telega-chatbuf--prompt-button)
    (goto-char (point-max))

    (setq telega-chatbuf--send-func 'telega-chat-send-msg
          telega-chatbuf--send-args (list msg))
    ))

(defun telega-msg-edit (msg)
  "Start editing the MSG."
  (interactive (list (button-get (button-at (point)) :value)))

  (with-telega-chat-buffer (telega-msg--chat msg)
    (button-put telega-chatbuf--auxmsg-button
                :value msg)
    (button-put telega-chatbuf--auxmsg-button
                :format 'telega-msg-button--format-aux-edit)
    (button-put telega-chatbuf--auxmsg-button
                'invisible nil)
    (telega-button--redisplay telega-chatbuf--auxmsg-button)

    (button-put telega-chatbuf--prompt-button
                :value telega-chat-edit-prompt)
    (telega-button--redisplay telega-chatbuf--prompt-button)

    ;; Replace any input text with edited message
    (delete-region telega-chatbuf--input-marker (point-max))
    (goto-char (point-max))
    (insert (substring-no-properties (telega-msg-text-with-props msg)))

    (setq telega-chatbuf--send-func 'telega-chat-edit-msg
          telega-chatbuf--send-args (list (plist-get msg :id)))
    ))

(defun telega-msg-delete (msg &optional revoke)
  "Delete message MSG.
With prefix arg delete only for yourself."
  (interactive (list (button-get (button-at (point)) :value) (not current-prefix-arg)))

  (when (y-or-n-p (concat (if revoke "Revoke" "Kill") " the message? "))
    (telega-msg--deleteMessages
     (plist-get msg :chat_id) (vector (plist-get msg :id)) revoke)))

(defun telega-chat-complete-username ()
  "Complete username at point."
  (interactive)
  (error "Username completion not yet implemented"))

(defun telega-chat-next-link (n)
  (error "`telega-chat-next-link' not yet implemented"))

(defun telega-chat-prev-link (n)
  (interactive)
  (telega-chat-next-link (- n)))

(defun telega-chat-complete-or-next-link ()
  "Complete username at point, or jump to next link."
  (interactive)
  (if (<= telega-chatbuf--input-marker (point))
      (call-interactively 'telega-chat-complete-username)
    (call-interactively 'telega-chat-next-link)))

(defun telega-chat-generate-invite-link (&optional chat-id)
  "Generate invite link for chat with CHAT-ID."
  (interactive)

  (telega-server--call
   (list :@type "generateChatInviteLink"
         :chat_id (or chat-id (plist-get telega-chatbuf--chat :id)))))

(defcustom telega-chat-image-preview-max-width 256
  "*Width for the chat buttons."
  :type 'integer
  :group 'telega)

(defun telega-chat-insert-media ()
  "Select file and send is as media."
  (interactive)
  (let* ((sel (read-file-name "Select file: " nil nil t))
         (fn (when sel (expand-file-name sel))))

    (when fn
      (insert (concat "\n"
                      (propertize (concat "photo:" fn)
                                  'with-file t
                                  'display (create-image fn 'imagemagick nil :max-width telega-chat-image-preview-max-width))
                      "\n")))))

(defun telega-chat-insert-file ()
  "Select file and send is as file."
  (interactive)
  (let* ((sel (read-file-name "Select file: " nil nil t))
         (fn (when sel (expand-file-name sel))))

    (when fn (insert (propertize (concat "file:" fn) 'with-file t 'file fn)))))

(defun telega-chat-insert-media-clipboard ()
  "Save image in clipboard to file and paste link to it."
  (interactive)
  (assert (fboundp 'x-get-selection))

  (let ((sel (x-get-selection 'CLIPBOARD 'image/png))
        tmp)

    (if (not sel)
        (message "Clipboard doesn't contain image")

      (setq tmp (make-temp-file "telega-photo" nil ".png"))
      (with-temp-file tmp (insert sel))
      (insert "\n")
      (insert (propertize (concat "photo:" tmp)
                          'with-file t
                          'display (create-image
                                    tmp 'imagemagick nil
                                    :max-width telega-chat-image-preview-max-width)))
      (insert "\n"))))

(provide 'telega-chat)

;;; telega-chat.el ends here

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

(defun telega-chat--title (chat &optional with-username)
  "Return title for the CHAT.
If WITH-USERNAME is specified, append trailing username for this chat."
  (let ((title (plist-get chat :title)))
    (when (string-empty-p title)
      (setq title (ecase (telega-chat--type chat)
                    (private
                     (telega-user--title (telega-chat--user chat))))))
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
  (let ((chat (telega-chat--get (plist-get event :chat_id)))
        (title (plist-get event :title)))
    (with-telega-chat-buffer chat
      (rename-buffer (telega-chat-buffer--name chat title)))

    (plist-put chat :title title)
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
  (when (and (eq (telega-chat--type chat 'raw) 'supergroup)
             (not (zerop (plist-get
                          (telega--full-info (telega-chat--supergroup chat))
                          :pinned_message_id))))
    (telega-server--call
     `(:@type "getChatPinnedMessage" :chat_id ,(plist-get chat :id)))))

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

(defun telega-chat-info (chat)
  "Show info about CHAT at point."
  (interactive
   (list (let ((button (button-at (point))))
           (and button (button-get button :value)))))
  (unless chat
    (error "No chat at point"))

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
    (when telega-debug
      (insert (format "Order: %s\n" (telega-chat--order chat))))

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

(defun telega-chat--send-action (chat action)
  "Send ACTION on CHAT."
  (telega-server--send
   `(:@type "sendChatAction" :chat_id (plist-get chat :id)
            :action (:@type (ecase action
                              ((typing Typing) "chatActionTyping")
                              ((cancel Cancel) "chatActionCancel"))))))


;;; Chat buttons in root buffer
(defcustom telega-chat-button-width 28
  "*Width for the chat buttons."
  :type 'integer
  :group 'telega)

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
  (telega-chat--pop-to-buffer (button-get button :value)))

(defun telega-chat--pop-to-buffer (chat)
  "Pop to CHAT's buffer."
  (pop-to-buffer (telega-chat-buffer--get-create chat)))

(defun telega-chat--create-private (user)
  "Create private chat with USER."
  (telega-server--call
   `(:@type "createPrivateChat" :user_id (plist-get user :id))))

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
                                   (string= (telega-user--title user 'with-username)
                                            needname)))))
        (setq chat (telega-chat--create-private user))))

    (telega-chat--pop-to-buffer chat)))


;;; Chat Buffer, resembles lui.el
(defgroup telega-chat nil
  "Customization for telega-chat-mode"
  :prefix "telega-chat-"
  :group 'telega)

(defvar telega-chat-mode-hook nil
  "Hook run when telega chat buffer is created.")

(defvar telega-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "\e\e") 'telega-chat-cancel-edit-reply)

    (define-key map (kbd "RET") 'telega-chat-send)
    (define-key map (kbd "M-m") 'telega-chat-send-media)
    (define-key map (kbd "M-f") 'telega-chat-send-file)
    (define-key map (kbd "M-p") 'telega-chat-input-prev)
    (define-key map (kbd "M-n") 'telega-chat-input-next)
    (define-key map (kbd "M-r") 'telega-chat-input-search)
    map))

(defvar telega-chatbuf--chat nil
  "Telega chat for the current chat buffer.")
(make-variable-buffer-local 'telega-chatbuf--chat)

(defvar telega-chatbuf--chat-action nil
  "Current action on the chat.")
(make-variable-buffer-local 'telega-chatbuf--chat-action)

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

(defvar telaga-chatbuf--auxmsg-button nil
  "Button that display reply/edit message above input.")
(make-variable-buffer-local 'telaga-chatbuf--auxmsg-button)

(defvar telega-chatbuf--prompt-button nil "Input prompt button.")
(make-variable-buffer-local 'telega-chatbuf--prompt-button)

(define-button-type 'telega-prompt
  :supertype 'telega
  :format '((lambda (prompt)
              (propertize prompt 'face 'telega-chat-prompt))
            " ")
  'rear-nonsticky t
  'front-sticky t
  'read-only t
  'cursor-intangible t
  'field 'telega-prompt)

(define-button-type 'telega-prompt-aux
  :supertype 'telega
  :format 'identity
  'read-only t
  'cursor-intangible t)

(define-derived-mode telega-chat-mode nil "Telega-Chat"
  "The mode for telega chat buffer.
Keymap:
\\{telega-chat-mode-map}"
  :group 'telega-chat
  (setq telega-chatbuf--input-ring (make-ring telega-chat-input-ring-size)
        telega-chatbuf--input-idx nil
        telega-chatbuf--input-pending-p nil
        telega-chatbuf--chat-action nil)

  (cursor-intangible-mode 1)
  (goto-char (point-max))
  (setq telega-chatbuf--output-marker (point-marker))
  (setq telaga-chatbuf--auxmsg-button
        (telega-button-insert 'telega-prompt-aux
          :value "no-value" 'invisible t))
  (setq telega-chatbuf--prompt-button
        (telega-button-insert 'telega-prompt
          :value telega-chat-input-prompt))
  (setq telega-chatbuf--input-marker (point-marker))

  (setq telega--chat-buffers
        (pushnew (current-buffer) telega--chat-buffers))

  (add-hook 'window-scroll-functions 'telega-chat-scroll nil t)
  (add-hook 'post-command-hook 'telega-chat-action-post-command nil t)
  (add-hook 'isearch-mode-hook 'telega-chat-buffer--input-isearch-setup nil t)
  (add-hook 'kill-buffer-hook 'telega-chat-buffer--killed nil t))

(defun telega-chat-buffer--killed ()
  "Called when chat buffer is killed."
  (telega-server--send
   `(:@type "closeChat" :chat_id ,(plist-get telega-chatbuf--chat :id)))

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
Also marks messages as read."
  (when telega-debug
    (message "Telega chatbuf scroll: %S" display-start))

  (when (= display-start 1)
    (with-current-buffer (window-buffer window)
      (telega-chat--load-history telega-chatbuf--chat))))

(defun telega-chat-action-post-command ()
  "Update chat's action after command execution."
  (when telega-chatbuf--input-marker
    (let ((input (buffer-substring telega-chatbuf--input-marker (point-max))))
      (cond ((and (not telega-chatbuf--chat-action)
                  (not (string-empty-p input)))
             (setq telega-chatbuf--chat-action 'typing)
             (telega-chat--send-action telega-chatbuf--chat 'typing))

            ((and telega-chatbuf--chat-action
                  (string-empty-p input))
             (setq telega-chatbuf--chat-action nil)
             (telega-chat--send-action telega-chatbuf--chat 'cancel))))))

(defmacro with-telega-chat-buffer (chat &rest body)
  "Execute BODY setting current buffer to chat buffer of CHAT.
Executes BODY only if chat buffer already exists.
If there is no corresponding buffer, then do nothing.
Inhibits read-only flag."
  (let ((bufsym (cl-gensym)))
    `(let ((,bufsym (get-buffer (telega-chat-buffer--name ,chat))))
       (when (buffer-live-p ,bufsym)
         (with-current-buffer ,bufsym
           (save-excursion
             (let ((inhibit-read-only t)
                   (inhibit-point-motion-hooks t))
               ,@body)))))))
(put 'with-telega-chat-buffer 'lisp-indent-function 'defun)

(defun telega-chat-buffer--set-prompt (prompt)
  "Set new PROMPT to the current chat buffer."
  (button-put telega-chatbuf--prompt-button :value prompt)
  (telega-button--redisplay telega-chatbuf--prompt-button))

(defun telega-chat-buffer--name (chat &optional title)
  "Return name for the CHAT buffer.
If TITLE is specified, use it instead of chat's title."
  (format "Telega-%S: %s" (telega-chat--type chat)
          (or title (telega-chat--title chat))))

(defun telega-chat-buffer--get-create (chat)
  "Get or create chat buffer for the CHAT."
  (let ((bufname (telega-chat-buffer--name chat)))
    (or (get-buffer bufname)

        (with-current-buffer (generate-new-buffer bufname)
          (telega-chat-mode)

          (setq telega-chatbuf--chat chat)
          (telega-server--send
           `(:@type "openChat" :chat_id ,(plist-get chat :id)))
          ;; Insert last message and some history
          (let ((last-msg (plist-get chat :last_message)))
            (when last-msg
              (telega-chat--new-message last-msg t)))
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

(defun telega-chat--new-message (msg &optional disable-notification)
  "New message MSG is arrived."
  (with-telega-chat-buffer (telega-msg--chat msg)
    (goto-char telega-chatbuf--output-marker)
    (let ((last-msg (telega-chat-buffer--youngest-msg telega-chatbuf--chat)))
      (telega-button-insert 'telega-msg
        :value msg
        :format (if (and last-msg (telega-msg-sender-same-p msg last-msg))
                    'telega-msg-button--format-same
                  'telega-msg-button--format-full)))
    (setq telega-chatbuf--output-marker (point-marker))))

(defun telega--on-updateNewMessage (event)
  "A new message was received; can also be an outgoing message."
  (telega-chat--new-message
   (plist-get event :message)
   (telega--tl-bool event :disable_notification)))

(defun telega-chat--getHistory (chat &optional from-msg limit)
  "Get CHAT history stating FROM-MSG.
If FROM-MSG is nil then use last message in chat."
  (unless from-msg
    (setq from-msg (plist-get chat :last_message)))
  (telega-server--call
   `(:@type "getChatHistory"
            :chat_id ,(plist-get chat :id)
            :from_message_id ,(or (and from-msg (plist-get from-msg :id)) 0)
            :offset 0
            :limit ,(or limit telega-chat-initial-history-messages))))

(defun telega-chat--load-history (chat)
  "Load and insert CHAT's history."
  ;; Get oldest message in chat and request history
  (with-telega-chat-buffer chat
    (let* ((msg-button (button-at (point-min)))
           (oldest-msg (and (eq (button-type msg-button) 'telega-msg)
                            (button-get msg-button :value)))
           (history (telega-chat--getHistory chat oldest-msg)))

      ;; NOTE: last-message already inserted before loading history,
      ;; so no need to update `telega-chatbuf--output-marker'
      (assert oldest-msg nil
              "Last message must be displayed before loading history")

      (mapc (lambda (msg)
              (when (and oldest-msg
                         (telega-msg-sender-same-p msg oldest-msg t))
                (button-put msg-button :format 'telega-msg-button--format-same)
                (telega-button--redisplay msg-button))

              (goto-char (point-min))
              (setq oldest-msg msg
                    msg-button (telega-button-insert 'telega-msg
                                 :value msg
                                 :format 'telega-msg-button--format-full)))
            (plist-get history :messages))
      )))

(defun telega-chat-send-msg (chat text &optional markdown reply-to-msg-id
                                  notify from-background reply-markup)
  "Send message to the CHAT.
REPLY-TO-MSG-ID - Id of the message to reply to.
Pass non-nil NOTIFY to generate notification for this message.
Pass non-nil FROM-BACKGROUND if message sent from background."
  (let ((tl-msg
         `(:@type "sendMessage"
                  :chat_id ,(plist-get chat :id)
                  :disable_notification ,(or (not notify) :json-false)
                  :input_message_content
                  ,(telega-msg--input-content text markdown))))
    (telega-debug "Message to '%s': %S" (telega-chat--title chat) tl-msg)
    (when reply-to-msg-id
      (setq tl-msg (plist-put tl-msg :reply_to_message_id reply-to-msg-id)))
    (when from-background
      (setq tl-msg (plist-put tl-msg :from_background t)))

    (telega-server--call tl-msg)))

(defun telega-chat-send (markdown)
  "Send current input to the chat.
With prefix arg, apply markdown formatter to message."
  (interactive "P")
  (let ((input (buffer-substring telega-chatbuf--input-marker (point-max))))
    (delete-region telega-chatbuf--input-marker (point-max))

    (when (string-empty-p input)
      (error "No input"))

    (when telega-chatbuf--input-pending-p
      (ring-remove telega-chatbuf--input-ring 0)
      (setq telega-chatbuf--input-pending-p nil))

    (ring-insert telega-chatbuf--input-ring input)
    (setq telega-chatbuf--input-idx nil
          telega-chatbuf--input-pending-p nil)

    (telega-chat-send-msg telega-chatbuf--chat input markdown)))

(defun telega-chat-msg-reply (msg)
  "Start replying to MSG."
  (button-put telaga-chatbuf--auxmsg-button
              :value msg)
  (button-put telaga-chatbuf--auxmsg-button
              :format `(("|" :face 'bold)
                        " Reply: "
                        (telega-msg-button--format-one-line
                         :min 30 :max 30 :elide t :align left)))
  (button-put telaga-chatbuf--auxmsg-button
              'invisible nil)
  (telega-button--redisplay telaga-chatbuf--auxmsg-button)

  (button-put telega-chatbuf--prompt-button
              telega-chat-reply-prompt)
  (telega-button--redisplay telega-chatbuf--prompt-button)
  )

(provide 'telega-chat)

;;; telega-chat.el ends here

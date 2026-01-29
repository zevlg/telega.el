;;; telega-transient.el --- Transient menus for telega  -*- lexical-binding: t -*-

;; Copyright (C) 2025 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Dec  2 20:21:44 2025
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
(require 'transient)

(require 'telega-core)
(require 'telega-ins)

(defvar telega-prefix-map)

(declare-function telega-msg-forward-dwim "telega-chat" (messages &optional remove-sender-p remove-caption-p chat))
(declare-function telega-msg-forward-dwim-to-many "telega-chat" (messages chats &optional remove-sender-p remove-caption-p))

(defvar telega-transient--variable-values nil
  "List of values used for transient.")

(defun telega-transient--variable-get (var)
  (if (keywordp var)
      (plist-get telega-transient--variable-values var)
    (cl-assert (symbolp var))
    (symbol-value var)))

(defun telega-transient--variable-set (var value)
  (if (keywordp var)
      (setq telega-transient--variable-values
            (plist-put telega-transient--variable-values var value))
    (cl-assert (symbolp var))
    (set var value)))

(defclass telega-transient--variable (transient-variable)
  ((global-p :initarg :global-p     :initform t)
   ))

(cl-defmethod transient-infix-set ((obj telega-transient--variable) value)
  (let ((obj-val (oset obj value value)))
    (when (oref obj global-p)
      (telega-transient--variable-set (oref obj variable) obj-val))))

(cl-defmethod transient-infix-value ((obj telega-transient--variable))
  (cons (oref obj variable) (oref obj value)))

(cl-defmethod transient-format-value ((obj telega-transient--variable))
  (let ((val (oref obj value)))
    (propertize (format "%s" val)
                'face (if val 'transient-value 'transient-inactive-value))))

(cl-defmethod transient-init-value ((obj telega-transient--variable))
  (when (oref obj global-p)
    (oset obj value
          (telega-transient--variable-get (oref obj variable)))))

(defclass telega-transient--chat (transient-variable)
  ((reader :initform #'telega-completing-read-chat)
   (format :initform " %k %d\n  chats chats")
   ))

(cl-defmethod transient-format-value ((obj telega-transient--chat))
  (when-let ((chat (oref obj value)))
    (telega-ins--as-string
     (telega-ins--msg-sender chat
       :with-avatar-p t
       :with-brackets-p t))))

(defun telega-transient--find-suffix (variable)
  (seq-find (lambda (suffix-obj)
              (and (cl-typep suffix-obj 'telega-transient--variable)
                   (equal (oref suffix-obj variable) variable)))
            transient--suffixes))

(defclass telega-transient--variable:bool (telega-transient--variable)
  ((format                            :initform " %k %v %d")
   (deps-list :initarg :deps-list     :initform nil)
   (on-symbol :initarg :on-symbol)
   (off-symbol :initarg :off-symbol))
  "Class used for parameter that can be turned on and off.")

(cl-defmethod transient-format-value ((obj telega-transient--variable:bool))
  (let ((on-p (oref obj value)))
    (propertize (telega-symbol
                 (if on-p (oref obj on-symbol) (oref obj off-symbol)))
                'face (if on-p 'transient-value 'transient-inactive-value))))

(cl-defmethod transient-infix-set :after ((obj telega-transient--variable:bool) value)
  ;; Set values for dependent variables
  (seq-doseq (dep (oref obj deps-list))
    (when (eq value (nth 0 dep))
      (seq-doseq (dep-var (nthcdr 2 dep))
        (when-let ((tr-suffix (telega-transient--find-suffix dep-var)))
          (transient-infix-set tr-suffix (nth 1 dep)))))))

(cl-defmethod transient-infix-read ((obj telega-transient--variable:bool))
  (not (oref obj value)))

(defclass telega-transient--checkbox-switch (telega-transient--variable:bool)
  ((on-symbol                         :initform 'checkbox-on)
   (off-symbol                        :initform 'checkbox-off))
  "Class used for sets of checkbox buttons.")

(defclass telega-transient--radiobox-switch (telega-transient--variable:bool)
  ((on-symbol                         :initform 'radiobox-on)
   (off-symbol                        :initform 'radiobox-off))
  "Class used for sets of mutually exclusive radio buttons.")


(transient-define-infix telega-transient--infix-fwd-remove-sender ()
  "Hide/show sender name when forwarding."
  :description (lambda () (telega-i18n "lng_forward_action_hide_sender"))
  :class 'telega-transient--checkbox-switch
  :variable :fwd-remove-sender
  :global-p nil
  :deps-list '((nil nil :fwd-remove-caption))
  )

(transient-define-infix telega-transient--infix-fwd-remove-caption ()
  "Hide/show caption when forwarding."
  :description (lambda () (telega-i18n "lng_forward_action_hide_caption"))
  :class 'telega-transient--checkbox-switch
  :if (lambda ()
        (seq-some (lambda (msg)
                    (telega--tl-get msg :content :caption))
                  (oref (transient-prefix-object) scope)))
  :variable :fwd-remove-caption
  :global-p nil
  :deps-list '((t t :fwd-remove-sender)))

(transient-define-infix telega-transient--infix-fwd-add-chat ()
  "Add chat to forward message to."
  :description (lambda () (telega-i18n "lng_forward_action_change_recipient"))
  :class 'telega-transient--chat
  :prompt (lambda () "Forward to: ")
  :multi-value t
  )

(transient-define-infix telega-transient--infix-fwd-about ()
  "Help for forwarding options."
  :format "  %d"
  :description (lambda ()
                 (telega-ins--as-string
                  (telega-ins--with-attrs (list :fill 'left
                                                :fill-column fill-column
                                                :fill-prefix "  ")
                    (telega-ins--with-face 'telega-shadow
                      (telega-ins (telega-i18n "lng_forward_about"))))))
  :class 'transient-information*)

(defclass telega-transient--chats (transient-argument) ()
   ((format      :initform " %k %d%v")
    (multi-value :initform rest)
    (prompt      :initform (lambda (_obj)
                             (concat (telega-i18n "lng_forward_choose") ": ")))
    (reader      :initform (lambda (prompt chats _history)
                             (telega-completing-read-chat prompt chats))))
   "Class for telega chats.")

(cl-defmethod transient-format-value ((obj telega-transient--chats))
  (telega-ins--as-string
   (seq-doseq (chat (oref obj value))
     (telega-ins "\n  ")
     (telega-ins--chat chat))))

(transient-define-argument telega-transient--infix-fwd-chats ()
  "List of chats to forward messges to."
  :description (lambda () (telega-i18n "lng_recent_chats"))
  :class 'telega-transient--chats
  :format " %k %d%v"
  :argument ""
  :prompt (lambda (_obj)
            (concat (telega-i18n "lng_forward_choose") ": "))
  :reader (lambda (prompt chats _history)
            (telega-completing-read-chat prompt chats))
  :init-value (lambda (obj)
                (oset obj value
                      (list (telega-completing-read-chat
                             (concat (telega-i18n "lng_forward_choose") ": ")))))
  :multi-value t)

(transient-define-suffix telega-transient-forward-to-chat (messages args)
  "Forward messages to a single chat."
  :description (lambda () (telega-i18n "lng_selected_forward"))
  (interactive (list (telega-transient-scope)
                     (transient-args
                      (or transient-current-command
                          'telega-transient-msg-forward))))
  (telega-msg-forward-dwim
   messages
   (alist-get :fwd-remove-sender args)
   (alist-get :fwd-remove-caption args)))

(transient-define-suffix telega-transient-forward-to-many-chats (messages args)
  "Forward messages to many chats."
  :description (lambda () "Forward to many")
  (interactive (list (telega-transient-scope)
                     (transient-args
                      (or transient-current-command
                          'telega-transient-msg-forward))))
  (telega-msg-forward-dwim-to-many
   messages
   (telega-completing-read-chat-list "Forward to Chats")
   (alist-get :fwd-remove-sender args)
   (alist-get :fwd-remove-caption args)))

(transient-define-suffix telega-transient-forward-to-last (messages args)
  "Forward to the last chat you've forwarded previously."
  :description
  (lambda ()
    (concat (telega-i18n "lng_selected_forward")
            " " (telega-symbol 'right-arrow) " "
            (telega-msg-sender-title telega-msg-forward--last-chat
              :with-avatar-p t
              :with-brackets-p t)))
  :if (lambda ()
        (and telega-msg-forward--last-chat
             (not (eq telega-msg-forward--last-chat
                      (telega-chat-me)))))

  (interactive (list (telega-transient-scope)
                     (transient-args
                      (or transient-current-command
                          'telega-transient-msg-forward))))

  (telega-msg-forward-dwim
   messages
   (alist-get :fwd-remove-sender args)
   (alist-get :fwd-remove-caption args)
   telega-msg-forward--last-chat))

(transient-define-suffix telega-transient-forward-to-saved-messages
  (messages args)
  "Forward messages to the \"Saved Messages\" chat."
  :description (lambda ()
                 (concat (telega-i18n "lng_selected_forward")
                         " " (telega-symbol 'right-arrow) " "
                         (telega-chat-title (telega-chat-me))))
  (interactive (list (telega-transient-scope)
                     (transient-args
                      (or transient-current-command
                          'telega-transient-msg-forward))))
  (let ((from-chat (telega-msg-chat (car messages))))
    (when telega-chatbuf--marked-messages
      (telega-chatbuf-msg-marks-toggle))

    (telega--forwardMessages (telega-chat-me) from-chat messages
      nil
      (alist-get :fwd-remove-sender args)
      (alist-get :fwd-remove-caption args))

    (message "telega: %s"
             (telega-i18n (if (> (length messages) 1)
                              "lng_share_messages_to_chat"
                            "lng_share_message_to_chat")
               :chat (telega-chat-title (telega-chat-me))))))

(transient-define-prefix telega-transient-msg-forward (messages)
  "Message forwarding."
  [:description
   (lambda ()
     (let ((msg-count (length (oref (transient-prefix-object) scope))))
       (if (> msg-count 1)
           (telega-i18n "lng_forward_many_title"
             :count msg-count)
         (telega-i18n "lng_forward_title"))))

   (telega-transient--infix-fwd-about)
   ("h" telega-transient--infix-fwd-remove-sender)
   ("c" telega-transient--infix-fwd-remove-caption)
   ]
  [
   ("f" telega-transient-forward-to-chat)
   ("F" telega-transient-forward-to-many-chats)
   ("L" telega-transient-forward-to-last)
   ("S" telega-transient-forward-to-saved-messages)
   ]

  (interactive
    ;; NOTE: Forward messages in the id order, see
    ;; https://github.com/zevlg/telega.el/issues/271
    (list (cl-sort (or telega-chatbuf--marked-messages
                       (when-let ((msg-at-point (telega-msg-at (point))))
                         (list msg-at-point)))
                   #'< :key (telega--tl-prop :id))))

   (when (telega-chat-secret-p telega-chatbuf--chat)
     (user-error "telega: Can't forward messages from secret chat"))

    ;; NOTE: Check every message can be forwarded
   (seq-doseq (msg messages)
     (unless (telega-msg-match-p msg '(message-property :can_be_forwarded))
       (telega-msg-goto msg 'highlight)
       (user-error "telega: Message MSG-ID=%S can't be forwarded"
                   (plist-get msg :id))))
   (transient-setup 'telega-transient-msg-forward nil nil :scope messages))


;;; Delete message
(defun telega-transient--msg-senders ()
  "Return list of unique message senders."
  (seq-uniq (mapcar #'telega-msg-sender (telega-transient-scope)) #'eq))

(transient-define-infix telega-transient--infix-msg-report-spam ()
  :description (lambda ()
                 (telega-i18n "lng_report_spam"))
  :if (lambda ()
        (seq-some (lambda (msg)
                    (telega-msg-match-p msg
                      '(message-property :can_report_supergroup_spam)))
                  (telega-transient-scope)))
  :class 'telega-transient--checkbox-switch
  :variable :msg-report-spam-p)

(transient-define-infix telega-transient--infix-msg-sender-delete-all ()
  :description (lambda ()
                 (let ((senders (telega-transient--msg-senders)))
                   (if (= (length senders) 1)
                       (telega-i18n "lng_delete_all_from_user"
                         :user (telega-msg-sender-title (car senders)
                                 :with-avatar-p t
                                 :with-brackets-p t
                                 :with-username-p 'telega-username))
                     (telega-i18n "lng_delete_all_from_users"))))
  :if (lambda ()
        (telega-chatbuf-match-p
         '(my-permission :can_delete_messages)))
  :class 'telega-transient--checkbox-switch
  :variable :msg-sender-delete-all-p)

(transient-define-infix telega-transient--infix-msg-sender-block ()
  :description (lambda ()
                 (let ((senders (telega-transient--msg-senders)))
                   (if (= (length senders) 1)
                       (telega-i18n "lng_ban_user")
                     (telega-i18n "lng_ban_users"))))
  :if (lambda ()
        (telega-chatbuf-match-p '(my-permission :can_restrict_members)))
  :class 'telega-transient--checkbox-switch
  :variable :msg-sender-block-p)

(transient-define-infix telega-transient--infix-msg-auto-delete-about ()
  "Help for message auto deletion duration."
  :format " %d"
  :description (lambda ()
                 (let ((auto-delete-in (plist-get (car (telega-transient-scope))
                                                  :auto_delete_in)))
                   (telega-ins--as-string
                    (telega-ins--with-attrs (list :fill 'left
                                                  :fill-column fill-column
                                                  :fill-prefix "  ")
                      (telega-ins--with-face 'telega-shadow
                        (telega-ins (telega-symbol 'flames))
                        (telega-ins (telega-i18n "lng_context_auto_delete_in"
                                      :duration (telega-duration-human-readable
                                                 auto-delete-in 1 'long))))))))
  :if (lambda ()
        (when-let* ((messages (telega-transient-scope))
                    (_ (= 1 (length messages)))
                    (auto-delete-in (plist-get (car messages) :auto_delete_in)))
          (not (telega-zerop auto-delete-in))))
  :class 'transient-information*)

(transient-define-infix telega-transient--infix-msg-revoke ()
  "Revoke message."
  :description (lambda ()
                 (if (telega-chatbuf-match-p '(type private secret))
                     (telega-i18n "lng_delete_for_other_check"
                       :user (telega-msg-sender-title telega-chatbuf--chat
                               :with-avatar-p t
                               :with-brackets-p t
                               :with-username-p 'telega-username))
                 (telega-i18n "lng_delete_for_everyone_check")))
  :if (lambda ()
        (seq-some (lambda (msg)
                    (telega-msg-match-p msg
                      '(and (message-property :can_be_deleted_only_for_self)
                            (message-property :can_be_deleted_for_all_users))))
                  (telega-transient-scope)))
  :class 'telega-transient--checkbox-switch
  :variable :msg-revoke-p)

(transient-define-suffix telega-transient--suffix-msg-delete (messages args)
  "Delete messages."
  :description (lambda ()
                 (propertize (telega-i18n "lng_context_delete_msg")
                             'face 'error))
  (interactive (list (telega-transient-scope)
                     (transient-args
                      (or transient-current-command
                          'telega-transient-msg-delete))))
  (telega--deleteMessages messages (alist-get :msg-revoke-p args))

  (when (alist-get :msg-report-spam-p args)
    (apply #'telega--reportSupergroupSpam
           (telega-chat--supergroup telega-chatbuf--chat)
           messages))

  (seq-doseq (sender (telega-transient--msg-senders))
    (when (alist-get :msg-sender-delete-all-p args)
      (telega--deleteChatMessagesBySender telega-chatbuf--chat sender))
    (when (alist-get :msg-sender-block-p args)
      (telega--setChatMemberStatus
       telega-chatbuf--chat sender
       (list :@type "chatMemberStatusBanned"
             :banned_until_date 0))))

  (when telega-chatbuf--marked-messages
    (setq telega-chatbuf--marked-messages nil)
    (telega-chatbuf--chat-update "marked-messages"))
  )

(transient-define-prefix telega-transient-msg-delete (messages)
  "Delete a MESSAGE."
  [:description (lambda ()
                  (let ((senders (telega-transient--msg-senders)))
                    (if (= (length senders) 1)
                        (telega-i18n "lng_restrict_user_full"
                          :emoji "")
                      (concat (telega-i18n "lng_restrict_users_full"
                                :emoji "")
                              (format "(%d)" (length senders))))))
   :if (lambda ()
         (and (telega-chatbuf-match-p '(my-permission :can_restrict_members))
              ;; Do not show if any of the message is outgoing
              (seq-every-p (lambda (msg)
                             (telega-msg-match-p msg '(not (is-outgoing t))))
                           (telega-transient-scope))))
   ("s" telega-transient--infix-msg-report-spam)
   ("a" telega-transient--infix-msg-sender-delete-all)
   ("b" telega-transient--infix-msg-sender-block)
   ]
  [:description
   (lambda ()
     (let ((msg-count (length (telega-transient-scope))))
       (if (> msg-count 1)
           (telega-i18n "lng_selected_delete_sure"
             :count msg-count)
         (telega-i18n "lng_selected_delete_sure_this"))))
   (telega-transient--infix-msg-auto-delete-about)
   ("r" telega-transient--infix-msg-revoke)
   " "
   ("D" telega-transient--suffix-msg-delete)
   ]

  (interactive
   (list (or telega-chatbuf--marked-messages
             (when-let ((msg (telega-msg-for-interactive)))
               (list msg)))))

  ;; NOTE: Check every message can be deleted
  (seq-doseq (msg messages)
    (unless (telega-msg-match-p msg
              '(or (message-property :can_be_deleted_only_for_self)
                   (message-property :can_be_deleted_for_all_users)))
      (telega-msg-goto msg 'highlight)
      (user-error "telega: Message MSG-ID=%S can't be deleted"
                  (plist-get msg :id))))
  (transient-setup 'telega-transient-msg-delete nil nil :scope messages)
  )


;;; Save message content
(transient-define-suffix telega-transient--suffix-save-to-animations (msg)
  "Save GIF to saved animations."
  :description (lambda ()
                 (telega-i18n "lng_context_save_gif"))
  :if (lambda ()
        (telega-msg-match-p (telega-transient-scope)
          '(type Animation)))

  (interactive (list (telega-transient-scope)))
  (let ((file (telega-msg--content-file msg)))
    (telega--addSavedAnimation
     (list :@type "inputFileId" :id (plist-get file :id)))
    (message "telega: saved new animation")))

(transient-define-suffix telega-transient--suffix-save-audio-to-profile (msg)
  :description (lambda () (telega-i18n "lng_context_save_music_profile"))
  :if (lambda ()
        (telega-msg-match-p (telega-transient-scope)
          '(type Audio)))
  (interactive (list (telega-transient-scope)))
  ;; TODO
  )

(transient-define-suffix telega-transient--suffix-save-to-downloads (msg)
  "Add message's file to Downloads list and start downloading it."
  :description (lambda () (telega-i18n "lng_context_save_music_folder"))

  (interactive (list (telega-transient-scope)))
  (let ((msg-file (telega-msg--content-file msg)))
    (telega--addFileToDownloads msg-file msg
      :priority 10)
    (message "telega: Saving file to Telegram Downloads")))

(transient-define-suffix telega-transient--suffix-save-to-file (msg)
  "Save message's file into local file."
  :description (lambda () (telega-i18n "lng_save_file"))
  (interactive (list (telega-transient-scope)))
  ;; TODO
  )

(transient-define-prefix telega-transient-msg-save (msg)
  "Save message's content."
  [
  [:description (lambda () (telega-i18n "lng_settings_save"))
   ("f" telega-transient--suffix-save-to-file)
   ("a" telega-transient--suffix-save-to-animations)
   ]
  [:description (lambda () (telega-i18n "lng_context_save_music_to"))
   ("P" telega-transient--suffix-save-audio-to-profile)
   ("D" telega-msg-save-to-downloads
    :description (lambda () (telega-i18n "lng_context_save_music_folder")))
   ("S" telega-msg-save-to-saved-messages
    :description (lambda ()
                   (telega-chat-title (telega-chat-me))))
   ]
  ]

  (interactive (list (telega-msg-for-interactive)))
  (transient-setup 'telega-transient-msg-save nil nil :scope msg)
  )


;;; Operate on message
(defun telega-transient-scope ()
  (oref (transient-prefix-object) scope))

(transient-define-prefix telega-transient-msg-operate (msg)
  "Operate on a message MSG."
  [:description (lambda () "Message operations")
   [
    ("i" telega-describe-message
     :description (lambda ()
                    (telega-i18n "lng_info_about_label")))
    ("r" telega-msg-reply
     :description (lambda ()
                    (telega-i18n "lng_context_reply_msg"))
     :if (lambda ()
           (telega-msg-match-p (telega-transient-scope)
             '(message-property :can_be_replied))))
    ("R" telega-msg-reply-in-another-chat
     :description (lambda () (telega-i18n "lng_reply_in_another_chat"))
     :if (lambda ()
           (telega-msg-match-p (telega-transient-scope)
             '(message-property :can_be_replied_in_another_chat))))
    ("f" telega-transient-msg-forward
     :description (lambda () (telega-i18n "lng_context_forward_msg"))
     :if (lambda ()
           (telega-msg-match-p (telega-transient-scope)
             '(message-property :can_be_forwarded))))
    ]

   [
    ("e" telega-msg-edit
     :description (lambda ()
                    (telega-i18n "lng_context_edit_msg"))
     :if (lambda ()
           (telega-msg-match-p (telega-transient-scope)
             '(and (not (type Poll))
                   (message-property :can_be_edited)))))
    ("p" telega-transient-link-preview-options
     :description (lambda ()
                    (telega-i18n "lng_link_options_header"))
     :if (lambda ()
           (telega-msg-match-p (telega-transient-scope)
             '(and link-preview
                   (message-property :can_be_edited)))))
    ("t" telega-transient-msg-translate
     :description (lambda ()
                    (telega-i18n "lng_context_translate"))
     :if (lambda () (telega-msg-content-text (telega-transient-scope))))
    ("t" telega-transient-msg-translate
     :description (lambda () (telega-i18n "lng_summarize_header_title"))
     :if (lambda ()
           (plist-get (telega-transient-scope) :summary_language_code)))
    ("T" telega-msg-open-thread-or-topic
     :description (lambda ()
                    (let ((msg (telega-transient-scope)))
                      (if (telega-msg-match-p msg 'is-thread)
                          (telega-i18n "lng_replies_view"
                            :count (telega-msg-replies-count msg))
                        (telega-i18n "lng_replies_view_topic"))))
     :if (lambda ()
           (telega-msg-match-p (telega-transient-scope)
             '(or is-thread is-forum-topic))))
    ("^" telega-msg-pin-toggle
     :description (lambda ()
                    (if (telega-msg-match-p (telega-transient-scope)
                          '(prop :is_pinned))
                        (telega-i18n "lng_context_unpin_msg")
                      (telega-i18n "lng_context_pin_msg")))
     :if (lambda ()
           (telega-msg-match-p (telega-transient-scope)
             '(message-property :can_be_pinned))))
    ("d" telega-transient-msg-delete
     :description (lambda ()
                    (propertize (telega-i18n "lng_context_delete_msg")
                                'face 'error))
     :if (lambda ()
           (telega-msg-match-p (telega-transient-scope)
             '(or (message-property :can_be_deleted_only_for_self)
                  (message-property :can_be_deleted_for_all_users)))))
    ]
   [
    ("c" telega-msg-copy-text
     :description (lambda () (telega-i18n "lng_context_copy_text"))
     :if (lambda ()
           (telega-msg-content-text (telega-transient-scope) 'with-voice-note)))
    ("l" telega-msg-copy-link
     :description (lambda () (telega-i18n "lng_context_copy_link")))
    ("s" telega-transient-msg-save
     :description (lambda () (telega-i18n "lng_settings_save"))
     :if (lambda ()
           (telega-msg-match-p (telega-transient-scope)
             '(message-property :can_be_saved))))
    ]
   ]

  (interactive (list (telega-msg-for-interactive)))
  (transient-setup 'telega-transient-msg-operate nil nil :scope msg))


;;; Operate on chat
(transient-define-prefix telega-transient-chat-operate (chat)
  "Operate on a CHAT."
  [:description (lambda () "Chat operations")
   [
    ("i" telega-describe-chat
     :description (lambda ()
                    (telega-i18n "lng_info_about_label")))
    ("a" telega-chat-add-member
     :description (lambda () (telega-i18n "lng_profile_add_participant")))
    ("r" telega-chat-toggle-read
     :description (lambda ()
                    (if (telega-chat-match-p (telega-transient-scope) 'unread)
                        (telega-i18n "lng_context_mark_read")
                      (telega-i18n "lng_context_mark_unread"))))
    ]
   [
    ("^" telega-chat-toggle-pin
     :description (lambda ()
                    (if (telega-chat-match-p (telega-transient-scope) 'is-pinned)
                        (telega-i18n "lng_context_unpin_from_top")
                      (telega-i18n "lng_context_pin_to_top"))))
    ("o" telega-chat-set-custom-order
     :description (lambda ()
                    "Set custom order"))
    ("d" telega-transient-chat-delete
     :description
     (lambda ()
       (let ((chat (telega-transient-scope)))
         (propertize (cond ((telega-chat-match-p chat '(type private secret))
                            (telega-i18n "lng_profile_delete_conversation"))
                           ((telega-chat-match-p chat '(type channel))
                            (telega-i18n "lng_profile_leave_channel"))
                           (t
                            (telega-i18n "lng_profile_clear_and_exit")))
                     'face 'error))))
    ]
   ]

  (interactive (list (telega-chat-for-interactive)))
  (transient-setup 'telega-transient-chat-operate nil nil :scope chat))


;;; Delete chat
(transient-define-infix telega-transient--infix-chat-delete-about ()
  "Help about deleting chat."
  :format "  %d"
  :description
  (lambda ()
    (let ((chat (telega-transient-scope)))
      (telega-ins--as-string
       (telega-ins--with-attrs (list :fill 'left
                                     :fill-column fill-column
                                     :fill-prefix "  ")
         (telega-ins--with-face 'telega-shadow
           (cond ((telega-chat-match-p chat '(type private))
                  (telega-ins-i18n "lng_sure_delete_history"
                    :contact (telega-msg-sender-title (telega-chat-user chat)
                               :with-avatar-p t
                               :with-title 'first-name)))
                 ((telega-chat-match-p chat '(type channel))
                  (telega-ins-i18n "lng_sure_leave_channel"))
                 ;; TODO: other chats
                 ))))))
  :class 'transient-information*)

(transient-define-infix telega-transient--infix-chat-user-block ()
  "Block corresponding user."
  :description
  (lambda ()
    (let ((chat (telega-transient-scope)))
      (cond ((telega-chat-match-p chat '(type bot))
             (telega-i18n "lng_profile_block_bot"))
            (t
             (telega-i18n "lng_blocked_list_confirm_title"
               :name (telega-msg-sender-title (telega-chat-user chat)
                       :with-avatar-p t
                       :with-title 'first-name))))))
  :if (lambda ()
        (telega-chat-user (telega-transient-scope)))
  :class 'telega-transient--checkbox-switch
  :variable :chat-user-block-p)

(transient-define-infix telega-transient--infix-chat-history-revoke ()
  "Switch to revoke chat history."
  :description
  (lambda ()
    (let ((chat (telega-transient-scope)))
      (cond ((telega-chat-match-p chat '(type private))
             (telega-i18n "lng_delete_for_other_check"
               :user (telega-msg-sender-title (telega-chat-user chat)
                       :with-avatar-p t
                       :with-title 'first-name)))
            ;; TODO: other chat types
            )))
  :if (lambda ()
        (telega-chat-match-p (telega-transient-scope)
          '(prop :can_be_deleted_for_all_users)))
  :class 'telega-transient--checkbox-switch
  :variable :chat-history-revoke-p)

(transient-define-infix telega-transient--infix-chat-remove-from-folders ()
  "Switch to remove channel from all folders."
  :description (lambda ()
                 (telega-i18n "lng_filters_checkbox_remove_channel"))
  :if (lambda ()
        (telega-chat-match-p (telega-transient-scope)
          '(type channel)))
  :class 'telega-transient--checkbox-switch
  :variable :chat-remove-from-folders-p)

(transient-define-suffix telega-transient--suffix-chat-delete (chat)
  "Delete CHAT."
  :description (lambda ()
                 (propertize (telega-i18n "lng_box_delete")
                             'face 'error))
  (interactive (list (telega-transient-scope)))
  (message "TODO: delete chat")
  )

(transient-define-suffix telega-transient--suffix-chat-archive (chat)
  "Move CHAT to Archive list."
  :description (lambda () (telega-i18n "lng_archived_add"))
  :if (lambda ()
        (telega-chat-match-p (telega-transient-scope)
          '(not archive)))

  (interactive (list (telega-transient-scope)))
  (telega-chat-toggle-archive chat))

(transient-define-prefix telega-transient-chat-delete (chat)
  "Delete a CHAT."
  [:description
   (lambda ()
     (telega-ins--as-string
      (telega-ins--with-face 'transient-heading
        (let ((chat (telega-transient-scope)))
          (cond ((telega-chat-match-p chat '(type private secret))
                 (telega-ins-i18n "lng_profile_delete_conversation"))
                ((telega-chat-match-p chat '(type channel))
                 (telega-ins-i18n "lng_profile_leave_channel")
                 (telega-ins " ")
                 (telega-ins--msg-sender chat
                   :with-avatar-p t
                   :with-brackets-p t
                   :with-username-p 'telega-username))
                (t
                 (telega-ins-i18n "lng_profile_clear_and_exit")))))))

   (telega-transient--infix-chat-delete-about)
   ;; ("m" telega-transient--infix-chat-delete-history-for-me)
   ("f" telega-transient--infix-chat-remove-from-folders)
   ("r" telega-transient--infix-chat-history-revoke)
   ("b" telega-transient--infix-chat-user-block)
   ]
  [
   ("A" telega-transient--suffix-chat-archive)
   ("D" telega-transient--suffix-chat-delete)
   ]

  (interactive (list (telega-chat-for-interactive)))
  ;; (unless (telega-chat-match-p chat
  ;;           '(or (prop :can_be_deleted_only_for_self)
  ;;                (prop :can_be_deleted_for_all_users)))
  ;;   (user-error "telega: chat can't be deleted"))
  (transient-setup 'telega-transient-chat-delete nil nil :scope chat)
  )


;;; Block user from action bar
(transient-define-infix telega-transient--infix-chat-block-about ()
  "Help for blocking message sender."
  :format "  %d"
  :description (lambda ()
                 (telega-ins--as-string
                  (telega-ins--with-attrs (list :fill 'left
                                                :fill-column fill-column
                                                :fill-prefix "  ")
                    (telega-ins--with-face 'telega-shadow
                      (telega-ins-i18n "lng_blocked_list_confirm_text"
                        :name (telega-msg-sender-title
                                  (telega-chat-user (telega-transient-scope))
                                :with-avatar-p t
                                :with-title 'first-name))))))
  :class 'transient-information*)

(transient-define-infix telega-transient--infix-chat-report-spam ()
  "Report chat as spam."
  :description (lambda ()
                 (telega-i18n "lng_report_spam"))
  :class 'telega-transient--checkbox-switch
  :variable :chat-report-spam-p)

(transient-define-infix telega-transient--infix-chat-delete ()
  "Delete chat."
  :description (lambda ()
                 (telega-i18n "lng_blocked_list_confirm_clear"))
  :if (lambda ()
        (telega-chat-match-p (telega-transient-scope)
          '(or (prop :can_be_deleted_only_for_self)
               (prop :can_be_deleted_for_all_users))))
  :class 'telega-transient--checkbox-switch
  :variable :chat-delete-p)

(transient-define-suffix telega-transient--suffix-chat-user-block (chat args)
  "Block user associated with private or secret CHAT."
  :description (lambda ()
                 (propertize (telega-i18n "lng_blocked_list_confirm_ok")
                             'face 'error))
  (interactive (list (telega-transient-scope)
                     (transient-args
                      (or transient-current-command
                          'telega-transient-chat-action-report-block))))
  (telega-msg-sender-block (telega-chat-user chat))

  (when (alist-get :chat-report-spam-p args)
    (telega--reportChat chat "Spam"))

  (when (alist-get :chat-delete-p args)
    (setq telega-deleted-chats
          (cl-pushnew chat telega-deleted-chats))
    (telega--deleteChatHistory chat 'remove-from-list)

    ;; Kill corresponding chat buffer
    (with-telega-chatbuf chat
      (kill-buffer (current-buffer))))
  )

(transient-define-prefix telega-transient-chat-action-report-block (chat)
  "Block user corresponding to a private or secret chat."
  [:description (lambda ()
                  (telega-i18n "lng_new_contact_block"))
   (telega-transient--infix-chat-block-about)
   ("r" telega-transient--infix-chat-report-spam)
   ("d" telega-transient--infix-chat-delete)
   ]
  [
   ("B" telega-transient--suffix-chat-user-block)
   ]

  (interactive (list (telega-chat-for-interactive)))
  (transient-setup 'telega-transient-chat-action-report-block nil nil :scope chat)
  )


;;; Create Group



;;; Tag Commands
(transient-define-suffix telega-transient--suffix-sm-tag-filter ()
  :key "/"
  :description (lambda () (telega-i18n "lng_context_filter_by_tag"))
  (interactive)
  (let* ((cmd-scope (oref (transient-prefix-object) scope))
         (tag (plist-get cmd-scope :tag)))
    (telega-chatbuf-filter-by-saved-messages-tag tag)))

(transient-define-suffix telega-transient--suffix-sm-tag-add-name ()
  :key "n"
  :description
  (lambda ()
    (let ((tag (plist-get (oref (transient-prefix-object) scope) :tag)))
      (if (telega-tl-str tag :label)
          (telega-i18n "lng_context_tag_edit_name")
        (telega-ins--as-string
         (telega-ins (telega-i18n "lng_context_tag_add_name") "\n")
         (telega-ins--help-message
          (telega-ins-i18n "lng_edit_tag_about"))))))
  (interactive)
  (let* ((cmd-scope (oref (transient-prefix-object) scope))
         (tag (plist-get cmd-scope :tag))
         (msg (plist-get cmd-scope :msg))
         (label (read-string
                 (telega-ins--as-string
                  (telega-ins-i18n "lng_edit_tag_name")
                  (telega-ins ": ")
                  (telega-ins--msg-reaction-type (plist-get tag :tag)))
                 (telega-tl-str tag :label))))
    (telega--setSavedMessagesTagLabel tag label
      (when msg
        (lambda (_ignored)
          (telega-msg-redisplay msg))))))

(transient-define-suffix telega-transient--suffix-sm-tag-remove ()
  :key "d"
  :description (lambda () (telega-i18n "lng_context_remove_tag"))
  (interactive)
  (let* ((cmd-scope (oref (transient-prefix-object) scope))
         (tag (plist-get cmd-scope :tag))
         (msg (plist-get cmd-scope :msg)))
    (telega--removeMessageReaction msg (plist-get tag :tag)
      (when msg
        (lambda-with-current-buffer (_ignored)
          ;; NOTE: Removing tag from the message might affect
          ;; message's visibility if message filter is applied at the
          ;; moment
          (let ((msg-node (telega-chatbuf--node-by-msg-id (plist-get msg :id))))
            (if (telega-chatbuf--filter-match-msg-p msg)
                (telega-chatbuf--redisplay-node msg-node)
              (ewoc-delete telega-chatbuf--ewoc msg-node))))))))

(transient-define-prefix telega-transient-sm-tag-commands ()
  [:description
   (lambda ()
     (telega-ins--as-string
      (telega-ins--saved-messages-tag
       (plist-get (oref (transient-prefix-object) scope) :tag))
      (telega-ins " ")
      (telega-ins--with-face 'transient-heading
        (telega-ins "Tag Commands"))))
   ("/" telega-transient--suffix-sm-tag-filter)
   ("n" telega-transient--suffix-sm-tag-add-name)
   ("d" telega-transient--suffix-sm-tag-remove)
   ])


;;; Link Preview Options
(defun telega-transient-link-preview-text-msg-p ()
  (telega-msg-match-p (telega-transient-scope) '(type Text)))

(defun telega-transient--link-preview-options-get (prop)
  (telega--tl-get (telega-transient-scope) :content :link_preview_options
                  prop))

(transient-define-infix telega-transient--infix-link-preview-up ()
  "Move link preview photo up."
  :description (lambda () (telega-i18n "lng_link_move_up"))
  :class 'telega-transient--radiobox-switch
  :global-p nil
  :variable :lp-move-up
  :deps-list '((t nil :lp-move-down))
  :init-value
  (lambda (obj)
    (oset obj value
          (telega-transient--link-preview-options-get :show_above_text)))
  )

(transient-define-infix telega-transient--infix-link-preview-down ()
  "Move link preview photo down."
  :description (lambda () (telega-i18n "lng_link_move_down"))
  :class 'telega-transient--radiobox-switch
  :global-p nil
  :variable :lp-move-down
  :deps-list '((t nil :lp-move-up))
  :init-value
  (lambda (obj)
    (oset obj value
          (not (telega-transient--link-preview-options-get :show_above_text))))
  )

(transient-define-infix telega-transient--infix-link-preview-shrink ()
  "Shrink preview photo."
  :description (lambda () (telega-i18n "lng_link_shrink_photo"))
  :class 'telega-transient--checkbox-switch
  :global-p nil
  :variable :lp-shrink-p
  :if (lambda ()
        (telega--tl-get (telega-transient-scope)
                        :content :link_preview :has_large_media))
  :init-value
  (lambda (obj)
    (oset obj value
          (telega-transient--link-preview-options-get :force_small_media)))
  )

(transient-define-suffix telega-transient--suffix-link-preview-save (msg args)
  :description (lambda () (telega-i18n "lng_settings_save"))
  (interactive (list (telega-transient-scope)
                     (transient-args
                      (or transient-current-command
                          'telega-transient-link-preview-options))))

  (telega-msg--set-link-preview-options msg
    (list :@type "linkPreviewOptions"
          :url (telega--tl-get msg :content :link_preview :url)
          :force_small_media (if (alist-get :lp-shrink-p args) t :false)
          :force_large_media (if (alist-get :lp-shrink-p args) :false t)
          :show_above_text (if (alist-get :lp-move-up args) t :false)
          ))
  )

(transient-define-suffix telega-transient--suffix-link-preview-remove ()
  :description (lambda ()
                 (telega-ins--as-string
                  (telega-ins--with-face 'error
                    (telega-ins-i18n "lng_link_remove"))))
  (interactive)
  (let ((msg (oref (transient-prefix-object) scope)))
    (telega-msg-disable-link-preview msg)))

(transient-define-prefix telega-transient-link-preview-options (msg)
  [:description
   (lambda ()
     (telega-ins--as-string
      (telega-ins--with-face 'transient-heading
        (telega-ins-i18n "lng_link_options_header"))))
   ("a" telega-transient--infix-link-preview-up)
   ("b" telega-transient--infix-link-preview-down)
   ("m" telega-transient--infix-link-preview-shrink)
   ]
  [
   ("RET" telega-transient--suffix-link-preview-save)
   ("D" telega-transient--suffix-link-preview-remove)
   ]

  (interactive (list (telega-msg-for-interactive)))
  (transient-setup 'telega-transient-link-preview-options nil nil :scope msg)
  )


;;; Join group by chat invite link
(defun telega-transient--invite-link-need-subscription-p ()
  "Return non-nil if subscription to channel needed."
  (when-let ((s-info (telega--tl-get (telega-transient-scope)
                                     :invite-link-info :subscription_info)))
    (not (plist-get s-info :can_reuse))))

(defun telega-transient--invite-link-fake-chat (il-info)
  "Create fake chat from chatInviteLinkInfo IL-INFO.
Return fake chat suitable for `telega-ins--msg-sender'."
  (list :@type "chat"
        :type (cl-ecase (telega--tl-type (plist-get il-info :type))
                (inviteLinkChatTypeBasicGroup
                 '(:@type "chatTypeBasicGroup"))
                (inviteLinkChatTypeSupergroup
                 '(:@type "chatTypeSupergroup"))
                (inviteLinkChatTypeChannel
                 '(:@type "chatTypeSupergroup" :is_channel t)))
        :title (telega-tl-str il-info :title)
        :photo (plist-get il-info :photo)
        ))

(transient-define-infix telega-transient--infix-invite-link-about ()
  "Group description."
  :format "  %d"
  :description
  (lambda ()
    (let* ((scope (telega-transient-scope))
           (il-info (plist-get scope :invite-link-info))
           (fake-chat (telega-transient--invite-link-fake-chat il-info)))
      (telega-ins--as-string
       (telega-ins--with-attrs (list :fill 'left
                                     :fill-column fill-column
                                     :fill-prefix "  ")
         (telega-ins--with-face 'bold
           (telega-ins--msg-sender fake-chat
             :with-avatar-p 2
             :with-brackets-p t
             :with-title (concat (telega-msg-sender--verification-badges
                                  (plist-get il-info :verification_status))
                                 (telega-tl-str il-info :title))
             :with-palette (telega-palette-by-color-id
                            (plist-get il-info :accent_color_id))))
         (telega-ins--with-face 'telega-shadow
           (telega-ins-i18n (if (telega-chat-match-p fake-chat '(type channel))
                                "lng_chat_status_subscribers"
                              "lng_chat_status_members")
             :plural-count (plist-get il-info :member_count)
             :count (telega-number-human-readable
                     (plist-get il-info :member_count))))
         (telega-ins "\n")

         (unless (telega-transient--invite-link-need-subscription-p)
           (when-let* ((desc (telega-tl-str il-info :description))
                       (desc-lines (string-split desc "\n")))
             (telega-ins (nth 0 desc-lines))
             (when (> (length desc-lines) 1)
               (telega-ins (telega-symbol 'eliding)))
             (telega-ins "\n")))

         (when-let ((s-info (plist-get il-info :subscription_info)))
           (telega-ins-i18n "lng_channel_invite_subscription_about"
             :channel (propertize (telega-tl-str il-info :title)
                                  'face 'bold)
             :price (format "%s%d" (telega-symbol 'telegram-star)
                            (telega--tl-get s-info :pricing :star_count)))
           (telega-ins "\n")

           ;; My stars balance
           (telega-ins-describe-item (telega-i18n "lng_credits_summary_balance")
             (telega-ins (telega-symbol 'telegram-star)
                         (format "%d" (floor telega--owned-stars))))
           (telega-ins "\n")

           (telega-ins--with-face 'telega-shadow
             (telega-ins-i18n "lng_channel_invite_subscription_terms"
               :link (telega-ins--as-string
                      (telega-ins--raw-button
                          (telega-link-props
                           'url "https://telegram.org/tos/stars"
                           'face 'link)
                        (telega-ins-i18n "lng_paid_react_agree_link")))))
           (telega-ins "\n"))

         (when (and (telega-chat-match-p fake-chat '(type channel))
                    (plist-get il-info :creates_join_request))
           (telega-ins "\n")
           (telega-ins--with-face 'telega-shadow
             (telega-ins-i18n "lng_group_request_about_channel"))
           (telega-ins "\n"))
         ))))
  :class 'transient-information*)

(transient-define-suffix telega-transient--suffix-tos-stars ()
  "Open Telegram starts TOS."
  :transient t
  :description (lambda ()
                 (telega-i18n "lng_paid_react_agree_link"))
  :if #'telega-transient--invite-link-need-subscription-p

  (interactive)
  (telega-browse-url "https://telegram.org/tos/stars"))

(transient-define-suffix telega-transient--suffix-invite-link-subscribe (scope)
  "Paid subscription to a channel."
  :description (lambda ()
                 (telega-i18n "lng_channel_invite_subscription_button"))
  :if (lambda () (telega-transient--invite-link-need-subscription-p))

  (interactive (list (telega-transient-scope)))
  (message "TODO: subscribe to channel")
  )

(transient-define-suffix telega-transient--suffix-invite-link-join (scope)
  "Join group by invite link."
  :description
  (lambda ()
    (let ((il-info (plist-get (telega-transient-scope) :invite-link-info)))
      (cond
       ((plist-get il-info :subscription_info)
        (telega-i18n "lng_channel_invite_subscription_button"))
       ((plist-get il-info :creates_join_request)
        (telega-i18n "lng_group_request_to_join"))
       (t
        (telega-i18n "lng_group_invite_join")))))
  :if-not #'telega-transient--invite-link-need-subscription-p

  (interactive (list (telega-transient-scope)))
  (telega--joinChatByInviteLink (plist-get scope :url)))

(transient-define-prefix telega-transient-invite-link-join ()
  [:description (lambda ()
                  (if (telega--tl-get (telega-transient-scope)
                                      :invite-link-info :subscription_info)
                      (telega-i18n "lng_channel_invite_subscription_title")
                    (telega-i18n "lng_group_invite_join")))
  (telega-transient--infix-invite-link-about)]
  [
   ("t" telega-transient--suffix-tos-stars)
   ("S" telega-transient--suffix-invite-link-subscribe)
   ("RET" telega-transient--suffix-invite-link-join)
   ]
  )


;;; Translate/Summarize
(defun telega-transient--msg-reset-translate-and-summary (msg)
  "Reset translation/summarization for the message MSG."
  (plist-put msg :telega-translated nil)
  (plist-put msg :telega-summary nil))

(transient-define-infix telega-transient--infix-language ()
  "Language to use for translation summarization."
  :description (lambda () (telega-i18n "lng_settings_change_lang"))
  :class 'telega-transient--variable
  :format " %k %d (%v)"
  :argument ""
  :always-read t
  :choices (lambda ()
             (mapcar #'car (telega-i18n-translate-languages-alist)))
  :prompt (lambda (_obj) (concat (telega-i18n "lng_languages") ": "))
  :init-value (lambda (obj)
                (oset obj value
                      (or (telega-i18n-translate-language-code-to-language
                           (telega--tl-get (telega-transient-scope)
                                           :telega-translated :to_language_code))
                          (telega-transient--variable-get :translate-language)
                          (telega-i18n-translate-language-code-to-language
                           telega-translate-to-language-by-default))))
  :variable :translate-language)

(transient-define-infix telega-transient--infix-translate-show-original ()
  "Keep original content of the message when translating."
  :description (lambda () "telega-translate-show-original-content")
  :class 'telega-transient--checkbox-switch
  :variable 'telega-translate-show-original-content)

(defun telega-transient--infix-language-code ()
  "Return language as language code."
  (let ((args (transient-args
               (or transient-current-command
                   'telega-transient-msg-translate))))
    (alist-get (alist-get :translate-language args)
               (telega-i18n-translate-languages-alist)
               nil nil #'equal)))

(transient-define-suffix telega-transient--suffix-translate (msg)
  "Translate message."
  :description
  (lambda ()
    (telega-ins--as-string
     (telega-ins-i18n "lng_context_translate")
     (when-let ((to-lang (telega-transient--variable-get
                          :translate-language)))
       (telega-ins--with-face 'telega-shadow
         (telega-ins " " (telega-symbol 'right-arrow) " " to-lang)))))

  (interactive (list (telega-transient-scope)))
  (telega-msg-translate msg (telega-transient--infix-language-code)))

(transient-define-suffix telega-transient--suffix-translate-summarize-disable
  (msg)
  "Disable message translation and summarization."
  :description (lambda ()
                 (propertize (telega-i18n "lng_translate_show_original")
                             'face 'error))
  :if (lambda ()
        (let ((msg (telega-transient-scope)))
          (or (plist-get msg :telega-translated)
              (plist-get msg :telega-summary))))

  (interactive (list (telega-transient-scope)))
  (plist-put msg :telega-translated nil)
  (plist-put msg :telega-summary nil)
  (telega-msg-redisplay msg))

(transient-define-suffix telega-transient--suffix-summarize (msg)
  :description
  (lambda ()
    (telega-ins--as-string
     (telega-ins-i18n "lng_summarize_header_title")
     (telega-ins--with-face 'telega-shadow
       (telega-ins " ("
                   (telega-i18n-translate-language-code-to-language
                    (plist-get (telega-transient-scope) :summary_language_code))
                   ")"))))
  :if (lambda ()
        (let* ((msg (telega-transient-scope))
               (summary-lang-code (plist-get msg :summary_language_code))
               (summary (plist-get msg :telega-summary)))
          (and summary-lang-code
               (or (not summary)
                   (plist-get summary :to_language_code)))))

  (interactive (list (telega-transient-scope)))
  (telega-msg-summarize msg))

(transient-define-suffix telega-transient--suffix-summarize-translate (msg)
  :description (lambda ()
                 (concat (telega-i18n "lng_summarize_header_title")
                         " & "
                         (telega-i18n "lng_context_translate")))
  :if (lambda ()
        (plist-get (telega-transient-scope) :summary_language_code))

  (interactive (list (telega-transient-scope)))
  (telega-msg-summarize msg (telega-transient--infix-language-code)))

(transient-define-prefix telega-transient-msg-translate (msg)
  [:description (lambda ()
                  (telega-i18n "lng_context_translate"))
   ("l" telega-transient--infix-language)
   ("o" telega-transient--infix-translate-show-original)
   ]
  [
   ("t" telega-transient--suffix-translate)
   ("s" telega-transient--suffix-summarize)
   ("S" telega-transient--suffix-summarize-translate)
   ("x" telega-transient--suffix-translate-summarize-disable)
   ]

  (interactive (list (telega-msg-for-interactive)))
  (transient-setup 'telega-transient-msg-translate nil nil :scope msg))


;; Chatbuf's input options
;; TODO
(defun telega-transient-input-options-cap-self-destruct-p ()
  (and (telega-chatbuf-match-p '(type private bot))
       (seq-some (lambda (text)
                   (when-let ((attach (get-text-property 0 'telega-attach text)))
                     (memq (telega--tl-type attach)
                           '(inputMessagePhoto
                             inputMessageVideo
                             inputMessageVideoNote
                             inputMessageVoiceNote))))
                 (telega-transient-scope))))

(defun telega-transient-input-options-has-spoiler-p ()
  (seq-some (lambda (text)
              (when-let ((attach (get-text-property 0 'telega-attach text)))
                (memq (telega--tl-type attach)
                      '(inputMessageAnimation
                        inputMessagePhoto
                        inputMessageVideo))))
            (telega-transient-scope)))

(defun telega-transient-input-options-caption-above-p ()
  (seq-some (lambda (text)
              (when-let ((attach (get-text-property 0 'telega-attach text)))
                (memq (telega--tl-type attach)
                      '(inputMessageAnimation
                        inputMessagePaidMedia
                        inputMessagePhoto
                        inputMessageVideo))))
            (telega-transient-scope)))

(transient-define-infix telega-transient--infix-input-option-spoiler ()
  "Hide media content under spoiler."
  :description (lambda () (telega-i18n "lng_context_spoiler_effect"))
  :if #'telega-transient-input-options-has-spoiler-p
  :class 'telega-transient--checkbox-switch
  :global-p nil
  :variable :option-hide-with-spoiler-p)

(transient-define-infix telega-transient--infix-input-option-self-destruct ()
  "Self-destruct media after viewed by peer."
  :description (lambda () "self-destructing media")
  :if #'telega-transient-input-options-cap-self-destruct-p
  :class 'telega-transient--checkbox-switch
  :global-p nil
  :variable :option-self-destruct-p)

(transient-define-suffix telega-transient--suffix-input-option-apply (args)
  :description (lambda ()
                 (telega-i18n "lng_settings_apply"))

  (interactive (list (transient-args
                      (or transient-current-command
                          'telega-transient-chatbuf-input-options))))

  (setq telega-chatbuf--input-options-plist args)
  (telega-chatbuf--chat-update "aux-plist")
  (telega-chatbuf--prompt-update))

(transient-define-prefix telega-transient-chatbuf-input-options (attaches)
  "Edit chatbuf input options before sending message."
  [:description (lambda () "Input Options")
   ["Media Options"             
    ("s" telega-transient--infix-input-option-spoiler)
    ("d" telega-transient--infix-input-option-self-destruct)
    ]
   ["Link Preview Options"
    ("l" telega-transient-link-preview-options)
    ]

   " "
   ("RET" telega-transient--suffix-input-option-apply)
   ("D" telega-chatbuf-cancel-input-options
    :description (propertize "Cancel Options" 'face 'error)
    :if (lambda ()
          telega-chatbuf--input-options-plist))
   ]

  (interactive (list (telega--split-by-text-prop
                         (telega-chatbuf-input-string) 'telega-attach)))
  (transient-setup 'telega-transient-chatbuf-input-options nil nil
                   :scope attaches))


;;; ellit-org: minor-modes
;; ** telega-transient-keymaps-mode -- Use transient for telega keymaps
;;
;; Use transient (Magit-like style) commands in the telega.  Enable it
;; with:
;;
;; #+begin_src emacs-lisp
;; (telega-transient-keymaps-mode 1)
;; #+end_src
;;
;; Control keymaps for which to use transient with:
;; - {{{user-option(telega-transient-keymaps, 2)}}}
(defcustom telega-transient-keymaps
  '(telega-prefix-map
    telega-sort-map
    telega-filter-map
    telega-describe-map
    telega-folder-map
    telega-voip-map
    telega-root-fastnav-map
    telega-root-view-map
    telega-chatbuf-fastnav-map)
  "List of keymaps names to apply transient for."
  :type '(repeat symbol)
  :group 'telega-modes)

(eval-when-compile
  (defun telega-transient--keymap-prefix-name (keymap-symbol)
    (intern (format "telega-transient--prefix-%S" keymap-symbol))))

(defun telega-transient--keymap-setup-children (keymap-symbol)
  (let ((keymap (symbol-value keymap-symbol)))
    (mapcar (lambda (kf)
              (list (key-description (vector (car kf)))
                    (car (split-string (documentation (cdr kf)) "\n"))
                    (cdr kf)))
            (cl-remove-if-not #'commandp (cdr keymap)
                              :key #'cdr))))

;; TODO: add support for columns, i.e. vector of vectors instead of
;; single vector, see `magit-diff' as example
(defmacro telega-transient-define-prefix-by-keymap (keymap &rest description)
  (declare (indent 1))
  (let ((prefix-name (telega-transient--keymap-prefix-name keymap)))
    `(transient-define-prefix ,prefix-name nil
       ,(format "Transient commands for `%S' keymap." keymap)
       [,@description
        [:class transient-column
         :setup-children
         (lambda (_)
           (transient-parse-suffixes
            ',prefix-name
            (telega-transient--keymap-setup-children ',keymap)))
         ]])))

(telega-transient-define-prefix-by-keymap telega-prefix-map
  "Telega commands:")
(telega-transient-define-prefix-by-keymap telega-sort-map
  "Chat Sorter to apply:")
(telega-transient-define-prefix-by-keymap telega-filter-map
  "Chat Filter to apply:")
(telega-transient-define-prefix-by-keymap telega-describe-map
  "Describe commands:")
(telega-transient-define-prefix-by-keymap telega-folder-map
  "Chat Folder commands:")
(telega-transient-define-prefix-by-keymap telega-voip-map
  "VoIP commands:")
(telega-transient-define-prefix-by-keymap telega-root-fastnav-map
  "Root buffer fast navigation commands:")
(telega-transient-define-prefix-by-keymap telega-root-view-map
  "Root View commands:")
(telega-transient-define-prefix-by-keymap telega-chatbuf-fastnav-map
  "Chatbuf fast navigation commands:")

;;;###autoload
(define-minor-mode telega-transient-keymaps-mode
  "Global mode to enable transient commands in the telega"
  :init-value nil :global t :group 'telega-modes
  (if telega-transient-keymaps-mode
      (progn
        (when (memq 'telega-sort-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "\\")
                      (telega-transient--keymap-prefix-name
                       'telega-sort-map)))
        (when (memq 'telega-filter-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "/")
                      (telega-transient--keymap-prefix-name
                       'telega-filter-map)))
        (when (memq 'telega-describe-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "?")
                      (telega-transient--keymap-prefix-name
                       'telega-describe-map)))
        (when (memq 'telega-folder-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "F")
                      (telega-transient--keymap-prefix-name
                       'telega-folder-map)))
        (when (memq 'telega-voip-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "c")
                      (telega-transient--keymap-prefix-name
                       'telega-voip-map)))
        (when (memq 'telega-root-fastnav-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "M-g")
                      (telega-transient--keymap-prefix-name
                       'telega-root-fastnav-map)))
        (when (memq 'telega-root-view-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "v")
                      (telega-transient--keymap-prefix-name
                       'telega-root-view-map)))

        (when (memq 'telega-chatbuf-fastnav-map telega-transient-keymaps)
          (define-key telega-chat-mode-map (kbd "M-g")
                      (telega-transient--keymap-prefix-name
                       'telega-chatbuf-fastnav-map)))
        )

    (define-key telega-root-mode-map (kbd "\\") telega-sort-map)
    (define-key telega-root-mode-map (kbd "/") telega-filter-map)
    (define-key telega-root-mode-map (kbd "?") telega-describe-map)
    (define-key telega-root-mode-map (kbd "F") telega-folder-map)
    (define-key telega-root-mode-map (kbd "c") telega-voip-map)
    (define-key telega-root-mode-map (kbd "M-g") telega-root-fastnav-map)
    (define-key telega-root-mode-map (kbd "v") telega-root-view-map)

    (define-key telega-chat-mode-map (kbd "M-g") telega-chatbuf-fastnav-map)
    ))

(provide 'telega-transient)


(defun telega-transient-setup-buffer ()
  (when-let ((transient-command (oref transient--prefix command)))
    (when (string-prefix-p "telega-" (symbol-name transient-command))
      (setq-local nobreak-char-display nil))))

(add-hook 'transient-setup-buffer-hook 'telega-transient-setup-buffer)

;;; telega-transient.el ends here

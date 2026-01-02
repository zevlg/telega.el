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

(declare-function telega-msg-forward-dwim "telega-chat" (messages &optional remove-sender-p remove-caption-p))
(declare-function telega-msg-forward-dwim-to-many "telega-chat" (messages chats &optional remove-sender-p remove-caption-p))

(defvar telega-transient--variable-values nil
  "List of values used for transient.")

(defun telega-transient--variable-get (var)
  (plist-get telega-transient--variable-values var))

(defun telega-transient--variable-set (var value)
  (setq telega-transient--variable-values
        (plist-put telega-transient--variable-values var value)))

(defclass telega-transient--variable (transient-variable)
  ((i18n        :initarg :i18n        :initform nil)
   (default     :initarg :default     :initform nil)))

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
  (let ((on-p (telega-transient--variable-get (oref obj variable))))
    (propertize (telega-symbol
                 (if on-p (oref obj on-symbol) (oref obj off-symbol)))
                'face (if on-p 'transient-value 'transient-inactive-value))))

(cl-defmethod transient-infix-set ((obj telega-transient--variable:bool) value)
  (telega-transient--variable-set (oref obj variable) (oset obj value value))
  ;; Set values for dependent variables
  (seq-doseq (dep (oref obj deps-list))
    (when (eq value (nth 0 dep))
      (seq-doseq (dep-var (nthcdr 2 dep))
        (transient-infix-set
         (telega-transient--find-suffix dep-var)
         (nth 1 dep))))))

(cl-defmethod transient-infix-read ((obj telega-transient--variable:bool))
  (not (oref obj value)))

(defclass telega-transient--checkbox-switches (telega-transient--variable:bool)
  ((on-symbol                         :initform 'checkbox-on)
   (off-symbol                        :initform 'checkbox-off))
  "Class used for sets of checkbox buttons.")

(defclass telega-transient--radiobox-switches (telega-transient--variable:bool)
  ((on-symbol                         :initform 'radiobox-on)
   (off-symbol                        :initform 'radiobox-off))
  "Class used for sets of mutually exclusive radio buttons.")


(transient-define-infix telega-transient--infix-fwd-remove-sender ()
  "Hide/show sender name when forwarding."
  :description (lambda () (telega-i18n "lng_forward_action_hide_sender"))
  :class 'telega-transient--checkbox-switches
  :variable :fwd-remove-sender
  :deps-list '((nil nil :fwd-remove-caption))
  )

(transient-define-infix telega-transient--infix-fwd-remove-caption ()
  "Hide/show caption when forwarding."
  :description (lambda () (telega-i18n "lng_forward_action_hide_caption"))
  :class 'telega-transient--checkbox-switches
  :variable :fwd-remove-caption
  :deps-list '((t t :fwd-remove-sender)))

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
  :multi-value t)

(transient-define-suffix telega-transient-forward-to-chat ()
  "Forward messages to a single chat."
  :key "f"
  :description (lambda () (telega-i18n "lng_selected_forward"))
  (interactive)
  (telega-msg-forward-dwim
   (oref transient-current-prefix scope)
   (telega-transient--variable-get :fwd-remove-sender)
   (telega-transient--variable-get :fwd-remove-caption)))

(transient-define-suffix telega-transient-forward-to-many-chats ()
  "Forward messages to many chats."
  :key "F"
  :description (lambda () "Forward to many")
  (interactive)
  (telega-msg-forward-dwim-to-many
   (oref transient-current-prefix scope)
   (telega-completing-read-chat-list "Forward to Chats")
   (telega-transient--variable-get :fwd-remove-sender)
   (telega-transient--variable-get :fwd-remove-caption)))

(transient-define-suffix telega-transient-forward-to-saved-messages ()
  "Forward messages to the \"Saved Messages\" chat."
  :key "S"
  :description (lambda ()
                 (concat (telega-i18n "lng_selected_forward")
                         " " (telega-symbol 'right-arrow) " "
                         (telega-chat-title (telega-chat-me))))
  (interactive)
  (let* ((messages (oref transient-current-prefix scope))
         (from-chat (telega-msg-chat (car messages))))
    (when telega-chatbuf--marked-messages
      (telega-chatbuf-msg-marks-toggle))

    (telega--forwardMessages (telega-chat-me) from-chat messages
      nil (telega-transient--variable-get :fwd-remove-sender)
      (telega-transient--variable-get :fwd-remove-caption))

    (message "telega: %s"
             (telega-i18n (if (> (length messages) 1)
                              "lng_share_messages_to_chat"
                            "lng_share_message_to_chat")
               :chat (telega-chat-title (telega-chat-me))))))

(transient-define-prefix telega-transient-msg-forward (messages)
  "Message forwarding."
  [:description
   (lambda ()
     (let ((msg-count (length (oref transient--prefix scope))))
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
(transient-define-infix telega-transient--infix-msg-report-spam ()
  :description (lambda ()
                 (telega-i18n "lng_report_spam"))
  :class 'telega-transient--checkbox-switches
  :variable :msg-report-spam-p)

(transient-define-suffix telega-transient--infix-msg-revoke ()
  "Revoke message."
  :description (lambda ()
                 (telega-i18n "lng_delete_for_everyone_hint"))
  (interactive)
  (let* ((messages (oref transient-current-prefix scope)))
    ;; TODO
    ))

(transient-define-suffix telega-transient--infix-msg-delete ()
  "Delete messages for me only."
  :key "d"
  :description (lambda ()
                 (telega-i18n "lng_delete_for_me_chat_hint"))
  (interactive)
  (let* ((messages (oref transient-current-prefix scope)))
    ;; TODO
    ))

(transient-define-prefix telega-transient-msg-delete (messages)
  "Delete a MESSAGE."
  [:description
   (lambda ()
     (let ((msg-count (length (oref transient--prefix scope))))
       (if (> msg-count 1)
           (telega-i18n "lng_selected_delete_sure"
             :count msg-count)
         (telega-i18n "lng_selected_delete_sure_this"))))
   ]
  [:if (lambda ()
         (plist-get (telega-chat-member-my-permissions telega-chatbuf--chat)
                    :can_restrict_members))
   :description (lambda () "Block Options")
   ("s" telega-transient--infix-msg-report-spam)
   ("a" telega-transient--infix-msg-sender-delete-all)
   ("b" telega-transient--infix-msg-sender-block)
   ]
  [
;   (telega-transient--infix-msg-del-about)
   ("d" telega-transient--suffix-msg-delete)
   ("D" telega-transient--suffix-msg-revoke)
   ]

  (interactive
   (list (or telega-chatbuf--marked-messages
             (when-let ((msg-at-point (telega-msg-at (point))))
               (list msg-at-point)))))

  ;; NOTE: Check every message can be deleted
  (let ((telega-server-call-timeout 3.0))
    (seq-doseq (msg messages)
      (let ((msg-props (telega--getMessageProperties msg)))
        (unless (or (plist-get msg-props :can_be_deleted_only_for_self)
                    (plist-get msg-props :can_be_deleted_for_all_users))
          (telega-msg-goto msg 'highlight)
          (user-error "telega: Message MSG-ID=%S can't be deleted"
                      (plist-get msg :id))))))
  (transient-setup 'telega-transient-msg-delete nil nil :scope messages)
  )


;;; Delete chat
(transient-define-prefix telega-transient-chat-delete (chat)
  "Delete a CHAT."
  [:if (lambda ()
         (plist-get (telega-chat-member-my-permissions telega-chatbuf--chat)
                    :can_restrict_members))
   :description (lambda () "Options")
   ("m" telega-transient--infix-chat-delete-history-for-me)
   ("h" telega-transient--infix-chat-delete-history-for-all)
   ("b" telega-transient--infix-chat-user-block)
   ]
  [
   ("D" telega-transient--suffix-chat-delete)
   ("A" telega-transient--suffix-chat-archive)
   ]

  (interactive (list (telega-chat-for-interactive)))
  (transient-setup 'telega-transient-chat-delete nil nil :scope chat)
  )


;;; Create Group



;;; Tag Commands
(transient-define-suffix telega-transient--suffix-sm-tag-filter ()
  :key "/"
  :description (lambda () (telega-i18n "lng_context_filter_by_tag"))
  (interactive)
  (let* ((cmd-scope (oref transient-current-prefix scope))
         (tag (plist-get cmd-scope :tag)))
    (telega-chatbuf-filter-by-saved-messages-tag tag)))

(transient-define-suffix telega-transient--suffix-sm-tag-add-name ()
  :key "n"
  :description
  (lambda ()
    (let ((tag (plist-get (oref transient--prefix scope) :tag)))
      (if (telega-tl-str tag :label)
          (telega-i18n "lng_context_tag_edit_name")
        (telega-ins--as-string
         (telega-ins (telega-i18n "lng_context_tag_add_name") "\n")
         (telega-ins--help-message
          (telega-ins-i18n "lng_edit_tag_about"))))))
  (interactive)
  (let* ((cmd-scope (oref transient-current-prefix scope))
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
  (let* ((cmd-scope (oref transient-current-prefix scope))
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
       (plist-get (oref transient--prefix scope) :tag))
      (telega-ins " ")
      (telega-ins--with-face 'transient-heading
        (telega-ins "Tag Commands"))))
   ("/" telega-transient--suffix-sm-tag-filter)
   ("n" telega-transient--suffix-sm-tag-add-name)
   ("d" telega-transient--suffix-sm-tag-remove)
   ])


;;; Link Preview Options
(transient-define-infix telega-transient--infix-link-preview-up ()
  "Move link preview photo up."
  :description (lambda () (telega-i18n "lng_link_move_up"))
  :class 'telega-transient--radiobox-switches
  :variable :lp-move-up
  :deps-list '((t nil :lp-move-down))
  )

(transient-define-infix telega-transient--infix-link-preview-down ()
  "Move link preview photo down."
  :description (lambda () (telega-i18n "lng_link_move_down"))
  :class 'telega-transient--radiobox-switches
  :variable :lp-move-down
  :deps-list '((t nil :lp-move-up))
  )

(transient-define-infix telega-transient--infix-link-preview-shrink ()
  "Shrink preview photo."
  :description (lambda () (telega-i18n "lng_link_move_down"))
  :class 'telega-transient--checkbox-switches
  :variable :lp-shrink-p
  )

(transient-define-suffix telega-transient--suffix-link-preview-save ()
  :description (lambda () (telega-i18n "lng_settings_save"))
  (interactive)
  (let ((msg (oref transient-current-prefix scope)))
    (telega-msg--set-link-preview-options msg
      (list :@type "linkPreviewOptions"
            :url 
            :force_small_media
            :force_large_media
            :show_above_text (telega-transient--variable-get :lp-move-up)
            ))
    (telega-msg-disable-link-preview msg)))

(transient-define-suffix telega-transient--suffix-link-preview-remove ()
  :description (lambda ()
                 (telega-ins--as-string
                  (telega-ins--with-face 'error
                    (telega-i18n "lng_link_remove"))))
  (interactive)
  (let ((msg (oref transient-current-prefix scope)))
    (telega-msg-disable-link-preview msg)))

(transient-define-prefix telega-transient-link-preview-options (msg)
  [:description
   (lambda ()
     (telega-ins--as-string
      (telega-ins--with-face 'transient-heading
        (telega-ins-i18n "lng_link_options_header"))))

   ("u" telega-transient--infix-link-preview-up)
   ("d" telega-transient--infix-link-preview-down)
   ("m" telega-transient--infix-link-preview-shrink)
   ]

  [
   ("S" telega-transient--suffix-link-preview-save)
   ("R" telega-transient--suffix-link-preview-remove)
   ]

  (interactive (list (telega-msg-for-interactive)))
  (transient-setup 'telega-transient-link-preview-options nil nil :scope msg)
  )


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

;;; telega-transient.el ends here

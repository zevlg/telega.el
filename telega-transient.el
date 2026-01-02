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
                                                :fill-column 60
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
   (transient-setup 'telega-transient-forward nil nil :scope messages))


;;; Delete message 
(transient-define-infix telega-transient--infix-msg-report-spam ()
  :description (lambda ()
                 (telega-i18n "lng_report_spam"))
  :class 'telega-transient--checkbox-switches
  :variable :msg-report-spam-p)

(transient-define-infix telega-transient--infix-msg-revoke ()
  "Revoke message."
  :description (lambda ()
                 ;; TODO
                 )
  :class 'telega-transient--checkbox-switches
  :variable :msg-revoke-p)

(transient-define-prefix telega-transient-msg-delete (message)
  "Delete a MESSAGE."
  [:if (plist-get (telega-chat-member-my-permissions telega-chatbuf--chat)
                  :can_restrict_members)
   :description (lambda () "Block Options")
   ("s" telega-transient--infix-msg-report-spam)
   ("a" telega-transient--infix-msg-sender-delete-all)
   ("b" telega-transient--infix-msg-sender-block)
   ]
  [
   (telega-transient--infix-fwd-about)
   ("d" telega-transient--infix-msg-delete)
   ("r" telega-transient--infix-msg-revoke)
   ]

  (interactive (list (or (telega-chatbuf-match-p
                          '(type supergroup channel secret))
                         (not current-prefix-arg))))
  )


;;; Delete chat
(transient-define-prefix telega-transient-msg-delete (message)
  "Delete a MESSAGE."
  [:if (plist-get (telega-chat-member-my-permissions telega-chatbuf--chat)
                  :can_restrict_members)
   :description (lambda () "Options")
   ("m" telega-transient--infix-chat-delete-history-for-me)
   ("h" telega-transient--infix-chat-delete-history-for-all)
   ("b" telega-transient--infix-chat-user-block)
   ]
  [
   ("d" telega-transient--infix-chat-delete)
   ("a" telega-transient--infix-chat-archive)
   ]

  (interactive (list (or (telega-chatbuf-match-p
                          '(type supergroup channel secret))
                         (not current-prefix-arg))))
  )


;;; Create Group


;; Saved Messages tags
(transient-define-prefix telega-saved-messages-tag-commands ()
  [:description
   (lambda ()
     (telega-ins--as-string
      (telega-ins--saved-messages-tag (car (oref transient--prefix scope)))
      (telega-ins " ")
      (telega-ins--with-face 'transient-heading
        (telega-ins "Tag Commands"))))
   ("/" telega-saved-messages-tag-filter)
   ("n" telega-saved-messages-tag-add-name)
   ("d" telega-saved-messages-tag-remove)
   ])


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
  :type 'list
  :group 'telega-modes)

;; TODO: add support for columns, i.e. vector of vectors instead of
;; single vector, see `magit-diff' as example
(defmacro telega-transient-define-prefix-by-keymap (name label keymap)
  (declare (indent 2))
  `(transient-define-prefix ,name nil
     ,(format "Transient command for `%S' keymap." keymap)
     [,label
      ,@(mapcar (lambda (kf)
                  (list (key-description (vector (car kf)))
                        (car (split-string (documentation (cdr kf)) "\n"))
                        (cdr kf)))
                (cl-remove-if-not #'commandp (cdr (symbol-value keymap))
                                  :key #'cdr))
      ]))

(telega-transient-define-prefix-by-keymap telega-transient-telega
    "Telega commands:" telega-prefix-map)
(telega-transient-define-prefix-by-keymap telega-transient-chat-sort
    "Chat Sorter to apply:" telega-sort-map)
(telega-transient-define-prefix-by-keymap telega-transient-chat-filter
    "Chat Filter to apply:" telega-filter-map)
(telega-transient-define-prefix-by-keymap telega-transient-describe
    "Describe commands:" telega-describe-map)
(telega-transient-define-prefix-by-keymap telega-transient-chat-folder
    "Chat Folder commands:" telega-folder-map)
(telega-transient-define-prefix-by-keymap telega-transient-voip
    "VoIP commands:" telega-voip-map)
(telega-transient-define-prefix-by-keymap telega-transient-root-fastnav
    "Root buffer fast navigation commands:" telega-root-fastnav-map)
(telega-transient-define-prefix-by-keymap telega-transient-root-view
    "Root View commands:" telega-root-view-map)
(telega-transient-define-prefix-by-keymap telega-transient-chatbuf-fastnav
    "Chatbuf fast navigation commands:" telega-chatbuf-fastnav-map)

;;;###autoload
(define-minor-mode telega-transient-keymaps-mode
  "Global mode to enable transient commands in the telega"
  :init-value nil :global t :group 'telega-modes
  (if telega-transient-keymaps-mode
      (progn
        (when (memq 'telega-sort-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "\\")
            'telega-transient-chat-sort))
        (when (memq 'telega-filter-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "/")
            'telega-transient-chat-filter))
        (when (memq 'telega-describe-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "?")
            'telega-transient-describe))
        (when (memq 'telega-folder-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "F")
            'telega-transient-chat-folder))
        (when (memq 'telega-voip-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "c")
            'telega-transient-voip))
        (when (memq 'telega-root-fastnav-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "M-g")
            'telega-transient-root-fastnav))
        (when (memq 'telega-root-view-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "v")
            'telega-transient-root-view))

        (when (memq 'telega-chatbuf-fastnav-map telega-transient-keymaps)
          (define-key telega-chat-mode-map (kbd "M-g")
            'telega-transient-chatbuf-fastnav))
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

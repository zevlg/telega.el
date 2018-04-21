;;; telega.el --- Telegram client

;; Copyright (C) 2016-2018 by Zajcev Evgeny

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Nov 30 19:04:26 2016
;; Keywords:
;; Version: 0.1.3

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
(eval-when-compile
  (require 'cl)) ;; for defsetf

(defconst telega-app '(72239 . "bbf972f94cc6f0ee5da969d8d42a6c76"))
(defconst telega-version "0.1.3")


(defgroup telega nil
  "Telegram client."
  :prefix "telega-"
  :group 'applications)

(defcustom telega-directory (expand-file-name "~/.telega")
  "Directory for telega runtime files."
  :type 'string
  :group 'telega)

(defcustom telega-cache-dir (expand-file-name "cache" telega-directory)
  "*Directory for telegram downloads."
  :type 'string
  :group 'telega)

(defcustom telega-debug nil
  "*Non-nil to enable telega debugging buffer."
  :type 'boolean
  :group 'telega)

(defcustom telega-use-test-dc nil
  "*Non-nil to use telegram's test environment instead of production."
  :type 'bool
  :group 'telega)

(defcustom telega-language "en"
  "*IETF language tag of the user's language."
  :type 'string
  :group 'telega)

(defcustom telega-options-plist nil
  "*Plist of options to set.
Only writable options can be set.  See: https://core.telegram.org/tdlib/options
NOT IMPLEMENTED"
  :type 'plist
  :group 'telega)

(defcustom telega-rsa-key-file (expand-file-name "server.pub" telega-directory)
  "*RSA key to use."
  :type 'string
  :group 'telega)

(defgroup telega-server nil
  "Customisation for telega-server."
  :prefix "telega-server-"
  :group 'telega)

(defgroup telega-root nil
  "Customization for telega-root-mode"
  :prefix "telega-root-"
  :group 'telega)

(defgroup telega-hooks nil
  "Hooks called by telega."
  :group 'telega)

(defcustom telega-ready-hook nil
  "Hook called when telega is ready to process queries."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-closed-hook nil
  "Hook called when telega exited."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-tracking '(groups users)
  "*Buffers to track activity on."
  :type '(repeat (choice (const :tag "groups" groups)
                         (const :tag "Users" users)
                         (const :tag "Bots" bots)
                         (const :tag "Channels" channels)))
  :group 'telega)

(defcustom telega-track-faces-priorities '(lui-highlight-face)
  "A list of faces which should show up in the tracking."
  :type '(repeat face)
  :group 'telega)

;;; Faces

(defface telega-atsign-face
  '((default (:weight bold))
    (((type tty)) (:foreground "cyan"))
    (((background dark)) (:foreground "#82e2ed"))
    (((background light)) (:foreground "#0445b7"))
    (t (:foreground "CadetBlue3")))
  "The face used to highlight text starting with @."
  :group 'telega)


(defcustom telega-prompt-string "> "
  "The string to initialize the prompt with.
To change the prompt dynamically or just in specific buffers, use
`lui-set-prompt' in the appropriate hooks."
  :type 'string
  :group 'telega)

;;; Runtime variables
(defvar telega--chats nil "Hash table (id -> chat) for all chats.")
(defvar telega--ordered-chats nil "Ordered list of all chats.")
(defvar telega--users nil "Hash table (id -> user) for all users.")

;;; Utility functions
(defsubst telega-debug (fmt &rest args)
  (when telega-debug
    (with-current-buffer (get-buffer-create "*telega-debug*")
      (goto-char (point-max))
      (insert (apply 'format (cons (concat fmt "\n") args))))))

(defun telega--create-hier ()
  "Ensure directory hier is valid."
  (ignore-errors
    (mkdir telega-directory))
  (ignore-errors
    (mkdir telega-cache-dir)))

;;;###autoload
(defun telega ()
  "Start telegramming."
  (interactive)
  (telega--create-hier)
  (with-current-buffer (get-buffer-create telega-root-buffer-name)
    (telega-root-mode)
    (telega-server--start))
  (pop-to-buffer-same-window telega-root-buffer-name))

;;;###autoload
(defun telega-logout ()
  "Switch to another telegram account."
  (interactive)
  (telega-server--send `(:@type "logOut")))


;;; telega-server stuff
(defmacro telega--tl-type (tl-obj)
  `(intern (plist-get ,tl-obj :@type)))
(defsetf telega--tl-type (tl-obj) (type-sym)
  `(plist-put ,tl-obj :@type (symbol-name ',type-sym)))

(defmacro telega--tl-bool (tl-obj prop)
  `(not (eq (plist-get ,tl-obj ,prop) ,json-false)))

(declare-function telega-server--send "telega-server" (sexp))

(defun telega--set-tdlib-parameters ()
  "Sets the parameters for TDLib initialization."
  (telega-server--send
   `(:@type "setTdlibParameters"
            :parameters
            (:@type "tdlibParameters"
                    :use_test_dc ,(or telega-use-test-dc json-false)
                    :database_directory ,telega-directory
                    :files_directory ,telega-cache-dir
                    :use_file_database t
                    :use_chat_info_database t
                    :use_message_database t
                    :api_id ,(car telega-app)
                    :api_hash ,(cdr telega-app)
                    :system_language_code ,telega-language
                    :application_version ,telega-version
                    :device_model "desktop"
                    :system_version "unknown"
                    :ignore_file_names ,json-false
                    :enable_storage_optimizer t
            ))))

(defun telega--check-database-encryption-key ()
  "Set database encryption key, if any."
  ;; NOTE: database encryption is disabled
  ;;   consider encryption as todo in future
  (telega-server--send
   `(:@type "checkDatabaseEncryptionKey"
            :encryption_key "")))

(defun telega--set-auth-phone-number ()
  "Sets the phone number of the user."
  (let ((phone (read-string "Telega phone number: " "+")))
    (telega-server--send
     `(:@type "setAuthenticationPhoneNumber"
              :phone_number ,phone
              :allow_flash_call ,json-false
              :is_current_phone_number ,json-false))))

(defun telega--resend-auth-code ()
  "Resends auth code, works only if current state is authorizationStateWaitCode."
  )

(defun telega--check-auth-code (registered-p)
  "Send login auth code."
  (let ((code (read-string "Telega login code: ")))
    (assert registered-p)
    (telega-server--send
     `(:@type "checkAuthenticationCode"
              :code ,code
              :first_name ""
              :last_name ""))))

(defun telega--authorization-ready ()
  "Called when tdlib is ready to receive queries."
  (setq telega--chats (make-hash-table :test 'eq))
  (setq telega--users (make-hash-table :test 'eq))
  (setq telega--ordered-chats nil)

  ;; Request for chats/users/etc
  (run-hooks 'telega-ready-hook)

  (cl-loop for (prop-name value) in telega-options-plist
           do (telega-server--send
               `(:@type "setOption" :name ,prop-name
                        :value (:@type ,(cond ((memq value '(t nil))
                                               "optionValueBoolean")
                                              ((integerp value)
                                               "optionValueInteger")
                                              ((stringp value)
                                               "optionValueString"))
                                       :value (or value ,json-false)))))
  (telega-chat--getChatList))

(defun telega--authorization-closed ()
  (telega-server-kill)
  (telega-root--state "Auth Closed")
  (run-hooks 'telega-closed-hook))

(defun telega--on-updateConnectionState (event)
  "Update telega connection state."
  (let* ((conn-state (plist-get (plist-get event :state) :@type))
         (root-state (substring conn-state 15)))
    (telega-root--state (concat "Conn " root-state))))

(defun telega--on-updateOption (event)
  "Proceed with option update from telega server."
  (setq telega--options
        (plist-put telega--options
                   (intern (concat ":" (plist-get event :name)))
                   (plist-get (plist-get event :value) :value))))

(defun telega--on-updateAuthorizationState (event)
  (let* ((state (plist-get event :authorization_state))
         (stype (plist-get state :@type)))

    (telega-root--state (concat "Auth " (substring stype 18)))

    (ecase (intern stype)
      (authorizationStateWaitTdlibParameters
       (telega-root--state "Connecting..")
       (telega--set-tdlib-parameters))

      (authorizationStateWaitEncryptionKey
       (telega--check-database-encryption-key))

      (authorizationStateWaitPhoneNumber
       (telega--set-auth-phone-number))

      (authorizationStateWaitCode
       (telega--check-auth-code (plist-get state :is_registered)))

      (authorizationStateReady
       ;; TDLib is now ready to answer queries
       (telega--authorization-ready))

      (authorizationStateLoggingOut
       (telega-root--state "Auth Logging Out"))

      (authorizationStateClosing
       (telega-root--state "Auth Closing"))

      (authorizationStateClosed
       (telega--authorization-closed))
      )))

(defun telega--on-event (event)
  (telega-debug "IN event: %s" event)

  (let ((event-sym (intern (format "telega--on-%s" (plist-get event :@type)))))
    (if (symbol-function event-sym)
        (funcall (symbol-function event-sym) event)

      (telega-debug "TODO: define `%S'" event-sym))))

(defun telega--on-error (err)
  (telega-debug "IN error: %s" err)

  (message "Telega error: %s" err))

(defun telega--on-ok (event)
  "On ok result from command function call."
  ;; no-op
  )

(provide 'telega)

;;; telega.el ends here

;;; telega.el --- Telegram client (unofficial)

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
(require 'telega-server)
(require 'telega-root)
(require 'telega-filter)
(require 'telega-chat)
(require 'telega-user)
(require 'telega-notifications)

(defconst telega-app '(72239 . "bbf972f94cc6f0ee5da969d8d42a6c76"))
(defconst telega-version "0.1.3")

(defun telega--create-hier ()
  "Ensure directory hier is valid."
  (ignore-errors
    (mkdir telega-directory))
  (ignore-errors
    (mkdir telega-cache-dir)))

(defun telega--init-vars ()
  "Initialize runtime variables."
  (setq telega--options nil)
  (setq telega--chats (make-hash-table :test 'eq))
  (setq telega--users (make-hash-table :test 'eq))
  (setq telega--ordered-chats nil)
  (setq telega--filtered-chats nil)
  (setq telega--filters nil)
  (setq telega--undo-filters nil)
  )

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

(defun telega--set-tdlib-parameters ()
  "Sets the parameters for TDLib initialization."
  (telega-server--send
   `(:@type "setTdlibParameters"
            :parameters
            (:@type "tdlibParameters"
                    :use_test_dc ,(or telega-use-test-dc json-false)
                    :database_directory ,telega-directory
                    :files_directory ,telega-cache-dir
                    :use_file_database ,telega-use-file-database
                    :use_chat_info_database ,telega-use-chat-info-database
                    :use_message_database ,telega-use-message-database
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
  (message "TODO: `telega--resend-auth-code'")
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

(defun telega--set-options ()
  "Send `telega-options-plist' to server."
  (cl-loop for (prop-name value) in telega-options-plist
           do (telega-server--send
               `(:@type "setOption" :name ,prop-name
                        :value (:@type ,(cond ((memq value '(t nil))
                                               "optionValueBoolean")
                                              ((integerp value)
                                               "optionValueInteger")
                                              ((stringp value)
                                               "optionValueString"))
                                       :value (or value ,json-false))))))

(defun telega--authorization-ready ()
  "Called when tdlib is ready to receive queries."
  (telega--init-vars)
  (telega--set-options)
  ;; Request for chats/users/etc
  (telega-chat--getChats)

  (run-hooks 'telega-ready-hook))

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
       (telega--authorization-closed)))))

(defun telega--on-ok (event)
  "On ok result from command function call."
  ;; no-op
  )

(provide 'telega)

;;; telega.el ends here

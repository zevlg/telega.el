;;; telega.el --- Telegram client (unofficial)

;; Copyright (C) 2016-2018 by Zajcev Evgeny

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Nov 30 19:04:26 2016
;; Keywords:
;; Version: 0.2.5

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
(require 'telega-info)

(defconst telega-app '(72239 . "bbf972f94cc6f0ee5da969d8d42a6c76"))
(defconst telega-version "0.2.5")

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

  (unless (process-live-p (telega-server--proc))
    (telega-server--start))
  (unless (buffer-live-p (telega-root--buffer))
    (with-current-buffer (get-buffer-create telega-root-buffer-name)
      (telega-root-mode)))

  (pop-to-buffer-same-window telega-root-buffer-name))

;;;###autoload
(defun telega-kill (force)
  "Kill currently running telega.
With prefix arg force quit without confirmation."
  (interactive "P")
  (when (or force (y-or-n-p (concat "Kill telega"
                                    (let ((chat-count (length telega--chat-buffers)))
                                      (cond ((eq chat-count 0) "")
                                            ((eq chat-count 1) (format " (and 1 chat buffer)"))
                                            (t (format " (and all %d chat buffers)" chat-count))))
                                    "? ")))
    (kill-buffer telega-root-buffer-name)))

(defun telega-logout ()
  "Switch to another telegram account."
  (interactive)
  (telega-server--send `(:@type "logOut")))

(defun telega--set-tdlib-parameters ()
  "Sets the parameters for TDLib initialization."
  (telega-server--send
   (list :@type "setTdlibParameters"
         :parameters (list :@type "tdlibParameters"
                           :use_test_dc (or telega-use-test-dc :false)
                           :database_directory telega-directory
                           :files_directory telega-cache-dir
                           :use_file_database telega-use-file-database
                           :use_chat_info_database telega-use-chat-info-database
                           :use_message_database telega-use-message-database
                           :api_id (car telega-app)
                           :api_hash (cdr telega-app)
                           :system_language_code telega-language
                           :application_version telega-version
                           :device_model "Emacs"
                           :system_version emacs-version
                           :ignore_file_names :false
                           :enable_storage_optimizer t
                           )))

  ;; List of proxies, since tdlib 1.3.0
  (dolist (proxy telega-proxies)
    (telega-server--send
     `(:@type "addProxy" ,@proxy))))

(defun telega--check-database-encryption-key ()
  "Set database encryption key, if any."
  ;; NOTE: database encryption is disabled
  ;;   consider encryption as todo in future
  (telega-server--send
   `(:@type "checkDatabaseEncryptionKey"
            :encryption_key ""))

  ;; Set proxy here, so registering phone will use it
  (when telega-socks5-proxy
    (telega-server--send
     `(:@type "setProxy" :proxy (:@type "proxySocks5" ,@telega-socks5-proxy)))))

(defun telega--set-auth-phone-number ()
  "Sets the phone number of the user."
  (let ((phone (read-string "Telega phone number: " "+")))
    (telega-server--send
     (list :@type "setAuthenticationPhoneNumber"
           :phone_number phone
           :allow_flash_call :false
           :is_current_phone_number :false))))

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

(defun telega--check-password (auth-state)
  "Check the password for the 2-factor authentification."
  (let* ((hint (plist-get auth-state :password_hint))
         (passwd (password-read
                  (concat "Telegram password"
                          (if (string-empty-p hint)
                              ""
                            (format "(hint='%s')" hint))
                          ": "))))
    (telega-server--send
     `(:@type "checkAuthenticationPassword" :password ,passwd))))

(defun telega--set-options ()
  "Send custom options from `telega-options-plist' to server."
  (cl-loop for (prop-name value) in telega-options-plist
           do (telega-server--send
               (list :@type "setOption"
                     :name prop-name
                     :value (list :@type (cond ((memq value '(t nil))
                                                "optionValueBoolean")
                                               ((integerp value)
                                                "optionValueInteger")
                                               ((stringp value)
                                                "optionValueString"))
                                  :value (or value :false))))))

(defun telega--authorization-ready ()
  "Called when tdlib is ready to receive queries."
  (setq telega--me-id
        (plist-get (telega-server--call (list :@type "getMe")) :id))
  (telega--set-options)
  ;; Request for chats/users/etc
  (telega-chat--getChats)

  (run-hooks 'telega-ready-hook))

(defun telega--authorization-closed ()
  (telega-server-kill)
  (run-hooks 'telega-closed-hook))

(defun telega--on-updateConnectionState (event)
  "Update telega connection state."
  (let* ((conn-state (plist-get (plist-get event :state) :@type))
         (status (substring conn-state 15)))
    (telega-status--set status)))

(defun telega--on-updateOption (event)
  "Proceed with option update from telega server."
  (setq telega--options
        (plist-put telega--options
                   (intern (concat ":" (plist-get event :name)))
                   (plist-get (plist-get event :value) :value))))

(defun telega--on-updateAuthorizationState (event)
  (let* ((state (plist-get event :authorization_state))
         (stype (plist-get state :@type)))
    (telega-status--set (concat "Auth " (substring stype 18)))
    (ecase (intern stype)
      (authorizationStateWaitTdlibParameters
       (telega--set-tdlib-parameters))

      (authorizationStateWaitEncryptionKey
       (telega--check-database-encryption-key))

      (authorizationStateWaitPhoneNumber
       (telega--set-auth-phone-number))

      (authorizationStateWaitCode
       (telega--check-auth-code (plist-get state :is_registered)))

      (authorizationStateWaitPassword
       (telega--check-password state))

      (authorizationStateReady
       ;; TDLib is now ready to answer queries
       (telega--authorization-ready))

      (authorizationStateLoggingOut
       )

      (authorizationStateClosing
       )

      (authorizationStateClosed
       (telega--authorization-closed)))))

(defun telega--on-ok (event)
  "On ok result from command function call."
  ;; no-op
  )

(provide 'telega)

;;; telega.el ends here

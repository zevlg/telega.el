;;; telega.el --- Telegram client (unofficial)  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2019 by Zajcev Evgeny

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Nov 30 19:04:26 2016
;; Keywords: comm
;; Package-Requires: ((emacs "26.1") (visual-fill-column "1.9"))
;; URL: https://github.com/zevlg/telega.el
;; Version: 0.5.0
(defconst telega-version "0.5.0")
(defconst telega-tdlib-min-version "1.5.0")

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

;; See https://github.com/zevlg/telega.el/blob/master/README.md
;;
;; Start with M-x telega RET

;;; Code:
(require 'password-cache)               ; `password-read'
(require 'cl-lib)

(require 'telega-customize)
(require 'telega-server)
(require 'telega-root)
(require 'telega-ins)
(require 'telega-filter)
(require 'telega-chat)
(require 'telega-user)
(require 'telega-info)
(require 'telega-media)
(require 'telega-sticker)
(require 'telega-util)
(require 'telega-vvnote)
(require 'telega-webpage)
(require 'telega-notifications)
(require 'telega-modes)

(defconst telega-app '(72239 . "bbf972f94cc6f0ee5da969d8d42a6c76"))

(defvar telega-prefix-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "t") 'telega)
    (define-key map (kbd "s") 'telega-saved-messages)
    (define-key map (kbd "b") 'telega-switch-buffer)
    (define-key map (kbd "f") 'telega-file-send)
    (define-key map (kbd "w") 'telega-save-buffer)
    map)
  "Keymap for the telega commands.")

(defun telega--create-hier ()
  "Ensure directory hier is valid."
  (ignore-errors
    (mkdir telega-directory))
  (ignore-errors
    (mkdir telega-cache-dir))
  (ignore-errors
    (mkdir telega-temp-dir))
  )

;;;###autoload
(defun telega (arg)
  "Start telegramming.
If prefix ARG is given, then will not pop to telega root buffer."
  (interactive "P")
  (telega--create-hier)

  (unless (telega-server-live-p)
    ;; NOTE: for telega-server restarts also recreate root buffer,
    ;; killing root buffer also cleanup all chat buffers and stops any
    ;; timers used for animation
    (when (buffer-live-p (telega-root--buffer))
      (kill-buffer (telega-root--buffer)))

    (telega--init-vars)
    (with-current-buffer (get-buffer-create telega-root-buffer-name)
      (telega-root-mode))

    (telega-server--check-version telega-version)
    (telega-server--start))

  (unless arg
    (pop-to-buffer-same-window telega-root-buffer-name)))

;;;###autoload
(defun telega-kill (force)
  "Kill currently running telega.
With prefix arg FORCE quit without confirmation."
  (interactive "P")
  (let* ((chat-count (length telega--chat-buffers))
         (suffix (cond ((eq chat-count 0) "")
                       ((eq chat-count 1) (format " (and 1 chat buffer)"))
                       (t (format " (and all %d chat buffers)" chat-count)))))
    (when (or force (y-or-n-p (concat "Kill telega" suffix "? ")))
      (kill-buffer telega-root-buffer-name))))

(defun telega-logout ()
  "Switch to another telegram account."
  (interactive)
  (telega-server--send `(:@type "logOut")))

(defun telega--setTdlibParameters ()
  "Set the parameters for TDLib initialization."
  (telega-server--send
   (list :@type "setTdlibParameters"
         :parameters (list :@type "tdlibParameters"
                           :use_test_dc (or telega-use-test-dc :false)
                           :database_directory telega-directory
                           :files_directory telega-cache-dir
                           :use_file_database telega-use-file-database
                           :use_chat_info_database telega-use-chat-info-database
                           :use_message_database telega-use-message-database
                           :use_secret_chats t
                           :api_id (car telega-app)
                           :api_hash (cdr telega-app)
                           :system_language_code telega-language
                           :device_model "Emacs"
                           :system_version emacs-version
                           :application_version telega-version
                           :enable_storage_optimizer t
                           :ignore_file_names :false
                           ))))

(defun telega--checkDatabaseEncryptionKey ()
  "Set database encryption key, if any."
  ;; NOTE: database encryption is disabled
  ;;   consider encryption as todo in future
  (telega-server--send
   (list :@type "checkDatabaseEncryptionKey"
         :encryption_key ""))

  ;; List of proxies, since tdlib 1.3.0
  ;; Do it after checkDatabaseEncryptionKey,
  ;; See https://github.com/tdlib/td/issues/456
  (dolist (proxy telega-proxies)
    (telega-server--send
     `(:@type "addProxy" ,@proxy))))

(defun telega--setAuthenticationPhoneNumber (&optional phone-number)
  "Set user's phone number to PHONE-NUMBER."
  (let ((phone (or phone-number (read-string "Telega phone number: " "+"))))
    (telega-server--send
     (list :@type "setAuthenticationPhoneNumber"
           :phone_number phone
           :allow_flash_call :false
           :is_current_phone_number :false))))

(defun telega-resend-auth-code ()
  "Resend auth code.
Works only if current state is `authorizationStateWaitCode'."
  (interactive)
  (telega-server--send
   (list :@type "resendAuthenticationCode")))

(defun telega--checkAuthenticationCode (registered-p &optional auth-code)
  "Send login AUTH-CODE.
Specify non-nil REGISTERED-P for already registered user."
  (let ((code (or auth-code (read-string "Telega login code: ")))
        ;; NOTE: first_name is required for newly registered accounts
        (first-name (or (and registered-p "")
                        (read-from-minibuffer "First Name: "))))
    (telega-server--send
     (list :@type "checkAuthenticationCode"
           :code code
           :first_name first-name
           :last_name ""))))

(defun telega--checkAuthenticationPassword (auth-state &optional password)
  "Check the PASSWORD for the 2-factor authentification.
AUTH-STATE is TDLib state taken from `updateAuthorizationState' event."
  (let* ((hint (plist-get auth-state :password_hint))
         (pswd (or password
                   (password-read
                    (concat "Telegram password"
                            (if (string-empty-p hint)
                                ""
                              (format "(hint='%s')" hint))
                            ": ")))))
    (telega-server--send
     (list :@type "checkAuthenticationPassword"
           :password pswd))))

(defun telega--setOption (prop-kw val)
  "Set option, defined by keyword PROP-KW to VAL."
  (declare (indent 1))
  (telega-server--send
   (list :@type "setOption"
         :name (substring (symbol-name prop-kw) 1) ; strip `:'
         :value (list :@type (cond ((memq val '(t nil :false))
                                    "optionValueBoolean")
                                   ((integerp val)
                                    "optionValueInteger")
                                   ((stringp val)
                                    "optionValueString")
                                   (t (error "Unknown value type: %S"
                                             (type-of val))))
                      :value (or val :false)))))

(defun telega--setOptions (options-plist)
  "Send custom OPTIONS-PLIST to server."
  (cl-loop for (prop-name value) on options-plist
           by 'cddr
           do (telega--setOption prop-name value)))

(defun telega--authorization-ready ()
  "Called when tdlib is ready to receive queries."
  ;; Validate tdlib version
  (when (string< (plist-get telega--options :version)
                 telega-tdlib-min-version)
    (error (concat "TDLib version=%s < %s (min required), "
                   "please upgrade TDLib and recompile `telega-server'")
           (plist-get telega--options :version)
           telega-tdlib-min-version))

  (setq telega--me-id (plist-get telega--options :my_id))
  (cl-assert telega--me-id)
  (telega--setOptions telega-options-plist)
  ;; In case language pack id has not yet been selected, then select
  ;; suggested one or fallback to "en"
  (unless (plist-get telega--options :language_pack_id)
    (telega--setOption :language_pack_id
      (or (plist-get telega--options :suggested_language_pack_id) "en")))

  ;; Apply&update notifications settings
  (when (car telega-notifications-defaults)
    (telega--setScopeNotificationSettings
     "notificationSettingsScopePrivateChats"
     (car telega-notifications-defaults)))
  (when (cdr telega-notifications-defaults)
    (telega--setScopeNotificationSettings
     "notificationSettingsScopeGroupChats"
     (cdr telega-notifications-defaults)))
  ;; NOTE: telega--scope-notification-alist will be updated uppon
  ;; `updateScopeNotificationSettings' event

  ;; All OK, request for chats/users/etc
  (telega-status--set nil "Fetching chats...")
  (telega--getChats)

  (run-hooks 'telega-ready-hook))

(defun telega--on-updateConnectionState (event)
  "Update telega connection state using EVENT."
  (let* ((conn-state (telega--tl-get event :state :@type))
         (status (substring conn-state 15)))
    (setq telega--conn-state (intern status))
    (telega-status--set status)

    ;; NOTE: Optimisation: for Updating state, inhibit redisplaying
    ;; filters, will speedup updating after TDLib wake up
    (cl-case telega--conn-state
      (connectionStateUpdating
       (setq telega-filters--inhibit-redisplay t))
      (connectionStateReady
       (setq telega-filters--inhibit-redisplay nil)
       (telega-filters--redisplay)))

    (run-hooks 'telega-connection-state-hook)))

(defun telega--on-updateOption (event)
  "Proceed with option update from telega server using EVENT."
  (setq telega--options
        (plist-put telega--options
                   (intern (concat ":" (plist-get event :name)))
                   (plist-get (plist-get event :value) :value))))

(defun telega--on-updateAuthorizationState (event)
  "Proceed with user authorization state change using EVENT."
  (let* ((state (plist-get event :authorization_state))
         (stype (plist-get state :@type)))
    (setq telega--auth-state (substring stype 18))
    (telega-status--set (concat "Auth " telega--auth-state))
    (cl-ecase (intern stype)
      (authorizationStateWaitTdlibParameters
       (telega--setTdlibParameters))

      (authorizationStateWaitEncryptionKey
       (telega--checkDatabaseEncryptionKey))

      (authorizationStateWaitPhoneNumber
       (telega--setAuthenticationPhoneNumber))

      (authorizationStateWaitCode
       (telega--checkAuthenticationCode (plist-get state :is_registered)))

      (authorizationStateWaitPassword
       (telega--checkAuthenticationPassword state))

      (authorizationStateReady
       ;; TDLib is now ready to answer queries
       (telega--authorization-ready))

      (authorizationStateLoggingOut
       )

      (authorizationStateClosing
       )

      (authorizationStateClosed
       (telega-server-kill)))))

(defun telega--on-ok (_event)
  "On ok result from command function call."
  ;; no-op
  )

(defun telega-version (&optional print-p)
  "Return telega (and TDLib) version.
If prefix arg PRINT-P is non-nil, then print version into echo
area."
  (interactive "p")
  (let* ((tdlib-version (plist-get telega--options :version))
         (version (concat "telega v"
                          telega-version
                          " ("
                          (if tdlib-version
                              (concat "TDLib version " tdlib-version)
                            "TDLib version unknown, server not running")
                          ")")))
    (if print-p
        (message version)
      version)))


(provide 'telega)

;; Load hook might install new symbols into
;; `telega-symbol-widths'
(run-hooks 'telega-load-hook)
(telega-symbol-widths-install telega-symbol-widths)

;;; telega.el ends here

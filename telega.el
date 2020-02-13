;;; telega.el --- Telegram client (unofficial)  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2019 by Zajcev Evgeny

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Nov 30 19:04:26 2016
;; Keywords: comm
;; Package-Requires: ((emacs "26.1") (visual-fill-column "1.9"))
;; URL: https://github.com/zevlg/telega.el
;; Version: 0.6.0
(defconst telega-version "0.6.0")
(defconst telega-server-min-version "0.5.0")
(defconst telega-tdlib-min-version "1.5.4")

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
(require 'find-func)                    ; `find-library-name'
(require 'emacsbug)                     ; `report-emacs-bug--os-description'

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
(require 'telega-i18n)

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
  (ignore-errors
    (mkdir telega-ton-keystore-dir))
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

    (telega-server--check-version telega-server-min-version)
    (telega-server--start)
    (telega-i18n-init))

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

(defun telega-resend-auth-code ()
  "Resend auth code.
Works only if current state is `authorizationStateWaitCode'."
  (interactive)
  (telega-server--send
   (list :@type "resendAuthenticationCode")))

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

  ;; Do not update filters on every chat fetched, update them at the end
  (setq telega-filters--inhibit-redisplay t)
  (telega--getChats "Main" 'telega-chat--on-getChats)
  ;; Also fetch chats from Archive
  ;; NOTE: We hope `telega--getChats' will return all chats in the
  ;; Archive, in general this is not true, we need special callback to
  ;; continue fetching, as with "Main" list
  (telega--getChats "Archive" 'ignore)

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
       (let ((phone (read-string "Telega phone number: " "+")))
         (telega--setAuthenticationPhoneNumber phone)))

      (authorizationStateWaitCode
       (let ((code (read-string "Telega login code: ")))
         (telega--checkAuthenticationCode code)))

      (authorizationStateWaitRegistration
       (let* ((names (split-string (read-from-minibuffer "Your Name: ") " "))
              (first-name (car names))
              (last-name (mapconcat 'identity (cdr names) " ")))
         (telega--registerUser first-name last-name)))

      (authorizationStateWaitPassword
       (let* ((hint (plist-get state :password_hint))
              (pass (password-read
                     (concat "Telegram password"
                             (if (string-empty-p hint)
                                 ""
                               (format "(hint='%s')" hint))
                             ": "))))
         (telega--checkAuthenticationPassword pass)))


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

(defun telega--on-updateServiceNotification (event)
  "Handle service notification EVENT from the server."
  (let ((help-window-select t))
    (with-telega-help-win "*Telega Service Notification*"
      ;; NOTE: use `telega-ins--content' in hope that only `:content'
      ;; property is used
      (telega-ins--with-attrs (list :fill 'center
                                    :fill-column telega-chat-fill-column)
        (telega-ins--content event))
      (when (string-prefix-p "AUTH_KEY_DROP_" (plist-get event :type))
        (telega-ins "\n")
        (telega-ins--button "Cancel"
          'action (lambda (_ignored)
                    (quit-window)))
        (telega-ins " ")
        (telega-ins--button "Logout"
          'action (lambda (_ignored)
                    (when (yes-or-no-p "Destroy all local data? ")
                      (telega-server--send (list :@type "destroy")))))))))

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

;;;###autoload
(defun telega-report-bug ()
  "Create bug report for https://github.com/zevlg/telega.el/issues."
  (interactive)
  
  (let ((help-window-select t))
    (with-telega-help-win "*Telega Bug Report*"
      (insert "<!--- Provide a general summary of the issue in the Title above -->"
              "\n\n")

      (insert "## Telega Setup\n")
      (insert "*OS*: " (or (ignore-errors (report-emacs-bug--os-description))
                           "unknown")
              "\n")
      (insert "*Emacs*: " emacs-version " (" system-configuration ")" "\n")
      (insert "*Telega*: " (telega-version) "\n")
      (when-let ((melpa-pkg (ignore-errors
                              (read (find-file-noselect
                                     (find-library-name "telega-pkg"))))))
        (insert "*MELPA*: " (caddr melpa-pkg) "\n"))
      (insert "\n")

      (insert "## Current Behavior\n")
      (insert "<!--- Tell us what happens instead of the expected behavior. -->\n")
      (insert "\n")

      (insert "## Steps to Reproduce\n")
      (insert "<!--- Provide an unambiguous set of steps to reproduce this issue. -->\n")
      (insert "<!--- Include code to reproduce, if relevant. -->\n")
      (insert "1.\n")
      (insert "2.\n")
      (insert "3.\n")
      (insert "\n")

      (insert "## Possible Solution\n")
      (insert "<!--- Not obligatory, but suggest a fix/reason for the issue. -->\n")
      (insert "<!--- Delete this section if you have no idea. -->\n"))))

(provide 'telega)

;; Load hook might install new symbols into
;; `telega-symbol-widths'
(run-hooks 'telega-load-hook)
(telega-symbol-widths-install telega-symbol-widths)
(require 'telega-obsolete)

;;; telega.el ends here

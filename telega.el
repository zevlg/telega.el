;;; telega.el --- Telegram client (unofficial)  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2020 by Zajcev Evgeny
;; Copyright (C) 2019-2020 by Brett Gilio

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Nov 30 19:04:26 2016
;; Keywords: comm
;; Package-Requires: ((emacs "26.1") (visual-fill-column "1.9") (rainbow-identifiers "0.2.2"))
;; URL: https://github.com/zevlg/telega.el
;; Version: 0.6.31
(defconst telega-version "0.6.31")
(defconst telega-server-min-version "0.6.6")
(defconst telega-tdlib-min-version "1.6.9")
(defconst telega-tdlib-max-version nil)

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
(require 'telega-folders)
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
(require 'telega-tdlib)
(require 'telega-tdlib-events)

(defconst telega-app '(72239 . "bbf972f94cc6f0ee5da969d8d42a6c76"))

;;;###autoload
(defvar telega-prefix-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    ;;; ellit-org: prefix-map-bindings
    ;; - {{{where-is(telega,telega-prefix-map)}}} ::
    ;;   {{{fundoc(telega, 2)}}}
    (define-key map (kbd "t") 'telega)
    ;;; ellit-org: prefix-map-bindings
    ;; - {{{where-is(telega-chat-with,telega-prefix-map)}}} ::
    ;;   {{{fundoc(telega-chat-with, 2)}}}
    (define-key map (kbd "c") 'telega-chat-with)
    ;;; ellit-org: prefix-map-bindings
    ;; - {{{where-is(telega-saved-messages,telega-prefix-map)}}} ::
    ;;   {{{fundoc(telega-saved-messages, 2)}}}
    (define-key map (kbd "s") 'telega-saved-messages)
    ;;; ellit-org: prefix-map-bindings
    ;; - {{{where-is(telega-switch-buffer,telega-prefix-map)}}} ::
    ;;   {{{fundoc(telega-switch-buffer, 2)}}}
    (define-key map (kbd "b") 'telega-switch-buffer)
    ;;; ellit-org: prefix-map-bindings
    ;; - {{{where-is(telega-buffer-file-send,telega-prefix-map)}}} ::
    ;;   {{{fundoc(telega-buffer-file-send, 2)}}}
    (define-key map (kbd "f") 'telega-buffer-file-send)
    ;;; ellit-org: prefix-map-bindings
    ;; - {{{where-is(telega-browse-url,telega-prefix-map)}}} ::
    ;;   {{{fundoc(telega-browse-url, 2)}}}
    (define-key map (kbd "w") 'telega-browse-url)
    ;;; ellit-org: prefix-map-bindings
    ;; - {{{where-is(telega-account-switch,telega-prefix-map)}}} ::
    ;;   {{{fundoc(telega-account-switch, 2)}}}
    (define-key map (kbd "a") 'telega-account-switch)
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

(defun telega-account-current ()
  "Return current account."
  (cl-find-if #'telega-account--current-p telega-accounts))

(defun telega-account--current-p (account)
  "Return non-nil if the ACCOUNT is current."
  (equal (plist-get (cdr account) 'telega-database-dir)
         telega-database-dir))

(defun telega-account-switch (account-name)
  "Switch to the ACCOUNT-NAME."
  (interactive
   (list (if (not telega-accounts)
             (user-error "telega: Single account setup, see `telega-accounts'")
           (funcall telega-completing-read-function
                    "Telegram Account: "
                    (mapcar #'car
                            (cl-remove-if #'telega-account--current-p
                                          telega-accounts))
                    nil 'require-match))))

  (let ((account (assoc account-name telega-accounts)))
    (cl-assert account)
    (unless (telega-account--current-p account)
      (setq account (cdr account))
      (while account
        (set (car account) (cadr account))
        (setq account (cddr account)))

      (telega-server-kill)
      ;; Wait for server to die
      (while (telega-server-live-p)
        (sit-for 0.1)))

    (telega nil)))

;;;###autoload
(defun telega (&optional arg)
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
  (let* ((chat-count (length telega--chat-buffers-alist))
         (suffix (cond ((eq chat-count 0) "")
                       ((eq chat-count 1) (format " (and 1 chat buffer)"))
                       (t (format " (and all %d chat buffers)" chat-count)))))
    (when (or force (y-or-n-p (concat "Kill telega" suffix "? ")))
      (kill-buffer telega-root-buffer-name))))

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
    (warn (concat "TDLib version=%s < %s (min required), "
                  "please upgrade TDLib and recompile `telega-server'")
          (plist-get telega--options :version)
          telega-tdlib-min-version))
  (when (and telega-tdlib-max-version
             (string< telega-tdlib-max-version
                      (plist-get telega--options :version)))
    (warn (concat "TDLib version=%s > %s (max required), "
                  "please downgrade TDLib and recompile `telega-server'")
          (plist-get telega--options :version)
          telega-tdlib-max-version))

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
  ;; NOTE: telega--scope-notification-alist will be updated upon
  ;; `updateScopeNotificationSettings' event

  ;; All OK, request for chats/users/etc
  (telega-status--set nil "Fetching chats...")

  (telega--getChats nil (list :@type "chatListMain")
    #'telega--on-initial-chats-fetch)
  ;; NOTE: We hope `telega--getChats' will return all chats in the
  ;; Archive, in general this is not true, we need special callback to
  ;; continue fetching, as with "chatListMain" list
  (telega--getChats nil (list :@type "chatListArchive")
    #'ignore)

  (run-hooks 'telega-ready-hook))

;;;###autoload
(defun telega-version (&optional insert-p)
  "Return telega (and TDLib) version.
If `\\[universal-argument] is specified, then insert the version
string at point."
  (interactive "P")
  (let* ((tdlib-version (plist-get telega--options :version))
         (version (concat "telega v"
                          telega-version
                          " ("
                          (if tdlib-version
                              (concat "TDLib v" tdlib-version)
                            "TDLib version unknown. Make sure server is running")
                          ")"
                          " (telega-server v"
                          (telega-server-version)
                          ")")))
    (if insert-p
        (insert version)
      (if (called-interactively-p 'interactive)
          (message "%s" version)
        version))))

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
      (insert "*Emacs*: " (let (emacs-build-time) (emacs-version)) "\n")
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

(push (expand-file-name "contrib" telega--lib-directory) load-path)

;; Load hook might install new symbols into
;; `telega-symbol-widths'
(run-hooks 'telega-load-hook)
(telega-symbol-widths-install telega-symbol-widths)
(require 'telega-obsolete)

;;; telega.el ends here

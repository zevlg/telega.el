;;; telega.el --- Telegram client (unofficial)  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2023 by Zajcev Evgeny
;; Copyright (C) 2019-2020 by Brett Gilio

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Nov 30 19:04:26 2016
;; Keywords: comm
;; Package-Requires: ((emacs "27.1") (visual-fill-column "1.9") (rainbow-identifiers "0.2.2"))
;; URL: https://github.com/zevlg/telega.el
;; Version: 0.8.214
(defconst telega-version "0.8.214")
(defconst telega-server-min-version "0.7.7")
(defconst telega-tdlib-min-version "1.8.21")
(defconst telega-tdlib-max-version nil)

(defconst telega-tdlib-releases '("1.8.0" . "1.9.0")
  "Cons cell with current and next TDLib releases.
Used for manual generation.")

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
(require 'telega-match)
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
(require 'telega-story)
(require 'telega-tdlib)
(require 'telega-tdlib-events)

;; Emacs26 compat
(eval-when-compile
  (unless (fboundp 'report-emacs-bug--os-description)
    (defun report-emacs-bug--os-description ()
      "unknown")))

(defconst telega-app '(72239 . "bbf972f94cc6f0ee5da969d8d42a6c76"))

;;;###autoload
(defvar telega-prefix-map
  (let ((map (make-sparse-keymap)))
    ;;; ellit-org: prefix-map-bindings
    ;; - {{{where-is(telega-account-switch,telega-prefix-map)}}} ::
    ;;   {{{fundoc(telega-account-switch, 2)}}}
    (define-key map (kbd "a") 'telega-account-switch)
    ;;; ellit-org: prefix-map-bindings
    ;; - {{{where-is(telega-switch-buffer,telega-prefix-map)}}} ::
    ;;   {{{fundoc(telega-switch-buffer, 2)}}}
    (define-key map (kbd "b") 'telega-switch-buffer)
    ;;; ellit-org: prefix-map-bindings
    ;; - {{{where-is(telega-chat-with,telega-prefix-map)}}} ::
    ;;   {{{fundoc(telega-chat-with, 2)}}}
    (define-key map (kbd "c") 'telega-chat-with)
    ;;; ellit-org: prefix-map-bindings
    ;; - {{{where-is(telega-edit-file-switch-buffer,telega-prefix-map)}}} ::
    ;;   {{{fundoc(telega-edit-file-switch-buffer, 2)}}}
    (define-key map (kbd "e") 'telega-edit-file-switch-buffer)
    ;;; ellit-org: prefix-map-bindings
    ;; - {{{where-is(telega-switch-important-chat,telega-prefix-map)}}} ::
    ;;   {{{fundoc(telega-switch-important-chat, 2)}}}
    (define-key map (kbd "i") 'telega-switch-important-chat)
    ;;; ellit-org: prefix-map-bindings
    ;; - {{{where-is(telega-buffer-file-send,telega-prefix-map)}}} ::
    ;;   {{{fundoc(telega-buffer-file-send, 2)}}}
    (define-key map (kbd "f") 'telega-buffer-file-send)
    ;;; ellit-org: prefix-map-bindings
    ;; - {{{where-is(telega-saved-messages,telega-prefix-map)}}} ::
    ;;   {{{fundoc(telega-saved-messages, 2)}}}
    (define-key map (kbd "s") 'telega-saved-messages)
    ;;; ellit-org: prefix-map-bindings
    ;; - {{{where-is(telega,telega-prefix-map)}}} ::
    ;;   {{{fundoc(telega, 2)}}}
    (define-key map (kbd "t") 'telega)
    ;;; ellit-org: prefix-map-bindings
    ;; - {{{where-is(telega-switch-unread-chat,telega-prefix-map)}}} ::
    ;;   {{{fundoc(telega-switch-unread-chat, 2)}}}
    ;;
    ;;   Customizable options:
    ;;   - {{{user-option(telega-unread-chat-temex, 4)}}}
    (define-key map (kbd "u") 'telega-switch-unread-chat)
    ;;; ellit-org: prefix-map-bindings
    ;; - {{{where-is(telega-browse-url,telega-prefix-map)}}} ::
    ;;   {{{fundoc(telega-browse-url, 2)}}}
    (define-key map (kbd "w") 'telega-browse-url)
    map)
  "Keymap for the telega commands.")

(defun telega--create-hier ()
  "Ensure directory hier is valid."
  (mkdir telega-directory t)
  (mkdir telega-cache-dir t)
  (mkdir telega-temp-dir t)

  ;; NOTE: make sure directory for `telega-server-logfile' exists
  ;; See https://github.com/zevlg/telega.el/issues/307
  ;; `telega-server-logfile' can be nil, see https://t.me/emacs_telega/30754
  (when-let ((logfile-dir (when telega-server-logfile
                            (file-name-directory telega-server-logfile))))
    (unless (file-exists-p logfile-dir)
      (error "telega: directory \"%s\" does not exists, \
can't write to `telega-server-logfile'" logfile-dir)))
  )

(defun telega-account--current-p (account)
  "Return non-nil if the ACCOUNT is current."
  (equal (plist-get (cdr account) 'telega-database-dir)
         telega-database-dir))

(defun telega-account-current ()
  "Return current account."
  (cl-find-if #'telega-account--current-p telega-accounts))

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
      ;; Set account's variables
      (telega--tl-dolist ((var-name var-value) (cdr account))
        (set var-name var-value))
      ;; After setting all the variables of the account it must
      ;; become current
      (unless (telega-account--current-p account)
        (user-error "telega: Invalid config for \"%s\" account: at least 'telega-database-dir variable must be provided"))

      (telega-server-kill)
      ;; Wait for server to die
      (while (telega-server-live-p)
        (sit-for 0.1)))

    (telega nil)))

(defun telega-set-network-type (network-type)
  "Interactively change network type.
Can be called even if telega is not running.
Modifies `telega-network-type' by side-effect."
  (interactive
   (let* ((choices (mapcar (lambda (nt)
                             (list (substring (plist-get (cdr nt) :@type) 11)
                                   (car nt)))
                           telega-tdlib-network-type-alist))
          (choice (funcall telega-completing-read-function
                           "Set Network Type: " choices nil t))
          (network-type (car (alist-get choice choices nil nil 'string=))))
     (list network-type)))

  (setq telega-network-type network-type)
  (when (telega-server-live-p)
    (let ((tdlib-network-type
           (alist-get network-type telega-tdlib-network-type-alist)))
      (telega--setNetworkType tdlib-network-type))))

;;;###autoload
(defun telega (&optional arg)
  "Start telega.el Telegram client.
Pop to root buffer.
If `\\[universal-argument]' is specified, then do not pop to root buffer."
  (interactive "P")

  ;; For multiple accounts setup possibly select (if there is no
  ;; default account declared) an account to use
  (if (and telega-accounts (not (telega-account-current)))
      (call-interactively #'telega-account-switch)

    (unless (telega-server-live-p)
      ;; NOTE: for telega-server restarts also recreate root buffer,
      ;; killing root buffer also cleanup all chat buffers and stops any
      ;; timers used for animation
      (when (buffer-live-p (telega-root--buffer))
        (kill-buffer (telega-root--buffer)))

      (telega--create-hier)
      (telega--init-vars)
      ;; NOTE: rootbuf might use i18n strings, so initialize it with
      ;; default strings before entering rootbuf
      (let ((telega-language "en"))
        (telega-i18n-init))
      (with-current-buffer (get-buffer-create telega-root-buffer-name)
        (telega-root-mode))

      (telega-server--ensure-build)
      (telega-server--start)
      (telega-i18n-init)

      ;; For telega-auto-download-mode and network statistics
      (unless (eq 'other telega-network-type)
        (telega-set-network-type telega-network-type))
      )

    (unless arg
      (pop-to-buffer-same-window telega-root-buffer-name))))

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
  (telega--tl-dolist ((prop-name value) telega-options-plist)
    (telega--setOption prop-name value))

  ;; In case language pack id has not yet been selected, then select
  ;; suggested one or fallback to "en"
  (unless (plist-get telega--options :language_pack_id)
    (telega--setOption :language_pack_id
      (or (plist-get telega--options :suggested_language_pack_id) "en")))

  ;; Apply&update notifications settings
  (dolist (scope-type telega-notification-scope-types)
    (when-let ((settings
                (alist-get (car scope-type) telega-notifications-defaults)))
      (apply #'telega--setScopeNotificationSettings (cdr scope-type) settings)))

  ;; Fetch blocked users for all block lists
  (telega--getBlockedMessageSenders 'blockListMain 0
    #'telega--on-blocked-senders-load)
  (telega--getBlockedMessageSenders 'blockListStories 0
    #'telega--on-blocked-senders-load)

  ;; NOTE: telega--scope-notification-alist will be updated upon
  ;; `updateScopeNotificationSettings' event

  ;; All OK, request for chats/users/etc
  (telega-status--set nil "Loading chats...")

  (telega--loadChats (list :@type "chatListMain")
    #'telega--on-initial-chats-load)
  ;; NOTE: We hope `telega--getChats' will return all chats in the
  ;; Archive, in general this is not true, we need special callback to
  ;; continue fetching, as with "chatListMain" list
  (telega--loadChats (list :@type "chatListArchive"))

  (run-hooks 'telega-ready-hook))

;;;###autoload
(defun telega-version (&optional insert-p)
  "Return telega (and TDLib) version.
If `\\[universal-argument]' is specified, then insert the version
string at point."
  (interactive "P")
  (let* ((tdlib-version (plist-get telega--options :version))
         (tdlib-sha1 (or (plist-get telega--options :commit_hash)
                         "unknown"))
         (version (concat "telega v"
                          telega-version
                          " ("
                          (if tdlib-version
                              (concat "TDLib v" tdlib-version "-"
                                      (substring tdlib-sha1 0 7))
                            "TDLib version unknown. Make sure server is running")
                          ")"
                          " (telega-server v"
                          (telega-server-version)
                          (when telega-use-docker
                            (format " [%s]" (if (stringp telega-use-docker)
                                                telega-use-docker
                                              "docker")))
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
      (telega-ins "*Features*: "
                  (when (image-type-available-p 'imagemagick)
                    "imagemagick ")
                  (when (image-type-available-p 'svg)
                    "svg ")
                  (when (image-type-available-p 'webp)
                    "webp ")
                  (when (executable-find "ffmpeg")
                    "ffmpeg ")
                  (when (executable-find "tgs2png")
                    "tgs2png ")
                  "\n")
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


;;; Emacs runtime environment for telega
;;
;; TODO: move runtime env stuff and timer functions here from
;; telega-root.el

(provide 'telega)


(push (expand-file-name "contrib" telega--lib-directory) load-path)

;; Enable some global minor modes by default
(telega-patrons-mode 1)
(telega-active-locations-mode 1)
(telega-active-video-chats-mode 1)
(telega-active-stories-mode 1)

;; Enable root auto fill mode by default
(telega-root-auto-fill-mode 1)

;; For messages loaded from history
(add-hook 'telega-chatbuf-pre-msg-insert-hook #'telega-msg-run-ignore-predicates)
(add-hook 'telega-chatbuf-pre-msg-insert-hook #'telega-msg--replied-message-fetch)
(add-hook 'telega-chatbuf-pre-msg-insert-hook #'telega-msg--replied-story-fetch)
(add-hook 'telega-chatbuf-pre-msg-insert-hook #'telega-msg--story-fetch)
(add-hook 'telega-chatbuf-pre-msg-insert-hook #'telega-msg--custom-emojis-fetch)
(add-hook 'telega-chatbuf-post-msg-update-hook #'telega-msg--custom-emojis-fetch)

;; WARN about usage of the obsolete variables
(require 'telega-obsolete)

(run-hooks 'telega-load-hook)

;;; telega.el ends here

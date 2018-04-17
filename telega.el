;;; telega.el --- Telegram client

;; Copyright (C) 2016-2018 by Zajcev Evgeny

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Nov 30 19:04:26 2016
;; Keywords:
;; Version: 0.1.1

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

;; Some code taken from circe


;;; Code:
(require 'telega-root)

(defconst telega-app '(72239 . "bbf972f94cc6f0ee5da969d8d42a6c76"))
(defconst telega-version "0.1.1")


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

(defcustom telega-use-test-dc nil
  "*Non-nil to use telegram's test environment instead of production."
  :type 'bool
  :group 'telega)

(defcustom telega-language "en"
  "*IETF language tag of the user's language."
  :type 'string
  :group 'telega)

(defcustom telega-rsa-key-file (expand-file-name "server.pub" telega-directory)
  "*RSA key to use."
  :type 'string
  :group 'telega)

(defcustom telega-server-command "telega-server"
  "Command to run as telega server."
  :type 'string
  :group 'telegram)

(defcustom telega-server-logfile (expand-file-name "telega-server.log" telega-directory)
  "*Write server logs to this file."
  :type 'string
  :group 'telega)

(defcustom telega-server-verbosity 5
  "*Verbosity level for server process."
  :type 'number
  :group 'telega)

(defcustom telega-tracking '(chats contacts)
  "*Buffers to track activity on."
  :type '(repeat (choice (const :tag "Chats" chats)
                         (const :tag "Contacts" contacts)
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

;;; Utility functions

(defun telega--create-hier ()
  "Ensure directory hier is valid."
  (ignore-errors
    (mkdir telega-directory))
  (ignore-errors
    (mkdir telega-cache-dir)))

(define-derived-mode telega-mode lui-mode "Telega"
  "Base mode for all telega buffers.

A buffer should never be in this mode directly, but rather in
modes that derive from this.

The mode inheritance hierarchy looks like this:

lui-mode
`--telega-mode
   `--telega-chat-mode
   `--telega-channel-mode"
  (add-hook 'lui-pre-output-hook 'lui-irc-colors t t)
  (add-hook 'lui-pre-output-hook 'telega--pre-output t t)
  (add-hook 'completion-at-point-functions
            'telega--completion-at-point nil t)
  (lui-set-prompt telega-prompt-string)
  (goto-char (point-max))
  (setq lui-input-function 'telega--input
        default-directory (expand-file-name telega-directory))
  ;; Tab completion should be case-insensitive
  (set (make-local-variable 'completion-ignore-case) t)
  (set (make-local-variable 'tracking-faces-priorities)
       telega-track-faces-priorities))

(define-derived-mode telega-root-mode nil "Telega-Root"
  "The mode for telega root buffer.

\\{telega-root-mode-map}"
  (add-hook 'kill-buffer-hook 'telega-root-killed nil t))

(define-derived-mode telega-chat-mode telega-mode "Telega-Chat"
  "The mode for telega chat buffers.

\\{telega-chat-mode-map}"
  (add-hook 'kill-buffer-hook 'telega-chat-killed nil t))

(defun telega ()
  "Start telegramming."
  (interactive)
  (telega--create-hier)
  (with-current-buffer (get-buffer-create telega-root-buffer-name)
    (telega-root-mode)
    (telega-server--start))
  (pop-to-buffer-same-window telega-root-buffer-name))

;;;###autoload
(defun telega-switch-account ()
  "Switch to another telegram account."
  (interactive)
  ;; TODO
  )


;;; telega-server stuff
(defmacro telega--tl-type (tl-obj)
  `(intern (plist-get ,tl-obj :@type)))
(defsetf telega--tl-type (tl-obj) (type-sym)
  `(plist-put ,tl-obj :@type (symbol-name ',type-sym)))

(defvar telega-server--bin nil)
(defvar telega-server--buffer nil)

(defun telega-server--find-bin ()
  "Find telega-server executable.
Raise error if not found"
  (or (executable-find "telega-server")
      (let ((exec-path telega-directory))
        (executable-find "telega-server"))
      (error "telega-server not found in exec-path")))

(defun telega-server--sentinel (proc event)
  "Sentinel for the telega-server process."
  (message "telega-server: %s" event))

(defun telega--on-updateAuthorizationState (event)
  (let ((state (plist-get event :authorization_state)))
    (message "state: %S" (stringp (plist-get state :@type)))
    (ecase (telega--tl-type state)
      (authorizationStateWaitTdlibParameters
       (telega-root--state 'connecting)
       (telega--provide-tdlib-params))

      (authorizationStateClosed
       (telega-root--state 'closed))
      )))

(defun telega--on-event (event)
  (message "Telega event: %s" event)
  (let ((event-sym (intern (format "telega--on-%s" (plist-get event :@type)))))
    (if (symbol-function event-sym)
        (funcall (symbol-function event-sym) event)
      (message "Telega TODO: define `%S'" event-sym)
      )))

(defun telega--on-error (err)
  (message "Telega error: %s" err)
  )

(defun telega-server--parse-events ()
  "Parse events from telega-server."
  (while (progn
           (goto-char (point-min))
           (re-search-forward "^\\([a-z]+\\) \\([0-9]+\\)\n" nil t))
    (let ((cmd (match-string 1))
          (jsonsz (string-to-number (match-string 2))))
      (when (> (- (point-max) (point)) jsonsz)
        (let* ((json-object-type 'plist)
               (value (json-read)))
          (unwind-protect
              (cond ((string= cmd "event")
                     (telega--on-event value))
                    ((string= cmd "error")
                     (telega--on-error value))
                    (t (error "Unknown cmd from telega-server: %s" cmd)))
            (delete-region (point-min) (point))))))))

(defun telega-server--filter (proc output)
  "Filter for the telega-server process."
  (let ((buffer (process-buffer proc)))
    (message "telega (live=%s) output: %s" (buffer-live-p buffer) output)
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert output)
          (telega-server--parse-events))

      ;; telega-server buffer is killed, but telega-server process
      ;; still sends us some events
      (with-temp-buffer
        (insert output)
        (telega-server--parse-events)))))

(defun telega-server--send (sexp &optional exec-p)
  "Compose SEXP to json and send to telega-server.
If EXEC-P is non-nil then send `exec' command instead of default `send'."
  (let* ((json-object-type 'plist)
         (value (json-encode sexp))
         (proc (get-buffer-process telega-server--buffer)))
    (process-send-string
     proc
     (format "%s %d\n" (or (and exec-p "exec") "send") (length value)))
    (process-send-string
     proc
     sexp)
    (process-send-string
     proc
     "\n")))

(defun telega-server--start ()
  "Start telega-server process."
  (unless telega-server--bin
    (setq telega-server--bin (telega-server--find-bin)))
  (let ((process-connection-type nil)
        (process-adaptive-read-buffering nil)
        proc)
    (with-current-buffer (generate-new-buffer " *telega-server*")
      (setq telega-server--buffer (current-buffer))
      (let ((proc (start-process
                   "telega-server"
                   (current-buffer)
                   telega-server--bin
                   "-v" (int-to-string telega-server-verbosity)
                   "-l" telega-server-logfile)))
        (set-process-query-on-exit-flag proc nil)
        (set-process-sentinel proc #'telega-server--sentinel)
        (set-process-filter proc #'telega-server--filter))))
  (current-buffer))

;;;###autoload
(defun telega-server-kill ()
  "Kill the telega-server process."
  (interactive)
  (when (buffer-live-p telega-server--buffer)
    (kill-buffer telega-server--buffer)))

;;; Highlighting output
(defun telega--pre-output ()
  "Highlight usernames started with @.
This is used in `lui-pre-output-hook'."
  )

;;; Completion
(defun telega--completion-at-point ()
  "Return a list of possible completions for the current buffer.
This is used in `completion-at-point-functions'."
  ;; Use markers so they move when input happens
  ;; TODO
  )


(provide 'telega)

;;; telega.el ends here

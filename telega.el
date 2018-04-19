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
(require 'telega-root)
(require 'telega-notifications)


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

(defcustom telega-server-command "telega-server"
  "Command to run as telega server."
  :type 'string
  :group 'telega-server)

(defcustom telega-server-logfile (expand-file-name "telega-server.log" telega-directory)
  "*Write server logs to this file."
  :type 'string
  :group 'telega-server)

(defcustom telega-server-verbosity 5
  "*Verbosity level for server process."
  :type 'number
  :group 'telega-server)

(defcustom telega-server-call-timeout 0.5
  "*Timeout for `telega-server--call'."
  :type 'number
  :group 'telega-server)

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

(defsubst telega-debug (fmt &rest args)
  (when telega-debug
    (with-current-buffer (get-buffer-create "*telega-debug*")
      (goto-char (point-max))
      (insert (apply 'format (cons (concat fmt "\n") args))))))


;;; telega-server stuff
(defmacro telega--tl-type (tl-obj)
  `(intern (plist-get ,tl-obj :@type)))
(defsetf telega--tl-type (tl-obj) (type-sym)
  `(plist-put ,tl-obj :@type (symbol-name ',type-sym)))

(defvar telega-server--bin nil)
(defvar telega-server--buffer nil)
(defvar telega-server--extra 0 "Value for :@extra used by `telega-server--call'.")
(defvar telega--options nil "Current options values.")

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
  ;;   consider encryption as todo in futuru
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
  )

(defun telega--on-ok (event)
  "On ok result from command function call."
  )

(defun telega--on-updateConnectionState (event)
  "Update telega connection state."
  (let* ((conn-state (plist-get (plist-get event :state) :@type))
         (root-state (substring conn-state 15)))
    (telega-root--state root-state)))

(defun telega--on-updateOption (event)
  "Proceed with option update from telega server."
  (setq telega--options
        (plist-put telega--options
                   (intern (concat ":" (plist-get event :name)))
                   (plist-get (plist-get event :value) :value))))

(defun telega--on-updateAuthorizationState (event)
  (let ((state (plist-get event :authorization_state)))
    (ecase (telega--tl-type state)
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

      (authorizationStateClosing
       (telega-root--state "Closing.."))

      (authorizationStateClosed
       (telega-root--state "Closed"))
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

(defsubst telega-server--parse-cmd ()
  "Parse single reply from telega-server."
  (when (re-search-forward "^\\([a-z]+\\) \\([0-9]+\\)\n" nil t)
    (let ((cmd (match-string 1))
          (jsonsz (string-to-number (match-string 2))))
      (when (> (- (point-max) (point)) jsonsz)
        (let* ((json-object-type 'plist)
               (value (json-read)))
          (prog1
              (list cmd value)
            (delete-region (point-min) (point))))))))

(defsubst telega-server--dispatch-cmd (cmd value)
  "Dispatch command CMD."
  (declare (special telega-server--extra-value))
  (cond ((string= cmd "event")
         (if (and (boundp 'telega-server--extra-value)
                  (eq (plist-get value :@extra) telega-server--extra))
             (setq telega-server--extra-value value)
           (telega--on-event value)))
        ((string= cmd "error")
         (telega--on-error value))
        (t (error "Unknown cmd from telega-server: %s" cmd))))

(defun telega-server--parse-commands ()
  "Parse all available events from telega-server."
  (goto-char (point-min))
  (let (cmd-val)
    (while (setq cmd-val (telega-server--parse-cmd))
      (apply 'telega-server--dispatch-cmd cmd-val))))

(defun telega-server--filter (proc output)
  "Filter for the telega-server process."
  (let ((buffer (process-buffer proc)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert output)
          (telega-server--parse-commands))

      ;; telega-server buffer is killed, but telega-server process
      ;; still sends us some events
      (with-temp-buffer
        (insert output)
        (telega-server--parse-commands)))))

(defun telega-server--send (sexp)
  "Compose SEXP to json and send to telega-server."
  (let* ((json-object-type 'plist)
         (value (json-encode sexp))
         (proc (get-buffer-process telega-server--buffer)))
    (process-send-string
     proc
     (concat "send " (number-to-string (length value)) "\n"))
    (process-send-string proc value)
    (process-send-string proc "\n")))

(defun telega-server--call (sexp)
  "Same as `telega-server--send', but waits for answer from telega-server."
  (let ((sexp-with-extra (plist-put sexp :@extra (incf telega-server--extra)))
        telega-server--extra-value)
    (telega-server--send sexp-with-extra)
    (accept-process-output
     (get-buffer-process telega-server--buffer) telega-server-call-timeout)
    telega-server--extra-value))

(defun telega-server--start ()
  "Start telega-server process."
  (when (process-live-p (get-buffer-process telega-server--buffer))
    (error "Error: telega-server already running"))

  (when telega-debug
    (with-current-buffer (get-buffer-create "*telega-debug*")
      (erase-buffer)
      (insert (format "%s ---[ telega-server started\n" (current-time-string)))))

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

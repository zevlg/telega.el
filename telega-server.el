;;; telega-server.el --- telega-server functionality  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Apr 20 13:52:34 2018
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

;; Interface to `telega-server' process

;;; Code:
(require 'cl-lib)

(require 'telega-core)
(require 'telega-customize)

(declare-function telega-chats-dirty--update "telega-tdlib-events")

(declare-function telega "telega")
(declare-function telega-root--buffer "telega-root")
(declare-function telega-status--set "telega-root" (conn-status &optional aux-status raw))

(declare-function telega-appindicator--on-event "telega-modes" (event))


(defun telega--on-event (event)
  (let ((event-name (plist-get event :@type)))
    (if (member event-name telega-server--inhibit-events)
        (telega-debug "event %s: %S" (propertize "IGNORED" 'face 'bold)
                      event)

      (let ((event-sym (intern (concat "telega--on-" event-name))))
        (if (symbol-function event-sym)
            (funcall (symbol-function event-sym) event)

          (telega-debug "TODO: define `%S'" event-sym))))))

(defun telega--on-error (err)
  (message "Telega error %s: %s"
           (plist-get err :code) (plist-get err :message)))

;; Server runtime vars
(defvar telega-server--buffer nil)
(defvar telega-server--extra 0 "Value for :@extra used by `telega-server--call'.")
(defvar telega-server--callbacks nil "Callbacks ruled by extra")
(defvar telega-server--results nil)
(defvar telega-server--on-event-func #'telega--on-event
  "Func used to trigger on event.
Used to make deferred calls.")
(defvar telega-server--deferred-events nil)
(defvar telega-server--inhibit-events nil
  "List of events to ignore.
Bind this to avoid processing some events, while executing something.")

(defvar telega-server--idle-timer nil
  "Timer to run `telega-handle-emacs-idle' after some data is received.")
(defvar telega-server-idle-delay 0.1
  "Idle delay to process dirtiness.")

(defun telega--on-deferred-event (event)
  (setq telega-server--deferred-events
        (nconc telega-server--deferred-events (list event))))

(defmacro with-telega-deferred-events (&rest body)
  "Execute BODY deferring telega-server events processing.
Events processing can be deferred only once.
If already deferring, then just executes the BODY."
  (declare (indent 0))
  (let ((evsym (gensym "event")))
    `(if (eq telega-server--on-event-func 'telega--on-deferred-event)
         (progn ,@body)

       (setq telega-server--on-event-func 'telega--on-deferred-event)
       (unwind-protect
           (progn ,@body)

         (unwind-protect
             (while telega-server--deferred-events
               (let ((,evsym (car telega-server--deferred-events)))
                 (telega-debug "%s event: %S"
                               (propertize "DEFERRED" 'face 'bold) ,evsym)
                 (setq telega-server--deferred-events
                       (cdr telega-server--deferred-events))
                 (telega--on-event ,evsym)))

           (setq telega-server--deferred-events nil
                 telega-server--on-event-func 'telega--on-event)
           )))))

(defmacro telega-server--callback-put (extra cb)
  `(puthash ,extra ,cb telega-server--callbacks))

(defmacro telega-server--callback-rm (extra)
  `(remhash ,extra telega-server--callbacks))

(defmacro telega-server--callback-get (extra)
  `(gethash ,extra telega-server--callbacks))

;;;###autoload
(defun telega-server-build (&optional build-flags)
  "Build and install `telega-server' binary.
If BUILD-FLAGS is specified, then rebuild server without any
queries using this flags for building, could be empty string.
Otherwise query user about building flags."
  (interactive)
  (telega-test-env 'quiet)
  (when (or build-flags
            (y-or-n-p "Build `telega-server'? "))
    (let ((default-directory telega--lib-directory))
      (unless build-flags
        (setq build-flags
              (concat
               ;; NOTE: Do not ask about VOIP support, because there
               ;; is no support for it yet
               (when (and nil
                          (y-or-n-p "Build `telega-server' with VOIP support? "))
                 " WITH_VOIP=t")
               ;; NOTE: TON is postponed, see https://t.me/durov/116
               ;; So do not ask for TON support
               )))
      (unless (zerop
               (shell-command
                (concat (or (executable-find "gmake")
                            "make")
                        " " build-flags " "
                        "LIBS_PREFIX=" (expand-file-name telega-server-libs-prefix) " "
                        "INSTALL_PREFIX=" (expand-file-name telega-directory) " "
                        "server-reinstall")))
        (error "`telega-server' installation failed")))))

(defun telega-server--ensure-build ()
  "Make sure telega-server is build and can run."
  (if telega-use-docker
      (or (executable-find "docker")
          (error "`docker' not found in exec-path"))

    (let ((exec-path (cons telega-directory exec-path)))
      (or (if (executable-find telega-server-command)
              (telega-server--check-version)
            (telega-server-build))
          (executable-find telega-server-command)
          (error "`%s' not found in exec-path" telega-server-command)))))

(defun telega-server--process-command (&rest flags)
  "Create command to start `telega-server' progress.
FLAGS - additional.
Raise error if not found."
  (mapconcat #'identity
             (cons
              (if telega-use-docker
                  (telega-docker-run-cmd telega-server-command)
                (let ((exec-path (cons telega-directory exec-path)))
                  (or (executable-find telega-server-command)
                      (error "`%s' not found in exec-path"
                             telega-server-command))))
              flags)
             " "))

(defun telega-server-version ()
  "Return telega-server version."
  (let ((ts-usage (shell-command-to-string
                   (telega-server--process-command "-h"))))
    (when (string-match "^Version \\([0-9.]+\\)" ts-usage)
      (match-string 1 ts-usage))))

(defvar telega-server-min-version)
(defun telega-server--check-version ()
  "Check telega-server version against `telega-server-min-version'.
If does not match, then query user to rebuild telega-server."
  ;; NOTE: do not check version if using dockerized telega-server
  (let ((ts-version (if telega-use-docker
                        telega-server-min-version
                      (or (telega-server-version) "0.0.0-unknown"))))
    (when (and (version< ts-version telega-server-min-version)
               (y-or-n-p
                (format "Installed `telega-server' version %s<%s, rebuild? "
                        ts-version telega-server-min-version)))
      ;; NOTE: remove old telega-server binary before rebuilding
      (let* ((sv-ver (car (split-string
                           (shell-command-to-string
                            (telega-server--process-command "-h"))
                           "\n")))
             (with-voip-p (string-match-p (regexp-quote "with VOIP") sv-ver)))
        (telega-server-build (concat (when with-voip-p " WITH_VOIP=t")
                                     ))))))

(defsubst telega-server--proc ()
  "Return telega-server process."
  (get-buffer-process telega-server--buffer))

(defun telega-server-live-p ()
  "Return non-nil if telega-sever process is alive."
  (process-live-p (telega-server--proc)))

(defsubst telega-server--parse-cmd ()
  "Parse single reply from telega-server.
Return parsed command."
  (when (re-search-forward "^\\([a-z-]+\\) \\([0-9]+\\)\n" nil t)
    (let ((cmd (match-string 1))
          (sexpsz (string-to-number (match-string 2))))
      ;; New command always start at the beginning, no garbage inbetween
      ;; commands
      (unless (= (match-beginning 0) 1)
        ;; Kill the garbage at the beginning
        (telega-debug "!!!GARBAGE!!! in telega-server buffer: %s"
                      (buffer-substring (point-min) (match-beginning 0)))
        (message "Telega: !GARBAGE! in the telega-server buffer")
        (delete-region (point-min) (match-beginning 0)))

      (when (> (- (point-max) (point)) sexpsz)
        (let ((value (read (current-buffer))))
          (prog1
              (list cmd (telega--tl-unpack value))
            (delete-region (point-min) (point))

            ;; remove trailing newline
            (cl-assert (= (following-char) ?\n))
            (delete-char 1)))))))

(defvar telega-server--last-error)
(defsubst telega-server--dispatch-cmd (cmd value)
  "Dispatch command CMD."
  (telega-debug "%s %s: %S" cmd (propertize "IN" 'face 'bold) value)

  (cond ((string= cmd "event")
         (let* ((extra (plist-get value :@extra))
                (call-cb (telega-server--callback-get extra)))
           (if call-cb
               (telega-server--callback-rm extra)
             (setq call-cb telega-server--on-event-func))

           ;; Function call may return errors
           (if (or (not (telega--tl-error-p value))
                   ;; If the error code is 406, the error message must
                   ;; not be processed in any way and must not be
                   ;; displayed to the user
                   (= (plist-get value :code) 406)
                   ;; 404 - webpage or message not found
                   (= (plist-get value :code) 404)
                   )
               (funcall call-cb value)

             ;; Error returned
             (if (boundp 'telega-server--last-error)
                 (set 'telega-server--last-error value)
               (message "telega-server error: %s"
                        (plist-get value :message))))))

        ((string= cmd "appindicator-event")
         (telega-appindicator--on-event value))

        ((string= cmd "error")
         (telega--on-error value))

        (t
         (telega-debug "%s %s: %S" (propertize "IN" 'face 'bold) cmd value)
         (error "Unknown cmd from telega-server: %s" cmd))))

(defun telega-server--idle-timer-function ()
  "Function to be called when telega-server gets idle."
  (setq telega-server--idle-timer nil)

  ;; Sync remote and local time. `telega-tdlib--unix-time' will be
  ;; used in the `(telega-time-seconds)' calls to adjust time to
  ;; match time on Telegram server side.  Also take into account
  ;; time used to accomplish request.
  ;; 
  ;; We do sync while idle to prevent local clock drift, see
  ;; https://github.com/tdlib/td/issues/1681
  (when (plist-get telega-tdlib--unix-time :need-update)
    (plist-put telega-tdlib--unix-time :need-update nil)
    (let ((request-time (telega-time-seconds 'as-is)))
      (telega--getOption :unix_time
        (lambda (tl-value)
          (cl-assert (eq (telega--tl-type tl-value) 'optionValueInteger))
          (if (<= (- (telega-time-seconds 'as-is) request-time) 1)
              (progn
                ;; Request took less then 1 second
                (setq telega-tdlib--unix-time
                      (list :remote (string-to-number (plist-get tl-value :value))
                            :local request-time))
                (telega-debug "Unix time: remote:%S - local:%S = adj:%S"
                              (plist-get telega-tdlib--unix-time :remote)
                              (plist-get telega-tdlib--unix-time :local)
                              (- (plist-get telega-tdlib--unix-time :remote)
                                 (plist-get telega-tdlib--unix-time :local))))
            ;; Try update "unix_time" on next idle
            (plist-put telega-tdlib--unix-time :need-update t))))))

  ;; Update dirty stuff
  ;; - Updating chats may cause filters became dirty, so update chats
  ;;   first before redisplaying filters
  (telega-chats-dirty--update)
  (telega-filters--redisplay))

(defun telega-server--commands-equal (cmd1 cmd2)
  "Return non-nil if CMD1 and CMD2 are equal and can be collapsed.
Used to optimize events processing in the `telega-server--parse-commands'."
  (let* ((cmd1-type (nth 0 cmd1))
         (cmd1-value (nth 1 cmd1))
         (value-type (telega--tl-type cmd1-value))
         (cmd2-type (nth 0 cmd2))
         (cmd2-value (nth 1 cmd2)))
    (and (string= cmd1-type "event")
         (string= cmd2-type "event")
         ;; No callback registered
         (not (plist-get cmd1-value :@extra))
         (not (plist-get cmd2-value :@extra))
         ;; Value types are equal
         (eq value-type (telega--tl-type cmd2-value))
         (cl-case value-type
           (updateFile
            (eq (telega--tl-get cmd1-value :file :id)
                (telega--tl-get cmd2-value :file :id)))
           ((updateChatLastMessage
             updateChatReadInbox
             updateChatReadOutbox
             updateChatUnreadMentionCount
             updateChatOnlineMemberCount
             ;; NOTE: `updateChatPosition' can't be collapsed this
             ;; way, because this event does not provide full info
             ;; about all chat list positions, it just updates a
             ;; single chat list position
             )
            (and (eq (plist-get cmd1-value :chat_id)
                     (plist-get cmd2-value :chat_id))))
           ((updateUserStatus
             updateUserFullInfo)
            (eq (telega--tl-get cmd1-value :user_id)
                (telega--tl-get cmd2-value :user_id)))
           (updateUser
            (eq (telega--tl-get cmd1-value :user :id)
                (telega--tl-get cmd2-value :user :id)))
           (updateHavePendingNotifications
            t)
           ((updateUnreadMessageCount
             updateUnreadChatCount)
            (equal (plist-get cmd1-value :chat_list)
                   (plist-get cmd2-value :chat_list)))
           )
         (progn
           (telega-debug "Collapsed events: %S" value-type)
           t)
         )))

(defun telega-server--parse-commands ()
  "Parse all available events from telega-server."
  (goto-char (point-min))
  (let (cmd-val parsed-commands)
    ;; NOTE: First parse all commands, then optimize events, because
    ;; some events (such as `updateFile', `updateChatLastMessage',
    ;; etc) can be collapsed to single event, then dispatch all the
    ;; events left after optimization
    (while (setq cmd-val (telega-server--parse-cmd))
      (setq parsed-commands (cons cmd-val parsed-commands)))
    (dolist (cmd (cl-delete-duplicates (nreverse parsed-commands)
                                       :test #'telega-server--commands-equal))
      (apply #'telega-server--dispatch-cmd cmd))

    (if telega-server--idle-timer
        (timer-set-time telega-server--idle-timer
                        (time-add nil (/ telega-server-idle-delay 2)))
      (setq telega-server--idle-timer
            (run-with-timer telega-server-idle-delay nil
                            #'telega-server--idle-timer-function)))
    ))

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
      ;;
      ;; NOTE: it causes problems when you quit telega in the middle
      ;; of chats updates, so commented out
      ;;
      ;; (with-temp-buffer
      ;;   (insert output)
      ;;   (telega-server--parse-commands))
      )))

(defun telega-server--sentinel (proc event)
  "Sentinel for the telega-server process."
  (let ((status (substring event 0 -1)) ; strip trailing \n
        (err (if (buffer-live-p (process-buffer proc))
                 (with-current-buffer (process-buffer proc) (buffer-string))
               "")))
    (telega-debug "SENTINEL: %S -> %S" proc status)
    (telega-status--set
     (concat "telega-server: " status (unless (string-empty-p err) "\n") err)
     ""
     'raw)

    ;; Notify in echo area if telega-server exited abnormally
    (unless (zerop (process-exit-status proc))
      (message "[%d]telega-server: %s" (process-exit-status proc) status))

    ;; If QR auth skipped, then relogin with phone number
    (when (and telega--relogin-with-phone-number
               (not (process-live-p proc)))
      (telega))))

(defun telega-server--send (sexp &optional command)
  "Send SEXP to telega-server."
  (let* ((print-circle nil)
         (print-level nil)
         (print-length nil)
         (sexp-packed (telega--tl-pack sexp))
         (value (prin1-to-string sexp-packed))
         (proc (telega-server--proc)))
    (cl-assert (process-live-p proc) nil "telega-server is not running")
    (telega-debug "%s: %s %d %s"
                  (propertize "OUTPUT" 'face 'bold)
                  (or command "send") (string-bytes value)
                  value)

    (process-send-string
     proc
     (concat (or command "send") " "
             (number-to-string (string-bytes value)) "\n"))
    (process-send-string proc value)
    (process-send-string proc "\n")))

(defun telega-server--call (sexp &optional callback command)
  "Same as `telega-server--send', but waits for answer from telega-server.
If CALLBACK is specified, then make async call and call CALLBACK
when result is received.
If CALLBACK is specified return `:@extra' value used for the call.
COMMAND is passed directly to `telega-server--send'."
  (unless (plist-get sexp :@extra)
    (setq sexp (plist-put sexp :@extra (cl-incf telega-server--extra))))
  (telega-server--send sexp command)

  (if callback
      (progn
        (telega-server--callback-put telega-server--extra callback)
        telega-server--extra)

    ;; synchronous call aka exec
    (let ((cb-extra telega-server--extra)
          (telega-server--last-error nil))
      (telega-server--callback-put
       cb-extra
       `(lambda (event)
          (puthash ,cb-extra event telega-server--results)))

      ;; Loop waiting for call completion
      (while (and (telega-server--callback-get cb-extra)
                  (accept-process-output
                   (telega-server--proc) telega-server-call-timeout)))

      ;; Return the result, checking for the error
      (let ((ret (gethash cb-extra telega-server--results)))
        (remhash cb-extra telega-server--results)

        (when (and (not ret) telega-server--last-error)
          (user-error
           "telega error=%d: %s"
           (plist-get telega-server--last-error :code)
           (plist-get telega-server--last-error :message)))
        ret))))

(defun telega-server--start ()
  "Start telega-server process."
  (when (process-live-p (telega-server--proc))
    (user-error "Error: telega-server already running"))

  (cl-assert (buffer-live-p (telega-root--buffer)) nil
             "Use M-x telega RET to start telega")

  (with-telega-debug-buffer
   (erase-buffer)
   (insert (format "%s ---[ telega-server started\n" (current-time-string))))

  ;;
  (let* ((process-connection-type nil)
         (process-adaptive-read-buffering nil)
         (telega-docker--cidfile
          (telega-docker--container-id-filename))
         (server-cmd (telega-server--process-command
                      (when telega-server-logfile
                        "-l")
                      (when telega-server-logfile
                        telega-server-logfile)
                      "-v"
                      (if telega-server-logfile
                          (int-to-string telega-server-verbosity)
                        "0"))))
    (telega-debug "telega-server CMD: %s" server-cmd)
    (with-current-buffer (generate-new-buffer " *telega-server*")
      (setq telega-server--on-event-func 'telega--on-event)
      (setq telega-server--deferred-events nil)
      (setq telega-server--inhibit-events nil)
      (setq telega-server--extra 0)
      (setq telega-server--callbacks (make-hash-table :test 'eq))
      (setq telega-server--results (make-hash-table :test 'eq))
      (setq telega-server--buffer (current-buffer))

      ;; NOTE: `docker' won't start if cidfile already exists
      (when (and telega-use-docker telega-docker--cidfile)
        (delete-file telega-docker--cidfile))

      (telega-status--set "telega-server: starting.")
      (let* ((proc-cmd-with-args (split-string server-cmd " " t))
             ;; NOTE: use `start-process-shell-command' for dockerized
             ;; `telega-server', to make env variables expansion work
             ;; as they might be specified in
             ;; `telega-docker-run-command'
             (proc (apply (if telega-use-docker
                              'start-process-shell-command
                            'start-process)
                          "telega-server" (current-buffer)
                          (if telega-use-docker
                              (list server-cmd)
                            proc-cmd-with-args))))
        (set-process-query-on-exit-flag proc nil)
        (set-process-sentinel proc #'telega-server--sentinel)
        (set-process-filter proc #'telega-server--filter)
        (set-process-coding-system proc 'utf-8 'utf-8)))))

(defun telega-server-kill ()
  "Kill the telega-server process."
  (interactive)
  (when telega-server--idle-timer
    (cancel-timer telega-server--idle-timer)
    (setq telega-server--idle-timer nil))

  (when (buffer-live-p telega-server--buffer)
    (kill-buffer telega-server--buffer)
    (run-hooks 'telega-kill-hook)))


;;; Misc TDLib API methods
(defun telega--searchHashtags (prefix &optional limit)
  (let ((reply (telega-server--call
                (list :@type "searchHashtags"
                      :prefix prefix
                      :limit (or limit 20)))))
    (append (plist-get reply :hashtags) nil)))

(provide 'telega-server)

;;; telega-server.el ends here

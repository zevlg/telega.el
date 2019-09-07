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

(declare-function telega-root--buffer "telega-root")
(declare-function telega-status--set "telega-root" (conn-status &optional aux-status raw))


(defun telega--on-event (event)
  (let ((event-sym (intern (format "telega--on-%s" (plist-get event :@type)))))
    (if (symbol-function event-sym)
        (funcall (symbol-function event-sym) event)

      (telega-debug "TODO: define `%S'" event-sym))))

(defun telega--on-error (err)
  (telega-debug "IN error: %s" err)

  (message "Telega error: %s" err))

;; Server runtime vars
(defvar telega-server--buffer nil)
(defvar telega-server--extra 0 "Value for :@extra used by `telega-server--call'.")
(defvar telega-server--callbacks nil "Callbacks ruled by extra")
(defvar telega-server--results nil)
(defvar telega-server--on-event-func #'telega--on-event
  "Func used to trigger on event.
Used to make deferred calls.")
(defvar telega-server--deferred-events nil)

(defun telega--on-deferred-event (event)
  (setq telega-server--deferred-events
        (nconc telega-server--deferred-events (list event))))

(defmacro with-telega-deferred-events (&rest body)
  "Execute BODY deferring telega-server events processing."
  (declare (indent 0))
  (let ((evsym (gensym "event")))
    `(progn
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

(defun telega-server-build (&optional no-query-p no-voip-query-p)
  "Build and install `telega-server' binary.
If NO-QUERY-P is non-nil, then install `telega-server' without
asking first.
If NO-VOIP-QUERY-P is non-nil, then build `telega-server' without
VOIP support, otherwise ask user.
Raise error if compilation/test fails."
  (interactive)
  (telega-test-env 'quiet)
  (when (or no-query-p
            (y-or-n-p "Build `telega-server'? "))

    (let ((default-directory telega--lib-directory)
          (with-voip-p (unless no-voip-query-p
                         (y-or-n-p "Build `telega-server' with VOIP support? "))))
      (unless (zerop
               (shell-command
                (concat "make " (when with-voip-p "WITH_VOIP=t ") "install")))
        (error "`telega-server' installation failed")))))

(defun telega-server--find-bin ()
  "Find telega-server executable.
Raise error if not found."
  (let ((exec-path (cons telega-directory exec-path)))
    (or (executable-find "telega-server")
        (progn (telega-server-build)
               (executable-find "telega-server"))
        (error "`telega-server' not found in exec-path"))))

(defun telega-server-version ()
  "Return telega-server version."
  (let ((ts-usage (shell-command-to-string
                   (concat (telega-server--find-bin) " -h"))))
    (when (string-match "^Version \\([0-9.]+\\)" ts-usage)
      (match-string 1 ts-usage))))

(defun telega-server--check-version (required-version)
  "Check telega-server version against REQUIRED-VERSION.
If does not match, then query user to rebuild telega-server.
If version does not match then query user to rebuild telega-server."
  (let ((ts-version (telega-server-version)))
    (unless (equal ts-version required-version)
      (when (y-or-n-p
             (format "Installed `telega-server' version %s≠%s, rebuild? "
                     ts-version required-version))
        (telega-server-build 'no-query)))))

(defsubst telega-server--proc ()
  "Return telega-server process."
  (get-buffer-process telega-server--buffer))

(defun telega-server-live-p ()
  "Return non-nil if telega-sever process is alive."
  (process-live-p (telega-server--proc)))

(defsubst telega-server--parse-cmd ()
  "Parse single reply from telega-server.
Return parsed command."
  (when (re-search-forward "^\\([a-z]+\\) \\([0-9]+\\)\n" nil t)
    ;; New command always start at the beginning, no garbage inbetween
    ;; commands
    (cl-assert (= (match-beginning 0) 1))

    (let ((cmd (match-string 1))
          (sexpsz (string-to-number (match-string 2))))
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
  (cond ((string= cmd "event")
         (let* ((extra (plist-get value :@extra))
                (call-cb (telega-server--callback-get extra)))
           (if call-cb
               (telega-server--callback-rm extra)
             (setq call-cb telega-server--on-event-func))

           (telega-debug "%s event: %S" (propertize "IN" 'face 'bold) value)

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
    (telega-status--set
     (concat "telega-server: " status (unless (string-empty-p err) "\n") err)
     ""
     'raw)

    ;; Notify in echo area if telega-server exited abnormally
    (unless (zerop (process-exit-status proc))
      (message "[%d]telega-server: %s" (process-exit-status proc) status))
    ))

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

(defun telega-server--call (sexp &optional callback)
  "Same as `telega-server--send', but waits for answer from telega-server.
If CALLBACK is specified, then make async call and call CALLBACK
when result is received.
If CALLBACK is specified return `:@extra' value used for the call."
  (unless (plist-get sexp :@extra)
    (setq sexp (plist-put sexp :@extra (cl-incf telega-server--extra))))
  (telega-server--send sexp)

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
           "telega: %s" (plist-get telega-server--last-error :message)))
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

  (let ((process-connection-type nil)
        (process-adaptive-read-buffering nil)
        (server-bin (telega-server--find-bin)))
    (with-current-buffer (generate-new-buffer " *telega-server*")
      (setq telega-server--on-event-func 'telega--on-event)
      (setq telega-server--deferred-events nil)
      (setq telega-server--extra 0)
      (setq telega-server--callbacks (make-hash-table :test 'eq))
      (setq telega-server--results (make-hash-table :test 'eq))
      (setq telega-server--buffer (current-buffer))

      (telega-status--set "telega-server: starting.")
      (let* ((proc-args (if telega-server-logfile
                            (list "-l" telega-server-logfile
                                  "-v" (int-to-string telega-server-verbosity))
                          (list "-v" "0")))
             (proc (apply 'start-process
                          "telega-server" (current-buffer) server-bin
                          proc-args)))
        (set-process-query-on-exit-flag proc nil)
        (set-process-sentinel proc #'telega-server--sentinel)
        (set-process-filter proc #'telega-server--filter))))
  (current-buffer))

(defun telega-server-kill ()
  "Kill the telega-server process."
  (interactive)
  (when (buffer-live-p telega-server--buffer)
    (kill-buffer telega-server--buffer)
    (setq telega-server--buffer nil)
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

;;; telega-server.el --- telega-server functionality

;; Copyright (C) 2018 by Zajcev Evgeny.

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

;;

;;; Code:
(require 'cl-lib)

(require 'telega-core)
(require 'telega-customize)

(defgroup telega-server nil
  "Customisation for telega-server."
  :prefix "telega-server-"
  :group 'telega)

(defcustom telega-server-command "telega-server"
  "Command to run as telega server."
  :type 'string
  :group 'telega-server)

(defcustom telega-server-logfile
  (expand-file-name "telega-server.log" telega-directory)
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

(defun telega--on-event (event)
  (telega-debug "IN event: %s" event)

  (let ((event-sym (intern (format "telega--on-%s" (plist-get event :@type)))))
    (if (symbol-function event-sym)
        (funcall (symbol-function event-sym) event)

      (telega-debug "TODO: define `%S'" event-sym))))

(defun telega--on-error (err)
  (telega-debug "IN error: %s" err)

  (message "Telega error: %s" err))

;; Server runtime vars
(defvar telega-server--buffer nil)
(defvar telega-server--last-error nil)
(defvar telega-server--extra 0 "Value for :@extra used by `telega-server--call'.")
(defvar telega-server--callbacks nil "Callbacks ruled by extra")

(defsubst telega-server--callback-add (extra cb)
  (setq telega-server--callbacks
        (plist-put telega-server--callbacks extra cb)))

(defsubst telega-server--callback-rm (extra)
  (if (eq extra (car telega-server--callbacks))
       (setq telega-server--callbacks (cddr telega-server--callbacks))
     (cl--do-remf telega-server--callbacks extra)))

(defsubst telega-server--callback-get (extra)
  (plist-get telega-server--callbacks extra))

(defun telega-server--find-bin ()
  "Find telega-server executable.
Raise error if not found"
  (let ((exec-path (cons telega-directory exec-path)))
    (or (executable-find "telega-server")
        (error "telega-server not found in exec-path"))))

(defsubst telega-server--proc ()
  "Return telega-server process."
  (get-buffer-process telega-server--buffer))

(defsubst telega-server--parse-cmd ()
  "Parse single reply from telega-server."
  (when (re-search-forward "^\\([a-z]+\\) \\([0-9]+\\)\n" nil t)
    (let ((cmd (match-string 1))
          (sexpsz (string-to-number (match-string 2))))
      (when (> (- (point-max) (point)) sexpsz)
        (let ((value (read (current-buffer))))
          (prog1
              (list cmd (telega--tl-unpack value))
            (delete-region (point-min) (point))))))))

(defsubst telega-server--dispatch-cmd (cmd value)
  "Dispatch command CMD."
  (cond ((string= cmd "event")
         (let* ((extra (plist-get value :@extra))
                (call-cb (telega-server--callback-get extra)))
           (if call-cb
               (telega-server--callback-rm extra)
             (setq call-cb #'telega--on-event))

           ;; Function call may return errors
           (if (or (not (eq 'error (telega--tl-type value)))
                   (= (plist-get value :code) 406))
               (funcall call-cb value)

             ;; Error returned
             (setq telega-server--last-error value)
             (message "telega-server error: %s" (plist-get value :message)))))

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

(defun telega-server--sentinel (proc event)
  "Sentinel for the telega-server process."
  (let ((status (substring event 0 -1)) ; strip trailing \n
        (err (if (buffer-live-p (process-buffer proc))
                 (with-current-buffer (process-buffer proc) (buffer-string))
               "")))
    (telega-status--set
     (concat "telega-server: " status (unless (string-empty-p err) "\n") err)
     'raw)))

(defun telega-server--send (sexp)
  "Send SEXP to telega-server."
  (let* ((print-circle nil)
         (value (prin1-to-string (telega--tl-pack sexp)))
         (proc (telega-server--proc)))
    (assert (process-live-p proc) nil "telega-server is not running")
    (telega-debug "OUTPUT: %d %s" (string-bytes value) value)

    (process-send-string
     proc
     (concat "send " (number-to-string (string-bytes value)) "\n"))
    (process-send-string proc value)
    (process-send-string proc "\n")))

(defun telega-server--call (sexp &optional callback)
  "Same as `telega-server--send', but waits for answer from telega-server.
If CALLBACK is specified, then make async call and call CALLBACK
when result is received."
  (telega-server--send (plist-put sexp :@extra (incf telega-server--extra)))

  (if callback
      (telega-server--callback-add telega-server--extra callback)

    ;; synchronous call aka exec
    (let ((cb-extra telega-server--extra)
          telega-server--result)
      (telega-server--callback-add
       telega-server--extra
       (lambda (event) (setq telega-server--result event)))

      (while (and (telega-server--callback-get cb-extra)
                  (accept-process-output
                   (telega-server--proc) telega-server-call-timeout)))
      telega-server--result)))

(defun telega-server--start ()
  "Start telega-server process."
  (when (process-live-p (telega-server--proc))
    (error "Error: telega-server already running"))

  (with-telega-debug-buffer
   (erase-buffer)
   (insert (format "%s ---[ telega-server started\n" (current-time-string))))

  (let ((process-connection-type nil)
        (process-adaptive-read-buffering nil)
        (server-bin (telega-server--find-bin))
        proc)
    (with-current-buffer (generate-new-buffer " *telega-server*")
      ;; init vars and start proc
      (telega--init-vars)
      (setq telega-server--extra 0)
      (setq telega-server--callbacks nil)
      (setq telega-server--buffer (current-buffer))

      (telega-status--set "telega-server: starting")
      (let ((proc (start-process
                   "telega-server"
                   (current-buffer)
                   server-bin
                   "-v" (int-to-string telega-server-verbosity)
                   "-l" telega-server-logfile)))
        (set-process-query-on-exit-flag proc nil)
        (set-process-sentinel proc #'telega-server--sentinel)
        (set-process-filter proc #'telega-server--filter))))
  (current-buffer))

(defun telega-server-kill ()
  "Kill the telega-server process."
  (interactive)
  (when (buffer-live-p telega-server--buffer)
    (kill-buffer telega-server--buffer)
    (setq telega-server--buffer nil)))

(provide 'telega-server)

;;; telega-server.el ends here

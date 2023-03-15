;;; telega-status-history.el --- Collect online status history.  -*- lexical-binding:t; no-byte-compile: t; -*-

;; Copyright (C) 2020 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Mar 19 14:59:25 2020
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

;;; ellit-org:
;; ** /telega-status-history.el/ -- Global minore mode to save user's online status history
;;
;; Saves online status history into ~telega-status-history-logs-dir~ directory.

;;; Code:
(require 'generator)

(require 'telega)

(defcustom telega-status-history-logs-dir
  (expand-file-name "online-history" telega-directory)
  "*Directory for online status logs."
  :type 'string
  :group 'telega-modes)

;;;###autoload
(define-minor-mode telega-status-history-mode
  "Global mode to collect online status history."
  :init-value nil :global t :group 'telega-modes
  (if telega-status-history-mode
      (progn
        (ignore-errors
          (mkdir telega-status-history-logs-dir))
        (advice-add 'telega--on-updateUserStatus
                    :after #'telega-status-history--on-status-update))

    (advice-remove 'telega--on-updateUserStatus
                   #'telega-status-history--on-status-update)))

(defun telega-status-history--filename (timestamp)
  "Return log filename for the TIMESTAMP."
  (expand-file-name (format "%s.log" (telega--time-at00 timestamp))
                    telega-status-history-logs-dir))

(defun telega-status-history--write (timestamp status user-id)
  "Write single entry to status history log file."
  (let ((log-fname (telega-status-history--filename timestamp)))
    (write-region (concat (prin1-to-string
                           (list timestamp status user-id)) "\n")
                  nil log-fname 'append 'quiet)))

(defun telega-status-history--on-status-update (event)
  "Save status update."
  (let* ((user-id (plist-get event :user_id))
         (user (telega-user-get user-id))
         (status (plist-get event :status))
         (online-p (eq (telega--tl-type status) 'userStatusOnline)))
    (telega-status-history--write (or (unless online-p
                                        (plist-get status :was_online))
                                      (telega-time-seconds))
                                  (if online-p :online :offline)
                                  user-id)))

(defun telega-status-history-file--entries (log-file &optional stop-ts)
  "Return list of the LOG-FILE entries."
  (with-temp-buffer
    (save-excursion
      (insert "(")
      (insert-file-contents log-file)
      (goto-char (point-max))
      (insert ")"))
    (read (current-buffer))))

(iter-defun telega-status-history--iter (start stop)
  "Iterator over history entries saved in log files."
  (let (log-file-name)
    (while (and (< start stop) (not log-file-name))
      (setq log-file-name (telega-status-history--filename start))
      (if (file-exists-p log-file-name)
          (let ((entries (telega-status-history-file--entries log-file-name)))
            (while (and entries (<= (car (car entries)) stop))
              (iter-yield (car entries))
              (setq entries (cdr entries))))

        ;; Continue
        (setq log-file-name nil))

      (setq start (+ start (* 24 60 60))))
    ))

(defun telega-status-history-histogram (start stop interval-names &rest users)
  "Generate histogram for USERS.
START and STOP - time interval to generate histogram.
INTERVAL-NAMES is the list of the names for the intervals.
Number of the elements in INTERVAL-NAMES denotes number of the intervals.
USERS - list of the users to collect info about."
  (cl-assert (not (null interval-names)))

  (with-temp-buffer
    (insert "Interval")
    (dolist (user users)
      (insert "\t" (substring-no-properties (telega-user-title user 'short))))
    (insert "\n")

    ;; Insert rows
    (let* ((n-intervals (length interval-names))
           (n-idx 0)
           (interval-dur (/ (- stop start) n-intervals))
           (next-interval-stop (+ start interval-dur))
           (user-stats nil))

      (iter-do (entry (telega-status-history--iter start stop))
        (cl-destructuring-bind (ts status user-id) entry
          ;; TODO: calculate user-id's duration

          (when (> ts (+ start (* (+ 1 n-idx) interval-dur)))
            ;; NOTE: Flushing tsv row
            (insert (nth n-idx interval-names))
            (dolist (user users)
              (let ((us (or (cadr (assq (plist-get user :id) user-stats)) 0)))
                (insert "\t" (number-to-string us))))
            (insert "\n")

            (setq n-idx (1+ n-idx)))
          )
        ))

    (buffer-string)))

(defun telega-status-history-histogram-day (&rest users)
  "Return online status histogram for this current day."
  (let ((current-ts (time-to-seconds (current-time))))
    (apply #'telega-status-history-histogram
           (telega--time-at00 current-ts)
           (telega--time-at00 (+ current-ts (* 24 60 60)))
           24 users)))

(provide 'telega-status-history)

;;; telega-status-history.el ends here

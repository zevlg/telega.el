;;; telega-status-history.el --- Collect online status history.

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

;; ** /telega-status-history.el/ -- Global minore mode to save user's online status history
;;
;; Saves online status history into ~telega-status-history-logs-dir~ directory.

;;; Code:
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

(defun telega-status-history--write (timestamp status user-id)
  "Write single entry to status history log file."
  (let ((log-fname (expand-file-name
                    (format "%s.log" (telega--time-at00 timestamp))
                    telega-status-history-logs-dir)))
    (write-region (concat (prin1-to-string
                           (list timestamp status user-id)) "\n")
                  nil log-fname 'append 'quiet)))

(defun telega-status-history--on-status-update (event)
  "Save status update."
  (let* ((user-id (plist-get event :user_id))
         (user (telega-user--get user-id))
         (status (plist-get event :status))
         (online-p (eq (telega--tl-type status) 'userStatusOnline)))
    (telega-status-history--write (or (unless online-p
                                        (plist-get status :was_online))
                                      (telega-time-seconds))
                                  (if online-p :online :offline)
                                  user-id)))

(defun telega-status-history-histogram (start stop n-intervals &rest users)
  "Generate histogram for USERS.
START and STOP - time interval to generate histogram.
N-INTERVALS - number of intervals in histogram."
  ;; TODO:
  )

(defun telega-status-history-histogram-day (&rest users)
  "Return online status histogram for this current day."
  (let ((current-ts (time-to-seconds (current-time))))
    (apply #'telega-status-history-histogram
           (telega--time-at00 current-ts)
           (telega--time-at00 (+ current-ts (* 24 60 60)))
           24 users)))

(provide 'telega-status-history)

;;; telega-status-history.el ends here

;;; telega-ffplay.el --- Interface to ffplay for telega  -*- lexical-binding:t -*-

;; Copyright (C) 2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Jan 23 20:58:35 2019
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

(defvar telega-ffplay-buffer-name
  (concat (unless telega-debug " ") "*ffplay telega*"))

(defun telega-ffplay-proc ()
  "Return current ffplay process."
  (let ((buf (get-buffer telega-ffplay-buffer-name)))
    (when (buffer-live-p buf)
      (get-buffer-process buf))))

(defun telega-ffplay-pause (&optional proc)
  "Pause ffplay process PROC."
  ;; SIGSTOP to pause ffplay
  (let ((ffproc (or proc (telega-ffplay-proc))))
    (when ffproc
      (signal-process ffproc 19))))

(defun telega-ffplay-resume (&optional proc)
  "Resume ffplay process PROC."
  ;; SIGCONT to resume ffplay
  (let ((ffproc (or proc (telega-ffplay-proc))))
    (when ffproc
      (signal-process ffproc 18))))

(defun telega-ffplay-stop ()
  "Stop running ffplay process."
  (let ((buf (get-buffer telega-ffplay-buffer-name)))
    (when (buffer-live-p buf)
      (kill-buffer buf)
      ;; NOTE:
      ;;  - killing buffer when ffplay is paused does not trigger
      ;;    sentinel, so we resume the process after killing buffer
      ;;
      ;;  - Callback will be called in sentinel
      (telega-ffplay-resume (get-buffer-process buf)))))

(defun telega-ffplay--sentinel (proc _event)
  "Sentinel for the ffplay process."
  (let* ((proc-plist (process-plist proc))
         (callback (plist-get proc-plist :progress-callback)))
    (when callback
      (funcall callback proc))))

(defun telega-ffplay--filter (proc output)
  "Filter for the telega-server process."
  (let* ((buffer (process-buffer proc))
         (proc-plist (process-plist proc))
         (callback (plist-get proc-plist :progress-callback))
         (progress (plist-get proc-plist :progress)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert output)
          (when (re-search-backward "\\s-*\\([0-9.]+\\)" nil t)
            (let ((np (string-to-number (match-string 1))))
              (set-process-plist
               proc (plist-put proc-plist :progress np))
              (when (and callback (> np progress))
                (funcall callback proc)))))

          (unless telega-debug
            (delete-region (point-min) (point-max)))
          )))

(defun telega-ffplay-run (filename callback &rest ffplay-args)
  "Start ffplay to play FILENAME.
CALLBACK is called on updates with single argument - process.
FFPLAY-ARGS is additional args to the ffplay.
Return newly created process."
  ;; Additional args:
  ;;   -nodisp       for sounds
  ;;   -ss <SECONDS> to seek
  ;; Kill previously running ffplay if any
  (telega-ffplay-stop)

  ;; Start new ffplay
  (let ((args (nconc (list "-hide_banner" "-autoexit")
                     ffplay-args (list (expand-file-name filename))))
        (ffplay-bin (or (executable-find "ffplay")
                        (error "ffplay not found in `exec-path'"))))
    (with-current-buffer (get-buffer-create telega-ffplay-buffer-name)
      (let ((proc (apply 'start-process "ffplay" (current-buffer)
                         ffplay-bin args)))
        (set-process-plist proc (list :progress-callback callback
                                      :progress 0.0))
        (set-process-query-on-exit-flag proc nil)
        (set-process-sentinel proc #'telega-ffplay--sentinel)
        (set-process-filter proc #'telega-ffplay--filter)
        proc))))

(provide 'telega-ffplay)

;;; telega-ffplay.el ends here

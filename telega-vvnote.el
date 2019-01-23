;;; telega-vvnote.el --- Voice/Video notes support for telega

;; Copyright (C) 2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Mon Jan 21 22:12:55 2019
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

(defcustom telega-vvnote-voice-cmd "ffmpeg -f alsa -i default -strict -2 -acodec opus -ab 30k"
  "Command to use to save voice notes."
  :type 'string
  :group 'telega-vvnote)

(defcustom telega-vvnote-voice-max-dur (* 30 60)
  "Maximum duration of voice command in seconds."
  :type 'number
  :group 'telega-vvnote)

(defcustom telega-vvnote-video-cmd "ffmpeg -f v4l2 -s 320x240 -i /dev/video0 -r 30 -f alsa -i default -vf format=yuv420p,crop=240:240:40:0 -strict -2 -vcodec hevc -acodec opus -vb 300k -ab 30k"
  "Command to use to save video notes."
  :type 'string
  :group 'telega-vvnote)

(defvar telega-vvnote--ffplay-buffer-name "*telega ffplay*")
(defvar telega-vvnote--progress nil)
(defvar telega-vvnote--callback nil)

(defun telega-vvnote--ffplay-sentinel (proc event)
  "Sentinel for the ffplay process."
  (unless telega-debug
    (kill-buffer telega-vvnote--ffplay-buffer-name))
  (when telega-debug
    (message "ffplay SENTINEL: %S" event))

  (when telega-vvnote--callback
    ;; nil progress mean DONE
    (funcall telega-vvnote--callback nil)))

(defun telega-vvnote--ffplay-filter (proc output)
  "Filter for the telega-server process."
  (let ((buffer (process-buffer proc)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert output)
          (when (re-search-backward "\\s-+\\([0-9.]+\\)" nil t)
            (let ((np (string-to-number (match-string 1))))
              (when (> (- np telega-vvnote-progress) 0.25)
                (setq telega-vvnote-progress np)
                (when telega-vvnote--callback
                  (funcall telega-vvnote--callback telega-vvnote-progress)))))

          (unless telega-debug
            (delete-region (point-min) (point-max)))
          ))))

(defun telega-vvnote--ffplay (filename &optional seek-seconds callback)
  "Play FILENAME with ffplay, monitoring progress."
  (let ((buf (get-buffer telega-vvnote--ffplay-buffer-name)))
    (when (buffer-live-p buf)
      ;; Kill currently running ffplay
      (kill-buffer buf)))

  ;; Start new ffplay
  (setq telega-vvnote--callback callback
        telega-vvnote-progress 0.0)

  (let ((args (list "-hide_banner" "-autoexit" filename)))
    (when seek-seconds
      (setq args (nconc (list "-ss" (number-to-string seek-seconds)) args)))

    (with-current-buffer (get-buffer-create telega-vvnote--ffplay-buffer-name)
      (let ((proc (apply 'start-process "ffplay" (current-buffer)
                         (executable-find "ffplay") args)))
        (set-process-query-on-exit-flag proc nil)
        (set-process-sentinel proc #'telega-vvnote--ffplay-sentinel)
        (set-process-filter proc #'telega-vvnote--ffplay-filter)))))

(provide 'telega-vvnote)

;;; telega-vvnote.el ends here

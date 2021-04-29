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

(defun telega-ffplay-check-codecs (how &rest codecs)
  "Check CODEC is available in ffmpeg.
Return list of available codecs."
  (let ((codecs-output
         (shell-command-to-string "ffmpeg -v quiet -codecs")))
    (cl-remove-if-not
     #'stringp
     (mapcar (lambda (codec)
               (when (string-match-p
                      (concat (concat (if (memq 'decoder how) "D" ".")
                                      (if (memq 'encoder how) "E" "."))
                              "....."
                              codec)
                      codecs-output)
                 codec))
             codecs))))

(defconst telega-ffplay--has-encoders
  (telega-ffplay-check-codecs '(encoder) "opus" "hevc" "aac" "h264"))

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

(defun telega-ffplay-stop (&optional proc)
  "Stop running ffplay process."
  (let ((buf (or (when proc (process-buffer proc))
                 (get-buffer telega-ffplay-buffer-name))))
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
  (telega-debug "ffplay SENTINEL: status=%S, live=%S"
                (process-status proc) (process-live-p proc))

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
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert output)
        (let (new-progress)
          (cond ((save-excursion
                   (re-search-backward "\\s-*\\([0-9.]+\\)" nil t))
                 ;; ffplay format when playing
                 (setq new-progress (string-to-number (match-string 1))))
                ((save-excursion
                   (re-search-backward
                    " time=\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9.]+\\) "
                    nil t))
                 ;; ffmpeg voice note capture format
                 (setq new-progress
                       (+ (* 3600 (string-to-number (match-string 1)))
                          (* 60 (string-to-number (match-string 2)))
                          (string-to-number (match-string 3))))))
          (when new-progress
            (set-process-plist
             proc (plist-put proc-plist :progress new-progress))
            (when (and callback (> new-progress progress))
              (funcall callback proc))))

        (unless telega-debug
          (delete-region (point-min) (point-max))))
      )))

(defun telega-ffplay-run-command (cmd &optional callback)
  "Run arbitrary ffplay/ffmpeg/ffprobe command CMD."
  ;; Kill previously running ffplay if any
  (telega-ffplay-stop)

  ;; Start new ffplay
  (telega-debug "ffplay START: %s" cmd)
  (with-current-buffer (get-buffer-create telega-ffplay-buffer-name)
    (let ((proc (apply 'start-process "ffplay" (current-buffer)
                       (split-string cmd " " t))))
      (set-process-plist proc (list :progress-callback callback
                                    :progress 0.0))
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel proc #'telega-ffplay--sentinel)
      (set-process-filter proc #'telega-ffplay--filter)
      proc)))

(defun telega-ffplay-run (filename callback &optional ffplay-args)
  "Start ffplay to play FILENAME.
CALLBACK is called on updates with single argument - process.
FFPLAY-ARGS is additional arguments string for the ffplay.
Return newly created process."
  ;; Additional args:
  ;;   -nodisp       for sounds
  ;;   -ss <SECONDS> to seek
  ;; Kill previously running ffplay if any
  (telega-ffplay-stop)

  ;; Start new ffplay
  (let ((args (nconc (list "-hide_banner" "-autoexit")
                     (split-string (or ffplay-args "") " " t)
                     (list (expand-file-name filename))))
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

(defun telega-ffplay-get-fps-ratio (filename &optional default)
  "Return fps ratio string for the FILENAME video file.
Ratio string is returned in form
\"<fps_numerator>/<fps_denominator>\", f.i. \"30000/1001\" for 29.97fps.
If fps is not available for FILENAME, then return DEFAULT or \"30/1\"
if ommited."
  (let ((fps-ratio (telega-strip-newlines
                    (shell-command-to-string
                     (telega-docker-exec-cmd
                       (concat "ffprobe -v error -select_streams v:0 "
                               "-show_entries stream=r_frame_rate "
                               "-of default=noprint_wrappers=1:nokey=1 "
                               "\"" (expand-file-name filename) "\"")
                       'try-host-cmd-first)))))
    (if (string-match-p "[0-9]+/[0-9]+" fps-ratio)
        fps-ratio
      (or default "30/1"))))

(defun telega-ffplay-get-nframes (filename)
  "Probe number of frames of FILENAME video file."
  (string-to-number
   (shell-command-to-string
    (telega-docker-exec-cmd
      (concat "ffprobe -v error -select_streams v:0 "
              "-show_entries stream=nb_frames "
              "-of default=nokey=1:noprint_wrappers=1 "
              "\"" (expand-file-name filename) "\"")
      'try-host-cmd-first))))

(defun telega-ffplay-get-metadata (filename)
  "Return metadata as alist for the media FILENAME."
  (let ((raw-metadata (shell-command-to-string
                       (telega-docker-exec-cmd
                         (concat "ffmpeg -v 0 -i "
                                 "\"" (expand-file-name filename) "\" "
                                 " -f ffmetadata -")
                         'try-host-cmd-first))))
    (delq nil (mapcar (lambda (line)
                        (when (string-match "\\([a-zA-Z]+\\)=\\(.+$\\)" line)
                          (cons (match-string 1 line) (match-string 2 line))))
                      (split-string raw-metadata "\n" t " \t")))))

(defun telega-ffplay-get-duration (filename)
  "Return duration as float number for the media FILENAME."
  (string-to-number
   (shell-command-to-string
    (telega-docker-exec-cmd
      (concat "ffprobe -v error "
              "-show_entries format=duration "
              "-of default=nokey=1:noprint_wrappers=1 "
              "\"" (expand-file-name filename) "\"")
      'try-host-cmd-first))))

(defun telega-ffplay-get-resolution (filename)
  "Return resolution for video FILENAME.
Return cons cell with width and height if resolution is extracted, nil
otherwise."
  (let ((raw-res (shell-command-to-string
                  (telega-docker-exec-cmd
                    (concat "ffprobe -v error "
                            "-show_entries stream=width,height "
                            "-of default=nokey=1:noprint_wrappers=1 "
                            "\"" (expand-file-name filename) "\"")
                    'try-host-cmd-first))))
    (when (string-match "\\([0-9]+\\)\n\\([0-9]+\\)" raw-res)
      (cons (string-to-number (match-string 1 raw-res))
            (string-to-number (match-string 2 raw-res))))))

(defun telega-ffplay--png-extract ()
  "Extract png image data from current buffer.
Return cons cell where car is the frame number and cdr is frame
filename.
Return nil if no image is available."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\([0-9]+\\) \\([^\r\n]+\\)\r?\n" nil t)
      (let ((frame-num (match-string 1))
            (frame-filename (match-string 2)))
        (delete-region (match-beginning 0) (match-end 0))
        (cons (string-to-number frame-num) frame-filename)))))

(defun telega-ffplay--png-sentinel (proc _event)
  "Sentinel for png extractor, see `telega-ffplay-to-png'."
  (telega-debug "ffplay-png SENTINEL: status=%S, live=%S"
                (process-status proc) (process-live-p proc))

  (unless (process-live-p proc)
    (let* ((proc-plist (process-plist proc))
           (callback (plist-get proc-plist :callback))
           (callback-args (plist-get proc-plist :callback-args)))
      ;; DONE executing
      (when callback
        (apply callback proc nil callback-args))

      ;; Delete all produced files for frames
      (dolist (frame (plist-get proc-plist :frames))
        (delete-file (cdr frame)))

      ;; NOTE: sentinel might be called multiple times with 'exit
      ;; status, handle this situation simple by unsetting callback
      (set-process-plist proc (plist-put proc-plist :callback nil)))))

(defun telega-ffplay--png-filter (proc output)
  "Filter for png extractor, see `telega-ffplay-to-png'."
  (let* ((proc-plist (process-plist proc))
         (callback (plist-get proc-plist :callback))
         (callback-args (plist-get proc-plist :callback-args))
         (buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer (process-buffer proc)
        (goto-char (point-max))
        (insert output)
        ;; If got multiple frames, skip to the latest
        (let ((next-frame (telega-ffplay--png-extract))
              frame)
          (while next-frame
            (setq frame next-frame
                  next-frame (telega-ffplay--png-extract))
            (when next-frame
              (telega-debug "ffplay-png: skipping frame %S" frame)
              (delete-file (cdr frame))))
          (when frame
            ;; Put frame in the list
            (let ((all-frames (plist-get proc-plist :frames)))
              (plist-put proc-plist :frames (cons frame all-frames))
              (set-process-plist proc proc-plist))
            (when callback
              (apply callback proc frame callback-args)))
          )))))

(defun telega-ffplay-to-png (filename ffmpeg-args callback &rest callback-args)
  "Play video FILENAME extracting png images from it.
FFMPEG-ARGS - Aditional arguments list for ffmpeg.
CALLBACK is called with args: <proc> <filename.png>  <callback-args>
CALLBACK is called with nil filename when finished.
Return newly created proc."
  (declare (indent 2))
  ;; Stop any ffplay running
  (telega-ffplay-stop)

  ;; Start new ffmpeg
  (with-current-buffer (get-buffer-create telega-ffplay-buffer-name)
    (let* ((prefix (telega-temp-name "png-video"))
           (ffmpeg-cmd-args
            (concat " -hide_banner -loglevel quiet"
                    (when filename
                      (concat " -i '" (expand-file-name filename) "'"))
                    " " (mapconcat 'identity ffmpeg-args " ")
                    ;; NOTE: hack, for custom capture arguments
                    ;; for video note recorder
                    (unless (member "image2pipe" ffmpeg-args)
                      " -f image2pipe -vcodec png -")))
           (pngext-cmd-args
            (concat "-E " prefix
                    ;; NOTE: -re causes sound problems in video
                    ;; notes "-re", instead we use '-f' flag in
                    ;; telega-server png extractor
                    (when-let ((fps-ratio (when filename
                                            (telega-ffplay-get-fps-ratio
                                             (expand-file-name filename)))))
                      (concat " -f " fps-ratio))))
           (shell-cmd
            (if (and telega-use-docker (member "-an" ffmpeg-args))
                ;; Can run both ffmpeg and pngextractor in docker
                (telega-docker-exec-cmd
                  (format "sh -c \"ffmpeg %s | telega-server %s\""
                          ffmpeg-cmd-args pngext-cmd-args))
              (format "%s %s | %s %s"
                      (or (executable-find "ffmpeg")
                          (error "ffmpeg not found in `exec-path'"))
                      ffmpeg-cmd-args
                      (telega-docker-exec-cmd
                        telega-server-command 'try-host-cmd-first "-i")
                      pngext-cmd-args)))
           (process-adaptive-read-buffering nil) ;no buffering please
           (proc (start-process-shell-command
                  "ffmpeg" (current-buffer)
                  shell-cmd)))

      (telega-debug "ffplay RUN: %s" shell-cmd)
      (set-process-plist proc (list :prefix prefix
                                    :nframes (if filename
                                                 (telega-ffplay-get-nframes filename)
                                               -1)
                                    :frames nil
                                    :callback callback
                                    :callback-args callback-args))
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel proc 'telega-ffplay--png-sentinel)
      (set-process-filter proc 'telega-ffplay--png-filter)
      proc)))


;;; Video Player
(defun telega-video-player--sentinel (proc _event)
  "Sentinel for incremental player."
  (telega-debug "video-player SENTINEL: status=%S, live=%S"
                (process-status proc) (process-live-p proc))

  (let* ((proc-plist (process-plist proc))
         (done-callback (plist-get proc-plist :done-callback)))
    (unless (process-live-p proc)
      (when done-callback
        (funcall done-callback)))))

(defun telega-video-player-run (filename &optional done-callback)
  "Start playing video FILENAME with `telega-video-player-command' command.
DONE-CALLBACK - callback to call, when done viewing video."
  (unless telega-video-player-command
    (user-error "telega: `telega-video-player-command' is unset"))

  (let* ((player-cmd-args (split-string telega-video-player-command " " t))
         (proc (apply #'start-process "telega-video-player" nil
                      (append player-cmd-args (list filename)))))
    (telega-debug "video-player RUN: %s %s"
                  telega-video-player-command filename)

    (set-process-plist proc (list :done-callback done-callback))
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc 'telega-video-player--sentinel)
    proc))

(provide 'telega-ffplay)

;;; telega-ffplay.el ends here

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
(require 'rx)

(require 'telega-core)

(defvar telega-ffplay-media-timestamp nil
  "Bind this variable to start playing at the given media timestamp.")

(defconst telega-ffplay-buffer-name
  (concat (unless telega-debug " ") "*ffplay telega*"))

(defconst telega-ffplay--check-regexp
  (rx string-start
      space
      (group (any ?. ?D)) ; decoder
      (group (any ?. ?E)) ; encoder
      (0+ (not space))
      space
      (group (0+ (not (any ?= space)))) ;
      space
      (0+ any)
      string-end)
  "Regexp to match output ffmpeg's output.")

(defun telega-ffplay--check-ffmpeg-output (option &rest names)
  "Check the ffmpeg output for the OPTION for the NAMES.
Return cons cell, where car is a list of decoders and cdr is a list of
encoders."
  (let* ((output (shell-command-to-string (concat "ffmpeg -v quiet " option)))
         (decoders nil)
         (encoders nil))
    (dolist (line (split-string output "\n" t))
      (when (string-match telega-ffplay--check-regexp line)
        (let ((d (match-string 1 line))
              (e (match-string 2 line))
              (codec (match-string 3 line))
              (enc-list (when (string-match "(encoders: \\([^)]+\\))" line)
                          (split-string (match-string 1 line) " " t " ")))
              (dec-list (when (string-match "(decoders: \\([^)]+\\))" line)
                          (split-string (match-string 1 line) " " t " "))))
          (when (member codec names)
            (when (string= "D" d)
              (setq decoders (nconc decoders (cons codec dec-list))))
            (when (string= "E" e)
              (setq encoders (nconc encoders (cons codec enc-list))))))))
    (cons decoders encoders)))

(defconst telega-ffplay--codecs
  (telega-ffplay--check-ffmpeg-output
   "-codecs" "opus" "hevc" "aac" "h264" "webp" "vp9")
  "Cons, where car is a list of encoders, and cdr is a list of decoders.")

(defun telega-ffplay-has-encoder-p (codec-name)
  "Return non-nil if ffmpeg supports CODEC-NAME as encoder."
  (member codec-name (cdr telega-ffplay--codecs)))

(defun telega-ffplay-has-decoder-p (codec-name)
  "Return non-nil if ffmpeg supports CODEC-NAME as decoder."
  (member codec-name (car telega-ffplay--codecs)))

(defun telega-ffplay-proc ()
  "Return current ffplay process."
  (let ((buf (get-buffer telega-ffplay-buffer-name)))
    (when (buffer-live-p buf)
      (get-buffer-process buf))))

(defun telega-ffplay-progress (proc)
  "Return current ffplay progress."
  (when-let* ((ffproc proc)
              (proc-plist (process-plist ffproc)))
    (plist-get proc-plist :progress)))

(defun telega-ffplay-pause (ffproc &optional pause-at no-callback-p)
  "Pause ffplay process FFPROC.
PAUSE-AT is the moment to pause at, by default pause at
current `(telega-ffplay-progress)'.
Specify non-nil NO-CALLBACK-P to ignore ffplay callbacks."
  (when (process-live-p ffproc)
    (telega-debug "ffplay PAUSE at %S"
                  (or pause-at (telega-ffplay-progress ffproc)))
    ;; Do not trigger callbacks, ffplay is about to be restarted
    (when no-callback-p
      (let ((proc-plist (process-plist ffproc)))
        (plist-put proc-plist :callback nil)
        (plist-put proc-plist :progress-callback nil)
        (set-process-plist ffproc proc-plist)))

    (unless pause-at
      (setq pause-at (or (telega-ffplay-progress ffproc) 0)))
    ;; Fix negative value when backward forwarding
    (when (< pause-at 0)
      (setq pause-at 0))
    (telega-ffplay-stop ffproc (cons 'paused pause-at))))

(defun telega-ffplay-playing-p (proc)
  "Return non-nil if ffplay PROC is running."
  (process-live-p proc))

(defun telega-ffplay-stop-reason (proc)
  "Return stop reason for ffplay process PROC."
  (when-let* ((ffproc proc)
              (proc-plist (process-plist ffproc)))
    (plist-get proc-plist :stop-reason)))

(defun telega-ffplay-paused-p (proc)
  "Return non-nil if PROC as been paused by `telega-ffplay-pause'.
If ffplay is paused, then return progress at which ffplay has been
paused."
  (let ((stop-reason (telega-ffplay-stop-reason proc)))
    (when (and (consp stop-reason) (eq 'paused (car stop-reason)))
      (cdr stop-reason))))

(defun telega-ffplay-resume (&optional proc)
  "Resume ffplay process PROC."
  ;; SIGCONT to resume ffplay
  (let ((ffproc (or proc (telega-ffplay-proc))))
    (when (process-live-p ffproc)
      (telega-debug "ffplay RESUME")
      (signal-process ffproc 18)
;      (continue-process ffproc t)
      )))

(defun telega-ffplay-stop (&optional proc stop-reason)
  "Stop running ffplay process."
  (when-let* ((ffproc (or proc (telega-ffplay-proc)))
              (buf (process-buffer ffproc)))
    (when (buffer-live-p buf)
      (plist-put (process-plist ffproc) :stop-reason
                 (or stop-reason 'killed))
      (kill-buffer buf)

      ;; Wait for process to die?  `inhibit-quit' is non-nil if
      ;; already running under process output
      (while (and (process-live-p ffproc) (null inhibit-quit))
        (accept-process-output ffproc)))))

(defun telega-ffplay--sentinel (proc _event)
  "Sentinel for the ffplay process."
  ;; NOTE: Killing "docker exec" process won't kill underlining
  ;; process running in the docker container, because process could be
  ;; running under shell
  (when telega-use-docker
    (shell-command-to-string
     ;; NOTE: We use killall from "psmisc" package, this killall tool
     ;; supports --wait flag to wait for process to terminate
     (telega-docker-exec-cmd "/usr/bin/killall --quiet --wait ffmpeg ffplay"
                             nil nil 'no-error)))

  (telega-debug "ffplay SENTINEL: status=%S, live=%S, callback=%S"
                (process-status proc) (process-live-p proc)
                (plist-get (process-plist proc) :progress-callback))

  (unless (process-live-p proc)
    (let* ((proc-plist (process-plist proc))
           (callback (plist-get proc-plist :progress-callback)))
      (unless (telega-ffplay-stop-reason proc)
        ;; Process exited gracefully
        (plist-put proc-plist :stop-reason 'finished)
        (set-process-plist proc proc-plist))

      (when callback
        (funcall callback proc))

      ;; NOTE: sentinel might be called multiple times with 'exit
      ;; status, handle this situation simply by unsetting callback
      (set-process-plist proc (plist-put proc-plist :progress-callback nil)))))

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

(defun telega-ffplay-run (filename ffplay-args &optional callback)
  "Start ffplay to play FILENAME.
FFPLAY-ARGS is additional arguments string for the ffplay.
CALLBACK is called on updates with single argument - process.
Return newly created process."
  (declare (indent 2))
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
    (telega-debug "ffplay START: %s %s"
                  ffplay-bin (mapconcat #'identity args " "))
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
  "Return fps ratio for the FILENAME video file.
Return list where first element is <fps_numerator> and second element
is <fps_denominator>.  f.i. \\(30000 1001\\) is returned for 29.97fps.
If fps is not available for FILENAME, then return DEFAULT or \\(30 1\\)."
  (let ((fps-ratio (telega-strip-newlines
                    (shell-command-to-string
                     (telega-docker-exec-cmd
                       (concat "ffprobe -v error -select_streams v:0 "
                               "-show_entries stream=r_frame_rate "
                               "-of default=noprint_wrappers=1:nokey=1 "
                               "\"" (expand-file-name filename) "\"")
                       'try-host-cmd-first)))))
    (if (string-match "\\([0-9]+\\)/\\([0-9]+\\)" fps-ratio)
        (list (string-to-number (match-string 1 fps-ratio))
              (string-to-number (match-string 2 fps-ratio)))
      (or default '(30 1)))))

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

(defun telega-ffplay--png-sentinel (proc event)
  "Sentinel for png extractor, see `telega-ffplay-to-png'."
  (telega-debug "ffplay-png SENTINEL: status=%S, live=%S, callback=%S"
                (process-status proc) (process-live-p proc)
                (plist-get (process-plist proc) :callback))

  ;; NOTE: `telega-ffplay--sentinel' has some docker-related
  ;; logic, and sets stop reason
  (telega-ffplay--sentinel proc event)

  (unless (process-live-p proc)
    (let* ((proc-plist (process-plist proc))
           (callback (plist-get proc-plist :callback))
           (callback-args (plist-get proc-plist :callback-args))
           (paused-p (telega-ffplay-paused-p proc))
           (all-frames (plist-get proc-plist :frames)))
      ;; DONE executing.
      ;; If paused, show last frame
      (when callback
        (apply callback proc (when paused-p (car all-frames)) callback-args))

      ;; Delete all produced files for frames
      ;; If paused, keep last shown frame
      (dolist (frame (if paused-p (cdr all-frames) all-frames))
        (delete-file (cdr frame)))

      ;; NOTE: sentinel might be called multiple times with 'exit
      ;; status, handle this situation simply by unsetting callback
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

(defun telega-ffplay-to-png--internal (ffmpeg-args callback &optional
                                                   callback-args pngext-args)
  "Play video FILENAME extracting png images from it.
FFMPEG-ARGS is a string for additional arguments to ffmpeg.
PNGEXT-ARGS is a string for additional arguments to pngextractor."
  (declare (indent 1))

  (cl-assert (string-suffix-p " -vcodec png -" ffmpeg-args))
  ;; Stop any ffplay running
  (telega-ffplay-stop)

  ;; Start new ffmpeg
  (with-current-buffer (get-buffer-create telega-ffplay-buffer-name)
    (let* ((prefix (telega-temp-name "png-video"))
           (ffmpeg-bin (executable-find "ffmpeg"))
           (ffmpeg-cmd-args
            (concat " -hide_banner -loglevel quiet"
                    (when ffmpeg-args
                      (concat " " ffmpeg-args))))
           (pngext-cmd-args
            (concat "-E " prefix
                    (when pngext-args
                      (concat " " pngext-args))))
           (shell-cmd
            (cond (ffmpeg-bin
                   (format "%s %s | %s %s" ffmpeg-bin ffmpeg-cmd-args
                           (telega-docker-exec-cmd telega-server-command
                                                   'try-host-cmd-first "-i")
                           pngext-cmd-args))
                  (telega-use-docker
                   (telega-docker-exec-cmd
                    (format "sh -c \"ffmpeg %s | telega-server %s\""
                            ffmpeg-cmd-args pngext-cmd-args)))
                  (t
                   (error "ffmpeg not found in `exec-path'"))))
           (process-adaptive-read-buffering nil) ;no buffering please
           (proc (start-process-shell-command
                  "ffmpeg" (current-buffer) shell-cmd)))
      (telega-debug "ffplay RUN: %s" shell-cmd)
      (set-process-plist proc (list :prefix prefix
                                    :nframes -1
                                    :frames nil
                                    :callback callback
                                    :callback-args callback-args))
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel proc 'telega-ffplay--png-sentinel)
      (set-process-filter proc 'telega-ffplay--png-filter)
      proc)))

(cl-defun telega-ffplay-to-png (filename ffmpeg-args callback-spec
                                         &key seek speed vcodec)
  "Play video FILENAME extracting png images from it.
FFMPEG-ARGS is a string for additional arguments to ffplay.

CALLBACK-SPEC specifies a callback to be used.  car of the
CALLBACK-SPEC is a function to be called and rest are additional
arguments to that function.
Callback is called with args: <proc> <filename.png> <additional-arguments>.
Callback is called with nil filename when finished.
SEEK specifies seek position to start playing from.
SPEED specifies a speed of png images extraction, default is 1 (realtime).
Return newly created proc."
  (declare (indent 2))
  (let* ((fps-ratio (let ((fn-fps (telega-ffplay-get-fps-ratio
                                   (expand-file-name filename))))
                      (if (or (not speed) (equal speed 1))
                          fn-fps
                        ;; Adjust fps ratio according to SPEED
                        (list (* speed (nth 0 fn-fps)) (nth 1 fn-fps)))))
         (callback (if (listp callback-spec)
                       (car callback-spec)
                     (cl-assert (functionp callback-spec))
                     callback-spec))
         (callback-args (when (listp callback-spec)
                          (cdr callback-spec)))
         (proc (telega-ffplay-to-png--internal
                (concat (when seek
                          (format " -ss %.2f" seek))
                        (when vcodec
                          (concat " -vcodec " vcodec))
                        " -i '" (expand-file-name filename) "'"
                        (when ffmpeg-args
                          (concat " " ffmpeg-args))
                        " -f image2pipe -vcodec png -")
                callback callback-args
                (concat "-f " (mapconcat #'int-to-string fps-ratio "/"))))
         (proc-plist (process-plist proc)))
    ;; Adjust `:nframes' and `:fps-ratio' for correct progress calculation
    (plist-put proc-plist :nframes (telega-ffplay-get-nframes filename))
    (set-process-plist proc proc-plist)
    proc))


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

(defun telega-video-player-run (filename &optional msg done-callback)
  "Start playing video FILENAME with `telega-video-player-command' command.
DONE-CALLBACK - callback to call, when done viewing video."
  (declare (indent 2))
  (unless telega-video-player-command
    (user-error "telega: `telega-video-player-command' is unset"))

  (let* ((video-player-cmd
          (if (listp telega-video-player-command)
              (eval telega-video-player-command)
            (cl-assert (stringp telega-video-player-command))
            telega-video-player-command))
         (player-cmd-args (split-string video-player-cmd " " t))
         (proc (apply #'start-process "telega-video-player" nil
                      (append player-cmd-args (list filename)))))
    (telega-debug "video-player RUN: %s %s" video-player-cmd filename)

    (set-process-plist proc (list :done-callback done-callback
                                  :message msg))
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc 'telega-video-player--sentinel)
    proc))

(provide 'telega-ffplay)

;;; telega-ffplay.el ends here

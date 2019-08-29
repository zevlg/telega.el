;;; telega-vvnote.el --- Voice/Video notes support for telega  -*- lexical-binding:t -*-

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
(require 'telega-ffplay)

(defcustom telega-vvnote-voice-max-dur (* 30 60)
  "Maximum duration of voice command in seconds."
  :type 'number
  :group 'telega-vvnote)

(defcustom telega-vvnote-waves-colors '("#006600" . "#229922")
  "Colors to display voice note waves."
  :type 'cons
  :group 'telega-vvnote)

(defcustom telega-vvnote-voice-play-next t
  "*After playing voice note continue playing next voice note in the chat."
  :type 'boolean
  :group 'telega-vvnote)

(defcustom telega-vvnote-voice-cmd "ffmpeg -f alsa -i default -strict -2 -acodec opus -ab 30k"
  "Command to use to save voice notes."
  :type 'string
  :group 'telega-vvnote)

(defcustom telega-vvnote-video-cmd "ffmpeg -f v4l2 -s 320x240 -i /dev/video0 -r 30 -f alsa -i default -vf format=yuv420p,crop=240:240:40:0 -strict -2 -vcodec hevc -acodec opus -vb 300k -ab 30k"
  "Command to use to save video notes."
  :type 'string
  :group 'telega-vvnote)

(defun telega-vvnote--waves-squeeze (waves factor)
  "Squeeze the decoded WAVES by FACTOR."
  ;; Squeeze by averaging and normalizing
  (let* ((min-o (apply 'min waves))
         (max-o (apply 'max waves))
         (nw (mapcar (lambda (p)
                       (/ (apply '+ p) factor))
                     (seq-partition waves factor)))
         (min-n (apply 'min nw))
         (min-normw (mapcar (lambda (v) (- v min-n)) nw))
         (n-factor (/ (- max-o min-o) (apply 'max min-normw))))
    (mapcar (lambda (v) (+ min-o (* v n-factor))) min-normw)))

(defun telega-vvnote--waves-svg (waves height duration &optional played)
  "From decoded WAVES create svg of HEIGHT for DURATION and PLAYED."
  (cl-assert (> height 8))
  (let* ((w-idx 0)
         (wv-width 3) (space-width 2)
         (wv-height (- height 6))
         (w (* (+ wv-width space-width) (length waves)))
         (aw-chars (telega-chars-in-width w))
         (cw (telega-chars-width aw-chars))
         (svg (svg-create cw height)))
    ;; bg - "#e1ffc7", fg - "#93d987", fg-played - "#3fc33b"
    ;;    (svg-rectangle svg 0 0 w h :fill-color "#e1ffc7")
    (dolist (wv waves)
      (let ((xoff (+ wv-width (* (+ wv-width space-width) w-idx)))
            (played-p (< (/ (float w-idx) (length waves))
                         (/ (or played 0) (if (zerop duration) 0.1 duration)))))
        (svg-line svg xoff (- height 3 (if played-p 0.5 0)) xoff
                  (- height 3 (if played-p 0.5 0) (* wv wv-height))
                  :stroke-color (if played-p
                                    (car telega-vvnote-waves-colors)
                                  (cdr telega-vvnote-waves-colors))
                  :stroke-width (if played-p (1+ wv-width) wv-width)
                  :stroke-linecap "round")
        (cl-incf w-idx)))
    (svg-image svg :scale 1
               :width cw :height height
               :mask 'heuristic
               :ascent 'center
               ;; text of correct width
               :telega-text (make-string aw-chars ?#))))

(defun telega-vvnote--waveform-decode (waveform)
  "Decode WAVEFORM returning list of heights.
heights are normalized to [0-1] values."
  (let ((bwv (base64-decode-string waveform))
        (cc 0) (cv 0) (needbits 5) (leftbits 8) result)
    (while (not (string-empty-p bwv))
      (setq cc (logand (aref bwv 0) (lsh 255 (- leftbits 8))))
      (when (<= leftbits needbits)
        (setq bwv (substring bwv 1)))

      (if (< leftbits needbits)
          ;; Value not yet ready
          (setq cv (logior (lsh cv leftbits) cc)
                needbits (- needbits leftbits)
                leftbits 8)

        ;; Ready (needbits <= leftbits)
        (push (logior (lsh cv needbits)
                      (lsh cc (- needbits leftbits)))
              result)
        (setq leftbits (- leftbits needbits)
              needbits 5
              cv 0)
        (when (zerop leftbits)
          (setq leftbits 8))))
    (mapcar (lambda (v) (/ v 31.0)) (nreverse result))))

(defun telega-vvnote--video-svg (framefile &optional duration progress)
  "Generate svg image for the video note FRAMEFILE.
DURATION is overall duration of the video note.
PROGRESS is current frame progress."
  (let* ((img-type (image-type-from-file-name framefile))
         (size (* (frame-char-height) telega-vvnote-video-height))
         (h size)
         (w (* (telega-chars-width 1) (telega-chars-in-width size)))
         (xoff (/ (- w size) 2))
         (yoff (/ (- h size) 2))
         (svg (svg-create w h))
         (clip (telega-svg-clip-path svg "clip"))
         (clip1 (telega-svg-clip-path svg "clip1")))
    (svg-circle clip (/ w 2) (/ h 2) (/ size 2))
    (svg-embed svg framefile
               (format "image/%S" img-type) nil
               :x xoff :y yoff
               :width size :height size
               :clip-path "url(#clip)")

    (when (and duration progress)
      (let* ((angle-o (* 2 pi (/ progress duration)))
             (angle (+ (* 2 pi (- (/ progress duration))) pi))
             (dx (+ (* (/ size 2) (sin angle)) (/ size 2)))
             (dy (+ (* (/ size 2) (cos angle)) (/ size 2))))
        ;; clip mask for the progress circle
        (let ((cp (format "M %d %d L %d %d L %d 0" (/ w 2) (/ h 2) (/ w 2) 0 w)))
          (when (> angle-o (/ pi 2))
            (setq cp (concat cp (format " L %d %d" w h))))
          (when (> angle-o pi)
            (setq cp (concat cp (format " L 0 %d" h))))
          (when (> angle-o (/ (* 3 pi) 2))
            (setq cp (concat cp (format " L 0 0"))))
          (setq cp (concat cp (format " L %d %d" (+ dx xoff) (+ dy yoff))))
          (setq cp (concat cp " Z"))
          (telega-svg-path clip1 cp))
        ;; Progress circle itself
        (svg-circle svg (/ w 2) (/ h 2) (- (/ size 2) 4)
                    :fill "none"
                    :stroke-width (/ size 30)
                    :stroke-opacity "0.35"
                    :stroke-color "white"
                    :clip-path "url(#clip1)")))

    (svg-image svg :scale 1.0 :ascent 'center)))


;; Play video note
;; Idea taken from https://github.com/chep/chep-video.el
(defun telega-vvnote--recent-frame (proc)
  "Return recent file and its frame-num.
Return `nil' if there is no recent file."
  (let* ((proc-plist (process-plist proc))
         (frame-num (plist-get proc-plist :frame))
         (prefix (plist-get proc-plist :prefix))
         (framefile (format "%s%05d.png" prefix frame-num)))
    (when (file-exists-p framefile)
      (set-process-plist proc (plist-put proc-plist :frame (1+ frame-num)))
      (cons framefile frame-num))))

(defun telega-vvnote--run-frame (proc frame)
  "Run PROC's callback (if any) on the FRAME.
Return non-nil if callback has been executed and frame deleted."
  (when frame
    (let* ((proc-plist (process-plist proc))
           (callback (plist-get proc-plist :callback))
           (callback-args (plist-get proc-plist :callback-args))
           (fps (or (plist-get proc-plist :fps) 30))
           (progress (/ (cdr frame) (float fps))))
      (set-process-plist proc (plist-put proc-plist :progress progress))
      (when callback
        (apply callback (car frame) progress callback-args))
      (delete-file (car frame))
      t)))

(defun telega-vvnote--start-timer (proc)
  "Start vvnote timer for the PROC."
  (let* ((proc-plist (process-plist proc))
         (timer (plist-get proc-plist :timer))
         (fps (plist-get proc-plist :fps)))
    (unless timer
      (setq timer (run-with-timer 0 (/ 1.0 fps) 'telega-vvnote--next-frame proc))
      (set-process-plist proc (plist-put proc-plist :timer timer)))))

(defun telega-vvnote--cancel-timer (proc)
  "Stop vvnote timer associated with PROC."
  (let* ((proc-plist (process-plist proc))
         (timer (plist-get proc-plist :timer)))
    (when timer
      (cancel-timer timer)
      (set-process-plist proc (plist-put proc-plist :timer nil)))))

(defun telega-vvnote--next-frame (proc)
  "Timer triggered, display next frame."
  (condition-case err
      ;; Run until the frame exists
      (unless (or (telega-vvnote--run-frame
                   proc (telega-vvnote--recent-frame proc))
                  (process-live-p proc))
        ;; Done playing, nil nil args mean DONE
        (let ((callback (plist-get (process-plist proc) :callback))
              (callback-args (plist-get (process-plist proc) :callback-args)))
          (apply callback nil nil callback-args)
          (telega-vvnote--cancel-timer proc)))
    (error
     (telega-vvnote--cancel-timer proc)
     (message "telega: error in vvnote callback: %S" err))))

(defun telega-vvnote--ffmpeg-sentinel (proc _event)
  "Sentinel for the ffmpeg process."
  (telega-debug "vvnote SENTINEL: status=%S, live=%S"
                (process-status proc) (process-live-p proc))

  ;; NOTE: timer will do all the job
  (cl-case (process-status proc)
    (run (telega-vvnote--start-timer proc))   ;resume
    (stop (telega-vvnote--cancel-timer proc)) ;pause
    (t (unless (plist-get (process-plist proc) :timer)
         ;; Done playing, nil nil args mean DONE
         (let ((callback (plist-get (process-plist proc) :callback))
               (callback-args (plist-get (process-plist proc) :callback-args)))
           (unwind-protect
               (apply callback nil nil callback-args)

           (telega-vvnote--cancel-timer proc)))

         (set-process-plist proc (plist-put (process-plist proc) :callback nil))
         (while (telega-vvnote--run-frame
                 proc (telega-vvnote--recent-frame proc))
           ;; no-op
           )))))

(defun telega-vvnote--ffmpeg-filter (proc output)
  "Filter for the ffmpeg process."
  (let ((buffer (process-buffer proc))
        (proc-plist (process-plist proc)))
    (unless (plist-get proc-plist :fps)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert output)
          ;; Search for the fps info
          (when (re-search-backward ", \\([0-9.]+\\) fps," nil t)
            (set-process-plist
             proc (plist-put proc-plist :fps
                             (string-to-number (match-string 1))))
            ;; Restart timer according to updated fps value
            (telega-vvnote--cancel-timer proc)
            (telega-vvnote--start-timer proc)
            ))))))

(defun telega-vvnote-play-video (videofile callback &rest callback-args)
  "Play video note using ffmpeg.
Play audio to ALSA output device.
And video to series of PNG images.
CALLBACK is called on updates with first two arguments - file to
show and progress and rest CALLBACK-ARGS.
File and progress are nil when file has been successfuly played."
  (declare (indent 1))
  ;; Stop any ffplay running
  (telega-ffplay-stop)

  ;; Start new ffmpeg
  (with-current-buffer (get-buffer-create telega-ffplay-buffer-name)
    (let* ((prefix (telega-temp-name "vvnote"))
           (ffmpeg-args (list "-hide_banner"
                              "-i" (expand-file-name videofile)
                              "-vf" "scale=240:240"
                              "-f" "alsa" "default"
                              "-vsync" "0"
                              (format "%s%%05d.png" prefix)))
           (proc (apply 'start-process "ffmpeg" (current-buffer)
                        (or (executable-find "ffmpeg")
                            (error "ffmpeg not found in `exec-path'"))
                        ffmpeg-args)))

      (set-process-plist proc (list :prefix prefix
                                    :frame 1
                                    :fps 30.0
                                    :callback callback
                                    :callback-args callback-args))
      (telega-vvnote--start-timer proc)
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel proc #'telega-vvnote--ffmpeg-sentinel)
      (set-process-filter proc #'telega-vvnote--ffmpeg-filter)
      proc)))

(defun telega-vvnote-video--create-image (thumb &optional _file)
  "Create image for video note frame THUMB."
  (let ((file (telega-file--renew thumb :photo)))
    (if (telega-file--downloaded-p file)
        (telega-vvnote--video-svg
         (telega--tl-get file :local :path))

      (let* ((cheight telega-vvnote-video-height)
             (x-size (* (frame-char-height) cheight)))
        (telega-media--progress-svg file x-size x-size cheight)))))

(provide 'telega-vvnote)

;;; telega-vvnote.el ends here

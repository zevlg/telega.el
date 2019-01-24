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

(defun telega-vvnote--waveform-decode (waveform)
  "Decode WAVEFORM returning list of heights."
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
    (nreverse result)))

(defun telega-vvnote-waveform-svg (waveform height duration &optional played)
  "Create SVG image for the voice note with WAVEFORM and DURATION."
  ;; TODO: use HEIGHT
  (let* ((wfd (telega-vvnote--waveform-decode waveform))
         (wfd-idx 0)
         (wv-width 3) (space-width 2)
         (w (* (+ wv-width space-width) (length wfd)))
         (need-h 36)
         (h height)
         (h 36)
         (svg (svg-create w h)))
    ;; bg - "#e1ffc7", fg - "#93d987", fg-played - "#3fc33b"
;    (svg-rectangle svg 0 0 w h :fill-color "#e1ffc7")
    (cl-dolist (wv wfd)
      (let ((xoff (+ wv-width (* (+ wv-width space-width) wfd-idx)))
            (played-p (< (/ (float wfd-idx) (length wfd))
                         (/ (or played 0) duration))))
        (svg-line svg xoff h xoff (- h (+ wv 1))
                  :stroke-color (if played-p "#006400" "#228b22")
                  :stroke-width (if played-p (1+ wv-width) wv-width)
                  :stroke-linecap "round")
        (cl-incf wfd-idx)))
    (svg-image svg :scale 1 :ascent 'center)))


;; Play video note
(defvar telega-vvnote--timer nil))

(defun telega-vvnote--ffmpeg-sentinel (proc event)
  "Sentinel for the ffmpeg process."
  (let ((pcb (plist-get (process-plist proc) :progress-callback)))
    (when pcb
      ;; nil progress mean DONE
      (funcall pcb nil))))

(defun telega-vvnote-play-video (filename callback &rest ffplay-args)
  "Play video note using ffmpeg.
Play audio to ALSA output device.
And video to series of PNG images.
CALLBACK is called on updates with single argument - progress.
progress is either float (in seconds) or nil (on ffplay exit).
CALLBACK with `nil' argument is not called if ffplay was stopped
prematurely, i.e. with explicit call to `telega-ffplay-stop'.
FFPLAY-ARGS is additional args to the ffplay."
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
        (set-process-plist proc (list :tmprefix
                                      :progress-callback callback
                                      :progress 0.0))
        (set-process-query-on-exit-flag proc nil)
        (set-process-sentinel proc #'telega-ffplay--sentinel)
        (set-process-filter proc #'telega-ffplay--filter)
        proc))))

(provide 'telega-vvnote)

;;; telega-vvnote.el ends here

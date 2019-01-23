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

(provide 'telega-vvnote)

;;; telega-vvnote.el ends here

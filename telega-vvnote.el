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
         (cw (telega-chars-xwidth aw-chars))
         (svg (telega-svg-create cw height)))
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

(defun telega-vvnote-video--svg (framefile &optional progress
                                           data-p frame-img-type)
  "Generate svg image for the video note FRAMEFILE.
PROGRESS is value from 0 to 1 indicating played content.
PROGRESS might be nil.
If DATA-P is non-nil then FRAMEFILE is actually an image data.
If DATA-P is non-nil then FRAME-IMG-TYPE specifies type of the image."
  (let* ((img-type (or frame-img-type (image-type-from-file-name framefile)))
         (size (telega-chars-xheight telega-video-note-height))
         (h size)
         (aw-chars (telega-chars-in-width size))
         (w (telega-chars-xwidth aw-chars))
         (xoff (/ (- w size) 2))
         (yoff (/ (- h size) 2))
         (svg (telega-svg-create w h))
         (clip (telega-svg-clip-path svg "clip"))
         (clip1 (telega-svg-clip-path svg "clip1")))
    (svg-circle clip (/ w 2) (/ h 2) (/ size 2))
    (svg-embed svg framefile
               (format "image/%S" img-type) data-p
               :x xoff :y yoff
               :width size :height size
               :clip-path "url(#clip)")

    (when progress
      (let* ((angle-o (* 2 pi progress))
             (angle (+ (* 2 pi (- progress)) pi))
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

    (svg-image svg :scale 1.0
               :width w :height h
               :mask 'heuristic
               :ascent 'center
               :telega-text (make-string aw-chars ?#))))

(defun telega-vvnote-video--create-image (note &optional _file)
  "Create image for video NOTE frame."
  (let* ((thumb (plist-get note :thumbnail))
         (thumb-file (telega-file--renew thumb :file))
         (minithumb (plist-get note :minithumbnail)))
    (cond ((telega-file--downloaded-p thumb-file)
           (telega-vvnote-video--svg
            (telega--tl-get thumb-file :local :path)))
          (minithumb
           (telega-vvnote-video--svg
            (base64-decode-string (plist-get minithumb :data))
            nil t 'jpeg))
          (t
           (let* ((cheight telega-video-note-height)
                  (x-size (telega-chars-xheight cheight)))
             (telega-media--progress-svg thumb-file x-size x-size cheight))))))

(provide 'telega-vvnote)

;;; telega-vvnote.el ends here

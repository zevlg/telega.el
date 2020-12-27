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
(require 'bindat)                       ;binary parsing

(require 'telega-ffplay)
(require 'telega-util)                  ;`with-telega-symbol-animate'

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

(defcustom telega-vvnote-voice-cmd
  (if (telega-ffplay-check-codec "opus" '(encoder))
      ;; Try OPUS if available, results in 3 times smaller files then
      ;; AAC version with same sound quality
      "ffmpeg -f alsa -i default -strict -2 -acodec opus -ab 32k"
    ;; Fallback to AAC
    "ffmpeg -f alsa -i default -acodec aac -ab 96k")
  "Command to use to save voice notes."
  :package-version '(telega . "0.7.4")
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

;; Encoding and Decoding splits into two situations:
;;     head-bits=5   tail-bits=0
;;   v v v v v
;; 0 1 2 3 4 5 6 7   0 1 2 3 4 5 6 7
;; * X X X X X * *   * * * * * * * *
;;   `-- bitoff here            bwv1
;; and:
;;     head-bits=3   tail-bits=2
;;           v v v   v v
;; 0 1 2 3 4 5 6 7   0 1 2 3 4 5 6 7
;; * * * * * X X X   X X * * * * * *
;; bwv0      `-- bitoff here    bwv1
;;
;; Handle them both correctly
(defun telega-vvnote--waveform-decode (waveform &optional raw-p)
  "Decode WAVEFORM returning list of heights.
Unless RAW-P is given, heights are normalized to [0-1] values."
  (let ((bwv (base64-decode-string waveform))
        (bitoff 0)
        result)
    (while (not (string-empty-p bwv))
      (let* ((bwv0 (aref bwv 0))
             (bwv1 (if (> (length bwv) 1) (aref bwv 1) 0))
             (head-bits (min 5 (- 8 bitoff)))
             (tail-bits (- 5 head-bits))
             (h-value (lsh (logand bwv0 (lsh 255 (- bitoff)))
                           (- (+ bitoff head-bits) 8)))
             (t-value (lsh bwv1 (- tail-bits 8))))
        (setq result (cons (logior (lsh h-value tail-bits) t-value) result)
              bitoff (+ bitoff 5))
        (when (>= bitoff 8)
          (cl-assert (= (- bitoff 8) tail-bits))
          (setq bwv (substring bwv 1)
                bitoff (- bitoff 8)))))
    (mapcar (lambda (v) (if raw-p v (/ v 31.0))) (nreverse result))))

(defun telega-vvnote--waveform-encode (waveform)
  "Encode WAVEFORM into base64 based form.
WAVEFORM is list of integers each in range [0-31] to fit into 5 bits."
  (cl-assert (cl-every (apply-partially #'>= 31) waveform))
  (let ((bytes nil)                     ; resulting bytes
        (bitoff 0)                      ; bit offset inside `value'
        (value 0))                      ; currently composing value
    (dolist (w-value waveform)
      (cl-assert (< bitoff 8))
      (let* ((head-bits (min 5 (- 8 bitoff)))
             (tail-bits (- 5 head-bits))
             (h-value (lsh (logand w-value (lsh 31 tail-bits)) (- tail-bits)))
             (t-value (logand w-value (lsh 31 (- head-bits)))))
        ;; Write head-bits
        (setq value (logior value (lsh h-value (- 8 (+ bitoff head-bits))))
              bitoff (+ bitoff 5))
        ;; Write tail bits
        (when (>= bitoff 8)
          (cl-assert (= (- bitoff 8) tail-bits))
          (setq bytes (cons value bytes)
                bitoff (- bitoff 8)
                value (lsh t-value (- 8 bitoff))))))

    (base64-encode-string (apply #'unibyte-string (nreverse bytes)) t)))

(defun telega-vvnote--wav-samples ()
  "Parse current buffer as wav file extracting audio samples.
Each sample is in range [-128;128]."
  (let* ((wav-data (buffer-string))
         (wav-bindat-spec
          '((:riff str 4)                 ;"RIFF"
            (:chunk-size u32r)
            (:format str 4)               ;"WAVE"
            (:subchunk1-id str 4)         ;"fmt "
            (:subchunk1-sz u32r)
            (:audio-format u16r)
            (:num-channels u16r)
            (:sample-rate u32r)
            (:byte-rate u32r)
            (:block-align u16r)
            (:bits-per-sample u16r)
            (:subchunk2-id str 4)         ;"data" or "LIST"
            (:subchunk2-sz u32r)))
         (wav-header
          (bindat-unpack wav-bindat-spec wav-data))
         (data-offset 44)
         (data-size (bindat-get-field wav-header :subchunk2-sz)))
    (cl-assert (string= "RIFF" (bindat-get-field wav-header :riff)))
    (cl-assert (string= "WAVE" (bindat-get-field wav-header :format)))
    (cl-assert (string= "fmt " (bindat-get-field wav-header :subchunk1-id)))
    (when (string= "LIST" (bindat-get-field wav-header :subchunk2-id))
      (let ((data-header (bindat-unpack
                          `((:skipped fill ,data-size)
                            (:subchunk3-id str 4)
                            (:subchunk3-sz u32r))
                          wav-data data-offset)))
        (setq data-offset (+ data-offset data-size 8)
              data-size (bindat-get-field data-header :subchunk3-sz))))

    (mapcar (lambda (v) (- v 128))
            (bindat-get-field (bindat-unpack `((:wave vec ,data-size u8))
                                             wav-data data-offset)
                              :wave))
    ))

(defun telega-vvnote--waveform-for-file (filename)
  "Create a waveform for audio FILENAME."
  (let* ((temp-wav (telega-temp-name "audio" ".wav"))
         (samples (progn
                    ;; Use `shell-command-to-string' to avoid noisy
                    ;; message when shell completes
                    (shell-command-to-string
                     (concat "ffmpeg -v error "
                             "-i \"" (expand-file-name filename) "\" "
                             "-ar 8000 -ac 1 "
                             "-acodec pcm_u8 \"" temp-wav "\""))
                    (unless (file-exists-p temp-wav)
                      (error "telega: Can't extract waves from %s" filename))
                    (unwind-protect
                        (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally temp-wav)
                          (telega-vvnote--wav-samples))
                      (delete-file temp-wav))))
         (frac-samples
          (seq-partition samples (1+ (/ (length samples) 96))))
         ;; Calculate loudness as root mean square, this gives "good
         ;; enough" results, suitable for our needs
         (loud-samples
          (mapcar (lambda (quant-samples)
                    (sqrt (/ (cl-reduce #'+ (mapcar (lambda (x) (* x x))
                                                    quant-samples))
                             (length quant-samples))))
                  frac-samples))
         (log10-samples
          (mapcar (lambda (ms) (if (> ms 0) (log ms 10) 0)) loud-samples))
         ;; Normalize values to fit into [0-31] so each value could be
         ;; encoded into 5bits
         (n-factor (/ 31.0 (apply #'max log10-samples)))
         (n-waves
          (mapcar (lambda (wave-value) (round (* wave-value n-factor)))
                  log10-samples)))
    (cl-assert (cl-every (apply-partially #'>= 31) n-waves))
    (telega-vvnote--waveform-encode n-waves)))

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
    (telega-svg-embed svg (if data-p
                              framefile
                            (list (file-name-nondirectory framefile)
                                  (file-name-directory framefile)))
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
               :base-uri (unless data-p framefile)
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


;; Recording notes
(defvar telega-vvnote-voice--record-progress nil
  "Current record progress.
Set to nil, when ffplay exists.")

(defun telega-vvnote-voice--record-callback (proc)
  "Progress callback for the voice note recording."
  (let ((proc-plist (process-plist proc)))
    (setq telega-vvnote-voice--record-progress
          (when (process-live-p proc)
            (plist-get proc-plist :progress)))))

(defun telega-vvnote-voice--record ()
  "Record a voice note.
Return filename with recorded voice note."
  (let* ((telega-vvnote-voice--record-progress 0)
         (note-file (telega-temp-name "voice-note" ".mp4"))
         (capture-proc (telega-ffplay-run-command
                        (concat telega-vvnote-voice-cmd " " note-file)
                        #'telega-vvnote-voice--record-callback)))
    ;; Wait for ffmpeg process to start
    (accept-process-output capture-proc)

    ;; Capture voice note
    (with-telega-symbol-animate 'audio 0.1 audio-sym
      ;; Animation exit condition 
      (or (not telega-vvnote-voice--record-progress)
          (not (process-live-p capture-proc)))

      ;; Animation body
      (message (concat "telega: "
                       (propertize "●" 'face 'error)
                       "VoiceNote " audio-sym
                       " " (telega-duration-human-readable
                            telega-vvnote-voice--record-progress)
                       " Press a key when done")))

    ;; Gracefully stop capturing and wait ffmpeg for exit
    (when (process-live-p capture-proc)
      (interrupt-process capture-proc)
      (while (process-live-p capture-proc)
        (accept-process-output capture-proc)))

    (unless (file-exists-p note-file)
      (error "telega: Can't capture voice note"))
    note-file))

(provide 'telega-vvnote)

;;; telega-vvnote.el ends here

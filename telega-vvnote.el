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
  :type '(cons color color)
  :group 'telega-vvnote)

(defcustom telega-vvnote-play-speed 1
  "*Playback speed for voice/video notes."
  :package-version '(telega . "0.7.52")
  :type 'number
  :group 'telega-vvnote)

(defcustom telega-vvnote-play-next t
  "*After playing voice note continue playing next voice note in the chat."
  :package-version '(telega . "0.7.52")
  :type 'boolean
  :group 'telega-vvnote)

(defconst telega-vvnote--has-audio-inputs
  (car (telega-ffplay--check-ffmpeg-output
        "-devices" "alsa" "pulse"))
  "List of audio inputs supported by telega.")

(defcustom telega-vvnote-voice-record-args
  (concat (if (eq system-type 'darwin)
              "-f avfoundation -i :default"
            ;; gnu/linux
            (concat "-f " (car telega-vvnote--has-audio-inputs)
                    " -i default"))
          (cond ((telega-ffplay-has-encoder-p "opus")
                 ;; Try OPUS if available, results in 3 times smaller
                 ;; files then AAC version with same sound quality
                 " -strict -2 -acodec opus -ac 1 -ab 32k")
                ((telega-ffplay-has-encoder-p "aac")
                 " -acodec aac -ac 1 -ab 96k")
                (t
                 " -acodec mp3 -ar 44100 -ac 1 -ab 96k")))
  "Arguments to ffmpeg to record a voice note."
  :package-version '(telega . "0.7.53")
  :type 'string
  :group 'telega-vvnote)

(defcustom telega-vvnote-video-record-args
  (concat (if (eq system-type 'darwin)
              "-f avfoundation -s 640x480 -framerate 30 -i default -r 30 -f avfoundation -i :default"
            ;; gnu/linux
            (concat "-f v4l2 -s 320x240 -i /dev/video0 -r 30 "
                    "-f " (car telega-vvnote--has-audio-inputs) " -i default"))
          " -vf format=yuv420p,scale=320:240,crop=240:240:40:0"
          " -vcodec " (if (telega-ffplay-has-encoder-p "hevc")
                          "hevc"
                        "h264")
          " -vb 300k"
          (if (telega-ffplay-has-encoder-p "opus")
              " -strict -2 -acodec opus -ac 1 -ab 32k"
            ;; Fallback to aac
            " -acodec aac -ac 1 -ab 96k"))
  "Arguments to ffmpeg to record a video note."
  :package-version '(telega . "0.7.53")
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
    (telega-svg-image svg :scale 1
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
             (h-value (ash (logand bwv0 (ash 255 (- bitoff)))
                           (- (+ bitoff head-bits) 8)))
             (t-value (ash bwv1 (- tail-bits 8))))
        (setq result (cons (logior (ash h-value tail-bits) t-value) result)
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
             (h-value (ash (logand w-value (ash 31 tail-bits)) (- tail-bits)))
             (t-value (logand w-value (ash 31 (- head-bits)))))
        ;; Write head-bits
        (setq value (logior value (ash h-value (- 8 (+ bitoff head-bits))))
              bitoff (+ bitoff 5))
        ;; Write tail bits
        (when (>= bitoff 8)
          (cl-assert (= (- bitoff 8) tail-bits))
          (setq bytes (cons value bytes)
                bitoff (- bitoff 8)
                value (ash t-value (- 8 bitoff))))))

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
                     (telega-docker-exec-cmd
                       (concat "ffmpeg -v error "
                               "-i \"" (expand-file-name filename) "\" "
                               "-ar 8000 -ac 1 "
                               "-acodec pcm_u8 \"" temp-wav "\"")
                       'try-host-cmd-first))
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
         ;; encoded into 5bits.
         ;; NOTE: Avoid division by 0 if all log10-samples are zeroes
         (n-factor (/ 31.0 (apply #'max 1 log10-samples)))
         (n-waves
          (mapcar (lambda (wave-value) (round (* wave-value n-factor)))
                  log10-samples)))
    (cl-assert (cl-every (apply-partially #'>= 31) n-waves))
    (telega-vvnote--waveform-encode n-waves)))

(defun telega-vvnote-video--svg (framefile &optional progress
                                           data-p frame-img-type)
  "Generate svg image for the video note FRAMEFILE.
PROGRESS is value from 0 to 1 indicating played content.
PROGRESS might be nil, meaning video not is not started.
PROGRESS also can be a cons cell, where car is `paused' and cdr is
value from 0 to 1, meaning video note is paused at given progress.
If DATA-P is non-nil then FRAMEFILE is actually an image data.
If DATA-P is non-nil then FRAME-IMG-TYPE specifies type of the image."
  (let* ((img-type (or frame-img-type (telega-image-supported-file-p framefile)))
         (size (telega-chars-xheight
                (if (consp telega-video-note-height)
                    (car telega-video-note-height)
                  telega-video-note-height)))
         (h size)
         (aw-chars (telega-chars-in-width size))
         (w (telega-chars-xwidth aw-chars))
         (xoff (/ (- w size) 2))
         (yoff (/ (- h size) 2))
         (svg (telega-svg-create w h))
         (clip (telega-svg-clip-path svg "clip"))
         (clip1 (telega-svg-clip-path svg "clip1"))
         (playing-p (numberp progress)))
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
      ;; Extract progress number in case progress is in the
      ;; (paused . progress) form
      (when (consp progress)
        (cl-assert (and (eq (car progress) 'paused)
                        (numberp (cdr progress))))
        (setq progress (cdr progress)))

      (let* ((angle-o (* 2 float-pi progress))
             (angle (+ (* 2 float-pi (- progress)) float-pi))
             (dx (+ (* (/ size 2) (sin angle)) (/ size 2)))
             (dy (+ (* (/ size 2) (cos angle)) (/ size 2))))
        ;; clip mask for the progress circle
        (let ((cp (format "M %d %d L %d %d L %d 0" (/ w 2) (/ h 2) (/ w 2) 0 w)))
          (when (> angle-o (/ float-pi 2))
            (setq cp (concat cp (format " L %d %d" w h))))
          (when (> angle-o float-pi)
            (setq cp (concat cp (format " L 0 %d" h))))
          (when (> angle-o (/ (* 3 float-pi) 2))
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

    ;; Draw play triangle
    (unless playing-p
      (let ((play-size (/ h 6)))
        (svg-polygon svg (list (cons (/ (- w play-size) 2)
                                     (/ (- h play-size) 2))
                               (cons (/ (- w play-size) 2)
                                     (/ (+ h play-size) 2))
                               (cons (/ (+ w play-size) 2)
                                     (/ h 2)))
                     :fill "red"
                     :opacity "0.5")))

    (telega-svg-image svg :scale 1.0
               :base-uri (if data-p "" framefile)
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
           (let* ((cheight (if (consp telega-video-note-height)
                               (car telega-video-note-height)
                             telega-video-note-height))
                  (x-size (telega-chars-xheight cheight)))
             (telega-media--progress-svg thumb-file x-size x-size cheight))))))


;; Recording notes
(defvar telega-vvnote--record-progress nil
  "Current record progress.
Set to nil, when ffplay exists.")

(defun telega-vvnote--record-callback (proc)
  "Progress callback for the video/voice note recording."
  (let ((proc-plist (process-plist proc)))
    (setq telega-vvnote--record-progress
          (when (process-live-p proc)
            (plist-get proc-plist :progress)))))

(defun telega-vvnote-voice--record ()
  "Record a voice note.
Return filename with recorded voice note."
  (let* ((telega-vvnote--record-progress 0)
         (note-file (telega-temp-name "voice-note" ".mp4"))
         (capture-proc (telega-ffplay-run-command
                        (telega-docker-exec-cmd
                         (concat "ffmpeg" " " telega-vvnote-voice-record-args
                                 " " note-file)
                         'try-host-cmd-first)
                        #'telega-vvnote--record-callback))
         (done-key
          (progn
            ;; Wait for ffmpeg process to start
            (accept-process-output capture-proc)

            ;; Capture voice note
            (with-telega-symbol-animate 'audio 0.1 audio-sym
              ;; Animation exit condition
              (or (not telega-vvnote--record-progress)
                  (not (process-live-p capture-proc)))

              (let (message-log-max)
                (message (concat "telega: "
                                 (propertize "●" 'face 'error)
                                 "VoiceNote " audio-sym
                                 " " (telega-duration-human-readable
                                      telega-vvnote--record-progress)
                                 " Press a key when done, C-g to cancel")))))))

    ;; Gracefully stop capturing and wait ffmpeg for exit
    (when (process-live-p capture-proc)
      (interrupt-process capture-proc)
      (while (process-live-p capture-proc)
        (accept-process-output capture-proc)))

    (unless (file-exists-p note-file)
      (error "telega: Can't capture voice note"))
    ;; C-g = 7
    (when (eq done-key 7)
      (delete-file note-file)
      (user-error "VoiceNote cancelled"))

    note-file))

(defvar telega-vvnote-video--preview nil
  "Plist holding preview for video note recording.")

(defun telega-vvnote-video--record-callback (_proc frame)
  "Callback when recording video note."
  ;; NOTE: fps = 30, hardcoded in `telega-vvnote-video-record-args'
  (let ((start (plist-get telega-vvnote-video--preview :start-point)))
    (with-current-buffer (marker-buffer start)
      (unless (= (point) start)
        (delete-region start (point)))

      (when frame
        (setq telega-vvnote--record-progress (/ (car frame) 30.0))

        ;; Copy first frame
        (when (= 1 (car frame))
          (let ((first-frame (telega-temp-name "video-note1" ".png")))
            (copy-file (cdr frame) first-frame)
            (plist-put telega-vvnote-video--preview :first-frame first-frame)))

        ;; Display frame
        (telega-ins--image
         (telega-vvnote-video--svg (cdr frame) nil nil 'png))))))

(defun telega-vvnote-video--record ()
  "Record a video note.
Return filename with recorded video note."
  ;; Check codecs availability
  (when (or (and (not (telega-ffplay-has-encoder-p "opus"))
                 (not (telega-ffplay-has-encoder-p "aac")))
            (and (not (telega-ffplay-has-encoder-p "hevc"))
                 (not (telega-ffplay-has-encoder-p "h264"))))
    (user-error "telega: sorry, your ffmpeg can't record video notes"))

  (setq telega-vvnote-video--preview
        (list :start-point (copy-marker (point))))
  (let* ((telega-vvnote--record-progress 0)
         (note-file (telega-temp-name "video-note" ".mp4"))
         (ffmpeg-args
          (concat telega-vvnote-video-record-args
                  " " note-file
                  " -f image2pipe"
                  " -vf hflip,scale=320:240,crop=240:240:40:0"
                  " -vcodec png -"))
         (capture-proc (telega-ffplay-to-png--internal ffmpeg-args
                         #'telega-vvnote-video--record-callback))
         (done-key
          (progn
            ;; Wait for ffmpeg process to start
            (accept-process-output capture-proc)

            (with-telega-symbol-animate 'video 0.25 video-sym
              ;; Animation exit condition
              (or (not telega-vvnote--record-progress)
                  (not (process-live-p capture-proc)))

              ;; Animation body
              (let (message-log-max)
                (message (concat "telega: "
                                 (propertize video-sym 'face 'error)
                                 "VideoNote "
                                 (format "%.1fs" telega-vvnote--record-progress)
                                 " Press a key when done, C-g to cancel")))))))

    ;; Gracefully stop capturing and wait ffmpeg for exit
    (when (process-live-p capture-proc)
      (internal-default-interrupt-process capture-proc)
      (while (process-live-p capture-proc)
        (accept-process-output capture-proc)))

    (unless (file-exists-p note-file)
      (error "telega: Can't capture video note"))
    ;; C-g = 7
    (when (eq done-key 7)
      (delete-file note-file)
      (user-error "VideoNote cancelled"))

    note-file))


;; Callbacks for ffplay controls, used by `telega-msg-button-map'
(defun telega-msg--vvnote-play-speed-toggle (msg)
  "Toggle playback speed for the media message.
Only two modes are available: normal speed and x2 speed."
  (interactive (list (telega-msg-for-interactive)))
  (when-let ((proc (plist-get msg :telega-ffplay-proc)))
    (if (equal telega-vvnote-play-speed 1)
        (setq telega-vvnote-play-speed 1.8)
      (setq telega-vvnote-play-speed 1))

    (telega-msg-redisplay msg)

    ;; Restart ffplay by reopenning message content
    (when (telega-ffplay-playing-p proc)
      (telega-ffplay-pause proc nil 'ignore-callback)
      (telega-msg-open-content msg))))

(defun telega-msg--vvnote-stop (msg)
  "Stop playing media message."
  (interactive (list (telega-msg-for-interactive)))
  (when-let ((proc (plist-get msg :telega-ffplay-proc)))
    (telega-ffplay-stop proc)

    ;; Force stop for possibly paused process
    (when (telega-ffplay-paused-p proc)
      (plist-put msg :telega-ffplay-frame nil)
      (plist-put msg :telega-ffplay-proc nil)
      (telega-msg-redisplay msg))))

(defun telega-msg--vvnote-rewind (msg step)
  "Rewind current ffplay position by STEP."
  (let ((proc (plist-get msg :telega-ffplay-proc)))
    (when (telega-ffplay-playing-p proc)
      (telega-ffplay-pause
       proc (+ step (or (telega-ffplay-progress proc) 0)) 'ignore-callback)
      (telega-msg-open-content msg))))

(defun telega-msg--vvnote-rewind-10-forward (msg)
  "Rewind 10 seconds forward."
  (interactive (list (telega-msg-for-interactive)))
  (telega-msg--vvnote-rewind msg 10))

(defun telega-msg--vvnote-rewind-10-backward (msg)
  "Rewind 10 seconds backward."
  (interactive (list (telega-msg-for-interactive)))
  (telega-msg--vvnote-rewind msg -10))

(defun telega-msg--vvnote-rewind-part (msg)
  "Rewind to the N's 10 part of the message duration.
I.e. if you press 7, then you will jump to 70% of the message
duration."
  (interactive (list (telega-msg-for-interactive)))

  (when-let ((proc (plist-get msg :telega-ffplay-proc)))
    (let* ((content (plist-get msg :content))
           (duration (cl-case (telega--tl-type content)
                       (messageVoiceNote
                        (telega--tl-get content :voice_note :duration))
                       (messageVideoNote
                        (telega--tl-get content :video_note :duration))
                       (messageAudio
                        (telega--tl-get content :audio :duration))
                       (t
                        ;; Possibly a element embedded into webpage
                        (let ((web-page (plist-get content :web_page)))
                          (plist-get (or (plist-get web-page :audio)
                                         (plist-get web-page :video_note)
                                         (plist-get web-page :voice_note))
                                     :duration)))))
           (n-part (string-to-number (this-command-keys))))
      (cl-assert (< 0 n-part 10))
      (when (and (telega-ffplay-playing-p proc) duration)
        (telega-ffplay-pause
         proc (floor (* (/ n-part 10.0) duration)) 'ignore-callback)
        (telega-msg-open-content msg)))))

(provide 'telega-vvnote)

;;; telega-vvnote.el ends here

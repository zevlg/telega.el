;;; telega-util.el --- Utility functions for telega  -*- lexical-binding:t -*-

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sat Apr 21 03:56:02 2018
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

;; Utility functions to be used by telega

;;; Code:

(require 'ewoc)
(require 'cl-lib)
(require 'puny)                         ; `puny-decode-domain'
(require 'files)                        ; `locate-file'
(require 'rx)                           ; `rx'
(require 'svg)
(require 'color)                        ; `color-XXX'
(require 'dired-aux)                    ; `dired-dwim-target-directory'
(require 'ansi-color)                   ; `ansi-color-apply'
(require 'url-util)                     ; `url-unhex-string'
(require 'org)                          ; `org-read-date', `org-do-emphasis-faces'
(require 'org-element)                  ; for `org-do-emphasis-faces'
(require 'rainbow-identifiers)

(require 'telega-core)
(require 'telega-customize)
(require 'telega-media)

(declare-function telega-root--buffer "telega-root")
(declare-function telega-chat--type "telega-chat" (chat))
(declare-function telega-chat-title "telega-chat")
(declare-function telega-chatbuf--name "telega-chat" (chat))
(declare-function telega-describe-chat "telega-chat" (chat))
(declare-function telega-folder-names "telega-folders")
(declare-function telega-browse-url "telega-webpage" (url &optional in-web-browser))

(defun telega-file-exists-p (filename)
  "Return non-nil if FILENAME exists.
Unlike `file-exists-p' this return nil for empty string FILENAME.
Also return `nil' if FILENAME is `nil'."
  (and filename
       (not (string-empty-p filename))
       (file-exists-p filename)))

(defun telega-plist-del (plist prop)
  "From PLIST remove property PROP."
  ;; NOTE: `cl--plist-remove' has been removed in Emacs master
  ;; See https://t.me/emacs_telega/27687
  ;; Code taken from `org-plist-delete'
  (let (p)
    (while plist
      (if (not (eq prop (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun telega-plist-map (func plist)
  "Map FUNCTION on PLIST and return resulting list.
FUNCTION must accept two arguments: KEY and VALUE."
  (let (result)
    (telega--tl-dolist ((prop-name value) plist)
      (setq result (cons (funcall func prop-name value) result)))
    (nreverse result)))

(defun telega-face-height (face)
  "Return float version of FACE height."
  (let ((height (face-attribute face :height)))
    (if (floatp height)
        height
      (/ (float height) (face-attribute 'default :height)))))

(defun telega-short-filename (filename)
  "Shortens FILENAME by removing `telega-directory' prefix."
  (if (and telega-use-short-filenames
           (string-prefix-p (concat telega-directory "/") filename))
      (substring filename (1+ (length telega-directory)))
    (abbreviate-file-name filename)))

(defun telega-x-frame ()
  "Return window system frame, if any.
Selected frame and frame displaying root buffer are examined first."
  (or (when-let ((root-frame
                  (window-frame
                   (get-buffer-window (or telega--current-buffer
                                          (telega-root--buffer))))))
        (when (display-graphic-p root-frame)
          root-frame))
      (cl-find-if #'display-graphic-p (frame-list))))

(defun telega-focus-state (&optional frame)
  "Return non-nil if FRAME has focus.
Can be used as value for `telega-online-status-function'."
  (if (fboundp 'frame-focus-state)
      (funcall 'frame-focus-state frame)
    ;; NOTE: For tty frame always return non-nil
    ;; see https://t.me/emacs_telega/7419
    (or (not (display-graphic-p frame))
        (frame-parameter frame 'x-has-focus))))

(defun telega-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is some telega buffer.
If BUFFER is ommited, current buffer is used.
Could be used as value for `telega-online-status-function'."
  (with-current-buffer (or buffer (current-buffer))
    (when (or (derived-mode-p 'telega-root-mode)
              (derived-mode-p 'telega-chat-mode)
              (string-prefix-p "*Telega" (buffer-name))
              (string-prefix-p "*Telegram" (buffer-name)))
      t)))

(defun telega-chars-xwidth (n &optional face)
  "Return pixel width for N characters.
If FACE is not specified, then `default' is used."
  ;; NOTE: Same (* n (window-font-width (get-buffer-window nil (telega-x-frame))))
  ;; but without tweaking on window configuration, which breaks inserters
  (* n (if-let ((tframe (telega-x-frame)))
           (with-current-buffer (or telega--current-buffer (current-buffer))
             (let* ((info (font-info
                           (face-font (or face 'default) tframe) tframe))
                    (width (aref info 11)))
               (if (> width 0)
                   width
                 (aref info 10))))
         (frame-char-width))))

(defun telega-chars-xheight (n &optional face)
  "Return pixel height for N characters.
If FACE is not specified, then `default' is used."
  (* n (if-let ((tframe (telega-x-frame)))
           (with-current-buffer (or telega--current-buffer (current-buffer))
             (aref (font-info (face-font (or face 'default) tframe) tframe) 3))
         (frame-char-height))))

(defun telega-chars-in-height (pixels)
  "Return how many lines needed to cover PIXELS height."
  (ceiling (/ pixels (float (telega-chars-xheight 1)))))

(defun telega-chars-in-width (pixels)
  "Return how many characters needed to cover PIXELS width."
  (ceiling (/ pixels (float (telega-chars-xwidth 1)))))

(defun telega-strip-newlines (string)
  "Strip STRING newlines from end and beginning."
  (replace-regexp-in-string
   (rx (or (: string-start (* (any ?\r ?\n)))
           (: (* (any ?\r ?\n)) string-end)))
   ""
   (or string "")))

(defun telega-window-current-column (&optional window)
  "Return current column in the window."
  (telega-chars-in-width
   (car (window-text-pixel-size window (line-beginning-position) (point)))))

(defun telega-window-string-width (str)
  "Return correct width in chars.
This function is very slow comparing to `string-width', however
returns precise value."
  (with-temp-buffer
    (telega-ins str)
    (save-window-excursion
      (set-window-dedicated-p nil nil)
      (set-window-buffer nil (current-buffer))
      (telega-window-current-column))))

(defun telega-current-column ()
  "Same as `current-column', but take into account width of the characters."
  (string-width (buffer-substring (line-beginning-position) (point))))

(defun telega-canonicalize-number (value from-value)
  "Canonicalize number VALUE.
VALUE can be a float, in this case take this fraction from FROM-VALUE.
Otherwise use VALUE as is.
MIN-VALUE specifies minimal value after canonicalization.
Used to calculate canonical values for with of some buttons such as
`telega-filter-button-width' and `telega-chat-button-width'."
  (let* ((min-value (when (listp value) (nth 1 value)))
         (max-value (when (listp value) (nth 2 value)))
         (value (if (listp value) (nth 0 value) value))
         (canon-value (if (floatp value)
                          (progn
                            (cl-assert (< 0 value 1))
                            (round (* value from-value)))
                        value))
         (ret-min-value (if min-value
                            (max min-value canon-value)
                          canon-value)))
    (if max-value
        (min max-value ret-min-value)
      ret-min-value)))

(defun telega-temp-name (prefix &optional ext)
  "Generate unique temporary file name with PREFIX and extension EXT.
Specify EXT with leading `.'."
  (concat (expand-file-name (make-temp-name prefix) telega-temp-dir) ext))

(defun telega-svg-raw-node (svg node-name node-attrs &rest node-childs)
  "Add string as is to the SVG."
  (svg--def svg (apply 'dom-node node-name node-attrs node-childs)))

(defun telega-svg-clip-path (svg id)
  (let ((cp (dom-node 'clipPath `((id . ,id)))))
    (svg--def svg cp)
    cp))

(defun telega-svg-image-mask (svg mask-id image img-type datap &rest args)
  "Create an svg mask using embedded IMAGE."
  (let ((mask (dom-node 'mask `((id . ,mask-id)
                                ,@(svg--arguments svg args)))))
    (svg--def svg mask)
    (apply #'telega-svg-embed mask image img-type datap args)
    mask))

(defun telega-svg-path (svg d &rest args)
  (svg--append svg (dom-node 'path
                             `((d . ,d)
                               ,@(svg--arguments svg args)))))

(defun telega-svg-embed (svg image img-type datap &rest args)
  "Embed IMAGE possible using `svg-embed-base-uri-image'.
IMAGE could be a list of two elements \\(RELATIVE-FNAME BASE-DIR\\),
so new Emacs `svg-embed-base-uri-image' functionality could be used."
  (if (and telega-use-svg-base-uri
           (not datap)
           ;; NOTE: embedding using `:base-uri' does not work under Windows
           ;; see https://github.com/zevlg/telega.el/issues/367
           (not (eq (framep-on-display (telega-x-frame)) 'w32))
           (listp image))
      (apply #'svg-embed-base-uri-image svg (car image) args)
    (apply #'svg-embed svg (if (listp image)
                               (apply #'expand-file-name image)
                             image)
           img-type datap args)))

(defun telega-svg-gradient (svg id type stops)
  "Same as `svg-gradient' but supports \"stop-opacity\" property."
  (svg--def
   svg
   (apply
    'dom-node
    (if (eq type 'linear)
        'linearGradient
      'radialGradient)
    `((id . ,id)
      (x1 . 0)
      (x2 . 0)
      (y1 . 0)
      (y2 . 1))
    (mapcar
     (lambda (stop)
       (dom-node 'stop `((offset . ,(format "%s%%" (car stop)))
                         (stop-color . ,(cadr stop))
                         (stop-opacity . ,(plist-get (cddr stop) :opacity)))))
     stops))))

(defun telega-svg-progress (svg progress)
  "Insert progress circle into SVG."
  (let* ((w (alist-get 'width (cadr svg)))
         (h (alist-get 'height (cadr svg)))
         ;; progress clipping mask
         (angle-o (+ float-pi (* 2 float-pi (- 1.0 progress))))
         (clip-dx (* (/ w 2) (1+ (sin angle-o))))
         (clip-dy (* (/ h 2) (1+ (cos angle-o))))
         (pclip (telega-svg-clip-path svg "pclip")))
    ;; clip mask for the progress circle
    (let ((cp (format "M %d %d L %d %d L %d 0" (/ w 2) (/ h 2) (/ w 2) 0 0)))
      (when (< progress 0.75)
        (setq cp (concat cp (format " L 0 %d" h))))
      (when (< progress 0.5)
        (setq cp (concat cp (format " L %d %d" w h))))
      (when (< progress 0.25)
        (setq cp (concat cp (format " L %d 0" w))))
      (setq cp (concat cp (format " L %d %d" clip-dx clip-dy)))
      (setq cp (concat cp " Z"))
      (telega-svg-path pclip cp))
    ;; progress circle
    (svg-circle svg (/ w 2) (/ h 2) (/ h 2)
                :fill-color (or (face-foreground 'shadow) "gray50")
                :fill-opacity "0.25"
                :clip-path "url(#pclip)")
    svg))

(defun telega-svg-squircle (svg x y width height &rest args)
  "In SVG at X and Y positioon draw squircle of WIDTHxHEIGHT size.
X and Y denotes left up corner."
  ;; Values are taken from
  ;; https://upload.wikimedia.org/wikipedia/commons/5/58/Squircle2.svg
  (let* ((w-factor (/ width 608.0))
         (h-factor (/ height 608.0))
         (squircle-cubic-beziers
          '(((126.2 . 288) (196.3563 . 288) (242.1782 . 242.1782))
            ((288 . 196.3563) (288 . 126.2) (288 . 0))
            ((288 . -126.2) (288 . -196.3563) (242.1782 . -242.1782))
            ((196.3563 . -288) (126.2 . -288) (0 . -288))
            ((-126.2 . -288) (-196.3563 . -288) (-242.1782 . -242.1782))
            ((-288 . -196.3563) (-288 . -126.2) (-288 . 0))
            ((-288 . 126.2) (-288 . 196.3563) (-242.1782 . 242.1782))
            ((-196.3563 . 288) (-126.2 . 288) (0 . 288))))
         (cmd-start (format "M%f,%f\n"
                            (+ x (* 304 w-factor))
                            (+ y (* (+ 288 304) h-factor))))
         (cmd-cb
          (mapconcat
           (lambda (cubic-bezier)
             (concat "C"
                     (mapconcat
                      (lambda (p)
                        (format "%f,%f"
                                (+ x (* (+ 304 (car p)) w-factor))
                                (+ y (* (+ 304 (cdr p)) h-factor))))
                      cubic-bezier ",")))
           squircle-cubic-beziers "\n")))
    (apply #'telega-svg-path svg (concat cmd-start cmd-cb "Z") args)))

(defun telega-svg-apply-outline (svg outline ratio &optional args)
  (apply #'telega-svg-path svg
         (mapconcat (lambda (op)
                      (concat (symbol-name (car op))
                              (mapconcat
                               (lambda (pnt)
                                 (concat (number-to-string (* ratio (car pnt)))
                                         " "
                                         (number-to-string
                                          (* ratio (cadr pnt)))))
                               (cdr op) " ")))
                    outline "\n")
         args))

(defun telega-svg-telega-logo (svg width &rest args)
  "Draw telega triangle of WIDTH."
  (declare (indent 2))
  (let ((ratio (/ width 32.0))
        (outline '((M (0 10.1891))
                   (l (7.9819 5.5418))
                   (c (0.8853 -0.322) (1.8202 -0.6638) (2.599 -0.9418)
                      (1.9609 -0.7)   (7.0539 -3.4182) (7.0539 -3.4182)
                      (-2.5145 2.2595) (-4.6401 4.5613) (-6.55 6.8691))
                   (L (17.5694 27))
                   (c (0.2653 -0.9309) (0.5279 -1.8618) (0.9135 -2.9018))
                   (C (20.4518 18.4196) (32 0) (32 0)
                      (24.4744 2.555) (10.7087 7.5896) (7.8333 8.5782)
                      (5.5816 9.3523) (2.1946 10.5884) (0 10.1892))
                   (z))))
    (telega-svg-apply-outline svg outline ratio args)))

(defun telega-svg-round-square (svg x y width height radius &rest args)
  "In SVG at X and Y positioon draw square with round corners.
RADIUS denotes radius for round corners.
X and Y denotes left up corner."
  ;; NOTE: Draw 8-corners polygon, and then circles in all four
  ;; corners
  (let ((poly-points (list (cons (+ x radius) y)
                           (cons (- (+ x width) radius) y)
                           (cons (+ x width) (+ y radius))
                           (cons (+ x width) (- (+ y height) radius))
                           (cons (- (+ x width) radius) (+ y height))
                           (cons (+ x radius) (+ y height))
                           (cons x (- (+  y height) radius))
                           (cons x radius))))
    (apply #'svg-polygon svg poly-points args)
    ;; Four round courners
    (apply #'svg-circle svg (+ x radius) (+ y radius) radius args)
    (apply #'svg-circle svg (- (+ x width) radius) (+ y radius) radius args)
    (apply #'svg-circle svg (- (+ x width) radius) (- (+ height y) radius) radius args)
    (apply #'svg-circle svg (+ x radius) (- (+ height y) radius) radius args)
    ))

(defun telega-svg-premium-logo (svg width &rest args)
  "Draw Telegram Premium logo."
  (declare (indent 2))
  (let ((ratio (/ width 16.0))
        (outline '((M (7.9673 1.7397))
                   (c (-0.8215 0.005) (-1.3307 3.6214) (-1.9924 4.1084)
                      (-0.6618 0.487) (-4.266 -0.102) (-4.5151 0.681)
                      (-0.1812 0.5691) (2.5226 2.3763) (2.5226 2.3763))
                   (s (5.0903 -1.0188) (5.181 -0.8892))
                   (c (0.1049 0.15) (-1.9601 1.3199) (-4.5458 2.74)
                      (-0.3426 1.263) (-1.1017 3.0947) (-0.6124 3.4457)
                      (0.6676 0.4789) (3.2046 -2.1477) (4.0261 -2.1524)
                      (0.8216 -0.005) (3.3918 2.5891) (4.0535 2.1022)
                      (0.6618 -0.487) (-1.0488 -3.714) (-0.7997 -4.4969)
                      (0.2492 -0.7829) (3.5078 -2.4236) (3.2492 -3.2035)
                      (-0.2586 -0.7798) (-3.852 -0.1518) (-4.5196 -0.6306)
                      (-0.6676 -0.4789) (-1.2258 -4.0856) (-2.0474 -4.081))
                   (z))))
    (telega-svg-apply-outline svg outline ratio args)))

(defun telega-svg-create (width height &rest args)
  "Create SVG image using `svg-create'.
Addresses some issues telega got with pure `svg-create' usage."
  ;; See https://t.me/emacs_telega/13764
  ;; Also see https://t.me/emacs_telega/20435
  (cl-assert (and (integerp width) (integerp height)))
  (apply #'svg-create width height
         :xmlns:xlink "http://www.w3.org/1999/xlink" args))

(defun telega-svg-image (svg &rest props)
  "Return an image object from SVG.
PROPS is passed on to `create-image' as its PROPS list."
  ;; NOTE: work around problem displaying unicode characters in some
  ;; librsvg versions (in my case 2.40.13).  Encoded (in &#xxxx format)
  ;; text is only displayed correctly if <xml ..?> node is specified
  (let ((svg-data (with-temp-buffer
                    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
                    (svg-print svg)
                    (buffer-string))))
    ;; NOTE: Do not check `svg' availability, so it will work when
    ;; `rsvg' is not compliled in and telega images disabled See
    ;; https://github.com/zevlg/telega.el/issues/219
    (nconc (list 'image :type 'svg :data svg-data)
           (unless (plist-member props :scale)
             (list :scale
                   (image-compute-scaling-factor image-scaling-factor)))
           props)))

(defun telega-svg-fit-into (width height fit-width fit-height)
  "Fit rectangle of WIDTHxHEIGHT size into FIT-WIDTHxFIT-HEIGHT rect.
Do touch outsize scaling.
Return resulting x,y,width,height."
  (let* ((w-ratio (/ (float fit-width) width))
         (h-ratio (/ (float fit-height) height))
         (fit-horiz-p (> (/ (float width) height)
                         (/ (float fit-width) fit-height)))
         (ret-height (if fit-horiz-p
                         fit-height
                       (round (* height w-ratio))))
         (ret-width (if (not fit-horiz-p)
                        fit-width
                      (round (* width h-ratio)))))
    (list (- (/ (- ret-width fit-width) 2))
          (- (/ (- ret-height fit-height) 2))
          ret-width ret-height)))

(defun telega-poll-create-svg (cwidth percents &optional face)
  "Create SVG for use in poll options inserter."
  (cl-assert (<= percents 100))
  (let* ((ndashes (ceiling (* cwidth (/ percents 100.0))))
         (telega-text (propertize
                       (if (> ndashes 0) (make-string ndashes ?\-) "·")
                       'face (or face 'bold)))
         (xheight (telega-chars-xheight 1))
         (xwidth (telega-chars-xwidth (string-width telega-text)))
         (stroke-xwidth (/ xheight 10))
         (dashes-xwidth (* (- (telega-chars-xwidth cwidth) (* 2 stroke-xwidth))
                           (/ percents 100.0)))
         (svg (telega-svg-create xwidth xheight)))
    (svg-line svg stroke-xwidth (/ xheight 2)
              (+ stroke-xwidth dashes-xwidth) (/ xheight 2)
              :stroke-color telega-poll-result-color
              :stroke-width stroke-xwidth
              :stroke-linecap "round")
    (telega-svg-image svg :scale 1
                      :width xwidth :height xheight
                      :mask 'heuristic
                      :ascent 'center
                      :telega-text telega-text)))

(defun telega-self-destruct-create-svg (minithumb &optional emoji-symbol)
  "Create svg image for the self destructing image with minithumbnail MINITHUMB.
EMOJI-SYMBOL is the emoji symbol to be used. (Default is `telega-symbol-flames')"
  (let* ((xh (* (ceiling (/ (nth 3 telega-photo-size-limits) 1.5))
                (telega-chars-xheight 1)))
         (xw xh)
         (svg (telega-svg-create xw xh)))

    (when minithumb
      (svg-embed svg (base64-decode-string (plist-get minithumb :data))
                 "image/jpeg" t
                 :x 0 :y 0 :width xw :height xh))

    (svg-circle svg (/ xw 2) (/ xh 2) (/ xh 4)
                :fill-color "white"
                :fill-opacity "0.75")

    (when telega-emoji-font-family
      (let ((font-size (/ xw 4)))
        (svg-text svg (or emoji-symbol telega-symbol-flames)
                  :font-family telega-emoji-font-family
                  :font-size font-size
                  :x (- (/ xw 2) (/ font-size 1.75))
                  :y (+ (/ font-size 3) (/ xh 2)))))

    (telega-svg-image svg :scale 1.0
                      :width xw :height xh
                      :ascent 'center)))

(defun telega-preview-one-line-create-svg (filename data-p width height
                                                    &optional video-p)
  "Create preview svg for FILENAME.
DATA-P is non-nil if FILENAME is actually an image data instead
WIDTH and HEIGHT is an image size.
Specify non-nil VIDEO-P if generating preview for video."
  (let* ((base-dir (if data-p
                       (telega-directory-base-uri telega-temp-dir)
                     (file-name-directory filename)))
         (svg-w (telega-chars-xwidth 2))
         (svg-h (min svg-w (telega-chars-xheight 1)))
         (margin 1)                     ; margin for the mask in pixels
         (svg (telega-svg-create svg-w svg-h))
         (pclip (telega-svg-clip-path svg "pclip")))
    (telega-svg-round-square pclip margin margin
                             (- svg-w (* 2 margin)) (- svg-h (* 2 margin))
                             (/ svg-w 6))
    (cl-destructuring-bind (x-fit y-fit w-fit h-fit)
        (telega-svg-fit-into width height svg-w svg-h)
      (telega-svg-embed svg (if data-p
                                filename
                              (list (file-relative-name filename base-dir)
                                    base-dir))
                        (format "image/%s"
                                (if data-p
                                    "jpeg"
                                  (telega-image-supported-file-p filename t)))
                        data-p :x x-fit :y y-fit :width w-fit :height h-fit
                        :clip-path "url(#pclip)"))

    ;; Draw play triangle
    (when video-p
      (let ((play-size (/ svg-w 3)))
        (svg-polygon svg (list (cons (/ (- svg-w play-size) 2)
                                     (/ (- svg-h play-size) 2))
                               (cons (/ (- svg-w play-size) 2)
                                     (/ (+ svg-h play-size) 2))
                               (cons (/ (+ svg-w play-size) 2)
                                     (/ svg-h 2)))
                     :fill "red"
                     :opacity "0.75")))

    (telega-svg-image svg :scale 1.0 :width svg-w :height svg-h
                      :ascent 'center
                      :mask 'heuristic
                      :base-uri (expand-file-name "dummy" base-dir))
    ))

(defun telega-video--create-svg (filename width height &optional data-p img-type)
  "Create image for the VIDEO.
With circle and triangle in the center."
  (let* ((img-type (or img-type (telega-image-supported-file-p filename)))
         (svg (telega-svg-create width height))
         (play-size (/ height 8.0))
         (xoff (/ play-size 8.0)))
    (telega-svg-embed svg (if data-p
                              filename
                            (list (file-name-nondirectory filename)
                                  (file-name-directory filename)))
                      (format "image/%S" img-type) data-p
                      :x 0 :y 0
                      :width width :height height)
    (svg-circle svg (/ width 2) (/ height 2) play-size
                :fill "black"
                :opacity "0.85")
    (svg-polygon svg (list (cons (+ xoff (/ (- width play-size) 2))
                                 (/ (- height play-size) 2))
                           (cons (+ xoff (/ (- width play-size) 2))
                                 (/ (+ height play-size) 2))
                           (cons (+ xoff (/ (+ width play-size) 2))
                                 (/ height 2)))
                 :fill "white"
                 :opacity "0.85")
    (telega-svg-image svg :scale 1.0
               :base-uri (if data-p "" filename)
               :width width :height height
               :ascent 'center)
    ))

(defun telega-time-seconds (&optional as-is)
  "Return current time as unix timestamp.
If AS-IS is non-nil, then do not apply time adjustment using
`telega-tdlib--unix-time'."
  (let ((ctime (floor (time-to-seconds))))
    (if as-is
        ctime
      (+ ctime (- (or (plist-get telega-tdlib--unix-time :remote) 0)
                  (or (plist-get telega-tdlib--unix-time :local) 0))))))

(defun telega-distance-human-readable (meters)
  "Convert METERS to human readable string."
  (cond ((not (integerp meters)) "unknown")
        ((> meters 10000) (format "%d km" (/ meters 1000)))
        ((>= meters 1000) (format "%.1f km" (/ meters 1000.0)))
        (t (format "%d meters" meters))))

(defun telega-number-human-readable (num)
  "Convert METERS to human readable string."
  (if (and telega-use-short-numbers (>= num 1000))
      (format "%.1fk" (/ num 1000.0))
    (number-to-string num)))

(defun telega-duration-human-readable (seconds &optional n
                                               day-label hour-label min-label)
  "Convert SECONDS to human readable string.
If N is given, then use only N significant components.
For example if duration is 4h:20m:3s then with N=2 4H:20m will be returned.
By default N=3 (all components).
N can't be 0."
  (cl-assert (or (null n) (> n 0)))
  ;; NOTE: force seconds to be a number, see
  ;; https://t.me/emacs_ru/283567?single
  (setq seconds (round seconds))
  (let ((ncomponents (or n 3))
        (intervals `((86400 . ,(or day-label "d"))
                     (3600 . ,(or hour-label "h"))
                     (60 . ,(or min-label "m"))))
        comps)
    ;; days, hours, minutes
    (while (and (> ncomponents 0) intervals)
      (let* ((ival (car intervals))
             (ival-seconds (car ival)))
        (when (>= seconds ival-seconds)
          (setq comps (nconc comps (list (concat (int-to-string
                                                  (/ seconds ival-seconds))
                                                 (cdr ival))))
                seconds (% seconds ival-seconds)
                ncomponents (1- ncomponents)))
        (setq intervals (cdr intervals))))

    ;; seconds
    (when (and (> ncomponents 0) (or (null comps) (> seconds 0)))
      (setq comps (nconc comps (list (format "%ds" seconds)))))
    (mapconcat #'identity comps ":")))

(defun telega-link-props (link-type link-to &optional face)
  "Generate props for link button openable with `telega-link--button-action'."
  (cl-assert (memq link-type '(url file username user sender hashtag)))

  (nconc (list 'action 'telega-link--button-action
               :telega-link (cons link-type link-to))
         (when face
           (list 'face face))))

(defun telega-link--button-action (button)
  "Browse url at point."
  (let ((link (button-get button :telega-link)))
    (telega-debug "Action on link: %S" link)
    (cl-ecase (car link)
      (sender
       (telega-describe-msg-sender (cdr link)))
      (user
       (telega-describe-user (telega-user-get (cdr link))))
      (username
       (let ((user (telega-user--by-username (cdr link))))
         (if user
             (telega-describe-user user)
           (message (format "Fetching info about %s" (cdr link)))
           (telega--searchPublicChat (cdr link)
             (lambda (chat)
               (if-let ((user (telega-chat-user chat)))
                   (telega-describe-user user)
                 (telega-describe-chat chat)))))))
      (hashtag
       (when telega-chatbuf--chat
         (telega-chatbuf-filter-hashtag (cdr link))))
      (url
       (telega-browse-url (cdr link) current-prefix-arg))
      (file
       (telega-open-file (cdr link) (telega-msg-at button)))
      )))

(defun telega-escape-underscores (text)
  "Replace underscores in TEXT's urls and mentions."
  ;; NOTE: replace _ also in mentions
  ;; see https://github.com/zevlg/telega.el/issues/143
  (let ((rep-rx (rx (and (or (and word-start
                                  (or (and "http" (? "s") "://") "www\."))
                             (and "@" word-start))
                         (1+ (not (in " \t\n")))
                         word-end))))
    (replace-regexp-in-string
     rep-rx
     (lambda (word-text)
       (replace-regexp-in-string "_" "\\_" word-text nil t))
     text nil t)))

(defun telega-puny-decode-url (url)
  "Decode ULR according to the IDNA/punycode algorithm.
See `puny-decode-domain' for details."
  (replace-regexp-in-string
   (eval-when-compile (rx string-start
                          (? "http" (? "s") "://")
                          (group (1+ (regexp "[^/]")))))
   (lambda (_)
     (save-match-data
       (puny-decode-domain (match-string 1 url))))
   url nil 'literal 1))

(defun telega--spoiler-sensor-func (_window oldpos dir)
  "Sensor function to show/hide spoilers."
  (when-let* ((pos (if (eq dir 'entered) (point) oldpos))
              (msg (telega-msg-at pos))
              (ent-type (get-text-property pos :tl-entity-type)))
    (when (eq 'textEntityTypeSpoiler (telega--tl-type ent-type))
      (plist-put ent-type :telega-show-spoiler (eq dir 'entered))
      (let ((cursor-sensor-inhibit t))
        (telega-msg-redisplay msg)))))

(defun telega--entity-to-properties (entity text)
  "Convert telegram ENTITY to emacs text properties to apply to TEXT."
  (let ((ent-type (plist-get entity :type)))
    (nconc
     (list :tl-entity-type ent-type)
     (cl-case (telega--tl-type ent-type)
       (textEntityTypeMention
        (telega-link-props 'username text
                           (if (and telega-msg-contains-unread-mention
                                    (telega-user-match-p (telega-user-me)
                                      (list 'username
                                            (concat "^"
                                                    (substring text 1) ;strip @
                                                    "$"))))
                               '(telega-entity-type-mention bold)
                             'telega-entity-type-mention)))
       (textEntityTypeMentionName
        (telega-link-props 'user (plist-get ent-type :user_id)
                           (if (and telega-msg-contains-unread-mention
                                    (eq (plist-get ent-type :user_id)
                                        telega--me-id))
                               '(telega-entity-type-mention bold)
                             'telega-entity-type-mention)))
       (textEntityTypeHashtag
        (telega-link-props 'hashtag text 'telega-link))
       (textEntityTypeBold
        '(face telega-entity-type-bold))
       (textEntityTypeItalic
        '(face telega-entity-type-italic))
       (textEntityTypeUnderline
        '(face telega-entity-type-underline))
       (textEntityTypeStrikethrough
        '(face telega-entity-type-strikethrough))
       (textEntityTypeCode
        '(face telega-entity-type-code))
       (textEntityTypePre
        '(face telega-entity-type-pre))
       (textEntityTypePreCode
        '(face telega-entity-type-pre))
       (textEntityTypeUrl
        ;; - Unhexify url, using `telega-display' property to be
        ;; substituted at `telega--desurrogate-apply' time
        ;; - Convert "xn--" domains to non-ascii version
        (nconc (list 'telega-display
                     (telega-puny-decode-url
                      (decode-coding-string
                       (url-unhex-string text) 'utf-8)))
               (telega-link-props 'url text 'telega-entity-type-texturl)))
       (textEntityTypeTextUrl
        (telega-link-props 'url (plist-get ent-type :url)
                           'telega-entity-type-texturl))
       (textEntityTypeBotCommand
        '(face telega-entity-type-botcommand))
       (textEntityTypeMediaTimestamp
        (list 'action (lambda (button)
                        (telega-msg-open-media-timestamp
                         (telega-msg-at button)
                         (plist-get ent-type :media_timestamp)))
              'face 'telega-link))
       (textEntityTypeSpoiler
        (nconc (list 'cursor-sensor-functions '(telega--spoiler-sensor-func))
               (unless (plist-get ent-type :telega-show-spoiler)
                 (list 'telega-display-by 'spoiler
                       'telega-display
                       (with-temp-buffer
                         (insert text)
                         (translate-region (point-min) (point-max)
                                           telega-spoiler-translation-table)
                         (propertize (buffer-string)
                                     'face 'telega-entity-type-spoiler))))
               ;; To make `telega-msg-copy-text' keep spoilers being
               ;; not quite visible
               (when telega-inhibit-telega-display-by
                 '(face telega-entity-type-spoiler))
               ))
       (textEntityTypeCustomEmoji
        (when telega-use-images
          (when-let ((sticker (gethash (plist-get ent-type :custom_emoji_id)
                                       telega--custom-emoji-stickers)))
            (list 'display (telega-sticker--image sticker)))))
       ))))

;; https://core.telegram.org/bots/api#markdown-style
(defsubst telega--entity-to-markdown (entity-text)
  "Convert ENTITY back to markdown syntax applied to TEXT.
ENTITY-TEXT is cons cell where car is the ENTITY and cdr is the TEXT.
Return now text with markdown syntax."
  ;; NOTE: text might have surrogated pairs, for example when editing
  ;; message with emojis
  (let ((ent-type (plist-get (car entity-text) :type))
        (text (cdr entity-text)))
    (cl-case (and ent-type (telega--tl-type ent-type))
      (textEntityTypeBold (concat "*" text "*"))
      (textEntityTypeItalic (concat "_" text "_"))
      (textEntityTypeUnderline (concat "__" text "__"))
      (textEntityTypeStrikethrough (concat "~" text "~"))
      (textEntityTypeCode (concat "`" text "`"))
      ((textEntityTypePreCode textEntityTypePre)
       (concat "```" (plist-get ent-type :language) "\n" text "```"))
      (textEntityTypeMentionName
       (format "[%s](tg://user?id=%d)"
               text (plist-get ent-type :user_id)))
      (textEntityTypeUrl
       ;; Hexify only spaces and tabs, removing `telega-display'
       ;; property, which is used in `telega--desurrogate-apply'
       (replace-regexp-in-string
        (regexp-quote "\t") "%09"
        (replace-regexp-in-string (regexp-quote " ") "%20"
                                  (substring-no-properties text))))
      (textEntityTypeTextUrl
       (format "[%s](%s)" text (plist-get ent-type :url)))
      (textEntityTypeCustomEmoji
       (apply #'propertize text
              (telega--entity-to-properties (car entity-text) text)))
      (t text))))

(defsubst telega--entity-to-org (entity-text)
  "Convert ENTITY back to markdown syntax applied to TEXT.
ENTITY-TEXT is cons cell where car is the ENTITY and cdr is the TEXT.
Return string with org mode syntax."
  ;; NOTE: text might have surrogated pairs, for example when editing
  ;; message with emojis
  (let ((ent-type (plist-get (car entity-text) :type))
        (text (cdr entity-text)))
    (cl-case (and ent-type (telega--tl-type ent-type))
      (textEntityTypeBold (concat "*" text "*"))
      (textEntityTypeItalic (concat "/" text "/"))
      (textEntityTypeUnderline (concat "_" text "_"))
      (textEntityTypeStrikethrough (concat "+" text "+"))
      (textEntityTypeCode (concat "~" text "~"))
      (textEntityTypePre (concat "=" text "="))
      (textEntityTypePreCode
       (concat "\n" "#+begin_src " (plist-get ent-type :language) "\n"
               text "\n"
               "#+end_src\n"))
      (textEntityTypeMentionName
       (format "[[tg://user?id=%d][%s]]" (plist-get ent-type :user_id) text))
      (textEntityTypeUrl
       ;; Hexify only spaces and tabs, removing `telega-display'
       ;; property, which is used in `telega--desurrogate-apply'
       (replace-regexp-in-string
        (regexp-quote "\t") "%09"
        (replace-regexp-in-string (regexp-quote " ") "%20"
                                  (substring-no-properties text))))
      (textEntityTypeTextUrl
       (format "[[%s][%s]]" (plist-get ent-type :url) text))
      (textEntityTypeCustomEmoji
       (apply #'propertize text
              (telega--entity-to-properties (car entity-text) text)))
      (t text))))

(defun telega-string-fmt-text-length (str &optional rstart rend)
  "Return resulting formattedText length for the string STR.
Takes into account use of surrogated pairs for unicode
supplimentary planes.
RSTART and REND specifies STR region to work on."
  (+ (length str)
     ;; Number of chars from unicode supplimentary planes
     (save-match-data
       (with-temp-buffer
         (insert str)
         (count-matches "[\U00010000-\U0010FFFF]" (or rstart 0) rend)))))

(defun telega-fmt-text (text &optional entity-type)
  "Create formattedText from TEXT, marking whole TEXT with ENTITY-TYPE."
  (list :@type "formattedText"
        :text text
        :entities (if entity-type
                      (vector
                       (list :@type "textEntity"
                             :offset 0
                             :length (telega-string-fmt-text-length text)
                             :type entity-type))
                    [])))

(defun telega-string-as-markup (str markup-name markup-func &rest markup-args)
  "From STR create string with markup named MARKUP-NAME.
MARKUP-NAME can be nil, in this case markup outline is not displayed,
used by custom emojis.
MARKUP-FUNC is function taking string and returning formattedText.
MARKUP-ARGS additional arguments to MARKUP-FUNC."
  (declare (indent 3))
  (concat (propertize
           "❰" 'display (if markup-name
                            (propertize (concat "<" markup-name ">")
                                        'face 'shadow)
                          "")
           'rear-nonsticky t
           :telega-markup-start markup-func
           :telega-markup-args markup-args)
          str
          (propertize
           "❱" 'display (if markup-name
                            (propertize (concat "</" markup-name ">")
                                        'face 'shadow)
                          "")
           'rear-nonsticky t
           :telega-markup-end markup-func)))

(defun telega-markup-org--emphasis-fmt (str)
  "Convert STR emphasised by org mode to formattedText.
Return nil if STR is not emphasised by org mode."
  (when (get-text-property 0 'org-emphasis str)
    (let* ((tl-face (car (get-text-property 0 'face str)))
           (entity-type
            (cl-case tl-face
              (telega-entity-type-bold
               '(:@type "textEntityTypeBold"))
              (telega-entity-type-italic
               '(:@type "textEntityTypeItalic"))
              (telega-entity-type-underline
               '(:@type "textEntityTypeUnderline"))
              (telega-entity-type-pre
               '(:@type "textEntityTypePre"))
              (telega-entity-type-code
               '(:@type "textEntityTypeCode"))
              (telega-entity-type-strikethrough
               '(:@type "textEntityTypeStrikethrough"))
              (telega-entity-type-spoiler
               '(:@type "textEntityTypeSpoiler"))))
           (emphasis-size
            (if (eq (telega--tl-type entity-type) 'textEntityTypeSpoiler)
                2                       ; length of "||"
              1)))
      (telega-fmt-text
       (substring-no-properties str emphasis-size (- emphasis-size))
       entity-type))))

(defun telega-markup-org--begin-src-fmt (_str)
  "Format org mode src block specified by STR to formattedText.
Return nil if STR does not specify org mode source block."
  (error "TODO: format `begin_src' block")
  )

(defun telega-markup-org--link-fmt (str)
  "Format org link specified by STR to formattedText.
Return nil if STR does not specify an org mode link."
  (when (string-match org-link-any-re str)
    (let* ((link-text (match-string 3 str))
           (link-url (match-string 2 str))
           (user-id (when (string-prefix-p "tg://user?id=" link-url)
                      (string-to-number (substring link-url 13)))))
      (if user-id
          (telega-fmt-text link-text (list :@type "textEntityTypeMentionName"
                                           :user_id user-id))
        (telega-fmt-text link-text (list :@type "textEntityTypeTextUrl"
                                         :url link-url))))))

(defun telega-markup-org-fmt (str)
  "Format string STR to formattedText using Org Mode markup."
  (let ((seen-org-emphasis nil)
        (fmt-strings nil)
        (substrings
         (with-temp-buffer
           (save-excursion
             (insert str))
           ;; Mark spoilers (||spoiler text||) with special
           ;; `org-emphasis' property
           (save-excursion
             (while (re-search-forward "||[^|]+||" nil t)
               (add-text-properties (match-beginning 0) (match-end 0)
                                    '(face (telega-entity-type-spoiler)
                                           org-emphasis t))))
           (let ((org-hide-emphasis-markers nil)
                 (org-emphasis-alist '(("*" telega-entity-type-bold)
                                       ("/" telega-entity-type-italic)
                                       ("_" telega-entity-type-underline)
                                       ("=" telega-entity-type-pre)
                                       ("~" telega-entity-type-code)
                                       ("+" telega-entity-type-strikethrough))))
             (org-do-emphasis-faces nil)
             (telega--split-by-text-prop (buffer-string) 'org-emphasis)))))
    ;; NOTE: Traverse result collecting formatted strings
    (while substrings
      (let* ((str1 (car substrings))
             (fmt-str (cond ((get-text-property 0 'org-emphasis str1)
                             (setq seen-org-emphasis t)
                             (telega-markup-org--emphasis-fmt str1))
                            (seen-org-emphasis
                             ;; Trailing string
                             (cl-assert (= 1 (length substrings)))
                             (telega-markup-org-fmt str1))
                            (t
                             ;; Non-emphasised text
                             (telega-fmt-text str1)))))
        (push fmt-str fmt-strings))
      (setq substrings (cdr substrings)))
    (apply #'telega-fmt-text-concat (nreverse fmt-strings))))

(defun telega-markup-html-fmt (str)
  "Format string STR to formattedText using html markup."
  (telega-fmt-text-desurrogate
   (telega--parseTextEntities str (list :@type "textParseModeHTML"))))

(defun telega-markup-as-is-fmt (str)
  "Format string STR to formattedTetx as is, without applying any markup."
  (telega-fmt-text (substring-no-properties str)))

(defun telega-markup-markdown1-fmt (str)
  ;; For markdown mode, escape underscores in urls
  ;; See https://github.com/tdlib/td/issues/672
  ;; See https://github.com/zevlg/telega.el/issues/143
  (telega-fmt-text-desurrogate
   (telega--parseTextEntities
    (telega-escape-underscores str)
    (list :@type "textParseModeMarkdown"
          :version 1))))

(defun telega-markup-markdown2-fmt (str)
  (let ((fmt-text (telega--parseMarkdown (telega-fmt-text str)))
        (offset-shift 0))
    ;; Apply `telega-markdown2-backquotes-as-precode' logic
    (when telega-markdown2-backquotes-as-precode
      ;; Changing `textEntityTypePre' to `textEntityTypePreCode'
      ;; modifies also text of the entity, so we shift entities by
      ;; `offset-shift' to keep correct offset values.
      (seq-doseq (ent (plist-get fmt-text :entities))
        ;; NOTE: In emacs27 `cl-incf' with `plist-get' does not work,
        ;; thats why we use `plist-put' instead. See
        ;; https://t.me/emacs_ru/454836
        (plist-put ent :offset (+ (plist-get ent :offset) offset-shift))

        (when-let* ((ent-type (plist-get ent :type))
                    (ent-len (plist-get ent :length))
                    (beg (plist-get ent :offset))
                    (end (+ beg ent-len))
                    (pre-p (eq 'textEntityTypePre (telega--tl-type ent-type)))
                    (text (plist-get fmt-text :text))
                    (part (substring text beg end))
                    (lines (split-string part "\n"))
                    (lang-name (when (> (length lines) 1)
                                 (car lines))))
          (when (or (memq telega-markdown2-backquotes-as-precode '(t always))
                    (and (eq telega-markdown2-backquotes-as-precode 'known)
                         (fboundp (intern (concat lang-name "-mode")))))
            ;; Do entity transformation
            (let* ((lang-len (+ (length lang-name) 1)) ;count \n
                   (new-len (- ent-len lang-len)))
              (plist-put ent :type (list :@type "textEntityTypePreCode"
                                         :language lang-name))
              (plist-put ent :length new-len)
              (cl-decf offset-shift lang-len)

              (plist-put fmt-text :text
                         (concat (substring text 0 beg)
                                 (substring text (+ beg lang-len))))
              )))))
    (telega-fmt-text-desurrogate fmt-text)))

(defun telega-string-split-by-tl-entity-type (text default-markup-func)
  "Split TEXT by `:tl-entity-type'.
Return list of list where first element is markup function, second is
substring and rest are additional arguments to markup function."
  (mapcar (lambda (ss)
            (if-let ((ent-type (get-text-property 0 :tl-entity-type ss)))
                (list #'telega-fmt-text ss ent-type)
              (list default-markup-func ss)))
          (telega--split-by-text-prop text :tl-entity-type)))

(defun telega-string-split-by-markup (text &optional default-markup-func)
  "Split TEXT by markups.
Use DEFAULT-MARKUP-FUNC for strings without markup.
Return list of list where first element is markup function and
second is substring."
  (unless default-markup-func
    (setq default-markup-func #'telega-markup-as-is-fmt))

  (let ((start 0) (end (length text)) markup-start result)
    (while (setq markup-start
                 (text-property-not-all
                  start end :telega-markup-start nil text))
      (unless (eq start markup-start)
        ;; Extract custom emojis (and other entity types) from
        ;; non-markup substring
        (seq-doseq (ret (telega-string-split-by-tl-entity-type
                         (substring text start markup-start)
                         default-markup-func))
          (push ret result)))
      (let ((markup-end (text-property-not-all
                         markup-start end :telega-markup-end nil text)))
        (unless markup-end
          (user-error "Markup is non-closed"))
        (let ((markup-func (get-text-property
                            markup-start :telega-markup-start text))
              (markup-args (get-text-property
                            markup-start :telega-markup-args text)))
          (cl-assert (eq markup-func (get-text-property
                                      markup-end :telega-markup-end text)))
          (push (nconc (list markup-func (substring
                                          text (1+ markup-start) markup-end))
                       markup-args)
                result))
        ;; Skip markup-end char
        (setq start (1+ markup-end))))

    ;; Rest of the string
    (when (< start end)
      ;; Extract custom emojis (and other entity types) from
      ;; non-markup substring
      (seq-doseq (ret (telega-string-split-by-tl-entity-type
                       (substring text start)
                       default-markup-func))
        (push ret result)))
    (nreverse result)))

(defun telega-string-fmt-text (text &optional default-markup-func)
  "Convert TEXT to `formattedText' type.
DEFAULT-MARKUP-FUNC is passed directly to
`telega-string-split-by-markup', this markup function is used for TEXT
parts without explicit markup."
  (apply #'telega-fmt-text-concat
         (mapcar (apply-partially #'apply #'funcall)
                 (telega-string-split-by-markup text default-markup-func))))

(defun telega--entity-for-substring (ent from to)
  "Return new entity for `telega-fmt-text-substring'."
  ;; Few cases:
  ;;          from                   to
  ;;           |------substring------|
  ;; |----------------original string---------------------------|
  ;; |--ent--| (outside substring)
  ;;    |---ent---| (partially inside substring)
  ;;                |----ent---| (inside substring)
  ;;                             |---ent---|  (partially inside)
  ;;                                      |--ent--| (outside substring)
  ;;
  (let* ((ent-off (plist-get ent :offset))
         (ent-len (plist-get ent :length))
         (ent-end (+ ent-off ent-len))
         (ent-ioff (if (< ent-off from) from ent-off))
         (ent-iend (if (> ent-end to) to ent-end)))
    (when (and (>= ent-ioff from) (<= ent-iend to) (> ent-iend ent-ioff))
      (list :@type "textEntity"
            :offset (- ent-ioff from)
            :length (- ent-iend ent-ioff)
            :type (plist-get ent :type)))))

(defun telega-fmt-text-substring (fmt-text &optional from to)
  "Return new formattedText whose contents are a substring of FMT-TEXT.
FROM and TO arguments are the same as for `substring'."
  (let* ((text (plist-get fmt-text :text))
         (text-length (length text))
         (ito (if (and to (< to 0)) (+ text-length to) (or to text-length)))
         (ifrom (if (and from (< from 0)) (+ text-length from) (or from 0)))
         (new-text (substring text ifrom ito))
         (new-ents (cl-map 'vector
                           (lambda (ent)
                             (telega--entity-for-substring ent ifrom ito))
                           (plist-get fmt-text :entities))))
    (list :@type "formattedText"
          :text new-text
          :entities (cl-remove nil new-ents))))

(defun telega-fmt-text-desurrogate (fmt-text)
  "Prepare FMT-TEXT to be used in imc.
Destructively desurrogates `:text' property of FMT-TEXT.
Return desurrogated formattedText."
  (plist-put fmt-text :text (or (telega-tl-str fmt-text :text 'no-props) "")))

(defun telega-fmt-text-concat (&rest fmt-texts)
  "Concatenate FMT-TEXTS, returning newly created formattedText."
  (let* ((offset 0)
         (ents (mapcar
                (lambda (fmt-text)
                  (prog1
                      (mapcar (lambda (ent)
                                (list :@type "textEntity"
                                      :offset (+ offset (plist-get ent :offset))
                                      :length (plist-get ent :length)
                                      :type (plist-get ent :type)))
                              (plist-get fmt-text :entities))
                    (cl-incf offset (telega-string-fmt-text-length
                                     (plist-get fmt-text :text)))))
                fmt-texts)))
    (list :@type "formattedText"
          :text (apply #'concat (mapcar (telega--tl-prop :text) fmt-texts))
          :entities (apply #'seq-concatenate 'vector ents))))

(defun telega--fmt-text-markup (fmt-text entity-to-markup-fun)
  "Return formatted text FMT-TEXT as string by applying entity function.
ENTITY-TO-MARKUP-FUN is function to convert TDLib entities to string."
  (let ((text (copy-sequence (plist-get fmt-text :text)))
        (offset 0)
        (strings nil))
    (seq-doseq (ent (plist-get fmt-text :entities))
      (let ((ent-off (plist-get ent :offset))
            (ent-len (plist-get ent :length)))
        ;; Part without attached entity
        (when (> ent-off offset)
          (push (cons nil (substring text offset ent-off)) strings))

        (setq offset (+ ent-off ent-len))
        (push (cons ent (substring text ent-off offset)) strings)))
    ;; Trailing part, may be empty
    (push (cons nil (substring text offset)) strings)

    ;; NOTE: remove any 'face properties from the string, so they
    ;; won't intermix with markdown syntax.
    ;; But keep 'display property, so emojis are still displayed as
    ;; images (if `telega-emoji-use-images' is set)
    (let ((ret-text (apply 'concat (mapcar entity-to-markup-fun
                                           (nreverse strings)))))
      (remove-text-properties 0 (length ret-text) (list 'face) ret-text)
      ret-text)))

(defun telega--fmt-text-markdown1 (fmt-text)
  "Return formatted text FMT-TEXT as string with markdown1 syntax."
  (telega--fmt-text-markup fmt-text #'telega--entity-to-markdown))

(defun telega--fmt-text-markdown2 (fmt-text)
  "Return formatted text FMT-TEXT as string with markdown2 syntax."
  (plist-get (telega--getMarkdownText
              (telega-fmt-text-desurrogate (copy-sequence fmt-text)))
             :text))

(defun telega--fmt-text-org (fmt-text)
  "Return formatted text FMT-TEXT as string with org mode syntax."
  (telega--fmt-text-markup fmt-text #'telega--entity-to-org))

;; NOTE: FOR-MSG might be used by advices, see contrib/telega-mnz.el
(defun telega--fmt-text-faces (fmt-text &optional _for-msg)
  "Apply faces to formatted text FMT-TEXT.
Return text string with applied faces."
  (let ((text (copy-sequence (plist-get fmt-text :text))))
    (seq-doseq (ent (plist-get fmt-text :entities))
      (let* ((beg (plist-get ent :offset))
             (end (+ (plist-get ent :offset) (plist-get ent :length)))
             (props (telega--entity-to-properties
                     ent (substring-no-properties text beg end)))
             (face (plist-get props 'face)))
        (when props
          (add-text-properties beg end (nconc (list 'rear-nonsticky t
                                                    'front-sticky t)
                                              (telega-plist-del props 'face))
                               text))
        (when face
          (add-face-text-property beg end face 'append text))))
    text))

(defun telega--region-by-text-prop (beg prop)
  "Return region after BEG point with text property PROP set."
  (unless (get-text-property beg prop)
    (setq beg (next-single-char-property-change beg prop)))
  (let ((end (next-single-char-property-change beg prop)))
    (when (> end beg)
      (cons beg end))))

(defun telega--split-by-text-prop (string prop)
  "Split STRING by property PROP changes."
  (let ((start 0) end result)
    (while (and (> (length string) start)
                (setq end (next-single-char-property-change start prop string)))
      (push (substring string start end) result)
      (setq start end))
    (nreverse result)))

(defun telega--region-with-cursor-sensor (pos)
  "Locate region of the button with `cursor-sensor-functions'.
Return `nil' if there is no button with `cursor-sensor-functions' at POS."
  (when-let ((sensor-funcs (get-text-property pos 'cursor-sensor-functions)))
    (let ((prev (previous-single-property-change pos 'cursor-sensor-functions)))
      (when (and prev (eq (get-text-property prev 'cursor-sensor-functions)
                          sensor-funcs))
        (setq pos prev))
      (telega--region-by-text-prop pos 'cursor-sensor-functions))))

(defun telega-completing-read-chat (prompt &optional chats sort-criteria)
  "Read chat by title.
CHATS - list of the chats to select from.  By default all chats are used.
SORT-CRITERIA is a chat sort criteria to apply. (NOT YET)"
  (telega-completing-read-msg-sender
   prompt
   (telega-sort-chats
    (or sort-criteria telega-chat-completing-sort-criteria)
    (telega-filter-chats (or chats telega--ordered-chats)
                         '(or main archive has-chatbuf)))))

(defmacro telega-gen-completing-read-list (prompt items-list item-fmt-fun
                                                  item-read-fun &rest args)
  "Generate list reading function."
  (let ((items (gensym "items"))
        (candidates (gensym "candidates"))
        (rm-item (gensym "rm-item")))
    `(let ((,candidates ,items-list)
           (,items nil))
       (while (and ,candidates
                   (condition-case nil
                       (setq ,items
                             (append ,items
                                     (list (funcall
                                            ,item-read-fun
                                            (concat ,prompt " (C-g when done)"
                                                    (when ,items
                                                      (concat
                                                       " [" (mapconcat
                                                             ,item-fmt-fun
                                                             ,items ",")
                                                       "]")) ": ")
                                            ,candidates
                                            ,@args))))
                     (quit nil)))
         (setq ,candidates (cl-remove-if (lambda (,rm-item)
                                           (memq ,rm-item ,items))
                                         ,candidates)))
       ,items)))

(defun telega-completing-read-chat-list (prompt &optional chats-list
                                                sort-criteria)
  "Read multiple chats from CHATS-LIST."
  (setq chats-list
        (telega-filter-chats (or chats-list telega--ordered-chats)
                             '(or main archive)))
  (telega-gen-completing-read-list prompt chats-list #'telega-chatbuf--name
                                   #'telega-completing-read-chat sort-criteria))

(defun telega-completing-read-user (prompt &optional users)
  "Read user by his name from USERS list."
  (declare (indent 1))
  (let ((choices (mapcar (lambda (user)
                           (list (concat (telega-symbol 'contact)
                                         (telega-user-title user))
                                 user))
                         (or users (hash-table-values
                                    (alist-get 'user telega--info))))))
    (car (alist-get (funcall telega-completing-read-function
                             prompt choices nil t)
                    choices nil nil 'string=))))

(defun telega-completing-read-user-list (prompt &optional users-list)
  "Read multiple users from USERS-LIST."
  (declare (indent 1))
  (unless users-list
    (setq users-list (hash-table-values (alist-get 'user telega--info))))
  (telega-gen-completing-read-list prompt users-list #'telega-user-title
                                   #'telega-completing-read-user))

(defvar telega-completing--chat-member-alist nil
  "Results from last `telega--searchChatMembers'.
To be used `telega-completing-read-chat-member' to get user.")

(defun telega-completing--chat-members-collection (chat prefix &rest _ignored)
  "Function used for programmed completing CHAT members.
Works only with `fido-mode' completion."
  (when prefix
    (setq telega-completing--chat-member-alist
          (mapcar (lambda (user)
                    (cons (propertize
                           (telega-msg-sender-title-for-completion user)
                           :user user)
                          user))
                  (telega--searchChatMembers chat prefix)))
    (mapcar #'car telega-completing--chat-member-alist)))

(defun telega-completing-read-chat-member (prompt chat)
  "Interactively read member of CHAT.
Return a user."
  (let* ((telega-completing--chat-member-alist nil)
         (name
          (if fido-mode
              ;; Dynamic completion, can complete any chat member
              ;; Works ok only in `fido-mode'
              (completing-read
               prompt
               (apply-partially
                #'telega-completing--chat-members-collection chat))

            ;; Static completion, can complete only 50 chat members
            (funcall telega-completing-read-function
                     prompt
                     (telega-completing--chat-members-collection chat "")
                     nil t))))
    (cdr (assoc name telega-completing--chat-member-alist))))

(defun telega-completing-read-folder (prompt &optional folder-names)
  "Read TDLib folder name completing."
  (funcall telega-completing-read-function
           prompt (or folder-names (telega-folder-names))
           nil t))

(defun telega-completing-read-folder-list (prompt &optional folder-names)
  "Read list of the Telegram folders prompting with PROMPT."
  (unless folder-names
    (setq folder-names (telega-folder-names)))
  (telega-gen-completing-read-list prompt folder-names #'identity
                                   #'telega-completing-read-folder))

(defun telega-completing-read-folder-icon-name (prompt &optional initial-input)
  "Read folder's icon name."
  (funcall telega-completing-read-function prompt
           (mapcar (lambda (icon-name)
                     (propertize icon-name 'display
                                 (concat (cdr (assoc icon-name
                                                     telega-folder-icons-alist))
                                         icon-name)))
                   telega-folder-icon-names)
           nil t initial-input))

(defun telega-location-distance (loc1 loc2 &optional components-p)
  "Return distance in meters between locations LOC1 and LOC2.
If COMPONENTS-P is given, then return cons with distances for
latitude and longitude."
  (let* ((lat1 (plist-get loc1 :latitude))
         (lat2 (plist-get loc2 :latitude))
         (lon1 (plist-get loc1 :longitude))
         (lon2 (plist-get loc2 :longitude))
         (lat-d (* 111111 (- lat1 lat2)))
         (lon-d (* 111111 (cos (degrees-to-radians (/ (+ lat1 lat2) 2)))
                   (- lon2 lon1))))
    (if components-p
        (cons lat-d lon-d)
      (round (sqrt (+ (* lat-d lat-d) (* lon-d lon-d)))))))

(defun telega-location-to-string (location)
  "Convert LOCATION plist to string representation."
  (concat (number-to-string
           (plist-get location :latitude))
          "N" ","
          (number-to-string
           (plist-get location :longitude))
          "E"))

(defun telega-read-file-name (prompt &optional dir default-filename
                                     mustmatch initial predicate)
  "Like `read-file-name' but taking `telega-dired-dwim-target' into account."
  (read-file-name prompt
                  (or dir
                      (when telega-dired-dwim-target
                        (let ((dired-dwim-target telega-dired-dwim-target))
                          (dired-dwim-target-directory))))
                  default-filename mustmatch initial predicate))

(defun telega-read-location (prompt &optional initial-loc default-loc history)
  "Read location with PROMPT.
INITIAL-LOC - location converted to INITIAL-INPUT argument to `read-string'.
DEFAULT-LOC - location converted to DEFAULT-VALUE argument to `read-string'.
Return location as plist."
  (let* ((default-value (or (when default-loc
                              (telega-location-to-string default-loc))
                            (when telega-my-location
                              (telega-location-to-string telega-my-location))))
         (initial-input (when initial-loc
                          (telega-location-to-string initial-loc)))
         loc)
    (while (let ((locstr (read-string
                          (concat prompt
                                  (when default-value
                                    (concat " [" default-value "]")) ": ")
                          initial-input history default-value)))
             (setq loc (mapcar #'string-to-number (split-string locstr ",")))
             (unless (and (numberp (car loc)) (numberp (cadr loc)))
               (message "Invalid location `%s', use: <LAT>,<LONG> format" locstr)
               (sit-for 1)
               t)))
    (list :latitude (car loc) :longitude (cadr loc))))

(defun telega-read-live-location (prompt &rest args)
  "Read live location with PROMPT.
All args are passed directly to `telega-read-location'.
Packages may advice this function to extend functionality."
  (apply #'telega-read-location prompt args))

(defun telega-read-im-sure-p (prompt)
  "Ask user he sure about some action.
Return non-nil only if \"i'm sure\" is typed in."
  (let ((input (read-string
                (concat prompt " (type \"i'm sure\" to confirm): "))))
    (string-equal input "i'm sure")))

(defun telega-completing-read-slow-mode-delay (prompt)
  "Read slow mode delay completing input."
  (let* ((choices (mapcar
                   (lambda (delay)
                     (cons (if (zerop delay)
                               (telega-i18n "lng_rights_slowmode_off")
                             (telega-duration-human-readable delay))
                           delay))
                   telega--slow-mode-delays))
         (choice (funcall telega-completing-read-function
                          prompt (mapcar #'car choices) nil t)))
    (cdr (assoc choice choices))))

(defun telega-completing-read-mute-for (prompt)
  "Read mute for notification parameter."
  (let* ((choices (mapcar
                   (lambda (delay)
                     (cons (cond ((>= delay telega-mute-for-ever)
                                  (telega-i18n "lng_mute_duration_forever"))
                                 ((zerop delay)
                                  "Disable")
                                 (t
                                  (telega-duration-human-readable
                                   delay 1 " days" " hours" " minutes")))
                           delay))
                   telega-mute-for-intervals))
         (choice (funcall telega-completing-read-function
                          prompt (mapcar #'car choices) nil t)))
    (cdr (assoc choice choices))))

(defun telega-read-discussion-chat ()
  "Interactively select or create a chat as discussion group for some channel.
Return a chat."
  (let* ((existing-p (y-or-n-p "Use existing chat? "))
         (linked-chat (if existing-p
                          (telega-completing-read-chat
                           "Select suitable chat: "
                           (telega--getSuitableDiscussionChats))
                        (telega-chat-create "supergroup")))
         (supergroup (when (eq (telega-chat--type linked-chat) 'supergroup)
                       (telega-chat--supergroup linked-chat))))
    (unless supergroup
      ;; Need to upgrade to supergroup first
      (unless (y-or-n-p "Upgrade it to supergroup? ")
        (error "Discussion group must be a supergroup"))
      (setq linked-chat (telega--upgradeBasicGroupChatToSupergroupChat
                         linked-chat)
            supergroup (telega-chat--supergroup linked-chat)))

    (cl-assert supergroup)
    (unless (plist-get (telega--full-info supergroup) :is_all_history_available)
      ;; Need to toggle is_all_history_available prop
      (unless (y-or-n-p "Toggle all history available? ")
        (error "Discussion group is required to have all the history visible"))
      (telega--toggleSupergroupIsAllHistoryAvailable supergroup t))
    linked-chat))

(defun telega-completing-read-permission (prompt &optional permissions)
  "Read a permission from PERMISSIONS list completing user input.
If PERMISSIONS is ommited, then `telega-chat--chat-permisions' is used."
  (let* ((raw-perms (or permissions telega-chat--chat-permisions))
         (i18n-choices (cl-remove
                        nil (mapcar (lambda (perm-spec)
                                      (when (cdr perm-spec)
                                        (cons (telega-i18n (cdr perm-spec))
                                              (car perm-spec))))
                                    raw-perms)))
         (perm-choice (funcall telega-completing-read-function
                               prompt (mapcar #'car i18n-choices) nil t)))
    (cdr (assoc perm-choice i18n-choices))))

(defun telega-msg-sender-title-for-completion (msg-sender)
  "Return MSG-SENDER title for completions."
  (let* ((user-p (telega-user-p msg-sender))
         (chat-p (not user-p)))
    (telega-ins--as-string
     (cond ((and chat-p (telega-chat-secret-p msg-sender))
            (telega-ins (telega-symbol 'lock)))
           (user-p
            (telega-ins (telega-symbol 'member))))
     (when user-p
       (telega-ins "{"))
     (when telega-use-images
       (telega-ins--image
        (telega-msg-sender-avatar-image-one-line msg-sender)))
     (if user-p
         (telega-ins--with-attrs
             (list :face (telega-msg-sender-title-faces msg-sender))
           (telega-ins (telega-msg-sender-title msg-sender)))
       (telega-ins
        (telega-chat-title-with-brackets msg-sender " ")))
     (when user-p
       (telega-ins "}"))
     )))

;; NOTE: ivy returns copy of the string given in choices, thats why we
;; need to use `assoc'
(defun telega-completing-read-msg-sender (prompt &optional msg-senders)
  "Read a message sender from list of MSG-SENDERS."
  (let* ((completion-ignore-case t)
         (choices (mapcar (lambda (sender)
                            (cons (telega-msg-sender-title-for-completion sender)
                                  sender))
                          msg-senders))
         (choice (funcall telega-completing-read-function
                          prompt (mapcar #'car choices) nil t)))
    (cdr (assoc choice choices))))

(defun telega--animate-dots (text)
  "Animate TEXT's trailing dots.
Return `nil' if there is nothing to animate and new string otherwise."
  (when (string-match "\\.+$" text)
    (concat (substring text nil (match-beginning 0))
            (make-string
             (1+ (% (- (match-end 0) (match-beginning 0)) 3)) ?.))))


;; ewoc stuff
(defun telega-ewoc--gen-pp (pp-fun)
  "Wrap pretty printer function PP-FUN trapping all errors.
Do not trap errors if `debug-on-error' is enabled."
  (if telega-debug
      pp-fun

    (lambda (arg)
      (condition-case-unless-debug pp-err
          (funcall pp-fun arg)
        (t
         (telega-debug "PP-ERROR: (%S %S) ==>\n" pp-fun arg)
         (telega-debug "    %S\n" pp-err)
         (telega-debug "--------\n")

         (telega-ins "---[telega bug]\n")
         (telega-ins-fmt "PP-ERROR: (%S %S) ==>\n" pp-fun arg)
         (telega-ins-fmt "  %S\n" pp-err)
         (telega-ins "------\n"))))))

(defun telega-ewoc--location (ewoc)
  "Return EWOC's start location."
  (ewoc-location (ewoc--header ewoc)))

(defun telega-ewoc--find (ewoc item test &optional key start-node iter-func)
  "Find EWOC's node by item and TEST funcion.
TEST function is run with two arguments - ITEM and NODE-VALUE.
Optionally KEY can be specified to get KEY from node value.
START-NODE is node to start from, default is first node if
ITER-FUNC is `ewoc--node-next' and last node if ITER-FUNC is
`ewoc--node-prev'.
ITER-FUNC is one of `ewoc--node-next' or `ewoc--node-prev'.
Default is `ewoc--node-next'.
Return EWOC node, nil if not found."
  (unless iter-func
    (setq iter-func #'ewoc--node-next))
  (cl-assert (memq iter-func '(ewoc--node-next ewoc--node-prev)))

  (ewoc--set-buffer-bind-dll-let* ewoc
      ((stop (if (eq iter-func #'ewoc--node-next)
                 (ewoc--footer ewoc)
               (cl-assert (eq iter-func #'ewoc--node-prev))
               (ewoc--header ewoc)))
       (node (or start-node
                 (if (eq iter-func #'ewoc--node-next)
                     (ewoc--node-next dll (ewoc--header ewoc))
                   (ewoc--node-prev dll (ewoc--footer ewoc)))))
       (inhibit-read-only t))
    (cl-block 'ewoc-node-found
      (while (not (eq node stop))
        (when (funcall test item (if key
                                     (funcall key (ewoc--node-data node))
                                   (ewoc--node-data node)))
          (cl-return-from 'ewoc-node-found node))
        (setq node (funcall iter-func dll node))))))

(defun telega-ewoc-map-refresh (ewoc map-function &rest args)
  "Refresh nodes if MAP-FUNCTION return non-nil.
If MAP-FUNCTION returns `stop' then stop refreshing."
  (declare (indent 1))
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((footer (ewoc--footer ewoc))
       (pp (ewoc--pretty-printer ewoc))
       (node (ewoc--node-nth dll 1)))
    (save-excursion
      (while (not (eq node footer))
        (when-let ((refresh-p (apply map-function (ewoc--node-data node) args)))
          (ewoc--refresh-node pp node dll)
          (when (eq refresh-p 'stop)
            (setq node (ewoc--node-prev dll footer))))
        (setq node (ewoc--node-next dll node))))))

(defun telega-ewoc--find-if (ewoc predicate &optional key start-node iter-func)
  "Find EWOC's node by PREDICATE run on node's data.
KEY, START-NODE and ITER-FUNC are passed directly to `telega-ewoc--find'."
  (declare (indent 1))
  (telega-ewoc--find
   ewoc nil (lambda (_ignored node-value)
              (funcall predicate node-value))
   key start-node iter-func))

(defmacro telega-ewoc--find-by-data (ewoc data)
  `(telega-ewoc--find ,ewoc ,data 'eq))

(defun telega-ewoc--set-header (ewoc header)
  "Set EWOC's new HEADER."
  ;; NOTE: No ewoc API to change just header :(
  ;; only `ewoc-set-hf'
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((head (ewoc--header ewoc))
       (hf-pp (ewoc--hf-pp ewoc)))
    (setf (ewoc--node-data head) header)
    (ewoc--refresh-node hf-pp head dll)))

(defun telega-ewoc--set-footer (ewoc footer)
  "Set EWOC's new FOOTER."
  ;; NOTE: No ewoc API to change just footer :(
  ;; only `ewoc-set-hf'
  (declare (indent 1))
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((foot (ewoc--footer ewoc))
       (hf-pp (ewoc--hf-pp ewoc)))
    (setf (ewoc--node-data foot) footer)
    (ewoc--refresh-node hf-pp foot dll)))

(defun telega-ewoc--set-pp (ewoc pretty-printer)
  "Set EWOC's pretty printer to PRETTY-PRINTER.
Does NOT refreshes the contents, use `ewoc-refresh' to refresh."
  (setf (ewoc--pretty-printer ewoc) pretty-printer))

(defun telega-ewoc--clean (ewoc)
  "Delete all nodes from EWOC.
Header and Footer are not deleted."
  (ewoc-filter ewoc 'ignore))

(defun telega-ewoc--empty-p (ewoc)
  "Return non-nil if there is no visible EWOC nodes."
  (let ((n0 (ewoc-nth ewoc 0)))
    (or (null n0)
        (= (ewoc-location (ewoc-nth ewoc 0))
           (ewoc-location (ewoc--footer ewoc))))))

(defun telega-ewoc--move-node (ewoc node before-node
                                    &optional save-point-p)
  "Move EWOC's NODE before BEFORE-NODE node, saving point at NODE position.
If NODE and BEFORE-NODE are the same, then just invalidate the node.
If BEFORE-NODE is nil, then move NODE to the bottom.
Save point only if SAVE-POINT is non-nil."
  (let* ((node-value (ewoc--node-data node))
         (button (button-at (point)))
         (point-off (and button save-point-p
                         (eq (button-get button :value) node-value)
                         (- (point) (button-start button)))))
    (telega-save-excursion
      (if (eq node before-node)
          (ewoc-invalidate ewoc node)

        (ewoc-delete ewoc node)
        (setq node
              (if before-node
                  (ewoc-enter-before ewoc before-node node-value)
                (ewoc-enter-last ewoc node-value)))))

    (when (and point-off
               ;; See https://github.com/zevlg/telega.el/issues/197
               (not (equal (ewoc-location node)
                           (when-let ((next-node (ewoc-next ewoc node)))
                             (ewoc-location next-node)))))
      (goto-char (+ (ewoc-location node) point-off))
      (dolist (win (get-buffer-window-list))
        (set-window-point win (point))))))

(defun telega-svg-create-vertical-bar (&optional bar-width bar-position bar-str)
  "Create svg image for vertical bar.
BAR-STR is string value for textual vertical bar, by default
`telega-symbol-vertical-bar' is used.
BAR-WIDTH and BAR-POSITION defines how vertical bar is drawn.  If
float values then it is relative to bar width in pixels.  If
integer values, then pixels used."
  (unless bar-str
    (setq bar-str telega-symbol-vertical-bar))
  (or (telega-emoji--image-cache-get bar-str (telega-chars-xheight 1))
      (let* ((xh (telega-chars-xheight 1))
             (xw (telega-chars-xwidth (string-width bar-str)))
             (svg (telega-svg-create xw xh))
             (bar-xpos (if (integerp bar-position)
                           bar-position
                         (round (* xw (or bar-position 0.3)))))
             (bar-xw (if (integerp bar-width)
                         bar-width
                       (round (* xw (or bar-width 0.07)))))
             image)
        (when (< bar-xw 1)
          (setq bar-xw 1))
        (svg-rectangle svg bar-xpos 0 bar-xw xh
                       ;; NOTE: this filling params inherits face's
                       ;; color this svg is displayed under
                       :fill-opacity 1
                       :fill-rule "nonzero")
        (setq image (telega-svg-image svg :scale 1.0
                                      :width xw :height xh
                                      :ascent 'center
                                      :mask 'heuristic
                                      :telega-text bar-str))
        (telega-emoji--image-cache-put bar-str image)
        image)))

(defun telega-svg-create-horizontal-bar (&optional bar-width bar-position bar-str)
  "Create svg image for a horizontal bar.
BAR-STR is string value for textual horizontal bar, by default
`telega-symbol-horizontal-bar' is used.
BAR-WIDTH and BAR-POSITION defines how horizontal bar is drawn.  If
float values then it is relative to bar height in pixels.  If
integer values, then absolute value in pixels is used."
  (unless bar-str
    (setq bar-str telega-symbol-horizontal-bar))
  ;; NOTE: horizontal bars are frequently used as consecutive
  ;; characters.  However in Emacs if consecutive chars has `eq'
  ;; `display' property it is displayed as single unit (ref: 40.16.1
  ;; Display Specs That Replace The Text).  So we use `seq-copy' to
  ;; make `display' property for horizontal-bar differ
  (seq-copy
   (or (telega-emoji--image-cache-get bar-str (telega-chars-xheight 1))
       (let* ((xh (telega-chars-xheight 1))
              (xw (telega-chars-xwidth (string-width bar-str)))
              (svg (telega-svg-create xw xh))
              (bar-xw (if (integerp bar-width)
                          bar-width
                        (round (* xh (or bar-width 0.07)))))
              (bar-ypos (- (if (integerp bar-position)
                               bar-position
                             (round (* xh (or bar-position 0.5))))
                           (/ bar-xw 2)))
              image)
         (when (< bar-xw 1)
           (setq bar-xw 1))
         (svg-rectangle svg 0 bar-ypos xw bar-xw
                        ;; NOTE: this filling params inherits face's
                        ;; color this svg is displayed under
                        :fill-opacity 1
                        :fill-rule "nonzero")
         (setq image (telega-svg-image svg :scale 1.0
                                       :width xw :height xh
                                       :ascent 'center
                                       :mask 'heuristic
                                       :telega-text bar-str))
         (telega-emoji--image-cache-put bar-str image)
         image))))

(defun telega-symbol-emojify (emoji &optional image-spec)
  "Return a copy of EMOJI with  `display' property of EMOJI svg image.
Optionally IMAGE-SPEC could be used to ommit svg creation and use
another image."
  ;; Possible eval a form to get real IMAGE-SPEC
  (when (and (listp image-spec)
             (not (eq 'image (car image-spec))))
    (setq image-spec (eval image-spec t)))

  (let ((image (cond ((and (listp image-spec) (eq 'image (car image-spec)))
                      image-spec)
                     (image-spec
                      (error "Invalid image spec for the %S symbol" emoji))
                     (t
                      (telega-emoji-create-svg emoji)))))
    (propertize emoji 'rear-nonsticky '(display) 'display image)))

(defun telega-symbol (ending)
  "Return possible emojified value for the symbol denoted by ENDING.
ENDING could be a string or a symbol.
If ENDING is a symbol, then value is taken from `telega-symbol-ENDING'
variable.
Only endings listed in `telega-symbols-emojify' are emojified."
  (let ((value (if (stringp ending)
                   ending
                 (symbol-value (intern (format "telega-symbol-%s" ending)))))
        (image-spec (cdr (assoc ending telega-symbols-emojify))))
    (cond ((functionp image-spec)
           (funcall image-spec ending))
          ((or (and telega-use-images image-spec)
               (and telega-emoji-use-images
                    (member ending telega-symbols-emojify)))
           (apply #'telega-symbol-emojify value image-spec))
          (t
           (or (and (functionp telega-symbols-emojify-function)
                    (funcall telega-symbols-emojify-function ending))
               value)))))


(defun telega-diff-wordwise (str1 str2 &optional colorize)
  "Compare two strings STR1 and STR2 wordwise.
If COLORIZE is non-nil, then colorize changes with `font-lock-face'.
Uses `git' command to compare."
  (let ((tmp1 (make-temp-file "telega-diff1"))
        (tmp2 (make-temp-file "telega-diff2")))
    (unwind-protect
        (progn
          (with-temp-file tmp1
            (insert str1))
          (with-temp-file tmp2
            (insert str2))
          (ansi-color-apply
           (shell-command-to-string
            (concat "git diff --word-diff-regex=. --word-diff="
                    (if colorize "color" "plain")
                    " --no-index " tmp1 " " tmp2 " | tail -n +6"))))

      ;; cleanup
      (delete-file tmp1)
      (delete-file tmp2))))

(defun telega-momentary-display (string &optional pos exit-char)
  "Momentarily display STRING in the buffer at POS.
Display remains until next event is input.
Same as `momentary-string-display', but keeps the point."
  (unless exit-char
    (setq exit-char last-command-event))
  ;; NOTE: `read-key' might trigger changes in the rootbuf, thats why
  ;; save start/end as markers
  (let* ((start (copy-marker (or pos (point))))
         (end start))
    (unwind-protect
        (progn
          (save-excursion
            (goto-char start)
            (insert string)
            (setq end (copy-marker (point) t)))
          (message "Type %s to continue editing."
                   (single-key-description exit-char))
          (let ((event (read-key)))
            ;; `exit-char' can be an event, or an event description list.
            (or (eq event exit-char)
                (eq event (event-convert-list exit-char))
                (setq unread-command-events
                      (append (this-single-command-raw-keys)
                              unread-command-events)))))
      (delete-region start end))))

(defun telega-remove-face-text-property (start end face &optional object)
  "Remove FACE value from face text property at START END region of the OBJECT."
  (let ((faces (get-text-property start 'face object)))
    (cond ((listp faces)
           (setq faces (delete face faces)))
          ((eq faces face)
           (setq faces nil)))
    (put-text-property start end 'face faces)))

(defun telega-button-highlight--sensor-func (_window oldpos dir)
  "Sensor function to highlight buttons with `telega-button-highlight'."
  (let ((inhibit-read-only t)
        (button (button-at (if (eq dir 'entered) (point) oldpos))))
    (when button
      (funcall (if (eq dir 'entered)
                   #'add-face-text-property
                 #'telega-remove-face-text-property)
               (button-start button)
               (button-end button)
               'telega-button-highlight)
      (when (eq dir 'entered)
        (telega-button--help-echo button)))))

(defun telega-screenshot-with-import (tofile &optional region-p)
  "Make a screenshot into TOFILE using imagemagick's import utility.
If REGION-P is non-nil, then make a screenshot of region."
  (let ((import-cmd
         (concat (or (executable-find "import")
                     (error "Utility `import' (imagemagick) not found"))
                 " -silent"             ;no beep
                 (unless region-p " -window root")
                 " " tofile)))
    (call-process-shell-command import-cmd)))

(defun telega-screenshot-with-flameshot (tofile &optional region-p)
  "Make a screenshot into TOFILE using `flameshot' utility.
If REGION-P is non-nil, then make a screenshot of region."
  (let* ((flameshot-cmd (concat (or (executable-find "flameshot")
                                    (error "Utility `flameshot' not found"))
                                " " (if region-p "gui" "full")
                                " -r"))
         (coding-system-for-write 'binary)
         (png-output (shell-command-to-string flameshot-cmd)))
    (when (string-prefix-p "\x89PNG" png-output)
      (write-region png-output nil tofile nil 'quiet))))

(defun telega-screenshot-with-scrot (tofile &optional region-p)
  "Make a screenshot into TOFILE using `scrot' utility.
If REGION-P is non-nil, then make a screenshot of region."
  (let ((scrot-cmd (concat (or (executable-find "scrot")
                               (error "Utility `scrot' not found"))
                           " " (when region-p "-s")
                           " " tofile)))
    (call-process-shell-command scrot-cmd)))

(defun telega-screenshot-with-maim (tofile &optional region-p)
  "Make a screenshot into TOFILE using `maim' utility.
If REGION-P is non-nil, then make a screenshot of region."
  (let ((maim-cmd (concat (or (executable-find "maim")
                              (error "Utility `maim' not found"))
                          " " (when region-p "-s")
                          " " tofile)))
    (call-process-shell-command maim-cmd)))

(defun telega-screenshot-with-gnome-screenshot (tofile &optional region-p)
  "Make a screenshot into TOFILE using `gnome-screenshot' utility.
If REGION-P is non-nil, then make a screenshot of region."
  (let ((gs-cmd (concat (or (executable-find "gnome-screenshot")
                            (error "Utility `gnome-screenshot' not found"))
                        " " (when region-p "-a")
                        " -f " tofile)))
    (call-process-shell-command gs-cmd)))

(defun telega-screenshot-with-screencapture (tofile &optional region-p)
  "Make a screenshot into TOFILE using `screencapture' utility.
If REGION-P is non-nil, then make a screenshot of region."
  (let ((screencapture-cmd (concat (or (executable-find "screencapture")
                                       (error "Utility `screencapture' not found"))
                                   (if region-p  " -i "  " ")
                                   tofile)))
    (call-process-shell-command screencapture-cmd)))

(defun telega-screenshot-with-pngpaste (tofile &optional _region-p)
  "Make a screenshot into TOFILE using `pngpaste' utility.
If REGION-P is non-nil, then make a screenshot of region."
  (let ((pngpaste-cmd (concat (or (executable-find "pngpaste")
                                  (error "Utility `pngpaste' not found"))
                              " " tofile)))
    (call-process-shell-command pngpaste-cmd)))

(defun telega-read-timestamp (prompt &optional only-date-p)
  "Interactively read timestamp in the future.
If ONLY-DATE-P is specified, then query for date only.
Return timestamp as unix time."
  (interactive)
  (let ((date-time (org-read-date (not only-date-p) t nil prompt)))
    ;; NOTE: we use `apply' to support Emacs 26
    ;; see https://t.me/emacs_telega/14017
    (round (time-to-seconds (apply #'encode-time (decode-time date-time))))))

(defun telega-open-file (filename &optional msg)
  "Open FILENAME inside telega.
MSG is the message associated with FILENAME."
  (let ((saved-buffer (current-buffer)))
    ;; NOTE: For newly downloaded FILENAME modtime could differ from
    ;; modtime of existing buffer, causing annoying "Reread file from
    ;; disk?" query from Emacs.  We workaround this by updating
    ;; buffer's modtime to filename's modtime
    (when-let ((buf (get-file-buffer filename)))
      (with-current-buffer buf
        (unless (buffer-modified-p)
          (set-visited-file-modtime))))

    (funcall telega-open-file-function filename)

    (unless (eq saved-buffer (current-buffer))
      ;; NOTE: FILENAME has been opened inside Emacs, we use
      ;; `telega--help-win-param' as backref to the message
      (setq telega--help-win-param msg)
      (run-hooks 'telega-open-file-hook))

    (when (eq telega-open-file-function
              (default-value 'telega-open-file-function))
      (telega-help-message 'open-file
          "To open files in external apps see https://zevlg.github.io/telega.el/#opening-files-using-external-programs")
      )))

(defun telega-file-local-copy (file)
  "Same as `file-local-copy', but use `telega-temp-dir' for temp files.
If FILE is local, then return expanded FILE."
  ;; NOTE: Do logic only for remote files, to avoid calling local-copy
  ;; handlers.  See https://t.me/emacs_telega/26267
  (if (not (file-remote-p file))
      ;; NOTE: if using `telega-server' in docker, and file is not
      ;; accessible by docker container (i.e. outside ~/.telega dir),
      ;; then copy file into temporary directory accessible by docker
      (let ((absfile (expand-file-name file)))
        (if (and telega-use-docker
                 (not (string-prefix-p
                       (concat telega-database-dir "/") absfile)))
            (let ((tmpfile (telega-temp-name
                            (concat (file-name-sans-extension
                                     (file-name-nondirectory absfile))
                                    "-")
                            (file-name-extension absfile t))))
              (copy-file absfile tmpfile 'ok-if-already-exists 'keep-time)
              tmpfile)
          absfile))

    ;; NOTE: `tramp-compat-temporary-file-directory'<f> uses standard
    ;; value for `temporary-file-directory', so just binding it won't
    ;; work.
    (let ((tfd-value (get 'temporary-file-directory 'standard-value))
          (temporary-file-directory telega-temp-dir))
      (unwind-protect
          (progn
            (put 'temporary-file-directory 'standard-value
                 (list telega-temp-dir))
            (or (file-local-copy file) (expand-file-name file)))
        (put 'temporary-file-directory 'standard-value tfd-value)))))

(defun telega-color-name-as-hex-2digits (color)
  "Convert COLOR to #rrggbb form."
  (apply #'color-rgb-to-hex (append (color-name-to-rgb color) '(2))))

(defun telega-color-rainbow-identifier (identifier &optional background-mode)
  "Return color for IDENTIFIER in BACKGROUND-MODE.
BACKGROUND-MODE is one of `light' or `dark'."
  (cl-assert (memq background-mode '(light dark)))
  (let ((rainbow-identifiers-cie-l*a*b*-lightness
         (if (eq background-mode 'dark)
             (cdr telega-rainbow-lightness)
           (car telega-rainbow-lightness)))
        (rainbow-identifiers-cie-l*a*b*-saturation
         (if (eq background-mode 'dark)
             (cdr telega-rainbow-saturation)
           (car telega-rainbow-saturation))))
    (plist-get (car (rainbow-identifiers-cie-l*a*b*-choose-face
                     (rainbow-identifiers--hash-function identifier)))
               :foreground)))

(defun telega-color-name-from-rgb24 (rgb24)
  "Convert RGB24 int value to color name."
  (format "#%06x" (+ 8388608 rgb24)))

(defun telega-color-name-from-argb (argb)
  "Convert ARGB int value to color name.
Strips alpha component."
  (format "#%06x" (logand (+ 2147483648 argb) 16777215)))

(defun telega-clear-assigned-colors ()
  "Clears assigned colors for all chats and users.
Use this if you planning to change `telega-rainbow-function'."
  (interactive)
  (dolist (user (hash-table-values (alist-get 'user telega--info)) )
    (plist-put user :color nil))
  (dolist (chat telega--ordered-chats)
    (plist-put (plist-get chat :uaprops) :color nil)))

(defun telega-keys-description (command &optional keymap)
  "Return string describing binding of the COMMAND in the KEYMAP.
If no keys corresponds to COMMAND, then return \"M-x COMMAND
RET\" string."
  (propertize
   (let ((keys-description (mapconcat #'key-description
                                      (where-is-internal command keymap) ", ")))
     (if (string-empty-p keys-description)
         (format "M-x %S RET" command)
       keys-description))
   'face 'help-key-binding))

(defun telega-xdg-open (url)
  "Open URL using \"xdg-open\" utility."
  (unless (zerop (call-process "xdg-open" nil nil nil url))
    (error "Telega: xdg-open failed on %S" url)))

(defun telega-buffer--hack-win-point ()
  "Workaround Emacs bug.
Emacs does not respect buffer local nil value for
`switch-to-buffer-preserve-window-point', so we hack window point
in `(window-prev-buffers)' to achive behaviour for nil-valued
`switch-to-buffer-preserve-window-point'."
  (when (version< emacs-version "28.1.0")
    (cl-assert (not (get-buffer-window)))
    (when-let ((entry (assq (current-buffer) (window-prev-buffers))))
      (setf (nth 2 entry)
            (copy-marker (point) (marker-insertion-type (nth 2 entry)))))))

(defun telega-window-recenter (win &optional nlines from-point)
  "Set WIN's start point so there will be NLINES to FROM-POINT.
If FROM-POINT is nil, then current point is taken.
If NLINES is nil, then recenter."
  (cl-assert win)
  ;; NOTE: Do not use `set-window-start`, since it might move point
  ;; See https://github.com/zevlg/telega.el/issues/291
  (with-selected-window win
    (save-excursion
      (when from-point
        (goto-char from-point))
      (recenter nlines))))

(defun telega-directory-base-uri (directory)
  "Return DIRECTORY following possible symlink.
Used as for SVG's `:base-uri' functionality."
  (let ((telega-dir-link (file-symlink-p directory)))
    (if telega-dir-link
        (expand-file-name telega-dir-link
                          (file-name-directory directory))
      directory)))

(defun telega-create-image (file-or-data &optional type data-p &rest props)
  "Wrapper around `create-image' that takes into account `telega-use-images'.
Also enforces `:transform-smoothing' property to be non-nil."
  (when telega-use-images
    (apply #'create-image file-or-data type data-p
           (nconc props (list :transform-smoothing t)))))

(defun telega-etc-file-create-image (filename cwidth)
  "Create image from etc's FILENAME.
Width for the resulting image will be of CWIDTH chars."
  (telega-create-image (telega-etc-file filename) nil nil
                       :scale 1.0 :ascent 'center :mask 'heuristic
                       :width (telega-chars-xwidth cwidth)))

(defconst telega-symbol-animations
  '((dots "." ".." "...")
    (braille1 "⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
    (braille2 "⠟" "⠯" "⠷" "⠾" "⠽" "⠻")
    (braille3 "⡿" "⣟" "⣯" "⣷" "⣾" "⣽" "⣻" "⢿")
    (circle "◴" "◷" "◶" "◵")
    (triangles "▹▹▹▹▹" "▸▹▹▹▹" "▹▸▹▹▹" "▹▹▸▹▹" "▹▹▹▸▹" "▹▹▹▹▸")
    (equal "[    ]" "[=   ]" "[==  ]" "[=== ]" "[ ===]" "[  ==]"
           "[   =]" "[    ]" "[   =]" "[  ==]" "[ ===]" "[====]"
           "[=== ]" "[==  ]" "[=   ]")
    (black-dot "( ●    )" "(  ●   )" "(   ●  )" "(    ● )" "(     ●)"
               "(    ● )" "(   ●  )" "(  ●   )" "( ●    )" "(●     )")
    (clock "🕛" "🕐" "🕑" "🕒" "🕓" "🕔" "🕕" "🕖" "🕗" "🕘" "🕙" "🕚")
    (segments "▰▱▱▱▱▱▱" "▰▰▱▱▱▱▱" "▰▰▰▱▱▱▱" "▰▰▰▰▱▱▱" "▰▰▰▰▰▱▱"
              "▰▰▰▰▰▰▱" "▰▰▰▰▰▰▰")
    (large-dots "∙∙∙∙∙" "●∙∙∙∙" "∙●∙∙∙" "∙∙●∙∙" "∙∙∙●∙" "∙∙∙∙●")
    (globe "🌍" "🌎" "🌏")
    (audio "[▁▁▁▁▁▁▁▁▁▁▁]" "[▂▃▂▁▁▂▁▁▁▂▁]" "[▃▄▃▁▄▇▃▁▁▅▂]" "[▃▆▅▁▆█▃▁▂█▅]"
           "[▆█▃▄▄▆▇▃▄▆█]" "[▅▆▃▆▃▅▆▃▃█▆]" "[▄▅▂█▂▃▅▁▂▇▅]" "[▃▄▁▇▁▂▄▁▁▆▄]"
           "[▂▃▁▆▁▁▃▁▁▄▃]" "[▁▂▁▃▁▁▃▁▁▃▂]")
    (video "🞅" "🞆" "🞇" "🞈" "🞉")
    ))

(defun telega-symbol-animate (symbol &optional reverse-p)
  "Animate character CHAT, i.e. return next char to create animations."
  (let* ((anim (cl-find symbol telega-symbol-animations :test #'member))
         (anim-tail (cdr (member symbol (if reverse-p (reverse anim) anim)))))
    (or (car anim-tail) (cadr anim))))

(defmacro with-telega-symbol-animate (anim-name interval sym-bind exit-form
                                                &rest body)
  "Animate symbols denoted by ANIM-NAME while there is no pending input.
Animate while there is no pending input and EXIT-FORM evaluates to nil.
Animate with INTERVAL seconds (default=0.1)
Binds current symbol to SYM-BIND."
  (declare (indent 3))
  `(let ((,sym-bind (cadr (assq ,anim-name telega-symbol-animations)))
         (inhibit-quit t))              ;C-g as ordinary keypress
     (while (and (not (input-pending-p))
                 (not ,exit-form))
       (progn
         ,@body)
       (sit-for (or ,interval 0.1))
       (setq ,sym-bind (telega-symbol-animate ,sym-bind)))
     (when (input-pending-p) (read-key))))


;;; Docker support
(defvar telega-tdlib-min-version)
(defvar telega-tdlib-max-version)
(defun telega-docker--image-name ()
  "Return image name for suitable docker container."
  (if (and (equal telega-tdlib-min-version telega-tdlib-max-version)
           ;; NOTE: Release version ends with ".0", to avoid cases
           ;; like https://github.com/zevlg/telega.el/issues/370
           (stringp telega-tdlib-min-version)
           (string-match-p "\\.0$" telega-tdlib-min-version))
      (format "zevlg/telega-server:%s" telega-tdlib-min-version)
    "zevlg/telega-server:latest"))

(defvar telega-docker--user-id nil)
(defun telega-docker--user-id ()
  "Return UID:GID suitable for docker's -u."
  (unless telega-docker--user-id
    (setq telega-docker--user-id
          (format "%s:%s" (user-uid) (group-gid))))

  (unless (string-match-p "[0-9]+:[0-9]+" telega-docker--user-id)
    (user-error "telega: Can't get UID/GID, set `telega-docker--user-id' explicitly to \"<UID>:<GID>\""))
  telega-docker--user-id)

(defun telega-docker--container-id-filename ()
  "Return file to store docker container id to."
  (expand-file-name "docker.cid" telega-database-dir))

(defun telega-docker--container-id ()
  "Return running container id."
  (when (and telega-use-docker (telega-server-live-p))
    (unless telega-docker--container-id
      (setq telega-docker--container-id
            (string-trim
             (or (let ((cid-filename (telega-docker--container-id-filename)))
                   (when (file-exists-p cid-filename)
                     (with-temp-buffer
                       (insert-file-contents cid-filename)
                       (string-trim (buffer-string)))))
                 (shell-command-to-string
                  (format "%s ps -qf \"ancestor=%s\""
                          (if (stringp telega-use-docker)
                              telega-use-docker
                            "docker")
                          (telega-docker--image-name)))))))
    telega-docker--container-id))

(defun telega-docker--selinux-p ()
  "Return non-nil if running in the selinux environment."
  (when-let ((selinux-enabled-bin (executable-find "selinuxenabled")))
    (zerop (call-process selinux-enabled-bin))))

(defvar telega-docker--cidfile nil
  "Filename to write container id into using --cidfile docker flag.")
(defun telega-docker-run-cmd (cmd &rest volumes)
  "Dockerize command CMD."
  (declare (indent 1))
  (concat
   (if telega-docker-run-command
       (format-spec telega-docker-run-command
                    (format-spec-make ?u (telega-docker--user-id)
                                      ?w telega-database-dir
                                      ?i (telega-docker--image-name)))
     (let ((selinux-p (telega-docker--selinux-p)))
       (concat
        (if (stringp telega-use-docker)
            telega-use-docker
          "docker")
        (format " run %s --rm --privileged -i -v %s:%s%s"
                (or telega-docker-run-arguments "")
                telega-directory telega-directory
                (if selinux-p ":z" ""))
        (when telega-docker--cidfile
          (concat " --cidfile " telega-docker--cidfile))
        " -u " (telega-docker--user-id)
        ;; Connect container to host networking
        " --net=host"
        ;; Add host devices to container to allow voice/video
        ;; recording
        " --device /dev/snd:/dev/snd"
        " --device /dev/video0:/dev/video0"
        " --device /dev/video1:/dev/video1"

        ;; Export resources for pulseaudio to work
        ;; ref: https://stackoverflow.com/questions/28985714/run-apps-using-audio-in-a-docker-container
        (concat " -v /dev/shm:/dev/shm"
                " -v /etc/machine-id:/etc/machine-id"
                (when-let ((xdg-runtime-dir (getenv "XDG_RUNTIME_DIR")))
                  (concat (format " -v %s:%s" xdg-runtime-dir xdg-runtime-dir)
                          " -e XDG_RUNTIME_DIR"))
                " -v /var/lib/dbus"
                " -e XDG_RUNTIME_DIR"
                ;; TODO
                )
        ;; Export volumes and env vars need to run appindicator
        (when telega-appindicator-mode
          (concat " --security-opt apparmor=unconfined"
                  (format " -v /tmp/.X11-unix:/tmp/.X11-unix%s"
                          (if selinux-p ":z" ""))
                  (when-let ((xauthority (getenv "XAUTHORITY")))
                    (format " -v %s:%s%s" xauthority xauthority
                            (if selinux-p ":z" "")))
                  (when-let ((bus-addr (getenv "DBUS_SESSION_BUS_ADDRESS"))
                             (bus-path (nth 1 (split-string bus-addr "="))))
                    (format " -v %s:%s%s" bus-path bus-path
                            (if selinux-p ":z" "")))
                  " -e DISPLAY -e XAUTHORITY -e DBUS_SESSION_BUS_ADDRESS"))
        ;; Additional volumes
        (mapconcat (lambda (volume)
                     (format " -v %s:%s%s" volume volume
                             (if selinux-p ":z" "")))
                   (or volumes telega-docker-volumes) "")
        " " (telega-docker--image-name))))
   " " cmd))

(defun telega-docker-host-cmd-find (program)
  "Return non-nil if PROGRAM is available on host platform.
Return absolute path to PROGRAM."
  (let ((exec-path (cons telega-directory exec-path)))
    (executable-find program)))

(defun telega-docker-exec-cmd (cmd &optional try-host-p exec-flags no-error)
  "Format docker exec command to run CMD in the running docker.
If TRY-HOST-P is specified, return CMD if corresponding program is available.
Additial EXEC-FLAGS can be specifier to exec command, for example \"-i\".
If NO-ERROR is specified and corresponding command is not found, do
not signal an error and just return nil."
  (let* ((cmd-args (when try-host-p
                     (split-string cmd " ")))
         (program (when try-host-p
                    (car cmd-args)))
         program-bin)
    (cond ((and try-host-p
                (or (not telega-use-docker)
                    (not (eq telega-debug 'docker)))
                (setq program-bin (telega-docker-host-cmd-find program)))
           (mapconcat #'identity (cons program-bin (cdr cmd-args)) " "))

          ((and telega-use-docker (telega-server-live-p))
           (concat (if (stringp telega-use-docker)
                       telega-use-docker
                     "docker")
                   " exec"
                   " " exec-flags
                   ;; NOTE: `exec-flags' might specify its own "-u",
                   ;; for example to run commands under root with
                   ;; "-u 0" `exec-flags'
                   (unless (string-prefix-p "-u " (or exec-flags ""))
                     (concat " -u " (telega-docker--user-id)))
                   " " (telega-docker--container-id)
                   " " cmd))

          (no-error nil)
          (t (error "telega: Install `%s' or set `telega-use-docker' to non-nil"
                    program))
          )))


;; QR code image generation
(defun telega-qr-code--create-image (text size)
  "Generate image of SIZE with the QR code encoding TEXT."
  (let* ((png-filename (telega-temp-name "qrcode-" ".png"))
         (qrcode-cmd (telega-docker-exec-cmd
                      (format "qrencode -m 1 -s %d -t png -o %s '%s'"
                              (telega-chars-xwidth 1) png-filename text)
                      'try-host-first)))
    (telega-debug "RUN: %s" qrcode-cmd)
    (shell-command-to-string qrcode-cmd)
    (telega-create-image png-filename
                         (when (fboundp 'imagemagick-types) 'imagemagick) nil
                         :scale 1.0 :ascent 'center
                         :width size :height size)))

(defun telega-completing-read-emoji-status-duration (prompt)
  "Read duration for the custom emoji."
  (let* ((choices (mapcar (lambda (delay)
                            (cons (cond ((zerop delay) "Custom")
                                        (t (telega-duration-human-readable
                                            delay 1 " days" " hours" " minutes")))
                                  delay))
                          '(3600 7200 28800 172800 0)))
         (choice (funcall telega-completing-read-function
                          prompt (mapcar #'car choices) nil t))
         (duration (cdr (assoc choice choices))))
    (if (zerop duration)
        (- (telega-read-timestamp "Timestamp: ")
           (telega-time-seconds))
      duration)))

(defun telega-completing-read-language-code (prompt)
  "A two-letter ISO 639-1 language code."
  (let* ((candidates-alist
          (mapcar (lambda (spec)
                    (cons (concat (car spec) " (" (cdr spec) ")")
                          (cdr spec)))
                  telega-translate-languages-alist))
         (lang (funcall telega-completing-read-function prompt
                        (mapcar #'car candidates-alist) nil t)))
    (cdr (assoc lang candidates-alist))))

(defun telega--gen-ins-continuation-callback (show-loading-p
                                              &optional insert-func
                                              for-param)
  "Generate callback to continue insertion at the point of current buffer.
Passes all arguments directly to the INSERT-FUNC.
If FOR-PARAM is specified, then insert only if
`telega--help-win-param' is eq to FOR-PARAM."
  (declare (indent 1))
  (let ((marker (point-marker))
        (marker-len 0))
    (when show-loading-p
      (if (stringp show-loading-p)
          (telega-ins show-loading-p)
        (telega-ins-i18n "lng_profile_loading"))
      (setq marker-len (- (point) marker)))

    (lambda (&rest insert-args)
      (let ((marker-buf (marker-buffer marker)))
        (when (buffer-live-p marker-buf)
          (with-current-buffer marker-buf
            (when (or (null for-param)
                      (eq telega--help-win-param for-param))
                (let ((inhibit-read-only t))
                  (when show-loading-p
                    (delete-region marker (+ marker marker-len)))
                  (telega-save-excursion
                    (goto-char marker)
                    (apply insert-func insert-args))))))))))

(defun telega-translate-region (beg end &optional choose-language-p inplace-p)
  "Translate region using Telegram API.
Use `\\[universal-argument]' to specify language to translate to."
  (interactive "r\nP")
  (let ((saved-position (when inplace-p (copy-marker (point) t)))
        (text (buffer-substring-no-properties beg end))
        (lang-code (if (or choose-language-p
                           (not telega-translate-to-language-by-default))
                       (telega-completing-read-language-code "Translate to: ")
                     telega-translate-to-language-by-default)))
    (telega--translateText text lang-code
      :callback (lambda (reply)
                  (if inplace-p
                      (with-current-buffer (marker-buffer saved-position)
                        (goto-char end)
                        (insert (telega-tl-str reply :text))
                        (goto-char saved-position)
                        (delete-region beg end))
                    (with-help-window "*Telegram Translation*"
                      (insert (telega-tl-str reply :text))))
                  (message "telega: Translating...DONE")))
    (message "telega: Translating...")
    ))

(defun telega-translate-region-inplace (beg end &optional choose-language-p)
  "Translate region and replace region with the translated text."
  (interactive "r\nP")
  (telega-translate-region beg end choose-language-p 'inplace))

(provide 'telega-util)

;;; telega-util.el ends here

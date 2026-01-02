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

(require 'telega-core)
(require 'telega-customize)
(require 'telega-media)
(require 'telega-topic)
(require 'telega-folders)

(declare-function telega-root--buffer "telega-root")
(declare-function telega-chatbuf--name "telega-chat" (chat))
(declare-function telega-describe-chat "telega-chat" (chat))
(declare-function telega-folder-names "telega-folders")
(declare-function telega-browse-url "telega-webpage" (url &optional in-web-browser))

(declare-function telega-user-list "telega-user" (&optional temex))
(declare-function telega-user> "telega-user" (user1 user2))

(declare-function telega-match-p "telega-match-p" (object temex))

(defun telega-file-exists-p (filename)
  "Return non-nil if FILENAME exists.
Unlike `file-exists-p' this return nil for empty string FILENAME.
Also return `nil' if FILENAME is `nil'."
  (and filename
       (not (string-empty-p filename))
       (file-exists-p filename)))

(defun telega-plist-del (plist prop)
  "From PLIST destructively remove property PROP."
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
  (or (when (display-graphic-p (selected-frame))
        (selected-frame))
      (when-let ((root-frame
                  (window-frame
                   (get-buffer-window (telega-root--buffer)))))
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

(defun telega-chars-xwidth (n)
  "Return pixel width for N characters.
`telega--default-face' is used for size calculation."
  ;; NOTE: Same (* n (window-font-width (get-buffer-window nil (telega-x-frame))))
  ;; but without tweaking on window configuration, which breaks inserters
  (* n (if-let ((tframe (telega-x-frame)))
           (let* ((info (font-info
                         (face-font telega--default-face tframe) tframe))
                  (width (aref info 11)))
             (if (> width 0)
                 width
               (aref info 10)))
         (frame-char-width))))

(defun telega-chars-xheight (n)
  "Return pixel height for N characters.
`telega--default-face' is used for size calculation."
  (ceiling
   (* n (if-let ((tframe (telega-x-frame)))
            (aref (font-info (face-font telega--default-face tframe) tframe) 3)
          (frame-char-height)))))

(defun telega-chars-in-height (pixels)
  "Return how many lines needed to cover PIXELS height."
  (ceiling (/ pixels (float (telega-chars-xheight 1)))))

(defun telega-chars-in-width (pixels)
  "Return how many characters needed to cover PIXELS width."
  ;; NOTE: Must not return negative result
  (max 0 (ceiling (/ pixels (float (telega-chars-xwidth 1))))))

(defun telega-em-height-ratio ()
  "Return font character ratio.
Used to calculate correct image `:height' in `em' elements."
  (if-let ((tframe (telega-x-frame)))
      (let ((info (font-info (face-font telega--default-face tframe) tframe)))
        ;; height / pixel size
        (/ (float (aref info 3)) (aref info 2)))
    1))

(defun telega-em-width-ratio ()
  "Return font character ratio.
Used to calculate correct image `:width' in `em' elements."
  (if-let ((tframe (telega-x-frame)))
      (let ((info (font-info (face-font telega--default-face tframe) tframe)))
        ;; avg-width / pixel size
        (/ (float (aref info 11)) (aref info 2)))
    0.5))

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

(defun telega-window-string-width (str &optional from to)
  "Return correct width in chars.
This function is very slow comparing to `string-width', however
returns precise value."
  ;; Keeping a work buffer around is more efficient than creating a
  ;; new temporary buffer.
  (with-current-buffer (get-buffer-create " *telega-string-width*")
    ;; If `display-line-numbers-mode' is enabled in internal
    ;; buffers, it breaks width calculation, so disable it (bug#59311)
    (when (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode -1))
    (delete-region (point-min) (point-max))
    ;; Disable line-prefix and wrap-prefix, for the same reason.
    (setq line-prefix nil
          wrap-prefix nil)

    (telega-ins (if (or from to) (substring str from to) str))
    (save-window-excursion
      (set-window-dedicated-p nil nil)
      (set-window-buffer nil (current-buffer))
      (telega-window-current-column))))

(defun telega-current-column ()
  "Same as `current-column', but take into account `line-prefix' property.
Also take into account `wrap-prefix' property.
Also take into account `:align-to' display property, used by
`telega-ins--move-to-column'."
  (let* ((bol-point (line-beginning-position))
         (spoint (point))
         (dpoint spoint)
         (ccolumn nil))

    ;; Search the first display with `:align-to' before the point
    (while (and (not ccolumn)
                (> dpoint bol-point)
                (setq dpoint (previous-single-char-property-change
                              dpoint 'display nil bol-point)))
      (let ((disp (get-text-property dpoint 'display)))
        (when (and (listp disp) (> (length disp) 2)
                   (eq (nth 0 disp) 'space)
                   (eq (nth 1 disp) :align-to))
          (setq ccolumn (+ (let ((align-val (nth 2 disp)))
                             (if (listp align-val)
                                 ;; specified in pixels
                                 (/ (car align-val) (telega-chars-xwidth 1))
                               align-val))
                           (string-width (buffer-substring dpoint spoint)))))))

    (+ (or ccolumn (current-column))
       telega--column-offset
       (or (when-let ((lwprefix (or (get-text-property bol-point 'line-prefix)
                                    (get-text-property bol-point 'wrap-prefix))))
             (string-width lwprefix))
           0))))

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

(defun telega-svg-width (svg)
  (dom-attr svg 'width))

(defun telega-svg-height (svg)
  (dom-attr svg 'height))

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

(defun telega-svg-progress (svg progress &optional with-border-p)
  "Insert progress circle into SVG."
  (let* ((w (telega-svg-width svg))
         (h (telega-svg-height svg))
         (font-size (/ h 4)))
    (telega-svg-append-glow-filter svg "glow")
    (when with-border-p
      (svg-rectangle svg 0 0 w h
                     :stroke-color "currentColor"
                     :stroke-width 1
                     :fill-color "none"))
    (svg-text svg (format "%d%%" (round (* progress 100)))
              :font-size font-size
              :font-family "monospace"
              :fill-color "currentColor"
              :x "50%"
              :text-anchor "middle"
              :filter "url(#glow)"
              ;; Insane Y calculation
              :y (+ (/ font-size 3) (/ h 2))
              )
    svg))

(defun telega-svg-squircle (svg x y width height &rest args)
  "In SVG at X and Y positioon draw squircle of WIDTHxHEIGHT size.
X and Y denotes left up corner."
  (declare (indent 5))
  ;; Values are taken from
  ;; https://upload.wikimedia.org/wikipedia/commons/5/58/Squircle2.svg
  (let ((outline (concat "M304,592\n"
                         "C430.2,592,500.3563,592,546.1782,546.1782\n"
                         "C592,500.3563,592,430.2,592,304\n"
                         "C592,177.8,592,107.6437,546.1782,61.8218\n"
                         "C500.3563,16,430.2,16,304,16\n"
                         "C177.8,16,107.6437,16,61.8218,61.8218\n"
                         "C16,107.6437,16,177.8,16,304\n"
                         "C16,430.2,16,500.3563,61.8218,546.1782\n"
                         "C107.6437,592,177.8,592,304,592\n"
                         "Z")))
    (setq args (plist-put args :transform
                          (concat (plist-get args :transform)
                                  " "
                                  (format "translate(%f,%f)" x y)
                                  " "
                                  (format "scale(%f,%f)"
                                          (/ width 608.0) (/ height 608.0)))))
    (apply #'telega-svg-path svg outline args)))

(defun telega-svg-apply-outline (svg outline ratio &optional args)
  "To SVG add OUTLINE svg path scaling it by RATIO."
  (let ((scale-transform (format "scale(%s)" ratio)))
    (apply #'telega-svg-path svg outline
           (if-let ((args-transform (plist-get args :transform)))
               (plist-put (copy-sequence args) :transform
                          (concat args-transform " " scale-transform))
             (nconc (list :transform scale-transform)
                    args)))))

(defun telega-svg-telega-logo (svg width &rest args)
  "Draw telega triangle of WIDTH."
  (declare (indent 2))
  (let ((ratio (/ width 32.0))
        (outline (concat "M0,10.1891\n"
                         "l7.9819,5.5418\n"
                         "c0.8853,-0.322 1.8202,-0.6638 2.599,-0.9418"
                         " 1.9609,-0.7 7.0539,-3.4182 7.0539, -3.4182"
                         " -2.5145,2.2595 -4.6401,4.5613 -6.55, 6.8691\n"
                         "L17.5694,27\n"
                         "c0.2653,-0.9309 0.5279,-1.8618 0.9135,-2.9018\n"
                         "C20.4518,18.4196 32,0 32,0"
                         " 24.4744,2.555 10.7087,7.5896 7.8333,8.5782"
                         " 5.5816,9.3523 2.1946,10.5884 0,10.1892\n"
                         "z")))
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
        (outline (concat "M7.9673,1.7397\n"
                         "c-0.8215,0.005 -1.3307,3.6214 -1.9924,4.1084"
                         " -0.6618,0.487 -4.266,-0.102 -4.5151,0.681"
                         " -0.1812,0.5691 2.5226,2.3763 2.5226,2.3763\n"
                         "s5.0903,-1.0188 5.181,-0.8892\n"
                         "c0.1049,0.15 -1.9601,1.3199 -4.5458,2.74"
                         " -0.3426,1.263 -1.1017,3.0947 -0.6124,3.4457"
                         " 0.6676,0.4789 3.2046,-2.1477 4.0261,-2.1524"
                         " 0.8216,-0.005 3.3918,2.5891 4.0535,2.1022"
                         " 0.6618,-0.487 -1.0488,-3.714 -0.7997,-4.4969"
                         " 0.2492,-0.7829 3.5078,-2.4236 3.2492,-3.2035"
                         " -0.2586,-0.7798 -3.852,-0.1518 -4.5196,-0.6306"
                         " -0.6676,-0.4789 -1.2258,-4.0856 -2.0474,-4.081\n"
                         "z")))
    (telega-svg-apply-outline svg outline ratio args)))

(defun telega-svg-forum-topic-icon (svg width &rest args)
  "Draw icon for a forum topic."
  (declare (indent 2))
  (let ((ratio (/ width 32.0))
        (outline (concat "M16.013,3.6908\n"
                         "C8.426,3.1848 0.3523,10.7055 3.307,18.4956\n"
                         "c0.6111,2.1934 2.5766,3.9355 3.9238,5.39"
                         " -0.8542,1.6842 -2.045,3.1894 -3.4501,4.4478"
                         " 3.0964,0.006 6.2267,-0.6528 8.953,-2.1524"
                         " 7.2454,1.9045 16.545,-2.9693 16.584,-11.0625"
                         " 0.028,-6.8663 -6.9149,-11.585 -13.3046,-11.4277\n"
                         "z")))
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
  (declare (indent 1))
  ;; NOTE: work around problem displaying unicode characters in some
  ;; librsvg versions (in my case 2.40.13).  Encoded (in &#xxxx format)
  ;; text is only displayed correctly if <xml ..?> node is specified
  (let ((svg-data (with-temp-buffer
                    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
                    (svg-print svg)
                    (buffer-string))))
    ;; NOTE: Do not check `svg' availability, so it will work when
    ;; `rsvg' is not compliled in and telega images is disabled.
    ;; See https://github.com/zevlg/telega.el/issues/219
    (nconc (list 'image :type 'svg :data svg-data)
           (unless (plist-member props :scale)
             (list :scale
                   (image-compute-scaling-factor image-scaling-factor)))
           props)))

(defun telega-svg-fit-into (width height fit-width fit-height)
  "Fit rectangle of WIDTHxHEIGHT size into FIT-WIDTHxFIT-HEIGHT rect.
Do touch outsize scaling.
Return resulting x,y,width,height."
  ;; NOTE: workaround for zero values for width/height.  In some cases
  ;; width/height might be 0 if photo is sent from tty Emacs instance
  ;; See https://github.com/zevlg/telega.el/issues/467
  (when (telega-zerop width)
    (setq width 100))
  (when (telega-zerop height)
    (setq height 100))

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
         (dashes-text (propertize
                       (if (> ndashes 0) (make-string ndashes ?\-) "·")
                       'face (or face 'bold)))
         (telega-text
          (concat dashes-text
                  (make-string (- cwidth (length dashes-text)) ?\s)))
         (xheight (telega-chars-xheight 1))
         (aw-chars (string-width telega-text))
         (xwidth (telega-chars-xwidth aw-chars))
         (stroke-xwidth (/ xheight 5))
         (dashes-xwidth (* (- (telega-chars-xwidth cwidth) (* 2 stroke-xwidth))
                           (/ percents 100.0)))
         (svg (telega-svg-create xwidth xheight)))
    (svg-line svg stroke-xwidth (/ xheight 2)
              (+ stroke-xwidth dashes-xwidth) (/ xheight 2)
              :stroke-color "currentColor"
              :stroke-width stroke-xwidth
              :stroke-linecap "round")
    (telega-svg-image svg
      :scale 1
      :width (telega-cw-width aw-chars)
      :max-height (telega-ch-height 1)
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

    (telega-svg-image svg
      :scale 1.0
      ;; TODO: move to `em' width/height
      :width xw
      :height xh
      :ascent 'center)))

(defconst telega-spoiler-turbulence-attrs
  '((baseFrequency . "0.1 0.1") (numOctaves . "2"))
  "Attributes to the \"feTurbulence\" node.")
(defconst telega-spoiler-displacement-attrs
  '((scale . "80"))
  "Attributes to the \"feDisplacementMap\" node.")

(defun telega-svg-append-spoiler-node (svg node-id)
  "Append spoiler noise node with NODE-ID into SVG."
  (svg--append
   svg
   (dom-node 'filter
             `((id . ,node-id))
             (dom-node 'feTurbulence
                       `((type . "turbulence")
                         (result . "NOISE")
                         ,@telega-spoiler-turbulence-attrs))
             (dom-node 'feDisplacementMap
                       `((in . "SourceGraphic")
                         (in2 . "NOISE")
                         (xChannelSelector . "R")
                         (yChannelSelector . "G")
                         ,@telega-spoiler-displacement-attrs))
             )))

(defun telega-spoiler-create-svg (minithumb &optional width height limits video-p)
  "Create svg image for MINITHUMB that has spoiler."
  (let* ((width (or width (plist-get minithumb :width)))
         (height (or height (plist-get minithumb :height)))
         (cheight (telega-media--cheight-for-limits width height limits))
         (svg (telega-svg-create width height)))
    (telega-svg-append-spoiler-node svg "noise")
    (svg-embed svg (base64-decode-string (plist-get minithumb :data))
               "image/jpeg" t
               :x 0 :y 0 :width width :height height
               :filter "url(#noise)")
    (when video-p
      (telega-svg-white-play-triangle-in-circle svg))
    (telega-svg-image svg
      :scale 1.0
      :height (telega-ch-height cheight)
      :telega-nslices cheight
      :ascent 'center)))

(defun telega-svg-append-glow-filter (svg node-id &optional std-deviation)
  "Create glow filter."
  (svg--append
   svg
   (dom-node 'filter
             `((id . ,node-id))
             (dom-node 'feGaussianBlur
                       `((stdDeviation . ,(or std-deviation "3"))
                         (result . "coloredBlur")))
             (dom-node 'feMerge nil
                       (dom-node 'feMergeNode
                                 `((in . "coloredBlur")))
                       (dom-node 'feMergeNode
                                 `((in . "SourceGraphic"))))
             )))

(defun telega-svg-embed-image-fitting (svg filename data-p img-width img-height
                                           &rest embed-attrs)
  "Create svg image by embedding FILENAME image into SVG fitting into its size."
  (seq-let (x-fit y-fit w-fit h-fit)
      (telega-svg-fit-into img-width img-height
                           (telega-svg-width svg) (telega-svg-height svg))
    (apply #'telega-svg-embed
           svg (if data-p
                   filename
                 (list (file-name-nondirectory filename)
                       (file-name-directory filename)))
           (format "image/%s"
                   (if data-p
                       "jpeg"
                     (telega-image-supported-file-p filename t)))
           data-p :x x-fit :y y-fit :width w-fit :height h-fit
           embed-attrs)))

(defun telega-svg-red-play-triangle (svg &optional fill opacity)
  "Draw play triangle at the SVG center."
  (let* ((svg-w (telega-svg-width svg))
         (svg-h (telega-svg-height svg))
         (play-size (/ svg-w 3)))
    (svg-polygon svg (list (cons (/ (- svg-w play-size) 2)
                                 (/ (- svg-h play-size) 2))
                           (cons (/ (- svg-w play-size) 2)
                                 (/ (+ svg-h play-size) 2))
                           (cons (/ (+ svg-w play-size) 2)
                                 (/ svg-h 2)))
                 :fill (or fill "red")
                 :opacity (or opacity "0.75"))))

(defun telega-svg-white-play-triangle-in-circle (svg)
  "Draw white play triangle in the black circle at the SVG center."
  (let* ((width (telega-svg-width svg))
         (height (telega-svg-height svg))
         (play-size (/ height 8.0))
         (xoff (/ play-size 8.0)))
    (svg-circle svg (/ width 2) (/ height 2) play-size
                :fill "black"
                :opacity "0.65")
    (svg-polygon svg (list (cons (+ xoff (/ (- width play-size) 2))
                                 (/ (- height play-size) 2))
                           (cons (+ xoff (/ (- width play-size) 2))
                                 (/ (+ height play-size) 2))
                           (cons (+ xoff (/ (+ width play-size) 2))
                                 (/ height 2)))
                 :fill "white"
                 :opacity "0.65")
    svg))

(defun telega-photo-preview--create-svg-one-line (filename data-p width height
                                                           &optional video-p)
  "Function to create svg image for photo preview."
  (let* ((svg-w (telega-chars-xwidth 2))
         (svg-h (min svg-w (telega-chars-xheight 1)))
         (svg (telega-svg-create svg-w svg-h))
         (pclip (telega-svg-clip-path svg "pclip"))
         (margin 1))
    (telega-svg-round-square pclip margin margin
                             (- svg-w (* 2 margin)) (- svg-h (* 2 margin))
                             (/ svg-w 6))
    (telega-svg-embed-image-fitting svg filename data-p width height
                                    :clip-path "url(#pclip)")
    (when video-p
      (telega-svg-red-play-triangle svg))

    (telega-svg-image svg
      :scale 1.0
      :max-height (telega-ch-height 1)
      :width (telega-cw-width 2)
      :telega-text "[]"
      :ascent 'center
      :mask 'heuristic
      :base-uri (if data-p "" filename))))

(defun telega-video-preview--create-svg-one-line (filename data-p width height)
  "Function to create svg image for video preview."
  (telega-photo-preview--create-svg-one-line filename data-p width height t))

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
  (cond ((not (integerp meters))
         "unknown")
        ((> meters 10000)
         (telega-i18n "lng_action_proximity_distance_km"
           :count (/ meters 1000)))
        ((>= meters 1000)
         (telega-i18n "lng_action_proximity_distance_km"
           :count (telega-float-clamp (/ meters 1000.0) 1)))
        (t
         (telega-i18n "lng_action_proximity_distance_m"
           :count meters))))

(defun telega-number-human-readable (num &optional fmt)
  "Convert METERS to human readable string.
By default \"%.1f\" FMT format string is used."
  (unless fmt (setq fmt "%.1f"))
  (cond ((and telega-use-short-numbers (>= num 1000000))
         (concat (format fmt (/ num 1000000.0)) "M"))
        ((and telega-use-short-numbers (>= num 1000))
         (concat (format fmt (/ num 1000.0)) "k"))
        (t
         (number-to-string num))))

(defun telega-duration-human-readable (seconds &optional n long-p)
  "Convert SECONDS to human readable string.
If N is given, then use only N significant components.
For example if duration is 4h:20m:3s then with N=2 4H:20m will be returned.
By default N=3 (all components).
N can't be 0.
If LONG-P is specified, then use long form."
  (cl-assert (or (null n) (> n 0)))
  ;; NOTE: force seconds to be a number, see
  ;; https://t.me/emacs_ru/283567?single
  (setq seconds (round seconds))
  (let ((ncomponents (or n 3))
        (intervals
         `((,(* 365 24 60 60) . ,(concat "lng_years" (unless long-p "_tiny")))
           (,(round (* 30.5 24 60 60)) . ,(concat "lng_months" (unless long-p "_tiny")))
           (,(* 7 24 60 60) . ,(concat "lng_weeks" (unless long-p "_tiny")))
           (,(* 24 60 60) . ,(concat "lng_days" (unless long-p "_tiny")))
           (,(* 60 60) . ,(concat "lng_hours" (unless long-p "_tiny")))
           (60 . ,(concat "lng_minutes" (unless long-p "_tiny")))
           (1 . ,(concat "lng_seconds" (unless long-p "_tiny")))))
        comps)

    (while (and (> ncomponents 0) intervals)
      (let* ((ival (car intervals))
             (ival-seconds (car ival)))
        (when (or (>= seconds ival-seconds)
                  ;; NOTE: special case for 0 SECONDS passed to
                  ;; `telega-duration-human-readable'
                  (and (null comps) (= seconds 0) (= ival-seconds 1)))
          ;; NOTE: "lng_hours_tiny" for ru, has leading newline,
          ;; workaround this problem, looks like a typo on Telegram
          ;; translation platform
          (setq comps (nconc comps (list (telega-strip-newlines
                                          (telega-i18n (cdr ival)
                                            :count (/ seconds ival-seconds)))))
                seconds (% seconds ival-seconds)
                ncomponents (1- ncomponents)))
        (setq intervals (cdr intervals))))

    (mapconcat #'identity comps ":")))

(defun telega-time-ago-human-readable (timestamp-ago)
  (cond ((< timestamp-ago 60)
         (telega-i18n "lng_mediaview_just_now"))
        ((< timestamp-ago 3600)
         (telega-i18n "lng_mediaview_minutes_ago"
           :count (/ timestamp-ago 60)))
        ((< timestamp-ago 86400)
         (telega-i18n "lng_mediaview_hours_ago"
           :count (telega-float-clamp (/ timestamp-ago 3600.0) 0)))
        (t
         (telega-i18n "lng_new_contact_updated_days"
           :count (telega-float-clamp (/ timestamp-ago 86400.0) 0)))))

(defun telega-link-props (link-type link-to &rest props)
  "Generate props for link button openable with `telega-link--button-action'."
  (cl-assert (memq link-type '(url file username user sender hashtag tdlib-link)))

  (nconc (list 'action 'telega-link--button-action
               :telega-link (cons link-type link-to))
         props))

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
      (tdlib-link
       (telega-tme-open-tdlib-link (cdr link)))
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

(defun telega--change-text-property (start end prop value how &optional object)
  "Change a property of the text from START to END.
Arguments PROP and VALUE specify the property and value to add/remove
to the value already in place.  The resulting property values are
always lists.
HOW specifies how list is changed, one of: `prepend', `append' or `remove'."
  (cl-assert (memq how '(prepend append remove)))
  (let ((val (if (listp value) value (list value)))
        next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
            prev (get-text-property start prop object))
      (let* ((list-prev (if (listp prev) prev (list prev)))
             (new-value (cl-ecase how
                          (prepend (append val list-prev))
                          (append (append list-prev val))
                          (remove (delete val list-prev)))))
        (put-text-property start next prop new-value object))
      (setq start next))))

(defun telega--text-entity-at-newline-p (ent text)
  "Return non-nil if textEntity ENT for TEXT starts at newline."
  (let ((beg (plist-get ent :offset)))
    (or (zerop beg) (= ?\n (aref text (1- beg))))))

(defun telega--text-entity-apply (ent &optional object)
  "Apply textEntity ENT to OBJECT.
Optional OBJECT could be a buffer (or nil, which means the current
buffer) or a string."
  (let* ((ent-type (plist-get ent :type))
         (ent-type-tl-type (telega--tl-type ent-type))
         (beg (plist-get ent :offset))
         (end (+ (plist-get ent :offset) (plist-get ent :length)))
         (ent-text (if object
                       (substring object beg end)
                     (buffer-substring beg end))))
    (add-text-properties beg end '(rear-nonsticky t front-sticky t) object)
    (telega--change-text-property beg end :tl-entities (list ent) 'append object)
    (put-text-property beg end :tl-entity-type ent-type object)

    ;; NOTE: Include newline for blockquotes even if newline is
    ;; outside of the blockquote, making
    ;; `telega-entity-type-blockquote' face to extend to the end of
    ;; line
    (when (and (stringp object)
               (memq ent-type-tl-type '(textEntityTypeBlockQuote
                                        textEntityTypeExpandableBlockQuote))
               (< end (length object))
               (= (aref object end) ?\n))
      (setq end (1+ end)))

    (cl-case ent-type-tl-type
      (textEntityTypeMention
       (add-text-properties
        beg end (telega-link-props 'username ent-text) object)
       (add-face-text-property
        beg end
        (if (and (plist-get telega-msg--current :contains_unread_mention)
                 (telega-user-match-p (telega-user-me)
                   (list 'username
                         ;; Strip leading "@" from ENT-TEXT
                         (concat "^" (substring ent-text 1) "$"))))
            '(telega-entity-type-mention bold)
          'telega-entity-type-mention)
       nil object))
      (textEntityTypeMentionName
       (add-text-properties
        beg end (telega-link-props 'user (plist-get ent-type :user_id)) object)
       (add-face-text-property
        beg end
        (if (and (plist-get telega-msg--current
                            :contains_unread_mention)
                 (eq (plist-get ent-type :user_id)
                     telega--me-id))
            '(telega-entity-type-mention bold)
          'telega-entity-type-mention)
        nil object))
      (textEntityTypeHashtag
       (add-text-properties
        beg end (telega-link-props 'hashtag ent-text) object)
       (add-face-text-property beg end 'telega-link nil object))
      (textEntityTypeBold
       (add-face-text-property beg end 'telega-entity-type-bold nil object))
      (textEntityTypeItalic
       (add-face-text-property beg end 'telega-entity-type-italic nil object))
      (textEntityTypeUnderline
       (add-face-text-property beg end 'telega-entity-type-underline nil object))
      (textEntityTypeStrikethrough
       (add-face-text-property
        beg end 'telega-entity-type-strikethrough nil object))
      (textEntityTypeCode
       (add-face-text-property beg end 'telega-entity-type-code nil object))
      (textEntityTypePre
       (add-face-text-property beg end 'telega-entity-type-pre nil object))
      (textEntityTypeUrl
       (add-text-properties
        beg end (telega-link-props 'url ent-text) object)
       (add-face-text-property beg end 'telega-entity-type-texturl nil object)
       (unless (telega--inhibit-telega-display-p 'telega-core)
         ;; - Unhexify url, using `telega-display' property to be
         ;;   substituted at `telega--desurrogate-apply' time
         ;; - Convert "xn--" domains to non-ascii version
         (put-text-property
          beg end 'telega-display (telega-puny-decode-url
                                   (decode-coding-string
                                    (url-unhex-string ent-text) 'utf-8))
          object)))
      (textEntityTypeTextUrl
       (add-text-properties
        beg end
        (telega-link-props 'url (telega-tl-str ent-type :url)
                           'help-echo (telega-tl-str ent-type :url))
        object)
       (add-face-text-property beg end 'telega-entity-type-texturl nil object))
      (textEntityTypeBotCommand
       (add-face-text-property beg end 'telega-entity-type-botcommand nil object))
      (textEntityTypeMediaTimestamp
       (add-text-properties
        beg end
        (list 'action (lambda (button)
                       (telega-msg-open-media-timestamp
                        (telega-msg-at button)
                        (plist-get ent-type :media_timestamp))))
        object)
       (add-face-text-property beg end 'telega-link nil object))
      (textEntityTypeCustomEmoji
       (when telega-use-images
         (when-let ((sticker (telega-custom-emoji-get
                              (plist-get ent-type :custom_emoji_id))))
           ;; NOTE: We use `copy-sequence' to make `display' property
           ;; differ, because consecutive chars has `eq' `display'
           ;; property it is displayed as single unit (ref: 40.16.1
           ;; Display Specs That Replace The Text)
           ;;
           ;; However, copying image will breake
           ;; `telega-sticker--animate' functionality, because
           ;; `telega-media--image-update' updates image inplace
           ;; (i.e. its cdr).  So we use `(magin nil)' hack to make
           ;; display property non-eq
           (put-text-property
            beg end 'display (list '(margin nil)
                                   (telega-sticker--image sticker))
            object)
           )))
      (textEntityTypePreCode
       (if (telega--inhibit-telega-display-p 'telega-core)
           (add-face-text-property
            beg end 'telega-entity-type-code 'append object)

         (add-text-properties
          beg end
          (list 'telega-display
                (let* ((telega-palette-context 'precode)
                       (palette (if telega-msg--current
                                    (telega-msg-sender-palette
                                     (telega-msg-sender telega-msg--current))
                                  ;; id=5 is "blue" palette
                                  (telega-palette-by-color-id 5))))
                  (telega-ins--as-string
                   (unless (telega--text-entity-at-newline-p ent object)
                     (telega-ins "\n"))
                   (telega-ins--with-outline-palette palette
                     (telega-ins--with-face (list 'bold 'underline
                                                  (assq :foreground palette))
                       (telega-ins (telega-symbol 'codeblock))
                       (telega-ins-prefix " "
                         (telega-ins
                          (capitalize (telega-tl-str ent-type :language)))))
                     (telega-ins "\n")
                     (telega-ins--with-face 'telega-entity-type-code
                       (telega-ins (telega-strip-newlines
                                    (telega--desurrogate-apply ent-text))))
                     (telega-ins "\n"))))
                'telega-display-by 'telega-core)
          object)))
      (textEntityTypeSpoiler
       (add-face-text-property
        beg end 'telega-entity-type-spoiler 'append object)

       (if (null object)
           ;; NOTE: For chatbuf's input, when spoiler formatting is
           ;; applied
           (add-face-text-property
            beg end (list :foreground (face-foreground 'default)
                          :background (face-foreground 'default))
            nil object)

         (when telega-msg--current
           (add-text-properties
            beg end (list :action #'telega-msg-text-spoiler-toggle) object))
         (unless (plist-get telega-msg--current :telega-text-spoiler-removed)
           (add-text-properties
            beg end (list 'telega-display
                          (with-temp-buffer
                            (insert ent-text)
                            (translate-region
                             (point-min) (point-max)
                             telega-spoiler-translation-table)
                            (buffer-string))
                          'telega-display-by 'telega-core)
            object))))
      (textEntityTypeBlockQuote
       (if (and nil (telega--inhibit-telega-display-p 'telega-core))
           (add-face-text-property
            beg end 'telega-entity-type-blockquote 'append object)

         (let* ((telega-palette-context 'blockquote)
                (palette (when telega-msg--current
                           (telega-msg-sender-palette
                            (telega-msg-sender telega-msg--current)))))
           (add-face-text-property
            beg end
            (telega-face-with-palette 'telega-entity-type-blockquote
                palette :background)
            'append object)
           (let ((lw-prefix (propertize
                             (telega-symbol 'vbar-left)
                             'face (append (assq :foreground palette)
                                           (assq :background palette)))))
             (add-text-properties
              beg end
              (list 'line-prefix lw-prefix
                    'wrap-prefix lw-prefix)
              object)))
         ))
      (textEntityTypeExpandableBlockQuote
       (if (telega--inhibit-telega-display-p 'telega-core)
           (add-face-text-property
            beg end 'telega-entity-type-blockquote 'append object)

         (let* ((telega-palette-context 'blockquote)
                (palette (when telega-msg--current
                           (telega-msg-sender-palette
                            (telega-msg-sender telega-msg--current))))
                (expanded-p
                 (plist-get ent :telega-blockquote-expanded)))
           (add-face-text-property
            beg end
            (telega-face-with-palette 'telega-entity-type-blockquote
                palette :background)
            'append object)
           (let ((lw-prefix (propertize
                             (telega-symbol 'vbar-left)
                             'face (append (assq :foreground palette)
                                           (assq :background palette)))))
             (add-text-properties
              beg end
              (list 'line-prefix lw-prefix
                    'wrap-prefix lw-prefix)
              object))
           (put-text-property
            beg end
            :action #'telega-msg-blockquote-expand-toggle
            object)

           ;; Collapse at point configurable by
           ;; `telega-expandable-blockquote-limit'
           (let ((collapse-pos
                  (when (and (not expanded-p) (stringp object))
                    (cond ((consp telega-expandable-blockquote-limit)
                           (let ((lbeg (car telega-expandable-blockquote-limit))
                                 (lend (cdr telega-expandable-blockquote-limit)))
                             (cond ((>= (+ beg lbeg) end)
                                    nil)
                                   ((and (string-match "\n" object (+ beg lbeg))
                                         (< (match-beginning 0) (+ beg lend)))
                                    (match-beginning 0))
                                   (t
                                    (+ beg lend)))))
                          ((integerp telega-expandable-blockquote-limit)
                           (+ telega-expandable-blockquote-limit beg))))))
             (when (and collapse-pos (< collapse-pos (- end 10)))
               ;; NOTE: Do not put eliding after spaces, so filling
               ;; won't put eliding on a separate line
               (while (and (> collapse-pos beg)
                           (= (char-syntax (aref object (1- collapse-pos))) ?\s))
                 (cl-decf collapse-pos))

               (put-text-property
                collapse-pos end
                'display (propertize (concat (telega-symbol 'eliding)
                                             (telega-symbol 'expand-details)
                                             (when (= (aref object (1- end)) ?\n)
                                               "\n"))
                                     'face 'telega-shadow)
                object)))
           )))
      )))

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
       (format "[%s](%s)" text (telega-tl-str ent-type :url)))
      (textEntityTypeCustomEmoji
       (let ((new-text (substring text)))
         (telega--text-entity-apply
          (list :offset 0 :length (length new-text) :type ent-type)
          new-text)
         new-text))
      (textEntityTypeBlockQuote
       (concat "> " text))
      (t text))))

(defun telega--entity-to-org (entity-text)
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
       (format "[[%s][%s]]" (telega-tl-str ent-type :url) text))
      (textEntityTypeCustomEmoji
       (let ((new-text (substring text)))
         (telega--text-entity-apply
          (list :offset 0 :length (length new-text) :type ent-type)
          new-text)
         new-text))
      (textEntityTypeBlockQuote
       ;; See `org-element-quote-block-interpreter'
       (format "#+begin_quote\n%s#+end_quote" text))
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
                                        'face 'telega-shadow)
                          "")
           'rear-nonsticky t
           :telega-markup-start markup-func
           :telega-markup-args markup-args)
          str
          (propertize
           "❱" 'display (if markup-name
                            (propertize (concat "</" markup-name ">")
                                        'face 'telega-shadow)
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
    (let ((text (match-string 3 str))
          (url (or (match-string 2 str) (match-string 0 str))))
      (cl-assert url)
      (cond ((null text)
             (telega-fmt-text url '(:@type "textEntityTypeUrl")))
            ((string-match (rx "tg://user?id=" (group (+ digit))) url)
             (telega-fmt-text text (list :@type "textEntityTypeMentionName"
                                         :user_id (string-to-number
                                                   (match-string 1 url)))))
            (t
             (telega-fmt-text text (list :@type "textEntityTypeTextUrl"
                                         :url url)))))))

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
           (save-excursion
             (while (re-search-forward org-link-any-re nil t)
               (add-text-properties (match-beginning 0) (match-end 0)
                                    '(face (telega-entity-type-texturl)
                                           org-emphasis t
                                           org-link t))))
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
             (fmt-str (cond ((get-text-property 0 'org-link str1)
                             (setq seen-org-emphasis t)
                             (telega-markup-org--link-fmt str1))
                            ((get-text-property 0 'org-emphasis str1)
                             (setq seen-org-emphasis t)
                             (telega-markup-org--emphasis-fmt str1))
                            (seen-org-emphasis
                             ;; Trailing string
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
  "Format string STR to formattedText as is, without applying any markup."
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

(defun telega-markup-markdown2-fmt (str-or-fmt-txt)
  (let ((fmt-text (telega--parseMarkdown
                   (if (stringp str-or-fmt-txt)
                       (telega-fmt-text str-or-fmt-txt)
                     str-or-fmt-txt)))
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
(put 'telega-markup-markdown2-fmt :telega-accepts-fmt-text t)

(defun telega-string-split-by-tl-entity-type (text default-markup-func)
  "Split TEXT by `:tl-entity-type'.
Return list of list where first element is markup function, second is
substring and rest are additional arguments to markup function."
  (let ((result nil)
        (tomarkup-ss nil))
    (seq-doseq (ss (telega--split-by-text-prop text :tl-entity-type))
      (let ((ent-type (get-text-property 0 :tl-entity-type ss)))
        ;; NOTE: if markup is applied, then ignore all tl entity types
        ;; except for custom emojis, mentions and explicit formatting
        ;; done with `C-c C-e', so you can cut&paste text already
        ;; having entity-type and apply new markup to it
        (if (or (eq default-markup-func #'telega-markup-as-is-fmt)
                (and ent-type
                     (memq (telega--tl-type ent-type)
                           '(textEntityTypeCustomEmoji
                             textEntityTypeMentionName)))
                (get-text-property 0 :tl-entity-explicit ss))
            (progn
              (when tomarkup-ss
                (push (list default-markup-func tomarkup-ss) result)
                (setq tomarkup-ss nil))
              (push (list #'telega-fmt-text ss ent-type) result))

          (setq tomarkup-ss (concat tomarkup-ss ss)))))

    (when tomarkup-ss
      (push (list default-markup-func tomarkup-ss) result))
    (nreverse result)))

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
  (if (get default-markup-func :telega-accepts-fmt-text)
      (funcall default-markup-func (telega-string-fmt-text text))

    (apply #'telega-fmt-text-concat
           (mapcar (apply-partially #'apply #'funcall)
                   (telega-string-split-by-markup text default-markup-func)))))

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
  (let ((text (copy-sequence (plist-get fmt-text :text)))
        complex-ents)
    ;; NOTE: First we apply emphasis entities, then we apply
    ;; formatting for complex entities, such as block quotes, because
    ;; it might override `display' property
    (seq-doseq (ent (plist-get fmt-text :entities))
      (if (telega-match-p (plist-get ent :type)
            '(tl-type textEntityTypeBlockQuote
                      textEntityTypeExpandableBlockQuote))
          (setq complex-ents (cons ent complex-ents))
        (telega--text-entity-apply ent text)))

    (seq-doseq (ent (nreverse complex-ents))
      (telega--text-entity-apply ent text))
    text))

(defun telega--split-by-text-prop (string prop &optional value-predicate)
  "Split STRING by property PROP changes.
If VALUE-PREDICATE is specified, then split in the places where
VALUE-PREDICATE returns non-nil for the PROP value."
  (declare (indent 2))
  (let ((finish (length string))
        (start 0) (pos 0) end result
        prev-value curr-value)
    (while (and (> finish pos)
                (setq end (next-single-char-property-change pos prop string)))
      (setq pos end)

      (when (or (null value-predicate)
                (funcall value-predicate
                         (setq curr-value (get-text-property pos prop string)))
                prev-value
                (= pos finish))
        (setq prev-value curr-value)
        (push (substring string start end) result)
        (setq start end)))
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

(defvar telega-chat-read-history nil)
(defun telega-completing-read-chat (prompt &optional chats sort-criteria)
  "Read chat by title.
CHATS - list of the chats to select from.  By default all chats are used.
SORT-CRITERIA is a chat sort criteria to apply. (NOT YET)"
  (telega-completing-read-msg-sender
   prompt
   (telega-sort-chats
    (or sort-criteria telega-chat-completing-sort-criteria)
    (telega-filter-chats (or chats (telega-chats-list))
      '(or is-known has-chatbuf)))
   'telega-chat-read-history))

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
                                                       "]"))
                                                    ": ")
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
        (telega-filter-chats (or chats-list (telega-chats-list))
          '(or is-known has-chatbuf)))
  (telega-gen-completing-read-list prompt chats-list #'telega-chatbuf--name
                                   #'telega-completing-read-chat sort-criteria))

(defvar telega-user-read-history nil)
(defun telega-completing-read-user (prompt &optional users)
  "Read user by his name from USERS list."
  (declare (indent 1))
  (telega-completing-read-msg-sender
   prompt
   (or users
       (sort (telega-user-list telega-user-completing-temex)
             #'telega-user>))
   'telega-user-read-history))

(defun telega-completing-read-user-list (prompt &optional users)
  "Read multiple users from USERS."
  (declare (indent 1))
  (unless users
    (setq users (sort (telega-user-list telega-user-completing-temex)
                      #'telega-user>)))
  (telega-gen-completing-read-list prompt users
                                   #'telega-msg-sender-title-for-completion
                                   #'telega-completing-read-user))

(defvar telega-completing--chat-member-alist nil
  "Results from last `telega--searchChatMembers'.
To be used `telega-completing-read-chat-member' to get user.")

(cl-defun telega-completing--chat-member-choices (chat &key (prefix "")
                                                       default-member)
  "Return completions list of CHAT members."
  (declare (indent 1))
  (setq telega-completing--chat-member-alist
        (mapcar (lambda (sender)
                  (cons (propertize
                         (telega-msg-sender-title-for-completion sender)
                         :sender sender)
                        sender))
                (let ((members (telega--searchChatMembers chat prefix)))
                  (if default-member
                      (cons default-member (delq default-member members))
                    members))))
  (mapcar #'car telega-completing--chat-member-alist))

(defun telega-completing-read (prompt collection &optional predicate
                                      require-match initial-input hist def
                                      inherit-input-method)
  "Same as `completing-read', but uses `telega-completing-read-function'."
  ;; NOTE: Use `unicode-name' completion category (see
  ;; `completion-category-defaults'), because it contains `substring'
  ;; completion style and can complete any candidate even if it is
  ;; prefixed with an image
  (let ((completion-ignore-case t)
        (candidates
         (if (eq telega-completing-read-function #'completing-read-default)
             (lambda (string pred action)
               (cond ((eq action 'metadata)
                      `(metadata (category . unicode-name)
                                 (display-sort-function . ,#'identity)
                                 (cycle-sort-function . ,#'identity)))
                     (t
                      (complete-with-action action collection string pred))))
           collection)))
    (funcall telega-completing-read-function
             prompt candidates predicate require-match
             initial-input hist def inherit-input-method)))

(defun telega-read-string-with-custom-emojis (prompt &rest args)
  "Read string allowing to insert custom emojis with `C-c C-e'."
  (let ((enable-recursive-minibuffers t)
        (minibuffer-allow-text-properties t))
    (minibuffer-with-setup-hook
        (lambda ()
          (local-set-key (kbd "C-c C-e") #'telega-custom-emoji-choose))
      (apply #'read-string prompt args))))

(defvar telega-chat-member-read-history nil)
(defun telega-completing-read-chat-member (prompt chat &optional default-member)
  "Interactively read member of CHAT.
DEFAULT-MEMBER specifies default member to complete.
Return a user."
  (let* ((telega-completing--chat-member-alist nil)
         (name
          (if fido-mode
              ;; Dynamic completion, can complete any chat member
              ;; Works ok only in `fido-mode'
              (completing-read
               prompt
               (lambda (prefix &rest _ignored)
                 (telega-completing--chat-member-choices chat
                   :prefix prefix
                   :default-member (when (or (not prefix)
                                             (string-empty-p prefix))
                                     default-member)))
               nil 'require-match nil 'telega-chat-member-read-history)

            ;; Static completion, can complete only 50 chat members
            (telega-completing-read
             prompt
             (telega-completing--chat-member-choices chat
               :default-member default-member)
             nil t nil 'telega-chat-member-read-history))))
    (cdr (assoc name telega-completing--chat-member-alist))))

(defvar telega-folder-read-history nil)
(defun telega-completing-read-folder (prompt &optional folder-names)
  "Read TDLib folder name completing."
  (telega-completing-read prompt (or folder-names (telega-folder-names)) nil t
                          nil 'telega-folder-read-history))

(defun telega-completing-read-folder-list (prompt &optional folder-names)
  "Read list of the Telegram folders prompting with PROMPT."
  (unless folder-names
    (setq folder-names (telega-folder-names)))
  (telega-gen-completing-read-list prompt folder-names #'identity
                                   #'telega-completing-read-folder))

(defvar telega-folder-icon-name-read-history nil)
(defun telega-completing-read-folder-icon-name (prompt &optional initial-input)
  "Read folder's icon name."
  (telega-completing-read
   prompt
   (mapcar (lambda (icon-name)
             (propertize icon-name 'display
                         (concat (cdr (assoc icon-name
                                             telega-folder-icons-alist))
                                 icon-name)))
           telega-folder-icon-names)
   nil t initial-input 'telega-folder-icon-name-read-history))

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

(defvar telega-location-read-history nil)
(defun telega-read-location (prompt &optional initial-loc default-loc)
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
                          initial-input 'telega-location-read-history
                          default-value)))
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
         (choice (telega-completing-read prompt (mapcar #'car choices) nil t)))
    (cdr (assoc choice choices))))

(defun telega-completing-read-duration (prompt intervals &optional
                                               interval-to-title-function)
  "Read duration completing input."
  (declare (indent 2))
  (let* ((choices (mapcar
                   (lambda (interval)
                     (cons (or (when interval-to-title-function
                                 (funcall interval-to-title-function interval))
                               (telega-duration-human-readable
                                interval 1 'long))
                           interval))
                   intervals))
         (choice (telega-completing-read prompt (mapcar #'car choices) nil t)))
    (cdr (assoc choice choices))))

(defun telega-completing-read-mute-for (prompt)
  "Read mute for notification parameter."
  (telega-completing-read-duration prompt telega-mute-for-intervals
    (lambda (delay)
      (cond ((>= delay telega-mute-for-ever)
             (telega-i18n "lng_mute_duration_forever"))
            ((zerop delay)
             "Disable")))))

(defun telega-read-discussion-chat ()
  "Interactively select or create a chat as discussion group for some channel.
Return a chat."
  (let* ((existing-p (y-or-n-p "Use existing chat? "))
         (linked-chat (if existing-p
                          (telega-completing-read-chat
                           "Select suitable chat: "
                           (telega--getSuitableDiscussionChats))
                        (telega-chat-create "supergroup")))
         (supergroup (when (telega-chat-match-p linked-chat '(type supergroup))
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

(defun telega-join-invite-link-y-or-n-p (invite-link-info)
  ;; NOTE: We create fake "chat" structure to draw brackets
  ;; using `telega-msg-sender-brackets'
  (let* ((fake-chat
          (list :@type "chat"
                :type (cl-ecase (telega--tl-type
                                 (plist-get invite-link-info :type))
                        (inviteLinkChatTypeBasicGroup
                         '(:@type "chatTypeBasicGroup"))
                        (inviteLinkChatTypeSupergroup
                         '(:@type "chatTypeSupergroup"))
                        (inviteLinkChatTypeChannel
                         '(:@type "chatTypeSupergroup" :is_channel t)))))
         (brackets (telega-msg-sender-brackets fake-chat))
         (invite-title
          (telega-ins--as-string
           (when-let ((photo (plist-get invite-link-info :photo)))
             (telega-ins--image
              (telega-chat-photo-info-image-one-line photo)))
           (telega-ins (nth 0 brackets))
           (telega-ins (telega-tl-str invite-link-info :title))
           (telega-ins " ")
           (telega-ins--with-face 'telega-shadow
             (telega-ins (telega-number-human-readable
                          (plist-get invite-link-info :member_count))))
           (telega-ins (telega-symbol 'member))
           (telega-ins (nth 1 brackets)))))
    (y-or-n-p (concat (if (plist-get invite-link-info :creates_join_request)
                          (telega-i18n "lng_group_request_to_join")
                        (telega-i18n "lng_group_invite_join"))
                      " "
                      invite-title
                      "? "))))

(defvar telega-permission-read-history nil)
(defun telega-completing-read-permission (prompt &optional permissions)
  "Read a permission from PERMISSIONS list completing user input.
If PERMISSIONS is ommited, then `telega-chat--chat-permissions' is used."
  (let* ((raw-perms (or permissions telega-chat--chat-permissions))
         (i18n-choices (cl-remove
                        nil (mapcar (lambda (perm-spec)
                                      (when (cdr perm-spec)
                                        (cons (telega-i18n (cdr perm-spec))
                                              (car perm-spec))))
                                    raw-perms)))
         (perm-choice (telega-completing-read
                       prompt (mapcar #'car i18n-choices) nil t nil
                       'telega-permission-read-history)))
    (cdr (assoc perm-choice i18n-choices))))

(defun telega-msg-sender-title-for-completion (msg-sender)
  "Return MSG-SENDER title for completions."
  (telega-ins--as-string
   (if (telega-user-p msg-sender)
       (telega-ins--msg-sender msg-sender
         :with-avatar-p t
         :with-username-p t
         :with-brackets-p t)

     (let ((telega-chat-button-width nil)
           (telega-chat-button-format-plist
            telega-chat-format-plist-for-completion))
       (telega-ins--chat msg-sender)))))

(defvar telega-msg-sender-read-history nil)
;; NOTE: ivy returns copy of the string given in choices, thats why we
;; need to use `assoc'
(defun telega-completing-read-msg-sender (prompt &optional msg-senders history)
  "Read a message sender from list of MSG-SENDERS."
  (let* ((completion-ignore-case t)
         ;; NOTE: use temporary buffer to format titles, to not
         ;; degrade if large buffer (such as rootbuf) is current
         (choices (with-temp-buffer
                    (mapcar
                     (lambda (sender)
                       (cons (telega-msg-sender-title-for-completion sender)
                             sender))
                     msg-senders)))
         (choice (minibuffer-with-setup-hook
                     (eval-when-compile
                       (lambda ()
                         (setq-local nobreak-char-display nil)))
                   (telega-completing-read
                    prompt (mapcar #'car choices) nil t nil
                    (or history 'telega-msg-sender-read-history)))))
    (cdr (assoc choice choices))))

(defun telega-completing-read-topic (chat &optional prompt)
  "Read a CHAT's topic completing user input."
  (let* ((completion-ignore-case t)
         (choices (mapcar (lambda (topic)
                            (cons (telega-ins--as-string
                                   (telega-ins--topic-title topic
                                     :with-icon-p t
                                     :with-maybe-pin-p t))
                                  topic))
                          (telega-chat-topics chat)))
         (choice (telega-completing-read
                  (or prompt
                      (concat (telega-i18n "lng_forum_topic_title") ": "))
                  (mapcar #'car choices) nil t)))
    (cdr (assoc choice choices))))

(defun telega-read-checklist-task ()
  "Read a new checklist task title."
  (telega-read-string-with-custom-emojis
   (concat (string-trim (telega-i18n "lng_todo_create_list_add") nil "\\.+")
           ": ")))

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
         (telega-ins-fmt "PP-ERROR: %S ==>\n" pp-fun)
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
        (= (ewoc-location n0)
           (ewoc-location (ewoc--footer ewoc))))))

(defun telega-ewoc--move-node (ewoc node before-node
                                    &optional save-point-p)
  "Move EWOC's NODE before BEFORE-NODE node, saving point at NODE position.
If NODE and BEFORE-NODE are the same, then just invalidate the node.
If BEFORE-NODE is nil, then move NODE to the bottom.
Save point only if SAVE-POINT is non-nil.
Return new node."
  (let* ((node-value (ewoc--node-data node))
         (node-start (ewoc-location node))
         (node-next (or (ewoc-next ewoc node) (ewoc--footer ewoc)))
         (point (point))
         (point-off (and save-point-p
                         (>= point node-start)
                         node-next
                         (< (point) (ewoc-location node-next))
                         (- point node-start))))
    (telega-save-excursion
      (if (eq node before-node)
          (ewoc-invalidate ewoc node)

        (ewoc-delete ewoc node)
        ;; NOTE: pretty printer for node might use position, see
        ;; `telega-chatbuf-msg--pp'
        (when before-node
          (goto-char (ewoc-location before-node)))
        (setq node
              (if before-node
                  (ewoc-enter-before ewoc before-node node-value)
                (ewoc-enter-last ewoc node-value)))))
    (when (and point-off
               ;; See https://github.com/zevlg/telega.el/issues/197
               (not (equal (ewoc-location node)
                           (when-let ((next-node (or (ewoc-next ewoc node)
                                                     (ewoc--footer ewoc))))
                             (ewoc-location next-node)))))
      (goto-char (+ (ewoc-location node) point-off))
      (telega-buffer--update-win-point))

    node))

(defun telega-svg-create-vertical-bar (&optional bar-width bar-position bar-str
                                                 mask)
  "Create svg image for vertical bar.
BAR-STR is string value for textual vertical bar, by default
`telega-symbol-vertical-bar' is used.
BAR-WIDTH and BAR-POSITION defines how vertical bar is drawn.  If
float values then it is relative to bar width in pixels.  If
integer values, then pixels used."
  (unless bar-str
    (setq bar-str telega-symbol-vertical-bar))
  (or ;(telega-emoji--image-cache-get bar-str 1)
      (let* ((xh (telega-chars-xheight 1))
             (aw-chars (string-width bar-str))
             (xw (telega-chars-xwidth aw-chars))
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
                       :fill-opacity 1
                       :fill "currentColor")
        (setq image (telega-svg-image svg
                      :scale 1.0
                      :height (telega-ch-height 1)
                      :ascent 'center
                      :mask (or mask 'heuristic)
                      :telega-text bar-str))
;        (telega-emoji--image-cache-put bar-str 1 image)
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
   (or (telega-emoji--image-cache-get bar-str 1)
       (let* ((xh (telega-chars-xheight 1))
              (aw-chars (string-width bar-str))
              (xw (telega-chars-xwidth aw-chars))
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
                        :fill-opacity 1
                        :fill "currentColor")
         (setq image (telega-svg-image svg
                       :scale 1.0
                       :width (telega-cw-width aw-chars)
                       :ascent 'center
                       :mask 'heuristic
                       :telega-text bar-str))
         (telega-emoji--image-cache-put bar-str 1 image)
         image))))

(defun telega-svg-create-user-rating-image (level)
  "Generate user level image."
  (let ((svg (telega-svg-create 32 32))
        (mask (dom-node 'mask `((id . "hole"))))
        (outline1 "M 24.635398,5.5565663 16.86354,2.4262347 c -0.647655,-0.2158849 -1.295309,-0.2158849 -1.835022,0 L 7.3646025,5.5565663 C 6.3931203,5.8803937 5.7454655,6.8518759 5.7454655,7.8233581 V 21.532052 c 0,0.755597 0.4317699,1.511194 0.9714822,2.050906 l 7.6639153,5.613009 c 0.43177,0.323827 0.971483,0.539712 1.511195,0.539712 0.539712,0 0.971482,-0.107942 1.511194,-0.43177 l 7.771858,-5.613008 c 0.647655,-0.43177 1.079425,-1.187367 1.079425,-2.050907 V 7.8233581 c 0,-0.9714822 -0.647655,-1.9429644 -1.619137,-2.2667918 z"))
    (svg--def svg mask)
    (svg-rectangle mask 0 0 32 32 :fill "white")
    (svg-text mask (format "%d" level)
              :font-size (/ 32 2)
              :font-family "monospace"
              :font-weight "bold"
              :fill-color "black"
              :text-anchor "middle"
              :x "50%"
              :y "66%")
    (telega-svg-apply-outline
     svg outline1 1
     '(:fill "currentColor" :stroke "currentColor" :mask "url(#hole)"))
    (telega-svg-image svg
      :scale 1.0
      :max-height (telega-ch-height 1)
      :width (telega-cw-width 2)
      :ascent 'center
      :mask 'heuristic
      :telega-text "()")))

(defun telega-box-button--edge-image (which style)
  "Create left or right button edge image
WHICH is one of: `left' or `right'.
STYLE is a style plist."
  (declare (indent 1))
  (cl-assert (memq which '(left right)))
  (let ((cacheprop (format "button-edge-%S-%S" which style)))
    (or (telega-emoji--image-cache-get cacheprop 1)
        (let* ((w (telega-chars-xwidth 1))
               (h (telega-chars-xheight 1))
               (svg (telega-svg-create w h))
               (mask (dom-node 'mask `((id . "hole"))))
               (left-p (eq which 'left))
               (pos-fraction
                (or (telega-box-button--style-get style :x-margin) 0.5))
               (round-fraction
                (or (telega-box-button--style-get style :round-corner) 0.25))
               (xpos (round (* w (if left-p pos-fraction (- 1 pos-fraction)))))
               (sw (or (telega-box-button--style-get style :outline-width) 0))
               (sw2 (/ sw 2.0))
               image)
          (svg--def svg mask)
          (svg-rectangle mask 0 0 w h :fill "white")
          (svg-rectangle mask (if left-p (+ xpos sw2) (- 0 w sw2)) sw2
                         (if left-p (+ w w) (+ w xpos)) (- h sw)
                         :stroke-width sw
                         :stroke-color "black"
                         :fill "black" :rx (round (* h round-fraction)))

          (let ((fill-color (or (telega-box-button--style-get style :background)
                                (face-background 'default))))
            (svg-rectangle svg 0 0 w h
                           :fill (telega-color-name-as-hex-2digits fill-color)
                           :mask "url(#hole)"))
          (when sw
            (svg-rectangle svg (if left-p (+ xpos sw2) (- 0 w sw2)) sw2
                           (if left-p (+ w w) (+ w xpos)) (- h sw)
                           :rx (round (* h round-fraction))
                           :fill "none"
                           :stroke-width sw
                           :stroke-color
                           (if-let ((o-color (telega-box-button--style-get
                                              style :outline-color)))
                               (telega-color-name-as-hex-2digits o-color)
                             "currentColor")))
          ;; Icon in the right top corner
          (unless left-p
            (when-let ((icon (telega-box-button--style-get style :icon-symbol)))
              (svg-text svg icon :font-size w :x 0 :y w
                        :stroke-color "currentColor")))

          (setq image (telega-svg-image svg
                        :scale 1.0
                        :height (telega-ch-height 1)
                        :telega-text (if left-p "[" "]")
                        :ascent 'center))
          (telega-emoji--image-cache-put cacheprop 1 image)
          image))))

(cl-defun telega-svg-create-checkmark (checkmark-sym &key double-p
                                                     (stroke-width 1.0))
  "Create a checkmark."
  (declare (indent 1))
  (or (telega-emoji--image-cache-get checkmark-sym 1)
       (let* ((aw-chars (string-width checkmark-sym))
              (xw (min (telega-chars-xwidth aw-chars)
                       (telega-chars-xheight 1)))
              (svg (telega-svg-create xw xw))
              (check-outline (concat "M1,8 5,14 12,4"
                                     (when double-p
                                       "M8,14 15,4")))
              image)
         (telega-svg-apply-outline
          svg check-outline (/ xw 16.0)
          (nconc (list :fill "none" :stroke "currentColor"
                       :stroke-linecap "round"
                       :stroke-linejoin "round"
                       :stroke-miterlimit (* stroke-width 3)
                       :stroke-width stroke-width)))
         (setq image (telega-svg-image svg
                       :scale 1.0
                       :max-height (telega-ch-height 1)
                       :width (telega-cw-width aw-chars)
                       :ascent 'center
                       :mask 'heuristic
                       :telega-text checkmark-sym))
         (telega-emoji--image-cache-put checkmark-sym 1 image)
         image)))

(defun telega-symbol-emojify (emoji &optional image-spec)
  "Return a copy of EMOJI with  `display' property of EMOJI svg image.
Optionally, IMAGE-SPEC can by specified to use existing image instead
of generating svg for the EMOJI."
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

(defun telega-symbol (ending &optional image)
  "Return possible emojified value for the symbol denoted by ENDING.
ENDING could be a string or a symbol.
If ENDING is a symbol, then value is taken from `telega-symbol-ENDING'
variable.
Only endings listed in `telega-symbols-emojify' are emojified."
  (let ((value (if (stringp ending)
                   ending
                 (symbol-value (intern (format "telega-symbol-%s" ending)))))
        (image-spec (or image (nth 1 (assoc ending telega-symbols-emojify)))))
    (cond ((functionp image-spec)
           (funcall image-spec ending))
          ((or (and telega-use-images image-spec)
               (and telega-emoji-use-images
                    (member ending telega-symbols-emojify)))
           (telega-symbol-emojify value image-spec))
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
  (alter-text-property
   start end 'face
   (lambda (faces)
     (cond ((listp faces)
            (delete face faces))
           ((eq faces face)
            nil)
           (t
            faces)))
   object))

(defun telega-button-highlight--sensor-func (_win oldpos dir)
  "Sensor function to highlight button when entered.
For box buttons use `telega-box-button-active' face, otherwise use
`telega-button-highlight' face."
  (let* ((inhibit-read-only t)
         (button-region (telega--region-with-cursor-sensor
                         (if (eq dir 'entered) (point) oldpos)))
         (start (car button-region))
         (stop (cdr button-region)))
    (when button-region
      (when-let ((active-face (get-text-property start :active-face)))
        (if (eq dir 'entered)
            (add-face-text-property start stop active-face)
          (telega-remove-face-text-property start stop active-face)))

      (when (eq dir 'entered)
        (when-let ((button (button-at start)))
          (telega-button--help-echo button)))

      ;; NOTE: removing face from face list not always makes button to
      ;; redisplay, so force redisplay
;      (redraw-frame (window-frame win))
      )))

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

(defun telega-read-self-destruct-timer (prompt)
  "Read self destruct timer and return TL MessageSelfDestructType."
  (let ((seconds
         (read-number
          (concat prompt " (0-" (telega-i18n "lng_seconds" :count 60) "): "))))
    (if (zerop seconds)
        '(:@type "messageSelfDestructTypeImmediately")
      (list :@type "messageSelfDestructTypeTimer"
            :self_destruct_time seconds))))

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

(defun telega-color-name-from-rgb24 (rgb24)
  "Convert RGB24 int value to color name."
  (format "#%06x" (+ 8388608 rgb24)))

(defun telega-color-name-from-argb (argb)
  "Convert ARGB int value to color name.
Strips alpha component."
  (format "#%06x" (logand (+ 2147483648 argb) 16777215)))

(defun telega-color-name-set-saturation-light (color-name saturation light)
  "For COLOR-NAME set SATURATION and LIGHT.
LIGHT and SATURATION could be nil, to not change its value."
  (let ((hsl (apply #'color-rgb-to-hsl (color-name-to-rgb color-name))))
    (apply #'color-rgb-to-hex
           (apply #'color-hsl-to-rgb
                  (list (nth 0 hsl)
                        (or saturation (nth 1 hsl))
                        (or light (nth 2 hsl)))))))

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

(defun telega-buffer--hack-win-point (&optional point)
  "Workaround Emacs bug.
Emacs does not respect buffer local nil value for
`switch-to-buffer-preserve-window-point', so we hack window point
in `(window-prev-buffers)' to achive behaviour for nil-valued
`switch-to-buffer-preserve-window-point'."
  (unless switch-to-buffer-preserve-window-point
;    (when (version< emacs-version "28.1.0")
    (when-let ((entry (assq (current-buffer) (window-prev-buffers))))
      (setf (nth 2 entry)
            (copy-marker (or point (point))
                         (marker-insertion-type (nth 2 entry)))))))

(defun telega-buffer--update-win-point (&optional point)
  "Update current buffer's window points."
  (let ((point (or point (point))))
    (telega-buffer--hack-win-point point)
    (dolist (win (get-buffer-window-list))
      (set-window-point win point))))

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
Also, applies `telega-image-transform-smoothing' setting."
  (declare (indent 3))
  (when telega-use-images
    (apply #'create-image
           file-or-data
           (or type (when (eq telega-use-images 'imagemagick) 'imagemagick))
           data-p
           (nconc (list :transform-smoothing telega-image-transform-smoothing)
                  props))))

(defun telega-etc-file-create-image (filename cwidth &rest props)
  "Create image from etc's FILENAME.
Width for the resulting image will be of CWIDTH chars.
Maximum height for the image is 1."
  (apply #'telega-create-image (telega-etc-file filename) nil nil
         :scale 1.0
         :ascent 'center
         :mask 'heuristic
         :max-height (telega-ch-height 1)
         :width (telega-cw-width cwidth)
         props))

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
        (when (file-exists-p "/dev/snd")
          " --device /dev/snd:/dev/snd")
        (when (file-exists-p "/dev/video0")
          " --device /dev/video0:/dev/video0")
        (when (file-exists-p "/dev/video1")
          " --device /dev/video1:/dev/video1")

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
    (telega-create-image png-filename nil nil
      :scale 1.0 :ascent 'center
      :width size :height size)))

(defun telega-completing-read-emoji-status-duration (prompt)
  "Read duration for the custom emoji."
  (let* ((choices (mapcar (lambda (delay)
                            (cons (cond ((zerop delay) "Custom")
                                        (t (telega-duration-human-readable
                                            delay 1 'long)))
                                  delay))
                          '(3600 7200 28800 172800 0)))
         (choice (telega-completing-read prompt (mapcar #'car choices) nil t))
         (duration (cdr (assoc choice choices))))
    (if (zerop duration)
        (- (telega-read-timestamp "Timestamp: ")
           (telega-time-seconds))
      duration)))

(defvar telega-language-code-read-history nil)
(defun telega-completing-read-language-code (prompt)
  "A two-letter ISO 639-1 language code."
  (let* ((candidates-alist
          (mapcar (lambda (spec)
                    (cons (concat (car spec) " (" (cdr spec) ")")
                          (cdr spec)))
                  telega-translate-languages-alist))
         (lang (telega-completing-read
                prompt (mapcar #'car candidates-alist) nil t nil
                'telega-language-code-read-history)))
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
              (telega-help-win--rm-tdlib-callback
               telega-server--callback-extra)
              (let ((inhibit-read-only t)
                    ;; NOTE: retain `line-prefix' and `wrap-prefix'
                    ;; text properties of the "Loading..." label, so
                    ;; new text will be inserted at the same place as
                    ;; "Loading..." label
                    (lwprefix (when show-loading-p
                                (get-text-property marker 'line-prefix))))
                (when show-loading-p
                  (delete-region marker (+ marker marker-len)))
                (telega-save-excursion
                  (goto-char marker)
                  (telega-ins--line-wrap-prefix lwprefix
                    (apply insert-func insert-args)))))))))))

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

(defun telega-format-mode-line (format &optional telega-default-face)
  "Same as `format-mode-line', but takes into account `telega--default-face'.
Also return nil if resulting string is empty."
  (let* ((telega--default-face (or telega-default-face 'mode-line))
         (ret-string (format-mode-line format nil nil (current-buffer))))
    (unless (string-empty-p ret-string)
      ret-string)))

(defun telega-mode-line-align (how with-string &optional column)
  "Align WITH-STRING accornding to HOW.
HOW is either `center' or `right'.
COLUMN is the column to aligned to."
  (when with-string
    (concat
     (propertize " " 'display
                 (list 'space :align-to (/ (- (or column (window-width))
                                              (string-width with-string))
                                           (cl-ecase how
                                             (center 2)
                                             (right 1)))))
     with-string)))

(defvar telega-text-formatting-entity-read-history nil)
(defun telega-completing-read-text-formatting-entity (prompt)
  "Interactively read text formatting entity type."
  (let* ((completion-ignore-case t)
         (fmt-alist (mapcar (lambda (fname)
                              (cons (telega-i18n fname) fname))
                            '("lng_menu_formatting_bold"
                              "lng_menu_formatting_italic"
                              "lng_menu_formatting_underline"
                              "lng_menu_formatting_spoiler"
                              "lng_menu_formatting_monospace"
                              "lng_menu_formatting_blockquote"
                              "lng_menu_formatting_strike_out"
                              "lng_menu_formatting_link_create"
                              "lng_menu_formatting_clear")))
         (i18n-fmt-name (telega-completing-read
                         prompt (mapcar #'car fmt-alist) nil t nil
                         'telega-text-formatting-entity-read-history))
         (fmt-name (cdr (assoc i18n-fmt-name fmt-alist))))
    (pcase fmt-name
      ("lng_menu_formatting_bold"
       '(:@type "textEntityTypeBold"))
      ("lng_menu_formatting_italic"
       '(:@type "textEntityTypeItalic"))
      ("lng_menu_formatting_underline"
       '(:@type "textEntityTypeUnderline"))
      ("lng_menu_formatting_strike_out"
       '(:@type "textEntityTypeStrikethrough"))
      ("lng_menu_formatting_spoiler"
       '(:@type "textEntityTypeSpoiler"))
      ("lng_menu_formatting_monospace"
       '(:@type "textEntityTypePre"))
      ("lng_menu_formatting_blockquote"
       '(:@type "textEntityTypeBlockQuote"))
      ("lng_menu_formatting_link_create"
       (list :@type "textEntityTypeTextUrl"
             :url (read-string
                   (concat (telega-i18n "lng_formatting_link_url") ": "))))
      ("lng_menu_formatting_clear"
       nil))))

(defun telega-msg-reaction-title-for-completion (reaction-type)
  "Return REACTION-TYPE title for completion."
  ;; NOTE: use textual emoji name underneath, so you can search
  ;; reaction by name. See https://t.me/emacs_telega/44607
  (cl-ecase (telega--tl-type reaction-type)
    (reactionTypeEmoji
     (let ((emoji (telega-tl-str reaction-type :emoji)))
       ;; NOTE: in Emacs if consecutive chars has `eq' `display'
       ;; property it is displayed as single unit (ref: 40.16.1
       ;; Display Specs That Replace The Text)
       (concat emoji
               (when-let ((emoji-name (telega-emoji-name emoji)))
                 (propertize emoji-name
                             'display (get-text-property 0 'display emoji))))))
    (reactionTypeCustomEmoji
     (let* ((custom-emoji-id (plist-get reaction-type :custom_emoji_id))
            (custom-emoji (or (telega-custom-emoji-get custom-emoji-id)
                              (seq-first (telega--getCustomEmojiStickers
                                             (list custom-emoji-id)))))
            (emoji (telega-tl-str custom-emoji :emoji)))
       (telega-ins--as-string
        (telega-ins--image
         (telega-sticker--image custom-emoji) nil
         :telega-text (concat emoji (telega-emoji-name emoji))))))
    (reactionTypePaid
     (telega-symbol 'telegram-star))))

(defvar telega-msg-reaction-read-history nil)
(defun telega-completing-read-msg-reaction (msg prompt &optional
                                                msg-available-reactions
                                                custom-label)
  "Read reaction for the message MSG.
Return `custom' if custom reaction has been chosen.
Return nil if no reaction is available for the MSG."
  (declare (indent 1))
  (let* ((custom-label (propertize (or custom-label "Custom")
                                   'face 'bold))
         (chat-av-reactions
          (plist-get (telega-msg-chat msg) :available_reactions))
         (msg-av-reactions
          (or msg-available-reactions
              (telega--getMessageAvailableReactions msg)))
         (top-reactions
          (mapcar (lambda (avr)
                    (telega--ReactionType (plist-get avr :type)))
                  (plist-get msg-av-reactions :top_reactions)))
         (reaction-choices
          (mapcar (lambda (rtype)
                    (cons (telega-msg-reaction-title-for-completion rtype)
                          rtype))
                  ;; NOTE: sort reactions making top-reactions be on
                  ;; top.  See https://t.me/emacs_telega/44605
                  ;; `telega-default-reaction-type' always goes first
                  (sort
                   (cl-case (telega--tl-type chat-av-reactions)
                     (chatAvailableReactionsSome
                      (mapcar #'telega--ReactionType
                              (plist-get chat-av-reactions :reactions)))
                     (chatAvailableReactionsAll
                      (mapcar (lambda (emoji)
                                (list :@type "reactionTypeEmoji" :emoji emoji))
                              telega-emoji-reaction-list)))
                   (lambda (rtype1 rtype2)
                     (> (length (member rtype1 top-reactions))
                        (length (member rtype2 top-reactions)))))))
         (all-choices
          (nconc reaction-choices
                 (when (plist-get msg-av-reactions :allow_custom_emoji)
                   (list (cons custom-label 'custom)))))
         (choice (when all-choices
                   (telega-completing-read
                    prompt (mapcar #'car all-choices) nil t nil
                    'telega-msg-reaction-read-history))))
    (when choice
      (cdr (assoc choice all-choices)))))

(defvar telega-saved-messages-tag-read-history nil)
(defun telega-completing-read-saved-messages-tag (prompt &optional sm-topic-id
                                                         new-tag-label)
  "Read a Saved Messages tag.
Return nil if there is no tags for the SM-TOPIC-ID or new tag is choosen."
  (when-let* ((tags (telega-saved-messages-tags sm-topic-id))
              (tag-choices
               (mapcar (lambda (tag)
                         (cons (concat (telega-msg-reaction-title-for-completion
                                        (plist-get tag :tag))
                                       (telega-tl-str tag :label))
                               tag))
                       tags))
              (choices (nconc (mapcar #'car tag-choices)
                              (when new-tag-label
                                (list (propertize (if (stringp new-tag-label)
                                                      new-tag-label
                                                    "Create New Tag")
                                                  'face 'bold)))))
              (choice (telega-completing-read
                       prompt choices nil t nil
                       'telega-saved-messages-tag-read-history)))
    (cdr (assoc choice tag-choices))))

(defun telega-float-clamp (number digits)
  "Clamp NUMBER to the number of DIGITS after the dot."
  (string-to-number
   (string-trim-right (format (format "%%.%df" digits) number) "\\.0+?")))

;; Help functions for developers
(defun telega-check-tdlib-methods ()
  (interactive)
  (let* ((tdlib-file (expand-file-name "telega-tdlib.el"
                                       telega--lib-directory))
         (td-spec-file (expand-file-name (format "etc/td-api-%s.tl"
                                                 telega-tdlib-min-version)
                                         telega--lib-directory))
         (methods-for-bots '("setBotName"))
         (method-regexp "\\([A-Z][a-zA-Z]+\\)")
         telega-methods td-methods)
    (with-temp-buffer
      (insert-file-contents tdlib-file)
      (goto-char (point-min))
      (while (re-search-forward (concat "^(\\(?:cl-\\)?defun telega--"
                                        method-regexp " ")
                                nil t)
        (setq telega-methods (cons (match-string 1) telega-methods))))
    (with-temp-buffer
      (insert-file-contents td-spec-file)
      (goto-char (point-min))
      (search-forward "---functions---")
      (while (re-search-forward (concat "^" method-regexp) nil t)
        (setq td-methods (cons (match-string 0) td-methods))))

    (let ((obsolete (seq-difference telega-methods td-methods #'string=))
          (unimplemented (seq-difference td-methods (nconc telega-methods
                                                           methods-for-bots)
                                         #'string=)))
      (with-help-window "*TDLib checks*"
        (telega-ins-fmt "TDLib %s\n" telega-tdlib-min-version)
        (telega-ins "Obsolete methods:\n"
                    "----------------\n")
        (dolist (method (sort obsolete #'string<))
          (telega-ins "  " method "\n"))
        (telega-ins "\n")

        (telega-ins "Not implemented methods:\n"
                    "----------------\n")
        (dolist (method (sort unimplemented #'string<))
          (telega-ins "  " method "\n"))))))

(defun telega-check-tdlib-events ()
  (interactive)
  (let ((tdlib-events-file (expand-file-name "telega-tdlib-events.el"
                                             telega--lib-directory))
        (td-spec-file (expand-file-name (format "etc/td-api-%s.tl"
                                                telega-tdlib-min-version)
                                        telega--lib-directory))
        (events-for-bots
         '("updateNewChatJoinRequest" "updateChatMember"
           "updatePollAnswer" "updatePoll" "updateNewCustomQuery"
           "updateNewCustomEvent" "updateNewPreCheckoutQuery"
           "updateNewShippingQuery" "updateNewInlineCallbackQuery"
           "updateNewCallbackQuery" "updateNewChosenInlineResult"
           "updateNewInlineQuery" "updateChatBoost"
           "updateMessageReaction" "updateMessageReactions"))
        telega-events td-events)
    (with-temp-buffer
      (insert-file-contents tdlib-events-file)
      (goto-char (point-min))
      (while (re-search-forward
              "^(defun telega--on-update\\([A-Z][a-zA-Z]+\\)" nil t)
        (setq telega-events (cons (concat "update" (match-string 1))
                                  telega-events))))

    (with-temp-buffer
      (insert-file-contents td-spec-file)
      (goto-char (point-min))
      (while (re-search-forward "^update\\([A-Z][a-zA-Z]+\\)" nil t)
        (setq td-events (cons (concat "update" (match-string 1))
                              td-events))))

    (let ((obsolete (seq-difference telega-events td-events))
          (unimplemented (seq-difference td-events
                                         (nconc telega-events events-for-bots))))
      (with-help-window "*TDLib checks*"
        (telega-ins-fmt "TDLib %s\n" telega-tdlib-min-version)
        (telega-ins "Obsolete events:\n"
                    "----------------\n")
        (dolist (event obsolete)
          (telega-ins "  " event "\n"))
        (telega-ins "\n")

        (telega-ins "Not implemented events:\n"
                    "----------------\n")
        (dolist (event unimplemented)
          (telega-ins "  " event "\n"))))))

(defun telega-line-pixel-height (&optional position)
  "Same as `line-pixel-height' but accept POSITION as argument."
  (if position
      (save-excursion
        (goto-char position)
        (line-pixel-height))
    (line-pixel-height)))

(defun telega-dockrefile-tdlib-version (&optional dockerfile)
  "Extract TDLib version from DOCKERFILE."
  (with-temp-buffer
    (insert-file-contents (or dockerfile (telega-etc-file "Dockerfile")))
    (goto-char (point-min))
    (let ((version (when (re-search-forward "^ARG tdlib_version=\\(.*\\)$")
                     (match-string 1)))
          (branch (when (re-search-forward "^ARG tdlib_branch=\\(.*\\)$")
                    (match-string 1))))
      (concat version "-" branch))))


;;; Stipple drawing
(defvar telega-stipple--cache-alist nil)

(defun telega-stipple-create (w h)
  (list w h (make-string (* h (/ (+ w 7) 8)) 0)))

;;
;; +---> x
;; |
;; v
;; y
;; w = 30, h = 2,  XX - trailing padding
;;        byte[1]    byte[2]    byte[3]     byte[0]
;; row0: |........| |........| |.......XX| |..........|
;;        byte[5]    byte[6]    byte[7]     byte[4]
;; row1: |........| |........| |.......XX| |..........|
;;
(defsubst telega-stipple-set-pixel (s x y val)
  (let* ((w (nth 0 s))
         (h (nth 1 s))
         (data (nth 2 s))
         (row-bits (* 8 (/ (+ w 7) 8)))
         (row-bit-idx (% (+ x 8 (- row-bits w)) row-bits))
         (bit-off (% (if (> row-bits 8) row-bit-idx x) 8))
         (byte-mask (ash 1 bit-off))
         (byte-idx (progn
                    (cl-assert (and (< x w) (< y h)))
                    (/ (+ row-bit-idx (* y row-bits)) 8)))
         (byte-val (aref data byte-idx)))
    (aset data byte-idx (if val
                            (logior byte-val byte-mask)
                          (logand byte-val (lognot byte-mask))))
    s))

(defun telega-stipple-fill-by-predicate (s predicate &optional no-cache-p)
  (declare (indent 1))
  (let* ((w (nth 0 s))
         (h (nth 1 s))
         (cache-key (list w h predicate))
         (s-cached (unless no-cache-p
                     (cdr (assoc cache-key telega-stipple--cache-alist)))))
    (unless s-cached
      (dotimes (y h)
        (dotimes (x w)
          (telega-stipple-set-pixel s x y (funcall predicate x y w h))))
      (unless no-cache-p
        (setq telega-stipple--cache-alist
              (cons (cons cache-key s)
                    telega-stipple--cache-alist)))
      (setq s-cached s))
    s-cached))

(defun telega-stipple-arc-p (x y r)
  ;; Left-Top arc's 4th
  ;; Check
  (let* ((r2 (* r r))
         (delta (+ (* x x) (* y y) (- r2))))
    (cond ((and (<= x r) (>= x y) (>= delta 0))
           (let ((delta1 (+ (* (1- x) (1- x)) (* y y) (- r2)))
                 (delta2 (+ (* (1+ x) (1+ x)) (* y y) (- r2))))
             (and (< delta1 0)
                  (> delta2 0))
             ))
          ((and (<= y r) (>= y x) (>= delta 0))
           (let ((delta1 (+ (* x x) (* (1- y) (1- y)) (- r2)))
                 (delta2 (+ (* x x) (* (1+ y) (1+ y)) (- r2))))
             (and (< delta1 0)
                  (> delta2 0))
             ))
          )))

(defun telega-stipple-fill-arc (x y _w _h)
  (telega-stipple-arc-p x y 20))

(defun telega-stipple-fill-button-left (x y w h)
  (let ((size (/ w 2)))
    (cond ((or (= y 1) (= y (- h 2)))
           (> x size))
          ((and (> y 1) (< y (- h 2)))
           (= x size)))))

(defun telega-stipple-fill-button-right (x y w h)
  (let ((size (/ w 2)))
    (cond ((or (= y 1) (= y (- h 2)))
           (< x size))
          ((and (> y 1) (< y (- h 2)))
           (= x size)))))

(defun telega-stipple--box-button-body-gen (style)
  (let ((h-offset (or (telega-box-button--style-get style :h-offset) 0))
        (sw (or (telega-box-button--style-get style :outline-width) 2)))
    (lambda (_x y _w h)
      (or (<= h-offset y (- sw 1))
          (<= (- h h-offset sw) y (- h h-offset 1))))))

(defun telega-stipple-hline-predicate (h-offset stroke-width)
  (lambda (_x y _w _h)
    (<= h-offset y (- stroke-width 1))))

(defun telega-stipple-fill-sm-tag-right (x y w h)
  (let* ((r (/ w 5))
         (c-x (+ r r))
         (c-y (/ h 2))
         (ratio (* 2 (/ (float w) h))))
    (when (or (= x 0)
              (and (<= y (/ h 2))
                   (<= (/ (float x) y) ratio))
              (and (>= y (/ h 2))
                   (<= (/ (float x) (- h y)) ratio)))
      ;; Check for circle
      (> (+ (* (- x c-x) (- x c-x))
            (* (- y c-y) (- y c-y)))
         (* r r)))))

(provide 'telega-util)

;;; telega-util.el ends here

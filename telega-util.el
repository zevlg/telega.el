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
(require 'ansi-color)                   ; `ansi-color-apply'
(require 'url-util)                     ; `url-unhex-string'
(require 'org)                          ; `org-read-date'
(require 'rainbow-identifiers)

(require 'telega-customize)

(declare-function telega-root--buffer "telega-root")
(declare-function telega-chat--type "telega-chat" (chat &optional no-interpret))
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

(defsubst telega-plist-del (plist prop)
  "From PLIST remove property PROP."
  (cl--plist-remove plist (plist-member plist prop)))

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
  (cl-find-if (lambda (frame)
                (frame-parameter frame 'window-system))
              (nconc (list (window-frame
                            (get-buffer-window (or telega--current-buffer
                                                   (telega-root--buffer)))))
                     (frame-list))))

(defun telega-focus-state (&optional frame)
  "Return non-nil if FRAME has focus.
Can be used as value for `telega-online-status-function'."
  (if (fboundp 'frame-focus-state)
      (funcall 'frame-focus-state frame)
    ;; NOTE: For tty frame always return non-nil
    ;; see https://t.me/emacs_telega/7419
    (or (not (display-graphic-p frame))
        (frame-parameter frame 'x-has-focus))))

(defun telega-buffer-p ()
  "Return non-nil if current buffer is some telega buffer.
Could be used as value for `telega-online-status-function'."
  (when (or (derived-mode-p 'telega-root-mode)
            (derived-mode-p 'telega-chat-mode)
            (string-prefix-p "*Telega" (buffer-name)))
    t))

(defun telega-chars-xwidth (n)
  "Return pixel width for N characters"
  ;; NOTE: Same (* n (window-font-width (get-buffer-window nil (telega-x-frame))))
  ;; but without tweaking on window configuration, which breaks inserters
  (* n (if-let ((tframe (telega-x-frame)))
           (with-current-buffer (or telega--current-buffer (current-buffer))
             (let* ((info (font-info (face-font 'default tframe) tframe))
                    (width (aref info 11)))
               (if (> width 0)
                   width
                 (aref info 10))))
         (frame-char-width))))

(defun telega-chars-xheight (n)
  "Return pixel height for N characters"
  (* n (if-let ((tframe (telega-x-frame)))
           (with-current-buffer (or telega--current-buffer (current-buffer))
             (aref (font-info (face-font 'default tframe) tframe) 3))
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

(defun telega-current-column ()
  "Same as `current-column', but take into account width of the characters."
  (string-width (buffer-substring (point-at-bol) (point))))

(defun telega-temp-name (prefix &optional ext)
  "Generate unique temporary file name with PREFIX and extension EXT.
Specify EXT with leading `.'."
  (concat (expand-file-name (make-temp-name prefix) telega-temp-dir) ext))

(defun telega-svg-clip-path (svg id)
  (let ((cp (dom-node 'clipPath `((id . ,id)))))
    (svg--def svg cp)
    cp))

(defun telega-svg-path (svg d &rest args)
  (svg--append svg (dom-node 'path
                             `((d . ,d)
                               ,@(svg--arguments svg args)))))

(defun telega-svg-progress (svg progress)
  "Insert progress circle into SVG."
  (let* ((w (alist-get 'width (cadr svg)))
         (h (alist-get 'height (cadr svg)))
         ;; progress clipping mask
         (angle-o (+ pi (* 2 pi (- 1.0 progress))))
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
                :fill-color (face-foreground 'shadow)
                :fill-opacity "0.25"
                :clip-path "url(#pclip)")
    svg))

(defun telega-svg-image (svg &rest props)
  "Return an image object from SVG.
PROPS is passed on to `create-image' as its PROPS list."
  ;; NOTE: work around problem displaying unicode characters in some
  ;; librsvg versions (in my case 2.40.13).  Encoded (in &#xxxx format)
  ;; text is only displayed corretly if <xml ..?> node is specified
  (apply #'create-image
         (with-temp-buffer
           (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
           (svg-print svg)
           (buffer-string))
         'svg t props))

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
    (svg-image svg :scale 1
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

;; code taken from
;; https://emacs.stackexchange.com/questions/14420/how-can-i-fix-incorrect-character-width
(defun telega-symbol-widths-install (symbol-widths-alist)
  "Add symbol widths from SYMBOL-WIDTHS-ALIST to `char-width-table'.
Use it if you have formatting issues."
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table)))
  (dolist (pair symbol-widths-alist)
    (let ((width (car pair))
          (symbols (cdr pair))
          (table (make-char-table nil)))
      (dolist (sym symbols)
        (set-char-table-range
         table (if (stringp sym) (string-to-char sym) sym) width))
      (optimize-char-table table)
      (set-char-table-parent table char-width-table)
      (setq char-width-table table))))

(defun telega-symbol-set-width (symbol width)
  "Declare that SYMBOL's width is equal to WIDTH.
SYMBOL could be a cons cell of codepoints, specifying the range."
  (setf (alist-get width telega-symbol-widths)
        (cons symbol (alist-get width telega-symbol-widths))))

(defun telega-time-seconds ()
  "Return current time as unix timestamp."
  (floor (time-to-seconds)))

(defun telega-distance-human-readable (meters)
  "Convert METERS to human readable string."
  (cond ((not (integerp meters)) "unknown")
        ((> meters 10000) (format "%d km" (/ meters 1000)))
        ((>= meters 1000) (format "%.1f km" (/ meters 1000.0)))
        (t (format "%d m" meters))))

(defun telega-duration-human-readable (seconds &optional n)
  "Convert SECONDS to human readable string.
If N is given, then use only N significant components.
For example if duration is 4h:20m:3s then with N=2 4H:20m will be returned.
By default N=3 (all components).
N can't be 0."
  (cl-assert (or (null n) (> n 0)))
  (let ((ncomponents (or n 3))
        comps)
    (when (>= seconds 3600)
      (setq comps (list (format "%dh" (/ seconds 3600)))
            seconds (% seconds 3600)
            ncomponents (1- ncomponents)))
    (when (and (> ncomponents 0) (>= seconds 60))
      (setq comps (nconc comps (list (format "%dm" (/ seconds 60))))
            seconds (% seconds 60)
            ncomponents (1- ncomponents)))
    (when (and (> ncomponents 0) (or (null comps) (> seconds 0)))
      (setq comps (nconc comps (list (format "%ds" seconds)))))
    (mapconcat #'identity comps ":")))

(defun telega-etc-file (filename)
  "Return absolute path to FILENAME from etc/ directory in telega."
  (expand-file-name (concat "etc/" filename) telega--lib-directory))

(defun telega-link-props (link-type link-to &optional face)
  "Generate props for link button openable with `telega-link--button-action'."
  (cl-assert (memq link-type '(url file username user hashtag)))

  (list 'action 'telega-link--button-action
        'face (or face 'telega-link)
        :telega-link (cons link-type link-to)))

(defun telega-link--button-action (button)
  "Browse url at point."
  (let ((link (button-get button :telega-link)))
    (telega-debug "Action on link: %S" link)
    (cl-ecase (car link)
      (user (telega-describe-user (telega-user--get (cdr link))))
      (username
       (let ((user (telega-user--by-username (cdr link))))
         (if user
             (telega-describe-user user)
           (message (format "Fetching info about %s" (cdr link)))
           (telega--searchPublicChat (cdr link)
             (lambda (chat)
               (if (eq (telega-chat--type chat 'no-interpret)
                       'private)
                   (telega-describe-user
                    (telega-user--get (plist-get chat :id)))
                 (telega-describe-chat chat)))))))
      (hashtag
       (message "TODO: `hashtag' button action: tag=%s" (cdr link)))
      (url
       (telega-browse-url (cdr link)))
      (file
       (telega-open-file (cdr link)))
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
   url nil nil 1))

(defun telega--entity-to-properties (entity text)
  "Convert telegram ENTITY to emacs text properties to apply to TEXT."
  (let ((ent-type (plist-get entity :type)))
    (cl-case (telega--tl-type ent-type)
      (textEntityTypeMention
       (telega-link-props 'username text
                          (if (and telega-msg-contains-unread-mention
                                   (equal text (telega-user--name
                                                (telega-user-me) 'short)))
                              (list 'telega-entity-type-mention 'bold)
                            'telega-entity-type-mention)))
      (textEntityTypeMentionName
       (telega-link-props 'user (plist-get ent-type :user_id)
                          (if (and telega-msg-contains-unread-mention
                                   (eq (plist-get ent-type :user_id)
                                       telega--me-id))
                              (list 'telega-entity-type-mention 'bold)
                            'telega-entity-type-mention)))
      (textEntityTypeHashtag
       (telega-link-props 'hashtag text))
      (textEntityTypeBold
       (list 'face 'telega-entity-type-bold))
      (textEntityTypeItalic
       (list 'face 'telega-entity-type-italic))
      (textEntityTypeUnderline
       (list 'face 'telega-entity-type-underline))
      (textEntityTypeStrikethrough
       (list 'face 'telega-entity-type-strikethrough))
      (textEntityTypeCode
       (list 'face 'telega-entity-type-code))
      (textEntityTypePre
       (list 'face 'telega-entity-type-pre))
      (textEntityTypePreCode
       (list 'face 'telega-entity-type-pre))
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
       (list 'face 'telega-entity-type-botcommand))
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
       (format "[%s](%s)" text (plist-get ent-type :url)))
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
  "Create formattedText from TEST, marking whole TEXT with ENTITY-TYPE."
  (list :@type "formattedText"
        :text text
        :entities (if entity-type
                      (vector
                       (list :@type "textEntity"
                             :offset 0
                             :length (telega-string-fmt-text-length text)
                             :type entity-type))
                    [])))

(defun telega-string-as-markup (str markup-name markup-func)
  "From STR create string with markup named MARKUP-NAME.
MARKUP-FUNC is function taking string and returning formattedText."
  (concat (propertize
           "❰" 'display (propertize (concat "<" markup-name ">")
                                    'face 'shadow)
           'rear-nonsticky t
           :telega-markup-start markup-func)
          str
          (propertize
           "❱" 'display (propertize (concat "</" markup-name ">")
                                    'face 'shadow)
           'rear-nonsticky t
           :telega-markup-end markup-func)))

(defun telega-markup-html-fmt (str)
  "Format string STR to formattedText using html markup."
  (telega-fmt-text-desurrogate
   (telega--parseTextEntities str (list :@type "textParseModeHTML"))))

(defun telega-markup-markdown-fmt (markdown-version str)
  "Format string STR to formattedTetx using MARKDOWN-VERSION."
  (if markdown-version
      ;; For markdown mode, escape underscores in urls
      ;; See https://github.com/tdlib/td/issues/672
      ;; See https://github.com/zevlg/telega.el/issues/143
      (telega-fmt-text-desurrogate
       (telega--parseTextEntities
        (telega-escape-underscores str)
        (list :@type "textParseModeMarkdown"
              :version markdown-version)))

    (telega-fmt-text (substring-no-properties str))))

(defun telega-markup-markdown1-fmt (str)
  (telega-markup-markdown-fmt 1 str))

(defun telega-markup-markdown2-fmt (str)
  (telega-markup-markdown-fmt 2 str))

(defun telega-string-split-by-markup (text &optional default-markup-func)
  "Split TEXT by markups.
Use DEFAULT-MARKUP-FUNC for strings without markup.
Return list of list where first element is markup function and
second is substring."
  (unless default-markup-func
    (setq default-markup-func
          (apply-partially #'telega-markup-markdown-fmt nil)))

  (let ((start 0) (end (length text)) markup-start result)
    (while (setq markup-start
                 (text-property-not-all
                  start end :telega-markup-start nil text))
      (unless (eq start markup-start)
        (push (list default-markup-func (substring text start markup-start))
              result))
      (let ((markup-end (text-property-not-all
                         markup-start end :telega-markup-end nil text)))
        (unless markup-end
          (user-error "Markup is non-closed"))
        (let ((markup-func (get-text-property
                            markup-start :telega-markup-start text)))
          (cl-assert (eq markup-func (get-text-property
                                      markup-end :telega-markup-end text)))
          (push (list markup-func (substring
                                   text (1+ markup-start) markup-end))
                result))
        ;; Skip markup-end char
        (setq start (1+ markup-end))))

    ;; Rest of the string
    (when (< start end)
      (push (list default-markup-func (substring text start))
            result))
    (nreverse result)))

(defun telega-string-fmt-text (text &optional markdown-version)
  "Convert TEXT to `formattedTex' type.
If MARKDOWN is non-nil then format TEXT as markdown.
TEXT could contain substrings marked with custom markup, for
these parts of the string MARKDOWN-VERSION is ignored."
  (apply #'telega-fmt-text-concat
         (mapcar (apply-partially #'apply #'funcall)
                 (telega-string-split-by-markup
                  text (apply-partially
                        #'telega-markup-markdown-fmt markdown-version)))))

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
    (list :@type "formattedTex"
          :text (apply #'concat (mapcar (telega--tl-prop :text) fmt-texts))
          :entities (apply #'seq-concatenate 'vector ents))))

(defun telega--fmt-text-markdown (fmt-text)
  "Return formatted text FMT-TEXT as markdown syntax."
  ;; TODO: support nested markdown
  (let ((text (substring (plist-get fmt-text :text)))
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
    (let ((ret-text (apply 'concat (mapcar 'telega--entity-to-markdown
                                           (nreverse strings)))))
      (remove-text-properties 0 (length ret-text) (list 'face) ret-text)
      ret-text)))

;; NOTE: FOR-MSG might be used by advices, see contrib/telega-mnz.el
(defun telega--fmt-text-faces (fmt-text &optional _for-msg)
  "Apply faces to formatted text FMT-TEXT.
Return text string with applied faces."
  (let ((text (substring (plist-get fmt-text :text))))
    (seq-doseq (ent (plist-get fmt-text :entities))
      (let* ((beg (plist-get ent :offset))
             (end (+ (plist-get ent :offset) (plist-get ent :length)))
             (props (telega--entity-to-properties
                     ent (substring-no-properties text beg end)))
             (face (plist-get props 'face)))
        (when props
          (add-text-properties
           beg end (nconc (list 'rear-nonsticky t)
                          (telega-plist-del props 'face)) text))
        (when face
          (add-face-text-property beg end face 'append text))))
    text))

(defun telega-fmt-text-string (fmt-text &optional as-markdown)
  "Return formattedText FMT-TEXT as string.
If AS-MARKDOWN is non-nil, then format it as markdown syntax,
otherwise return propertized string."
  (if as-markdown
      (telega--fmt-text-markdown fmt-text)
    (telega--fmt-text-faces fmt-text)))

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
  "Locate region of the button with `cursor-sensor-functions' set.
Return `nil' if there is no button with `cursor-sensor-functions' at POS."
  (when (get-text-property pos 'cursor-sensor-functions)
    (let ((prev (previous-single-property-change pos 'cursor-sensor-functions)))
      (when (and prev (get-text-property prev 'cursor-sensor-functions))
        (setq pos prev))
      (telega--region-by-text-prop pos 'cursor-sensor-functions))))

;; NOTE: ivy returns copy of the string given in choices, thats why we
;; need to use 'string= as testfun in `alist-get'
(defun telega-completing-read-chat (prompt &optional chats sort-criteria)
  "Read chat by title.
CHATS - list of the chats to select from.  By default all chats are used.
SORT-CRITERIA is a chat sort criteria to apply. (NOT YET)"
  (let ((choices (mapcar (lambda (chat)
                           (list (telega-chatbuf--name chat) chat))
                         (telega-sort-chats
                          (or sort-criteria telega-chat-completing-sort-criteria)
                          (telega-filter-chats (or chats telega--ordered-chats)
                                               '(or main archive))))))
    (car (alist-get (funcall telega-completing-read-function
                             prompt choices nil t)
                    choices nil nil 'string=))))

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
                                     (list (,item-read-fun
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
                                   telega-completing-read-chat sort-criteria))

(defun telega-completing-read-user (prompt &optional users)
  "Read user by his name from USERS list."
  (declare (indent 1))
  (let ((choices (mapcar (lambda (user)
                           (list (telega-user--name user) user))
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
  (telega-gen-completing-read-list prompt users-list #'telega-user--name
                                   telega-completing-read-user))

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
                                   telega-completing-read-folder))

(defun telega-location-to-string (location)
  "Convert LOCATION plist to string representation."
  (concat (number-to-string
           (plist-get location :latitude))
          "N" ","
          (number-to-string
           (plist-get location :longitude))
          "E"))

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

(defun telega-completing-titles ()
  "Return list of titles ready for completing.
KIND is one of `chats', `users' or nil."
  (let ((result))
    (dolist (chat (telega-filter-chats telega--ordered-chats 'all))
      (setq result (cl-pushnew (telega-chat-title chat 'with-username) result
                               :test #'string=)))
    (dolist (user (hash-table-values (alist-get 'user telega--info)))
      (setq result (cl-pushnew (telega-user--name user) result
                               :test #'string=)))
    (nreverse result)))

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
START-NODE is node to start from, default is first node.
ITER-FUNC is one of `ewoc--node-next' or `ewoc--node-prev'.
Default is `ewoc--node-next'.
Return EWOC node, nil if not found."
  (unless iter-func
    (setq iter-func #'ewoc--node-next))
  (cl-assert (memq iter-func '(ewoc--node-next ewoc--node-prev)))

  (ewoc--set-buffer-bind-dll-let* ewoc
      ((node (or start-node (ewoc--node-nth dll 1)))
       (stop (if (eq iter-func #'ewoc--node-next)
                 (ewoc--footer ewoc)
               (ewoc--header ewoc)))
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


;; Emoji
(defvar telega-emoji-alist nil)
(defvar telega-emoji-candidates nil)
(defvar telega-emoji-max-length 0)
(defvar telega-emoji-svg-images nil
  "Cache of SVG images for emojis of one char height.
Alist with elements in form (emoji . image)")

(defun telega-emoji-init ()
  "Initialize emojis."
  (unless telega-emoji-alist
    (setq telega-emoji-alist
          (nconc (with-temp-buffer
                   (insert-file-contents (telega-etc-file "emojis.alist"))
                   (goto-char (point-min))
                   (read (current-buffer)))
                 telega-emoji-custom-alist))
    (setq telega-emoji-candidates (mapcar 'car telega-emoji-alist))
    (setq telega-emoji-max-length
          (apply 'max (mapcar 'length telega-emoji-candidates)))))

(defun telega-emoji-name (emoji)
  "Find EMOJI name."
  (telega-emoji-init)
  (car (cl-find emoji telega-emoji-alist :test 'string= :key 'cdr)))

(defun telega-emoji-create-svg (emoji &optional cheight)
  "Create svg image for the EMOJI.
CHEIGHT is height for the svg in characters, default=1."
  (let* ((emoji-cheight (or cheight 1))
         (use-cache-p (and (= 1 (length emoji)) (= emoji-cheight 1)))
         (image (when use-cache-p
                  (cdr (assoc emoji telega-emoji-svg-images)))))
    (unless image
      (let* ((xh (telega-chars-xheight emoji-cheight))
             (font-size (- xh (/ xh 4)))
             (aw-chars (* (or (telega-emoji-svg-width emoji) (length emoji))
                          (telega-chars-in-width (- xh (/ xh 8)))))
             (xw (telega-chars-xwidth aw-chars))
             (svg (telega-svg-create xw xh))
             ;; NOTE: if EMOJI width matches final width, then use
             ;; EMOJI itself as telega-text
             (telega-text (if (= (string-width emoji) aw-chars)
                              emoji
                            (make-string aw-chars ?E))))
        (svg-text svg emoji
                  :font-family telega-emoji-font-family
                  :font-size font-size
                  :x 0 :y font-size)
        (setq image (telega-svg-image svg :scale 1.0
                                      :width xw :height xh
                                      :ascent 'center
                                      :mask 'heuristic
                                      :telega-text telega-text)))
      (when use-cache-p
        (setq telega-emoji-svg-images
              (cons (cons emoji image) telega-emoji-svg-images))))
    image))

(defun telega-emoji-has-zero-joiner-p (emoji)
  "Return non-nil if EMOJI has ZWJ char inside."
  (string-match-p (regexp-quote "\U0000200D") emoji))

(defun telega-emoji-fitz-p (emoji)
  "Return non-nil if EMOJI uses Fitzpatrick's modifier."
  (and (= (length emoji) 2)
       (memq (aref emoji 1) '(?\🏻 ?\🏼 ?\🏽 ?\🏾 ?\🏿))))

(defun telega-emoji-flag-p (emoji)
  "Return non-nil if EMOJI is a flag."
  (and (= (length emoji) 2)
       (>= (aref emoji 0) ?\🇦)
       (>= (aref emoji 1) ?\🇦)
       (<= (aref emoji 0) ?\🇿)
       (<= (aref emoji 1) ?\🇿)))

(defun telega-emoji-fe0f-p (emoji)
  "Return non-nil if EMOJI ends with \ufe0f."
  (and (= (length emoji) 2)
       (= (aref emoji 1) (aref "\ufe0f" 0))))

(defun telega-emoji-svg-width (emoji)
  (if (or (telega-emoji-fitz-p emoji)
          (telega-emoji-flag-p emoji)
          (telega-emoji-fe0f-p emoji)
          (telega-emoji-has-zero-joiner-p emoji))
      1
    nil))


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
  (let* ((start (or pos (point)))
         (end pos))
    (unwind-protect
        (progn
          (save-excursion
            (goto-char start)
            (insert string)
            (setq end (point)))
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
    (unless (string-empty-p png-output)
      (write-region png-output nil tofile nil 'quiet))))

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

(defun telega-read-timestamp (prompt)
  "Interactively read timestamp in the future.
Return timestamp as unix time."
  (interactive)
  (let ((date-time (org-read-date 'with-time t nil prompt)))
    ;; NOTE: we use `apply' to support Emacs 26
    ;; see https://t.me/emacs_telega/14017
    (round (time-to-seconds (apply #'encode-time (decode-time date-time))))))

(defun telega-open-file (filename &optional msg)
  "Open FILENAME inside telega.
MSG is the message associated with FILENAME."
  (let ((saved-buffer (current-buffer)))
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
  (let ((keys-description (mapconcat #'key-description
                                     (where-is-internal command keymap) ", ")))
    (if (string-empty-p keys-description)
        (format "M-x %S RET" command)
      keys-description)))

(defun telega-xdg-open (url)
  "Open URL using \"xdg-open\" utility."
  (unless (zerop (call-process "xdg-open" nil nil nil url))
    (error "Telega: xdg-open failed on %S" url)))

(provide 'telega-util)

;;; telega-util.el ends here

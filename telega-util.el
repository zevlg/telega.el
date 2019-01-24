;;; telega-util.el --- Utility functions for telega

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
(require 'files)                        ; `locate-file'
(require 'rx)                           ; `rx'
(require 'svg)
(require 'color)                        ; `color-XXX'

(require 'telega-customize)
(require 'telega-tme)
(require 'telega-webpage)

(defun telega-browse-url (url &optional in-web-browser)
  "Open the URL.
If URL can be opened directly inside telega, then do it.
Invite links and link to users can be directly opened in telega.
If IN-WEB-BROWSER is non-nil then force opening in web browser."
  (unless (or in-web-browser
              (cond ((string-prefix-p "tg:" url)
                     (telega-tme-open-tg url))
                    ((or (string-prefix-p "https://t.me/" url)
                         (string-prefix-p "https://telegram.me/" url)
                         (string-prefix-p "https://telegram.dog/" url))
                     (telega-tme-open url))
                    (t
                     ;; Try instant view
                     (let ((iv (telega--getWebPageInstantView url)))
                       (when iv
                         (telega-webpage--instant-view url "Telegra.ph" iv)
                         t)))))

    ;; TODO: maybe use webkit x-widget to browse the URL
    (browse-url url)))

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
    filename))

(defun telega-x-frame ()
  "Return window system frame, if any."
  (cl-find-if (lambda (frame)
                (frame-parameter frame 'window-system))
              (cons (window-frame (get-buffer-window (telega-root--buffer)))
                    (frame-list))))

(defun telega-chars-width (n)
  "Return pixel width for N characters"
  (* (frame-char-width (telega-x-frame)) n))

(defun telega-chars-in-width (pixels)
  "Return how many characters needed to cover PIXELS width."
  (ceiling (/ pixels (float (frame-char-width (telega-x-frame))))))

(defun telega-strip-newlines (string)
  "Strip STRING newlines from end and beginning."
  (replace-regexp-in-string
   (rx (or (: string-start (* (any ?\r ?\n)))
           (: (* (any ?\r ?\n)) string-end)))
   ""
   string))

(defun telega-current-column ()
  "Same as `current-column', but take into account width of the characters."
  (string-width (buffer-substring (point-at-bol) (point))))

(defsubst telega-color-to-hex (col)
  (color-rgb-to-hex (car col) (cadr col) (caddr col) 2))

(defun telega-color-random (&optional lightness)
  "Generates random color with lightness below LIGHTNESS.
Default LIGHTNESS is 0.85."
  (telega-color-to-hex
   (color-hsl-to-rgb (cl-random 1.0) (cl-random 1.0)
                     (cl-random (or lightness 0.85)))))

(defun telega-color-gradient (color)
  "For given color return its darker version.
Used to create gradients."
  (telega-color-to-hex
   (mapcar (lambda (c) (/ c 2)) (color-name-to-rgb color))))

;; code taken from
;; https://emacs.stackexchange.com/questions/14420/how-can-i-fix-incorrect-character-width
(defun telega-symbol-set-widths (symbol-widths-alist)
  "Add symbol widths from SYMBOL-WIDTHS-ALIST to `char-width-table'.
Use it if you have formatting issues."
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table)))
  (dolist (pair symbol-widths-alist)
    (let ((width (car pair))
          (symbols (cdr pair))
          (table (make-char-table nil)))
      (dolist (symbol-str symbols)
        (set-char-table-range table (string-to-char symbol-str) width))
      (optimize-char-table table)
      (set-char-table-parent table char-width-table)
      (setq char-width-table table))))

(defun telega-time-seconds ()
  "Return current time as unix timestamp."
  (floor (time-to-seconds)))

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
  (locate-file (concat "etc/" filename) load-path))

(defun telega-link-props (link-type link-to &optional face)
  "Generate props for link button openable with `telega-link--button-action'."
  (cl-assert (memq link-type '(url file user hashtag download cancel-download
                                   upload cancel-upload hashtag)))

  (list 'action 'telega-link--button-action
        'face (or face 'telega-link)
        :telega-link (cons link-type link-to)))

(defun telega-link--button-action (button)
  "Browse url at point."
  (let ((link (button-get button :telega-link)))
    (telega-debug "Action on link: %S" link)
    (cl-ecase (car link)
      (user (with-help-window "*Telegram User Info*"
              (set-buffer standard-output)
              (telega-info--insert-user
               (telega-user--get (cdr link)))))
      (hashtag
       (message "TODO: `hashtag' button action: tag=%s" (cdr link)))
      (url
       (telega-browse-url (cdr link)))
      (file (find-file (cdr link)))

      ;; `link' for download is (PLACE PROP MSG)
      (download
       (telega-file--download-monitoring
        (cadr link) (caddr link)
        'telega-file--update-msg (cadddr link)))
      ;; `link' for cancel-download is FILE-ID
      (cancel-download
       (telega--cancelDownloadFile (cadr link)))

      ;; `link' for upload is (PLACE PROP MSG)
      (upload
       (telega-file--upload-monitoring
        (cadr link) (caddr link)
        'telega-file--update-msg (cadddr link)))
      ;; `link' for cancel-upload is FILE-ID
      (cancel-upload
       (telega--cancelUploadFile (cadr link)))
      )))

(defun telega--entity-to-properties (entity text)
  "Convert telegram ENTITY to emacs text properties to apply to TEXT."
  (let ((ent-type (plist-get entity :type)))
    (cl-case (telega--tl-type ent-type)
      (textEntityTypeMention
       (list 'face 'telega-entity-type-mention))
      (textEntityTypeMentionName
       (telega-link-props 'user (plist-get ent-type :user_id)
                          'telega-entity-type-mention))
      (textEntityTypeHashtag
       (telega-link-props 'hashtag text))
      (textEntityTypeBold
       (list 'face 'telega-entity-type-bold))
      (textEntityTypeItalic
       (list 'face 'telega-entity-type-italic))
      (textEntityTypeCode
       (list 'face 'telega-entity-type-code))
      (textEntityTypePre
       (list 'face 'telega-entity-type-pre))
      (textEntityTypePreCode
       (list 'face 'telega-entity-type-pre))

      (textEntityTypeUrl
       (telega-link-props 'url text 'telega-entity-type-texturl))
      (textEntityTypeTextUrl
       (telega-link-props 'url (plist-get ent-type :url)
                          'telega-entity-type-texturl))
      )))

(defun telega--entities-apply (entities text)
  "Apply telegram ENTITIES to TEXT."
  (mapc (lambda (ent)
          (let* ((beg (plist-get ent :offset))
                 (end (+ (plist-get ent :offset) (plist-get ent :length)))
                 (props (telega--entity-to-properties
                         ent (substring text beg end))))
            (when props
              (add-text-properties beg end props text))))
        entities)
  text)

(defun telega--split-by-text-prop (string prop)
  "Split STRING by property PROP changes."
  (let ((start 0) end result)
    (while (and (> (length string) start)
                (setq end (next-single-char-property-change start prop string)))
      (push (substring string start end) result)
      (setq start end))
    (nreverse result)))

(defun telega--properties-to-entities (text)
  "Convert propertiezed TEXT to telegram ENTITIES."
  ;; TODO: convert text properties to tl text entities
  )

(defun telega--merge-face (start end face &optional object)
  "Merge OBJECT's face property at START and END by adding FACE."
  (let ((val (if (listp face) face (list face))) next prev)
    (while (/= start end)
      (setq next (next-single-property-change start 'face object end)
            prev (get-text-property start 'face object))
      (put-text-property start next 'face
                         (append val (if (listp prev) prev (list prev)))
                         object)
      (setq start next))))

(defun telega-completing-titles ()
  "Return list of titles ready for completing."
  (let ((result))
    (dolist (chat (telega-filter-chats 'all))
      (setq result (cl-pushnew (telega-chat-title chat 'with-username) result
                               :test #'string=)))
    (dolist (user (hash-table-values (cdr (assq 'user telega--info))))
      (setq result (cl-pushnew (telega-user--name user) result
                               :test #'string=)))
    (nreverse result)))


;; ewoc stuff
(defun telega-ewoc--find-node (ewoc predicate)
  "Find EWOC's node by PREDICATE run on node's data."
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((node (ewoc--node-nth dll 1))
       (footer (ewoc--footer ewoc))
       (inhibit-read-only t))
    (cl-block 'ewoc-node-found
      (while (not (eq node footer))
        (when (funcall predicate (ewoc--node-data node))
          (cl-return-from 'ewoc-node-found node))
        (setq node (ewoc--node-next dll node))))))

(defun telega-ewoc--find-node-by-data (ewoc data)
  "Find EWOC's node by its DATA."
  (telega-ewoc--find-node ewoc (lambda (node-data) (eq node-data data))))

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

(defun telega-ewoc--clean (ewoc)
  "Delete all nodes from EWOC.
Header and Footer are not deleted."
  (ewoc-filter ewoc 'ignore))

(provide 'telega-util)

;;; telega-util.el ends here

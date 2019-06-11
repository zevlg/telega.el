;;; telega-core.el --- Core functionality for telega  -*- lexical-binding:t -*-

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Mon Apr 23 18:09:01 2018
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

;; Variables and runtime goodies for telega

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'ring)
(require 'cursor-sensor)

(require 'telega-customize)

(defconst telega-chat-types
  '(private secret basicgroup supergroup bot channel)
  "All types of chats supported by telega.")

;;; Runtime variables
(defvar telega--chat nil
  "Telega chat for the current buffer.
Used in help buffers to refer chat.")
(make-variable-buffer-local 'telega--chat)

(defvar telega--me-id nil "User id of myself.")
(defvar telega--gifbot-id nil "Bot used to search for animations.")
(defvar telega--imgbot-id nil "Bot used to search for photos.")
(defvar telega--options nil "Options updated from telega-server.")
(defvar telega--conn-state nil)
(defvar telega--status "Not Started" "Status of the connection to telegram.")
(defvar telega--status-aux
  "Aux status used for long requests, such as fetching chats/searching/etc")
(defvar telega--chats nil "Hash table (id -> chat) for all chats.")
(defvar telega--actions nil "Hash table (chat-id -> alist-of-user-actions)")
(defvar telega--ordered-chats nil "Ordered list of all chats.")
(defvar telega--filtered-chats nil
  "Chats filtered by currently active filters.
Used to calculate numbers displayed in custom filter buttons.")
(defvar telega--filters nil "List of active filters.")
(defvar telega--undo-filters nil "List of undo entries.")

(defvar telega--info nil "Alist of (TYPE . INFO-TABLE)")
(defvar telega--full-info nil "Alist of (TYPE . FULL-INFO-TABLE)")

(defvar telega--top-chats nil
  "Alist of (CATEGORY LAST-UPDATE-TIME ..)
CATEGORY is one of `Users', `Bots', `Groups', `Channels',
`InlineBots', `Calls'")
(defvar telega--last-buffer nil
  "Track switching buffers, so we can run the code when switching
from chat buffer.")
(defvar telega--stickersets nil
  "Alist of seen sticker sets.
ID -> sticker set.
Take into account that ID is the string.")
(defvar telega--stickersets-installed-ids nil
  "List of installed sticker sets.")
(defvar telega--stickersets-trending nil
  "List of trending sticker sets info.")
(defvar telega--stickers-favorite nil
  "List of favorite stickers.")
(defvar telega--stickers-recent nil
  "List of recently used stickers.")
(defvar telega--stickers-recent-attached nil
  "List of recently attached stickers.")
(defvar telega--animations-saved nil
  "List of saved animations.")

;; Searching
(defvar telega-search-query nil
  "Last search query done by `telega-search'.
Used to continue searching messages.")
(defvar telega-search-history nil
  "List of recent search queries.")

(defvar telega--search-chats nil
  "Result of last `telega--searchChats' or `telega--searchChatsOnServer'.")
(defvar telega--search-contacts nil
  "Result of last `telega--searchContacts'")
(defvar telega--search-global-loading nil
  "Non-nil if globally searching public chats asynchronously.
Actualy value is `:@extra' of the call.")
(defvar telega--search-messages-loading nil
  "Non-nil if searching messages asynchronously.
Actualy value is `:@extra' of the call.")

(defvar telega--logo-image-cache nil "Cached loaded logo image.")
(defvar telega--unread-message-count nil
  "Plist with counts for unread/unmuted messages.
Props are `:unread_count' and `:unread_unmuted_count'")
(defvar telega--unread-chat-count nil
  "Plist with counts for unread/unmuted chats.
Props are `:unread_count', `:unread_unmuted_count', `:marked_as_unread_count'
and `:marked_as_unread_unmuted_count'")

(defvar telega--chat-buffers nil "List of all chat buffers.")
(defvar telega--files nil
  "Files hash FILE-ID -> (list FILE UPDATE-CALBACKS..)")
(defvar telega--files-updates
  "Hash of FILE-ID -> (list-of (UPDATE-CB CB-ARGS))
UPDATE-CB is callback to call when file updates. UPDATE-CB is
called with FILE and CB-ARGS as arguments.
UPDATE-CB should return non-nil to be removed after its being called.")
;; DEPRECATED
(defvar telega--downloadings nil
  "Hash of active downloadings FILE-ID -> (list-of (UPDATE-CB CB-ARGS)).
Where UPDATE-CB is callback to call with FILE and CB-ARGS when file updates.
Used to update messages on file updates.")
;; DEPRECATED
(defvar telega--uploadings nil
  "Hash of active uploadings FILE-ID -> (list of (UPDATE-CB CB-ARGS)).")
(defvar telega--proxy-pings nil
  "Alist for the proxy pings.
(PROXY-ID . TIMESTAMP SECONDS)")
(defvar telega-voip--alist nil
  "Alist of all calls currently in processing.
In form (ID . CALL)")
(defvar telega-voip--active-call nil
  "Currently active call.
Active call is either outgoing call or accepted incoming call.
Only one call can be currently active.")
(defvar telega--scope-notification-alist (cons nil nil)
  "Default notification settings for chats.
alist where key is one of `private', `group' or `channel'.")

;; Minibuffer stuff used by chatbuf and stickers
(defvar telega-minibuffer--choices nil
  "Bind to list of choices.")
(defvar telega-minibuffer--chat nil
  "Bind to chat currently active.")
(defvar telega-minibuffer--string nil
  "Bind to Saved string entered to minibuffer.")

(defvar telega--ignored-messages-ring (make-ring 0)
  "Ring of ignored messages.
Use M-x telega-ignored-messages RET to display the list.")

(defun telega--init-vars ()
  "Initialize runtime variables.
Done when telega server is ready to receive queries."
  (setq telega--conn-state nil)
  (setq telega--status "Disconnected")
  (setq telega--status-aux "")
  (setq telega--me-id -1)
  (setq telega--gifbot-id nil)
  (setq telega--imgbot-id nil)
  (setq telega--options
        ;; default limits
        (list :message_caption_length_max 1024
              :message_text_length_max 4096))
  (setq telega--chats (make-hash-table :test 'eq))
  (setq telega--top-chats nil)

  (setq telega-search-query nil)
  (setq telega--search-chats nil)
  (setq telega--search-contacts nil)
  (setq telega--search-global-loading nil)
  (setq telega--search-messages-loading nil)

  (setq telega--ordered-chats nil)
  (setq telega--filtered-chats nil)
  (setq telega--actions (make-hash-table :test 'eq))
  (setq telega--filters nil)
  (setq telega--undo-filters nil)
  (setq telega--info
        (list (cons 'user (make-hash-table :test 'eq))
              (cons 'secretChat (make-hash-table :test 'eq))
              (cons 'basicGroup (make-hash-table :test 'eq))
              (cons 'supergroup (make-hash-table :test 'eq))))
  (setq telega--full-info
        (list (cons 'user (make-hash-table :test 'eq))
              (cons 'basicGroup (make-hash-table :test 'eq))
              (cons 'supergroup (make-hash-table :test 'eq))))

  (setq telega--ignored-messages-ring
        (make-ring telega-ignored-messages-ring-size))
  (setq telega--unread-message-count nil)
  (setq telega--unread-chat-count nil)

  (setq telega--files (make-hash-table :test 'eq))
  (setq telega--files-updates (make-hash-table :test 'eq))
  (setq telega--downloadings (make-hash-table :test 'eq))
  (setq telega--uploadings (make-hash-table :test 'eq))

  (setq telega-voip--alist nil)
  (setq telega-voip--active-call nil)

  (setq telega--proxy-pings nil)
  (setq telega--scope-notification-alist nil)

  (setq telega--stickersets nil)
  (setq telega--stickersets-installed-ids nil)
  (setq telega--stickersets-trending nil)
  (setq telega--stickers-favorite nil)
  (setq telega--stickers-recent nil)
  (setq telega--stickers-recent-attached nil)
  (setq telega--animations-saved nil)
  )

(defmacro telega-save-excursion (&rest body)
  "Save current point as moving marker."
  (declare (indent 0))
  (let ((pnt-sym (gensym)))
    `(let ((,pnt-sym (copy-marker (point) t)))
       (unwind-protect
           (progn ,@body)
         (goto-char ,pnt-sym)))))

(defmacro telega-save-cursor (&rest body)
  "Execute BODY saving cursor's line and column position."
  (declare (indent 0))
  (let ((line-sym (gensym "line"))
        (col-sym (gensym "col")))
    `(let ((,line-sym (+ (if (bolp) 1 0) (count-lines 1 (point))))
           (,col-sym (current-column)))
       (unwind-protect
           (progn ,@body)
         (goto-char (point-min))
         (cl-assert (> ,line-sym 0))
         (forward-line (1- ,line-sym))
         (move-to-column ,col-sym)))))

(defmacro with-telega-debug-buffer (&rest body)
  "Execute BODY only if `telega-debug' is non-nil, making debug buffer current."
  `(when telega-debug
     (with-current-buffer (get-buffer-create "*telega-debug*")
       (telega-save-excursion
         ,@body))))

(defmacro with-telega-help-win (buffer-or-name &rest body)
  "Execute BODY in help buffer."
  (declare (indent 1))
  `(progn
     (with-help-window ,buffer-or-name)
     (redisplay)
     (with-help-window ,buffer-or-name
       (set-buffer standard-output)
       (cursor-sensor-mode 1)
       ,@body)))

(defsubst telega-debug (fmt &rest args)
  (with-telega-debug-buffer
   (goto-char (point-max))
   (insert (apply 'format (cons (concat fmt "\n") args)))))

(defmacro telega--tl-type (tl-obj)
  `(intern (plist-get ,tl-obj :@type)))

(defmacro telega--tl-get (obj prop1 &rest props)
  "`plist-get' which works with multiple arguments.
For example:
`(telega--tl-get obj :prop1 :prop2)' is equivalent to
`(plist-get (plist-get obj :prop1) :prop2)`"
  (let ((ret `(plist-get ,obj ,prop1)))
    (dolist (prop props)
      (setq ret (list 'plist-get ret prop)))
    ret))

(defmacro telega--tl-prop (prop1 &rest props)
  "Generates function to get property by PROP1 and PROPS.
Uses `telega--tl-get' to obtain the property."
  (let ((tl-obj-sym (cl-gensym "tl-obj")))
    `(lambda (,tl-obj-sym)
       (telega--tl-get ,tl-obj-sym ,prop1 ,@props))))

(defun telega--tl-desurrogate (str)
  "Decode surrogate pairs in STR string.
Attach `display' text property to surrogated regions."
  (dotimes (idx (1- (length str)))
    (let ((high (aref str idx))
          (low (aref str (1+ idx))))
      (when (and (>= high #xD800) (<= high #xDBFF)
                 (>= low #xDC00) (<= low #xDFFF))
        (add-text-properties
         idx (+ idx 2) (list 'display (char-to-string
                                       (+ (lsh (- high #xD800) 10)
                                          (- low #xDC00) #x10000))
                             'telega-desurrogate t)
         str))))
  str)

(defun telega--desurrogate-apply (str)
  "Apply `telega-desurrogate' properties to STR.
Resulting in new string with no surrogate pairs."
  (let ((ret "") (beg 0) (fin (length str)) end)
    (while (setq end (text-property-any beg fin 'telega-desurrogate t str))
      (setq ret (concat ret (substring-no-properties str beg end)
                        (get-text-property end 'display str))
            beg (+ end 2)))
    (concat ret (substring-no-properties str beg end))))

(defsubst telega--tl-unpack (obj)
  "Unpack (i.e. desurrogate strings) object OBJ."
  (cond ((stringp obj) (telega--tl-desurrogate obj))
        ((vectorp obj) (cl-map 'vector 'telega--tl-unpack obj))
        ((listp obj) (mapcar 'telega--tl-unpack obj))
        (t obj)))

(defsubst telega--tl-pack (obj)
  "Pack object OBJ."
  ;; Remove text props from strings, etc
  (cond ((stringp obj) (substring-no-properties obj))
        ((vectorp obj) (cl-map 'vector 'telega--tl-pack obj))
        ((listp obj) (mapcar 'telega--tl-pack obj))
        (t obj)))


;;; Formatting
(defun telega-fmt-eval-fill (estr attrs)
  "Fill ESTR to :fill-column.
Keeps newlines in ESTR.
Return list of strings."
  (let ((fill-column (- (or (plist-get attrs :fill-column) fill-column)
                        (length (plist-get attrs :fill-prefix)))))
    (apply #'nconc
           (mapcar (if (plist-get attrs :fill)
                       (lambda (str)
                         (split-string
                          (with-temp-buffer
                            (insert str)
                            (fill-region (point-min) (point-max)
                                         (plist-get attrs :fill) t)
                            (buffer-substring (point-min) (point-max)))
                          "\n"))
                     #'list)
                   (split-string estr "\n")))))

(defun telega-fmt-eval-truncate (estr attrs)
  (let* ((max (plist-get attrs :max))
         ;; NOTE: always apply elide
;         (elide (plist-get attrs :elide))
         (elide-str (or (plist-get attrs :elide-string) telega-symbol-eliding))
         (elide-trail (or (plist-get attrs :elide-trail) 0))
         (estr-trail (if (> elide-trail 0) (substring estr (- elide-trail)) ""))
         (estr-lead (truncate-string-to-width
                     estr (- max (string-width elide-str) elide-trail))))
    (concat estr-lead elide-str estr-trail)))
    ;;      result)
    ;; ;; Correct truncstr in case of multibyte chars
    ;; (while (and (not (string-empty-p estr-lead))
    ;;             (< max (string-width
    ;;                     (setq result (concat estr-lead elide-str estr-trail)))))
    ;;   (setq estr-lead (substring estr-lead 0 -1)))

    ;; result))

(defun telega-fmt-eval-align (estr attrs)
  (let* ((min (plist-get attrs :min))
         (width (- min (string-width estr)))
         (align (plist-get attrs :align))
         (align-symbol (or (plist-get attrs :align-symbol) " "))
         (left "")
         (right ""))
    ;; Grow `left' and `right' until they have required width
    (while (< (string-width left) (/ width 2))
      (setq left (concat left align-symbol)))
    (while (< (string-width right) (- width (/ width 2)))
      (setq right (concat right align-symbol)))

    (cl-ecase align
      (left (concat estr left right))
      (right (concat left right estr))
      ((center centre) (concat left estr right)))))

(defun telega-fmt-eval-min-max (estr attrs)
  "Apply `:min' and `:max' properties to ESTR."
  (let ((max (plist-get attrs :max))
        (min (plist-get attrs :min))
        (estr-width (string-width estr)))
    (cond ((and max (> estr-width max))
           (telega-fmt-eval-truncate estr attrs))
          ((and min (< estr-width min))
           (telega-fmt-eval-align estr attrs))
          (t estr))))

(defun telega-fmt-eval-face (estr attrs)
  "Apply `:face' attribute to ESTR."
  (let ((face (plist-get attrs :face)))
    (when face
      (add-face-text-property 0 (length estr) face t estr))
    estr))

(defun telega-fmt-eval-attrs (estr attrs)
  "Apply all attributes to ESTR."
  ;; Blackmagic for fast execution, but
  ;; NOTE:
  ;;  - Do not prefix first line
  ;;  - Do not prefix empty lines with blank prefix
  ;;  - If last string is empty, do not prefix it
  (let* ((fpx (plist-get attrs :fill-prefix))
         (fpx-blank-p (or (not fpx) (string-blank-p fpx)))
         (filled-estrs (telega-fmt-eval-fill estr attrs))
         (formatted-estrs (list (telega-fmt-eval-min-max
                                 (pop filled-estrs) attrs)))
         (festr-tail formatted-estrs))
    (while filled-estrs
      (let* ((estr (car filled-estrs))
             (estr-last-p (not (cdr filled-estrs)))
             (festr-elem
              (list (telega-fmt-eval-min-max
                     (concat (unless (and (string-empty-p estr)
                                          (or estr-last-p fpx-blank-p))
                               fpx)
                             estr)
                     attrs))))
        (setcdr festr-tail festr-elem)
        (setq festr-tail festr-elem)
        (setq filled-estrs (cdr filled-estrs))))
    (telega-fmt-eval-face
     (mapconcat #'identity formatted-estrs "\n")
     attrs)))

(defsubst telega-fmt-atom (atom)
  "Convert ATOM to string.
NIL yields empty string for the convenience."
  (cond ((stringp atom) atom)
        ((null atom) "")
        (t (with-output-to-string
             (princ atom)))))

(defun telega-fmt-eval-elem (elem value)
  "Format single element ELEM."
  (let (attrs)
    (when (and (not (functionp elem)) (listp elem))
      (setq attrs (cdr elem)
            elem (car elem)))

    (telega-fmt-eval-attrs
     (cond ((functionp elem)
            (telega-fmt-atom (funcall elem value)))
           ((symbolp elem)
            (telega-fmt-atom (symbol-value elem)))
           ((listp elem)
            (telega-fmt-eval elem value))
           (t (telega-fmt-atom elem)))
     attrs)))

(defun telega-fmt-eval (fmt-spec value)
  "Evaluate simple format FMT-SPEC, applying it to VALUE."
  (when (functionp fmt-spec)
    (setq fmt-spec (funcall fmt-spec value)))

  (let ((fmt-result (if (stringp fmt-spec) fmt-spec "")))
    (while (consp fmt-spec)
      (when (car fmt-spec)
        (setq fmt-result
              (concat fmt-result
                      (telega-fmt-eval-elem (car fmt-spec) value))))
      (setq fmt-spec (cdr fmt-spec)))
    fmt-result))

(defsubst telega--time-at00 (timestamp &optional decoded-ts)
  "Return time at 00:00:001 at TIMESTAMP's day."
  (let ((dt (or decoded-ts (decode-time timestamp))))
    (1+ (- timestamp (* 3600 (nth 2 dt)) (* 60 (nth 1 dt)) (nth 0 dt)))))

;; DEPRECATED
(defun telega-fmt-timestamp (timestamp)
  "Format unix TIMESTAMP to human readable form."
  (telega-ins--as-string
   (telega-ins--date timestamp)))

;;; Buttons for telega
(defun telega-button--ins-error (_val)
  (error "Button `:inserter' is unset."))

;; Make 'telega-button be separate (from 'button) type
(put 'telega-button 'type 'telega)
(put 'telega-button 'keymap button-map)
(put 'telega-button 'action 'ignore)
(put 'telega-button 'rear-nonsticky t)
(put 'telega-button 'face nil)
(put 'telega-button :inserter 'telega-button--ins-error)
(put 'telega-button :value nil)
(put 'telega 'button-category-symbol 'telega-button)

(defun telega-button--sensor-func (_window oldpos dir)
  "Function to be used in `cursor-sensor-functions' text property.
Activates button if cursor enter, deactivates if leaves."
  (let ((inhibit-read-only t)
        (button-region (telega--region-with-cursor-sensor
                        (if (eq dir 'entered) (point) oldpos))))
    (when button-region
      (put-text-property (car button-region) (cdr button-region)
                         'face (if (eq dir 'entered)
                                   'telega-button-active
                                 'telega-button))
      (when (eq dir 'entered)
        (telega-button--help-echo (car button-region)))
      )))

;; `:help-echo' is also available for buttons
(defun telega-button--help-echo (button)
  "Show help message for BUTTON defined by `:help-echo' property."
  (let ((help-echo (button-get button :help-echo)))
    (when (functionp help-echo)
      (setq help-echo (funcall help-echo (button-get button :value))))
    (when help-echo
      (message "%s" (eval help-echo)))))

(defun telega-button--insert (button-type value &rest props)
  "Insert telega button of BUTTON-TYPE with VALUE and PROPS."
  (declare (indent 2))
  (let ((button (apply 'make-text-button
                       (prog1 (point)
                         (funcall (or (plist-get props :inserter)
                                      (button-type-get button-type :inserter))
                                  value))
                       (point)
                       :type button-type
                       :value value
                       props)))
    (button-at button)))

(defmacro telega-button--change (button new-button)
  "Change BUTTON to NEW-BUTTON."
  (declare (indent 1))
  (let ((newbutton (gensym "newbutton")))
    `(let ((inhibit-read-only t))
       (goto-char (button-start ,button))
       (let ((,newbutton ,new-button))
         (delete-region (point) (button-end ,button))
         (set-marker ,button ,newbutton)))))

(defun telega-button--update-value (button new-value &rest props)
  "Update BUTTON's value to VALUE.
Renews the BUTTON."
  (telega-button--change button
    (apply 'telega-button--insert (button-type button) new-value props)))

(defun telega-button--observable-p (button)
  "Return non-nil if BUTTON is observable in some window.
I.e. shown in some window, see `pos-visible-in-window-p'."
  (when (markerp button)
    (let* ((bwin (get-buffer-window (marker-buffer button)))
           (bframe (window-frame bwin)))
      (and bframe
           ;; NOTE: 26.1 Emacs has no `frame-focus-state'
           (or (not (fboundp 'frame-focus-state))
               (frame-focus-state bframe))
           (pos-visible-in-window-p button bwin)))))

(defun telega-button-forward (n &optional button-type)
  "Move forward to N visible/active button.
If BUTTON-TYPE is specified, then forward only buttons of BUTTON-TYPE."
  (interactive "p")
  (let (button)
    (dotimes (_ (abs n))
      (while (and (setq button (forward-button (cl-signum n)))
                  (or (and button-type
                           (not (eq (button-type button) button-type)))
                      (button-get button 'invisible)
                      (button-get button 'inactive)))))
    (telega-button--help-echo button)
    button))

(defun telega-button-backward (n &optional button-type)
  "Move backward to N visible/active button.
If BUTTON-TYPE is specified, then forward only buttons of BUTTON-TYPE."
  (interactive "p")
  (telega-button-forward (- n) button-type))

(provide 'telega-core)

;;; telega-core.el ends here

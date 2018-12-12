;;; telega-core.el --- Core functionality for telega.

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

(require 'telega-customize)

(defconst telega-chat-types
  '(private secret basicgroup supergroup bot channel)
  "All types of chats supported by telega.")

;;; Runtime variables
(defvar telega--me-id nil "User id of myself.")
(defvar telega--options nil "Options updated from telega-server.")
(defvar telega--status "Not Started" "Status of the connection to telegram.")
(defvar telega--chats nil "Hash table (id -> chat) for all chats.")
(defvar telega--actions nil "Hash table (chat-id -> alist-of-user-actions)")
(defvar telega--ordered-chats nil "Ordered list of all chats.")
(defvar telega--filtered-chats nil
  "Chats filtered by currently active filters.")
(defvar telega--filters nil "List of active filters.")
(defvar telega--undo-filters nil "List of undo entries.")

(defvar telega--info nil "Alist of (TYPE . INFO-TABLE)")
(defvar telega--full-info nil "Alist of (TYPE . FULL-INFO-TABLE)")

(defvar telega--logo-image-cache nil "Cached loaded logo image.")
(defvar telega--unread-count 0 "Total number of unread messages.")
(defvar telega--unread-unmuted-count 0 "Total number of unread/unmuted messages.")
(defvar telega--unread-message-count nil "Plist with counts for unread/unmuted messages.")
(defvar telega--unread-chat-count nil "Plist with counts for unread/unmuted chats.")

(defvar telega--chat-buffers nil "List of all chat buffers.")
(defvar telega--downloadings nil
  "Hash of active downloadings FILE-ID -> (list-of (UPDATE-CB CB-ARGS)).
Where UPDATE-CB is callback to call with FILE and CB-ARGS when file updates.
Used to update messages on file updates.")
(defvar telega--uploadings nil
  "Hash of active uploadings FILE-ID -> (list of (UPDATE-CB CB-ARGS)).")

(defun telega--init-vars ()
  "Initialize runtime variables.
Done when telega server is ready to receive queries."
  (setq telega--me-id -1)
  (setq telega--options nil)
  (setq telega--chats (make-hash-table :test 'eq))
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

  (setq telega--downloadings (make-hash-table :test 'eq))
  (setq telega--uploadings (make-hash-table :test 'eq)))

(defmacro telega-save-excursion (&rest body)
  "Save current point as moving marker."
  (let ((pnt-sym (gensym)))
    `(let ((,pnt-sym (copy-marker (point) t)))
       (unwind-protect
           (progn ,@body)
         (goto-char ,pnt-sym)))))
(put 'telega-save-excursion 'lisp-indent-function 0)

(defmacro telega-save-cursor (&rest body)
  "Execute BODY saving cursor's line and column position."
  (let ((line-sym (gensym "line"))
        (col-sym (gensym "col")))
    `(let ((,line-sym (+ (if (bolp) 1 0) (count-lines 1 (point))))
           (,col-sym (current-column)))
       (unwind-protect
           (progn ,@body)
         (goto-line ,line-sym)
         (move-to-column ,col-sym)))))
(put 'telega-save-cursor 'lisp-indent-function 0)

(defmacro with-telega-debug-buffer (&rest body)
  "Execute BODY only if `telega-debug' is non-nil, making debug buffer current."
  `(when telega-debug
     (with-current-buffer (get-buffer-create "*telega-debug*")
       (telega-save-excursion
         ,@body))))

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

(defmacro telega-x-frame ()
  "Return any window system frame, if any."
  (let ((fsym (gensym "f")))
    `(cl-find-if (lambda (,fsym)
                   (frame-parameter ,fsym 'window-system))
                 (cons (selected-frame) (frame-list)))))

(defmacro telega-prefix (prefix str)
  "Concatenate PREFIX and STR in case STR is non empty."
  (let ((strsym (gensym "str")))
    `(let ((,strsym ,str))
       (when (and ,strsym (not (string-empty-p ,strsym)))
         (concat ,prefix ,strsym)))))

(defun telega-short-filename (filename)
  "Shortens FILENAME by removing `telega-cache-dir' prefix."
  (if (and telega-use-short-filenames
           (string-prefix-p telega-cache-dir filename))
      (substring filename (length telega-cache-dir))
    filename))
  

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
         (elide (plist-get attrs :elide))
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

(defun telega-fmt-eval-fill-prefix (estr attrs)
  (concat (or (plist-get attrs :fill-prefix) "") estr))

(defun telega-fmt-eval-face (estr attrs)
  "Apply `:face' attribute to ESTR."
  (let ((face (plist-get attrs :face)))
    (when face
      (telega--merge-face 0 (length estr) face estr))
    estr))

(defun telega-fmt-eval-attrs (estr attrs)
  "Apply all attributes to ESTR."
  (let ((formatted-estrs
         (mapcar (lambda (estrline)
                   (telega-fmt-eval-min-max
                    (telega-fmt-eval-fill-prefix estrline attrs)
                    attrs))
                 (telega-fmt-eval-fill estr attrs))))
    ;; NOTE: strip prefix on the first line
    (telega-fmt-eval-face
     (mapconcat #'identity
                (cons (substring (car formatted-estrs)
                                 (length (plist-get attrs :fill-prefix)))
                      (cdr formatted-estrs)) "\n")
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

(defun telega-fmt-timestamp (timestamp)
  "Format unix TIMESTAMP to human readable form."
  ;; - HH:MM      if today
  ;; - Mon/Tue/.. if on this week
  ;; - DD.MM.YY   otherwise
  (let* ((dtime (decode-time timestamp))
         (current-ts (time-to-seconds (current-time)))
         (ctime (decode-time current-ts))
         (today00 (telega--time-at00 current-ts ctime)))
    (if (> timestamp today00)
        (format "%02d:%02d" (nth 2 dtime) (nth 1 dtime))

      (let* ((week-day (nth 6 ctime))
             (mdays (+ week-day
                       (- (if (< week-day telega-week-start-day) 7 0)
                          telega-week-start-day)))
             (week-start00 (telega--time-at00
                            (- current-ts (* mdays 24 3600)))))
        (if (> timestamp week-start00)
            (nth (nth 6 dtime) telega-week-day-names)

          (format "%02d.%02d.%02d"
                  (nth 3 dtime) (nth 4 dtime) (- (nth 5 dtime) 2000))))
      )))

(defun telega-fmt-timestamp-iso8601 (timestamp)
  (format-time-string "%FT%T%z" timestamp))

(defun telega-fmt-labeled-text (label text &optional fill-col)
  "Format TEXT filling it, prefix with LABEL."
  (telega-fmt-eval
   `(,label (identity :fill left
                      :fill-prefix ,(make-string (length label) ?\s)
                      :fill-column ,fill-col))
   text))

(defun telega-fmt-chat-member-status (status)
  "Format chat member STATUS."
  (cl-case (telega--tl-type status)
    (chatMemberStatusCreator " (creator)")
    (chatMemberStatusAdministrator " (admin)")
    (chatMemberStatusBanned " (banned)")
    (chatMemberStatusRestricted " (restricted)")
    (chatMemberStatusLeft " (left)")
    (t "")))

(defun telega-fmt-chat-member (member)
  "Formatting for the chat MEMBER."
  (let ((user (telega-user--get (plist-get member :user_id)))
        (joined (plist-get member :joined_chat_date)))
    (list
     (telega-user--name user)
     (telega-fmt-chat-member-status (plist-get member :status))
     (unless (zerop joined)
       (concat " joined at " (telega-fmt-timestamp joined))))))

;;; Buttons for telega

(defun telega-button--format-error (msg)
  (error "Button `:format' is unset."))

(defun telega-button--ins-error (val)
  (error "Button `:inserter' is unset."))

;; Make 'telega-button be separate (from 'button) type
(put 'telega-button 'type 'telega)
(put 'telega-button 'keymap button-map)
(put 'telega-button 'action 'ignore)
(put 'telega-button 'rear-nonsticky t)
(put 'telega-button :format 'telega-button--format-error)
(put 'telega-button :inserter 'telega-button--ins-error)
;; Function that returns additional properties for the button
(put 'telega-button :button-props-func 'ignore)
(put 'telega-button :value nil)
(put 'telega 'button-category-symbol 'telega-button)

(defun telega-button--apply-props-func (button)
  "Apply runtime BUTTON properties."
  (let ((props (funcall (button-get button :button-props-func)
                        (button-get button :value))))
    (cl-loop for (prop val) on props by 'cddr
             do (button-put button prop val))))

(defun telega-button--insert (button-type value)
  "Insert telega button of BUTTON-TYPE with VALUE."
  (let ((button (make-text-button
                 (prog1 (point)
                   (funcall (button-type-get button-type :inserter) value))
                 (point)
                 :type button-type
                 :value value)))
    (telega-button--apply-props-func button)
    button))

(defun telega-button--update-value (button new-value)
  "Update BUTTON's value to VALUE.
Renews the BUTTON."
  (let ((inhibit-read-only t)
        (button-type (button-type button)))
    (save-excursion
      (goto-char (button-start button))
      (delete-region (button-start button) (button-end button))
      (let ((cpnt (point)))
        (telega-button--insert button-type new-value)
        (set-marker button cpnt)))))

(defun telega-button-move (button point &optional new-label)
  "Move BUTTON to POINT location."
    (unless new-label
      (setq new-label
            (buffer-substring (button-start button) (button-end button))))
    (goto-char point)
    (telega-button-delete button)
    (let ((cpnt (point)))
      (insert new-label)
      (set-marker button cpnt)))

(defun telega-button-properties (button)
  "Return all BUTTON properties specific for this type of buttons."
  (let ((text-props (text-properties-at button))
        (type-plist (symbol-plist
                     (button-category-symbol (button-type button)))))
    (cl-loop for (key _) on text-props by 'cddr
             if (or (plist-member type-plist key)
                    (memq key '(button category)))
             nconc (list key (plist-get text-props key)))))

(defmacro telega-button-foreach0 (advance-func butt-type args &rest body)
  "Run point accross all buttons of BUTTON-TYPE.
Run BODY for each found point.
Use `cl-block' and `cl-return' to prematurely stop the iteration.
Run under `save-excursion' to preserve point."
  (let ((button (car args)))
    `(let ((,button (button-at (point))))
       (while ,button
         (goto-char ,button)
         (when (eq (button-type ,button) ,butt-type)
           ,@body)
         (setq ,button (,advance-func ,button))))))
(put 'telega-button-foreach0 'lisp-indent-function 'defun)

(defmacro telega-button-foreach (butt-type args &rest body)
  "Run point accross all buttons of BUTTON-TYPE.
Run BODY for each found point.
Use `cl-block' and `cl-return' to prematurely stop the iteration.
Run under `save-excursion' to preserve point."
  `(telega-button-foreach0 next-button ,butt-type ,args ,@body))
(put 'telega-button-foreach 'lisp-indent-function 'defun)

(defun telega-button-find (butt-type &rest value)
  "Find button by BUTT-TYPE and its VALUE.
If VALUE is not specified, then find fist one button of BUTT-TYPE."
  (cl-block 'button-found
    (telega-button-foreach butt-type (button)
      (when (or (null value) (eq (button-get button :value) (car value)))
        (cl-return-from 'button-found button)))))

(defun telega-button-insert (button-type &rest props)
  "Insert button of BUTTON-TYPE with properties PROPS.
Return newly created button."
  (let ((value (plist-get props :value))
        (button-fmt (or (plist-get props :format)
                        (button-type-get button-type :format))))
    (button-at
     (apply #'insert-text-button
            (telega-fmt-eval button-fmt value)
            :type button-type props))))
(put 'telega-button-insert 'lisp-indent-function 'defun)

(defun telega-button-delete (button)
  "Delete the BUTTON."
  (let ((inhibit-read-only t))
    (delete-region (button-start button) (button-end button))))

(defun telega-button-move (button point &optional new-label)
  "Move BUTTON to POINT location."
  (let ((inhibit-read-only t))
    (unless new-label
      (setq new-label
            (buffer-substring (button-start button) (button-end button))))
    (goto-char point)
    (telega-button-delete button)
    (let ((cpnt (point)))
      (insert new-label)
      (set-marker button cpnt))))

(defun telega-button--redisplay (button)
  "Redisplay the BUTTON contents."
  (telega-button-move button (button-start button)
                      (apply #'propertize
                             (telega-fmt-eval (button-get button :format)
                                              (button-get button :value))
                             (telega-button-properties button))))

(defun telega-button--observable-p (button)
  "Return non-nil if BUTTON is observable in some window.
I.e. shown in some window, see `pos-visible-in-window-p'."
  (and (markerp button)
       (get-buffer-window (marker-buffer button))
       (pos-visible-in-window-p
        button (get-buffer-window (marker-buffer button)))))

(defun telega-button-forward (n)
  "Move forward to N visible/active button."
  (interactive "p")
  (let (button)
    (dotimes (_ (abs n))
      (while (and (setq button (forward-button (cl-signum n)))
                  (or (button-get button 'invisible)
                      (button-get button 'inactive)))))
    (when (= (following-char) ?\[)
      (forward-char 1))

    (when (button-get button :help-format)
      (message (telega-fmt-eval (button-get button :help-format)
                                (button-get button :value))))
    button))

(defun telega-button-backward (n)
  "Move backward to N visible/active button."
  (interactive "p")
  (telega-button-forward (- n)))

(provide 'telega-core)

;;; telega-core.el ends here

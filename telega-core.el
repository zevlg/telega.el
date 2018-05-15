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
(eval-when-compile
  (require 'cl)) ;; for defsetf

(defconst telega-chat-types
  '(private secret basicgroup supergroup bot channel)
  "All types of chats supported by telega.")

;;; Runtime variables
(defvar telega--options nil "Options updated from telega-server.")
(defvar telega--status "Not Started" "Status of the connection to telegram.")
(defvar telega--chats nil "Hash table (id -> chat) for all chats.")
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

(defvar telega--chat-buffers nil "List of all chat buffers.")

(defun telega--init-vars ()
  "Initialize runtime variables.
Done when telega server is ready to receive queries."
  (setq telega--options nil)
  (setq telega--chats (make-hash-table :test 'eq))
  (setq telega--ordered-chats nil)
  (setq telega--filtered-chats nil)
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
  )

(defmacro with-telega-debug-buffer (&rest body)
  "Execute BODY only if telega-debug is enabled making debug buffer current."
  `(when telega-debug
     (with-current-buffer (get-buffer-create "*telega-debug*")
       ,@body)))

(defsubst telega-debug (fmt &rest args)
  (with-telega-debug-buffer
      (goto-char (point-max))
      (insert (apply 'format (cons (concat fmt "\n") args)))))

(defmacro telega--tl-type (tl-obj)
  `(intern (plist-get ,tl-obj :@type)))
(defsetf telega--tl-type (tl-obj) (type-sym)
  `(plist-put ,tl-obj :@type (symbol-name ',type-sym)))

(defmacro telega--tl-bool (tl-obj prop)
  `(eq t (plist-get ,tl-obj ,prop)))

(defmacro telega--tl-prop (prop)
  "Generates function to get property PROP."
  (let ((tl-obj-sym (cl-gensym "tl-obj")))
    `(lambda (,tl-obj-sym)
       (plist-get ,tl-obj-sym ,prop))))


(defmacro telega-fill-single (prefix str)
  `(with-temp-buffer
     (insert ,prefix ,str)
     (fill-region (point-min) (point-max) 'left t t)
     (buffer-substring (point-min) (point-max))))

(defun telega-fill-string (title str)
  (let ((strings (split-string str "\n"))
        (spaces-prefix (make-string (length title) ?\s)))
    (mapconcat #'identity
               (cons (let ((fill-prefix spaces-prefix))
                       (telega-fill-single title (car strings)))
                     (mapcar (lambda (str)
                               (telega-fill-single spaces-prefix str))
                             (cdr strings)))
               "\n")))

;;; Formatting
(defsubst telega-fmt-to-string (elemval)
  "Convert element value ELEMVAL to string."
  (if (stringp elemval)
      elemval
    (with-output-to-string
      (princ elemval))))

(defun telega-fmt-eval-fill (estr &rest attrs)
  "Fill ESTR to :fill-column.
Keeps newlines in ESTR.
Return list of strings."
  (apply #'nconc
         (mapcar (if (plist-get attrs :fill)
                     (let* ((fill-column (or (plist-get attrs :fill-column)
                                             fill-column))
                            (prfx-len (length (plist-get attrs :fill-prefix)))
                            (prfx (make-string prfx-len ?A)))
                       `(lambda (str)
                          (split-string
                           (substring
                            (telega-fill-single ,prfx str) ,prfx-len)
                           "\n")))
                   #'list)
                 (split-string estr "\n"))))

(defun telega-fmt-eval-truncate (estr &rest attrs)
  (let* ((max (plist-get attrs :max))
         (elide (plist-get attrs :elide))
         (elide-str (or (plist-get attrs :elide-string) telega-eliding-string))
         (elide-trail (or (plist-get attrs :elide-trail) 0))
         (estr-trail (if (> elide-trail 0) (substring estr (- elide-trail)) ""))
         (estr-lead (substring estr 0 (- max (length elide-str) elide-trail)))
         result)
    ;; Correct truncstr in case of multibyte chars
    (while (and (not (string-empty-p estr-lead))
                (< max (string-width
                        (setq result (concat estr-lead elide-str estr-trail)))))
      (setq estr-lead (substring estr-lead 0 -1)))

    result))

(defun telega-fmt-eval-align (estr &rest attrs)
  (let* ((min (plist-get attrs :min))
         (width (- min (string-width estr)))
         (align (plist-get attrs :align))
         (align-char (or (plist-get attrs :align-char) ?\s))
         (left (make-string (/ width 2) align-char))
         (right (make-string (- width (/ width 2)) align-char)))
    (ecase align
      (left (concat estr left right))
      (right (concat left right estr))
      ((center centre) (concat left estr right)))))

(defun telega-fmt-eval-min-max (estr &rest attrs)
  "Apply `:min' and `:max' properties to ESTR."
  (let ((max (plist-get attrs :max))
        (min (plist-get attrs :min))
        (estr-width (string-width estr)))
    (cond ((and max (> estr-width max))
           (apply #'telega-fmt-eval-truncate estr attrs))
          ((and min (< estr-width min))
           (apply #'telega-fmt-eval-align estr attrs))
          (t estr))))

(defun telega-fmt-eval-fill-prefix (estr &rest attrs)
  (concat (or (plist-get attrs :fill-prefix) "") estr))

(defun telega-fmt-eval-face (estr &rest attrs)
  "Apply `:face' attribute to ESTR."
  (let ((face (plist-get attrs :face)))
    (if face
        (propertize estr 'face face)
      estr)))

(defun telega-fmt-eval-attrs (estr &rest attrs)
  "Apply all attributes to ESTR."
  (let ((eval-fill-prefix nil))
    (mapconcat (lambda (estr)
                 (let ((res (apply #'telega-fmt-eval-min-max estr attrs)))
                   (if eval-fill-prefix
                       (setq res (apply #'telega-fmt-eval-fill-prefix res attrs))
                     (setq eval-fill-prefix t))
                   (apply #'telega-fmt-eval-face res attrs)))
               (apply #'telega-fmt-eval-fill estr attrs) "\n")))

(defun telega-fmt-eval-elem (elem &rest value)
  "Format single element ELEM."
  (let (attrs)
    (when (and (not (functionp elem)) (listp elem))
      (setq attrs (cdr elem)
            elem (car elem)))

    (apply #'telega-fmt-eval-attrs
           (cond ((functionp elem)
                  (telega-fmt-to-string (apply elem value)))
                 ((symbolp elem)
                  (telega-fmt-to-string (symbol-value elem)))
                 ((listp elem)
                  (apply #'telega-fmt-eval elem value))
                 (t (telega-fmt-to-string elem)))
           attrs)))

(defun telega-fmt-eval (fmt-simple &rest value)
  "Evaluate simple format FMT-SIMPLE, applying it to VALUE."
  (when (functionp fmt-simple)
    (setq fmt-simple (apply fmt-simple value)))

  (let ((fmt-result (if (stringp fmt-simple) fmt-simple "")))
    (while (consp fmt-simple)
      (when (car fmt-simple)
        (setq fmt-result
              (concat fmt-result
                      (apply #'telega-fmt-eval-elem (car fmt-simple) value))))
      (setq fmt-simple (cdr fmt-simple)))
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

;;; Buttons for telega

;; Make 'telega-button be separate (from 'button) type
(put 'telega-button 'keymap button-map)
(put 'telega-button 'type 'telega)
(put 'telega-button 'action 'ignore)
(put 'telega-button 'rear-nonsticky t)
(put 'telega 'button-category-symbol 'telega-button)

(defmacro telega-button-foreach (butt-type args &rest body)
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
         (setq ,button (next-button ,button))))))
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
  (delete-region (button-start button) (button-end button)))

(defun telega-button-move (button point &optional new-label)
  "Move BUTTON to POINT location."
  (unless new-label
    (setq new-label
          (buffer-substring (button-start button) (button-end button))))
  (goto-char point)
  (telega-button-delete button)
  (save-excursion
    (insert new-label))
  (set-marker button (point)))

(defun telega-button--redisplay (button)
  "Redisplay the BUTTON contents."
  (telega-button-move button (button-start button)
                      (apply #'propertize 
                             (telega-fmt-eval (button-get button :format)
                                              (button-get button :value))
                             (text-properties-at button))))

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

(defun telega-completing-titles ()
  "Return list of titles ready for completing."
  (let ((result))
    (dolist (chat telega--ordered-chats)
      (setq result (pushnew (telega-chat--title chat 'withusername) result
                            :test #'string=)))
    (dolist (user (hash-table-values (cdr (assq 'user telega--full-info))))
      (setq result (pushnew (telega-user--title user 'withusername) result
                            :test #'string=)))
    (nreverse result)))
    
(provide 'telega-core)

;;; telega-core.el ends here

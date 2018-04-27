;;; telega-core.el --- Core functionality for telega.

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Mon Apr 23 18:09:01 2018
;; Keywords: 

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

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
(defvar telega--users nil "Hash table (id -> user) for all users.")
(defvar telega--filters nil "List of active filters.")
(defvar telega--undo-filters nil "List of undo entries.")
(defvar telega--full-info nil "Alist of (TYPE . FULL-INFO-TABLE)")

(defun telega--init-vars ()
  "Initialize runtime variables.
Done when telega server is ready to receive queries."
  (setq telega--options nil)
  (setq telega--chats (make-hash-table :test 'eq))
  (setq telega--users (make-hash-table :test 'eq))
  (setq telega--ordered-chats nil)
  (setq telega--filtered-chats nil)
  (setq telega--filters (list (list telega-filter-default)))
  (setq telega--undo-filters nil)
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
  `(not (eq (plist-get ,tl-obj ,prop) ,json-false)))

(defmacro telega--tl-prop (prop)
  "Generates function to get property PROP."
  (let ((tl-obj-sym (cl-gensym "tl-obj")))
    `(lambda (,tl-obj-sym)
       (plist-get ,tl-obj-sym ,prop))))

(defun telega--replace-region (from to rep)
  "Replace region FROM TO by REP."
  (save-excursion
    (goto-char to)
    (insert rep)
    (delete-region from to)))


;;; Formatting
(defun telega-fmt-truncate (estr &rest attrs)
  (let* ((max (plist-get attrs :max))
         (elide (plist-get attrs :elide))
         (elide-str (or (plist-get attrs :elide-string) telega-eliding-string))
         (elide-trail (or (plist-get attrs :elide-trail) 0))
         (trunstr (concat (substring estr 0 (- max (length elide-str) elide-trail))
                          elide-str)))
    (when (> elide-trail 0)
      (setq trunstr (concat trunstr (substring estr (- elide-trail)))))
    trunstr))

(defun telega-fmt-align (estr &rest attrs)
  (let* ((min (plist-get attrs :min))
         (width (- min (length estr)))
         (align (plist-get attrs :align))
         (align-char (or (plist-get attrs :align-char) ?\s))
         (left (make-string (/ width 2) align-char))
         (right (make-string (- width (/ width 2)) align-char)))
    (ecase align
      (left (concat estr left right))
      (right (concat left right estr))
      ((center centre) (concat left estr right)))))

(defun telega-fmt-eval-attrs (estr &rest attrs)
  (let* ((max (plist-get attrs :max))
         (min (plist-get attrs :min))
         (ret-str (cond ((and max (> (length estr) max))
                         (apply #'telega-fmt-truncate estr attrs))
                        ((and min (< (length estr) min))
                         (apply #'telega-fmt-align estr attrs))
                        (t estr)))
         (face (plist-get attrs :face)))
    (if face
        (propertize ret-str 'face face)
      ret-str)))

(defun telega-fmt-eval-elem (elem &rest value)
  "Format single element ELEM."
  (let (attrs)
    (when (listp elem)
      (setq attrs (cdr elem)
            elem (car elem)))

    (apply #'telega-fmt-eval-attrs
           (cond ((stringp elem) elem)
                 ((numberp elem) (number-to-string elem))
                 ((functionp elem)
                  (with-output-to-string
                    (princ (apply elem value))))
                 ((symbolp elem)
                  (with-output-to-string
                    (princ (symbol-value elem))))
                 ((listp elem)
                  (apply #'telega-fmt-eval elem value))
                 (t (error "Can't format ELEM: %s" elem)))
           attrs)))

(defun telega-fmt-eval (fmt-simple &rest value)
  (when (functionp fmt-simple)
    (setq fmt-simple (apply fmt-simple value)))
  (mapconcat (lambda (elem)
               (apply #'telega-fmt-eval-elem elem value))
             (cl-remove-if #'null fmt-simple) ""))

;;; Buttons for telega

(defmacro telega-button-foreach (butt-type args &rest body)
  "Run point accross all buttons of BUTTON-TYPE.
Run BODY for each found point.
Use `cl-block' and `cl-return' to prematurely stop the iteration.
Run under `save-excursion' to preserve point."
  (let ((button (car args)))
    `(let ((,button (point)))
       (while ,button
         (goto-char ,button)
         (when (eq (button-type ,button) ,butt-type)
           ,@body)
         (setq ,button (next-button ,button))))))
(put 'telega-button-foreach 'lisp-indent-function 'defun)

(defun telega-button-find (butt-type &optional value)
  "Find button by BUTT-TYPE and its VALUE.
If VALUE is not specified, then find fist one button of BUTT-TYPE."
  (cl-block 'button-found
    (telega-button-foreach butt-type (button)
      (when (or (null value) (eq (button-get button :value) value))
        (cl-return-from 'button-found button)))))

(defun telega-button-insert (button-type &rest props)
  "Insert button of BUTTON-TYPE with properties PROPS."
  (let ((value (plist-get props :value))
        (button-fmt (or (plist-get props :format)
                        (button-type-get button-type :format))))
    (apply #'insert-text-button
           (telega-fmt-eval button-fmt value)
           :type button-type props)))
(put 'telega-button-insert 'lisp-indent-function 'defun)

(defun telega-button-delete (button)
  "Delete the BUTTON."
  (delete-region (button-start button) (button-end button)))

(defun telega-button-move (button point)
  "Move BUTTON to POINT location."
  (let ((butt-type (button-type button))
        (props (text-properties-at button)))
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char point)
        (telega-button-delete button)

        ;; retain all properties except for those set by
        ;; `make-text-button'
        (cl--do-remf props 'category)
        (cl--do-remf props 'button)
        (apply #'telega-button-insert butt-type props)))))

(defun telega-button--redisplay (button)
  "Redisplay the BUTTON contents."
  (telega-button-move button (button-end button)))

(defun telega-button-value-set (button value)
  "For BUTTON set new VALUE."
  (assert value nil "Empty value for telega button is forbidden")

  (button-put button :value value)
  (telega-button--redisplay button))

(defun telega-button-forward (n &optional wrap display-message)
  "Move forward to N visible/active button."
  (interactive "p\nd\nd")
  (let (button)
    (dotimes (_ (abs n))
      (while (and (setq button (forward-button (cl-signum n) wrap))
                  (or (button-get button 'invisible)
                      (button-get button 'inactive)))))
    (when (= (following-char) ?\[)
      (forward-char 1))

    (when (button-get button :help-format)
      (message (telega-fmt-eval (button-get button :help-format)
                                (button-get button :value))))
    button))

(defun telega-button-backward (n &optional wrap display-message)
  "Move backward to N visible/active button."
  (interactive "p\nd\nd")
  (telega-button-forward (- n) wrap display-message))

;; FullInfo
(defun telega--on-updateUserFullInfo (event)
  (let ((ufi (cdr (assq 'user telega--full-info))))
    (puthash (plist-get event :user_id)
             (plist-get event :user_full_info) ufi)))

(defun telega--on-updateBasicGroupFullInfo (event)
  (let ((ufi (cdr (assq 'basicGroup telega--full-info))))
    (puthash (plist-get event :basic_group_id)
             (plist-get event :basic_group_full_info) ufi)))

(defun telega--on-updateSupergroupFullInfo (event)
  (let ((ufi (cdr (assq 'supergroup telega--full-info))))
    (puthash (plist-get event :supergroup_id)
             (plist-get event :supergroup_full_info) ufi)))

(defun telega--full-info (tlobj)
  "Get FullInfo for the TLOBJ.
TLOBJ could be one of: user, basicGroup or supergroup."
  (let* ((tlobj-type (telega--tl-type tlobj))
         (tlobj-id (plist-get tlobj :id))
         (fi-hash (cdr (assq tlobj-type telega--full-info)))
         (full-info (gethash tlobj-id fi-hash)))
    (unless full-info
      (setq full-info
            (telega-server--call
             (ecase tlobj-type
               (user
                `(:@type "getUserFullInfo" :user_id ,tlobj-id))
               (basicGroup
                `(:@type "getBasicGroupFullInfo" :basic_group_id ,tlobj-id))
               (supergroup
                `(:@type "getSupergroupFullInfo" :supergroup_id ,tlobj-id)))))
      (puthash tlobj-id full-info fi-hash))
    full-info))

(provide 'telega-core)

;;; telega-core.el ends here

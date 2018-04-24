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

(require 'telega-fmt)

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


;;; Buttons for telega
(defun telega-button-find (button-type)
  "Find button by its type BUTTON-TYPE.
Run under `save-excursion' to preserve point."
  (let ((button (or (button-at (point)) (next-button (point)))))
    (while (and button (not (eq (button-type button) button-type)))
      (setq button (next-button (point))))
    button))

(defun telega-button-value-set (button value)
  "For BUTTON set new VALUE."
  (let* ((props (text-properties-at button))
         (button-type (get (plist-get props 'category) 'type))
         (kwprops (cl-loop for (pname pval) on props by 'cddr
                           when (keywordp pname)
                           collect pname
                           when (keywordp pname)
                           collect pval)))
    (setq kwprops (plist-put kwprops :value value))
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (button-end button))
        (apply #'telega-button-insert button-type kwprops)
        (delete-region (button-start button) (button-end button))))))

(defun telega-button-insert (button-type &rest props)
  "Insert button of BUTTON-TYPE with properties PROPS."
  (let ((value (plist-get props :value)))
    (apply #'insert-text-button (telega-fmt-button button-type value)
           :type button-type props)))
                   
(provide 'telega-core)

;;; telega-core.el ends here

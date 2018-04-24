;;; telega-fmt.el --- Formatting routines for telega

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Mon Apr 23 21:29:56 2018
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

;;

;;; Code:
(require 'button)

(defcustom telega-eliding-string "..."
  "*String used for eliding long string in formats.
Nice looking middle dots can be done by setting
`telega-eliding-string' to `(make-string 3 #x00b7)'."
  :type 'string
  :group 'telega)

(defun telega-fmt-expand (fmt &rest body)
  "Expand format FMT to form understood by `telega-fmt-eval'."
  (let (locals symdefs value)
    (while (keywordp (car body))
      (case (pop body)
        (:locals (setq locals (pop body)))
        (:symdefs (setq symdefs (pop body)))
        (:value (setq value (list (pop body))))))
    (unless value (setq value body))

    ;; TODO:
    ))

(defun telega-fmt-compile (fmt &rest body)
  "Compiles format to a function of one argument OBJ."
  ;; TODO
  )

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
                 ((symbolp elem)
                  (if (symbol-function elem)
                      (apply elem value)
                    (symbol-value elem)))
                 ((listp elem)
                  (apply #'telega-fmt-eval elem value))
                 (t (error "Can't format ELEM: %s" elem)))
           attrs)))

(defun telega-fmt-eval (fmt-simple &rest value)
  (mapconcat (lambda (elem)
               (apply #'telega-fmt-eval-elem elem value)) fmt-simple ""))

(defun telega-fmt-button (button-type &rest value)
  "Apply button format of BUTTON-TYPE to the VALUE."
  (let ((fmt-simple (button-type-get button-type :format))
        (fmt-extended (button-type-get button-type :format-ext)))
    (assert fmt-simple nil "TODO: only simple formats supported")
    (apply #'telega-fmt-eval fmt-simple value)))

(provide 'telega-fmt)

;;; telega-fmt.el ends here

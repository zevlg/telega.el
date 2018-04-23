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

(defcustom telega-eliding-string "..."
  "*String used for eliding long string in formats.
Nice looking middle dots can be done by setting
`telega-eliding-string' to `(make-string 3 #x00b7)'."
  :type 'string
  :group 'telega)

(defun telega-fmt-compile (fmt format-defs)
  "Compiles format to a function of one argument OBJ."
  )

(defun telega-fmt-apply (obj fmt format-defs)
  
  
(provide 'telega-fmt)

;;; telega-fmt.el ends here

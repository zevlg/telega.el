;;; telega-make.el --- Helper for some make tasks  -*- lexical-binding: t -*-

;; Copyright (C) 2020 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Dec 22 21:01:01 2020
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

;;; Code:
(require 'cl-lib)

(defun telega-ensure-dependencies ()
  "Ensure telega dependencies are installed."
  (package-initialize)

  (let* ((core-pkgs '(visual-fill-column rainbow-identifiers))
         (contrib-pkgs '(all-the-icons alert dashboard transient))
         (all-pkgs (append core-pkgs contrib-pkgs))
         (need-pkgs (cl-remove-if #'package-installed-p all-pkgs)))
    (when need-pkgs
      (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;      (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
      (package-refresh-contents)

      (dolist (pkg need-pkgs)
        (cl-assert (not (package-installed-p pkg)))
        (package-install pkg)))))

(defun telega-byte-compile-everything ()
  "Recompiler everything in telega repository."
  ;; NOTE: `telega-ensure-dependencies' might change `default-directory'
  (let ((src-dir default-directory))
    (telega-ensure-dependencies)

    (let* ((load-prefer-newer t)     ;do not load outdated .elc files
           (result (byte-recompile-directory src-dir 0 t))
           (failed (string-match-p ", [0-9]+ failed" result)))
      (kill-emacs (if failed 1 0)))))

(defun telega-run-tests ()
  "Run telega tests."
  (telega-ensure-dependencies)

  (load "ert")
  (load "test")
  (ert-run-tests-batch-and-exit))

(provide 'telega-make)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; telega-make.el ends here

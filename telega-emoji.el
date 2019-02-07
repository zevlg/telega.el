;;; telega-emoji.el --- Emoji completions support for telega

;; Copyright (C) 2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Feb  7 15:57:06 2019
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

;;

;;; Code:
(require 'telega-util)
(eval-when (load)
  (require 'company))                      ; company-grab

(defvar telega-emoji-alist nil)
(defvar telega-emoji-candidates nil)
(defvar telega-emoji-max-length 0)

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

(defun telega-company-emoji (command &optional arg &rest ignored)
  "Backend for `company'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'telega-company-emoji))
    (init (telega-emoji-init))
    (sorted t)
    ;; Always match if having `:'
    (prefix (let ((cg (company-grab ":[^: _]+" nil
                                    (- (point) telega-emoji-max-length))))
              (when cg (cons cg company-minimum-prefix-length))))
    (candidates
     (let ((fuzzy-regexp (regexp-quote (concat "-" (substring arg 1)))))
       (cl-remove-if-not (lambda (en)
                           (or (string-prefix-p arg en)
                               (string-match-p fuzzy-regexp en)))
                         telega-emoji-candidates)))
    (annotation
     (concat (make-string (- telega-emoji-max-length (length arg)) ?\s)
             (cdr (assoc arg telega-emoji-alist))))
    (post-completion
     (delete-region (- (point) (length arg)) (point))
     (insert (cdr (assoc arg telega-emoji-alist))))
    ))

(provide 'telega-emoji)

;;; telega-emoji.el ends here

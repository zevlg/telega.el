;;; telega-emoji.el --- Emoji support for telega

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
(require 'company)

(defvar telega-emoji-alist nil)

(defun telega-emoji-init ()
  "Initialize emojis."
  (unless telega-emoji-alist
    (setq telega-emoji-alist
          (with-temp-buffer
            (insert-file-contents (telega-etc-file "emojis.alist"))
            (goto-char (point-min))
            (read (current-buffer))))))

(defun telega-company-emoji-insert (match)
  (message "INSERT: %S" match)
  )

(defun telega-company-emoji (command &optional arg &rest ignored)
  "Backend for `company'."
  (message "COMMAND: %S / %S " command arg)
  (cl-case command
    (init (telega-emoji-init))
    (prefix (company-grab-symbol))
    (candidates (mapcar 'car telega-emoji-alist))
    (meta (cdr (assoc arg telega-emoji-alist)))))

(defun telega-emoji-mode (&optional arg)
  "Toggle emoji autocomplete mode."
  (interactive "p")
  (if (or (null arg) (> arg 0))
      (progn
        (company-mode 1)
        (company-begin-backend 'telega-company-emoji
                               'telega-company-emoji-insert))

    (company-mode -1)))

(provide 'telega-emoji)

;; On load
(telega-emoji-init)

;;; telega-emoji.el ends here

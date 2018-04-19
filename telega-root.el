;;; telega-root.el --- Root buffer for telega.

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sat Apr 14 15:00:27 2018
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

(defcustom telega-root-buffer-name "Telega:Root"
  "*Buffer name for telega root buffer."
  :group 'telega)


(defvar telega-root-mode-hook nil
  "Hook run when telega root buffer is created.")

(defvar telega-root-mode-map (make-sparse-keymap)
  "The key map for telega root buffer.")

(define-derived-mode telega-root-mode nil "Telega-Root"
  "The mode for telega root buffer.

\\{telega-root-mode-map}"
  (telega-root--redisplay)
  (add-hook 'kill-buffer-hook 'telega-root-killed nil t))

(defun telega-root-killed ()
  "Run when telega root buffer is killed.

Terminate telega-server and kill all chat buffers."
  )

;; widgets used in root buffer
(defvar telega-root--widget-state nil)
(defvar telega-root--widget-search nil)
(defvar telega-root--widget-chats nil)
(defvar telega-root--widget-contacts nil)
(defvar telega-root--widget-channels nil)

(defsubst telega-root--state (state)
  "Change current telega-server state to STATE."
  (widget-value-set telega-root--widget-state state))

(defun telega-root--redisplay ()
  "Redisplay telega root buffer."
  (with-current-buffer telega-root-buffer-name
    (erase-buffer)
    (setq telega-root--widget-state
          (widget-create 'string :format "State: %v" :value "Unknown"))
    (setq telega-root--widget-search
          (widget-create 'string :format "Search: %v"))

    (widget-insert "\n")
    (widget-insert "Chats           Contacts        Channels\n")
    (widget-insert "-----           --------        --------\n")
    ))

(provide 'telega-root)

;;; telega-root.el ends here

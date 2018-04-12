;;; telega.el --- Telegram client

;; Copyright (C) 2016-2018 by Zajcev Evgeny

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Nov 30 19:04:26 2016
;; Keywords: 
;; Version: 0.1.0

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

;; Some code taken from circe


;;; Code:
(defconst telega-app '(72239 . "bbf972f94cc6f0ee5da969d8d42a6c76"))
(defconst telega-version "0.1.0")


(defgroup telega nil
  "Telegram client."
  :prefix "telega-"
  :group 'applications)

(defcustom telega-directory (expand-file-name "~/.telega")
  "Directory for telega runtime files."
  :type 'string
  :group 'telega)

(defcustom telega-cache-dir (expand-file-name "cache" telega-directory)
  "*Directory for telegram downloads."
  :type 'string
  :group 'telega)

(defcustom telega-rsa-key-file (expand-file-name "server.pub" telega-directory)
  "*RSA key to use."
  :type 'string
  :group 'telega)

(defcustom telega-server-command "telega-server"
  "Command to run is telega server."
  :type 'string
  :group 'telegram)

(defcustom telega-server-logfile nil
  "*Non-nil to write server logs to file."
  :type 'boolean
  :group 'telega)

(defcustom telega-server-verbosity 5
  "*Verbosity level for server process."
  :type 'number
  :group 'telega)

;;; Faces

(defface telega-atsign-face
  '((default (:weight bold))
    (((type tty)) (:foreground "cyan"))
    (((background dark)) (:foreground "#82e2ed"))
    (((background light)) (:foreground "#0445b7"))
    (t (:foreground "CadetBlue3")))
  "The face used to highlight text starting with @."
  :group 'telega)


(defcustom telega-prompt-string "> "
  "The string to initialize the prompt with.
To change the prompt dynamically or just in specific buffers, use
`lui-set-prompt' in the appropriate hooks."
  :type 'string
  :group 'telega)

;;; Utility functions

(defun telega--create-hier ()
  "Ensure directory hier is valid."
  (ignore-errors
    (mkdir telega-directory))
  (ignore-errors
    (mkdir telega-cache-dir)))

(define-derived-mode telegram-mode lui-mode "Telega"
  "Base mode for all telega buffers.

A buffer should never be in this mode directly, but rather in
modes that derive from this.

The mode inheritance hierarchy looks like this:

lui-mode
`--telega-mode
   `--telega-root-mode
   `--telega-chat-mode"
  (add-hook 'lui-pre-output-hook 'lui-irc-colors t t)
  (add-hook 'lui-pre-output-hook 'telega--pre-output t t)
  (add-hook 'completion-at-point-functions
            'telega--completion-at-point nil t)
  (lui-set-prompt telega-prompt-string)
  (goto-char (point-max))
  (setq lui-input-function 'telega--input
        default-directory (expand-file-name telega-directory))
  ;; Tab completion should be case-insensitive
  (set (make-local-variable 'completion-ignore-case) t)
  (set (make-local-variable 'tracking-faces-priorities)
       telega-track-faces-priorities))

(defun telega ()
  "Start telegramming."
  (interactive)
  (telega--create-hier)
  (let ((rootbuf (telega--root-genbuffer)))
    (with-current-buffer rootbuf
      (telega--root-mode)
      (telega--server-connect))
    (pop-to-buffer-same-window rootbuf)))

(defun telega-quit ()
  "Quit from current telegram account.
Next call to M-x telega RET will request account info."
  ;; TODO
  )


;;; Highlighting output
(defun telega--pre-output ()
  "Highlight usernames started with @.
This is used in `lui-pre-output-hook'."
  )

;;; Completion
(defun telega--completion-at-point ()
  "Return a list of possible completions for the current buffer.
This is used in `completion-at-point-functions'."
  ;; Use markers so they move when input happens
  ;; TODO
  )


(provide 'telega)

;;; telega.el ends here

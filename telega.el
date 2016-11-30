;;; telega.el --- Telegram client

;; Copyright (C) 2016 by Zajcev Evgeny

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Nov 30 19:04:26 2016
;; Keywords: 

;; This file is part of GNU Emacs.

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

(defgroup telega nil
  "Telegram client"
  :prefix "telega-"
  :group 'applications)

(defcustom telega-directory (expand-file-name "~/.telega")
  "Directory for telega runtime files."
  :type 'string
  :group 'telega)

(defcustom telega-downloads-dir (expand-file-name "downloads" telega-directory)
  "*Directory for telegram downloads."
  :type 'string
  :group 'telega)

(defcustom telega-rsa-key-file (expand-file-name "server.pub" telega-directory)
  "*RSA key to use."
  :type 'string
  :group 'telega)

(defcustom telega-test-mode-enable t
  "*Non-nil to enable test mode."
  :type 'boolean
  :group 'telega)

(defcustom telega-pfs-enable t
  "*Non-nil to enable Perfect Forward Secrecy mode."
  :type 'boolean
  :group 'telega)

(defcustom telega-binlog-enable t
  "*Non-nil to use binlog mode."
  :type 'boolean
  :group 'telega)

(defcustom telega-verbosity 2
  "*Verbosity level for telegram"
  :type 'number
  :group 'telega)

(defvar telega-state nil)

(defun telega--file (fwhat)
  (expand-file-name 
   (cl-ecase fwhat
     (:downloads telega-downloads-dir)
     (:rsa-key telega-rsa-key-file)
     (:config "config")
     ((:auth :auth-key) "auth")
     (:state "state")
     (:secret "secret")
     (:binlog "binlog"))
   telega-directory))

(defun telega--create-hier ()
  "Ensure directory hier is valid."
  (ignore-errors
    (mkdir telega-directory))
  (ignore-errors
    (mkdir telega-downloads-dir)))

(defun telega ()
  "Start telegramming."
  (interactive)
  (unless telega-state
    (telega--create-hier)

    (setq telega-state (tgl:state_alloc))
    (tgl:set_verbosity telega-state telega-verbosity)

    (when telega-test-mode-enable
      (tgl:set_test_mode telega-state))

    (when telega-pfs-enable
      (tgl:enable_pfs telega-state))

    (if telega-binlog-enable
        (progn
          (tgl:set_binlog_mode telega-state 1)
          (tgl--set-binlog-path telega-state (telega--file :binlog)))

      (tgl:set_binlog_mode telega-state 0))

    (tgl--set-download-dir telega-state telega-downloads-dir)
    (tgl--set-key telega-state (telega--file :rsa-key))
    ))

(provide 'telega)

;;; telega.el ends here

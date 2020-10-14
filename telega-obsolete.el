;;; telega-obsolete.el --- Check the use of obsolete functionality  -*- lexical-binding: t -*-

;; Copyright (C) 2020 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Jan 17 14:55:16 2020
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

;; Some telega functionality become obsolete sometimes.
;; telega warns user at load time in case he uses obsolete functionality.

;;; Code:

(require 'cl-lib)

(defvar telega-obsolete--variables nil
  "List of obsolete variables to examine at load time.")

(defun telega-obsolete--variable (obsolete-name &rest args)
  "Same as `make-obsolete-variable'."
  (setq telega-obsolete--variables
        (cl-pushnew obsolete-name telega-obsolete--variables))
  (apply 'make-obsolete-variable obsolete-name args))

(defun telega-obsolete--warning (var obsolescence-data)
  (let ((instead (car obsolescence-data))
        (version (nth 2 obsolescence-data)))
    (format-message
     "`%S' is an obsolete in telega %s%s" var version
     (cond ((stringp instead) (concat "; " (substitute-command-keys instead)))
           (instead (format-message "; use `%s' instead." instead))
           (t ".")))))


(telega-obsolete--variable 'telega-chat-use-markdown-formatting
                           'telega-chat-use-markdown-version "0.5.6")
(telega-obsolete--variable 'telega-use-tracking
                           'telega-use-tracking-for "0.5.7")
(telega-obsolete--variable 'telega-avatar-factors
                           'telega-avatar-factors-alist "0.5.8")
(telega-obsolete--variable 'telega-url-shorten-patterns
                           'telega-url-shorten-regexps "0.6.7")
(telega-obsolete--variable 'telega-chat-mark-observable-messages-as-read
                           nil "0.6.12")
(telega-obsolete--variable 'telega-root-compact-view
                           nil "0.6.21")
(telega-obsolete--variable 'telega-filter-custom-push-list
                           'telega-filter-custom-folders "0.6.24")

(telega-obsolete--variable 'telega-chat-label-format
                           'telega-chat-folder-format "0.6.30")
(telega-obsolete--variable 'telega-root-view-topics-custom-labels
                           'telega-root-view-topics-folders "0.6.30")
(telega-obsolete--variable 'telega-root-view-show-other-chats
                           'telega-root-view-topics-other-chats "0.6.30")
(telega-obsolete--variable 'telega-user-photo-maxsize
                           'telega-user-photo-size "0.6.30")
(telega-obsolete--variable 'telega-photo-maxsize
                           'telega-photo-size-limits "0.6.30")
(telega-obsolete--variable 'telega-thumbnail-height
                           'telega-thumbnail-size-limits "0.6.30")
(telega-obsolete--variable 'telega-webpage-photo-maxsize
                           'telega-webpage-photo-size-limits "0.6.30")

(telega-obsolete--variable 'telega-find-file-hook
                           'telega-open-file-hook "0.6.31")

;; Check some obsolete var/fun is used
(cl-eval-when (eval load)
  (dolist (obsolete-var telega-obsolete--variables)
    (when (boundp obsolete-var)
      (display-warning
       'telega (telega-obsolete--warning
                obsolete-var (get obsolete-var 'byte-obsolete-variable)))))
  )

(provide 'telega-obsolete)

;;; telega-obsolete.el ends here

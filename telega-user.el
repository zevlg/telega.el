;;; telega-user.el --- User related stuff for telega

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Apr 20 00:24:21 2018
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
(require 'telega-core)
(require 'telega-server)

(defun telega-user--get (user_id)
  "Get user by USER_ID."
  (let ((user (gethash user_id telega--users)))
    (unless user
      (puthash user_id
               (setq user
                     (telega-server--call
                      `(:@type "getUser" :user_id ,user_id)))
               telega--users))
    user))

(defun telega-user--type (user)
  "Return USER type."
  (intern (downcase (substring (plist-get (plist-get user :type) :@type) 8))))

(defun telega-user--bot-p (user)
  "Return non-nil if USER is bot."
  (eq (telega-user--type user) 'bot))

(defun telega-user--title (user)
  "Return title for the USER."
  (if (eq (telega-user--type user) 'deleted)
      "Deleted Account"
    (format "%s %s @%s"
            (plist-get user :first_name)
            (plist-get user :last_name)
            (plist-get user :username))))

(provide 'telega-user)

;;; telega-user.el ends here

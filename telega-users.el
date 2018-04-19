;;; telega-users.el --- Telega users

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
(defun telega-user--get (user_id)
  (telega-server--call
   `(:@type "getUser" :user_id ,user_id)))

(defun telega-user--title (user)
  "Return title for the USER."
  (let ((user-type (plist-get user :type))
        (title (format "%s %s @%s" (plist-get user :first_name)
                       (plist-get user :last_name) (plist-get user :username))))
    (if (string= title "  @")
        (ecase (intern (plist-get user-type :@type))
          (userTypeDeleted "Deleted Account"))
      title)))

(provide 'telega-users)

;;; telega-users.el ends here

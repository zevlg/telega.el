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

(defun telega-user--me ()
  "Return me as telegram user."
  (telega-server--call `(:@type "getMe")))

(defun telega-user--get (user-id)
  "Get user by USER-ID."
  (let ((user (gethash user-id telega--users)))
    (unless user
      (setq user (telega-server--call
                  `(:@type "getUser" :user_id ,user-id)))
      (assert user nil "getUser timed out user_id=%d" user-id)
      (puthash user-id user telega--users))
    user))

(defun telega--on-updateUser (event)
  (let ((user (plist-get event :user)))
    (puthash (plist-get user :id) user telega--users)
    ;; TODO: may affect changes in root/chat
    ))

(defun telega-user--type (user)
  "Return USER type."
  (intern (downcase (substring (plist-get (plist-get user :type) :@type) 8))))

(defun telega-user--bot-p (user)
  "Return non-nil if USER is bot."
  (eq (telega-user--type user) 'bot))

(defun telega-user--title (user)
  "Return title for the USER."
  (if (eq (telega-user--type user) 'deleted)
      (format "DeletedUser-%d" (plist-get user :id))
    (format "%s %s @%s"
            (plist-get user :first_name)
            (plist-get user :last_name)
            (plist-get user :username))))

(defun telega-user--full-info (user)
  (telega-server--call
   `(:@type "getUserFullInfo" :user_id ,(plist-get user :id))))

(defun telega-user--seen-status (user)
  "Return last seen status for the USER."
  (substring (plist-get (plist-get user :status) :@type) 10))

(defun telega-user-info--insert (user)
  "Insert USER info into current buffer."
  (let ((full-info (telega-user--full-info user)))
    (insert (telega-user--title user))
    (insert "\n")
    (insert (format "Seen status: %s\n"
                    (telega-user--seen-status user)))
    (unless (string-empty-p (plist-get user :phone_number))
      (insert (format "phone: +%s\n\n" (plist-get user :phone_number))))
    (unless (string-empty-p (plist-get full-info :bio))
      (insert (format "bio: %s\n\n" (plist-get full-info :bio))))

    (when (> (plist-get full-info :group_in_common_count) 0)
      (insert (format "%d groups in common:\n"
                      (plist-get full-info :group_in_common_count)))
      (dolist (chat (telega-chat--getGroupsInCommon user))
        (insert "    ") 
        (telega-button-insert 'telega-chat
            :value chat
            :format '("[" (telega-chat--title
                           :min 25 :max 25
                           :align left :align-char ?\s
                           :elide t :elide-trail 0)
                      "]"))))
    ))

(provide 'telega-user)

;;; telega-user.el ends here

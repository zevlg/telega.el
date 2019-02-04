;;; telega-user.el --- Users handling for the telega

;; Copyright (C) 2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Jan 23 23:49:52 2019
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

(define-button-type 'telega-user
  :supertype 'telega
  'action #'telega-user-button--action)

(define-button-type 'telega-member
  :supertype 'telega-user
  :inserter 'telega-ins--chat-member
  'action #'telega-member-button--action)

(defun telega-member-button--action (button)
  (let ((member (button-get button :value)))
    (telega-describe-chat
     (telega-chat-get (plist-get member :user_id)))))

(defun telega-user--get (user-id)
  "Get user by USER-ID."
  (telega--info 'user user-id))

(defun telega--getMe ()
  "Return me as telegram user."
  (telega-server--call `(:@type "getMe")))

(defun telega-user--type (user)
  "Return USER type."
  (intern (downcase (substring (plist-get (plist-get user :type) :@type) 8))))

(defun telega-user--bot-p (user)
  "Return non-nil if USER is bot."
  (eq (telega-user--type user) 'bot))

(defun telega-user--name (user &optional fmt-type)
  "Return name for the USER.
Format name using FMT-TYPE, one of:
  `name' - Uses only first and last names
  `short' - Uses username if set, name otherwise
  `full' - Uses all available namings
Default is: `full'"
  (if (eq (telega-user--type user) 'deleted)
      (format "DeletedUser-%d" (plist-get user :id))

    (let ((fmt-type (or fmt-type 'full))
          (name ""))
      (when (memq fmt-type '(full short))
        (let ((un (plist-get user :username)))
          (if (string-empty-p un)
              (when (eq fmt-type 'short)
                (setq fmt-type 'name))
            (setq name (concat "@" un)))))
      (when (or (memq fmt-type '(full name)) (string-empty-p name))
        (let ((ln (plist-get user :last_name)))
          (unless (string-empty-p ln)
            (setq name (concat ln (if (string-empty-p name) "" " ") name)))))
      (when (or (memq fmt-type '(full name)) (string-empty-p name))
        (let ((fn (plist-get user :first_name)))
          (unless (string-empty-p fn)
            (setq name (concat fn (if (string-empty-p name) "" " ") name)))))
      name)))

(defun telega-user--seen (user)
  "Return last seen status for the USER."
  (substring (plist-get (plist-get user :status) :@type) 10))

(defun telega--on-updateUserStatus (event)
  "User status has been changed."
  (let ((user (telega-user--get (plist-get event :user_id)))
        (status (plist-get event :status)))
    (plist-put user :status status)
    ;; NOTE: For online status, set special USER property with value
    ;; of time last seen online
    (when (eq (telega--tl-type status) 'userStatusOnline)
      (plist-put user :telega-last-online (telega-time-seconds)))
    (run-hook-with-args 'telega-user-update-hook user)))

(defun telega-user--chats-in-common (with-user)
  "Return CHATS in common WITH-USER."
  (let ((gic-cnt (plist-get (telega--full-info with-user)
                            :group_in_common_count)))
    (unless (zerop gic-cnt)
      (telega-chats-list-get
       (telega-server--call
        (list :@type "getGroupsInCommon"
              :user_id (plist-get with-user :id)
              :offset_chat_id 0
              :limit gic-cnt))))))

(defun telega-user-initials (user)
  "Return initials composed from first and last name of the USER."
  (let ((ufn (telega--desurrogate-apply (plist-get user :first_name)))
        (uln (telega--desurrogate-apply (plist-get user :last_name)))
        (res ""))
    (unless (string-empty-p ufn)
      (setq res (capitalize (substring ufn 0 1))))
    (unless (string-empty-p uln)
      (setq res (concat res (capitalize (substring uln 0 1)))))
    (when (string-empty-p res)
      ;; NOTE: Might be so for deleted users
      ;; i.e. (eq (telega-user--type user) 'deleted)
      (setq res "DU"))
    res))

(defun telega-user-avatar-svg (user)
  "Return avatar for the USER."
  (let ((ava (plist-get user :telega-avatar)))
    (unless ava
      (let* ((chat (telega-chat-get (plist-get user :id) 'offline))
             (color (telega-chat-uaprop chat :color)))
        (unless color
          ;; Assign the color to the user
          (setq color (telega-color-random))

          ;; If there corresponding chat, then update its color
          (when chat
            (setf (telega-chat-uaprop chat :color)
                  (telega-color-random))))

        (setq ava (telega-avatar--gen-svg
                   (telega-user-initials user) 2 color))
        (plist-put user :telega-avatar ava)))

    ava))
                                        
(provide 'telega-user)

;;; telega-user.el ends here

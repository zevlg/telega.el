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
(defvar telega-user-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map [remap self-insert-command] 'ignore)
    (define-key map (kbd "n") 'telega-button-forward)
    (define-key map (kbd "p") 'telega-button-backward)

    (define-key map (kbd "i") 'telega-describe-user)
    (define-key map (kbd "m") 'telega-user-message)
    (define-key map (kbd "d") 'telega-user-delete)
    (define-key map (kbd "k") 'telega-user-delete)
    (define-key map (kbd "DEL") 'telega-user-delete)
    map))

(define-button-type 'telega-user
  :supertype 'telega
  'read-only t
  'keymap telega-user-button-map
  'action #'telega-user-button--action)

(define-button-type 'telega-member
  :supertype 'telega-user
  :inserter 'telega-ins--chat-member)

(defun telega-user-at (pos)
  "Return user at position POS."
  (let ((member-or-user (button-get (button-at pos) :value)))
    (if (eq (telega--tl-type member-or-user) 'chatMember)
        (telega-user--get (plist-get member-or-user :user_id))
      member-or-user)))

(defun telega-user-button--action (button)
  (telega-describe-user (telega-user-at button)))

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
             (colors (telega-chat-uaprop chat :color)))
        (unless colors
          ;; Assign the color to the user
          (let ((col (telega-color-random)))
            (setq colors (list (telega-color-gradient col 'light)
                               col
                               (telega-color-gradient col))))
          ;; If there corresponding chat, then update its color
          (when chat
            (setf (telega-chat-uaprop chat :color) colors)))

        (setq ava (telega-avatar--gen-svg
                   (telega-user-initials user) 2
                   (if (eq (frame-parameter nil 'background-mode) 'light)
                       (cdr colors)
                     colors)))
        (plist-put user :telega-avatar ava)))
    ava))

(defun telega--searchChatMembers (chat query &optional filter limit)
  "Search CHAT members by QUERY.
FILTER is one \"Administrators\", \"Members\", \"Restricted\",
\"Banned\", \"Bots\", default is \"Members\".
LIMIT by default is 50."
  (let ((reply (telega-server--call
                (list :@type "searchChatMembers"
                      :chat_id (plist-get chat :id)
                      :query query
                      :limit (or limit 50)
                      :filter (list :@type (concat "chatMembersFilter"
                                                   (or filter "Members")))))))
    (mapcar (lambda (member)
              (telega-user--get (plist-get member :user_id)))
            (plist-get reply :members))))

(provide 'telega-user)

;;; telega-user.el ends here

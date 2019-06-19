;;; telega-user.el --- Users handling for the telega  -*- lexical-binding:t -*-

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
(require 'telega-core)

(declare-function telega-info--insert-user "telega-info" (user &optional chat))

(defvar telega-user-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
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
  "Action to take when user BUTTON is pressed.
If BUTTON has custom `:action', then use it, otherwise describe the user."
  (let ((user (telega-user-at button))
        (custom-action (button-get button :action)))
    (cl-assert user)
    (if custom-action
        (funcall custom-action user)
      (telega-describe-user user))))

(defun telega-user--get (user-id)
  "Get user by USER-ID."
  (telega--info 'user user-id))

(defmacro telega-user-me ()
  "Return me is a user."
  `(telega-user--get telega--me-id))

(defun telega-user--type (user)
  "Return USER type."
  (intern (downcase (substring (plist-get (plist-get user :type) :@type) 8))))

(defsubst telega-user-bot-p (user)
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

(defun telega-user-color (user)
  "Return color list associated with USER."
  (or (plist-get user :color)
      (let* ((chat (telega-chat-get (plist-get user :id) 'offline))
             (colors (if chat
                         (telega-chat-color chat)
                       (telega-color-tripple (telega-color-random)))))
        (plist-put user :color colors)
        colors)))

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

(defun telega-user-avatar-image (user)
  "Return avatar image for the USER."
  (let ((photo (plist-get user :profile_photo)))
    (telega-media--image
     (cons user 'telega-avatar--create-image)
     (cons photo :small))))

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

(defun telega--getUserProfilePhotos (user &optional offset limit)
  "Return the profile photos (`UserProfilePhotos') of a USER.
OFFSET - number of photos to skip (default=0)
LIMIT - limit number of photos (default=100)."
  (telega-server--call
   (list :@type "getUserProfilePhotos"
         :user_id (plist-get user :id)
         :offset (or offset 0)
         :limit (or limit 100))))

(defun telega-describe-user (user &optional full-p)
  "Show info about USER.
If FULL-P is non-nil, then show full info about user."
  (with-telega-help-win "*Telega User*"
    (when full-p
      (telega-ins "Name: ")
      (when (telega-ins (plist-get user :first_name))
        (telega-ins " "))
      (telega-ins (plist-get user :last_name))
      (telega-ins " ")
      (telega-ins--button "Chat With"
        :value user
        :action 'telega-user-chat-with)
      (telega-ins " "))
    (telega-info--insert-user user)))

(defun telega-user-chat-with (user)
  "Start private chat with USER."
  (telega-chat--pop-to-buffer
   (telega--createPrivateChat user)))


;;; Contacts
(defun telega-ins--root-contact (contact)
  "Inserter for CONTACT user."
  (telega-ins telega-symbol-contact " ")
  (when (telega-ins (plist-get contact :first_name))
    (telega-ins " "))
  (when (telega-ins (plist-get contact :last_name))
    (telega-ins " "))
  (telega-ins-prefix "@"
    (when (telega-ins (plist-get contact :username))
      (telega-ins " ")))
  (telega-ins-prefix "+"
    (telega-ins (plist-get contact :phone_number))))

(defun telega-contact-root--pp (contact)
  "Pretty printer for CONTACT button shown in root buffer.
CONTACT is some user you have exchanged contacs with."
  (telega-button--insert 'telega-user contact
    'keymap button-map
    :inserter 'telega-ins--root-contact
    :action 'telega-user-chat-with)
  (telega-ins "\n"))

(defun telega--getContacts ()
  "Return users that are in contact list."
  (mapcar 'telega-user--get
          (plist-get (telega-server--call
                      (list :@type "getContacts"))
                     :user_ids)))

(defun telega--removeContacts (&rest user-ids)
  "Remove users determined by USER-IDS from contacts."
  (telega-server--call
   (list :@type "removeContacts"
         :user_ids (cl-map 'vector 'identity user-ids))))

(defun telega--searchContacts (query &optional limit)
  "Search contacts for already chats by QUERY."
  (mapcar 'telega-user--get
          (plist-get (telega-server--call
                      (list :@type "searchContacts"
                            :query query
                            :limit (or limit 200)))
                     :user_ids)))

(defun telega--importContacts (&rest contacts)
  "Import CONTACTS into contacts list."
  (telega-server--call
   (list :@type "importContacts"
         :contacts (cl-map 'vector 'identity contacts))))

(defun telega-contact-add (phone &optional name)
  "Add user by PHONE to contact list."
  (interactive (list (read-string "Phone number: ")
                     (read-string "Name: ")))
  (let* ((names (split-string name " "))
         (reply (telega--importContacts
                 (nconc (list :@type "contact" :phone_number phone)
                        (unless (string-empty-p (car names))
                          (list :first_name (car names)))
                        (when (cdr names)
                          (list :last_name
                                (mapconcat 'identity (cdr names) " "))))))
         (user-id (aref (plist-get reply :user_ids) 0)))
    (when (zerop user-id)
      (user-error "No telegram user with phone %s" phone))
    (telega-describe-user (telega-user--get user-id) 'full)))

(defun telega-describe-contact (contact)
  "Show CONTACT information."
  (with-telega-help-win "*Telega Contact*"
    (let* ((user-id (plist-get contact :user_id))
           (user (telega-user--get user-id))
           (full-info (telega--full-info user)))
      (when (telega-ins (plist-get contact :first_name))
        (telega-ins " "))
      (telega-ins (plist-get contact :last_name) "\n")
      (telega-ins-fmt "Phone: %s\n" (plist-get contact :phone_number))
      (if (eq (telega--tl-type (plist-get user :outgoing_link))
              'linkStateIsContact)
          (telega-ins--button "RemoveContact"
            :value contact
            :action (lambda (contact)
                      (telega--removeContacts (plist-get contact :user_id))
                      (telega-save-cursor
                        (telega-describe-contact contact))))

        (telega-ins--button "ImportContact"
          :value contact
          :action (lambda (contact)
                    (telega--importContacts contact)
                    (telega-save-cursor
                      (telega-describe-contact contact)))))
      (telega-ins "\n")

      (telega-ins "\n--- Telegram User Info ---\n")
      (telega-ins "Name: " (telega-user--name user 'name) "\n")
      (telega-ins--button "ChatWith"
        :value user
        :action 'telega-user-chat-with)
      (telega-ins " ")
      (telega-info--insert-user user))))

(provide 'telega-user)

;;; telega-user.el ends here

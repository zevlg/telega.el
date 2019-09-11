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
(require 'telega-info)

(declare-function telega-root--chat-update "telega-root" (chat &optional for-reorder))

(declare-function telega--getGroupsInCommon "telega-chat" (with-user))
(declare-function telega--createPrivateChat "telega-chat" (user))
(declare-function telega-chat-get "telega-chat" (chat-id &optional offline-p))
(declare-function telega-chat-color "telega-chat" (chat))
(declare-function telega-chat--pop-to-buffer "telega-chat" (chat))
(declare-function telega-chatbuf-mode-line-update "telega-chat")


(defvar telega-user-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "i") 'telega-describe-user)
    (define-key map (kbd "m") 'telega-user-chat-with)
    (define-key map (kbd "B") 'telega-user-block)
    (define-key map (kbd "D") 'telega-user-block)
    (define-key map (kbd "K") 'telega-user-block)
    (define-key map (kbd "DEL") 'telega-user-block)
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

(defun telega-user--by-username (username)
  "Get user by his USERNAME.
If ASYNC-CALLBACK is specified, then call it, when info about
user is fetched from server."
  (when (string-prefix-p "@" username)
    (setq username (substring username 1)))
  (let ((users (hash-table-values (alist-get 'user telega--info))))
    (cl-find username users :key (telega--tl-prop :username) :test 'equal)))

(defmacro telega-user-me ()
  "Return me is a user."
  `(telega-user--get telega--me-id))

(defsubst telega-user-online-p (user)
  "Return non-nil if USER is online."
  (string= (telega-user--seen user) "Online"))

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
        (let ((un (telega-tl-str user :username)))
          (if (string-empty-p un)
              (when (eq fmt-type 'short)
                (setq fmt-type 'name))
            (setq name (concat "@" un)))))
      (when (or (memq fmt-type '(full name)) (string-empty-p name))
        (let ((ln (telega-tl-str user :last_name)))
          (unless (string-empty-p ln)
            (setq name (concat ln (if (string-empty-p name) "" " ") name)))))
      (when (or (memq fmt-type '(full name)) (string-empty-p name))
        (let ((fn (telega-tl-str user :first_name)))
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
  (let* ((user-id (plist-get event :user_id))
         (user (telega-user--get user-id))
         (status (plist-get event :status)))
    (plist-put user :status status)
    ;; NOTE: For online status, set special USER property with value
    ;; of time last seen online
    (when (eq (telega--tl-type status) 'userStatusOnline)
      (plist-put user :telega-last-online (telega-time-seconds)))

    ;; Update chatbuf's modeline and root as well
    (unless (telega-me-p user)
      (when-let ((chat (telega-chat-get user-id 'offline)))
        (with-telega-chatbuf chat
          (telega-chatbuf-mode-line-update))
        (telega-root--chat-update chat)))

    (run-hook-with-args 'telega-user-update-hook user)))

(defun telega-user--chats-in-common (with-user)
  "Return CHATS in common WITH-USER."
  (let ((gic-cnt (plist-get (telega--full-info with-user)
                            :group_in_common_count)))
    (unless (zerop gic-cnt)
      (telega--getGroupsInCommon with-user))))

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

(defun telega--getUserProfilePhotos (user &optional offset limit _callback)
  "Return the profile photos (`UserProfilePhotos') of a USER.
OFFSET - number of photos to skip (default=0)
LIMIT - limit number of photos (default=100)."
  (let ((reply (telega-server--call
                (list :@type "getUserProfilePhotos"
                      :user_id (plist-get user :id)
                      :offset (or offset 0)
                      :limit (or limit 100)))))
    (append (plist-get reply :photos) nil)))

(defun telega-describe-user (user)
  "Show info about USER."
  (interactive (list (telega-user-at (point))))
  (with-telega-help-win "*Telega User*"
    (telega-ins "Name: ")
    (when (telega-ins (telega-tl-str user :first_name))
      (telega-ins " "))
    (telega-ins (telega-tl-str user :last_name))
    (telega-ins "\n")
    (telega-info--insert-user
     user nil (lambda () (telega-describe-user user)))))

(defun telega-user-chat-with (user)
  "Start private chat with USER."
  (interactive (list (telega-user-at (point))))
  (telega-chat--pop-to-buffer
   (telega--createPrivateChat user)))

(defun telega--blockUser (user)
  "Add USER to the blacklist."
  (telega-server--call
   (list :@type "blockUser"
         :user_id (plist-get user :id))))

(defun telega--unblockUser (user)
  "Remove USER from the blacklist."
  (telega-server--call
   (list :@type "unblockUser"
         :user_id (plist-get user :id))))

(defun telega--getBlockedUsers (&optional offset)
  "Get list of blocked users."
  (let ((reply (telega-server--call
                (list :@type "getBlockedUsers"
                      :offset (or offset 0)
                      :limit 100))))
    (mapcar 'telega-user--get (plist-get reply :user_ids))))

(defun telega-user-block (user &optional unblock-p)
  "Toggle block state of the USER.
If UNBLOCK-P is specified, then unblock USER."
  (interactive (list (telega-user-at (point))))
  (if unblock-p
      (telega--unblockUser user)
    (when (yes-or-no-p
           (format "Really block user %s? " (telega-user--name user)))
      (telega--blockUser user))))


;;; Contacts
(defun telega-ins--root-contact (contact)
  "Inserter for CONTACT user."
  (telega-ins telega-symbol-contact " ")
  (when (telega-ins (telega-tl-str contact :first_name))
    (telega-ins " "))
  (when (telega-ins (telega-tl-str contact :last_name))
    (telega-ins " "))
  (telega-ins-prefix "@"
    (when (telega-ins (telega-tl-str contact :username))
      (telega-ins " ")))
  (telega-ins-prefix "+"
    (telega-ins (plist-get contact :phone_number))))

(defun telega-contact-root--pp (contact)
  "Pretty printer for CONTACT button shown in root buffer.
CONTACT is some user you have exchanged contacs with."
  (telega-button--insert 'telega-user contact
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
         :user_ids (apply 'vector user-ids))))

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
         :contacts (apply 'vector contacts))))

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
    (telega-describe-user (telega-user--get user-id))))

(defun telega-describe-contact (contact)
  "Show CONTACT information."
  (with-telega-help-win "*Telega Contact*"
    (let ((user (telega-user--get (plist-get contact :user_id))))
      (when (telega-ins (telega-tl-str contact :first_name))
        (telega-ins " "))
      (telega-ins (telega-tl-str contact :last_name) "\n")
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
      (telega-info--insert-user
       user nil (lambda () (telega-describe-contact contact))))))

(provide 'telega-user)

;;; telega-user.el ends here

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
(require 'telega-tdlib)

(declare-function telega-chat-get "telega-chat" (chat-id &optional offline-p))
(declare-function telega-chat-user "telega-chat" (chat))
(declare-function telega-chat-color "telega-chat" (chat))
(declare-function telega-chat--pop-to-buffer "telega-chat" (chat))


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
  :inserter #'telega-ins--user

  ;; NOTE: To make input method works under message buttons,
  ;; See `quail-input-method' for details
  'read-only t
  'front-sticky t

  'keymap telega-user-button-map
  'action #'telega-user-button--action)

(define-button-type 'telega-member
  :supertype 'telega-user
  :inserter #'telega-ins--chat-member)

(defun telega-user-at (pos)
  "Return user at position POS."
  (let ((member-or-user-or-chat (button-get (button-at pos) :value)))
    (cl-ecase (telega--tl-type member-or-user-or-chat)
      (chatMember
       (telega-user-get
        (telega--tl-get member-or-user-or-chat :member_id :user_id)))
      (chat
       (telega-chat-user member-or-user-or-chat))
      (user
       member-or-user-or-chat))))

(defun telega-user-button--action (button)
  "Action to take when user BUTTON is pressed.
If BUTTON has custom `:action', then use it, otherwise describe the user."
  (let ((user (telega-user-at button))
        (custom-action (button-get button :action)))
    (cl-assert user)
    (if custom-action
        (funcall custom-action user)
      (telega-describe-user user))))

(defun telega-user-get (user-id &optional locally-p)
  "Get user by USER-ID."
  (telega--info 'user user-id locally-p))

(defun telega-user-list (&optional temex)
  "Return list of users matching TEMEX."
  (cl-remove-if-not
   (when temex
     (telega-match-gen-predicate 'user
       temex))
   (hash-table-values (alist-get 'user telega--info))))

(defun telega-user--by-username (username)
  "Get user by his USERNAME."
  (when (string-prefix-p "@" username)
    (setq username (substring username 1)))
  (catch 'user-found
    (maphash (lambda (_user-id user)
               (let ((usernames (plist-get user :usernames)))
                 (when (seq-some (lambda (active-username)
                                   (equal username active-username))
                                 (plist-get usernames :active_usernames))
                   (throw 'user-found user))))
             (alist-get 'user telega--info))))

(defun telega-user-me (&optional locally-p)
  "Return me is a user."
  (telega-user-get telega--me-id locally-p))

(defsubst telega-user-online-p (user)
  "Return non-nil if USER is online."
  (equal (telega-user--seen user) "Online"))

(defun telega-user-chat (user)
  "Return private chat with USER."
  (telega-chat-get (plist-get user :id) 'offline))

(defun telega-user--type (user)
  "Return USER type."
  (intern (downcase (substring (plist-get (plist-get user :type) :@type) 8))))

(defsubst telega-user-bot-p (user)
  "Return non-nil if USER is bot."
  (eq (telega-user--type user) 'bot))

(defun telega-user-title (user fmt-type &optional no-badges)
  "Return formatted title for the USER.
Format name using FMT-TYPE, one of:
  `first-name' - Uses only first name.
  `last-name' - Uses only last name.
  `full-name' - Uses only first and last name.
  `username' - Uses username only.
Non-nil NO-BADGES to not attach any badges for the user title.
Return nil if given FMT-TYPE is not available."
  ;; NOTE: USER might be of "contact" type
  (let* ((user-p (eq (telega--tl-type user) 'user))
         (name (cond ((and user-p (telega-user-match-p user 'is-deleted))
                      ;; I18N: deleted -> Deleted Account
                      (format "%s-%d" (telega-i18n "lng_deleted")
                              (plist-get user :id)))
                     ((eq fmt-type 'first-name)
                      (telega-tl-str user :first_name))
                     ((eq fmt-type 'last-name)
                      (telega-tl-str user :last_name))
                     ((eq fmt-type 'full-name)
                      (let ((first-name (telega-tl-str user :first_name))
                            (last-name (telega-tl-str user :last_name)))
                        (when (or first-name last-name)
                          (concat first-name
                                  (when (and first-name last-name)
                                    " ")
                                  last-name))))
                     ((eq fmt-type 'username)
                      (when-let ((un (telega-msg-sender-username user)))
                        (concat "@" un)))
                     (t
                      (error "Invalid FMT-TYPE for `telega-user-title': %S"
                             fmt-type)))))

    (cond ((and (not no-badges) user-p name)
           ;; Scam/Fake/Blacklist badge, apply for users only
           ;; see https://t.me/emacs_telega/30318
           (concat name
                   ;; Badges
                   (when (plist-get user :is_verified)
                     (telega-symbol 'verified))
                   (cond ((plist-get user :emoji_status)
                          (telega-ins--as-string
                           (telega-ins--user-emoji-status user)))
                         ((plist-get user :is_premium)
                          (telega-symbol 'premium)))
                   (when (plist-get user :is_scam)
                     (propertize (telega-i18n "lng_scam_badge") 'face 'error))
                   (when (plist-get user :is_fake)
                     (propertize (telega-i18n "lng_fake_badge") 'face 'error))
                   (when (telega-user-match-p user 'is-blocked)
                     (telega-symbol 'blocked))))
          (name name)
          ((not (eq fmt-type 'username))
           ;; For some users only ID is known
           (format "UNKNOWN-%d" (plist-get user :id))))))

(defun telega-user--seen (user &optional as-number)
  "Return last seen status for the USER.
If AS-NUMBER is specified, return online status as number:
0 - Unknown
1 - Empty
2 - Offline
3 - LastMonth
4 - LastWeek
5 - Recently
6 - Online."
  (let ((online-status
         (substring (plist-get (plist-get user :status) :@type) 10)))
    (if as-number
        (length (member online-status
                        '("Online" "Recently" "LastWeek"
                          "LastMonth" "Offline" "Empty")))
      online-status)))

(defun telega-user-color (user)
  "Return color list associated with USER."
  (or (plist-get user :color)
      (let* ((chat (telega-chat-get (plist-get user :id) 'offline))
             (colors (if chat
                         (telega-chat-color chat)
                       (let ((user-title (telega-user-title user 'full-name)))
                         (list (funcall telega-rainbow-color-function
                                        user-title 'light)
                               (funcall telega-rainbow-color-function
                                        user-title 'dark))))))
        (plist-put user :color colors)
        colors)))

(defun telega-user--chats-in-common (with-user &optional callback)
  "Return CHATS in common WITH-USER."
  (declare (indent 1))
  (let ((gic-cnt (plist-get (telega--full-info with-user)
                            :group_in_common_count)))
    (unless (zerop gic-cnt)
      (telega--getGroupsInCommon with-user nil callback))))

(defun telega-describe-user--inserter (user-id)
  "Inserter for the user info buffer."
  (let ((user (telega-user-get user-id)))
    (let ((telega-user-show-relationship nil))
      (telega-ins--user user nil 'show-phone))
    (telega-ins "\n")
    (telega-info--insert-user user)))

(defun telega-describe-user (user)
  "Show info about USER."
  (interactive (list (telega-user-at (point))))
  ;; NOTE: `getUserFullInfo' might generate `updateUserFullInfo' event
  ;; causing redisplay *Telega User*, so we get user full info before
  ;; showing *Telega User* buffer
  (let ((telega-full-info-offline-p nil))
    (telega--full-info user))

  (with-telega-help-win "*Telega User*"
    ;; NOTE: Set these params *before* calling inserter, because
    ;; inserter might use these params.
    (setq telega--help-win-param (plist-get user :id))
    (setq telega--help-win-inserter #'telega-describe-user--inserter)

    (telega-describe-user--inserter (plist-get user :id))

    ;; Animate emoji status, if any
    (when-let ((emoji-status (plist-get user :emoji_status)))
      (telega-emoji-status--animate emoji-status))
    ))

(defun telega-describe-user--maybe-redisplay (user-id)
  "Possible redisplay \\*Telega User\\* buffer for the USER-ID."
  (telega-help-win--maybe-redisplay "*Telega User*" user-id))

(defun telega-user-chat-with (user)
  "Start private chat with USER."
  (interactive (list (telega-user-at (point))))
  (telega-chat--pop-to-buffer
   (telega--createPrivateChat user)))

(defun telega-user-block (user &optional unblock-p)
  "Toggle block state of the USER.
If UNBLOCK-P is specified, then unblock USER."
  (interactive (list (telega-user-at (point))))

  (if unblock-p
      (telega-msg-sender-unblock user)
    (when (yes-or-no-p
           (telega-i18n "lng_blocked_list_confirm_text"
             :name (telega-msg-sender-title user
                     :with-avatar-p t
                     :with-username-p t)))
      (telega-msg-sender-block user))))

(defun telega-user> (user1 user2)
  "Compare two users USER1 and USER2."
  (let ((chat1 (telega-user-chat user1))
        (chat2 (telega-user-chat user2)))
    (cond ((and chat1 chat2)
           (let ((chatbuf1 (telega-chat-match-p chat1 'has-chatbuf))
                 (chatbuf2 (telega-chat-match-p chat2 'has-chatbuf)))
             (cond ((and chatbuf1 (not chatbuf2))
                    t)
                   ((and chatbuf2 (not chatbuf1))
                    nil)
                   (t
                    (telega-chats-compare nil chat1 chat2)))))
          (chat1 t)
          (chat2 nil)
          ((and (telega-user-online-p user1)
                (not (telega-user-online-p user2)))
           t)
          ((telega-user-online-p user2)
           nil)
          (t
           (let ((mutual1 (telega-user-match-p user1 '(contact mutual)))
                 (contact1 (telega-user-match-p user1 'contact))
                 (mutual2 (telega-user-match-p user2 '(contact mutual)))
                 (contact2 (telega-user-match-p user2 'contact)))
             (cond ((or (and mutual1 (not mutual2))
                        (and contact1 (not contact2)))
                    t)
                   ((or (and mutual2 (not mutual1))
                        (and contact2 (not contact1)))
                    nil)
                   (t
                    (string> (telega-user-title user1 'full-name 'no-badges)
                             (telega-user-title user2 'full-name 'no-badges))))
             )))))

(defun telega-user-cmp-by-status (user1 user2)
  "Function to sort users by their online status.
Return non-nil if USER1 > USER2."
  (cond ((telega-user-online-p user1) t)
        ((telega-user-online-p user2) nil)
        (t (let ((u1-last-online (plist-get user1 :telega-last-online))
                 (u2-last-online (plist-get user2 :telega-last-online)))
             (cond (u1-last-online (>= u1-last-online (or u2-last-online 0)))
                   (u2-last-online nil)
                   (t (>= (telega-user--seen user1 'as-number)
                          (telega-user--seen user2 'as-number))))))))

(defun telega-user-as-contact (user)
  "Return USER as \"contact\"."
  (list :@type "contact"
        :phone_number (when-let ((phone (telega-tl-str user :phone_number)))
                        (concat "+" phone))
        :first_name (telega-tl-str user :first_name)
        :last_name (telega-tl-str user :last_name)
        :user_id (plist-get user :id)))


;;; Contacts
(defun telega-contact-add (phone &optional name)
  "Add user by PHONE to contact list."
  (interactive (list (read-string "Phone number: ")
                     (read-string "Name: ")))
  (let* ((names (split-string (or name "") " "))
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
    (telega-describe-user (telega-user-get user-id))))

(defun telega-describe-contact--inserter (contact)
  "Inserter for the contact info buffer."
    (let* ((user (telega-user-get (plist-get contact :user_id)))
           (user-ava (when (and telega-user-show-avatars user)
                       (telega-msg-sender-avatar-image user))))
      (when user-ava
        (telega-ins--image user-ava 0))
      (telega-ins--contact contact
        :with-avatar-p nil)
      (telega-ins "\n")

      (when user-ava
        (telega-ins--image user-ava 1))
      (if (plist-get user :is_contact)
          (telega-ins--box-button (telega-i18n "lng_info_delete_contact")
            :value contact
            :action (lambda (contact)
                      (telega--removeContacts (plist-get contact :user_id))
                      (telega-save-cursor
                        (telega-describe-contact contact))))
        (telega-ins--box-button (telega-i18n "lng_new_contact_add")
          :value contact
          :action (lambda (contact)
                    (telega--addContact contact)
                    (telega-save-cursor
                      (telega-describe-contact contact)))))
      (telega-ins "\n")

      (telega-ins "\n")
      (telega-ins--with-face 'telega-describe-section-title
        (telega-ins (upcase (telega-i18n "lng_info_user_title")) "\n"))

      (telega-ins-describe-item "Name"
        (telega-ins (telega-user-title user 'full-name)))
      (telega-info--insert-user user))
    )

(defun telega-describe-contact (contact)
  "Show CONTACT information."
  (with-telega-help-win "*Telega Contact*"
    (telega-describe-contact--inserter contact)

    (setq telega--help-win-param (plist-get contact :user_id))
    (setq telega--help-win-inserter #'telega-describe-contact--inserter)
    ))

(defun telega-describe-contact--maybe-redisplay (user-id)
  "Possible redisplay \\*Telega Contact\\* buffer for the USER-ID."
  (when-let ((contact (when-let ((help-buf (get-buffer "*Telega Contact*")))
                        (with-current-buffer help-buf
                          telega--help-win-param))))
    (when (eq user-id (plist-get contact :user_id))
      (telega-help-win--maybe-redisplay "*Telega Contact*" contact))))


;;; Close Friends
(defun telega-describe-close-friends ()
  "List close friends."
  (interactive)
  (let ((help-window-select t))
    (with-telega-help-win "*Telegram Close Friends*"
      (save-excursion
        (telega-ins-describe-item (telega-i18n "lng_edit_privacy_close_friends")
          (telega-ins--box-button "Add"
            'action (lambda (_button)
                      (call-interactively #'telega-close-friend-add)
                      (telega-save-cursor
                        (telega-describe-close-friends))))
          (telega-ins " ")
          (telega-ins--box-button "Remove"
            'action (lambda (_button)
                      (call-interactively #'telega-close-friend-remove)
                      (telega-save-cursor
                        (telega-describe-close-friends))))
          (telega-ins "\n")
          (seq-doseq (friend (telega-user-list 'is-close-friend))
            (telega-ins--user friend nil :with-phone)
            (telega-ins "\n")))))))

(defun telega-close-friend-add (user &optional callback)
  "Add USER to the list of close friends."
  (interactive (list (telega-completing-read-user "Add Close Friend: "
                       (telega-user-list '(not is-close-friend)))))
  (telega--setCloseFriends
   (cons user (telega-user-list 'is-close-friend))
   callback))

(defun telega-close-friend-remove (user &optional callback)
  "Remove USER from list of close friends."
  (interactive (list (telega-completing-read-user "Remove Close Friend: "
                       (telega-user-list 'is-close-friend))))
  (telega--setCloseFriends
   (delq user (telega-user-list 'is-close-friend))
   callback))

(provide 'telega-user)

;;; telega-user.el ends here

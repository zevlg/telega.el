;;; telega-info.el --- Users/Secrets/Groups stuff for telega  -*- lexical-binding:t -*-

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Apr 20 00:24:21 2018
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
(require 'telega-tdlib)
(require 'telega-util)
(require 'telega-filter)
(require 'telega-chat)


;; Info
(defmacro telega--info-update (tlobj)
  `(puthash (plist-get ,tlobj :id) ,tlobj
            (cdr (assq (telega--tl-type ,tlobj) telega--info))))

(defun telega--info (tlobj-type tlobj-id &optional locally-p)
  (let* ((info-hash (alist-get tlobj-type telega--info))
         (info (gethash tlobj-id info-hash)))
    (unless info
      (unless locally-p
        (cl-assert (not (zerop tlobj-id)))
        (setq info (telega-server--call
                    (cl-ecase tlobj-type
                      (user
                       `(:@type "getUser" :user_id ,tlobj-id))
                      (secretChat
                       `(:@type "getSecretChat" :secret_chat_id ,tlobj-id))
                      (basicGroup
                       `(:@type "getBasicGroup" :basic_group_id ,tlobj-id))
                      (supergroup
                       `(:@type "getSupergroup" :supergroup_id ,tlobj-id)))))
        (cl-assert info nil "getting info for %S(id=%S) timeout"
                   tlobj-type tlobj-id)
        (puthash tlobj-id info info-hash)))
    info))


;; FullInfo
(defun telega--full-info (tlobj &optional _callback)
  "Get FullInfo for the TLOBJ.
TLOBJ could be one of: user, basicgroup or supergroup.
If OFFLINE-P is non-nil, then do not send a request to telega-server."
  (let* ((tlobj-type (telega--tl-type tlobj))
         (tlobj-id (plist-get tlobj :id))
         (fi-hash (cdr (assq tlobj-type telega--full-info)))
         (full-info (gethash tlobj-id fi-hash)))
    (when (and (not full-info) (not telega-full-info-offline-p))
      (setq full-info
            (telega-server--call
             (cl-ecase tlobj-type
               (user
                `(:@type "getUserFullInfo" :user_id ,tlobj-id))
               (basicGroup
                `(:@type "getBasicGroupFullInfo" :basic_group_id ,tlobj-id))
               (supergroup
                `(:@type "getSupergroupFullInfo" :supergroup_id ,tlobj-id)))))
      (cl-assert full-info nil
                 "getting full-info for type=%S(%S) timeout" tlobj-type tlobj-id)
      (puthash tlobj-id full-info fi-hash))
    full-info))


(defun telega--account-ttl-button-value ()
  "Return value for the Account TTL button."
  (telega-i18n "lng_self_destruct_months"
    :count (round (/ (telega--getAccountTtl) 30.4))))

(defun telega--account-ttl-button-action (button)
  "Action to take when Account TTL button is pressed."
  (let* ((choices
          (mapcar (lambda (nmonth)
                    (cons (telega-i18n "lng_self_destruct_months"
                            :count nmonth)
                          (round (* nmonth 30.4))))
                  '(1 3 6 12)))
         (nmonth (funcall telega-completing-read-function
                          (concat (telega-i18n "lng_settings_destroy_if")
                                  " ")
                          (mapcar 'car choices) nil t))
         (days (cdr (assoc nmonth choices))))
    (telega--setAccountTtl days)

    (telega-save-cursor
      (telega-button--change button
        (telega-ins--box-button (telega--account-ttl-button-value)
          'action #'telega--account-ttl-button-action)))))

(defun telega-ins--account-ttl ()
  "Insert account TTL settings."
  (telega-ins-describe-item (telega-i18n "lng_self_destruct_title")
    (telega-ins (telega-i18n "lng_settings_destroy_if") " ")
    (telega-ins--box-button (telega--account-ttl-button-value)
      'action #'telega--account-ttl-button-action)
    (telega-ins "\n")
    (telega-ins--help-message
     (telega-ins-i18n "lng_self_destruct_description")
     ;; NOTE: no newline
     nil)))

(defun telega-ins--chat-as-sender (chat)
  "Insert CHAT as message sender."
  (telega-ins--msg-sender chat
    :with-avatar-p t
    :with-username-p 'telega-username
    :with-brackets-p t))

(defun telega-ins--chat-photo (chat-photo)
  "Inserter for user CHAT-PHOTO (TDLib's ChatPhoto)."
  (if-let ((fake-anim (plist-get chat-photo :telega-fake-animation)))
      (telega-ins--animation-image fake-anim)

    (telega-ins--image
     (telega-photo--image
      chat-photo
      (list telega-user-photo-size
            telega-user-photo-size
            telega-user-photo-size
            telega-user-photo-size)))))

(defun telega-ins--user-profile-photos (user &optional profile-photos
                                             redisplay-func)
  "Insert USER's profile photos."
  (declare (indent 2))
  (when-let ((profile-photos (or profile-photos
                                 (telega--getUserProfilePhotos user))))
    (dolist (photo profile-photos)
      ;; NOTE: For animated user profile photos, create fake animation
      ;; to be used for animation when cursor enters the photo
      (when-let ((photo-anim (plist-get photo :animation))
                 (size1 (car (append (plist-get photo :sizes) nil))))
        (plist-put photo :telega-fake-animation
                   (list :@type "animation"
                         :width (plist-get photo-anim :length)
                         :height (plist-get photo-anim :length)
                         :minithumbnail (plist-get photo :minithumbnail)
                         :thumbnail (list :@type "thumbnail"
                                          :format (list :@type "thumbnailFormatJpeg")
                                          :width (plist-get size1 :width)
                                          :height (plist-get size1 :height)
                                          :file (plist-get size1 :photo))
                         :animation (plist-get photo-anim :file))))

      ;; (when (> (telega-current-column) fill-column)
      ;;   (telega-ins "\n"))
      (telega-button--insert 'telega photo
        :inserter #'telega-ins--chat-photo
        :action 'telega-photo--open
        'cursor-sensor-functions
        (when-let ((fake-anim (plist-get photo :telega-fake-animation)))
          (list (telega-animation--gen-sensor-func fake-anim)))
        'local-keymap
        (when (telega-me-p user)
          (let ((pp-del (lambda (profile-photo)
                          (interactive (list (button-get
                                              (button-at (point)) :value)))
                          (telega--deleteProfilePhoto
                              (plist-get profile-photo :id)
                            (when redisplay-func
                              (lambda (_ignored)
                                (telega-save-cursor
                                  (funcall redisplay-func)))))))
                (map (make-sparse-keymap)))
            (define-key map (kbd "DEL") pp-del)
            (define-key map (kbd "d") pp-del)
            map))
        'help-echo (when (telega-me-p user)
                     "Press `d' to delete profile photo"))
      (telega-ins " "))))

(defun telega-info--insert-user (user &optional chat)
  "Insert USER info into current buffer."
  (unless chat
    (setq chat (telega-user-chat user)))

  ;; Scam/Fake/Blacklist status
  (telega-ins-prefix telega-symbol-blocked
    (telega-ins--with-face 'error
      (when (plist-get user :is_fake)
        (telega-ins (telega-i18n "lng_fake_badge") " "))
      (when (plist-get user :is_scam)
        (telega-ins (telega-i18n "lng_scam_badge") " "))
      (when (telega-user-match-p user 'is-blocked)
        (telega-ins "BLOCKED "))))

  ;; Buttons line
  ;; I18N: profile_send_message -> Chat With
  (telega-ins--box-button (telega-i18n "lng_profile_send_message")
    :value user
    :action 'telega-user-chat-with)
  (telega-ins " ")
  (unless (or (telega-user-bot-p user)
              (plist-get user :is_contact))
    ;; I18N: profile_share_contact -> Share My Contact
    (telega-ins--box-button (telega-i18n "lng_profile_share_contact")
      :value chat
      :action 'telega-chat-share-my-contact)
    (telega-ins " "))
  ;; NOTE: Secret chat with bots and myself is not possible
  (unless (or (telega-user-bot-p user)
              (telega-me-p user))
    ;; TODO: search for existing secret chat with Ready state and
    ;; create [Open Secret Chat] button instead
    (telega-ins--box-button (concat (telega-symbol 'lock) "Start Secret Chat")
      :value user
      :action (lambda (user)
                (telega-chat--pop-to-buffer
                 (telega--createNewSecretChat user))))
    (telega-ins " "))

  (let* ((telega-full-info-offline-p nil)
         (full-info (telega--full-info user))
         (user-blocked-p (telega-user-match-p user 'is-blocked)))
    (when (plist-get full-info :can_be_called)
      (telega-ins--box-button (concat (telega-symbol 'phone) "Call")
        :value user
        :action #'telega-voip-call)
      (telega-ins " "))
    (unless (telega-me-p user)
      (telega-ins--box-button
          (concat (telega-symbol 'blocked)
                  (if user-blocked-p
                      ;; I18N: profile_unblock_user -> Unblock User
                      (telega-i18n "lng_profile_unblock_user")
                    ;; I18N: profile_block_user -> Block User
                    (telega-i18n "lng_profile_block_user")))
        'action (lambda (_ignored)
                  (if user-blocked-p
                      (telega-msg-sender-unblock user)
                    (telega-msg-sender-block user)))))
    (telega-ins "\n")

    ;; Clickable user's profile photos
    (telega-ins-describe-item "Profile Photos"
      (telega-help-win--add-tdlib-callback
       (telega--getUserProfilePhotos user nil nil
         (telega--gen-ins-continuation-callback 'loading
           (lambda (profile-photos)
             (telega-ins-fmt "%d\n" (length profile-photos))
             (telega-ins--line-wrap-prefix "  "
               (telega-ins--user-profile-photos user profile-photos)))
           telega--help-win-param))))

    ;; Status emoji stickerset
    (when-let* ((emoji-status (plist-get user :emoji_status))
                (emoji-sticker (telega-custom-emoji-get
                                (plist-get emoji-status :custom_emoji_id)))
                (sset-id (plist-get emoji-sticker :set_id)))
      (telega-ins-describe-item "Emoji Status Stickerset"
        (telega-stickerset-get sset-id nil
          (telega--gen-ins-continuation-callback 'loading
            (lambda (sset)
              (telega-ins--box-button (telega-stickerset-title sset)
                :value sset
                :action #'telega-describe-stickerset))))))

    (when-let ((patron (telega-msg-sender-patron-p user)))
      (telega-ins-describe-item "Telega Patron Since"
        (telega-ins--date (plist-get patron :since_date) 'date-long)))

    (telega-ins-describe-item "Id"
      (telega-ins-fmt (if telega-debug
                          "(telega-user-get %d)"
                        "%d")
        (plist-get user :id)))
    (when (telega-me-p user)
      ;; Saved Messages
      (telega-ins-describe-item "Logged in"
        (telega-ins--date
         (plist-get telega--options :authorization_date) 'date-long))
      (telega-ins--account-ttl))

    (let ((relationship (telega-ins--as-string
                         (telega-ins--user-relationship user))))
      (unless (string-empty-p relationship)
        (telega-ins-describe-item "Relationship"
          (telega-ins relationship))))

    (when-let ((birthdate (plist-get full-info :birthdate)))
      (telega-ins-describe-item (telega-i18n "lng_info_birthday_label")
        (telega-ins--birthdate birthdate 'with-years-old)))

    (when-let ((username (telega-msg-sender-username user)))
      ;; I18N: profile_username -> Username:
      (telega-ins-describe-item (telega-i18n "lng_profile_username")
        (telega-ins "@" username)

        ;; Also insert other usernames
        (let ((other-usernames
               (seq-rest (telega--tl-get user :usernames :active_usernames))))
          (unless (seq-empty-p other-usernames)
            (telega-ins--with-face 'telega-shadow
              (telega-ins " " (telega-i18n "lng_info_usernames_label") " "))
            (telega-ins--sequence (other-username other-usernames)
              (telega-ins--with-face 'telega-shadow
                (telega-ins ", "))
              (telega-ins "@" other-username))))))

    (when-let* ((personal-chat-id (plist-get full-info :personal_chat_id))
                (personal-chat (unless (telega-zerop personal-chat-id)
                                 (telega-chat-get personal-chat-id))))
      (telega-ins-describe-item (telega-i18n "lng_settings_channel_label")
        (telega-button--insert 'telega-chat personal-chat
          :inserter #'telega-ins--chat-as-sender)
        (let ((subs (plist-get (telega-chat--info personal-chat) :member_count)))
          (unless (telega-zerop subs)
            (telega-ins--with-face 'telega-shadow
              (telega-ins " " (telega-i18n "lng_chat_status_subscribers"
                                :count subs)))))))

    (when-let ((phone (telega-tl-str user :phone_number)))
      ;; I18N: profile_mobile_number -> Phone:
      (telega-ins-describe-item (telega-i18n "lng_profile_mobile_number")
        (telega-ins "+" phone)))

    ;; Online status
    (telega-ins-describe-item "Last seen"
      (cond ((telega-user-online-p user)
             (telega-ins-i18n "lng_status_online"))
            ((plist-get user :telega-last-online)
             (if (stringp telega-user-last-seen-date-format)
                 (telega-ins--date (plist-get user :telega-last-online)
                                   telega-user-last-seen-date-format)
               ;; Relative
               (telega-ins (telega-duration-human-readable
                            (- (telega-time-seconds)
                               (plist-get user :telega-last-online))
                            1 'long)
                           " ago")))
            (t
             (telega-ins (telega-user--seen user)))))

    (when-let ((bio (telega-tl-str full-info :bio)))
      ;; I18N: profile_bio -> Bio:
      (telega-ins-describe-item (telega-i18n "lng_profile_bio")
        (telega-ins "\n")
        (telega-ins--line-wrap-prefix "  "
          (telega-ins bio))))

    (when-let ((share-text (telega-tl-str full-info :short_description)))
      (telega-ins-describe-item "Share text"
        (telega-ins "\n")
        (telega-ins--line-wrap-prefix "  "
          (telega-ins share-text))))

    ;; Bot Info & Bot Commands
    (when-let ((bot-info (plist-get full-info :bot_info)))
      ;; TODO: insert info from BOT-INFO
      (when-let ((bot-cmds (append (plist-get bot-info :commands) nil)))
        (telega-ins-describe-item "Bot commands"
          (telega-ins--line-wrap-prefix "  "
            (seq-doseq (cmd bot-cmds)
              (telega-ins "\n")
              (telega-ins--with-attrs (list :min 10 :align 'left)
                (telega-ins "/" (telega-tl-str cmd :command)))
              (when-let ((cmd-descr (telega-tl-str cmd :description)))
                (telega-ins--column nil nil
                  (telega-ins " - " cmd-descr))))))))

    ;; Chats common with USER
    (let ((gic-cnt (plist-get full-info :group_in_common_count)))
      (unless (zerop gic-cnt)
        (telega-ins-describe-item (telega-i18n "lng_profile_common_groups"
                                    :count gic-cnt)
          (telega-help-win--add-tdlib-callback
           (telega--getGroupsInCommon user nil
             (telega--gen-ins-continuation-callback 'loading
               (lambda (chats-in-common)
                 (when chats-in-common
                   (dolist (chat chats-in-common)
                     (telega-ins "\n")
                     (telega-ins "  ")
                     (telega-button--insert 'telega-chat chat
                       :inserter #'telega-ins--chat))))
               telega--help-win-param))))))

    ;; TODO: view shared media as thumbnails

    (let ((call (telega-voip--by-user-id (plist-get user :id))))
      (when (and call (listp telega-debug) (memq 'info telega-debug))
        (telega-ins "\n---DEBUG---\n")
        (telega-ins-fmt "Call: %S\n" call)))
    ))

(defun telega-info--insert-secretchat (secretchat chat)
  "Insert info about SECRETCHAT into current buffer."
  (telega-ins-describe-item "State"
    (telega-ins (substring (telega--tl-get secretchat :state :@type) 15))
    (cl-case (telega--tl-type (plist-get secretchat :state))
      (secretChatStateClosed
       (telega-ins " ")
       (telega-ins--box-button "Delete and Exit"
         :value chat
         :action (lambda (chat)
                   (telega-chat-delete chat)
                   (quit-window))))

      ((secretChatStatePending secretChatStateReady)
       (telega-ins " ")
       (telega-ins--box-button "Close Secret Chat"
         :value chat
         :action (lambda (chat)
                   (telega--closeSecretChat secretchat)
                   (telega-save-cursor
                     (telega-describe-chat chat)))))))

  (telega-ins-describe-item "Created"
    (telega-ins (if (plist-get secretchat :is_outbound)
                    "me"
                  "him")))

  (telega-ins-describe-item "Layer"
    (telega-ins-fmt "%d" (plist-get secretchat :layer))
    (when (>= (plist-get secretchat :layer) 66)
      (telega-ins " (Video notes are supported)")))

  ;; Encryption key
  (let ((enc-key (plist-get secretchat :key_hash)))
    (when (and enc-key (not (string-empty-p enc-key)))
      (telega-ins-describe-item "Key"
        (telega-ins--line-wrap-prefix "  "
          (telega-ins "\n")
          (let ((ekey (base64-decode-string enc-key))
                (efaces (list 'telega-enckey-00 'telega-enckey-01
                              'telega-enckey-10 'telega-enckey-11)))
            (dotimes (ki (length ekey))
              (when (and (> ki 0) (= (% ki 3) 0))
                (telega-ins "\n"))
              (let* ((kv (aref ekey ki))
                     (k1 (logand kv 3))
                     (k2 (ash (logand kv 12) -2))
                     (k3 (ash (logand kv 48) -4))
                     (k4 (ash (logand kv 192) -6)))
                (dolist (kk (list k1 k2 k3 k4))
                  (telega-ins (propertize telega-symbol-square
                                          'face (nth kk efaces))))))
            (telega-ins "\n")

            ;; NOTE: TDLib docs: Alternatively, the first 32 bytes of
            ;; the hash can be converted to the hexadecimal format and
            ;; printed as 32 2-digit hex numbers
            (cl-assert (<= 32 (length ekey)))
            (dotimes (ki 32)
              (cond ((and (> ki 0) (= (% ki 8) 0))
                     (telega-ins "\n"))
                    ((= (% ki 8) 4)
                     (telega-ins " ")))
              (telega-ins-fmt "%02x " (aref ekey ki))))))
      )))

(defun telega-info--insert-invite-link (chat chat-invite-link)
  "Insert CHAT-INVITE-LINK into current info buffer."
  ;; NOTE: maybe use `checkChatInviteLink' to check the invitation
  ;; link for validity?
  (let ((can-generate-p (and (telega-chat-match-p chat '(me-is-owner or-admin))
                             (plist-get (telega-chat-member-my-permissions chat)
                                        :can_invite_users)))
        (invite-link (telega-tl-str chat-invite-link :invite_link)))
    (when (or can-generate-p invite-link)
      (telega-ins-describe-item (telega-i18n "lng_create_invite_link_title")
        (when invite-link
          (telega-ins--raw-button (telega-link-props 'url invite-link 'face 'link)
            (telega-ins invite-link)))
        (when can-generate-p
          (telega-ins " ")
          (telega-ins--box-button (if invite-link
                                      "Regenerate"
                                    (telega-i18n "lng_group_invite_add"))
            :value chat
            :action #'telega-chat-generate-invite-link))
        ))))

(defun telega-info--insert-basicgroup (basicgroup chat)
  (let* ((full-info (telega--full-info basicgroup))
         (members (append (plist-get full-info :members) nil))
         (member-status-name (plist-get (plist-get basicgroup :status) :@type)))
    (telega-ins-describe-item "Status"
      (telega-ins (substring member-status-name 16)))
    (when-let ((creator-id (plist-get full-info :creator_user_id))
               (creator (unless (zerop creator-id)
                          (telega-user-get creator-id))))
      (telega-ins-describe-item "Created"
        (telega-ins--msg-sender creator
          :with-avatar-p t
          :with-username-p t
          :with-brackets-p t)
        (telega-ins "  ")
        (when-let ((creator-member
                    (cl-find creator members :key #'telega-msg-sender)))
          (telega-ins--date (plist-get creator-member :joined_chat_date)))))

    (telega-info--insert-invite-link chat (plist-get full-info :invite_link))

    (when-let ((descr (telega-tl-str full-info :description)))
      (telega-ins-describe-item (telega-i18n "lng_info_about_label")
        (telega-ins--line-wrap-prefix "  "
          (telega-ins "\n" descr))))

    (telega-ins-describe-item (telega-i18n "lng_profile_participants_section")
      (telega-ins-fmt "%d%s (%d online, %d admins)"
        (plist-get basicgroup :member_count)
        (telega-symbol 'member)
        (or (plist-get chat :x-online-count) 0)
        (length (cl-remove-if-not
                 (lambda (member)
                   (memq (telega--tl-type (plist-get member :status))
                         '(chatMemberStatusCreator
                           chatMemberStatusAdministrator)))
                 members)))
      (telega-ins " ")
      (telega-ins--box-button (telega-i18n "telega_show")
        :value chat
        :action #'telega-describe-chat-members))))

(defun telega-info--insert-supergroup (supergroup chat)
  (let* ((telega-full-info-offline-p nil)
         (full-info (telega--full-info supergroup))
         (member-status (plist-get supergroup :status))
         (my-perms (telega-chat-member-my-permissions chat))
         (member-status-name (plist-get member-status :@type)))
    (cl-assert full-info)
    ;; Scam status first
    (when (telega-ins-prefix telega-symbol-blocked
            (telega-ins--with-face 'error
              (when (plist-get supergroup :is_fake)
                (telega-ins (telega-i18n "lng_fake_badge") " "))
              (when (plist-get supergroup :is_scam)
                (telega-ins (telega-i18n "lng_scam_badge") " "))))
      (telega-ins "\n"))

    (telega-ins-describe-item (if (telega-chat-match-p chat 'me-is-member)
                                  "Joined at"
                                "Created at")
      (telega-ins--date (plist-get supergroup :date)))

    ;; Supergroup username, only owner can set/change group's username
    (let* ((usernames (plist-get supergroup :usernames))
           (username (plist-get usernames :editable_username))
           (can-set-username-p (telega-chat-match-p chat 'me-is-owner)))
      (when (or username can-set-username-p)
        (telega-ins-describe-item (telega-i18n "lng_info_username_label")
          (when username
            (telega-ins "@" username " "))
          (when can-set-username-p
            (telega-ins--box-button (if username "Change" "Set")
              'action (lambda (_ignored)
                        (let ((new-username
                               (read-string "Set Username to: " "@")))
                          (when (string-prefix-p "@" new-username)
                            (setq new-username (substring new-username 1)))
                          (telega--setSupergroupUsername
                           supergroup new-username)))))
          )))

    (let ((boost-level (plist-get supergroup :boost_level)))
      (unless (telega-zerop boost-level)
        (telega-ins-describe-item (telega-i18n "lng_boosts_title")
          (telega-ins-i18n "lng_boost_level"
            :count boost-level))

        (let ((my-boosts (plist-get full-info :my_boost_count))
              (unrestrict-boosts (plist-get full-info :unrestrict_boost_count)))
          (unless (and (telega-zerop my-boosts)
                       (telega-zerop unrestrict-boosts))
            (telega-ins-describe-item "My Boosts"
              (telega-ins-fmt "%d" my-boosts)
              (telega-ins "\n")
              (telega-ins--help-message
               (telega-ins-fmt "Boost more %d times to ignore slow mode \
and chat permission restrictions"
                 unrestrict-boosts)
               ;; NOTE: no newline at the end
               nil))))))

    (telega-ins-describe-item "Status"
      (telega-ins (substring member-status-name 16))
      ;; Buttons for the owner of the group
      (telega-ins--line-wrap-prefix "  "
        (when (telega-chat-match-p chat 'me-is-owner)
          (let ((channel-p (telega-chat-channel-p chat)))
            ;; Delete supergroup/channel for everyone
            (telega-ins " ")
            (telega-ins--box-button
                (telega-i18n (if channel-p
                                 "lng_profile_delete_channel"
                               "lng_profile_delete_group"))
              'action (lambda (_ignored)
                        (when (telega-read-im-sure-p
                               (telega-i18n (if channel-p
                                                "lng_sure_delete_channel"
                                              "lng_sure_delete_group")))
                          (telega--deleteChat chat))))

            (telega-ins " ")
            (telega-ins--box-button (telega-i18n (if channel-p
                                                 "lng_rights_transfer_channel"
                                               "lng_rights_transfer_group"))
              :value chat
              :action #'telega-chat-transfer-ownership)
            ))))

    ;; Buttons for administrators
    (when (telega-chat-match-p chat '(me-is-owner or-admin))
      (let* ((owner-p (telega-chat-match-p chat 'me-is-owner))
             (my-status (telega-chat-member-my-status chat))
             (my-perms (telega-chat-member-my-permissions chat))
             (can-edit-p (plist-get my-perms :can_be_edited))
             (channel-p (telega-chat-channel-p chat))
             (custom-title (telega-tl-str my-status :custom_title)))
        ;; Custom Title:
        (when (or custom-title can-edit-p)
          (telega-ins-describe-item
              (telega-i18n "lng_rights_edit_admin_rank_name")
            (when (telega-ins custom-title)
              (telega-ins " "))
            (when can-edit-p
              (telega-ins--box-button "Set"
                'action (lambda (_ignored)
                          (let ((new-title (read-string "My Custom title: ")))
                            (plist-put my-status :custom_title new-title)
                            (telega--setChatMemberStatus
                             chat (telega-user-me) my-status)))))
            (telega-ins "\n")
            (telega-ins--help-message
             (telega-ins-i18n "lng_rights_edit_admin_rank_about"
               :title (telega-i18n (if owner-p
                                       "lng_owner_badge"
                                     "lng_admin_badge")))
             ;; NOTE: no newline at the end
             nil)))

        ;; Admin Permissions:
        (telega-ins-describe-item (telega-i18n "lng_rights_edit_admin_header")
          (telega-ins--line-wrap-prefix "  "
            (dolist (perm-spec telega-chat--admin-permissions)
              ;; list only those permissions which has title
              ;; NOTE: special permissions applies for channels only,
              ;; defined by `telega-chat--admin-permissions-for-channels'
              (when (and (cdr perm-spec)
                         (or channel-p
                             (not (memq (car perm-spec)
                                        telega-chat--admin-permissions-for-channels))))
                (telega-ins "\n")
                (let ((perm-value (plist-get my-perms (car perm-spec)))
                      (active-p (and can-edit-p
                                     (or (not owner-p)
                                         (eq :is_anonymous (car perm-spec))))))
                  (telega-ins--text-button (if perm-value
                                               (telega-symbol 'checkbox-on)
                                             (telega-symbol 'checkbox-off))
                    'face (if active-p
                              'telega-link
                            'telega-shadow)
                    'action (when active-p
                              (lambda (_button)
                                (cl-case (car perm-spec)
                                  (:is_anonymous
                                   (plist-put my-status :is_anonymous
                                              (if perm-value :false t))
                                   (telega--setChatMemberStatus
                                    chat (telega-user-me) my-status))
                                  (t
                                   (user-error "Not yet implemented"))))))
                  (telega-ins " " (telega-i18n (cdr perm-spec))))))))))

    (when (plist-get my-perms :can_change_info)
      ;; All history setting is available to supergroups only, in
      ;; channels all history is always available
      (unless (telega-chat-channel-p chat)
        (let* ((all-history-available-p
                (plist-get full-info :is_all_history_available))
               (action-function
                (lambda (_ignored)
                  (telega--toggleSupergroupIsAllHistoryAvailable
                   supergroup (not all-history-available-p)))))
          (telega-ins-describe-item
              (telega-i18n "lng_manage_history_visibility_title")
            (telega-ins "\n")
            (telega-ins--line-wrap-prefix "  "
              (telega-ins--text-button (if all-history-available-p
                                           (telega-symbol 'radiobox-on)
                                         (telega-symbol 'radiobox-off))
                'face (if all-history-available-p
                          'telega-shadow
                        'telega-link)
                'action (unless all-history-available-p
                          action-function))
              (telega-ins
               " " (telega-i18n "lng_manage_history_visibility_shown") "\n")
              (telega-ins--help-message
               (telega-ins-i18n "lng_manage_history_visibility_shown_about")
               nil)

              (telega-ins "\n")
              (telega-ins--text-button (if all-history-available-p
                                           (telega-symbol 'radiobox-off)
                                         (telega-symbol 'radiobox-on))
                'face (if all-history-available-p
                          'telega-link
                        'telega-shadow)
                'action (when all-history-available-p
                          action-function))
              (telega-ins
               " " (telega-i18n "lng_manage_history_visibility_hidden") "\n")
              (telega-ins--help-message
               (telega-ins-i18n "lng_manage_history_visibility_hidden_about")
               nil)))))

      ;; Sign messages is available in channels only
      (when (telega-chat-channel-p chat)
        (telega-ins-describe-item (telega-i18n "lng_edit_sign_messages")
          (telega-ins--text-button (if (plist-get supergroup :sign_messages)
                                       (telega-symbol 'checkbox-on)
                                     (telega-symbol 'checkbox-off))
            'face 'telega-link
            'action (lambda (_ignored)
                      (telega--toggleSupergroupSignMessages
                       supergroup (not (plist-get supergroup :sign_messages)))))))
      )

    ;; Aggressive anti-spam mode
    (when (plist-get full-info :can_toggle_aggressive_anti_spam)
      (telega-ins-describe-item (telega-i18n "lng_manage_peer_antispam")
        (let ((anti-spam-p
               (plist-get full-info :has_aggressive_anti_spam_enabled)))
          (telega-ins--text-button (if anti-spam-p
                                       (telega-symbol 'checkbox-on)
                                     (telega-symbol 'checkbox-off))
            'face 'telega-link
            'action (lambda (_ignored)
                      (telega--toggleSupergroupHasAggressiveAntiSpamEnabled
                       supergroup (not anti-spam-p)))))
        (telega-ins "\n")
        (telega-ins--help-message
         (telega-ins-i18n "lng_manage_peer_antispam_about")
         ;; NOTE: No trailing newline
         nil)))

    ;; Slow Mode is available only for supergroups
    (unless (telega-chat-channel-p chat)
      (let* ((slow-mode-delay (plist-get full-info :slow_mode_delay))
             (smd-str (if (zerop slow-mode-delay)
                          (telega-i18n "lng_rights_slowmode_off")
                        (telega-duration-human-readable slow-mode-delay nil t))))
        (telega-ins-describe-item (telega-i18n "lng_rights_slowmode_header")
          (if (plist-get my-perms :can_restrict_members)
              (telega-ins--box-button smd-str
                'action (lambda (_ignored)
                          (telega--setChatSlowModeDelay
                           chat
                           (telega-completing-read-slow-mode-delay
                            (concat (telega-i18n "lng_rights_slowmode_header")
                                    ": ")))))
            (telega-ins smd-str))
          (telega-ins "\n")
          (telega-ins--help-message
           (telega-ins-i18n "lng_rights_slowmode_about")
           ;; NOTE: No trailing newline
           nil))))

    ;; Location based chats
    (when (plist-get supergroup :has_location)
      (let* ((chat-loc (plist-get full-info :location))
             (loc (plist-get chat-loc :location))
             (address (telega-tl-str chat-loc :address)))
        (telega-ins-describe-item "Location"
          (telega-ins (telega-location-to-string loc)))
        (when address
          (telega-ins-describe-item "Address"
            (telega-ins address)))))

    ;; Creator and admins can [re]generate invite link
    (telega-info--insert-invite-link chat (plist-get full-info :invite_link))

    (when-let ((descr (telega-tl-str full-info :description)))
      (telega-ins-describe-item (telega-i18n "lng_info_about_label")
        (telega-ins--line-wrap-prefix "  "
          (telega-ins "\n" descr))))
    (when-let ((restr-reason (telega-tl-str supergroup :restriction_reason)))
      (telega-ins--labeled "Restriction: " nil
        (telega-ins restr-reason "\n")))

    ;; Discussion group / Linked Chat
    (let ((linked-chat (telega-chat-linked-chat chat))
          (can-set-discussion-group-p
           (and (telega-chat-channel-p chat)
                (plist-get my-perms :can_change_info))))
      (when (or linked-chat can-set-discussion-group-p)
        (telega-ins-describe-item
            (if (telega-chat-channel-p chat)
                (telega-i18n "lng_manage_discussion_group")
              (telega-i18n "lng_manage_linked_channel"))
          (if linked-chat
              (telega-button--insert 'telega-chat linked-chat
                :inserter #'telega-ins--chat-as-sender)
            (telega-ins--with-face 'telega-shadow
              (telega-ins "None")))
          (when can-set-discussion-group-p
            (telega-ins " ")
            (telega-ins--box-button (if linked-chat "Unset" "Set")
              'action (lambda (_ignored)
                        (telega--setChatDiscussionGroup
                         chat (unless linked-chat
                                (telega-read-discussion-chat)))))))))

    ;; Similar channels
    (when (telega-chat-channel-p chat)
      (telega-ins-describe-item (telega-i18n "lng_similar_channels_title")
        (telega-help-win--add-tdlib-callback
         (telega--getChatSimilarChatCount chat nil
           (telega--gen-ins-continuation-callback 'loading
             (lambda (count)
               (telega-ins-fmt "%d" count)
               (unless (telega-zerop count)
                 (telega-ins " ")
                 (telega-ins--box-button
                     (telega-i18n "lng_similar_channels_view_all")
                   :value chat
                   :action #'telega-describe-chat-similar-channels)))
             telega--help-win-param)))))

    ;; Members
    (telega-ins "\n")
    (when (plist-get full-info :can_hide_members)
      (telega-ins-describe-item (telega-i18n "lng_profile_hide_participants")
        (let ((has-hidden-members-p (plist-get full-info :has_hidden_members)))
          (telega-ins--text-button (if has-hidden-members-p
                                       (telega-symbol 'checkbox-on)
                                     (telega-symbol 'checkbox-off))
            'face 'telega-link
            'action (lambda (_ignored)
                      (telega--toggleSupergroupHasHiddenMembers
                       supergroup (not has-hidden-members-p)))))
        (telega-ins "\n")
        (telega-ins--help-message
         (telega-ins-i18n "lng_profile_hide_participants_about")
         nil)))

    (telega-ins-describe-item (telega-i18n "lng_profile_participants_section")
      (telega-ins-fmt "%d%s (%d online, %d admins)"
        (plist-get full-info :member_count)
        (telega-symbol 'member)
        (or (plist-get chat :x-online-count) 0)
        (plist-get full-info :administrator_count))
      (when (plist-get full-info :can_get_members)
        (telega-ins " ")
        (telega-ins--box-button (telega-i18n "telega_show")
          :value chat
          :action #'telega-describe-chat-members)))

    (when (and (listp telega-debug) (memq 'info telega-debug))
      (insert "\n---DEBUG---\n")
      (insert (propertize "Info: " 'face 'bold)
              (format "%S" supergroup) "\n")
      (insert (propertize "Full-Info: " 'face 'bold)
              (format "%S" full-info) "\n"))))

(defun telega-describe-connected-websites (&optional websites)
  "Describe connected WEBSITES."
  (interactive)
  (with-telega-help-win "*Telega Connected Websites*"
    ;; Title
    (telega-ins-describe-section
     (telega-i18n "lng_settings_connected_title"))
    (telega-ins-describe-item (telega-i18n "lng_settings_connected_title")
      (telega-ins (if websites
                      (number-to-string (length websites))
                    (telega-i18n "lng_profile_loading")))
      (if (not websites)
          (telega--getConnectedWebsites 'telega-describe-connected-websites)

        (telega-ins " ")
        (telega-ins--box-button (telega-i18n "lng_settings_disconnect_all")
          'action (lambda (_ignore)
                    (when (yes-or-no-p
                           (concat (telega-i18n "lng_settings_disconnect_all_sure")
                                   " "))
                      (telega--disconnectAllWebsites)
                      (telega-describe-connected-websites))))
        (telega-ins "\n")
        (telega-ins--help-message
         (telega-ins-i18n "lng_settings_logged_in_description"))
        (telega-ins "\n")

        (telega-ins-describe-section
         (telega-i18n "lng_settings_logged_in_title"))
        (dolist (website websites)
          (let ((bot-user (telega-user-get (plist-get website :bot_user_id))))
            (telega-ins--raw-button
                (telega-link-props 'sender bot-user 'type 'telega)
              (telega-ins--msg-sender bot-user
                :with-avatar-p 2
                :with-username-p 'telega-username)))
          (let ((domain (telega-tl-str website :domain_name)))
            (telega-ins domain)
            (telega-ins " ")
            (telega-ins--box-button (telega-i18n "lng_settings_disconnect")
              'action (lambda (_ignore)
                        (when (yes-or-no-p
                               (concat (telega-i18n "lng_settings_disconnect_sure"
                                         :domain domain)
                                       " "))
                          (telega--disconnectWebsite (plist-get website :id))
                          (telega-describe-connected-websites))))
            (telega-ins "\n"))
          (telega-ins-describe-item (telega-i18n "lng_sessions_browser")
            (telega-ins (telega-tl-str website :browser)
                        " (" (telega-tl-str website :platform) ")"))
          (telega-ins-describe-item (telega-i18n "lng_sessions_ip")
            (telega-ins (telega-tl-str website :ip_address)))
          (telega-ins-describe-item (telega-i18n "lng_sessions_location")
            (telega-ins (telega-tl-str website :location))
            (telega-ins "\n")
            (telega-ins--help-message
             (telega-ins-i18n "lng_sessions_location_about")
             nil))
          (telega-ins-describe-item "Login"
            (telega-ins--date (plist-get website :log_in_date)))
          (telega-ins-describe-item "Last Active"
            (telega-ins--date (plist-get website :last_active_date)))
          (telega-ins "\n")
          )))
    ))

(defun telega-describe-active-sessions (&optional sessions)
  "Describe active SESSIONS."
  (interactive)
  (with-telega-help-win "*Telega Active Sessions*"
    (telega-ins-describe-section
     (concat (telega-i18n "lng_settings_sessions_title")
             ": " (if sessions
                      (number-to-string (length sessions))
                    (telega-i18n "lng_profile_loading"))))
    (if (not sessions)
        (telega--getActiveSessions 'telega-describe-active-sessions)

      (dolist (session sessions)
        (telega-ins (telega-tl-str session :application_name))
        (when-let (app-ver (telega-tl-str session :application_version))
          (telega-ins " v" app-ver))
        (telega-ins " ")
        (if (plist-get session :is_official_application)
            (telega-ins "(official)")
          (telega-ins-fmt "(ID:%s)" (plist-get session :api_id)))

        ;; Logout/Terminate button
        ;; see https://github.com/zevlg/telega.el/issues/113
        (if (plist-get session :is_current)
            (progn
              (telega-ins " ")
              (telega-ins--with-face 'bold
                (telega-ins "(" (telega-i18n "lng_sessions_header") ")"))
              (telega-ins " ")
              (telega-ins--box-button (telega-i18n "lng_settings_logout")
                'action (lambda (_ignore)
                          (when (yes-or-no-p "Really Logout? ")
                            (telega-logout)))))

          (telega-ins " ")
          (telega-ins--box-button (telega-i18n "lng_sessions_terminate")
            :value session
            :action (lambda (sess)
                      (when (yes-or-no-p
                             (format "Terminate '%s'? "
                                     (telega-tl-str sess :application_name)))
                        (telega--terminateSession (plist-get sess :id))
                        (telega-save-cursor
                          (telega-describe-active-sessions
                           (delq sess sessions)))))))
        (telega-ins "\n")

        (telega-ins--line-wrap-prefix "  "
          (let ((device (plist-get session :device_model))
                (platform (telega-tl-str session :platform))
                (sys-ver (telega-tl-str session :system_version)))
            (telega-ins-describe-item (telega-i18n "lng_sessions_system")
              (telega-ins device ", " platform (when platform " ") sys-ver)))
          (when-let ((ip (telega-tl-str session :ip_address)))
            (telega-ins-describe-item (telega-i18n "lng_sessions_ip")
              (telega-ins ip)))
          (when-let ((location (telega-tl-str session :location)))
            (telega-ins-describe-item (telega-i18n "lng_sessions_location")
              (telega-ins location "\n")
              (telega-ins--help-message
               (telega-ins-i18n "lng_sessions_location_about")
               nil)))
          (let ((login-ts (plist-get session :log_in_date)))
            (telega-ins-describe-item "First Login"
              (telega-ins--date login-ts 'date-long)))
          (let ((last-ts (plist-get session :last_active_date)))
            (telega-ins-describe-item "Last Login"
              (telega-ins--date last-ts 'date-long)))
          (when (plist-get session :is_password_pending)
            (telega-ins--with-face 'error
              (telega-ins-i18n "lng_sessions_incomplete"))
            (telega-ins "\n")
            (telega-ins--help-message
             (telega-ins-i18n "lng_sessions_incomplete_about"))))

        (telega-ins "\n")))))


;; Proxies code
(defun telega--pingProxies (proxies &optional callback)
  "Update ping stats for the PROXIES.
Call CALLBACK on updates."
  (let ((track-timeout nil))
    (dolist (proxy proxies)
      (let* ((proxy-id (plist-get proxy :id))
             (ping (assq proxy-id telega--proxy-pings))
             (currts (time-to-seconds)))
        (when (> currts (+ (or (cadr ping) 0) 60))
          (setq track-timeout t)
          (setf (alist-get proxy-id telega--proxy-pings) (cons currts nil))
          (telega-server--call
           (list :@type "pingProxy" :proxy_id proxy-id)
           (lambda (seconds)
             (setf (alist-get proxy-id telega--proxy-pings)
                   (cons (time-to-seconds) (plist-get seconds :seconds)))
             (when callback
               (funcall callback)))))))

    (when (and track-timeout callback)
      ;; to track ping timeouts
      (run-with-timer 10 nil callback))))

(defun telega--enableProxy (proxy)
  (telega-server--call
   (list :@type "enableProxy"
         :proxy_id (plist-get proxy :id))))

(defun telega--disableProxy ()
  (telega-server--send
   (list :@type "disableProxy")))

(defun telega-proxy-last-used (&optional proxies)
  "Return last time used proxy."
  (car (cl-sort (or (copy-sequence proxies) (telega--getProxies))
                '> :key (telega--tl-prop :last_used_date))))

(defun telega-proxy-enabled (&optional proxies)
  "Return enable proxy from PROXIES list."
  (cl-find t (or proxies (telega--getProxies))
           :test 'eq :key (telega--tl-prop :is_enabled)))

(defun telega-ins--network (only-current)
  "Insert network settings/stats."
  (let* ((proxies (telega--getProxies))
         (recent (telega-proxy-last-used proxies))
         (enabled (telega-proxy-enabled proxies)))
    (telega--pingProxies proxies 'telega-describe-network)
    (when proxies
      (cl-assert recent nil "No recent proxy")
      (telega-ins-describe-section "Proxy Settings")
      (telega-ins-describe-item "Use Proxy"
        (telega-ins--text-button (if enabled
                                     (telega-symbol 'checkbox-on)
                                   (telega-symbol 'checkbox-off))
          'face 'telega-link
          :value (and (not enabled) recent)
          :action (lambda (proxy)
                    (if proxy
                        (and (telega--enableProxy proxy)
                             (setq telega--conn-state 'ConnectingToProxy))
                      (telega--disableProxy))
                    (telega-describe-network)))
        (telega-ins "\n")
        (dolist (proxy proxies)
          (if (eq proxy enabled)
              (telega-ins (telega-symbol 'checkbox-on))

            (telega-ins--text-button
                (if (and (eq proxy recent) (eq proxy enabled))
                    (telega-symbol 'checkbox-on)
                  (telega-symbol 'checkbox-off))
              'face 'telega-link
              :value proxy
              :action (lambda (proxy)
                        (and (telega--enableProxy proxy)
                             (setq telega--conn-state 'ConnectingToProxy))
                        (telega-describe-network))))
          (telega-ins " ")
          (telega-ins--column nil nil
            (telega-ins-fmt "%s:%d, "
              (plist-get proxy :server) (plist-get proxy :port))

            (let ((ping (alist-get (plist-get proxy :id) telega--proxy-pings)))
              (telega-ins-fmt "%s, "
                (upcase (substring (telega--tl-get proxy :type :@type) 9)))
              (when (eq proxy enabled)
                (telega-debug "CONNECTION state: %S" telega--conn-state)
                (cl-case telega--conn-state
                  ((WaitingForNetwork ConnectingToProxy)
                   (telega-ins "connecting.., ")
                   (add-hook 'telega-connection-state-hook
                             'telega-describe-network))
                  (t
                   (telega-ins "connected, ")
                   (remove-hook 'telega-connection-state-hook
                                'telega-describe-network))))

              (cond ((cdr ping)
                     (telega-ins-fmt "%dms ping"
                       (round (* (cdr ping) 1000))))
                    ((> (- (time-to-seconds) (car ping)) 10)
                     ;; 10 seconds ping timeout
                     (telega-ins "unavailable"))
                    (t
                     (telega-ins "pinging..")))))
          (telega-ins "\n")))))

  (telega-ins-describe-section "Network Statistics")
  (let* ((net-stats (telega-server--call
                     (list :@type "getNetworkStatistics"
                           :only_current (or only-current :false))))
         (total-sent 0)
         (total-recv 0))
    (telega-ins-describe-item "Since"
      (telega-ins--date (plist-get net-stats :since_date)))
    (insert "\n")
    (mapc (lambda (entry)
            (cl-incf total-sent (plist-get entry :sent_bytes))
            (cl-incf total-recv (plist-get entry :received_bytes))

            (cl-ecase (telega--tl-type entry)
              (networkStatisticsEntryFile
               (telega-ins-fmt
                   "File: %s"
                 (substring
                  (plist-get (plist-get entry :file_type) :@type) 8)))
              (networkStatisticsEntryCall
               (telega-ins-fmt
                   "Call: %s" (telega-duration-human-readable
                               (plist-get entry :duration)))))
            (insert " via ")
            (cl-ecase (telega--tl-type (plist-get entry :network_type))
              (networkTypeNone (insert "Unknown"))
              (networkTypeMobile (insert "Mobile"))
              (networkTypeMobileRoaming (insert "Roaming"))
              (networkTypeWiFi (insert "Wi-Fi"))
              (networkTypeOther (insert "Ethernet")))
            (insert " ")
            (telega-ins-fmt "sent: %s received %s"
              (file-size-human-readable
               (plist-get entry :sent_bytes))
              (file-size-human-readable
               (plist-get entry :received_bytes)))
            (insert "\n"))
          (plist-get net-stats :entries))
    (insert "\n")
    (telega-ins-describe-item "Total sent"
      (telega-ins (file-size-human-readable total-sent)))
    (telega-ins-describe-item "Total recv"
      (telega-ins (file-size-human-readable total-recv)))
    ))

(defvar telega-describe-network--only-current nil)

(defun telega-describe-network (&optional only-current)
  "Show network settings/statistics.
If prefix argument is given, then show statistics only for current launch."
  (interactive "P")
  (if (called-interactively-p 'interactive)
      (with-telega-help-win "*Telega Network*"
        (setq telega-describe-network--only-current (not (null only-current)))
        (telega-ins--network telega-describe-network--only-current))

    (let ((buffer (get-buffer "*Telega Network*")))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (telega-save-cursor
              (erase-buffer)
              (telega-ins--network
               telega-describe-network--only-current))))))))

(defun telega--getUserPrivacySettingRules (setting)
  "Get privacy SETTING.
SETTING is one of `show-status', `allow-chat-invites' or `allow-calls'."
  (let ((privacy-setting
         (cl-ecase setting
           (show-status "userPrivacySettingShowStatus")
           (allow-chat-invites "userPrivacySettingAllowChatInvites")
           (allow-calls "userPrivacySettingAllowCalls"))))
    (telega-server--call
     (list :@type "getUserPrivacySettingRules"
           :setting (list :@type privacy-setting)))))

(defun telega-describe-privacy-settings ()
  "Show user privacy settings."
  (interactive)
  (with-telega-help-win "*Telega Privacy Settings*"
    ;; I18N: settings_privacy_title -> Privacy
    (telega-ins-describe-section
     (telega-i18n "lng_settings_privacy_title"))

    ;; NOTE: senders are only cddr of the reply from
    ;; `telega--getBlockedMessageSenders',
    ;; see comments in the `telega--on-blocked-senders-load'
    (when-let ((blocked-senders
                (cddr (telega--getBlockedMessageSenders 'blockListMain))))
      ;; I18N: blocked_list_title -> Blocked Users
      (telega-ins-describe-item (telega-i18n "lng_blocked_list_title")
        (telega-ins-fmt "%d" (length blocked-senders))
        (dolist (msg-sender blocked-senders)
          (telega-ins "\n")
          ;; I18N: profile_unblock_user -> Unblock User
          (telega-ins--box-button (telega-i18n "lng_profile_unblock_user")
            :value msg-sender
            :action #'telega-msg-sender-unblock)
          (telega-ins " ")
          (if (telega-user-p msg-sender)
              (telega-button--insert 'telega-user msg-sender
                :inserter #'telega-ins--user)
            (telega-button--insert 'telega-chat msg-sender
              :inserter #'telega-ins--chat)))))
    (dolist (setting '(show-status allow-chat-invites allow-calls))
      (telega-ins-fmt "%S: " setting)
      (telega-ins-fmt "%S" (telega--getUserPrivacySettingRules setting))
      (telega-ins "\n"))

    (telega-ins "\n")
    (telega-ins-describe-section
     (telega-i18n "lng_settings_section_privacy"))
    (telega-ins "TODO")
    (telega-ins "\n")

    (telega-ins "\n")
    (telega-ins-describe-section
     (telega-i18n "lng_settings_self_destruct"))
    (telega-ins--account-ttl)))

(defun telega-describe ()
  "Describe current telega state and configuration."
  (user-error "`telega-describe' not yet implemented")
  )

(provide 'telega-info)

;;; telega-info.el ends here

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
        (telega-ins--button (telega--account-ttl-button-value)
          'action #'telega--account-ttl-button-action)))))

(defun telega-ins--account-ttl ()
  "Insert account TTL settings."
  (telega-ins (telega-i18n "lng_self_destruct_title") ": "
              (telega-i18n "lng_settings_destroy_if") " ")
  (telega-ins--button (telega--account-ttl-button-value)
    'action #'telega--account-ttl-button-action)
  (telega-ins "\n")
  (telega-ins--help-message
   (telega-ins-i18n "lng_self_destruct_description")))

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

      (when (> (telega-current-column) fill-column)
        (telega-ins "\n"))
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
    (setq chat (telega-chat-get (plist-get user :id) 'offline)))

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
  (telega-ins--button (telega-i18n "lng_profile_send_message")
    :value user
    :action 'telega-user-chat-with)
  (telega-ins " ")
  (unless (or (telega-user-bot-p user)
              (plist-get user :is_contact))
    ;; I18N: profile_share_contact -> Share My Contact
    (telega-ins--button (telega-i18n "lng_profile_share_contact")
      :value chat :action 'telega-chat-share-my-contact)
    (telega-ins " "))
  ;; NOTE: Secret chat with bots and myself is not possible
  (unless (or (telega-user-bot-p user)
              (telega-me-p user))
    ;; TODO: search for existing secret chat with Ready state and
    ;; create [Open Secret Chat] button instead
    (telega-ins--button (concat (telega-symbol 'lock) "Start Secret Chat")
      :value user
      :action (lambda (user)
                (telega-chat--pop-to-buffer
                 (telega--createNewSecretChat user))))
    (telega-ins " "))

  (let* ((telega-full-info-offline-p nil)
         (full-info (telega--full-info user))
         (user-blocked-p (telega-user-match-p user 'is-blocked)))
    (when (plist-get full-info :can_be_called)
      (telega-ins--button (concat telega-symbol-phone "Call")
        :value user
        :action #'telega-voip-call)
      (telega-ins " "))
    (unless (telega-me-p user)
      (telega-ins--button
          (concat telega-symbol-blocked
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
    (telega-ins "Profile Photos: ")
    (telega--getUserProfilePhotos user nil nil
      (telega--gen-ins-continuation-callback 'loading
        (lambda (profile-photos)
          (telega-ins-fmt "%d\n" (length profile-photos))
          (telega-ins--user-profile-photos user profile-photos))))
    (telega-ins "\n")

    ;; Status emoji stickerset
    (when-let* ((emoji-status (plist-get user :emoji_status))
                (emoji-sticker (telega-custom-emoji-get
                                (plist-get emoji-status :custom_emoji_id)))
                (sset-id (plist-get emoji-sticker :set_id)))
      (telega-ins "Emoji Status Stickerset: ")
      (telega-stickerset-get sset-id nil
        (telega--gen-ins-continuation-callback 'loading
          (lambda (sset)
            (telega-ins--button (telega-stickerset-title sset)
              :value sset
              :action #'telega-describe-stickerset))
          telega--help-win-param))
      (telega-ins "\n"))

    (when-let ((patron (telega-msg-sender-patron-p user)))
      (telega-ins "Telega Patron Since: ")
      (telega-ins--date-full (plist-get patron :since_date))
      (telega-ins "\n"))

    (telega-ins-fmt "Id: %s\n"
      (if telega-debug
          (format "(telega-user-get %d)" (plist-get user :id))
        (format "%d" (plist-get user :id))))
    (when (telega-me-p user)
      ;; Saved Messages
      (telega-ins "Logged in: ")
      (telega-ins--date-full (plist-get telega--options :authorization_date))
      (telega-ins "\n")
      (telega-ins--account-ttl))

    (telega-ins-prefix "Relationship: "
      (when (telega-ins--user-relationship user)
        (telega-ins "\n")))

    (when-let ((username (telega-msg-sender-username user)))
      ;; I18N: profile_username -> Username:
      (telega-ins (telega-i18n "lng_profile_username")
                  " @" username)

      ;; Also insert other usernames
      (let ((other-usernames
             (seq-rest (telega--tl-get user :usernames :active_usernames))))
        (unless (seq-empty-p other-usernames)
          (telega-ins--with-face 'telega-shadow
            (telega-ins " " (telega-i18n "lng_info_usernames_label") " "))
          (telega-ins--sequence (other-username other-usernames)
            (telega-ins--with-face 'telega-shadow
              (telega-ins ", "))
            (telega-ins "@" other-username))))
      (telega-ins "\n"))
    (when-let ((phone (telega-tl-str user :phone_number)))
      ;; I18N: profile_mobile_number -> Phone:
      (telega-ins (telega-i18n "lng_profile_mobile_number")
                  " +" phone "\n"))
    ;; Online status
    (telega-ins "Last seen: ")
    (cond ((telega-user-online-p user)
           (telega-ins-i18n "lng_status_online"))
          ((plist-get user :telega-last-online)
           (telega-ins "in " (telega-duration-human-readable
                              (- (telega-time-seconds)
                                 (plist-get user :telega-last-online))
                              1 'long)))
          (t (telega-ins (telega-user--seen user))))
    (telega-ins "\n")

    (when-let ((bio (telega-tl-str full-info :bio)))
      ;; I18N: profile_bio -> Bio:
      (telega-ins--labeled (concat (telega-i18n "lng_profile_bio") " ") nil
        (telega-ins bio))
      (telega-ins "\n"))
    (when-let ((share-text (telega-tl-str full-info :short_description)))
      (telega-ins--labeled "Share text: " nil
        (telega-ins share-text))
      (telega-ins "\n"))

    ;; Bot Info & Bot Commands
    (when-let ((bot-info (plist-get full-info :bot_info)))
      ;; TODO: insert info from BOT-INFO
      (when-let ((bot-cmds (append (plist-get bot-info :commands) nil)))
        (telega-ins "Bot commands: \n")
        (seq-doseq (cmd bot-cmds)
          (telega-ins "  ")
          (telega-ins--with-attrs (list :min 10 :align 'left)
            (telega-ins "/" (telega-tl-str cmd :command)))
          (when-let ((cmd-descr (telega-tl-str cmd :description)))
            (telega-ins--column nil nil
              (telega-ins " - " cmd-descr)))
          (telega-ins "\n"))
        (telega-ins "\n")))

    ;; Chats common with USER
    (let ((gic-cnt (plist-get full-info :group_in_common_count)))
      (unless (zerop gic-cnt)
        (telega-ins (telega-i18n "lng_profile_common_groups"
                      :count gic-cnt)
                    ": ")
        (telega--getGroupsInCommon user nil
          (telega--gen-ins-continuation-callback 'loading
            (lambda (chats-in-common)
              (when chats-in-common
                (dolist (chat chats-in-common)
                  (telega-ins "\n")
                  (telega-ins "  ")
                  (telega-button--insert 'telega-chat chat
                    :inserter telega-inserter-for-chat-button))))))
        (telega-ins "\n")))

    ;; TODO: view shared media as thumbnails

    (let ((call (telega-voip--by-user-id (plist-get user :id))))
      (when (and call telega-debug)
        (telega-ins "\n---DEBUG---\n")
        (telega-ins-fmt "Call: %S\n" call)))
    ))

(defun telega-info--insert-secretchat (secretchat chat)
  "Insert info about SECRETCHAT into current buffer."
  (telega-ins "State: "
              (substring (telega--tl-get secretchat :state :@type) 15))
  (cl-case (telega--tl-type (plist-get secretchat :state))
    (secretChatStateClosed
     (telega-ins " ")
     (telega-ins--button "Delete and Exit"
       :value chat
       :action (lambda (chat)
                 (telega-chat-delete chat)
                 (quit-window))))

    ((secretChatStatePending secretChatStateReady)
     (telega-ins " ")
     (telega-ins--button "Close Secret Chat"
       :value chat
       :action (lambda (chat)
                 (telega--closeSecretChat secretchat)
                 (telega-save-cursor
                   (telega-describe-chat chat))))))
  (telega-ins "\n")
  (telega-ins "Created: " (if (plist-get secretchat :is_outbound)
                              "me" "him") "\n")
  (telega-ins-fmt "Layer: %d" (plist-get secretchat :layer))
  (when (>= (plist-get secretchat :layer) 66)
    (telega-ins " (Video notes are supported)"))
  (telega-ins "\n")

  ;; Encryption key
  (let ((enc-key (plist-get secretchat :key_hash)))
    (when (and enc-key (not (string-empty-p enc-key)))
      (telega-ins--labeled "Key: " nil
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
            (telega-ins-fmt "%02x " (aref ekey ki)))))
      (telega-ins "\n"))))

(defun telega-info--insert-invite-link (chat chat-invite-link)
  "Insert CHAT-INVITE-LINK into current info buffer."
  ;; NOTE: maybe use `checkChatInviteLink' to check the invitation
  ;; link for validity?
  (let ((can-generate-p (and (telega-chat-match-p chat '(me-is-owner or-admin))
                             (plist-get (telega-chat-member-my-permissions chat)
                                        :can_invite_users)))
        (invite-link (telega-tl-str chat-invite-link :invite_link)))
    (when (or can-generate-p invite-link)
      (telega-ins "Invite link:")
      (when invite-link
        (telega-ins " ")
        (telega-ins--raw-button (telega-link-props 'url invite-link 'face 'link)
          (telega-ins invite-link)))
      (when can-generate-p
        (telega-ins " ")
        (telega-ins--button (if invite-link "Regenerate" "Generate")
          :value chat
          :action #'telega-chat-generate-invite-link))
      (telega-ins "\n"))))

(defun telega-info--insert-basicgroup (basicgroup chat)
  (let* ((full-info (telega--full-info basicgroup))
         (members (append (plist-get full-info :members) nil))
         (member-status-name (plist-get (plist-get basicgroup :status) :@type)))
    (telega-ins "Status: " (substring member-status-name 16) "\n")
    (when-let ((creator-id (plist-get full-info :creator_user_id))
               (creator (unless (zerop creator-id)
                          (telega-user-get creator-id))))
      (telega-ins "Created: ")
      (telega-ins--msg-sender creator
        :with-avatar-p t
        :with-username-p t
        :with-brackets-p t)
      (telega-ins "  ")
      (when-let ((creator-member
                  (cl-find creator members :key #'telega-msg-sender)))
        (telega-ins--date (plist-get creator-member :joined_chat_date))
        (telega-ins "\n")))

    (telega-info--insert-invite-link chat (plist-get full-info :invite_link))

    (when-let ((descr (telega-tl-str full-info :description)))
      (telega-ins--labeled "Desc: " nil
        (telega-ins descr "\n")))

    (telega-ins "\n")
    (telega-ins-i18n "lng_profile_participants_section")
    (telega-ins-fmt ": %d%s (%d online, %d admins)\n"
      (plist-get basicgroup :member_count)
      (telega-symbol 'member)
      (or (plist-get chat :x-online-count) 0)
      (length (cl-remove-if-not
               (lambda (member)
                 (memq (telega--tl-type (plist-get member :status))
                       '(chatMemberStatusCreator chatMemberStatusAdministrator)))
               members)))
    (telega-ins--chat-members members)
    ))

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

    (telega-ins (if (telega-chat-match-p chat 'me-is-member)
                    "Joined at: "
                  "Created at: "))
    (telega-ins--date (plist-get supergroup :date))
    (telega-ins "\n")

    (telega-ins "Status: " (substring member-status-name 16))
    ;; Buttons for the owner of the group
    (when (telega-chat-match-p chat 'me-is-owner)
      (let ((channel-p (telega-chat-channel-p chat)))
        ;; Delete supergroup/channel for everyone
        (telega-ins " ")
        (telega-ins--button
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
        (telega-ins--button (telega-i18n (if channel-p
                                             "lng_rights_transfer_channel"
                                           "lng_rights_transfer_group"))
          :value chat
          :action #'telega-chat-transfer-ownership)
        ))
    (telega-ins "\n")
    ;; Buttons for administrators
    (when (telega-chat-match-p chat '(me-is-owner or-admin))
      (telega-ins--labeled "  " nil
        (let* ((owner-p (telega-chat-match-p chat 'me-is-owner))
               (my-status (telega-chat-member-my-status chat))
               (my-perms (telega-chat-member-my-permissions chat))
               (can-edit-p (plist-get my-perms :can_be_edited))
               (channel-p (telega-chat-channel-p chat))
               (custom-title (telega-tl-str my-status :custom_title)))
          ;; Custom Title:
          (when (or custom-title can-edit-p)
            (telega-ins (telega-i18n "lng_rights_edit_admin_rank_name") ": ")
            (when (telega-ins custom-title)
              (telega-ins " "))
            (when can-edit-p
              (telega-ins--button "Set"
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
                                     "lng_admin_badge")))))

          ;; Admin Permissions:
          (telega-ins
           (propertize (telega-i18n "lng_manage_peer_permissions")
                       'face 'bold)
           "\n")
          (telega-ins
           (propertize (telega-i18n "lng_rights_edit_admin_header")
                       'face 'telega-shadow)
           "\n")
          (telega-ins--labeled "  " nil
            (dolist (perm-spec telega-chat--admin-permissions)
              ;; list only those permissions which has title
              ;; NOTE: special permissions applies for channels only,
              ;; defined by `telega-chat--admin-permissions-for-channels'
              (when (and (cdr perm-spec)
                         (or channel-p
                             (not (memq (car perm-spec)
                                        telega-chat--admin-permissions-for-channels))))
                (let ((perm-value (plist-get my-perms (car perm-spec))))
                  (if (and can-edit-p
                           (or (not owner-p) (eq :is_anonymous (car perm-spec))))
                      (telega-ins--button (if perm-value
                                              telega-symbol-heavy-checkmark
                                            telega-symbol-blank-button)
                        :value chat
                        :action (lambda (chat)
                                  (cl-case (car perm-spec)
                                    (:is_anonymous
                                     (plist-put my-status :is_anonymous
                                                (if perm-value :false t))
                                     (telega--setChatMemberStatus
                                      chat (telega-user-me) my-status))
                                    (t
                                     (user-error "Not yet implemented")))))

                    (telega-ins (if perm-value
                                    telega-symbol-ballout-check
                                  telega-symbol-ballout-empty)))
                  (telega-ins " " (telega-i18n (cdr perm-spec)))
                  (telega-ins "\n"))))))))

    ;; Supergroup username, only owner can set/change group's username
    (let* ((usernames (plist-get supergroup :usernames))
           (username (plist-get usernames :editable_username))
           (can-set-username-p (telega-chat-match-p chat 'me-is-owner)))
      (when (or username can-set-username-p)
        (telega-ins "Username: ")
        (when username
          (telega-ins "@" username " "))
        (when can-set-username-p
          (telega-ins--button (if username "Change" "Set")
            'action (lambda (_ignored)
                      (let ((new-username (read-string "Set Username to: " "@")))
                        (when (string-prefix-p "@" new-username)
                          (setq new-username (substring new-username 1)))
                        (telega--setSupergroupUsername
                         supergroup new-username)))))
        (telega-ins "\n")))
    (when (plist-get my-perms :can_change_info)
      ;; All history setting is available to supergroups only, in
      ;; channels all history is always available
      (unless (telega-chat-channel-p chat)
        (telega-ins "All history available: ")
        (let ((all-history-available-p
               (plist-get full-info :is_all_history_available)))
          (telega-ins--button (if all-history-available-p
                                  telega-symbol-heavy-checkmark
                                telega-symbol-blank-button)
            'action (lambda (_ignored)
                      (telega--toggleSupergroupIsAllHistoryAvailable
                       supergroup (not all-history-available-p)))))
        (telega-ins "\n"))

      ;; Sign messages is available in channels only
      (when (telega-chat-channel-p chat)
        (telega-ins (telega-i18n "lng_edit_sign_messages") ": ")
        (telega-ins--button (if (plist-get supergroup :sign_messages)
                                telega-symbol-heavy-checkmark
                              telega-symbol-blank-button)
          'action (lambda (_ignored)
                    (telega--toggleSupergroupSignMessages
                     supergroup (not (plist-get supergroup :sign_messages)))))
        (telega-ins "\n"))
      )

    ;; Aggressive anti-spam mode
    (when (plist-get full-info :can_toggle_aggressive_anti_spam)
      (telega-ins (telega-i18n "lng_manage_peer_antispam") ": ")
      (let ((anti-spam-p
             (plist-get full-info :has_aggressive_anti_spam_enabled)))
        (telega-ins--button (if anti-spam-p
                                telega-symbol-heavy-checkmark
                              telega-symbol-blank-button)
          'action (lambda (_ignored)
                    (telega--toggleSupergroupHasAggressiveAntiSpamEnabled
                     supergroup (not anti-spam-p)))))
      (telega-ins "\n")
      (telega-ins--help-message
       (telega-ins-i18n "lng_manage_peer_antispam_about")))

    ;; Slow Mode is available only for supergroups
    (unless (telega-chat-channel-p chat)
      (let* ((slow-mode-delay (plist-get full-info :slow_mode_delay))
             (smd-str (if (zerop slow-mode-delay)
                          (telega-i18n "lng_rights_slowmode_off")
                        (telega-duration-human-readable slow-mode-delay)))
             (i18n-slowmode-header
              (concat (telega-i18n "lng_rights_slowmode_header") ": ")))
        (telega-ins i18n-slowmode-header)
        (if (plist-get my-perms :can_restrict_members)
            (telega-ins--button smd-str
              'action (lambda (_ignored)
                        (telega--setChatSlowModeDelay
                         chat
                         (telega-completing-read-slow-mode-delay
                          i18n-slowmode-header))))
          (telega-ins smd-str))
        (telega-ins "\n")
        (telega-ins--help-message
         (telega-ins-i18n "lng_rights_slowmode_about"))))

    ;; Discussion group / Linked Chat
    (let ((linked-chat (telega-chat-linked-chat chat))
          (can-set-discussion-group-p
           (and (telega-chat-channel-p chat)
                (plist-get my-perms :can_change_info))))
      (when (or linked-chat can-set-discussion-group-p)
        (telega-ins (if (telega-chat-channel-p chat)
                        "Discussion Group: "
                      "Linked Channel: "))
        (if linked-chat
            (telega-button--insert 'telega-chat
                linked-chat
              :inserter #'telega-ins--chat)
          (telega-ins (propertize "None" 'face 'telega-shadow)))
        (telega-ins " ")
        (when can-set-discussion-group-p
          (telega-ins--button (if linked-chat "Unset" "Set")
            'action (lambda (_ignored)
                      (telega--setChatDiscussionGroup
                       chat (unless linked-chat
                              (telega-read-discussion-chat))))))
        (telega-ins "\n")))

    ;; Location based chats
    (when (plist-get supergroup :has_location)
      (let* ((chat-loc (plist-get full-info :location))
             (loc (plist-get chat-loc :location))
             (address (telega-tl-str chat-loc :address)))
        (telega-ins "Location: " (telega-location-to-string loc) "\n")
        (when address
          (telega-ins "Address: " address "\n"))))

    ;; Creator and admins can [re]generate invite link
    (telega-info--insert-invite-link chat (plist-get full-info :invite_link))

    (when-let ((descr (telega-tl-str full-info :description)))
      (telega-ins--labeled "Desc: " nil
        (telega-ins descr "\n")))
    (when-let ((restr-reason (telega-tl-str supergroup :restriction_reason)))
      (telega-ins--labeled "Restriction: " nil
        (telega-ins restr-reason "\n")))

    (telega-ins "\n")
    (when (plist-get full-info :can_hide_members)
      (telega-ins (telega-i18n "lng_profile_hide_participants") ": ")
      (let ((has-hidden-members-p (plist-get full-info :has_hidden_members)))
        (telega-ins--button (if has-hidden-members-p
                                telega-symbol-heavy-checkmark
                              telega-symbol-blank-button)
          'action (lambda (_ignored)
                    (telega--toggleSupergroupHasHiddenMembers
                     supergroup (not has-hidden-members-p)))))
      (telega-ins "\n")
      (telega-ins--help-message
       (telega-ins-i18n "lng_profile_hide_participants_about")))
    (telega-ins-i18n "lng_profile_participants_section")
    (telega-ins-fmt ": %d%s (%d online, %d admins)"
      (plist-get full-info :member_count)
      (telega-symbol 'member)
      (or (plist-get chat :x-online-count) 0)
      (plist-get full-info :administrator_count))
    (when (plist-get full-info :can_get_members)
      ;; Asynchronously fetch/insert supergroup members when [Show]
      ;; button is pressed.  Button is used to avoid implicit members
      ;; loading, see https://t.me/emacs_telega/36426
      (telega--getSupergroupMembers supergroup nil nil nil
        (telega--gen-ins-continuation-callback 'loading
          (lambda (members)
            (when members
              (telega-ins "\n")
              (telega-ins--chat-members members)))))
      (telega-ins "\n"))

    (when telega-debug
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
    (telega-ins--with-face 'bold
      (telega-ins "* " (telega-i18n "lng_settings_connected_title")
                  ": " (if websites
                           (number-to-string (length websites))
                         (telega-i18n "lng_profile_loading"))))
    (if (not websites)
        (telega--getConnectedWebsites 'telega-describe-connected-websites)

      (telega-ins " ")
      (telega-ins--button (telega-i18n "lng_settings_disconnect_all")
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

      (telega-ins--with-face 'bold
        (telega-ins "* " (telega-i18n "lng_settings_logged_in_title") "\n\n"))

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
          (telega-ins--button (telega-i18n "lng_settings_disconnect")
            'action (lambda (_ignore)
                      (when (yes-or-no-p
                             (concat (telega-i18n "lng_settings_disconnect_sure"
                                       :domain domain)
                                     " "))
                        (telega--disconnectWebsite (plist-get website :id))
                        (telega-describe-connected-websites))))
          (telega-ins "\n"))
        (telega-ins (telega-i18n "lng_sessions_browser") ": "
                    (telega-tl-str website :browser)
                    " (" (telega-tl-str website :platform) ")"
                    "\n")
        (telega-ins (telega-i18n "lng_sessions_ip") ": "
                    (telega-tl-str website :ip_address) "\n")
        (telega-ins (telega-i18n "lng_sessions_location") ": "
                    (telega-tl-str website :location) "\n")
        (telega-ins--help-message
         (telega-ins-i18n "lng_sessions_location_about"))
        (telega-ins "Login: ")
        (telega-ins--date (plist-get website :log_in_date))
        (telega-ins ", Last: ")
        (telega-ins--date (plist-get website :last_active_date))
        (telega-ins "\n")
        
        (telega-ins--inline-delim)
        (telega-ins "\n")))
    ))

(defun telega-describe-active-sessions (&optional sessions)
  "Describe active SESSIONS."
  (interactive)
  (with-telega-help-win "*Telega Active Sessions*"
    ;; Title
    (telega-ins--with-face 'bold
      (telega-ins "* " (telega-i18n "lng_settings_sessions_title")
                  ": " (if sessions
                           (number-to-string (length sessions))
                         (telega-i18n "lng_profile_loading"))
                  "\n\n"))

    (if (not sessions)
        (telega--getActiveSessions 'telega-describe-active-sessions)

      (dolist (session sessions)
        (let ((app-name (plist-get session :application_name))
              (app-ver (plist-get session :application_version))
              (api-id (plist-get session :api_id))
              (official-p (plist-get session :is_official_application))
              (current-p (plist-get session :is_current))
              (device (plist-get session :device_model))
              (platform (telega-tl-str session :platform))
              (sys-ver (telega-tl-str session :system_version))
              (ip (plist-get session :ip_address))
              (location (telega-tl-str session :location))
              (login-ts (plist-get session :log_in_date))
              (last-ts (plist-get session :last_active_date)))
          (telega-ins app-name " v" app-ver " ")
          (if official-p
              (telega-ins "(official)")
            (telega-ins-fmt "(ID:%s)" api-id ))

          ;; Logout/Terminate button
          ;; see https://github.com/zevlg/telega.el/issues/113
          (if current-p
              (progn
                (telega-ins " ")
                (telega-ins--with-face 'bold
                  (telega-ins "(" (telega-i18n "lng_sessions_header") ")"))
                (telega-ins " ")
                (telega-ins--button "Logout"
                  'action (lambda (_ignore)
                            (when (yes-or-no-p "Really Logout? ")
                              (telega-logout)))))

            (telega-ins " ")
            (telega-ins--button (telega-i18n "lng_sessions_terminate")
              :value session
              :action (lambda (sess)
                        (when (yes-or-no-p
                               (format "Terminate '%s v%s'? " app-name app-ver))
                          (telega--terminateSession (plist-get sess :id))
                          (telega-save-cursor
                            (telega-describe-active-sessions
                             (delq sess sessions)))))))
          (telega-ins "\n")

          (telega-ins (telega-i18n "lng_sessions_system") ": "
                      device ", " platform (when platform " ") sys-ver "\n")
          (telega-ins (telega-i18n "lng_sessions_ip") ": " ip "\n")
          (telega-ins (telega-i18n "lng_sessions_location") ": " location "\n")
          (telega-ins--help-message
           (telega-ins-i18n "lng_sessions_location_about"))
          (telega-ins "Login: ")
          (telega-ins--date login-ts)
          (telega-ins ", Last: ")
          (telega-ins--date last-ts)
          (telega-ins "\n")
          (when (plist-get session :is_password_pending)
            (telega-ins--with-face 'error
              (telega-ins-i18n "lng_sessions_incomplete"))
            (telega-ins "\n")
            (telega-ins--help-message
             (telega-ins-i18n "lng_sessions_incomplete_about")))
          (telega-ins--inline-delim)
          (telega-ins "\n"))))))


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
      (telega-ins "Proxy Settings\n")
      (telega-ins "--------------\n")

      (telega-ins "Use Proxy: ")
      (telega-ins--button (if enabled
                              telega-symbol-ballout-check
                            telega-symbol-ballout-empty)
        'face 'telega-link
        :value (and (not enabled) recent)
        :action `(lambda (proxy)
                   (if proxy
                       (and (telega--enableProxy proxy)
                            (setq telega--conn-state 'ConnectingToProxy))
                     (telega--disableProxy))
                   (telega-describe-network)))
      (telega-ins "\n")
      (dolist (proxy proxies)
        (if (eq proxy enabled)
            (telega-ins telega-symbol-ballout-check)

          (telega-ins--button
              (if (and (eq proxy recent) (eq proxy enabled))
                  telega-symbol-ballout-check
                telega-symbol-ballout-empty)
            'face 'telega-link
            :value proxy
            :action `(lambda (proxy)
                       (and (telega--enableProxy proxy)
                            (setq telega--conn-state 'ConnectingToProxy))
                       (telega-describe-network))))
        (telega-ins " ")
        (telega-ins--column nil nil
          (telega-ins-fmt "%s:%d\n"
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
        (telega-ins "\n"))
      (telega-ins "\n")))

  (telega-ins "Network Statistics\n")
  (telega-ins "------------------\n")
  (let* ((net-stats (telega-server--call
                     (list :@type "getNetworkStatistics"
                           :only_current (or only-current :false))))
         (total-sent 0)
         (total-recv 0))
    (insert "Since: ")
    (telega-ins--date (plist-get net-stats :since_date))
    (insert "\n\n")
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
    (telega-ins-fmt "Total sent: %s\n"
      (file-size-human-readable total-sent))
    (telega-ins-fmt "Total recv: %s\n"
      (file-size-human-readable total-recv))
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
    (telega-ins--with-face '(telega-webpage-header underline)
      (telega-ins-i18n "lng_settings_privacy_title"))
    (telega-ins "\n")
    ;; NOTE: senders are only cddr of the reply from
    ;; `telega--getBlockedMessageSenders',
    ;; see comments in the `telega--on-blocked-senders-load'
    (when-let ((blocked-senders
                (cddr (telega--getBlockedMessageSenders 'blockListMain))))
      ;; I18N: blocked_list_title -> Blocked Users
      (telega-ins (telega-i18n "lng_blocked_list_title") ":" "\n")
      (dolist (msg-sender blocked-senders)
        ;; I18N: profile_unblock_user -> Unblock User
        (telega-ins--button (telega-i18n "lng_profile_unblock_user")
          :value msg-sender
          :action #'telega-msg-sender-unblock)
        (telega-ins " ")
        (if (telega-user-p msg-sender)
            (telega-button--insert 'telega-user msg-sender
              :inserter #'telega-ins--user)
          (telega-button--insert 'telega-chat msg-sender
            :inserter #'telega-ins--chat))
        (telega-ins "\n"))
      (telega-ins "\n"))
    (dolist (setting '(show-status allow-chat-invites allow-calls))
      (telega-ins-fmt "%S: " setting)
      (telega-ins-fmt "%S" (telega--getUserPrivacySettingRules setting))
      (telega-ins "\n"))

    (telega-ins "\n")
    (telega-ins--with-face '(telega-webpage-header underline)
      (telega-ins-i18n "lng_settings_section_privacy"))
    (telega-ins "\n")
    (telega-ins "TODO")
    (telega-ins "\n")

    (telega-ins "\n")
    (telega-ins--with-face '(telega-webpage-header underline)
      (telega-ins-i18n "lng_settings_self_destruct"))
    (telega-ins "\n")
    (telega-ins--account-ttl)))

(defun telega-describe ()
  "Describe current telega state and configuration."
  (user-error "`telega-describe' not yet implemented")
  )

(provide 'telega-info)

;;; telega-info.el ends here

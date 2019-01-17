;;; telega-info.el --- Users/Secrets/Groups stuff for telega

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
(require 'telega-util)
(require 'telega-server)

;; Info
(defmacro telega--info-update (tlobj)
  `(puthash (plist-get ,tlobj :id) ,tlobj
            (cdr (assq (telega--tl-type ,tlobj) telega--info))))

(defun telega--info (tlobj-type tlobj-id)
  (let* ((info-hash (cdr (assq tlobj-type telega--info)))
         (info (gethash tlobj-id info-hash)))
    (unless info
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
      (puthash tlobj-id info info-hash))
    info))

(defun telega--on-updateUser (event)
  (let ((user (plist-get event :user)))
    (telega--info-update user)
    (run-hook-with-args 'telega-user-update-hook user)))

(defun telega--on-updateBasicGroup (event)
  (telega--info-update (plist-get event :basic_group)))

(defun telega--on-updateSupergroup (event)
  (telega--info-update (plist-get event :supergroup)))

(defun telega--on-updateSecretChat (event)
  (let ((secretchat (plist-get event :secret_chat)))
    (telega--info-update secretchat)

    ;; update corresponding chat button
    (let ((chat (cl-find secretchat (telega-filter-chats '(type secret))
                         :test 'eq :key #'telega-chat--info)))
      (when chat
        (telega-root--chat-update chat)))))

;; FullInfo
(defun telega--on-updateUserFullInfo (event)
  (let ((ufi (cdr (assq 'user telega--full-info))))
    (puthash (plist-get event :user_id)
             (plist-get event :user_full_info) ufi)))

(defun telega--on-updateBasicGroupFullInfo (event)
  (let ((ufi (cdr (assq 'basicGroup telega--full-info))))
    (puthash (plist-get event :basic_group_id)
             (plist-get event :basic_group_full_info) ufi)))

(defun telega--on-updateSupergroupFullInfo (event)
  (let ((ufi (cdr (assq 'supergroup telega--full-info))))
    (puthash (plist-get event :supergroup_id)
             (plist-get event :supergroup_full_info) ufi)
    ;; TODO: chatbuf might need to be updated, since for example
    ;; pinned message might change
    ))

(defun telega--full-info (tlobj)
  "Get FullInfo for the TLOBJ.
TLOBJ could be one of: user, basicgroup or supergroup."
  (let* ((tlobj-type (telega--tl-type tlobj))
         (tlobj-id (plist-get tlobj :id))
         (fi-hash (cdr (assq tlobj-type telega--full-info)))
         (full-info (gethash tlobj-id fi-hash)))
    (unless full-info
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
                 "getting full-info for type=%S timeout" tlobj-type)
      (puthash tlobj-id full-info fi-hash))
    full-info))


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
  (let ((user (telega-user--get (plist-get event :user_id))))
    (plist-put user :status (plist-get event :status))
    (run-hook-with-args 'telega-user-update-hook user)))

(defun telega-user--chats-in-common (with-user)
  "Return CHATS in common WITH-USER."
  (let* ((gic-cnt (plist-get (telega--full-info with-user) :group_in_common_count))
         (gic (when (> gic-cnt 0)
                (telega-server--call
                 (list :@type "getGroupsInCommon"
                       :user_id (plist-get with-user :id)
                       :offset_chat_id 0
                       :limit gic-cnt)))))
    (mapcar #'telega-chat--get (plist-get gic :chat_ids))))

(defun telega--getSupergroupMembers (supergroup &optional filter)
  "Get SUPERGRUOP members.
Default FILTER is \"supergroupMembersFilterRecent\"."
  (telega-server--call
   (list :@type "getSupergroupMembers"
         :supergroup_id (plist-get supergroup :id)
         :filter (list :@type (or filter "supergroupMembersFilterRecent"))
         :offset 0
         :limit 200)))

(defun telega-info--insert-user (user &optional chat)
  "Insert USER info into current buffer."
  (let* ((full-info (telega--full-info user))
         (username (plist-get user :username))
         (phone_number (plist-get user :phone_number))
         (bio (plist-get full-info :bio))
         (share-text (plist-get full-info :share_text))
         (out-link (plist-get user :outgoing_link))
         (in-link (plist-get user :incoming_link)))

    (unless (eq (telega--tl-type in-link) 'linkStateIsContact)
      (telega-ins--button "[Share My Contact Info]"
        :value chat :action 'telega-chat-share-my-contact)
      (telega-ins "  "))
    ;; NOTE: Secret chat with myself is not possible
    (unless (eq (plist-get user :id) telega--me-id)
      (telega-ins--button "[Start Secret Chat]"
        :value user
        :action (lambda (user)
                  (telega-chat--pop-to-buffer
                   (telega--createNewSecretChat user))))
      (telega-ins "\n"))

    (telega-ins-fmt "Relationship: %s <-in---out-> %s\n"
      (substring (plist-get in-link :@type) 9)
      (substring (plist-get out-link :@type) 9))
    (unless (string-empty-p username)
      ;; I18N: lng_info_username_label
      (telega-ins-fmt "Username: @%s\n" username))
    (unless (string-empty-p phone_number)
      ;; I18N: lng_settings_phone_label
      (telega-ins-fmt "phone: +%s\n" phone_number))
    (telega-ins-fmt "Seen: %s\n" (telega-user--seen user))
    (unless (string-empty-p bio)
      ;; I18N: lng_bio_placeholder
      (telega-ins--labeled "bio: " nil (telega-ins bio))
      (telega-ins "\n"))
    (unless (string-empty-p share-text)
      (telega-ins--labeled "Share text: " nil
        (telega-ins share-text))
      (telega-ins "\n")))

  (let ((chats-in-common (telega-user--chats-in-common user)))
    (when chats-in-common
      ;; I18N: lng_profile_common_groups_section
      (telega-ins-fmt "%d chats in common:\n" (length chats-in-common))
      (dolist (chat chats-in-common)
        (telega-ins "    ")
        (telega-button--insert 'telega-chat chat)
        (telega-ins "\n"))))

  ;; TODO: view shared media as thumbnails

  (let ((call (telega-voip--by-user-id (plist-get user :id))))
    (when (and call telega-debug)
      (telega-ins "\n---DEBUG---\n")
      (telega-ins-fmt "Call: %S\n" call)))
  )

(defun telega-info--insert-secretchat (secretchat chat)
  "Insert info about SECRETCHAT into current buffer."
  (telega-ins "State: "
              (substring (telega--tl-get secretchat :state :@type) 15))
  (cl-case (telega--tl-type (plist-get secretchat :state))
    (secretChatStateClosed
     (telega-ins " ")
     (telega-ins--button "[Delete and Exit]"
       :value chat
       :action (lambda (chat)
                 (telega-chat-delete chat)
                 (quit-window)))

    ((secretChatStatePending secretChatStateReady)
     (telega-ins "  ")
     (telega-ins--button "[Close Secret Chat]"
      :value chat
      :action (lambda (chat)
                (telega--closeSecretChat (telega-chat--info chat))
                (telega-save-cursor
                  (telega-describe-chat chat)))))))
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
                   (k2 (lsh (logand kv 12) -2))
                   (k3 (lsh (logand kv 48) -4))
                   (k4 (lsh (logand kv 192) -6)))
              (cl-dolist (kk (list k1 k2 k3 k4))
                (telega-ins (propertize telega-symbol-square
                                        'face (nth kk efaces))))))
          (telega-ins "\n")
          (dotimes (ki (length ekey))
            (cond ((and (> ki 0) (= (% ki 8) 0))
                   (telega-ins "\n"))
                  ((= (% ki 8) 4)
                   (telega-ins " ")))
            (telega-ins-fmt "%02x " (aref ekey ki)))))
      (telega-ins "\n"))))

(defun telega-info--insert-invite-link (chat invite-link can-generate-p)
  "Insert CHAT's INVITE-LINK into current info buffer.
CAN-GENERATE-P is non-nil if invite link can be [re]generated."
  ;; NOTE: maybe use `checkChatInviteLink' to check the invitation
  ;; link for validity?
  (let ((valid-link-p (not (string-empty-p invite-link))))
    (when (or can-generate-p valid-link-p)
      (telega-ins "Invite link:")
      (when valid-link-p
        (telega-ins " ")
        (apply 'insert-text-button
               invite-link (telega-link-props 'url invite-link)))
      (when can-generate-p
        (telega-ins " ")
        (telega-ins--button (if valid-link-p "[Regenerate]" "[Generate]")
          :value chat
          :action (lambda (chat)
                    (telega-chat-generate-invite-link
                     (plist-get chat :id))
                    (telega-save-cursor
                      (telega-describe-chat chat)))))
      (telega-ins "\n"))))

(defun telega-info--insert-basicgroup (basicgroup chat)
  (let* ((full-info (telega--full-info basicgroup))
         (members (plist-get full-info :members))
         (creator-id (plist-get full-info :creator_user_id))
         (creator (telega-user--get creator-id))
         (creator-member
          (cl-find creator-id members :test '= :key (telega--tl-prop :user_id)))
         (member-status-name (plist-get (plist-get basicgroup :status) :@type))
         (invite-link (plist-get full-info :invite_link)))
    (insert "Status: " (substring member-status-name 16) "\n")
    (insert (format "Created: %s  %s\n"
                    (telega-user--name creator)
                    (if creator-member
                        (telega-fmt-timestamp
                         (plist-get creator-member :joined_chat_date))
                      "")))

    ;; For basic groups only creator can generate invite link
    (telega-info--insert-invite-link
     chat invite-link (string= member-status-name "chatMemberStatusCreator"))

    (insert (format "Members: %d users\n"
                    (plist-get basicgroup :member_count)))
    (mapc (lambda (mbr)
            (let ((mbr-id (plist-get mbr :user_id)))
              (insert "    ")
              (apply 'insert-text-button
                     (telega-user--name (telega-user--get mbr-id))
                     (telega-link-props 'user mbr-id))
              (insert "\n")))
          members)))

(defun telega-info--insert-supergroup (supergroup chat)
  (let* ((full-info (telega--full-info supergroup))
         (descr (plist-get full-info :description))
         (restr-reason (plist-get supergroup :restriction_reason))
         (pin-msg-id (plist-get full-info :pinned_message_id))
         (member-status (plist-get supergroup :status))
         (member-status-name (plist-get member-status :@type))
         (invite-link (plist-get full-info :invite_link)))
    (insert "Status: " (substring member-status-name 16) "\n")
    (insert (if (or (string= member-status-name "chatMemberStatusMember")
                    (and (member member-status-name
                                 '("chatMemberStatusCreator"
                                   "chatMemberStatusRestricted"))
                         (plist-get member-status :is_member)))
                "Joined at: "
              "Created at: ")
            (telega-fmt-timestamp (plist-get supergroup :date))
            "\n")

    ;; Creator and admins can [re]generate invite link
    (telega-info--insert-invite-link
     chat invite-link (member member-status-name
                              '("chatMemberStatusCreator"
                                "chatMemberStatusAdministrator")))

    (unless (string-empty-p descr)
      (insert (telega-fmt-labeled-text "Desc: " descr) "\n"))
    (unless (string-empty-p restr-reason)
      (insert (telega-fmt-labeled-text "Restriction: " restr-reason) "\n"))

    (unless (zerop pin-msg-id)
      (let ((pinned-msg (telega--getChatPinnedMessage chat)))
        (cl-assert pinned-msg)
        (insert "----(pinned message)----\n")
        (telega-button-insert 'telega-msg
          :value pinned-msg
          :format (telega-msg-button--format pinned-msg)
          :action 'ignore))
      (insert "------------------------\n"))

    (insert (format "Members: %d" (plist-get full-info :member_count)) "\n")
    (when (plist-get full-info :can_get_members)
      (mapc (lambda (member)
              (insert "  " (telega-fmt-eval 'telega-fmt-chat-member member) "\n"))
            (plist-get (telega--getSupergroupMembers supergroup) :members)))

    (when telega-debug
      (insert "\n---DEBUG---\n")
      (insert (format "Info: %S\n" supergroup))
      (insert (format "\nFull-Info: %S\n" full-info)))))

(defun telega-info--insert (tlobj chat)
  "Insert information about TLOBJ into current buffer."
  (cl-ecase (telega--tl-type tlobj)
    (chatTypePrivate
     (telega-info--insert-user
      (telega--info 'user (plist-get tlobj :user_id)) chat))
    (chatTypeSecret
     (telega-info--insert-secretchat
      (telega--info 'secretChat (plist-get tlobj :secret_chat_id)) chat))
    (chatTypeBasicGroup
     (telega-info--insert-basicgroup
      (telega--info 'basicGroup (plist-get tlobj :basic_group_id)) chat))
    (chatTypeSupergroup
     (telega-info--insert-supergroup
      (telega--info 'supergroup (plist-get tlobj :supergroup_id)) chat))))

(defun telega-describe-active-sessions ()
  "Describe active sessions."
  (interactive)
  (with-help-window "*Telega Active Sessions*"
    (set-buffer standard-output)
    (mapc (lambda (session)
            (let ((app_name (plist-get session :application_name))
                  (app_ver (plist-get session :application_version))
                  (api_id (plist-get session :api_id))
                  (official-p (plist-get session :is_official_application))
                  (current-p (plist-get session :is_current))
                  (device (plist-get session :device_model))
                  (platform (plist-get session :platform))
                  (sys_ver (plist-get session :system_version))
                  (ip (plist-get session :ip))
                  (country (plist-get session :country))
                  (login-ts (plist-get session :log_in_date))
                  (last-ts (plist-get session :last_active_date)))
              (insert (format "%s v%s " app_name app_ver)
                      (if official-p
                          "(official)"
                        (format "(ID:%s)" api_id))
                      (if current-p
                          (propertize " (current)" 'face 'bold)
                        "")
                      "\n")
              (insert (format "%s, %s %s\n" device platform sys_ver))
              (insert (format "%s %s\n" ip country))
              (insert (format "Login: %s, Last: %s\n"
                              (telega-fmt-timestamp login-ts)
                              (telega-fmt-timestamp last-ts)))
              (insert "\n")
              ))
          (plist-get (telega-server--call '(:@type "getActiveSessions"))
                     :sessions))))

(defun telega-describe-network0 (only-current)
  "Insert network settings/stats into current buffer."
  (let* ((proxies (telega--getProxies))
         (recent (telega-proxy-last-used proxies))
         (enabled (telega-proxy-enabled proxies)))
    (telega--pingProxies
     proxies `(lambda ()
                (let ((buffer (get-buffer "*Telega Network*")))
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (let ((inhibit-read-only t))
                        (telega-save-cursor
                          (erase-buffer)
                          (telega-describe-network0 ,only-current))))))))

    (when proxies
      (assert recent nil "No recent proxy")
      (telega-ins "Proxy Settings\n")
      (telega-ins "--------------\n")

      (telega-ins "Use Proxy: " (if enabled "Yes" "No"))
      (telega-ins " ")
      (telega-ins--button (if enabled "[Disable]" "[Enable]")
        :value (and (not enabled) recent)
        :action `(lambda (proxy)
                   (if proxy
                       (telega--enableProxy proxy)
                     (telega--disableProxy))
                   (telega-save-cursor
                     (telega-describe-network ,only-current))))
      (telega-ins "\n")
      (cl-dolist (proxy proxies)
        (let ((ping (cdr (assq (plist-get proxy :id) telega--proxy-pings))))
          (telega-ins--labeled
              (concat (cond ((plist-get proxy :is_enabled)
                             telega-symbol-msg-viewed)
                            ((eq recent proxy)
                             telega-symbol-msg-succeed)
                            (t "• ")) " ") nil
            (telega-ins-fmt "%s:%d"
              (plist-get proxy :server) (plist-get proxy :port))
            (unless (eq proxy enabled)
              (telega-ins " ")
              (telega-ins--action-button "[Enable]"
                :value proxy
                :value-action `(lambda (proxy)
                                 (telega--enableProxy proxy)
                                 (telega-save-cursor
                                   (telega-describe-network ,only-current)))))
            (telega-ins "\n")
            (telega-ins-fmt "%s, "
              (upcase (substring (telega--tl-get proxy :type :@type) 9)))
            (cond ((cdr ping)
                   (telega-ins-fmt "%dms ping"
                     (round (* (cdr ping) 1000))))
                  ((> (- (time-to-seconds) (car ping)) 10)
                   ;; 10 seconds ping timeout
                   (telega-ins "unavailable"))
                  (t
                   (telega-ins "pinging.."))))
          (telega-ins "\n")))
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
            (incf total-sent (plist-get entry :sent_bytes))
            (incf total-recv (plist-get entry :received_bytes))

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

(defun telega-describe-network (only-current)
  "Show network settings/statistics.
If prefix argument is given, then show statistics only for current launch."
  (interactive "P")
  (with-help-window "*Telega Network*"
    (set-buffer standard-output)
    (telega-describe-network0 only-current)))

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

(defun telega--getBlockedUsers (&optional offset)
  "Get list of blocked users."
  (let ((reply (telega-server--call
                (list :@type "getBlockedUsers"
                      :offset (or offset 0)
                      :limit 100))))
    (mapcar 'telega-user--get (plist-get reply :user_ids))))

(defun telega-describe-privacy-settings ()
  "Show user privacy settings."
  (interactive)
  (with-help-window "*Telega Privacy Settings*"
    (set-buffer standard-output)
    ;; I18N: lng_settings_privacy_title
    (insert "Privacy\n")
    (insert "-------\n")
    (let ((blocked-users (telega--getBlockedUsers)))
      ;; I18N: lng_blocked_list_title
      (insert "Blocked Users: "
              (if blocked-users
                  (mapconcat 'telega-user--name blocked-users ", ")
                "None"))
      (insert "\n"))
    (cl-dolist (setting '(show-status allow-chat-invites allow-calls))
      (telega-ins-fmt "%S: " setting)
      (telega-ins-fmt "%S" (telega--getUserPrivacySettingRules setting))
      (insert "\n")
      )

    (insert "\n")
    (insert "Security\n")
    (insert "--------\n")
    (insert "TODO")
    ))


;; Contacts
(defun telega--removeContacts (&rest user-ids)
  "Remove users determined by USER-IDS from contacts."
  (telega-server--call
   (list :@type "removeContacts"
         :user_ids (cl-map 'vector 'identity user-ids))))

(defun telega--importContacts (&rest contacts)
  "Import CONTACTS into contacts list."
  (telega-server--call
   (list :@type "importContacts"
         :contacts (cl-map 'vector 'identity contacts))))

(defun telega-describe-contact (contact)
  "Show CONTACT information."
  (with-help-window "*Telega Contact*"
    (set-buffer standard-output)
    (let* ((user-id (plist-get contact :user_id))
           (user (telega-user--get user-id))
           (full-info (telega--full-info user)))
      (when (telega-ins (plist-get contact :first_name))
        (telega-ins " "))
      (telega-ins (plist-get contact :last_name) "\n")
      (telega-ins-fmt "Phone: %s\n" (plist-get contact :phone_number))
      (if (eq (telega--tl-type (plist-get user :outgoing_link))
              'linkStateIsContact)
          (telega-ins--button "[RemoveContact]"
            :value contact
            :action (lambda (contact)
                      (telega--removeContacts (plist-get contact :user_id))
                      (telega-save-cursor
                        (telega-describe-contact contact))))

        (telega-ins--button "[ImportContact]"
          :value contact
          :action (lambda (contact)
                    (telega--importContacts contact)
                    (telega-save-cursor
                      (telega-describe-contact contact)))))
      (telega-ins "\n")

      (telega-ins "\n--- Telegram User Info ---\n")
      (telega-ins "Name: " (telega-user--name user 'name))
      (telega-ins " ")
      (telega-ins--button "[ChatWith]"
        :value user
        :action (lambda (user)
                  (telega-chat--pop-to-buffer
                   (telega--createPrivateChat user))))
      (when (plist-get full-info :can_be_called)
        (telega-ins " ")
        (telega-ins--button "[Call]"
          :value user
          :action 'telega-voip-call))
      (telega-ins "\n")
      (telega-info--insert-user user))))

(provide 'telega-info)

;;; telega-info.el ends here

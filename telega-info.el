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
(require 'telega-util)
(require 'telega-server)

;; Info
(defmacro telega--info-update (tlobj)
  `(puthash (plist-get ,tlobj :id) ,tlobj
            (cdr (assq (telega--tl-type ,tlobj) telega--info))))

(defun telega--info (tlobj-type tlobj-id)
  (let* ((info-hash (alist-get tlobj-type telega--info))
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

    ;; Update corresponding private chat button
    (when-let ((chat (telega-chat-get (plist-get user :id) 'offline)))
      (telega-root--chat-update chat))

    (run-hook-with-args 'telega-user-update-hook user)))

(defun telega--on-updateBasicGroup (event)
  (telega--info-update (plist-get event :basic_group))

  ;; TODO: chatbuf might need to be updated, status might be changed
  ;; due to someone removed me from basic group
  )

(defun telega--on-updateSupergroup (event)
  (telega--info-update (plist-get event :supergroup)))

(defun telega--on-updateSecretChat (event)
  (let ((secretchat (plist-get event :secret_chat)))
    (telega--info-update secretchat)

    ;; update corresponding secret chat button
    (let ((chat (cl-find secretchat
                         (telega-filter-chats '(type secret) telega--ordered-chats)
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
    ;; Might affect root's buffer view
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
                 "getting full-info for type=%S(%S) timeout" tlobj-type tlobj-id)
      (puthash tlobj-id full-info fi-hash))
    full-info))



(defun telega--getSupergroupMembers (supergroup &optional filter callback)
  "Get SUPERGRUOP members.
Default FILTER is \"supergroupMembersFilterRecent\"."
  (declare (indent 2))
  (telega-server--call
   (list :@type "getSupergroupMembers"
         :supergroup_id (plist-get supergroup :id)
         :filter (list :@type (or filter "supergroupMembersFilterRecent"))
         :offset 0
         :limit 200)
   callback))

(defun telega-info--insert-user (user &optional chat redisplay)
  "Insert USER info into current buffer."
  (let* ((full-info (telega--full-info user))
         (username (telega-tl-str user :username))
         (phone_number (plist-get user :phone_number))
         (bio (telega-tl-str user :bio))
         (share-text (telega-tl-str full-info :share_text))
         (out-link (plist-get user :outgoing_link))
         (in-link (plist-get user :incoming_link))
         (profile-photos (telega--getUserProfilePhotos user)))

    ;; Scam&Blacklist status
    (when (or (plist-get user :is_scam) (plist-get full-info :is_blocked))
      (telega-ins--with-face 'error
        (telega-ins telega-symbol-blocked)
        (when (plist-get user :is_scam)
          (telega-ins "SCAM "))
        (when (plist-get full-info :is_blocked)
          (telega-ins "BLOCKED "))))

    ;; Buttons line
    (telega-ins--button "Chat With"
      :value user
      :action 'telega-user-chat-with)
    (telega-ins " ")
    (unless (or (telega-user-bot-p user)
                (eq (telega--tl-type in-link) 'linkStateIsContact))
      (telega-ins--button "Share My Contact"
        :value chat :action 'telega-chat-share-my-contact)
      (telega-ins " "))
    ;; NOTE: Secret chat with bots and myself is not possible
    (unless (or (telega-user-bot-p user)
                (telega-me-p user))
      ;; TODO: search for existing secret chat with Ready state and
      ;; create [Open Secret Chat] button instead
      (telega-ins--button (concat telega-symbol-lock "Start Secret Chat")
        :value user
        :action (lambda (user)
                  (telega-chat--pop-to-buffer
                   (telega--createNewSecretChat user))))
      (telega-ins " "))
    (when (plist-get full-info :can_be_called)
      (telega-ins--button (concat telega-symbol-phone "Call")
        :value user
        :action 'telega-voip-call)
      (telega-ins " "))
    (let ((user-blocked-p (plist-get full-info :is_blocked)))
      (telega-ins--button
          (concat telega-symbol-blocked
                  (if user-blocked-p "Unblock" "Block") " User")
        'action (lambda (_ignored)
                  (telega-user-block user user-blocked-p)
                  (when redisplay
                    (telega-save-cursor
                      (funcall redisplay))))))
    (telega-ins "\n")

    ;; Clickable user's profile photos
    (when profile-photos
      (dolist (photo profile-photos)
        (telega-button--insert 'telega photo
          :inserter (lambda (photo-val)
                      (telega-ins--image
                       (telega-photo--image
                        photo-val telega-user-photo-maxsize)))
          :action 'telega-photo--open)
        (telega-ins " "))
      (telega-ins "\n"))

    (when (telega-me-p user)
      ;; Saved Messages
      (telega-ins "Logged in: ")
      (telega-ins--date-full
       (plist-get telega--options :authorization_date))
      (telega-ins "\n")
      (telega-ins-fmt "Account TTL: %d days\n"
        (plist-get (telega-server--call
                    (list :@type "getAccountTtl")) :days)))

    (telega-ins-fmt "Relationship: %s <-in---out-> %s\n"
      (substring (plist-get in-link :@type) 9)
      (substring (plist-get out-link :@type) 9))
    (unless (string-empty-p username)
      ;; I18N: lng_info_username_label
      (telega-ins-fmt "Username: @%s\n" username))
    (unless (string-empty-p phone_number)
      ;; I18N: lng_settings_phone_label
      (telega-ins-fmt "phone: +%s\n" phone_number))
    ;; Online status
    (let ((last-online (plist-get user :telega-last-online))
          (seen (telega-user--seen user)))
      (telega-ins "Last seen: ")
      (cond ((string= "Online" seen)
             (telega-ins seen))
            (last-online
             (telega-ins "in " (telega-duration-human-readable
                                (- (telega-time-seconds) last-online) 1)))
            (t (telega-ins seen)))
      (telega-ins "\n"))
    (unless (string-empty-p bio)
      ;; I18N: lng_bio_placeholder
      (telega-ins--labeled "bio: " nil (telega-ins bio))
      (telega-ins "\n"))
    (unless (string-empty-p share-text)
      (telega-ins--labeled "Share text: " nil
        (telega-ins share-text))
      (telega-ins "\n"))

    ;; Bot info
    (let* ((bot-info (telega-tl-str full-info :bot_info))
           (bot-descr (telega-tl-str bot-info :description))
           (bot-cmds (append (plist-get bot-info :commands) nil)))
      (when bot-info
        (unless (string-empty-p bot-descr)
          (telega-ins--labeled "Bot info: " nil
            (telega-ins bot-descr))
          (telega-ins "\n"))
        (when bot-cmds
          (telega-ins "Bot cmds: \n"))
        (dolist (cmd bot-cmds)
          (telega-ins--labeled
              (format "  /%s - " (telega-tl-str cmd :command)) nil
            (telega-ins (telega-tl-str cmd :description)))
          (telega-ins "\n"))
        (telega-ins "\n")))
    )

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
                 (telega--closeSecretChat (telega-chat--info chat))
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
                   (k2 (lsh (logand kv 12) -2))
                   (k3 (lsh (logand kv 48) -4))
                   (k4 (lsh (logand kv 192) -6)))
              (dolist (kk (list k1 k2 k3 k4))
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
        (telega-ins--button (if valid-link-p "Regenerate" "Generate")
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
         (creator (unless (zerop creator-id)
                    (telega-user--get creator-id)))
         (member-status-name (plist-get (plist-get basicgroup :status) :@type))
         (invite-link (plist-get full-info :invite_link)))
    (telega-ins "Status: " (substring member-status-name 16) "\n")
    (when creator
      (telega-ins "Created: " (telega-user--name creator) "  ")
      (let ((creator-member
             (cl-find creator-id members :test '= :key (telega--tl-prop :user_id))))
        (when creator-member
          (telega-ins--date (plist-get creator-member :joined_chat_date)))))
    (telega-ins "\n")

    ;; For basic groups only creator can generate invite link
    (telega-info--insert-invite-link
     chat invite-link (string= member-status-name "chatMemberStatusCreator"))

    (telega-ins-fmt "Members: %d users (%d online)\n"
      (plist-get basicgroup :member_count)
      (or (plist-get chat :x-online-count) 0))
    (telega-ins--chat-members members)
    ))

(defun telega-info--insert-supergroup (supergroup chat)
  (let* ((full-info (telega--full-info supergroup))
         (descr (plist-get full-info :description))
         (restr-reason (telega-tl-str supergroup :restriction_reason))
         (pin-msg-id (plist-get chat :pinned_message_id))
         (member-status (plist-get supergroup :status))
         (member-status-name (plist-get member-status :@type))
         (invite-link (plist-get full-info :invite_link)))
    ;; Scam status first
    (when (plist-get supergroup :is_scam)
      (telega-ins--with-face 'error
        (telega-ins telega-symbol-blocked)
        (telega-ins "SCAM"))
      (telega-ins "\n"))

    (telega-ins "Status: " (substring member-status-name 16) "\n")
    (telega-ins (if (or (string= member-status-name "chatMemberStatusMember")
                        (and (member member-status-name
                                     '("chatMemberStatusCreator"
                                       "chatMemberStatusRestricted"))
                             (plist-get member-status :is_member)))
                    "Joined at: "
                  "Created at: "))
    (telega-ins--date (plist-get supergroup :date))
    (telega-ins "\n")

    ;; Creator and admins can [re]generate invite link
    (telega-info--insert-invite-link
     chat invite-link (member member-status-name
                              '("chatMemberStatusCreator"
                                "chatMemberStatusAdministrator")))

    (unless (string-empty-p descr)
      (telega-ins--labeled "Desc: " nil
        (telega-ins descr "\n")))
    (unless (string-empty-p restr-reason)
      (telega-ins--labeled "Restriction: " nil
        (telega-ins restr-reason "\n")))

    (unless (zerop pin-msg-id)
      (let ((pinned-msg (telega-msg--get (plist-get chat :id) pin-msg-id))
            (inhibit-read-only t))
        (when pinned-msg
          (telega-ins "----(pinned message)----\n")
          (telega-button--insert 'telega-msg pinned-msg
            :inserter 'telega-ins--content
            :action 'telega-msg-goto-highlight)
          (telega-ins "\n")
          (insert "------------------------\n"))))

    (telega-ins-fmt "Members: %d (%d online)\n"
      (plist-get full-info :member_count)
      (or (plist-get chat :x-online-count) 0))
    (when (plist-get full-info :can_get_members)
      ;; Asynchronously fetch/insert supergroup members
      (telega--getSupergroupMembers supergroup nil
        (let ((buffer (current-buffer))
              (at-point (point)))
          (lambda (reply)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (let ((inhibit-read-only t))
                  (save-excursion
                    (goto-char at-point)
                    (telega-ins--chat-members (plist-get reply :members)))))))
          )))

    (when telega-debug
      (insert "\n---DEBUG---\n")
      (insert (propertize "Info: " 'face 'bold)
              (format "%S" supergroup) "\n")
      (insert (propertize "Full-Info: " 'face 'bold)
              (format "%S" full-info) "\n"))))

(defun telega-describe-active-sessions (&optional sessions)
  "Describe active SESSIONS."
  (interactive)
  (with-telega-help-win "*Telega Active Sessions*"
    (if (not sessions)
        (progn
          (telega-ins "Loading...")
          ;; Async sessions loading
          (telega-server--call
           (list :@type "getActiveSessions")
           (lambda (reply)
             (telega-describe-active-sessions
              (append (plist-get reply :sessions) nil)))))

      (dolist (session sessions)
        (let ((app-name (plist-get session :application_name))
              (app-ver (plist-get session :application_version))
              (api-id (plist-get session :api_id))
              (official-p (plist-get session :is_official_application))
              (current-p (plist-get session :is_current))
              (device (plist-get session :device_model))
              (platform (plist-get session :platform))
              (sys-ver (plist-get session :system_version))
              (ip (plist-get session :ip))
              (country (plist-get session :country))
              (login-ts (plist-get session :log_in_date))
              (last-ts (plist-get session :last_active_date)))
          (telega-ins app-name " v" app-ver " ")
          (if official-p
              (telega-ins "(official)")
            (telega-ins-fmt "(ID:%s)" api-id ))
          (when current-p
            (telega-ins (propertize " (current)" 'face 'bold)))
          (telega-ins "\n")
          (telega-ins-fmt "%s, %s %s\n" device platform sys-ver)
          (telega-ins-fmt "%s %s\n" ip country)
          (telega-ins "Login: ")
          (telega-ins--date login-ts)
          (telega-ins ", Last: ")
          (telega-ins--date last-ts)
          (telega-ins "\n")
          (insert "\n"))))))


;; Proxies code
(defun telega--getProxies ()
  "Return list of currently registered proxies."
  (let ((reply (telega-server--call (list :@type "getProxies"))))
    (mapcar #'identity (plist-get reply :proxies))))

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
  (telega-server--call
   (list :@type "disableProxy")))

(defun telega-proxy-last-used (&optional proxies)
  "Return last time used proxy."
  (car (cl-sort (or (cl-copy-list proxies) (telega--getProxies))
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
    ;; I18N: lng_settings_privacy_title
    (telega-ins "Privacy\n")
    (telega-ins "-------\n")
    (when-let ((blocked-users (telega--getBlockedUsers)))
      ;; I18N: lng_blocked_list_title
      (telega-ins "Blocked Users:\n")
      (dolist (user blocked-users)
        (telega-ins--button "Unblock"
          'action (lambda (_ignored)
                    (telega--unblockUser user)
                    (telega-save-cursor
                      (telega-describe-privacy-settings))))
        (telega-ins " ")
        (telega-button--insert 'telega-user user
          :inserter 'telega-ins--user)
        (telega-ins "\n"))
      (telega-ins "\n"))
    (dolist (setting '(show-status allow-chat-invites allow-calls))
      (telega-ins-fmt "%S: " setting)
      (telega-ins-fmt "%S" (telega--getUserPrivacySettingRules setting))
      (insert "\n")
      )

    (insert "\n")
    (insert "Security\n")
    (insert "--------\n")
    (insert "TODO")
    ))

(defun telega-describe ()
  "Describe current telega state and configuration."
  (user-error "`telega-describe' not yet implemented")
  )

(provide 'telega-info)

;;; telega-info.el ends here

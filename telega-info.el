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
                  (ecase tlobj-type
                    (user
                     `(:@type "getUser" :user_id ,tlobj-id))
                    (secretChat
                     `(:@type "getSecretChat" :secret_chat_id ,tlobj-id))
                    (basicGroup
                     `(:@type "getBasicGroup" :basic_group_id ,tlobj-id))
                    (supergroup
                     `(:@type "getSupergroup" :supergroup_id ,tlobj-id)))))
      (assert info nil "getting info for %S(id=%S) timeout" tlobj-type tlobj-id)
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
  (telega--info-update (plist-get event :secret_chat)))

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
             (plist-get event :supergroup_full_info) ufi)))

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
             (ecase tlobj-type
               (user
                `(:@type "getUserFullInfo" :user_id ,tlobj-id))
               (basicGroup
                `(:@type "getBasicGroupFullInfo" :basic_group_id ,tlobj-id))
               (supergroup
                `(:@type "getSupergroupFullInfo" :supergroup_id ,tlobj-id)))))
      (assert full-info nil
              "getting full-info for type=%S timeout" tlobj-type)
      (puthash tlobj-id full-info fi-hash))
    full-info))


(defun telega-user--get (user-id)
  "Get user by USER-ID."
  (telega--info 'user user-id))

(defun telega-user--me ()
  "Return me as telegram user."
  (telega-server--call `(:@type "getMe")))

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
                 `(:@type "getGroupsInCommon"
                          :user_id ,(plist-get with-user :id)
                          :offset_chat_id 0 :limit ,gic-cnt)))))
    (mapcar #'telega-chat--get (plist-get gic :chat_ids))))

(defun telega-info--insert-user (user)
  "Insert USER info into current buffer."
  (let* ((full-info (telega--full-info user))
         (username (plist-get user :username))
         (phone_number (plist-get user :phone_number))
         (bio (plist-get full-info :bio))
         (share-text (plist-get full-info :share_text)))
    (unless (string-empty-p username)
      (insert (format "Username: @%s\n" username)))
    (unless (string-empty-p phone_number)
      (insert (format "phone: +%s\n" phone_number)))
    (insert (format "Seen: %s\n" (telega-user--seen user)))
    (unless (string-empty-p bio)
      (insert (format "bio: %s\n" bio)))
    (unless (string-empty-p share-text)
      (insert (format "Share text: %s\n" share-text))))

  (let ((chats-in-common (telega-user--chats-in-common user)))
    (when chats-in-common
      (insert (format "%d chats in common:\n" (length chats-in-common)))
      (dolist (chat chats-in-common)
        (insert "    ") 
        (telega-button-insert 'telega-chat
          :value chat
          :format '("[" (telega-chat--title
                         :min 25 :max 25
                         :align left :align-char ?\s
                         :elide t :elide-trail 0)
                    "]"))
        (insert "\n"))))

    ;; TODO: view shared media as thumbnails
  )

(defun telega-info--insert-secretchat (secretchat)
  (insert "!TODO!\n")
  (insert (format "Info: %S\n\n" secretchat))
  )

(defun telega-info--insert-basicgroup (basicgroup)
  (let* ((full-info (telega--full-info basicgroup))
         (invite_link (plist-get full-info :invite_link))
         (members (plist-get full-info :members))
         (creator_id (plist-get full-info :creator_user_id))
         (creator (telega-user--get creator_id))
         (creator-member
          (cl-find creator_id members
                   :test (lambda (crt_id m)
                           (= crt_id (plist-get m :user_id)))))
         )
    (insert (format "Created: %s  %s\n"
                    (telega-user--title creator)
                    (if creator-member
                        (telega-fmt-timestamp
                         (plist-get creator-member :joined_chat_date))
                      "")))
    (unless (string-empty-p invite_link)
      (insert "Invite link: ")
      (insert-text-button invite_link 'follow-link t)
      (insert "\n"))

    (insert (format "Members: %d users\n"
                    (plist-get basicgroup :member_count)))
    (mapc (lambda (mbr)
            (insert (format "  %s\n"
                            (telega-user--title
                             (telega-user--get (plist-get mbr :user_id))))))
          members)
    ))

(defun telega-info--insert-supergroup (supergroup)
  (let* ((full-info (telega--full-info supergroup)))
    (insert "!TODO!\n")
    (insert (format "Info: %S\n\n" supergroup))
    (insert (format "Full: %S" full-info))
    ))

(defun telega-info--insert (tlobj)
  "Insert information about TLOBJ into current buffer."
  (ecase (telega--tl-type tlobj)
    (chatTypePrivate
     (telega-info--insert-user
      (telega--info 'user (plist-get tlobj :user_id))))
    (chatTypeSecret
     (telega-info--insert-secretchat
      (telega--info 'secretChat (plist-get tlobj :secret_chat_id))))
    (chatTypeBasicGroup
     (telega-info--insert-basicgroup
      (telega--info 'basicGroup (plist-get tlobj :basic_group_id))))
    (chatTypeSupergroup
     (telega-info--insert-supergroup
      (telega--info 'supergroup (plist-get tlobj :supergroup_id))))))

(provide 'telega-info)

;;; telega-info.el ends here

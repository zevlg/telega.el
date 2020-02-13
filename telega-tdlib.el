;;; telega-tdlib.el --- TDLib API interface  -*- lexical-binding:t -*-

;; Copyright (C) 2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Sep 17 15:01:21 2019
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

;; Emacs lisp interface to TDLib API

;;; Code:
(require 'telega-server)

(declare-function telega-chat-get "telega-chat" (chat-id &optional offline-p))
(declare-function telega-chat--ensure "telega-chat" (chat))
(declare-function telega-stickerset--ensure "telega-sticker" (sset))
(declare-function telega-user--get "telega-user" (user-id))

(defvar telega-version)
(defvar telega-app)

(defmacro with-telega-server-reply (reply post-form call-sexp &optional callback)
  "Do sync or async call to telega-server, processing REPLY by POST-FORM.
CALL-SEXP and CALLBACK are passed directly to `telega-server--call'."
  (declare (indent 2))
  (let ((reply-sym (gensym "reply"))
        (reply-var (car reply)))
    `(let ((,reply-var (telega-server--call
                        ,call-sexp
                        (when ,callback
                          (lambda (,reply-sym)
                            (let ((,reply-var ,reply-sym))
                              (funcall ,callback ,post-form)))))))
       (if ,callback
           ,reply-var
         ,post-form))))

(defun telega--setOption (prop-kw val)
  "Set option, defined by keyword PROP-KW to VAL."
  (declare (indent 1))
  (telega-server--send
   (list :@type "setOption"
         :name (substring (symbol-name prop-kw) 1) ; strip `:'
         :value (list :@type (cond ((memq val '(t nil :false))
                                    "optionValueBoolean")
                                   ((integerp val)
                                    "optionValueInteger")
                                   ((stringp val)
                                    "optionValueString")
                                   (t (error "Unknown value type: %S"
                                             (type-of val))))
                      :value (or val :false)))))

(defun telega--setOptions (options-plist)
  "Send custom OPTIONS-PLIST to server."
  (cl-loop for (prop-name value) on options-plist
           by 'cddr
           do (telega--setOption prop-name value)))

(defun telega--searchEmojis (text &optional exact-match-p callback)
  "Search for emojis by TEXT keywords.
Non-nil EXACT-MATCH-P to return only emojis that exactly matches TEXT."
  (with-telega-server-reply (reply)
      (mapcar (lambda (emoji)
                (telega--desurrogate-apply emoji 'no-props))
              (plist-get reply :emojis))

    (list :@type "searchEmojis"
          :text text
          :exact_match (or exact-match-p :false))
    callback))

(defun telega--setChatDescription (chat descr)
  "Set CHAT's description to DESCR."
  (telega-server--send
   (list :@type "setChatDescription"
         :chat_id (plist-get chat :id)
         :description (or descr ""))))

(defun telega--createNewSecretChat (user)
  "Create secret chat with USER.
Return newly created chat."
  (telega-chat-get
   (plist-get
    (telega-server--call
     (list :@type "createNewSecretChat"
           :user_id (plist-get user :id))) :id)))

(defun telega--createBasicGroupChat (basic-group-id &optional force)
  "Create chat for BASIC-GROUP-ID."
  (telega-chat-get
   (plist-get
    (telega-server--call
     (list :@type "createBasicGroupChat"
           :basic_group_id basic-group-id
           :force (if force t :false)))
    :id)))

(defun telega--createSupergroupChat (supergroup-id &optional force)
  "Create chat for SUPERGROUP-ID."
  (telega-chat-get
   (plist-get
    (telega-server--call
     (list :@type "createSupergroupChat"
           :supergroup_id supergroup-id
           :force (if force t :false)))
    :id)))

(defun telega--closeSecretChat (secretchat)
  "Close SECRETCHAT."
  (telega-server--send
   (list :@type "closeSecretChat"
         :secret_chat_id (plist-get secretchat :id))))

(defun telega--getPublicMessageLink (chat-id msg-id &optional for-album)
  "Get https link to public message."
  (plist-get
   (telega-server--call
    (list :@type "getPublicMessageLink"
          :chat_id chat-id
          :message_id msg-id
          :for_album (or for-album :false)))
   :link))

(defun telega--getMessage (chat-id msg-id &optional callback)
  "Get message by CHAT-ID and MSG-ID.
If CALLBACK is specified, then get message asynchronously.
If message is not found, then return `nil'."
  (with-telega-server-reply (reply)
      (unless (telega--tl-error-p reply)
        reply)

    (list :@type "getMessage"
          :chat_id chat-id
          :message_id msg-id)
     callback))

(defun telega--getMessageLink (chat-id msg-id)
  "Get https link for message in private supergroup/channel."
  (plist-get
   (telega-server--call
    (list :@type "getMessageLink"
          :chat_id chat-id
          :message_id msg-id))
   :url))

(defun telega--checkChatInviteLink (invite-link &optional callback)
  "Check invitation link INVITE-LINK."
  (telega-server--call
   (list :@type "checkChatInviteLink"
         :invite_link invite-link)
   callback))

(defun telega--joinChatByInviteLink (invite-link &optional callback)
  "Return new chat by its INVITE-LINK.
Return nil if can't join the chat."
  (with-telega-server-reply (reply)
      (telega-chat-get (plist-get reply :id))

    (list :@type "joinChatByInviteLink"
          :invite_link invite-link)
    callback))

(defun telega--getChatEventLog (chat &optional query from-event-id
                                     limit filters users callback)
  "Return event log for the CHAT.
FILTERS are created with `telega-chatevent-log-filter'."
  (with-telega-server-reply (reply)
      (append (plist-get reply :events) nil)

    (nconc (list :@type "getChatEventLog"
                 :chat_id (plist-get chat :id)
                 :from_event_id (or from-event-id 0)
                 :limit (or limit 100))
           (when query
             (list :query query))
           (when filters
             (list :filters filters))
           (when users
             (list :user_ids
                   (cl-map 'vector (telega--tl-prop :id) users))))
    callback))

(defun telega--getCreatedPublicChats (&optional callback)
  "Return list of public chats created by the user."
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat-get (append (plist-get reply :chat_ids) nil))

  (telega-server--call
   (list :@type "getCreatedPublicChats")
   callback)))

(defun telega--blockUser (user &optional callback)
  "Block user by USER."
  (telega-server--call
   (list :@type "blockUser"
         :user_id (plist-get user :id))
   callback))

(defun telega--unblockUser (user &optional callback)
  "Unblock user by USER."
  (telega-server--call
   (list :@type "unblockUser"
         :user_id (plist-get user :id))
   callback))

(defun telega--getBlockedUsers (&optional offset callback)
  "Get list of blocked users."
  (with-telega-server-reply (reply)
      (mapcar 'telega-user--get (plist-get reply :user_ids))

    (list :@type "getBlockedUsers"
          :offset (or offset 0)
          :limit 100)
    callback))

(defun telega--getStickers (emoji &optional limit callback)
  "Return installed stickers that correspond to a given EMOJI.
LIMIT defaults to 20."
  (declare (indent 2))
  (with-telega-server-reply (reply)
      (append (plist-get reply :stickers) nil)

    (list :@type "getStickers"
          :emoji emoji
          :limit (or limit 20))
    callback))

(defun telega--searchStickers (emoji &optional limit callback)
  "Search for the public stickers that correspond to a given EMOJI.
LIMIT defaults to 20."
  (declare (indent 2))
  (with-telega-server-reply (reply)
      (append (plist-get reply :stickers) nil)

    (list :@type "searchStickers"
          :emoji emoji
          :limit (or limit 20))
    callback))

(defun telega--getInstalledStickerSets (&optional masks-p callback)
  "Return a list of installed sticker sets."
  (declare (indent 1))
  (cl-assert (not masks-p) t "installed masks not yet supported")
  (with-telega-server-reply (reply)
      (append (plist-get reply :sets) nil)

    (list :@type "getInstalledStickerSets"
          :is_masks (or masks-p :false))
    callback))

(defun telega--getTrendingStickerSets (&optional callback)
  "Return a list of trending sticker sets."
  (with-telega-server-reply (reply)
      (append (plist-get reply :sets) nil)

    (list :@type "getTrendingStickerSets")
    callback))

(defun telega--getStickerSet (set-id &optional callback)
  "Get information about a sticker set by its identifier SET-ID."
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (telega-stickerset--ensure reply)

    (list :@type "getStickerSet"
          :set_id set-id)
    callback))

(defun telega--searchStickerSet (name &optional callback)
  "Search for sticker set by NAME."
  (with-telega-server-reply (reply)
      (telega-stickerset--ensure reply)

    (list :@type "searchStickerSet"
          :name name)
    callback))

(defun telega--resendMessage (message)
  "Resend MESSAGE."
  (telega-server--send
   (list :@type "resendMessages"
         :chat_id (plist-get message :chat_id)
         :message_ids (vector (plist-get message :id)))))

(defun telega--deleteChatReplyMarkup (chat-id msg-id)
  "Deletes the default reply markup from a chat.
Must be called after a one-time keyboard or a ForceReply reply
markup has been used."
  (telega-server--send
   (list :@type "deleteChatReplyMarkup"
         :chat_id chat-id
         :message_id msg-id)))

(defun telega--getSupergroupMembers (supergroup &optional filter offset limit callback)
  "Get SUPERGROUP members.
Default FILTER is \"supergroupMembersFilterRecent\".
Default OFFSET is 0.
Default LIMIT is 200.
Return list of \"ChatMember\" objects."
  (declare (indent 4))
  (with-telega-server-reply (reply)
      (append (plist-get reply :members) nil)

    (list :@type "getSupergroupMembers"
          :supergroup_id (plist-get supergroup :id)
          :filter (list :@type (or filter "supergroupMembersFilterRecent"))
          :offset (or offset 0)
          :limit (or limit 200))
    callback))

(defun telega--getActiveSessions (&optional callback)
  "Get and return list of active sessions."
  (with-telega-server-reply (reply)
      (append (plist-get reply :sessions) nil)

    (list :@type "getActiveSessions")
    callback))

(defun telega--terminateSession (session-id)
  "Terminate a session of the current user by SESSION-ID."
  (telega-server--send
   (list :@type "terminateSession"
         :session_id session-id)))

(defun telega--terminateAllOtherSessions ()
  "Terminate all other sessions of the current user."
  ;; NOTE: Dangerous call, avoid using it! so XXXX added
  ;; see https://github.com/tdlib/td/issues/830
  (telega-server--send
   (list :@type "XXXXterminateAllOtherSessions")))

(defun telega--getProxies (&optional callback)
  "Return list of currently registered proxies."
  (with-telega-server-reply (reply)
      (append (plist-get reply :proxies) nil)

    (list :@type "getProxies")
    callback))

(defun telega--pinChatMessage (msg &optional disable-notifications)
  "Pin message MSG."
  (telega-server--send
   (list :@type "pinChatMessage"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id)
         :disable_notification (if disable-notifications t :false))))

(defun telega--unpinChatMessage (chat)
  "In CHAT unpin message."
  (telega-server--send
   (list :type "unpinChatMessage"
         :chat_id (plist-get chat :id))))


(defun telega--setAuthenticationPhoneNumber (phone-number)
  "Set user's phone number to PHONE-NUMBER."
  (telega-server--send
   (list :@type "setAuthenticationPhoneNumber"
         :phone_number phone-number
         :settings (list :@type "phoneNumberAuthenticationSettings"
                         :allow_flash_call :false
                         :is_current_phone_number :false))))

(defun telega--checkAuthenticationPassword (password)
  "Check the PASSWORD for the 2-factor authentification."
  (telega-server--send
   (list :@type "checkAuthenticationPassword"
         :password password)))

(defun telega--checkAuthenticationCode (code)
  "Checks the authentication CODE."
  (telega-server--send
   (list :@type "checkAuthenticationCode"
         :code code)))

(defun telega--registerUser (first-name &optional last-name)
  "Finish new user registration."
  (telega-server--send
   (list :@type "registerUser"
         :first_name first-name
         :last_name (or last-name ""))))

(defun telega-logout ()
  "Switch to another telegram account."
  (interactive)
  (telega-server--send
   (list :@type "logOut")))

(defun telega--setAccountTtl (days)
  "Change the period of account inactivity to DAYS.
After that period the account of the current user will
automatically be deleted."
  (telega-server--send
   (list :@type "setAccountTtl"
         :ttl (list :@type "accountTtl"
                    :days days))))

(defun telega--getAccountTtl (&optional callback)
  "Return number of days as account inactivity before account is deleted."
  (with-telega-server-reply (reply)
      (plist-get reply :days)

    (list :@type "getAccountTtl")
    callback))

(defun telega--getUserProfilePhotos (user &optional offset limit callback)
  "Return the profile photos (`UserProfilePhotos') of a USER.
OFFSET - number of photos to skip (default=0)
LIMIT - limit number of photos (default=100)."
  (with-telega-server-reply (reply)
      (append (plist-get reply :photos) nil)

    (list :@type "getUserProfilePhotos"
          :user_id (plist-get user :id)
          :offset (or offset 0)
          :limit (or limit 100))
    callback))

(defun telega--setProfilePhoto (filename &optional callback)
  "Upload a new profile photo for the current user."
  (telega-server--call
   (list :@type "setProfilePhoto"
         :photo (list :@type "inputFileLocal"
                      :path (expand-file-name filename)))
   (or callback 'ignore)))

(defun telega--deleteProfilePhoto (profile-photo-id &optional callback)
  "Delete profile photo by PROFILE-PHOTO-ID."
  (declare (indent 1))
  (telega-server--call
   (list :@type "deleteProfilePhoto"
         :profile_photo_id profile-photo-id)
   (or callback 'ignore)))

(defun telega--setChatPhoto (chat filename &optional callback)
  "Changes the photo of a CHAT.
Requires `:can_change_info' rights."
  (declare (indent 2))
  (telega-server--call
   (list :@type "setChatPhoto"
         :chat_id (plist-get chat :id)
         :photo (list :@type "inputFileLocal"
                      :path (expand-file-name filename)))
   (or callback 'ignore)))

(defun telega--setChatChatList (chat list-name &optional callback)
  "Move CHAT to a different chat list named LIST-NAME.
LIST-NAME is one of: \"Main\" or \"Archive\"."
  (telega-server--call
   (list :@type "setChatChatList"
         :chat_id (plist-get chat :id)
         :chat_list (list :@type (concat "chatList" (capitalize list-name))))
   (or callback 'ignore)))

(defun telega--setTdlibParameters ()
  "Set the parameters for TDLib initialization."
  (telega-server--send
   (list :@type "setTdlibParameters"
         :parameters (list :@type "tdlibParameters"
                           :use_test_dc (or telega-use-test-dc :false)
                           :database_directory telega-directory
                           :files_directory telega-cache-dir
                           :use_file_database telega-use-file-database
                           :use_chat_info_database telega-use-chat-info-database
                           :use_message_database telega-use-message-database
                           :use_secret_chats t
                           :api_id (car telega-app)
                           :api_hash (cdr telega-app)
                           :system_language_code telega-language
                           :device_model "Emacs"
                           :system_version emacs-version
                           :application_version telega-version
                           :enable_storage_optimizer t
                           :ignore_file_names :false
                           ))))

(defun telega--sendMessage (chat imc &optional reply-to-msg disable-notify
                                 from-background reply-markup callback)
  "Send the message content represented by IMC to CHAT.
If CALLBACK is specified, then call it with one argument - new
message uppon message is created."
  ;; We catch new message with `telega--on-updateNewMessage', so
  ;; ignore result returned from `sendMessage'
  (telega-server--call
   (nconc (list :@type "sendMessage"
                :chat_id (plist-get chat :id)
                :disable_notification (or disable-notify :false)
                :input_message_content imc)
          (when reply-to-msg
            (list :reply_to_message_id (plist-get reply-to-msg :id)))
          (when from-background
            (list :from_background t))
          (when reply-markup
            (list :reply_markup reply-markup)))
   (or callback 'ignore)))

(defun telega--sendMessageAlbum (chat imcs &optional reply-to-msg disable-notify
                                      from-background callback)
  "Send IMCS as media album.
If CALLBACK is specified, then call it with one argument - new
message uppon message is created."
  (let ((tsm (list :@type "sendMessageAlbum"
                   :chat_id (plist-get chat :id)
                   :disable_notification (or disable-notify :false)
                   :input_message_contents (apply 'vector imcs))))
    (when reply-to-msg
      (setq tsm (plist-put tsm :reply_to_message_id
                           (plist-get reply-to-msg :id))))
    (when from-background
      (setq tsm (plist-put tsm :from_background t)))
    (telega-server--call tsm (or callback 'ignore))))

(defun telega--sendInlineQueryResultMessage (chat imc &optional reply-to-msg
                                                  disable-notify from-background)
  "Send IMC as inline query result from bot.
If CALLBACK is specified, then call it with one argument - new
message uppon message is created."
  (telega-server--send
   (nconc (list :@type "sendInlineQueryResultMessage"
                :chat_id (plist-get chat :id)
                :disable_notification (or disable-notify :false)
                :query_id (plist-get imc :query-id)
                :result_id (plist-get imc :result-id))
          (when reply-to-msg
            (list :reply_to_message_id (plist-get reply-to-msg :id)))
          (when from-background
            (list :from_background t))
          (when (plist-get imc :hide-via-bot)
            (list :hide_via_bot t)))))

(defun telega--forwardMessages (chat from-chat messages &optional disable-notify
                                     from-background as-album
                                     send-copy remove-caption)
  "Forward MESSAGES FROM-CHAT into CHAT."
  (telega-server--send
   (list :@type "forwardMessages"
         :chat_id (plist-get chat :id)
         :from_chat_id (plist-get from-chat :id)
         :message_ids (cl-map 'vector (telega--tl-prop :id) messages)
         :disable_notification (or disable-notify :false)
         :from_background (or from-background :false)
         :as_album (or as-album :false)
         :send_copy (if send-copy t :false)
         :remove_caption (if remove-caption t :false))))

(defun telega--editMessageText (chat msg imc &optional reply-markup)
  "Edit the text of a message, or a text of a game message."
  (telega-server--send
   (nconc (list :@type "editMessageText"
                :chat_id (plist-get chat :id)
                :message_id (plist-get msg :id)
                :input_message_content imc)
          (when reply-markup
            (list :reply_markup reply-markup)))))

(defun telega--editMessageLiveLocation (chat msg location &optional reply-markup)
  "Edit the message content of a live location.
Pass nill to stop sharing live location."
  (telega-server--send
   (nconc (list :@type "editMessageLiveLocation"
                :chat_id (plist-get chat :id)
                :message_id (plist-get msg :id)
                :location location)
          (when reply-markup
            (list :reply_markup reply-markup)))))

(defun telega--editMessageMedia (chat msg imc &optional reply-markup)
  "Edit the content of a message with media content.
Media content is an animation, an audio, a document, a photo or a video."
  (telega-server--send
   (nconc (list :@type "editMessageMedia"
                :chat_id (plist-get chat :id)
                :message_id (plist-get msg :id)
                :input_message_content imc)
          (when reply-markup
            (list :reply_markup reply-markup)))))

(defun telega--editMessageCaption (chat msg caption &optional reply-markup)
  "Edits the message content caption."
  (telega-server--send
   (nconc (list :@type "editMessageCaption"
                :chat_id (plist-get chat :id)
                :message_id (plist-get msg :id)
                :caption caption)
          (when reply-markup
            (list :reply_markup reply-markup)))))

(defun telega--getActiveLiveLocationMessages (&optional callback)
  "Return list of messages with active live locatins."
  (with-telega-server-reply (reply)
      (append (plist-get reply :messages) nil)

    (list :@type "getActiveLiveLocationMessages")
    callback))

(defun telega--deleteMessages (chat-id message-ids &optional revoke)
  "Delete messages by its MESSAGES-IDS list.
If REVOKE is non-nil then delete message for all users."
  (telega-server--send
   (list :@type "deleteMessages"
         :chat_id chat-id
         :message_ids (apply 'vector message-ids)
         :revoke (or revoke :false))))

(defun telega--searchMessages (query last-msg &optional _list-name callback)
  "Search messages by QUERY.
Specify LAST-MSG to continue searching from LAST-MSG searched.
If LIST-NAME is given, then fetch chats from chat list named LIST-NAME.
LIST-NAME is one of: \"Main\" or \"Archive\".
If CALLBACK is specified, then do async call and run CALLBACK
with list of chats received."
  (with-telega-server-reply (reply)
      (append (plist-get reply :messages) nil)

    (list :@type "searchMessages"
          ;; DO NOT specify chatlist, some chat's in TDLib 1.5.4 does
          ;; not have :chat_list property and `searchMessages' won't
          ;; search for messages in them.  So we just search in all
          ;; chats and then filter messages

          ;; :chat_list (list :@type (concat "chatList"
          ;;                                 (capitalize (or list-name "Main"))))
          :query query
          :offset_date (or (plist-get last-msg :date) 0)
          :offset_chat_id (or (plist-get last-msg :chat_id) 0)
          :offset_message_id (or (plist-get last-msg :id) 0)
          :limit 100)
    callback))

(defun telega--getMapThumbnailFile (loc &optional zoom width height scale
                                        chat callback)
  "Get file with the map showing LOC.
ZOOM - zoom level in [13-20], default=13
WIDTH/HEIGHT - in [16-1024]
SCALE - in [1-3]"
  (declare (indent 6))
  (with-telega-server-reply (reply)
      (telega-file--ensure reply)

    (list :@type "getMapThumbnailFile"
          :location (list :@type "location"
                          :latitude (plist-get loc :latitude)
                          :longitude (plist-get loc :longitude))
          :zoom (or zoom 13)
          :width (or width 300)
          :height (or height 200)
          :scale (or scale 1)
          :chat_id (or (plist-get chat :id) 0))
    callback))

(defun telega--addContact (contact &optional share-phone-p)
  "Add CONTACT to contacts list.
If SHARE-PHONE-P is specified, then allow CONTACT to see my phone number."
  (telega-server--send
   (list :@type "addContact"
         :contact contact
         :share_phone_number (if share-phone-p t :false))))

(defun telega--sharePhoneNumber (user)
  "Share the phone number of the current user with a mutual contact USER."
  (telega-server--send
   (list :@type "sharePhoneNumber"
         :user_id (plist-get user :id))))

(defun telega--searchPublicChat (username &optional callback)
  "Search public chat with USERNAME.
If CALLBACK is specified, call it with one argument - CHAT."
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (when reply
        (telega-chat-get (plist-get reply :id)))

    (list :@type "searchPublicChat"
          :username username)
    callback))

(defun telega--searchPublicChats (query &optional callback)
  "Search public chats by looking for specified QUERY.
Return nil if QUERY is less then 5 chars.
If CALLBACK is specified, then do async call and run CALLBACK
with list of chats received."
  (unless (< (length query) 5)
    (with-telega-server-reply (reply)
        (mapcar #'telega-chat-get (plist-get reply :chat_ids))

      (list :@type "searchPublicChats"
            :query query)
      callback)))

(defun telega--searchChats (query &optional limit callback)
  "Search already known chats by QUERY."
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat-get (plist-get reply :chat_ids))

    (list :@type "searchChats"
          :query query
          :limit (or limit 200))
    callback))

(defun telega--searchChatsOnServer (query &optional limit callback)
  "Search already known chats on server by QUERY."
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat-get (plist-get reply :chat_ids))

    (list :@type "searchChatsOnServer"
          :query query
          :limit (or limit 200))
    callback))

(defun telega--getGroupsInCommon (with-user &optional limit callback)
  "Return list of common chats WITH-USER.
LIMIT - number of chats to get (default=100)"
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat-get (plist-get reply :chat_ids))

    (list :@type "getGroupsInCommon"
          :user_id (plist-get with-user :id)
          :offset_chat_id 0
          :limit (or limit 100))
    callback))

(defun telega--getTopChats (category &optional limit callback)
  "CATEGORY is string denoting category for the top chats.
CATEGORY is one of: \"Users\", \"Bots\", \"Groups\",
\"Channels\", \"InlineBots\", \"Calls\", \"ForwardChats\".
Default LIMIT is 30."
  (declare (indent 2))
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat-get (plist-get reply :chat_ids))

    (list :@type "getTopChats"
          :category (list :@type (concat "topChatCategory" category))
          :limit (or limit 30))
    callback))

(defun telega--getChats (&optional list-name callback)
  "Retreive all chats from the server in async manner.
If LIST-NAME is given, then fetch chats from chat list named LIST-NAME.
LIST-NAME is one of: \"Main\" or \"Archive\"."
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat--ensure
              (mapcar #'telega-chat-get (plist-get reply :chat_ids)))

    (let* ((chat-list (if list-name
                          (capitalize list-name)
                        "Main"))
           (last-chat (when (string= chat-list "Main")
                        (car (last telega--ordered-chats))))
           (offset-order (or (plist-get last-chat :order) "9223372036854775807"))
           (offset-chatid (or (plist-get last-chat :id) 0)))
      (list :@type "getChats"
            :offset_order offset-order
            :offset_chat_id offset-chatid
            :limit 1000
            :chat_list (list :@type (concat "chatList" chat-list))))
    callback))

(defun telega--reportChat (chat reason &optional messages)
  "Report a CHAT to the Telegram moderators.
REASON is one of: \"Spam\", \"Violence\", \"Pornography\",
\"ChildAbuse\", \"Copyright\" or \"UnrelatedLocation\"."
  (telega-server--send
   (list :@type "reportChat"
         :chat_id (plist-get chat :id)
         :reason (list :@type (concat "chatReportReason" reason))
         :message_ids (cl-map 'vector (telega--tl-prop :id) messages))))

(defun telega--removeChatActionBar (chat)
  "Remove CHAT's action bar without any other action."
  (telega-server--send
   (list :@type "removeChatActionBar"
         :chat_id (plist-get chat :id))))


;; I18N
(defun telega--getLocalizationTargetInfo (&optional offline callback)
  (with-telega-server-reply (reply)
      (append (plist-get reply :language_packs) nil)

    (list :@type "getLocalizationTargetInfo"
          :only_local (if offline t :false))
    callback))

(defun telega--getLanguagePackInfo (lang-pack-id &optional callback)
  (declare (indent 1))
  (telega-server--call
   (list :@type "getLanguagePackInfo"
         :language_pack_id lang-pack-id)
   callback))

(defun telega--getLanguagePackStrings (lang-pack-id &optional keys callback)
  (declare (indent 2))
  (with-telega-server-reply (reply)
      (mapcar (lambda (str)
                (cons (telega-tl-str str :key)
                      (cddr (plist-get str :value))))
              (plist-get reply :strings))

    (list :@type "getLanguagePackStrings"
          :language_pack_id lang-pack-id
          :keys (apply 'vector keys))
    callback))

(defun telega--setCustomLanguagePackString (lang-pack-id str-key &rest str-val)
  (telega-server--call
   (list :@type "setCustomLanguagePackString"
         :language_pack_id lang-pack-id
         :new_string
         (list :@type "languagePackString"
               :key str-key
               :value (nconc (list :@type (if (plist-get str-val :value)
                                              "languagePackStringValueOrdinary"
                                            "languagePackStringValuePluralized"))
                             str-val)))))

(provide 'telega-tdlib)

;;; telega-tdlib.el ends here

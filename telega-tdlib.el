;;; telega-tdlib.el --- TDLib API interface  -*- lexical-binding:t -*-

;; Copyright (C) 2019-2020 by Zajcev Evgeny.

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

;; Emacs lisp interface to TDLib API v1.7.0

;;; Code:
(require 'telega-core)
(require 'telega-server)

(declare-function telega-chat-get "telega-chat" (chat-id &optional offline-p))
(declare-function telega-chat--ensure "telega-chat" (chat))
(declare-function telega-chatbuf--message-thread-id "telega-chat")
(declare-function telega-stickerset--ensure "telega-sticker" (sset))
(declare-function telega-user-get "telega-user" (user-id))
(declare-function telega-file--ensure "telega-media" (file))

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

(defun telega--MessageSender (msg-sender)
  "Convert user or chat to TDLib MessageSender."
  (if (telega-user-p msg-sender)
      (list :@type "messageSenderUser"
            :user_id (plist-get msg-sender :id))
    (cl-assert (telega-chat-p msg-sender))
    (list :@type "messageSenderChat"
          :chat_id (plist-get msg-sender :id))))

(defun telega--getOption (prop-kw &optional callback)
  (declare (indent 1))
  (telega-server--call
   (list :@type "getOption"
         :name (substring (symbol-name prop-kw) 1)) ; strip `:'
   callback))

(defun telega--setOption (prop-kw val &optional sync-p)
  "Set option, defined by keyword PROP-KW to VAL.
If SYNC-P is specified, then set option is sync manner."
  (declare (indent 1))
  (funcall (if sync-p #'telega-server--call #'telega-server--send)
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

(defun telega--addProxy (proxy-spec)
  "Add PROXY-SPEC to the list of proxies."
  (telega-server--send
   `(:@type "addProxy" ,@proxy-spec)))

(defun telega--searchEmojis (text &optional exact-match-p
                                  language-codes callback)
  "Search for emojis by TEXT keywords.
Non-nil EXACT-MATCH-P to return only emojis that exactly matches TEXT."
  (with-telega-server-reply (reply)
      (mapcar (lambda (emoji)
                (telega--desurrogate-apply emoji 'no-props))
              (plist-get reply :emojis))

    (list :@type "searchEmojis"
          :text text
          :exact_match (or exact-match-p :false)
          :input_language_codes (apply #'vector language-codes))
    callback))

(defun telega--getAnimatedEmoji (emoji &optional callback)
  "Return an animated emoji corresponding to a given EMOJI."
  (telega-server--call
   (list :@type "getAnimatedEmoji"
         :emoji emoji)
   callback))

(defun telega--getCustomEmojiStickers (custom-emoji-ids &optional callback)
  "Returns list of custom emoji stickers by their identifiers."
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (append (plist-get reply :stickers) nil)

    (list :@type "getCustomEmojiStickers"
          :custom_emoji_ids (apply #'vector custom-emoji-ids))
    callback))

(defun telega--setChatTitle (chat title)
  "Changes the CHAT title to TITLE."
  (telega-server--send
   (list :@type "setChatTitle"
         :chat_id (plist-get chat :id)
         :title title)))

(defun telega--toggleChatDefaultDisableNotification (chat disable-p)
  (telega-server--send
   (list :@type "toggleChatDefaultDisableNotification"
         :chat_id (plist-get chat :id)
         :default_disable_notification (if disable-p t :false))))

(defun telega--setChatDescription (chat descr)
  "Set CHAT's description to DESCR."
  (telega-server--send
   (list :@type "setChatDescription"
         :chat_id (plist-get chat :id)
         :description (or descr ""))))

(defun telega--createNewBasicGroupChat (title users &optional callback)
  "Create new basicgroup with TITLE and USERS.
Return (or call the CALLBACK with) newly created chat."
  (with-telega-server-reply (reply)
      (telega-chat-get (plist-get reply :id))

    (list :@type "createNewBasicGroupChat"
          :user_ids (cl-map #'vector (telega--tl-prop :id) users)
          :title title)
    callback))

(cl-defun telega--createNewSupergroupChat (title &key forum-p channel-p
                                                 description location callback)
  "Create new supergroup with TITLE.
Specify non-nil CHANNEL-P to create new channel.
Specify LOCATION to create location-based supergroup.
Return (or call the CALLBACK with) newly created chat."
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (telega-chat-get (plist-get reply :id))

    (nconc (list :@type "createNewSupergroupChat"
                 :title title
                 :is_forum (if forum-p t :false)
                 :is_channel (if channel-p t :false))
           (when description
             (list :description description))
           (when location
             (list :location location)))
    callback))

(defun telega--createNewSecretChat (user)
  "Create secret chat with USER.
Return newly created chat."
  (telega-chat-get
   (plist-get
    (telega-server--call
     (list :@type "createNewSecretChat"
           :user_id (plist-get user :id))) :id)))

(defun telega--createBasicGroupChat (basic-group-id &optional force)
  "Return an existing chat corresponding to a known basicgroup.
BASIC-GROUP-ID is the id of the basicgroup."
  (telega-chat-get
   (plist-get
    (telega-server--call
     (list :@type "createBasicGroupChat"
           :basic_group_id basic-group-id
           :force (if force t :false)))
    :id)))

(defun telega--upgradeBasicGroupChatToSupergroupChat (chat &optional callback)
  "Creates a new supergroup from an existing basic.
Requires creator privileges.
Deactivates the original basic group.
Return (or call the CALLBACK with) newly create chat."
  (with-telega-server-reply (reply)
      (telega-chat-get (plist-get reply :id))

    (list :@type "upgradeBasicGroupChatToSupergroupChat"
          :chat_id (plist-get chat :id))
    callback))

(defun telega--createSupergroupChat (supergroup-id &optional force)
  "Create chat for SUPERGROUP-ID."
  (telega-chat-get
   (plist-get
    (telega-server--call
     (list :@type "createSupergroupChat"
           :supergroup_id supergroup-id
           :force (if force t :false)))
    :id)))

(defun telega--setSupergroupUsername (supergroup username)
  "Change SUPERGROUP's username to USERNAME.
Requires owner right."
  (telega-server--send
   (list :@type "setSupergroupUsername"
         :supergroup_id (plist-get supergroup :id)
         :username username)))

(defun telega--toggleSupergroupSignMessages (supergroup sign-messages-p)
  (telega-server--send
   (list :@type "toggleSupergroupSignMessages"
         :supergroup_id (plist-get supergroup :id)
         :sign_messages (if sign-messages-p t :false))))

(defun telega--toggleSupergroupIsAllHistoryAvailable (supergroup all-history-available-p)
  (telega-server--send
   (list :@type "toggleSupergroupIsAllHistoryAvailable"
         :supergroup_id (plist-get supergroup :id)
         :is_all_history_available (if all-history-available-p t :false))))

(defun telega--reportSupergroupSpam (supergroup msg &rest other-messages)
  "Report some messages in a supergroup as spam.
all messages must have same user sender."
  (telega-server--send
   (list :@type "reportSupergroupSpam"
         :supergroup_id (plist-get supergroup :id)
         :message_ids (cl-map #'vector (telega--tl-prop :id)
                              (cons msg other-messages)))))

(defun telega--createSecretChat (secret-chat-id)
  "Return existing secret chat with id equal to SECRET-CHAT-ID."
  (telega-chat-get
   (plist-get
    (telega-server--call
     (list :@type "createSecretChat"
           :secret_chat_id secret-chat-id))
    :id)))

(defun telega--closeSecretChat (secretchat)
  "Close SECRETCHAT."
  (telega-server--send
   (list :@type "closeSecretChat"
         :secret_chat_id (plist-get secretchat :id))))

(defun telega-chat-message-thread-id (chat)
  "Return current message_thread_id for the CHAT."
  (or (with-telega-chatbuf chat
        (telega-chatbuf--message-thread-id))
      0))

(defun telega--sendChatAction (chat action)
  "Send ACTION on CHAT."
  (telega-server--send
   (list :@type "sendChatAction"
         :chat_id (plist-get chat :id)
         :message_thread_id (telega-chat-message-thread-id chat)
         :action action)))

(defun telega--getMessageEmbeddingCode (msg &optional for-album-p)
  "Returns an HTML code for embedding the message MSG."
  ;; TDLib: Available only for messages in supergroups and channels
  ;; with a username
  (plist-get
   (telega-server--call
    (list :@type "getMessageEmbeddingCode"
          :chat_id (plist-get msg :chat_id)
          :message_id (plist-get msg :id)
          :for_album (if for-album-p t :false)))
   :text))

(defun telega--getMessage (chat-id msg-id &optional callback)
  "Get message by CHAT-ID and MSG-ID.
If CALLBACK is specified, then get message asynchronously.
If message is not found, then return `nil'."
  (declare (indent 2))
  (with-telega-server-reply (reply)
      (unless (telega--tl-error-p reply)
        reply)

    (list :@type "getMessage"
          :chat_id chat-id
          :message_id msg-id)
    callback))

(defun telega--getRepliedMessage (msg &optional callback)
  "Returns information about a message that is replied by a given message.
Also returns the pinned message, the game message, and the invoice
message for messages of the types messagePinMessage, messageGameScore,
and messagePaymentSuccessful respectively."
  (declare (indent 1))
  (telega-server--call
   (list :@type "getRepliedMessage"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id))
   callback))

(defun telega--getChatMessageByDate (chat-id date &optional callback)
  "Returns the last message sent in a chat no later than the specified DATE.
DATE is a unix timestamp."
  (declare (indent 2))
  (with-telega-server-reply (reply)
      (unless (telega--tl-error-p reply)
        reply)

    (list :@type "getChatMessageByDate"
          :chat_id chat-id
          :date date)
    callback))

(defun telega--getMessages (chat-id message-ids &optional callback)
  "Get messages by CHAT-ID and MESSAGE-IDS."
  (declare (indent 2))
  (with-telega-server-reply (reply)
      ;; Filter out nil messages
      ;; See https://github.com/tdlib/td/issues/1511
      (delq nil (append (plist-get reply :messages) nil))

    (list :@type "getMessages"
          :chat_id chat-id
          :message_ids (apply #'vector message-ids))
    callback))

(cl-defun telega--getMessageLink (msg &key for-album-p for-thread-p
                                      media-timestamp)
  "Get https link for message MSG in a supergroup or a channel."
  (declare (indent 1))
  (plist-get
   (telega-server--call
    (list :@type "getMessageLink"
          :chat_id (plist-get msg :chat_id)
          :message_id (plist-get msg :id)
          :media_timestamp (or media-timestamp 0)
          :for_album (if for-album-p t :false)
          :in_message_thread (if for-thread-p t :false)))
   :link))

(defun telega--recognizeSpeech (msg)
  "Recognize speech in a voice note message MSG."
  (telega-server--send
   (list :@type "recognizeSpeech"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id))))

(defun telega--replacePrimaryChatInviteLink (chat)
  "Generate a new primary invite link for a CHAT.
Available for basic groups, supergroups, and channels.
Return generated link as string."
  (plist-get
   (telega-server--call
    (list :@type "replacePrimaryChatInviteLink"
          :chat_id (plist-get chat :id)))
   :invite_link))

(defun telega--checkChatInviteLink (invite-link &optional callback)
  "Check invitation link INVITE-LINK."
  (telega-server--call
   (list :@type "checkChatInviteLink"
         :invite_link invite-link)
   callback))

(defun telega--joinChat (chat)
  "Add current user as a new member to a CHAT."
  (telega-server--send
   (list :@type "joinChat" :chat_id (plist-get chat :id))))

(defun telega--joinChatByInviteLink (invite-link &optional callback)
  "Return new chat by its INVITE-LINK.
Return nil if can't join the chat."
  (with-telega-server-reply (reply)
      (telega-chat-get (plist-get reply :id))

    (list :@type "joinChatByInviteLink"
          :invite_link invite-link)
    callback))

(defun telega--leaveChat (chat)
  "Remove current user from CHAT members."
  (telega-server--send
   (list :@type "leaveChat" :chat_id (plist-get chat :id))))

(defun telega--getChatPinnedMessage (chat &optional callback)
  "Get newest pinned message for the CHAT, if any."
  (telega-server--call
   (list :@type "getChatPinnedMessage"
         :chat_id (plist-get chat :id))
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

(defun telega--getChatAvailableMessageSenders (chat &optional callback)
  "Return message senders available for the CHAT."
  (with-telega-server-reply (reply)
      (mapcar #'telega-msg-sender (plist-get reply :senders))
    (list :@type "getChatAvailableMessageSenders"
          :chat_id (plist-get chat :id))
    callback))

(defun telega--setChatMessageSender (chat sender)
  "For CHAT set default message SENDER."
  (telega-server--send
   (list :@type "setChatMessageSender"
         :chat_id (plist-get chat :id)
         :message_sender_id (telega--MessageSender sender))))

(defun telega--getPaymentForm (invoice)
  "Return a payment form for an INVOICE.
INVOICE could be a message or a name from the `internalLinkTypeInvoice' link.
TDLib 1.8.4"
  (telega-server--call
   (list :@type "getPaymentForm"
         :input_invoice
         (cond ((telega-msg-p invoice)
                (list :@type "inputInvoiceMessage"
                      :chat_id (plist-get invoice :chat_id)
                      :message_id (plist-get invoice :id)))
               ((stringp invoice)
                (list :@type "inputInvoiceName"
                      :name invoice))
               (t (error "telega: invalid invoice: %S" invoice))))))

(defun telega--sendPaymentForm (msg order-info-id shipping-id credentials)
  (telega-server--call
   (list :@type "sendPaymentForm"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id)
         :order_info_id (or order-info-id "")
         :shipping_option_id (or shipping-id "")
         :credentials credentials)))

(defun telega--getCreatedPublicChats (chat-type &optional callback)
  "Return list of public chats created by the user.
CHAT-TYPE is one of `has-username' or `location-based'."
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat-get (append (plist-get reply :chat_ids) nil))

   (list :@type "getCreatedPublicChats"
         :type (cl-ecase chat-type
                 (has-username
                  (list :@type "publicChatTypeHasUsername"))
                 (location-based
                  (list :@type "publicChatTypeIsLocationBased"))))
   callback))

(defun telega--getInactiveSupergroupChats (&optional callback)
  "Return a list of recently inactive supergroups and channels.
Can be used when user reaches limit on the number of joined
supergroups and channels and receives CHANNELS_TOO_MUCH error."
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat-get (plist-get reply :chat_ids))

   (list :@type "getInactiveSupergroupChats")
   callback))

(defun telega--getSuitableDiscussionChats (&optional callback)
  "Returns a list of chats suitable to be discussion group for a channel."
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat-get (plist-get reply :chat_ids))

   (list :@type "getSuitableDiscussionChats")
   callback))

(defun telega--setMessageSenderBlockList (msg-sender block-list
                                                     &optional callback)
  "Set Toggle block state of a CHAT."
  (telega-server--call
   (list :@type "setMessageSenderBlockList"
         :sender_id (telega--MessageSender msg-sender)
         :block_list (when block-list
                       (list :@type (symbol-name block-list))))
   (or callback #'ignore)))

(defun telega--getBlockedMessageSenders (block-list &optional offset callback)
  "Get list of chats blocked by me.
BLOCK-LIST is one of `blockListMain' or `blockListStories'.
First element is the list is total number of blocked message senders."
  (declare (indent 2))
  (with-telega-server-reply (reply)
      (nconc (list block-list (plist-get reply :total_count))
            (mapcar #'telega-msg-sender (plist-get reply :senders)))

    (list :@type "getBlockedMessageSenders"
          :block_list (list :@type (symbol-name block-list))
          :offset (or offset 0)
          :limit 100)
    callback))

(defun telega--blockMessageSenderFromReplies
    (msg delete-msg-p &optional delete-all-msg-p report-spam-p)
  "Block an original sender of a message in the Replies chat."
  (telega-server--send
   (list :@type "blockMessageSenderFromReplies"
         :message-id (plist-get msg :id)
         :delete_message (if delete-msg-p t :false)
         :delete_all_messages (if delete-all-msg-p t :false)
         :report_spam (if report-spam-p t :false))))

(cl-defun telega--getStickers
    (query &key chat (limit 100)
           (tl-sticker-type '(:@type "stickerTypeRegular")) callback)
  "Return installed stickers that correspond to a given QUERY.
QUERY could be an emoji or a keyword prefix.
LIMIT defaults to 20."
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (append (plist-get reply :stickers) nil)

    (nconc (list :@type "getStickers"
                 :sticker_type tl-sticker-type
                 :query query
                 :limit limit)
           (when chat
             (list :chat_id (plist-get chat :id))))
    callback))

(cl-defun telega--getAllStickerEmojis (query &key chat
                                             (tl-sticker-type
                                              '(:@type "stickerTypeRegular"))
                                             only-main-p
                                             callback)
  "Return unique emoji that correspond to installed stickers."
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (append (plist-get reply :stickers) nil)

    (nconc (list :@type "getAllStickerEmojis"
                 :query query
                 :sticker_type tl-sticker-type
                 :return_only_main_emoji (if only-main-p t :false))
           (when chat
             (list :chat_id (plist-get chat :id))))
    callback))

(cl-defun telega--searchStickers (emoji &key (tl-sticker-type
                                              '(:@type "stickerTypeRegular"))
                                        limit callback)
  "Search for the public stickers that correspond to a given EMOJI.
LIMIT defaults to 20."
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (append (plist-get reply :stickers) nil)

    (list :@type "searchStickers"
          :emojis emoji
          :sticker_type tl-sticker-type
          :limit (or limit 100))
    callback))

(cl-defun telega--getInstalledStickerSets (&key (tl-sticker-type
                                                 '(:@type "stickerTypeRegular"))
                                                callback)
  "Return a list of installed sticker sets.
TL-STICKER-TYPE is a TL StickerType object, such as.
By default TL-STICKER-TYPE is `(:@type \"stickerTypeRegular\")'."
  (declare (indent 1))
  (cl-assert (member tl-sticker-type
                     '((:@type "stickerTypeRegular")
                       (:@type "stickerTypeCustomEmoji")))
             "installed masks not yet supported")
  (with-telega-server-reply (reply)
      (append (plist-get reply :sets) nil)

    (list :@type "getInstalledStickerSets"
          :sticker_type tl-sticker-type)
    callback))

(cl-defun telega--getTrendingStickerSets
    (&key (offset 0) (limit 100) callback
          (tl-sticker-type '(:@type "stickerTypeRegular")))
  "Return a list of trending sticker sets."
  (with-telega-server-reply (reply)
      (append (plist-get reply :sets) nil)

    (list :@type "getTrendingStickerSets"
          :sticker_type tl-sticker-type
          :offset offset
          :limit limit)
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
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (telega-stickerset--ensure reply)

    (list :@type "searchStickerSet"
          :name name)
    callback))

(defun telega--searchStickerSets (query &optional callback)
  "Searches for ordinary sticker sets by looking for specified QUERY."
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (append (plist-get reply :sets) nil)

    (list :@type "searchStickerSets"
          :query query)
    callback))

(defun telega--viewTrendingStickerSets (set-id &rest other-ids)
  (telega-server--call
   (list :@type "viewTrendingStickerSets"
         :sticker_set_ids (apply 'vector set-id other-ids))))

(defun telega--getRecentStickers (&optional attached-p callback)
  "Returns a list of recently used stickers.
Pass non-nil ATTACHED-P to return only stickers attached to photos/videos."
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (append (plist-get reply :stickers) nil)

    (list :@type "getRecentStickers"
          :is_attached (if attached-p t :false))
    callback))

(defun telega--getFavoriteStickers (&optional callback)
  "Return favorite stickers."
  (declare (indent 0))
  (with-telega-server-reply (reply)
      (append (plist-get reply :stickers) nil)
    (list :@type "getFavoriteStickers")
    callback))

(defun telega--addFavoriteSticker (sticker-input-file &optional callback)
  "Add STICKER-INPUT-FILE on top of favorite stickers."
  (telega-server--call
   (list :@type "addFavoriteSticker"
         :sticker sticker-input-file)
   callback))

(defun telega--removeFavoriteSticker (sticker-input-file &optional callback)
  (telega-server--call
   (list :@type "removeFavoriteSticker"
         :sticker sticker-input-file)
   callback))

(defun telega--getStickerEmojis (sticker-input-file &optional callback)
  (telega-server--call
   (list :@type "getStickerEmojis"
         :sticker sticker-input-file)
   callback))

(defun telega--changeStickerSet (stickerset install-p &optional archive-p)
  "Install/Uninstall STICKERSET."
  (telega-server--call
   (list :@type "changeStickerSet"
         :set_id (plist-get stickerset :id)
         :is_installed (or install-p :false)
         :is_archived (or archive-p :false))))

(defun telega--getAttachedStickerSets (file-id)
  "Return sticker sets attached to the FILE-ID.
Photo and Video files have attached sticker sets."
  (telega-server--call
   (list :@type "getAttachedStickerSets"
         :file_id file-id)))

(defun telega--searchInstalledStickerSets (query &optional masks-p limit)
  "Searches for installed sticker sets by QUERY."
  (telega-server--call
   (list :@type "searchInstalledStickerSets"
         :is_masks (or masks-p :false)
         :query query
         :limit (or limit 20))))

(defun telega--getSavedAnimations ()
  "Return list of saved animations."
  (let* ((reply (telega-server--call
                 (list :@type "getSavedAnimations")))
         (anims (append (plist-get reply :animations) nil)))
    ;; Start downloading animations
    (when telega-animation-download-saved
      (mapc 'telega-animation--download anims))
    anims))

(defun telega--addSavedAnimation (input-file)
  "Manually adds a new animation to the list of saved animations."
  (telega-server--send
   (list :@type "addSavedAnimation"
         :animation input-file)))

(defun telega--removeSavedAnimation (input-file)
  "Removes an animation from the list of saved animations."
  (telega-server--send
   (list :@type "removeSavedAnimation"
         :animation input-file)))

(defun telega--getRecentInlineBots (&optional callback)
  "Return recently used inline bots."
  (with-telega-server-reply (reply)
      (mapcar #'telega-user-get (plist-get reply :user_ids))

    (list :@type "getRecentInlineBots")
    callback))

(defun telega--getWebPageInstantView (url &optional partial)
  "Return instant view for the URL.
Return nil if URL is not available for instant view."
  (let ((reply (telega-server--call
                (list :@type "getWebPageInstantView"
                      :url url
                      :force_full (or (not partial) :false)))))
    ;; NOTE: May result in 404 error, return nil in this case
    (and reply
         (eq (telega--tl-type reply) 'webPageInstantView)
         reply)))

(defun telega--resendMessages (&rest messages)
  "Resend MESSAGES."
  (telega-server--send
   (list :@type "resendMessages"
         :chat_id (plist-get (car messages) :chat_id)
         :message_ids (cl-map 'vector (telega--tl-prop :id) messages))))

(defun telega--deleteChatReplyMarkup (message)
  "Deletes the default reply markup from a chat.
Must be called after a one-time keyboard or a ForceReply reply
markup has been used."
  (telega-server--send
   (list :@type "deleteChatReplyMarkup"
         :chat_id (plist-get message :chat_id)
         :message_id (plist-get message :id))))

(cl-defun telega--searchChatMembers (chat query &optional members-filter
                                          &key limit as-member-p callback)
  "Search CHAT members by QUERY.
MEMBERS-FILTER is TDLib's \"ChatMembersFilter\".
LIMIT by default is 200.
If AS-MEMBER-P is non-nil, then return \"chatMember\" structs instead
of message sender."
  (declare (indent 3))
  (with-telega-server-reply (reply)
      (mapcar (if as-member-p #'identity #'telega-msg-sender)
              (plist-get reply :members))
    (nconc (list :@type "searchChatMembers"
                 :chat_id (plist-get chat :id)
                 :query query
                 :limit (or limit 200))
           (when members-filter
             (list :filter members-filter)))
    callback))

(defun telega--getChatAdministrators (chat &optional callback)
  "Return a list of administrators for the CHAT."
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (append (plist-get reply :administrators) nil)
    (list :@type "getChatAdministrators"
          :chat_id (plist-get chat :id))
    callback))

(defun telega--clearAllDraftMessages (&optional exclude-secret-chats-p)
  "Clear draft messages in all chats."
  (telega-server--send
   (list :@type "clearAllDraftMessages"
         :exclude_secret_chats (if exclude-secret-chats-p t :false))))

(defun telega--getChatNotificationSettingsExceptions
    (scope-type &optional compare-sound-p callback)
  "Returns list of chats with non-default notification settings."
  (declare (indent 2))
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat-get (append (plist-get reply :chat_ids) nil))

    (list :@type "getChatNotificationSettingsExceptions"
          :scope (list :@type scope-type)
          :compare_sound (if compare-sound-p t :false))
    callback))

(defun telega--getScopeNotificationSettings (scope-type &optional callback)
  "Return the notification settings for chats of a given type SCOPE-TYPE.
SCOPE-TYPE is one of:
\"notificationSettingsScopePrivateChats\",
\"notificationSettingsScopeGroupChats\" or
\"notificationSettingsScopeChannelChats\"."
  (telega-server--call
   (list :@type "getScopeNotificationSettings"
         :scope (list :@type scope-type))
   callback))

(defun telega--setScopeNotificationSettings (scope-type &rest settings)
  "Change notification settings for chats of a given SCOPE-TYPE.
SCOPE-TYPE is the same as in `telega--getScopeNotificationSettings'.
SETTINGS is a plist with notification settings to set."
  (let ((scope-settings (telega-chat-notification-scope scope-type))
        (request (list :@type "scopeNotificationSettings")))
    (telega--tl-dolist ((prop-name value) (append scope-settings settings))
      (setq request (plist-put request prop-name (or value :false))))
    (cl-assert (stringp scope-type))
    (telega-server--send
     (list :@type "setScopeNotificationSettings"
           :scope (list :@type scope-type)
           :notification_settings request))))

(defun telega--resetAllNotificationSettings ()
  "Resets all notification settings to their default values.
By default, all chats are unmuted, the sound is set to
\"default\" and message previews are shown."
  (telega-server--call
   (list :@type "resetAllNotificationSettings")))

(defun telega--getSupergroupFullInfo (supergroup-id &optional callback)
  "Refresh SUPERGROUP full info."
  (declare (indent 1))
  (telega-server--call
   (list :@type "getSupergroupFullInfo"
         :supergroup_id supergroup-id)
   callback))

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

(defun telega--toggleSupergroupHasAggressiveAntiSpamEnabled (supergroup has-aggressive-antispam-p)
  "Toggle whether aggressive anti-spam checks are enabled in the supergroup."
  (telega-server--send
   (list :@type "toggleSupergroupHasAggressiveAntiSpamEnabled"
         :supergroup_id (plist-get supergroup :id)
         :has_aggressive_anti_spam_enabled (if has-aggressive-antispam-p t :false))))

(defun telega--toggleSupergroupHasHiddenMembers (supergroup has-hidden-members-p)
  "Toggles whether non-administrators can receive only administrators and bots."
  (telega-server--send
   (list :@type "toggleSupergroupHasHiddenMembers"
         :supergroup_id (plist-get supergroup :id)
         :has_hidden_members (if has-hidden-members-p t :false))))

(defun telega--setChatMemberStatus (chat msg-sender status &optional callback)
  "Change the STATUS of a MSG-SENDER, needs appropriate privileges.
STATUS is a tl object."
  (telega-server--send
   (list :@type "setChatMemberStatus"
         :chat_id (plist-get chat :id)
         :member_id (telega--MessageSender msg-sender)
         :status status)
   callback))

(defun telega--banChatMember (chat msg-sender &optional revoke-messages-p
                                   until-date callback)
  "Ban MSG-SENDER in the CHAT."
  (telega-server--send
   (list :@type "banChatMember"
         :chat_id (plist-get chat :id)
         :member_id (telega--MessageSender msg-sender)
         :revoke_messages (if revoke-messages-p t :false)
         :banned_until_date (or until-date 0))
   callback))

(defun telega--addChatMember (chat user &optional forward-limit)
  "Add new member USER to the CHAT."
  (telega-server--send
   (list :@type "addChatMember"
         :chat_id (plist-get chat :id)
         :user_id (plist-get user :id)
         :forward_limit (or forward-limit 100))))

(defun telega--addChatMembers (chat users)
  "Add new members to the CHAT.
CHAT must be supergroup or channel."
  (telega-server--send
   (list :@type "addChatMembers"
         :chat_id (plist-get chat :id)
         :user_ids (cl-map #'vector (telega--tl-prop :id) users))))

(defun telega--canTransferOwnership (&optional callback)
  (telega-server--call
   (list :@type "canTransferOwnership")
   callback))

(defun telega--transferChatOwnership (chat to-user password &optional callback)
  "Transfer ownership of the CHAT supergroup TO-USER."
  (declare (indent 3))
  (telega-server--call
   (list :@type "transferChatOwnership"
         :chat_id (plist-get chat :id)
         :user_id (plist-get to-user :id)
         :password password)
   (or callback 'ignore)))

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

(defun telega--getConnectedWebsites (&optional callback)
  (with-telega-server-reply (reply)
      (append (plist-get reply :websites) nil)

    (list :@type "getConnectedWebsites")
    callback))

(defun telega--disconnectWebsite (website-id)
  (telega-server--send
   (list :@type "disconnectWebsite"
         :website_id website-id)))

(defun telega--disconnectAllWebsites ()
  (telega-server--send
   (list :@type "disconnectAllWebsites")))

(defun telega--getProxies (&optional callback)
  "Return list of currently registered proxies."
  (with-telega-server-reply (reply)
      (append (plist-get reply :proxies) nil)

    (list :@type "getProxies")
    callback))

(defun telega--pinChatMessage (msg &optional disable-notifications only-for-self)
  "Pin message MSG.
Specify non-nil DISABLE-NOTIFICATIONS, if there should be no
notification about the pinned message. Notifications are always
disabled in channels and private chats.
For message in private chat, non-nil ONLY-FOR-SELF can be
specified, to pin message for self only."
  (telega-server--send
   (list :@type "pinChatMessage"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id)
         :disable_notification (if disable-notifications t :false)
         :only_for_self (if only-for-self t :false))))

(defun telega--unpinChatMessage (msg)
  "Unpin message MSG in the corresponding chat."
  (telega-server--send
   (list :@type "unpinChatMessage"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id))))

(defun telega--unpinAllChatMessages (chat)
  "Remove all pinned messages from a chat."
  (telega-server--send
   (list :@type "unpinAllChatMessages"
         :chat_id (plist-get chat :id))))

(defun telega--getChatHistory (chat from-msg-id offset
                                    &optional limit only-local callback)
  "Returns messages in a chat.
The messages are returned in a reverse chronological order."
  (declare (indent 5))
  (telega-server--call
   (list :@type "getChatHistory"
         :chat_id (plist-get chat :id)
         :from_message_id from-msg-id
         :offset offset
         :limit (or limit telega-chat-history-limit)
         :only_local (or only-local :false))
   callback))

(defun telega--deleteChat (chat)
  "Delete a CHAT along with all messages.
Requires owner privileges."
  (telega-server--send
   (list :@type "deleteChat"
         :chat_id (plist-get chat :id))))

(defun telega--deleteChatHistory (chat &optional remove-from-list revoke)
  "Deletes all messages in the CHAT only for the user.
Cannot be used in channels and public supergroups.
Pass REVOKE to try to delete chat history for all users."
  (telega-server--send
   (list :@type "deleteChatHistory"
         :chat_id (plist-get chat :id)
         :remove_from_chat_list (if remove-from-list t :false)
         :revoke (if revoke t :false))))

(defun telega--getChatScheduledMessages (chat &optional callback)
  "Return all scheduled messages in a CHAT."
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (append (plist-get reply :messages) nil)

    (list :@type "getChatScheduledMessages"
          :chat_id (plist-get chat :id))
    callback))

(cl-defun telega--searchChatMessages (chat tdlib-msg-filter
                                           from-msg-id offset
                                           &key query limit sender callback)
  "Search messages in CHAT by QUERY."
  (declare (indent 4))
  (cl-assert (and tdlib-msg-filter from-msg-id offset))
  (telega-server--call
    (nconc (list :@type "searchChatMessages"
                 :chat_id (plist-get chat :id)
                 :filter tdlib-msg-filter
                 ;; NOTE: for `searchMessagesFilterPinned' filter do
                 ;; not set `:message_thread_id', because threads does
                 ;; not have pinned messages, and request with thread
                 ;; id will result in error.
                 :message_thread_id
                 (if (eq 'searchMessagesFilterPinned
                         (telega--tl-type tdlib-msg-filter))
                     0
                   (telega-chat-message-thread-id chat))
                 :query (or query "")
                 :from_message_id from-msg-id
                 :offset offset
                 :limit (or limit telega-chat-history-limit))
           (when sender
             (list :sender_id (telega--MessageSender sender))))
    callback))

(defun telega--getChatMessageCount (chat tdlib-msg-filter
                                         &optional local-p callback)
  "Return approximate number of messages of FILTER type in the CHAT.
Specify non-nil LOCAL-P to avoid network requests."
  (declare (indent 3))
  (with-telega-server-reply (reply)
      (plist-get reply :count)
    (list :@type "getChatMessageCount"
          :chat_id (plist-get chat :id)
          :filter tdlib-msg-filter
          :return_local (if local-p t :false))
    callback))

(defun telega--getChatMessagePosition (msg tdlib-msg-filter
                                           &optional msg-thread-id callback)
  "Returns approximate 1-based position of a message among found messages.
Which can be found by the specified FILTER in the chat."
  (declare (indent 3))
  ;; NOTE: from TDLib docs: Filter for message content;
  ;; searchMessagesFilterEmpty, searchMessagesFilterUnreadMention,
  ;; searchMessagesFilterUnreadReaction, and
  ;; searchMessagesFilterFailedToSend are unsupported in this function
  (cl-assert (or (not tdlib-msg-filter)
                 (not (memq (telega--tl-type tdlib-msg-filter)
                            '(searchMessagesFilterEmpty
                              searchMessagesFilterUnreadMention
                              searchMessagesFilterUnreadReaction
                              searchMessagesFilterFailedToSend)))))
  (with-telega-server-reply (reply)
      (plist-get reply :count)
    (list :@type "getChatMessagePosition"
          :chat_id (plist-get msg :chat_id)
          :message_id (plist-get msg :id)
          :filter tdlib-msg-filter
          :message_thread_id
          (or msg-thread-id
              (telega-chat-message-thread-id (telega-msg-chat msg))))
    callback))

;; Threads
(defun telega--getMessageThreadHistory (chat thread-msg-id from-msg-id offset
                                             &optional limit callback)
  "Return messages in a message thread of a message."
  (declare (indent 5))
  (telega-server--call
   (list :@type "getMessageThreadHistory"
         :chat_id (plist-get chat :id)
         :message_id thread-msg-id
         :from_message_id from-msg-id
         :offset offset
         :limit (or limit telega-chat-history-limit))
   callback))

(defun telega--getMessageThread (chat msg-id &optional callback)
  (telega-server--call
   (list :@type "getMessageThread"
         :chat_id (plist-get chat :id)
         :message_id msg-id)
   callback))

(defun telega--getMessageViewers (msg &optional callback)
  "Get MSG message viewers list.
TDLib 1.8.12:
  getMessageViewers chat_id:int53 message_id:int53 = MessageViewers;"
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (append (plist-get reply :viewers) nil)

    (list :@type "getMessageViewers"
          :chat_id (plist-get msg :chat_id)
          :message_id (plist-get msg :id))
    callback))

(defun telega--getMessagePublicForwards (msg &optional offset limit callback)
  "Return forwarded copies of a channel message to different public channels."
  (telega-server--call
   (list :@type "getMessagePublicForwards"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id)
         :offset (or offset "")
         :limit (or limit 100))
   callback))

(defun telega--getChatSponsoredMessages (chat &optional callback)
  "Return list of sponsored messages for the CHAT."
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (unless (telega--tl-error-p reply)
        reply)

    (list :@type "getChatSponsoredMessages"
          :chat_id (plist-get chat :id))
    callback))

(defun telega--viewSponsoredMessage (chat sponsored-msg)
  "Inform that SPONSORED-MSG has been viewed in the CHAT."
  (cl-assert (eq 'sponsoredMessage (telega--tl-type sponsored-msg)))
  (telega-server--send
   (list :@type "viewMessages"
         :chat_id (plist-get chat :id)
         :message_ids (vector (plist-get sponsored-msg :message_id)))))


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

(defun telega--requestQrCodeAuthentication ()
  (telega-server--send
   (list :@type "requestQrCodeAuthentication")))

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

(defun telega--setAccountTtl (days &optional callback)
  "Change the period of account inactivity to DAYS.
After that period the account of the current user will
automatically be deleted."
  (telega-server--call
   (list :@type "setAccountTtl"
         :ttl (list :@type "accountTtl"
                    :days days))
   callback))

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
  (declare (indent 3))
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

(defun telega--setName (first-name last-name)
  "Set me name to FIRST-NAME and LAST-NAME."
  (telega-server--send
   (list :@type "setName"
         :first_name (or first-name "")
         :last_name (or last-name ""))))

(defun telega--setBio (bio)
  "Set me bio to BIO."
  (telega-server--send
   (list :@type "setBio"
         :bio (or bio ""))))

(defun telega--setUsername (username)
  "Set me username to USERNAME.
Empty string to unset username."
  (telega-server--send
   (list :@type "setUsername"
         :username (or username ""))))

(defun telega--setChatPhoto (chat filename &optional callback)
  "Changes the photo of a CHAT.
Requires `:can_change_info' rights."
  (declare (indent 2))
  (telega-server--call
   (list :@type "setChatPhoto"
         :chat_id (plist-get chat :id)
         :photo (list :@type "inputChatPhotoStatic"
                      :photo (list :@type "inputFileLocal"
                                   :path (expand-file-name filename))))
   (or callback 'ignore)))

(defun telega--setChatMessageAutoDeleteTime (chat auto-delete-seconds)
  "Change the message auto-delete time in a CHAT.
For secret chats change self-destruct time.  Requires
`:can_change_info' administrator right in basic groups, supergroups
and channels.
AUTO-DELETE-SECONDS must be from 0 up to 365 * 86400 and be divisible
by 86400."
  (cl-assert (or (telega-chat-secret-p chat)
                 (and (<= 0 auto-delete-seconds (* 365 86400))
                      (= (% auto-delete-seconds 86400) 0))))
  (telega-server--send
   (list :@type "setChatMessageAutoDeleteTime"
         :chat_id (plist-get chat :id)
         :message_auto_delete_time auto-delete-seconds)))

(defun telega--setChatPermissions (chat &rest permissions)
  "Set CHAT's permission with updated values from PERMISSIONS."
  (declare (indent 1))
  (let ((chat-perms (plist-get chat :permissions))
        (request (list :@type "chatPermissions")))
    (telega--tl-dolist ((prop-name value) (append chat-perms permissions))
      (setq request (plist-put request prop-name (or value :false))))
    (telega-server--send
     (list :@type "setChatPermissions"
           :chat_id (plist-get chat :id)
           :permissions request))))

(defun telega--setChatTheme (chat theme-name)
  "For private or secret CHAT set theme denoted by THEME-NAME."
  (telega-server--send
   (list :@type "setChatTheme"
         :chat_id (plist-get chat :id)
         :theme_name theme-name)))

(defun telega--setChatNotificationSettings (chat &rest settings)
  "Set CHAT's notification settings to SETTINGS."
  (declare (indent 1))
  (let ((not-cfg (plist-get chat :notification_settings))
        (request (list :@type "chatNotificationSettings")))
    (telega--tl-dolist ((prop-name value) (append not-cfg settings))
      (setq request (plist-put request prop-name (or value :false))))
    (telega-server--call
     (list :@type "setChatNotificationSettings"
           :chat_id (plist-get chat :id)
           :notification_settings request))))

(defun telega--setChatDraftMessage (chat &optional draft-msg)
  "Set CHAT's draft message to DRAFT-MSG.
If DRAFT-MSG is ommited, then clear draft message."
  (telega-server--send
   (nconc (list :@type "setChatDraftMessage"
                :chat_id (plist-get chat :id)
                :message_thread_id (telega-chat-message-thread-id chat))
          (when draft-msg
            (list :draft_message draft-msg)))))

(defun telega--addChatToList (chat tdlib-chat-list &optional callback)
  "Add a CHAT to a TDLIB-CHAT-LIST.
A chat can't be simultaneously in Main and Archive chat lists, so
it is automatically removed from another one if needed."
  (telega-server--call
   (list :@type "addChatToList"
         :chat_id (plist-get chat :id)
         :chat_list tdlib-chat-list)
   (or callback 'ignore)))

(defun telega--getChatFolder (folder-id)
  (telega-server--call
   (list :@type "getChatFolder"
         :chat_folder_id folder-id)))

(defun telega--createChatFolder (chat-folder &optional callback)
  "Create new CHAT-FOLDER.
Return chatFolderInfo."
  (telega-server--call
   (list :@type "createChatFolder"
         :folder chat-folder)
   callback))

(defun telega--editChatFolder (folder-id new-chat-folder &optional callback)
  (declare (indent 2))
  (telega-server--call
   (list :@type "editChatFolder"
         :chat_folder_id folder-id
         :folder new-chat-folder)
   (or callback 'ignore)))

(defun telega--deleteChatFolder (folder-id &optional leave-chats)
  (telega-server--send
   (list :@type "deleteChatFolder"
         :chat_folder_id folder-id
         :leave_chat_ids (seq-into (mapcar (telega--tl-prop :id) leave-chats)
                                   'vector))))

(defun telega--reorderChatFolders (folder-ids)
  (telega-server--send
   (list :@type "reorderChatFolders"
         :chat_folder_ids (seq-into folder-ids 'vector))))

(defun telega--getChatFolderChatsToLeave (folder-id &optional callback)
  "Return chats suggested to leave when folder with FOLDER-ID is deleted."
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat-get (plist-get reply :chat_ids))

    (list :@type "getChatFolderChatsToLeave"
          :chat_folder_id folder-id)
    callback))

(defun telega--getRecommendedChatFolders (&optional callback)
  "Return recommended chat folders for the current user."
  (with-telega-server-reply (reply)
      (append (plist-get reply :chat_folders) nil)

    (list :@type "getRecommendedChatFolders")
    callback))

(defun telega--getChatsForChatFolderInviteLink (folder-id &optional callback)
  "Return chats from folder identified by FOLDER-ID suitable for invite link."
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat-get (plist-get reply :chat_ids))

    (list :@type "getChatsForChatFolderInviteLink"
          :chat_folder_id folder-id)
    callback))

(defun telega--createChatFolderInviteLink (folder-id name chats
                                                     &optional callback)
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat-get (plist-get reply :chat_ids))

    (list :@type "createChatFolderInviteLink"
          :chat_folder_id folder-id
          :name name
          :chat_ids (seq-into (mapcar (telega--tl-prop :id) chats) 'vector))
    callback))

(defun telega--getChatFolderInviteLinks (folder-id &optional callback)
  "Return invite links created by me user for a shareable chat folder."
  (with-telega-server-reply (reply)
      reply

    (list :@type "getChatFolderInviteLinks"
          :chat_folder_id folder-id)
    callback))

(defun telega--checkChatFolderInviteLink (invite-link &optional callback)
  (with-telega-server-reply (reply)
      reply
    (list :@type "checkChatFolderInviteLink"
          :invite_link invite-link)
    callback))

(defun telega--setChatDiscussionGroup (chat discussion-chat)
  "For channel CHAT set discussion chat to DISCUSSION-CHAT.
Requires `:can_change_info' rights in the channel.
Pass nil as DISCUSSION-CHAT to unset discussion group."
  (telega-server--send
   (list :@type "setChatDiscussionGroup"
         :chat_id (plist-get chat :id)
         :discussion_chat_id (or (plist-get discussion-chat :id) 0))))

(defun telega--setChatSlowModeDelay (chat delay)
  "Changes the slow mode delay of a CHAT.
New slow mode DELAY for the chat must be one of 0, 10, 30, 60,
300, 900, 3600"
  (cl-assert (memq delay telega--slow-mode-delays))
  (telega-server--send
   (list :@type "setChatSlowModeDelay"
         :chat_id (plist-get chat :id)
         :slow_mode_delay delay)))

(defun telega--setTdlibParameters (&optional encryption-key)
  "Set the parameters for TDLib initialization."
  (telega-server--send
   (list :@type "setTdlibParameters"
         :use_test_dc (if telega-use-test-dc t :false)
         :database_directory telega-database-dir
         :files_directory telega-cache-dir
         :database_encryption_key (or encryption-key "")
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
         :enable_storage_optimizer telega-enable-storage-optimizer
         :ignore_file_names :false
         )))

(defun telega--parseTextEntities (text parse-mode)
  "Parse TEXT using PARSE-MODE.
PARSE-MODE is one of:
  (list :@type \"textParseModeMarkdown\" :version 0|1|2)
or
  (list :@type \"textParseModeHTML\")"
  (telega-server--call
   (list :@type "parseTextEntities"
         :text text
         :parse_mode parse-mode)))

(defun telega--parseMarkdown (fmt-text)
  "Parse Markdown entities in a human-friendly format, ignoring markup errors."
  (telega-server--call
   (list :@type "parseMarkdown"
         :text fmt-text)))

(defun telega--getMarkdownText (fmt-text)
  "Replace text entities with Markdown formatting in a human-friendly format.
Consider this as reverse operation to `telega--parseMarkdown'."
  (telega-server--call
   (list :@type "getMarkdownText"
         :text fmt-text)))

(defun telega--getFile (file-id &optional callback)
  (declare (indent 1))
  (telega-server--call
   (list :@type "getFile"
         :file_id file-id)
   callback))

(defun telega--getRemoteFile (remote-file-id &optional file-type callback)
  (telega-server--call
   (nconc (list :@type "getRemoteFile"
                :remote_file_id remote-file-id)
          (when file-type
            (list :file_type file-type)))
   callback))

(cl-defun telega--downloadFile (file-id &key priority offset limit sync-p
                                        callback)
  "Asynchronously downloads a file by its FILE-ID from the cloud.
`telega--on-updateFile' will be called to notify about the
download progress and successful completion of the download.
PRIORITY is integer in range 1-32 (higher downloads faster), default is 1.
CALLBACK is callback to call with single argument - file, by
default `telega-file--update' is called."
  (declare (indent 1))
  (telega-server--call
   (nconc (list :@type "downloadFile"
                :file_id file-id
                :priority (or priority 1))
          (when offset
            (list :offset offset))
          (when limit
            (list :limit limit))
          (when sync-p
            (list :synchronous (if sync-p t :false))))
   (or callback (when sync-p 'telega-file--update))))

(defun telega--cancelDownloadFile (file &optional only-if-pending callback)
  "Stop downloading the FILE.
If ONLY-IF-PENDING is non-nil then stop downloading only if it
hasn't been started, i.e. request hasn't been sent to server."
  (declare (indent 2))
  (telega-server--call
   (list :@type "cancelDownloadFile"
         :file_id (plist-get file :id)
         :only_if_pending (if only-if-pending t :false))
   (or callback #'ignore)))

(defun telega--getSuggestedFileName (file directory)
  "Return suggested name for saving a FILE in a given DIRECTORY.
TDLib 1.8.3"
  (with-telega-server-reply (reply)
      (telega-tl-str reply :text)

    (list :@type "getSuggestedFileName"
          :file_id (plist-get file :id)
          :directory directory)))

(defun telega--deleteFile (file)
  "Delete FILE from cache."
  (telega-server--send
   (list :@type "deleteFile"
         :file_id (plist-get file :id))))

(defun telega--addFileToDownloads (file msg &optional priority callback)
  "Add a FILE from a message MSG to the list of file downloads.
TDLib 1.8.2"
  (telega-server--call
   (list :@type "addFileToDownloads"
         :file_id (plist-get file :id)
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id)
         :priority (or priority 1))
   callback))

(defun telega--preliminaryUploadFile (filename &optional file-type priority)
  "Asynchronously upload file denoted by FILENAME.
FILE-TYPE is one of `photo', `animation', etc
PRIORITY is same as for `telega-file--download'."
  (telega-server--call
   (list :@type "preliminaryUploadFile"
         :file (list :@type "inputFileLocal" :path filename)
         :file_type (list :@type (format "fileType%S" (or file-type 'Unknown)))
         :priority (or priority 1))))

(defun telega--cancelPreliminaryUploadFile (file)
  "Stop uploading FILE."
  (telega-server--send
   (list :@type "cancelPreliminaryUploadFile"
         :file_id (plist-get file :id))))

(cl-defun telega--sendMessage (chat imc &optional input-reply-to
                                    options &key reply-markup callback sync-p)
  "Send the message content represented by IMC to CHAT.
If CALLBACK is specified, then call it with one argument - new
message uppon message is created."
  ;; We catch new message with `telega--on-updateNewMessage', so
  ;; ignore result returned from `sendMessage'
  (telega-server--call
   (nconc (list :@type "sendMessage"
                :chat_id (plist-get chat :id)
                :message_thread_id (telega-chat-message-thread-id chat)
                :input_message_content imc)
          (when input-reply-to
            (list :reply_to input-reply-to))
          (when options
            (list :options options))
          (when reply-markup
            (list :reply_markup reply-markup)))
   (or callback (unless sync-p #'ignore))))

(cl-defun telega--sendMessageAlbum (chat imcs &optional input-reply-to
                                         options &key callback sync-p)
  "Send IMCS as media album.
If CALLBACK is specified, then call it with one argument - new
message uppon message is created."
  (telega-server--call
   (nconc (list :@type "sendMessageAlbum"
                :chat_id (plist-get chat :id)
                :message_thread_id (telega-chat-message-thread-id chat)
                :input_message_contents (apply 'vector imcs))
          (when input-reply-to
            (list :reply_to input-reply-to))
          (when options
            (list :options options)))
   (or callback (unless sync-p #'ignore))))

(defun telega--sendBotStartMessage (bot-user chat &optional param)
  "Invite a BOT-USER to a CHAT and sends it the /start command.
Invite only if BOT-USER is not yet a member.
PARAM is additional parameter for deep linking."
  (telega-server--send
   (nconc (list :@type "sendBotStartMessage"
                :bot_user_id (plist-get bot-user :id)
                :chat_id (plist-get chat :id))
          (when param
            (list :parameter param)))
   ))

(cl-defun telega--sendInlineQueryResultMessage (chat imc &optional input-reply-to
                                                     options &key callback sync-p)
  "Send IMC as inline query result from bot.
If CALLBACK is specified, then call it with one argument - new
message uppon message is created."
  (telega-server--call
   (nconc (list :@type "sendInlineQueryResultMessage"
                :chat_id (plist-get chat :id)
                :message_thread_id (telega-chat-message-thread-id chat)
                :query_id (plist-get imc :query-id)
                :result_id (plist-get imc :result-id))
          (when input-reply-to
            (list :reply_to input-reply-to))
          (when options
            (list :options options))
          (when (plist-get imc :hide-via-bot)
            (list :hide_via_bot t)))
   (or callback (unless sync-p #'ignore))))

(cl-defun telega--forwardMessages (chat from-chat messages &optional
                                     options send-copy remove-caption
                                     &key callback sync-p)
  "Forward MESSAGES FROM-CHAT into CHAT."
  (telega-server--call
   (nconc (list :@type "forwardMessages"
                :chat_id (plist-get chat :id)
                :from_chat_id (plist-get from-chat :id)
                :message_ids (cl-map 'vector (telega--tl-prop :id) messages)
                :send_copy (if send-copy t :false)
                :remove_caption (if remove-caption t :false))
          (when options
            (list :options options)))
   (or callback (unless sync-p #'ignore))))

(cl-defun telega--editMessageText (msg imc &key reply-markup
                                       callback sync-p)
  "Edit the text of a message, or a text of a game message."
  (telega-server--call
   (nconc (list :@type "editMessageText"
                :chat_id (plist-get msg :chat_id)
                :message_id (plist-get msg :id)
                :input_message_content imc)
          (when reply-markup
            (list :reply_markup reply-markup)))
   (or callback (unless sync-p #'ignore))))

(cl-defun telega--editMessageLiveLocation (msg location
                                               &key reply-markup
                                               heading proximity-alert-radius
                                               callback sync-p)
  "Edit the message content of a live location.
Pass nil as LOCATION to stop sharing live location.
HEADING - the new direction in which the location moves, in degrees;
1-360. Pass 0 if unknown.
PROXIMITY-ALERT-RADIUS - the new maximum distance for proximity
alerts, in meters (0-100000). Pass 0 if the notification is disabled."
  (let ((content (plist-get msg :content)))
    ;; Keep heading/proximity-alert-radius values if not explicitly
    ;; specified
    (unless heading
      (setq heading
            (plist-get content :heading)))
    (unless proximity-alert-radius
      (setq proximity-alert-radius
            (plist-get content :proximity_alert_radius))))

  (telega-server--call
   (nconc (list :@type "editMessageLiveLocation"
                :chat_id (plist-get msg :chat_id)
                :message_id (plist-get msg :id)
                :location location)
          (when heading
            (list :heading heading))
          (when proximity-alert-radius
            (list :proximity_alert_radius proximity-alert-radius))
          (when reply-markup
            (list :reply_markup reply-markup)))
   (or callback (unless sync-p #'ignore))))

(cl-defun telega--editMessageMedia (msg imc &key reply-markup
                                        callback sync-p)
  "Edit the content of a message with media content.
Media content is an animation, an audio, a document, a photo or a video."
  (telega-server--call
   (nconc (list :@type "editMessageMedia"
                :chat_id (plist-get msg :chat_id)
                :message_id (plist-get msg :id)
                :input_message_content imc)
          (when reply-markup
            (list :reply_markup reply-markup)))
   (or callback (unless sync-p #'ignore))))

(cl-defun telega--editMessageCaption (msg caption &key reply-markup
                                          callback sync-p)
  "Edits the message content caption."
  (telega-server--call
   (nconc (list :@type "editMessageCaption"
                :chat_id (plist-get msg :chat_id)
                :message_id (plist-get msg :id)
                :caption caption)
          (when reply-markup
            (list :reply_markup reply-markup)))
   (or callback (unless sync-p #'ignore))))

(defun telega--editMessageSchedulingState (msg scheduling-state)
  "Edit scheduling state for the message MSG.
MSG must be previously scheduled."
  (declare (indent 1))
  (telega-server--send
   (list :@type "editMessageSchedulingState"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id)
         :scheduling_state scheduling-state)))

(defun telega--searchChatRecentLocationMessages (chat &optional callback)
  "Returns recent location messages of CHAT members that were sent to the CHAT."
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (append (plist-get reply :messages) nil)

    (list :@type "searchChatRecentLocationMessages"
          :chat_id (plist-get chat :id)
          :limit 20)
    callback))

(defun telega--getActiveLiveLocationMessages (&optional callback)
  "Return list of messages with active live locatins."
  (with-telega-server-reply (reply)
      (append (plist-get reply :messages) nil)

    (list :@type "getActiveLiveLocationMessages")
    callback))

(defun telega--deleteMessages (messages &optional revoke)
  "Delete MESSAGES.
All MESSAGES must have same `:chat_id' property.
If REVOKE is non-nil then delete MESSAGES for all users.
REVOKE is always non-nil for supergroups, channels and secret chats."
  (let ((chat-id (plist-get (car messages) :chat_id)))
    (cl-assert (cl-every (lambda (msg)
                           (eq chat-id (plist-get msg :chat_id)))
                         messages))
    (telega-server--send
     (list :@type "deleteMessages"
           :chat_id chat-id
           :message_ids (cl-map #'vector (telega--tl-prop :id) messages)
           :revoke (if revoke t :false)))))

(defun telega--deleteChatMessagesBySender (chat sender)
  "Delete all messages by SENDER in the CHAT."
  (telega-server--send
   (list :@type "deleteChatMessagesBySender"
         :chat_id (plist-get chat :id)
         :sender_id (telega--MessageSender sender))))

(cl-defun telega--searchMessages (query &key offset (limit 100)
                                         chat-list tl-msg-filter
                                        (min-date 0) (max-date 0)
                                        callback)
  "Search messages by QUERY.
OFFSET is th offset of the first entry to return as received from the
previous request.
If CHAT-LIST is given, then fetch chats from TDLib's CHAT-LIST.
If CALLBACK is specified, then do async call and run CALLBACK
with list of chats received.
Return FoundMessages TL structure."
  (declare (indent 1))
  (telega-server--call
   (nconc (list :@type "searchMessages"
                :query query
                :offset (or offset "")
                :min_date min-date
                :max-date max-date
                :limit limit)
          (when tl-msg-filter
            (list :filter tl-msg-filter))
          ;; NOTE: Uncomment when chat list is fully supported
          ;; see https://t.me/tdlibchat/42478
          (when chat-list
            ;;   (list :chat_list chat-list)
            ))
   callback))

(cl-defun telega--searchOutgoingDocumentMessages (&optional query &key limit callback)
  "Search for outgoing document messages.
Return FoundMessages TL structure."
  (declare (indent 1))
  (telega-server--call
   (list :@type "searchOutgoingDocumentMessages"
         :query (or query "")
         :limit (or limit 100))
   callback))

(defun telega--getMapThumbnailFile (loc &optional zoom width height scale
                                        chat callback)
  "Get file with the map showing LOC.
ZOOM - zoom level in [13-20], default=13
WIDTH/HEIGHT - in [16-1024]
SCALE - in [1-3]"
  (declare (indent 6))
  ;; NOTE: width and height are limited to 1024px max, otherwise TDLib
  ;; generates "Wrong width/height" error
  (when (and width (> width 1024))
    (setq width 1024))
  (when (and height (> height 1024))
    (setq height 1024))

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

(defun telega--getContacts (&optional callback)
  "Return users that are in contact list."
  (with-telega-server-reply (reply)
      (mapcar #'telega-user-get (plist-get reply :user_ids))

    (list :@type "getContacts")
    callback))

(defun telega--removeContacts (&rest user-ids)
  "Remove users determined by USER-IDS from contacts."
  (telega-server--call
   (list :@type "removeContacts"
         :user_ids (apply 'vector user-ids))))

(defun telega--searchContacts (query &optional limit callback)
  "Search contacts by QUERY.
If QUERY is empty, then return all the contacts.
LIMIT defaults to 200."
  (declare (indent 2))
  (with-telega-server-reply (reply)
      (mapcar #'telega-user-get (plist-get reply :user_ids))

    (list :@type "searchContacts"
          :query query
          :limit (or limit 200))
    callback))

(defun telega--importContacts (&rest contacts)
  "Import CONTACTS into contacts list."
  (telega-server--call
   (list :@type "importContacts"
         :contacts (apply 'vector contacts))))

(defun telega--sharePhoneNumber (user)
  "Share the phone number of the current user with a mutual contact USER."
  (telega-server--send
   (list :@type "sharePhoneNumber"
         :user_id (plist-get user :id))))

(defun telega--searchUserByPhoneNumber (phone-number &optional callback)
  "Search a user by their PHONE-NUMBER."
  (declare (indent 1))
  (telega-server--call
   (list :@type "searchUserByPhoneNumber"
         :phone_number phone-number)
   callback))

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
  (declare (indent 1))
  (unless (< (length query) 5)
    (with-telega-server-reply (reply)
        (mapcar #'telega-chat-get (plist-get reply :chat_ids))

      (list :@type "searchPublicChats"
            :query query)
      callback)))

(defun telega--searchChats (query &optional limit callback)
  "Search already known chats by QUERY."
  (declare (indent 2))
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

(defun telega--setLocation (location)
  "Changes the location of the current user.
Needs to be called if `:is_location_visible' option from
`telega--options' is non-nil."
  (telega-server--send
   (list :@type "setLocation"
         :location (cons :@type (cons "location" location)))))

(defun telega--searchChatsNearby (location &optional callback)
  "Returns a list of chats nearby LOCATION.
Distance info is stored in `telega--nearby-chats'."
  (declare (indent 1))

  (setq telega--nearby-chats nil)
  (with-telega-server-reply (reply)
      (mapcar (lambda (nbc)
                (telega-chat-get
                 (plist-get (telega-chat-nearby--ensure nbc) :chat_id)))
              (append (plist-get reply :users_nearby)
                      (plist-get reply :supergroups_nearby)
                      nil))

    (list :@type "searchChatsNearby"
          :location (cons :@type (cons "location" location)))
    callback))

(defun telega--getGroupsInCommon (with-user &optional limit callback)
  "Return list of common chats WITH-USER.
LIMIT - number of chats to get (default=100)"
  (declare (indent 2))
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
  (cl-assert (member category '("Users" "Bots" "Groups" "Channels"
                                "InlineBots" "Calls" "ForwardChats")))
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat-get (plist-get reply :chat_ids))

    (list :@type "getTopChats"
          :category (list :@type (concat "topChatCategory" category))
          :limit (or limit 30))
    callback))

(defun telega--getChat (chat-id)
  "Get chat from server by CHAT-ID."
  (cl-assert chat-id)
  (telega-server--call
   (list :@type "getChat"
         :chat_id chat-id)))

(defun telega--loadChats (chat-list &optional callback)
  "Load more chats from a CHAT-LIST.
Return error if all chats are loaded."
  (declare (indent 1))
  (telega-server--call
   (list :@type "loadChats"
         :chat_list chat-list
         :limit 1000)
   (or callback 'ignore)))

(defun telega--getChats (chat-list &optional callback)
  "Retreive all chats from the server in async manner.
OFFSET-CHAT is the chat to start getting chats from."
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat--ensure
              (mapcar #'telega-chat-get (plist-get reply :chat_ids)))

    (list :@type "getChats"
          :chat_list chat-list
          :limit 1000)
    callback))

(defun telega--reportChat (chat reason &optional messages text)
  "Report a CHAT to the Telegram moderators.
REASON is one of: \"Spam\", \"Violence\", \"Pornography\",
\"ChildAbuse\", \"Copyright\", \"UnrelatedLocation\",
\"Fake\" or \"Custom\"."
  (telega-server--send
   (list :@type "reportChat"
         :chat_id (plist-get chat :id)
         :reason (list :@type (concat "chatReportReason" reason))
         :message_ids (cl-map 'vector (telega--tl-prop :id) messages)
         :text (or text ""))))

(defun telega--reportChatPhoto (chat photo-file reason &optional text)
  (telega-server--send
   (list :@type "reportChatPhoto"
         :chat_id (plist-get chat :id)
         :file_id (plist-get photo-file :id)
         :reason (list :@type (concat "chatReportReason" reason))
         :text (or text ""))))

(defun telega--removeChatActionBar (chat)
  "Remove CHAT's action bar without any other action."
  (telega-server--send
   (list :@type "removeChatActionBar"
         :chat_id (plist-get chat :id))))

(defun telega--createPrivateChat (user &optional callback)
  "Create private chat with USER.
Return newly created chat."
  (with-telega-server-reply (reply)
      (telega-chat-get (plist-get reply :id))

    (list :@type "createPrivateChat"
           :user_id (plist-get user :id))
    callback))

(defun telega--openMessageContent (msg)
  "Open content of the message MSG."
  (telega-server--send
   (list :@type "openMessageContent"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id))))

(defun telega--clickAnimatedEmojiMessage (msg &optional callback)
  "Animated emoji message has been clicked.
Return non-nil if animated sticker need to be played.
TDLib 1.7.8"
  (declare (indent 1))
  (telega-server--call
   (list :@type "clickAnimatedEmojiMessage"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id))
   callback))

(defun telega--getInternalLink (tdlib-link-type &optional internal-p)
  "Return http or tg link to the TDLIB-LINK-TYPE."
  (with-telega-server-reply (reply)
      (telega-tl-str reply :url)

    (list :@type "getInternalLink"
          :type tdlib-link-type
          :is_http (if internal-p :false t))))

(defun telega--getInternalLinkType (link-url &optional callback)
  "Return information about the type of an internal link.
Return nil if LINK-URL is not Telegram's internal link.
TDLib 1.7.8"
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (unless (telega--tl-error-p reply)
        reply)

    (list :@type "getInternalLinkType"
          :link link-url)
    callback))

(defun telega--getExternalLinkInfo (link-url &optional callback)
  ;; --> LoginUrlInfo
  "Return action to be done when the current user clicks an external link."
  ;; NOTE from TDLib docs: Don't use this method for links from secret
  ;; chats if web page preview is disabled in secret chats
  (telega-server--call
   (list :@type "getExternalLinkInfo"
         :link link-url)
   callback))

(defun telega--getMessageLinkInfo (msg-url &optional callback)
  "Return information about a public or private message link."
  (telega-server--call
   (list :@type "getMessageLinkInfo"
         :url msg-url)
   callback))

(cl-defun telega--viewMessages (chat messages &key source force)
  "Mark CHAT's MESSAGES as read.
Use non-nil value for FORCE, if messages in closed chats should
be marked as read."
  (declare (indent 2))
  (cl-assert chat)
  ;; NOTE: we mark messages with internal `:telega-viewed-in-thread', so
  ;; "viewMessages" will be called once
  (let* ((thread-id (telega-chat-message-thread-id chat))
         (non-viewed-messages
          (cl-remove-if
           (lambda (msg)
             (eq thread-id (plist-get msg :telega-viewed-in-thread)))
           messages)))
    ;; Mark messages with internal `:telega-viewed-in-thread' prop
    (when force
      (dolist (msg non-viewed-messages)
        (plist-put msg :telega-viewed-in-thread thread-id)))

    (when non-viewed-messages
      (telega-server--send
       (list :@type "viewMessages"
             :chat_id (plist-get chat :id)
             :message_ids (cl-map #'vector (telega--tl-prop :id)
                                  non-viewed-messages)
             :source (or source '(:@type "messageSourceOther"))
             :force_read (if force t :false))))))

(defun telega--toggleChatIsPinned (chat)
  "Toggle pin state of the CHAT in the `telega-tdlib--chat-list'."
  (telega-server--send
   (list :@type "toggleChatIsPinned"
         :chat_list telega-tdlib--chat-list
         :chat_id (plist-get chat :id)
         :is_pinned (if (plist-get (telega-chat-position chat) :is_pinned)
                        :false
                      t))))

(defun telega--readChatList (tdlib-chat-list)
  "Mark all chats in the TDLIB-CHAT-LIST as read."
  (telega-server--send
   (list :@type "readChatList"
         :chat_list tdlib-chat-list)))

(defun telega--toggleChatHasProtectedContent (chat)
  "Toogle ability of users to save, forward, or copy CHAT content."
  (telega-server--send
   (list :@type "toggleChatHasProtectedContent"
         :chat_id (plist-get chat :id)
         :has_protected_content
         (if (plist-get chat :has_protected_content) :false t))))

(defun telega--toggleChatIsMarkedAsUnread (chat)
  "Toggle marked as read state of the CHAT."
  (telega-server--send
   (list :@type "toggleChatIsMarkedAsUnread"
         :chat_id (plist-get chat :id)
         :is_marked_as_unread
         (if (plist-get chat :is_marked_as_unread) :false t))))

(defun telega--readAllChatMentions (chat)
  "Read all mentions in CHAT."
  (telega-server--send
   (list :@type "readAllChatMentions" :chat_id (plist-get chat :id))))

(defun telega--readAllChatReactions (chat)
  "Read all reactions in CHAT."
  (telega-server--send
   (list :@type "readAllChatReactions" :chat_id (plist-get chat :id))))

(defun telega--openChat (chat)
  "Mark CHAT as opened."
  (telega-server--send
   (list :@type "openChat"
         :chat_id (plist-get chat :id))))

(defun telega--closeChat (chat)
  "Mark CHAT as closed."
  (telega-server--send
   (list :@type "closeChat"
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


(defun telega--getPollVoters (msg option-id &optional offset limit callback)
  "Return users voted for the specified OPTION-ID in a non-anonymous poll."
  (telega-server--call
   (list :@type "getPollVoters"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id)
         :option_id option-id
         :offset (or offset 0)
         :limit (or limit 50))
   callback))

(defun telega--stopPoll (msg)
  "Stops a poll."
  (telega-server--send
   (list :@type "stopPoll"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id))))

(defun telega--setPollAnswer (msg &rest option-ids)
  "Changes user answer to a poll.
OPTION-IDS - 0-based identifiers of option, chosen by the user.
If OPTION-IDS is not specified, then retract the voice."
  (telega-server--send
   (list :@type "setPollAnswer"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id)
         :option_ids (apply 'vector option-ids))))

(defun telega--getLoginUrlInfo (msg kbd-type &optional callback)
  (telega-server--call
   (list :@type "getLoginUrlInfo"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id)
         :button_id (plist-get kbd-type :id))
   callback))

(defun telega--getLoginUrl (msg kbd-type allow-write-access &optional callback)
  (telega-server--call
   (list :@type "getLoginUrl"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id)
         :button_id (plist-get kbd-type :id)
         :allow_write_access (if allow-write-access t :false))
   callback))

(defun telega--createCall (user)
  "Create outgoing call to the USER."
  (telega-server--call
   (list :@type "createCall"
         :user_id (plist-get user :id)
         :protocol telega-voip-protocol)))

(defun telega--acceptCall (call-id)
  "Accept incomming call, defined by CALL-ID."
  (telega-server--call
   (list :@type "acceptCall"
         :call_id call-id
         :protocol telega-voip-protocol)))

(defun telega--discardCall (call-id &optional disconnected-p
                                    duration connection-id)
  "Discard call defined by CALL-ID."
  (telega-server--send
   (nconc (list :@type "discardCall"
                :call_id call-id)
          (when disconnected-p
            (list :is_disconnected (or disconnected-p :false)))
          (when duration
            (list :duration duration))
          (when connection-id
            :connection_id connection-id))))

(cl-defun telega--searchCallMessages (&key offset (limit 100)
                                           only-missed-p callback)
  "Search for call messages.
Return FoundMessages TL structure."
  (telega-server--call
   (list :@type "searchCallMessages"
         :offset (or offset "")
         :limit limit
         :only_missed (if only-missed-p t :false))
   callback))

;; Group Calls section
(defun telega--getGroupCall (group-call-id &optional callback)
  (declare (indent 1))
  (telega-server--call
   (list :@type "getGroupCall"
         :group_call_id group-call-id)
   callback))

(defun telega--loadGroupCallParticipants (group-call &optional limit)
  ;; TDLib: The group call must be previously received through
  ;; getGroupCall and must be joined or being joined
  (telega-server--send
   (list :@type "loadGroupCallParticipants"
         :group_call_id (plist-get group-call :id)
         :limit (or limit 100))))

(cl-defun telega--createVideoChat (chat title &key (start-time 0)
                                        rtmp-stream-p callback)
  "Create a voice chat (a group call bound to a chat).
Available only for basic groups and supergroups.
Return an ID of group call."
  (declare (indent 2))
  (with-telega-server-reply (reply)
      (plist-get reply :id)

    (list :@type "createVideoChat"
          :chat_id (plist-get chat :id)
          :title title
          :start_date start-time
          :is_rtmp_stream (if rtmp-stream-p t :false))
    callback))

(defun telega--setGroupCallTitle (group-call title)
  "Set GROUP-CALL TITLE."
  (cl-assert (plist-get group-call :can_be_managed))
  (telega-server--send
   (list :@type "setGroupCallTitle"
         :group_call_id (plist-get group-call :id)
         :title title)))

(defun telega--leaveGroupCall (group-call)
  "Leave a GROUP-CALL."
  (telega-server--send
   (list :@type "leaveGroupCall"
         :group_call_id (plist-get group-call :id)))
  )

(defun telega--endGroupCall (group-call)
  "End a GROUP-CALL."
  (telega-server--send
   (list :@type "endGroupCall"
         :group_call_id (plist-get group-call :id)))
  )

(defun telega--getGroupCallInviteLink (group-call &optional can-self-unmute-p
                                                  callback)
  (telega-server--call
   (list :@type "getGroupCallInviteLink"
         :group_call_id (plist-get group-call :id)
         :can_self_unmute (if can-self-unmute-p t :false))
   callback))

(defun telega--startGroupCallRecording (group-call title)
  "Starts recording of a GROUP-CALL."
  (cl-assert (plist-get group-call :can_be_managed))
  (telega-server--call
   (list :@type "startGroupCallRecording"
         :group_call_id (plist-get group-call :id)
         :title title)))

(defun telega--endGroupCallRecording (group-call)
  "Ends recording of a GROUP-CALL."
  (cl-assert (plist-get group-call :can_be_managed))
  (telega-server--call
   (list :@type "endGroupCallRecording"
         :group_call_id (plist-get group-call :id))))

(defun telega--getVideoChatAvailableParticipants (chat &optional callback)
  "Get list of message senders, which can be used as voice chat alias.
CHAT is ordinary Telegram chat."
  (declare (indent 1))
  (with-telega-server-reply (reply)
      (mapcar #'telega-msg-sender (plist-get reply :senders))

    (list :@type "getVideoChatAvailableParticipants"
          :chat_id (plist-get chat :id))
    callback))

(defun telega--startScheduledGroupCall (group-call)
  "Starts a scheduled GROUP-CALL."
  (telega-server--send
   (list :@type "startScheduledGroupCall"
         :group_call_id (plist-get group-call :id))))

(defun telega--toggleGroupCallEnabledStartNotification (group-call)
  (telega-server--send
   (list :@type "startScheduledGroupCall"
         :group_call_id (plist-get group-call :id)
         :enabled_start_notification
         (if (plist-get group-call :enabled_start_notification) :false t))))

;;; Reactions
(cl-defun telega--getMessageAvailableReactions (msg &optional row-size callback)
  "Return reactions, which can be added to the message MSG.
ROW-SIZE - Number of reaction per row, 5-25."
  (declare (indent 1))
  (with-telega-server-reply (reply)
      reply
      ;; (let ((me (telega-user-me)))
      ;;   (delq nil (mapcar (lambda (reaction)
      ;;                       (when (or (not (plist-get reaction :needs_premium))
      ;;                                 (telega-user-match-p me 'is-premium))
      ;;                         (telega-tl-str reaction :reaction)))
      ;;                     (plist-get reply :reactions))))

    (nconc (list :@type "getMessageAvailableReactions"
                 :chat_id (plist-get msg :chat_id)
                 :message_id (plist-get msg :id))
           (when row-size
             (list :row_size row-size)))
    callback))

(cl-defun telega--getMessageAddedReactions
    (msg &key tl-reaction-type offset limit callback)
  "Return reactions added for a message MSG, along with their sender."
  (unless (plist-get msg :can_get_added_reactions)
    (error "Can't get added reactions for the message"))

  (telega-server--call
   (list :@type "getMessageAddedReactions"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id)
         :reaction tl-reaction-type
         :offset (or offset "")
         :limit (or limit 100))
   callback))

(defun telega--ReactionType (reaction-type)
  "Make sure REACTION-TYPE is suitable to be passed to TDLib method."
  ;; NOTE: emoji needs to be desurrogated
  ;; before passing to any TDLib method
  (if (eq (telega--tl-type reaction-type) 'reactionTypeEmoji)
      (list :@type "reactionTypeEmoji"
            :emoji (telega-tl-str reaction-type :emoji))
    reaction-type))

(defun telega--addMessageReaction (msg tl-reaction-type &optional
                                       big-p update-recent-reactions-p)
  "Add a reaction to a message.
REACTION-TYPE is a reaction to add.
BIG-P is non-nil if the reaction is added with a big animation.
Pass non-nil UPDATE-RECENT-REACTIONS-P to update recent reactions."
  (telega-server--send
   (list :@type "addMessageReaction"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id)
         :reaction_type (telega--ReactionType tl-reaction-type)
         :is_big (if big-p t :false)
         :update_recent_reactions (if update-recent-reactions-p t :false))))

(defun telega--removeMessageReaction (msg tl-reaction-type)
  "Remove a reaction from a message."
  (telega-server--send
   (list :@type "removeMessageReaction"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id)
         :reaction_type (telega--ReactionType tl-reaction-type))))

(defun telega--setChatAvailableReactions (chat reactions)
  "Change REACTIONS, available in a CHAT."
  (telega-server--send
   (list :@type "setChatAvailableReactions"
         :chat_id (plist-get chat :id)
         :available_reactions (apply 'vector reactions))))

(defun telega--acceptTermsOfService (tos-id &optional callback)
  "Accepts Telegram terms of services."
  (declare (indent 1))
  (telega-server--call
   (list :@type "acceptTermsOfService"
         :terms_of_service_id tos-id)
   callback))

;; Emoji status from TDLib 1.8.6

;; NOTE: Since TDLib 1.8.15 API changed to return `:custom_emoji_ids'
;; instead of `:emoji_statuses', so we wrap them into emoji status
;; alike structure
(defun telega--custom-emoji-id-wrap-into-emoji-status (custom-emoji-id)
  (list :custom_emoji_id custom-emoji-id))

(defun telega--getThemedEmojiStatuses (&optional callback)
  "Returns up to 8 themed emoji statuses."
  (with-telega-server-reply (reply)
      (mapcar #'telega--custom-emoji-id-wrap-into-emoji-status
              (plist-get reply :custom_emoji_ids))
   (list :@type "getThemedEmojiStatuses")
   callback))

(defun telega--getRecentEmojiStatuses (&optional callback)
  "Return recent emoji statuses."
  (with-telega-server-reply (reply)
      (mapcar #'telega--custom-emoji-id-wrap-into-emoji-status
              (plist-get reply :custom_emoji_ids))
   (list :@type "getRecentEmojiStatuses")
   callback))

(defun telega--getDefaultEmojiStatuses (&optional callback)
  "Return default emoji statuses"
  (with-telega-server-reply (reply)
      (mapcar #'telega--custom-emoji-id-wrap-into-emoji-status
              (plist-get reply :custom_emoji_ids))
   (list :@type "getDefaultEmojiStatuses")
   callback))

(defun telega--clearRecentEmojiStatuses ()
  "Clear the list of recently used emoji statuses."
  (telega-server--send
   (list :@type "clearRecentEmojiStatuses")))

(defun telega--setEmojiStatus (custom-emoji-id &optional duration)
  "Change the emoji status for me.
DURATION of the status, in seconds.
For Telegram Premium users only."
  (telega-server--send
   (list :@type "setEmojiStatus"
         :emoji_status
         (when custom-emoji-id
           (list :@type "emojiStatus"
                 :custom_emoji_id custom-emoji-id
                 :expiration_date (if duration
                                      (+ (telega-time-seconds) duration)
                                    0))))))

;; 2step verification
(defun telega--getPasswordState (&optional callback)
  "Return the current state of 2-step verification."
  (telega-server--call
   (list :@type "getPasswordState")
   callback))

(defun telega--setPassword (old-password new-password new-hint
                                         &optional new-recovery-email
                                         callback)
  "Change the 2-step verification password for me."
  (telega-server--call
   (list :@type "setPassword"
         :old_password old-password
         :new_password new-password
         :new_hint (or new-hint "")
         :set_recovery_email_address (if new-recovery-email t :false)
         :new_recovery_email_address new-recovery-email)
   callback))

(cl-defun telega--translateText (text to-language-code &key callback)
  "Translate TEXT to a language specified by TO-LANGUAGE-CODE.
TO-LANGUAGE-CODE is a two-letter ISO 639-1 language code. "
  (declare (indent 2))
  (telega-server--call
   (list :@type "translateText"
         ;; NOTE: Accepts "formattedText" as argument
         :text (if (stringp text)
                   (telega-fmt-text text)
                 text)
         :to_language_code to-language-code)
   callback))

(defun telega--setAlarm (seconds &optional callback)
  "Succeeds after a specified amount of time has passed."
  (declare (indent 1))
  (telega-server--call
   (list :@type "setAlarm"
         :seconds seconds)
   callback))

(defun telega--setNetworkType (tdlib-network-type &optional callback)
  (telega-server--call
   (list :@type "setNetworkType"
         :type tdlib-network-type)
   (or callback 'ignore)))

;; Autodownload
(defun telega--getAutoDownloadSettingsPresets (&optional callback)
  "Return auto-download settings presets for me."
  (telega-server--call
   (list :@type "getAutoDownloadSettingsPresets")
   callback))

(defun telega--setAutoDownloadSettings (settings &optional tdlib-network-type)
  "Set auto-download SETTINGS for the TDLIB-NETWORK-TYPE."
  (telega-server--send
   (nconc (list :@type "setAutoDownloadSettings"
                :settings settings)
          (when tdlib-network-type
            (list :type tdlib-network-type)))
   ))


;;; Topics
(defun telegea--toggleSupergroupIsForum (supergroup forum-p)
  "Toggle whether the supergroup is a forum.
Requires owner privileges."
  (telega-server--send
   (list :@type "toggleSupergroupIsForum"
         :supergroup_id (plist-get supergroup :id)
         :is_forum (if forum-p t :false))))

(defun telega--readAllMessageThreadMentions (chat msg-thread-id)
  )

(defun telega--readAllMessageThreadReactions (chat msg-thread-id)
  )

(defun telega--unpinAllMessageThreadMessages (chat msg-thread-id)
  )

(defun telega--createForumTopic (chat name &optional icon)
  )

(cl-defun telega--editForumTopic (chat msg-thread-id &key name icon)
  )

(defun telega--toggleForumTopicIsClosed (chat msg-thread-id closed-p)
  )

(defun telega--deleteForumTopic (chat msg-thread-id)
  )

(defun telega--getForumTopicDefaultIcons (&optional callback)
  "Returns list of custom emojis, which can be used as forum topic icon."
  (with-telega-server-reply (reply)
      (mapcar #'telega-custom-emoji--ensure
              (plist-get reply :stickers))
    (list :@type "getForumTopicDefaultIcons")
    callback))

(defun telega--getForumTopic (chat message-thread-id &optional callback)
  "Return information about a forum topic."
  (declare (indent 2))
  (telega-server--call
   (list :@type "getForumTopic"
         :chat_id (plist-get chat :id)
         :message_thread_id message-thread-id)
   callback))

(defun telega--getForumTopicLink (chat message-thread-id &optional callback)
  "Return an HTTPS link to a topic in a forum chat."
  (declare (indent 2))
  (telega-server--call
   (list :@type "getForumTopicLink"
         :chat_id (plist-get chat :id)
         :message_thread_id message-thread-id)
   callback))

(cl-defun telega--getForumTopics (chat query &key offset-date offset-message-id
                                       offset-message-thread-id limit callback)
  "Return found forum topics in a forum chat."
  (declare (indent 2))
  (telega-server--call
   (list :@type "getForumTopics"
         :chat_id (plist-get chat :id)
         :query query
         :offset-date (or offset-date 0)
         :offset-message-id (or offset-message-id 0)
         :offset-message-thread-id (or offset-message-thread-id 0)
         :limit (or limit 100))
   callback))

(defun telega--searchRecentlyFoundChats (query &optional limit callback)
  "Searches for the specified query in the title of recently found chats."
  (declare (indent 2))
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat-get (plist-get reply :chat_ids))

    (list :@type "searchRecentlyFoundChats"
          :query query
          :limit (or limit 50))
    callback))

(defun telega--addRecentlyFoundChat (chat)
  "Add a CHAT to the list of recently found chats."
  (telega-server--send
   (list :@type "addRecentlyFoundChat"
         :chat_id (plist-get chat :id))))

(defun telega--removeRecentlyFoundChat (chat)
  "Remove a CHAT from the list of recently found chats."
  (telega-server--send
   (list :@type "removeRecentlyFoundChat"
         :chat_id (plist-get chat :id))))

(defun telega--clearRecentlyFoundChats ()
  (telega-server--send
   '(:@type "clearRecentlyFoundChats")))

(defun telega--getRecentlyOpenedChats (&optional limit callback)
  (declare (indent 1))
  (telega-server--call
   (list :@type "getRecentlyOpenedChats"
         :limit (or limit 50))
   callback))

;;; Stories
(defun telega--getStory (chat-id story-id &optional only-local-p callback)
  "Returns a story denoted by CHAT-ID and STORY-ID."
  (declare (indent 3))
  (with-telega-server-reply (reply)
      (progn
        (when (telega--tl-error-p reply)
          (plist-put reply :sender_chat_id chat-id)
          (plist-put reply :id story-id)
          (setf (telega-story-deleted-p reply) t))
        (telega-story--ensure reply 'no-root-update))

    (list :@type "getStory"
          :story_sender_chat_id chat-id
          :story_id story-id
          :only_local (if only-local-p t :false))
    callback))

(defun telega--setStoryPrivacySettings (my-story story-privacy-settings)
  "Change privacy settings of a previously sent MY-STORY."
  (telega-server--send
   (list :@type "setStoryPrivacySettings"
         :story_id (plist-get my-story :id)
         :privacy_settings story-privacy-settings)))

(defun telega--toggleStoryIsPinned (my-story &optional pinned-p)
  "Toggle whether a MY-STORY is accessible after expiration."
  (telega-server--send
   (list :@type "toggleStoryIsPinned"
         :story_id (plist-get my-story :id)
         :is_pinned (if pinned-p t :false))))

(defun telega--deleteStory (story)
  "Delete a previously sent STORY."
  (telega-server--send
   (list :@type "deleteStory"
         :story_id (plist-get story :id))))

(defun telega--getStoryNotificationSettingsExceptions (&optional callback)
  "Return list of chats with non-default notification settings for stories."
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat-get (plist-get reply :chat_ids))

    '(:@type "getStoryNotificationSettingsExceptions")
    callback))

(defun telega--loadActiveStories (story-list)
  "Loads more active stories from a story list.
The loaded stories will be sent through updates."
  (telega-server--send
   (list :@type "loadActiveStories"
         :story_list story-list)))

(defun telega--getChatActiveStories (chat &optional callback)
  "Return the list of active stories posted by the given chat."
  (declare (indent 1))
  (telega-server--call
   (list :@type "getChatActiveStories"
         :chat_id (plist-get chat :id))
   callback))

(defun telega--getChatPinnedStories (chat &optional from-story-id limit callback)
  (declare (indent 3))
  (telega-server--call
   (list :@type "getChatPinnedStories"
         :chat_id (plist-get chat :id)
         :from_story_id (or from-story-id 0)
         :limit (or limit 100))
   callback))

(defun telega--openStory (story)
  (telega-server--call
   (list :@type "openStory"
         :story_sender_chat_id (plist-get story :sender_chat_id)
         :story_id (plist-get story :id))))

(defun telega--closeStory (story)
  (telega-server--call
   (list :@type "closeStory"
         :story_sender_chat_id (plist-get story :sender_chat_id)
         :story_id (plist-get story :id))))

(defun telega--activateStoryStealthMode ()
  "Activate stealth mode.
Mode activates for
`:story_stealth_mode_past_period' (`telega--options') seconds."
  (telega-server--send
   '(:@type "activateStoryStealthMode")))

(defun telega--getChatsToSendStories (&optional callback)
  (with-telega-server-reply (reply)
      (mapcar #'telega-chat-get (plist-get reply :chat_ids))

    '(:@type "getChatsToSendStories")
    callback))

(defun telega--canSendStory (chat &optional callback)
  (telega-server--call
   (list :@type "canSendStory"
         :chat_id (plist-get chat :id))
   callback))

;;; Boosts
(defun telega--getChatBoostStatus (chat &optional callback)
  (telega-server--call
   (list :@type "getChatBoostStatus"
         :chat_id (plist-get chat :id))
   callback))

(defun telega--canBoostChat (chat &optional callback)
  (telega-server--call
   (list :@type "canBoostChat"
         :chat_id (plist-get chat :id))
   callback))

(defun telega--boostChat (chat)
  (telega-server--send
   (list :@type "boostChat"
         :chat_id (plist-get chat :id))))

(defun telega--getChatBoostLinkInfo (url &optional callback)
  (telega-server--call
   (list :@type "getChatBoostLinkInfo"
         :url url)
   callback))

(cl-defun telega--getChatBoosts (chat &key offset limit callback)
  (declare (indent 1))
  (telega-server--call
   (list :@type "getChatBoosts"
         :chat_id (plist-get chat :id)
         :offset (or offset "")
         :limit (or limit 100))
   callback))

(defun telega--getPremiumGiveawayInfo (msg &optional callback)
  "Get information about a Telegram Premium giveaway."
  (declare (indent 1))
  (telega-server--call
   (list :@type "getPremiumGiveawayInfo"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id))
   callback))

(provide 'telega-tdlib)

;;; telega-tdlib.el ends here

;;; telega-tme.el --- Handling internal telegram links  -*- lexical-binding:t -*-

;; Copyright (C) 2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sat Jan 19 16:36:01 2019
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
(require 'cl-lib)
(require 'rx)
(require 'url-parse)
(require 'url-util)

(require 'telega-tdlib)
(require 'telega-i18n)
(require 'telega-sticker)
(require 'telega-util)
(require 'telega-transient)

;; telega-chat.el depends on telega-tme.el
(declare-function telega-chat-get "telega-chat" (chat-id &optional offline-p))
(declare-function telega-chat-by-username "telega-chat" (username))
(declare-function telega-chat--goto-msg "telega-chat" (chat msg-id &optional highlight callback))
(declare-function telega-chat--pop-to-buffer "telega-chat" (chat &optional no-history-load))
(declare-function telega-chatbuf--prompt-update "telega-chat")

(declare-function telega-webpage--instant-view "telega-webpage" (url &optional sitename instant-view))
(declare-function telega-browse-url "telega-webpage" (url &optional in-web-browser))


(defun telega-tme--media-timestamp-callback (media-timestamp-str)
  "Generate callback to open media message at MEDIA-TIMESTAMP."
  (when media-timestamp-str
    (lambda ()
      (let ((msg (telega-msg-at (point))))
        (when (telega-msg-match-p msg '(type VideoNote VoiceNote Audio Video))
          (let ((telega-ffplay-media-timestamp
                 (string-to-number media-timestamp-str)))
            (telega-msg-open-content msg)))))))

(defun telega-tme-internal-link-to (chat-or-msg &rest params)
  "Return internal tme link to CHAT-OR-MSG.
Return nil if link can't be created.
PARAMS is a plist of additional parameters to the returned link."
  (let ((chat (if (telega-chat-p chat-or-msg)
                  chat-or-msg
                (telega-chat-get (plist-get chat-or-msg :chat_id) 'offline))))
    (concat "telega:"
            (if-let ((chat-username (telega-chat-username chat)))
                (concat "@" chat-username)
              (number-to-string (plist-get chat :id)))
            (when params
              (concat "?" (telega-tme-build-query-string params)))
            (when-let ((msg (unless (telega-chat-p chat-or-msg) chat-or-msg)))
              (concat "#" (number-to-string
                           (/ (plist-get msg :id) telega-msg-id-step))))
            )))

(defun telega-tme-internal-link-open (telega-link-url)
  "Open internal telega link to any chat or message."
  ;; Internal links to any chat or message
  ;; See: https://github.com/zevlg/telega.el/issues/139
  ;; Internal links in form telega:<CHAT-SPEC>[?<PARAMS>][#<POST-ID>]
  ;;   CHAT-SPEC =  @USERNAME | CHAT-ID
  ;;   PARAMS = query params such as "open_content=t"
  ;;   POST-ID = MSG-ID div 1048576
  (let* ((urlobj (url-generic-parse-url telega-link-url))
         (path-query (url-path-and-query urlobj))
         (chat-spec (car path-query))
         (post-spec (url-target urlobj))
         (params (telega-tme-parse-query-string (cdr path-query)))
         (chat (or (unless (equal "telega" (url-type urlobj))
                     (error "telega: non-internal link: %s" telega-link))
                   (if (string-prefix-p "@" chat-spec)
                       (telega-chat-by-username (substring chat-spec 1))
                     (telega-chat-get (string-to-number chat-spec) 'offline))
                   (user-error "telega: No chat with CHAT-SPEC=%S" chat-spec)))
         (post-id (when post-spec (string-to-number post-spec)))
         ;; NOTE: For backward compatibility check if POST-ID is
         ;; actually a MSG-ID
         (msg-id (when post-id
                   (if (zerop (% post-id telega-msg-id-step))
                       post-id
                     (* post-id telega-msg-id-step)))))
    (cond ((plist-get params :open_content)
           (cl-assert msg-id)
           (telega-msg-get chat msg-id
             (lambda (message &optional _offline-p)
               (telega-msg-open-content message))))
          (t
           (telega-chat--pop-to-buffer chat)
           (when msg-id
             (telega-chat--goto-msg chat msg-id 'highlight))))))

(defun telega-tme--post-msg-id (post)
  "Convert POST number to the message id."
  ;; See https://github.com/tdlib/td/issues/16
  ;; msg-id = post * 1048576
  (* (string-to-number post) telega-msg-id-step))

;; TODO: move code to `telega-tme-open-tdlib-link' and remove this function
(defun telega-tme-open-privatepost (supergroup post &optional media-timestamp)
  "Open POST in private SUPERGROUP."
  (let ((chat (telega-chat-get (string-to-number (concat "-100" supergroup))
                               'offline)))
    (unless chat
      (error "telega: %s" (telega-i18n "lng_error_post_link_invalid")))
    (telega-chat--goto-msg chat (telega-tme--post-msg-id post) 'highlight
      (telega-tme--media-timestamp-callback media-timestamp))))

;; TODO: move code to `telega-tme-open-tdlib-link' and remove this function
(defun telega-tme-open-username (username &rest params)
  "Open chat by its USERNAME.
PARAMS are additional params."
  (cond ((string= username "telegrampassport")
         ;; TODO: passport
         (message "telega TODO: handle `telegrampassport'"))

        ;; See https://core.telegram.org/bots#deep-linking for
        ;; `:start' and `:startgroup' meaning
        ((plist-get params :startgroup)
         (let ((bot-user (telega-chat-user (telega--searchPublicChat username)))
               (chat (telega-completing-read-chat
                      ;; TODO: i18n
                      (format "Start «%s» in group: "
                              (propertize (concat "@" username) 'face 'bold))
                      (telega-filter-chats (telega-chats-list)
                        '(my-permission :can_invite_users)))))
           (telega-chat--pop-to-buffer chat)
           (telega--sendBotStartMessage
            bot-user chat (plist-get params :startgroup))))

        ((plist-get params :start)
         (let* ((bot-chat (telega--searchPublicChat username))
                (bot-user (telega-chat-user bot-chat)))
           (telega-chat--pop-to-buffer bot-chat)
           (telega--sendBotStartMessage
            bot-user bot-chat (plist-get params :start))))

        (t
         ;; Ordinary user/channel/group, :post
         (let ((chat (telega--searchPublicChat username))
               (post (plist-get params :post))
               (comment (plist-get params :comment))
               (thread (plist-get params :thread))
               (media-timestamp (plist-get params :t)))
           (unless chat
             (error "Unknown public chat: %s" username))

           (cond ((and post thread)
                  (telega-chat--goto-thread
                   chat (telega-tme--post-msg-id thread)
                   (telega-tme--post-msg-id post)))
                 ((and post comment)
                  ;; comment with post is the same as post with thread
                  (telega-chat--goto-thread
                   chat (telega-tme--post-msg-id post)
                   (telega-tme--post-msg-id comment)))
                 (post
                  (telega-chat--goto-msg chat
                      (telega-tme--post-msg-id post) 'highlight
                    (telega-tme--media-timestamp-callback media-timestamp)))
                 (t
                  (telega-chat--pop-to-buffer chat)))))
        ))

(defun telega-tme-open-invite-link (url)
  "Join the GROUP by invitation link specified by URL."
  (let* ((invite-link (let ((tl-obj (telega--checkChatInviteLink url)))
                        (when (telega--tl-error-p tl-obj)
                          (error "telega: %s" (telega-tl-str tl-obj :error)))
                        tl-obj))
         (chat-id (plist-get invite-link :chat_id))
         (chat (when invite-link
                 (if (zerop chat-id)
                     ;; Can only join (or request to join) by invite link
                     (transient-setup 'telega-transient-invite-link-join
                                      nil nil :scope
                                      (list :url url
                                            :invite-link-info invite-link))

                   ;; Can preview messages before deciding to join
                   (telega-chat-get chat-id)))))
    (when chat
      (telega-chat--pop-to-buffer chat))))

(defun telega-tme-open-stickerset (setname)
  "Open sticker set with SETNAME."
  (message "telega: Searching for %S stickerset.." setname)
  (telega--searchStickerSet setname #'telega-describe-stickerset))

(defun telega-tme-open-theme (_slug)
  (user-error "`telega-tme-open-theme' not yet implemented"))

(defun telega-tme-open-lang (lang)
  "Open setlanguage tg link to change language to LANG."
  (when (equal lang telega-language)
    (user-error (concat "Language is already " lang)))

  (let ((lang-pack (telega--getLanguagePackInfo lang)))
    (when (yes-or-no-p (concat "Telega: change UI language to "
                               (plist-get lang-pack :native_name)
                               "? "))
      (setq telega-language lang)
      (telega--setOption :language_pack_id lang 'sync)
      (telega-i18n-init))))

(defun telega-tme-parse-query-string (query-string)
  "Parse QUERY-STRING and return it as plist.
Multiple params with same name in QUERY-STRING is disallowed."
  (let ((query (ignore-errors
                 (url-parse-query-string query-string 'downcase))))
    (cl-loop for (name val) in query
             nconc (list (intern (concat ":" name)) val))))

(defun telega-tme-build-query-string (query-params &optional
                                                   semicolons keep-empty)
  "Build a query string for the QUERY-PARAMS.
QUERY-PARAMS should be in form returned from `telega-tme-parse-query-string'.
SEMICOLONS and KEEP-EMPTY are passed directly to `url-build-query-string'."

  (url-build-query-string
   (telega-plist-map (lambda (key val)
                       (cl-assert (keywordp key))
                       (list (substring (symbol-name key) 1) val))
                     query-params)
   semicolons keep-empty))

(defconst telega-tme--url-regexp
  (rx string-start
      (? (group "http" (? "s") "://"))
      (or "t.me" "telegram.me" "telegram.dog")
      (group "/" (1+ (regexp "[^?]")))  ;path
      (? "?" (group (1+ any))))         ;query
  "Regexp to match urls to the Telegram resources.
Matches only t.me, telegram.me and telegram.dog domains.")

(defun telega-tme-open (url)
  "Open Telegram URL with https://t.me prefix.
URL also can be telega internal url or tg:// url.
Return non-nil if URL has been handled."
  (cond ((string-prefix-p "telega:" url)
         (telega-tme-internal-link-open url)
         t)
        ((or (string-prefix-p "tg://" url)
             (string-match telega-tme--url-regexp url))
         (when-let ((tdlib-link (telega--getInternalLinkType url)))
           (telega-tme-open-tdlib-link tdlib-link)
           t))
        (t
         (telega-debug "WARN: Can't open \"%s\" internally" url)
         nil)))

(defun telega-tme-open-tdlib-link (tdlib-link)
  "Open TDLib's internal link.
To convert url to TDLib link, use `telega--getInternalLinkType'."
  (cl-ecase (telega--tl-type tdlib-link)
    ;; TODO: add other link types
    (internalLinkTypeChatAffiliateProgram
     (let ((ap-chat (telega--searchChatAffiliateProgram
                        (telega-tl-str tdlib-link :username)
                        (telega-tl-str tdlib-link :referrer))))
       (unless ap-chat
         (error "telega: Affiliate program not found"))
       (telega-chat--pop-to-buffer ap-chat)))

    (internalLinkTypeUpgradedGift
     (let ((gift (telega--getUpgradedGift (telega-tl-str tdlib-link :name))))
       (message "telega: TODO, show upgraded gift \"%s\""
                (telega-tl-str tdlib-link :name))
       ))

    (internalLinkTypePublicChat
     (when-let* ((username (plist-get tdlib-link :chat_username))
                 (chat (telega--searchPublicChat username)))
       (telega-chat--pop-to-buffer chat)))

    (internalLinkTypeBotStart
     (let* ((bot-username (plist-get tdlib-link :bot_username))
            (bot-chat (telega--searchPublicChat bot-username))
            (bot-user (when bot-chat
                        (telega-chat-user bot-chat))))
       (unless bot-user
         (error "telega: No such bot @%s" bot-username))

       (telega-chat--pop-to-buffer bot-chat)
       (setq telega-chatbuf--bot-start-parameter
             (telega-tl-str tdlib-link :start_parameter))
       (telega-chatbuf--chat-update "bot-start-parameter")
       ;; Now wait till [START] is pressed
       ))

    ;; (internalLinkTypeAttachmentMenuBot
    ;;  (let ((url
    ;;         getAttachmentMenuBot
    ;;  (telega-browse-url (telega-tl-str tdlib-link :url)))

    (internalLinkTypeMessage
     (let* ((msg-info (telega--getMessageLinkInfo
                       (telega-tl-str tdlib-link :url)))
            (chat-id (plist-get msg-info :chat_id))
            (chat (telega-chat-get chat-id))
            (msg-topic (plist-get msg-info :topic_id))
            (msg (plist-get msg-info :message)))
       (unless msg
         (error (concat "telega: " (telega-i18n "lng_message_not_found"))))
       (cond ((null msg-topic)
              (telega-msg-goto-highlight msg))
             ((eq (telega--tl-type msg-topic) 'messageTopicThread)
              (telega-chat--goto-thread
               chat
               (plist-get msg-topic :message_thread_id)
               (plist-get msg :id)))
             (msg-topic
              (let ((topic (telega-topic-get
                            chat (telega--MessageTopic-id msg-topic))))
                (telega-topic-goto topic (plist-get msg :id))))
             (t
              ;; unreachable
              (cl-assert nil)))))

    (internalLinkTypeChatInvite
     (telega-tme-open-invite-link (plist-get tdlib-link :invite_link)))

    (internalLinkTypeStickerSet
     (telega-tme-open-stickerset (telega-tl-str tdlib-link :sticker_set_name)))

    (internalLinkTypeInstantView
     (let* ((url (telega-tl-str tdlib-link :url))
            (fallback-url (telega-tl-str tdlib-link :fallback_url))
            (iv (telega--getWebPageInstantView url)))
       (if iv
           (telega-webpage--instant-view url "Telegra.ph" iv)
         (telega-browse-url fallback-url 'in-browser))))

    (internalLinkTypeChatBoost
     (let* ((info (telega--getChatBoostLinkInfo (telega-tl-str tdlib-link :url)))
            (chat (telega-chat-get (plist-get info :chat_id)))
            (title (telega-ins--as-string
                    (telega-ins--msg-sender chat
                      :with-avatar-p t
                      :with-username-p 'username
                      :with-brackets-p t)))
            (status (telega--getChatBoostStatus chat)))
       (when (y-or-n-p (concat (telega-i18n "lng_boost_channel_button")
                               " " title
                               (format " [%d/%d]"
                                       (plist-get status :boost_count)
                                       (plist-get status :next_level_boost_count))
                               "? "))
         (telega--boostChat chat)
         (message "telega: %s" (telega-i18n "lng_boost_channel_you_title"
                                 :channel title)))))

    (internalLinkTypeLanguagePack
     (telega-tme-open-lang (telega-tl-str tdlib-link :language_pack_id)))

    (internalLinkTypeStory
     (let* ((chat (telega--searchPublicChat
                      (plist-get tdlib-link :story_poster_username)))
            (story (telega--getStory
                       (plist-get chat :id) (plist-get tdlib-link :story_id))))
       (telega-story-open story)))

    (internalLinkTypeWebApp
     (let* ((chat-bot (telega--searchPublicChat
                          (plist-get tdlib-link :bot_username)))
            (_ (unless (telega-chat-bot-p chat-bot)
                 (error "WebApp username must be bot")))
            (web-app (telega--searchWebApp
                         (or (telega-chat-user chat-bot)
                             (error "WebApp username must be bot"))
                         (plist-get tdlib-link :web_app_short_name))))
       (message "found webapp: %S" web-app)))

    (internalLinkTypeAttachmentMenuBot
     (let ((target-chat (plist-get tdlib-link :target_chat)))
       (cl-ecase (telega--tl-type target-chat)
         (targetChatCurrent
          (let* ((bot-chat (telega--searchPublicChat
                               (plist-get tdlib-link :bot_username)))
                 (web-app-info (telega--openWebApp
                                bot-chat (telega-chat-user bot-chat)
                                (plist-get tdlib-link :url))))
            (message "Webapp info: %S" web-app-info)
            (telega-browse-url (plist-get web-app-info :url))))
         )))

    (internalLinkTypeProxy
     (telega-transient-add-proxy (plist-get tdlib-link :proxy)))

    (internalLinkTypeBackground
     (error "TODO: searchBackground"))

    (internalLinkTypeChatFolderInvite
     (let ((folder-info (telega--checkChatFolderInviteLink
                         (plist-get tdlib-link :invite_link))))
       (error "TODO: telega-describe-chat-folder-info")
       ))
    ))

(defun browse-url-telega (url &rest _args)
  "Browse Telegram urls with telega."
  (telega-tme-open url))
(function-put 'browse-url-telega 'browse-url-browser-kind 'internal)

(provide 'telega-tme)


(if (boundp 'browse-url-default-handlers)
    (add-to-list 'browse-url-default-handlers
                 (cons telega-tme--url-regexp 'browse-url-telega))
  ;; Old Emacs
  (if (and (consp browse-url-browser-function)
           (not (functionp browse-url-browser-function)))
      (add-to-list 'browse-url-browser-function
                   (cons telega-tme--url-regexp 'browse-url-telega))
    (setq browse-url-browser-function
          (cons (cons telega-tme--url-regexp 'browse-url-telega)
                (cons "." browse-url-browser-function)))))

;;; telega-tme.el ends here

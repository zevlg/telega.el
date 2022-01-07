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

;; telega-chat.el depends on telega-tme.el
(declare-function telega-chat-get "telega-chat" (chat-id &optional offline-p))
(declare-function telega-chat-by-username "telega-chat" (username))
(declare-function telega-chat--goto-msg "telega-chat" (chat msg-id &optional highlight callback))
(declare-function telega-chat--pop-to-buffer "telega-chat" (chat &optional no-history-load))
(declare-function telega-chatbuf--prompt-update "telega-chat")


(defun telega-tme--media-timestamp-callback (media-timestamp-str)
  "Generate callback to open media message at MEDIA-TIMESTAMP."
  (when media-timestamp-str
    (lambda ()
      (let ((msg (telega-msg-at (point))))
        (when (telega-msg-type-p
               '(messageVideoNote messageVoiceNote messageAudio messageVideo)
               msg)
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
    (concat "tg:telega:"
            (if-let ((chat-username (telega-chat-username chat)))
                (concat "@" chat-username)
              (number-to-string (plist-get chat :id)))
            (when params
              (concat "?" (telega-tme-build-query-string params)))
            (when-let ((msg (unless (telega-chat-p chat-or-msg) chat-or-msg)))
              (concat "#" (number-to-string (/ (plist-get msg :id) 1048576))))
            )))

(defun telega-tme-open-internal (chat-spec &optional post-spec params)
  "Open internal link to any chat or message.
CHAT-SPEC = @<username> | <chat-id>
POST-SPEC = <POST-ID> | <MSG-ID> (for backward compatibility)
PARAMS is a plist with additional parameters, supported parameters are:
`:open_content' to open the message contents from MSG-SPEC."
  (let* ((chat (or (if (string-prefix-p "@" chat-spec)
                       (telega-chat-by-username (substring chat-spec 1))
                     (telega-chat-get (string-to-number chat-spec) 'offline))
                   (user-error "No chat with CHAT-SPEC=%S" chat-spec)))
         (post-id (when post-spec (string-to-number post-spec)))
         ;; NOTE: For backward compatibility check if POST-ID is
         ;; actually a MSG-ID
         (msg-id (when post-id (if (zerop (% post-id 1048576))
                                   post-id
                                 (* post-id 1048576)))))
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
  (* (string-to-number post) 1048576))

(defun telega-tme-open-privatepost (supergroup post &optional media-timestamp)
  "Open POST in private SUPERGROUP."
  (let ((chat (telega-chat-get (string-to-number (concat "-100" supergroup))
                               'offline)))
    (unless chat
      (error "telega: %s" (telega-i18n "lng_error_post_link_invalid")))
    (telega-chat--goto-msg chat (telega-tme--post-msg-id post) 'highlight
      (telega-tme--media-timestamp-callback media-timestamp))))

(defun telega-tme-open-username (username &rest params)
  "Open chat by its USERNAME.
PARAMS are additional params."
  (cond ((string= username "telegrampassport")
         ;; TODO: passport
         (message "telega TODO: handle `telegrampassport'"))

        ;; See https://core.telegram.org/bots#deep-linking for
        ;; `:start' and `:startgroup' meaning
        ((plist-get params :startgroup)
         (let ((bot-user (telega-chat-user
                          (telega--searchPublicChat username) 'inc-bots))
               (chat (telega-completing-read-chat
                      ;; TODO: i18n
                      (format "Start «%s» in group: "
                              (propertize (concat "@" username) 'face 'bold))
                      (telega-filter-chats
                       telega--ordered-chats
                       '(my-permission :can_invite_users)))))
           (telega-chat--pop-to-buffer chat)
           (telega--sendBotStartMessage
            bot-user chat (plist-get params :startgroup))))

        ((plist-get params :start)
         (let* ((bot-chat (telega--searchPublicChat username))
                (bot-user (telega-chat-user bot-chat 'inc-bots)))
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

(defun telega-tme-open-group (group)
  "Join the GROUP."
  (let* ((url (concat (or (plist-get telega--options :t_me_url)
                          "https://t.me/")
                      "joinchat/" group))
         (link-check (let ((tl-obj (telega--checkChatInviteLink url)))
                       (when (telega--tl-error-p tl-obj)
                         (error "telega: %s" (telega-tl-str tl-obj :error)))
                       tl-obj))
         (chat-id (plist-get link-check :chat_id))
         (chat (when link-check
                 (if (zerop chat-id)
                     ;; Can only join by link
                     (when (y-or-n-p (format "Join \"%s\"? "
                                             (telega-tl-str link-check :title)))
                       (telega--joinChatByInviteLink url))

                   ;; Can preview messages before deciding to join
                   (telega-chat-get chat-id)))))
    (when chat
      (telega-chat--pop-to-buffer chat))))

(defun telega-tme-open-proxy (_type _proxy)
  "Open the PROXY."
  ;; TYPE is "socks" or "proxy"
  ;; :server, :port, :user, :pass, :secret
  (message "TODO: `telega-tme-open-proxy'")
  )

(defun telega-tme-open-stickerset (setname)
  "Open sticker set with SETNAME."
  (let ((sset (telega--searchStickerSet setname)))
    (unless sset
      (user-error "No such sticker set: %s" setname))
    (telega-describe-stickerset sset)))

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

(defun telega-tme-open-tg (url)
  "Open URL starting with `tg:'.
Return non-nil, meaning URL has been handled."
  (when (string-prefix-p "tg://" url)
    ;; Convert it to `tg:' form
    (setq url (concat "tg:" (substring url 5))))

  (let* ((urlobj (url-generic-parse-url url))
         (path-query (url-path-and-query urlobj))
         (path (car path-query))
         (query (telega-tme-parse-query-string (cdr path-query))))
    (cond ((string= path "resolve")
           (let ((username (plist-get query :domain)))
             (setq query (telega-plist-del query :domain))
             (apply #'telega-tme-open-username username query)))
          ((string= path "join")
           (telega-tme-open-group (plist-get query :invite)))
          ((string= path "addstickers")
           (telega-tme-open-stickerset (plist-get query :set)))
          ((string= path "addtheme")
           (telega-tme-open-theme (plist-get query :slug)))
          ((string= path "setlanguage")
           (telega-tme-open-lang (plist-get query :lang)))
          ((string= path "privatepost")
           (telega-tme-open-privatepost
            (plist-get query :channel) (plist-get query :post)
            (plist-get query :t)))
          ((or (string= path "msg") (string= path "share"))
           )
          ((string= path "msg_url")
           )
          ((string= path "confirmphone")
           )
          ((or (string= path "passport") (string= path "secureid"))
           )
          ((or (string= path "socks") (string= path "proxy"))
           (telega-tme-open-proxy path query))
          ((string= path "login")
           )
          ;; Internal links to any chat or message
          ;; See: https://github.com/zevlg/telega.el/issues/139
          ;; Internal links are in form
          ;; tg:telega:<CHAT-SPEC>[?<PARAMS>][#<POST-ID>]
          ;;   CHAT-SPEC =  @USERNAME | CHAT-ID
          ;;   PARAMS = query params such as "open_content=t"
          ;;   POST-ID = MSG-ID div 1048576
          ((string-match "^telega:\\([^#]+\\)" path)
           (telega-tme-open-internal
            (match-string 1 path) (url-target urlobj) query))
          (t
           (message "telega: Unsupported tg url: %s" url))))
  t)

(defconst telega-tme--url-regexp
  (rx string-start
      (? (group "http" (? "s") "://"))
      (or "t.me" "telegram.me" "telegram.dog")
      (group "/" (1+ (regexp "[^?]")))  ;path
      (? "?" (group (1+ any))))         ;query
  "Regexp to match urls to the Telegram resources.
Matches only t.me, telegram.me and telegram.dog domains.")

(defun telega-tme-open (url &optional just-convert)
  "Open any URL with https://t.me prefix.
If JUST-CONVERT is non-nil, return converted link value.
Return non-nil if url has been handled."
  ;; Convert URL to `tg:' form and call `telega-tme-open-tg'
  (when (string-match telega-tme--url-regexp url)
    (let* ((path (match-string 2 url))
           (query (match-string 3 url))
           (case-fold-search nil)         ;ignore case
           (tg
            (cond ((string-match "^/joinchat/\\([a-zA-Z0-9._-]+\\)$" path)
                   (concat "tg:join?invite=" (match-string 1 path)))
                  ((string-match "^/addstickers/\\([a-zA-Z0-9._-]+\\)$" path)
                   (concat "tg:addstickers?set=" (match-string 1 path)))
                  ((string-match "^/addtheme/\\([a-zA-Z0-9._-]+\\)$" path)
                   (concat "tg:addtheme?slug=" (match-string 1 path)))
                  ((string-match "^/setlanguage/\\([a-zA-Z0-9._-]+\\)$" path)
                   (concat "tg:setlanguage?lang=" (match-string 1 path)))
                  ((string-match "^/share/url$" path)
                   (concat "tg:msg_url?" query))
                  ((string-match "^/\\(socks\\|proxy\\)$" path)
                   (concat "tg:" (match-string 1 path) "?" query))
                  ((string-match
                    (eval-when-compile
                      (rx (and line-start "/c/"
                               (group (? "-") (1+ digit))
                               "/"
                               (group (1+ digit)))))
                    path)
                   (concat "tg:privatepost?channel=" (match-string 1 path)
                           "&post=" (match-string 2 path)
                           (when query (concat "&" query))))
                  ((string-match "^/\\+\\([^/]+\\)$" path)
                   (concat "tg:join?invite=" (match-string 1 path)))
                  ((string-match
                    (eval-when-compile
                      (rx (and line-start "/"
                               (group (1+ (regexp "[a-zA-Z0-9\\.\\_]")))
                               (? "/" (group (1+ digit))))))
                    path)
                   (concat "tg:resolve?domain=" (match-string 1 path)
                           (when (match-string 2 path)
                             (concat "&post=" (match-string 2 path)))
                           (when query (concat "&" query)))))))
      (cond (just-convert tg)
            (tg (telega-tme-open-tg tg) t)
            (t
             (telega-debug "WARN: Can't open \"%s\" internally" url)
             nil)))))

(defun telega-tme-open-tdlib-link (tdlib-link)
  "Open TDLib's internal link.
To convert url to TDLib link, use `telega--getInternalLinkType'."
  (cl-ecase (telega--tl-type tdlib-link)
    (internalLinkTypeBotStart
     (let* ((bot-username (plist-get tdlib-link :bot_username))
            (bot-chat (telega--searchPublicChat bot-username))
            (bot-user (when bot-chat
                        (telega-chat-user bot-chat 'inc-bots))))
       (unless bot-user
         (error "telega: No such bot @%s" bot-username))

       (telega-chat--pop-to-buffer bot-chat)
       (setq telega-chatbuf--bot-start-parameter
             (telega-tl-str tdlib-link :start_parameter))
       (telega-chatbuf--prompt-update)
       ;; Now wait till [START] is pressed
       ))
    (internalLinkTypeMessage
     (telega-tme-open (plist-get tdlib-link :url)))

    (internalLinkTypeActiveSessions
     )

    (internalLinkTypeVoiceChat
     )

    (internalLinkTypeChatInvite
     (let* ((invite-link (plist-get tdlib-link :invite_link))
            (invite-link-info (telega--checkChatInviteLink invite-link)))
       ;; TODO
       ))
    ))

(provide 'telega-tme)

;;; telega-tme.el ends here

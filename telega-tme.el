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

(require 'telega-sticker)

(declare-function telega-chat-get "telega-chat" (chat-id &optional offline-p))
(declare-function telega-chat--goto-msg "telega-chat" (chat msg-id &optional highlight))
(declare-function telega-chat--pop-to-buffer "telega-chat" (chat))
(declare-function telega--searchPublicChat "telega-chat" (username))
(declare-function telega--checkChatInviteLink "telega-chat" (invite-link))
(declare-function telega--joinChatByInviteLink "telega-chat" (invite-link))


(defun telega-tme-open-username (username &rest bot-params)
  "Open chat by its USERNAME.
BOT-PARAMS are additional params."
  (cond ((string= username "telegrampassport")
         ;; TODO: passport
         (message "telega TODO: handle `telegrampassport'"))
        ((plist-get bot-params :start)
         ;; :start :startgroup :game :post
         (message "telega TODO: handle bot start"))

        (t
         ;; Ordinary user/channel/group, :post
         (let ((chat (telega--searchPublicChat username))
               (post (plist-get bot-params :post)))
           (unless chat
             (error "Unknown public chat: %s" username))
           (if post
               ;; See https://github.com/tdlib/td/issues/16
               ;; msg-id = post * 1048576
               (telega-chat--goto-msg
                chat (* (string-to-number post) 1048576) 'highlight)
             (telega-chat--pop-to-buffer chat))))
        ))

(defun telega-tme-open-group (group)
  "Join the GROUP."
  (let* ((url (concat (or (plist-get telega--options :t_me_url)
                          "https://t.me/")
                      "joinchat/" group))
         (link-check (telega--checkChatInviteLink url))
         (chat-id (plist-get link-check :chat_id))
         (chat (when link-check
                 (if (zerop chat-id)
                     ;; Can only join by link
                     (when (y-or-n-p (format "Join \"%s\"? "
                                             (plist-get link-check :title)))
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

(defun telega-tme-parse-query-string (query-string)
  "Parse QUERY-STRING and return it as plist.
Multiple params with same name in QUERY-STRING is disallowed."
  (let ((query (url-parse-query-string query-string 'downcase)))
    (cl-loop for (name val) in query
             nconc (list (intern (concat ":" name)) val))))

(defun telega-tme-open-tg (url)
  "Open URL starting with `tg:'.
Return non-nil, meaning URL has been handled."
  (when (string-prefix-p "tg://" url)
    ;; Convert it to `tg:' form
    (setq url (concat "tg:" (substring url 5))))

  (let* ((path-query (url-path-and-query
                      (url-generic-parse-url url)))
         (path (car path-query))
         (query (telega-tme-parse-query-string (cdr path-query))))
    (cond ((string= path "resolve")
           (let ((username (plist-get query :domain)))
             (setq query (telega-plist-del query :domain))
             (apply 'telega-tme-open-username username query)))
          ((string= path "join")
           (telega-tme-open-group (plist-get query :invite)))
          ((string= path "addstickers")
           (telega-tme-open-stickerset (plist-get query :set)))
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
          (t
           (message "telega: Unsupported tg url: %s" url))))
  t)

(defun telega-tme-open (url &optional just-convert)
  "Open any URL with https://t.me prefix.
If JUST-CONVERT is non-nil, return converted link value.
JUST-CONVERT used for testing only.
Return non-nil if url has been handled."
  ;; Convert URL to `tg:' form and call `telega-tme-open-tg'
  (let* ((path-query (url-path-and-query (url-generic-parse-url url)))
         (path (car path-query))
         (query (cdr path-query))
         (case-fold-search nil)         ;ignore case
         (tg (cond ((string-match "^/joinchat/\\([a-zA-Z0-9._-]+\\)$" path)
                    (concat "tg:join?invite=" (match-string 1 path)))
                   ((string-match "^/addstickers/\\([a-zA-Z0-9._]+\\)$" path)
                    (concat "tg:addstickers?set=" (match-string 1 path)))
                   ((string-match "^/share/url$" path)
                    (concat "tg:msg_url?" query))
                   ((string-match "^/\\(socks\\|proxy\\)$" path)
                    (concat "tg:" (match-string 1 path) "?" query))
                   ((string-match
                     (rx (and line-start "/"
                              (group (1+ (regexp "[a-zA-Z0-9\\.\\_]")))
                              (? "/" (group (1+ digit)))))
                     path)
                    (concat "tg:resolve?domain=" (match-string 1 path)
                            (when (match-string 2 path)
                              (concat "&post=" (match-string 2 path)))
                            (when query (concat "&" query)))))))
    (cond (just-convert tg)
          (tg (telega-tme-open-tg tg) t)
          (t (telega-debug "WARN: Can't open \"%s\" internally") nil))))

(provide 'telega-tme)

;;; telega-tme.el ends here

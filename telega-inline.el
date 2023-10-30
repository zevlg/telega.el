;;; telega-inline.el --- Support for inline stuff  -*- lexical-binding:t -*-

;; Copyright (C) 2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Feb 14 04:51:54 2019
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

;; Inline bots support

;;  @gif @youtube @pic @vid etc

;;; Code:
(require 'telega-core)
(require 'telega-tdlib)

(declare-function telega-browse-url "telega-webpage" (url &optional in-web-browser))
(declare-function telega-chatbuf-input-insert "telega-chat" (imc))
(declare-function telega-chatbuf-attach-inline-bot-query "telega-chat" (&optional no-empty-search))
(declare-function telega-chat--pop-to-buffer "telega-chat" (chat))
(declare-function telega-chat-private-p "telega-chat" (chat))

(defvar telega--inline-bot nil
  "BOT value for the inline results help buffer.")
(defvar telega--inline-query nil
  "Query string in help buffer.")
(defvar telega--inline-results nil
  "Value for `inlineQueryResults' in help buffer.")


(defun telega--on-callbackQueryAnswer (reply)
  "Handle callback reply answer."
  (let ((text (telega-tl-str reply :text))
        (link (telega-tl-str reply :url)))
    (if (plist-get reply :show_alert)
        ;; Popup message from the bot
        (with-telega-help-win "*Callback Alert*"
          (telega-ins text)
          (when link
            (telega-ins "\n")
            (telega-ins--raw-button (telega-link-props 'url link 'face 'link)
              (telega-ins link))))

      (message text)
      (when link
        (telega-browse-url link)))))

(defun telega--getCallbackQueryAnswer (msg payload)
  "Async send callback to bot."
  (telega-server--send
   (list :@type "getCallbackQueryAnswer"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id)
         :payload payload)))

(defun telega-inline--callback (kbd-button msg)
  "Action to take when KBD-BUTTON is pressed."
  (let ((kbd-type (plist-get kbd-button :type)))
    (cl-ecase (telega--tl-type kbd-type)
      (keyboardButtonTypeText
       ;; A simple button, with text that should be sent
       ;; when the button is pressed
       ;; NOTE: (see https://t.me/emacs_telega/14354)
       ;;  - Send as reply to the message for group chats
       ;;  - Send as ordinary message for private chats
       (let ((chat (telega-msg-chat msg))
             (imc (list :@type "inputMessageText"
                        :text (telega-string-fmt-text
                               (telega-tl-str kbd-button :text))))
             (reply-msg (unless (telega-chat-private-p (telega-msg-chat msg))
                          msg)))
         (telega--sendMessage chat imc reply-msg)))

      (keyboardButtonTypeRequestPhoneNumber
       (telega-chat-share-my-contact (telega-msg-chat msg)))

      (inlineKeyboardButtonTypeUrl
       (telega-browse-url (telega-tl-str kbd-type :url)))

      (inlineKeyboardButtonTypeLoginUrl
       (let* ((url-info (telega--getLoginUrlInfo msg kbd-type))
              (url (or (plist-get url-info :url)
                       (plist-get kbd-type :url))))
         (when (or (plist-get url-info :skip_confirmation)
                   (not (memq telega-inline-login-url-action
                              '(query-all query-open)))
                   (y-or-n-p
                    (telega-i18n "lng_url_auth_open_confirm" :link url)))
           ;; Check we need to log into domain ?
           ;; See https://github.com/tdlib/td/issues/1189#issuecomment-691445789
           (when (and (eq (telega--tl-type url-info)
                          'loginUrlInfoRequestConfirmation)
                      (memq telega-inline-login-url-action
                            '(query-all query-login-and-write-access
                                        query-login-only))
                      (y-or-n-p
                       (concat (telega-i18n "lng_url_auth_login_option"
                                 :domain (propertize
                                          (telega-tl-str url-info :domain)
                                          'face 'bold)
                                 :user (telega-msg-sender-title (telega-user-me)
                                         :with-username-p t))
                               "? ")))
             (let ((write-p (when (and (memq telega-inline-login-url-action
                                             '(query-all
                                               query-login-and-write-access))
                                       (plist-get url-info :request_write_access))
                              (y-or-n-p
                               (concat (telega-i18n "lng_url_auth_allow_messages"
                                         :bot (propertize
                                               (telega-msg-sender-title
                                                (telega-user-get
                                                 (plist-get
                                                  url-info :bot_user_id))
                                                :with-username-p t)
                                               'face 'bold))
                                       "? ")))))
               (setq url (plist-get (telega--getLoginUrl msg kbd-type write-p)
                                    :url))))

           (telega-browse-url url))))

      (inlineKeyboardButtonTypeCallback
       (telega--getCallbackQueryAnswer
        msg (list :@type "callbackQueryPayloadData"
                  :data (plist-get kbd-type :data))))

      (inlineKeyboardButtonTypeSwitchInline
       ;; Generate another inline query to the bot
       (let* ((via-bot-user-id (plist-get msg :via_bot_user_id))
              (bot (if (not (zerop via-bot-user-id))
                       (telega-user-get via-bot-user-id)
                     (telega-msg-sender msg)))
              (new-query (telega-tl-str kbd-type :query)))
         (when (and bot (telega-user-bot-p bot))
           (unless (plist-get kbd-type :in_current_chat)
             (telega-chat--pop-to-buffer
              (telega-completing-read-chat "To chat: ")))

           (telega-chatbuf--input-delete)
           (telega-chatbuf-input-insert
            (concat (telega-msg-sender-username bot 'with-@)
                    " " (or new-query "")))
           (telega-chatbuf-attach-inline-bot-query 'no-search))))

      (inlineKeyboardButtonTypeCallbackGame
       (telega--getCallbackQueryAnswer
        msg (list :@type "callbackQueryPayloadGame"
                  :game_short_name
                  (telega--tl-get msg :content :game :short_name))))

      (inlineKeyboardButtonTypeBuy
       (let ((payment-form (telega--getPaymentForm msg)))
         (message "payment-form: %S" payment-form)))

      (inlineKeyboardButtonTypeUser
       (when-let ((user (telega-user-get (plist-get kbd-type :user_id))))
         (telega-chat-with user)))

      ;; TODO: other types
      )))

(defun telega-inline--help-echo (kbd-button _msg)
  "Generate help-echo value for KBD-BUTTON."
  (let ((kbd-type (plist-get kbd-button :type)))
    (cl-case (telega--tl-type kbd-type)
      ((inlineKeyboardButtonTypeUrl inlineKeyboardButtonTypeLoginUrl)
       (telega-tl-str kbd-type :url))
      )))


(defun telega--getInlineQueryResults (bot-user query &optional chat
                                               offset location callback)
  "Query BOT-ID for the QUERY."
  (declare (indent 5))
  (telega-server--call
   (nconc (list :@type "getInlineQueryResults"
                :bot_user_id (plist-get bot-user :id)
                :query query)
          (when chat
            (list :chat_id (plist-get chat :id)))
          (when location
            (list :location location))
          (when offset
            (list :offset offset)))
   callback))

(defun telega-ins--inline-delim ()
  "Inserter for the delimiter."
  (telega-ins--with-props
      '(face default display ((space-width 2) (height 0.5)))
    (telega-ins (make-string 30 ?─) "\n")))

(defun telega-inline-bot--action (qr)
  "Action to take when corresponding query result QR button is pressed."
  (cl-assert telega--chat)
  (cl-assert telega--inline-bot)
  (cl-assert telega--inline-results)
  (cl-assert (eq major-mode 'help-mode))

  (let ((chat telega--chat)
        (inline-query telega--inline-query)
        (inline-results telega--inline-results)
        (bot telega--inline-bot))
    ;; NOTE: Kill help win before modifying chatbuffer, because it
    ;; recovers window configuration on kill
    (quit-window 'kill-buffer)

    (let* ((thumb (cl-case (telega--tl-type qr)
                    (inlineQueryResultAnimation
                     (telega--tl-get qr :animation :thumbnail))
                    (inlineQueryResultArticle
                     (plist-get qr :thumbnail))
                    (inlineQueryResultPhoto
                     (telega-photo--thumb (plist-get qr :photo)))
                    (inlineQueryResultGame
                     (telega-photo--thumb (telega--tl-get qr :game :photo)))
                    (inlineQueryResultVideo
                     (telega--tl-get qr :video :thumbnail))))
           (thumb-file (when thumb (telega-file--renew thumb :file)))
           (thumb-img (when (telega-file--downloaded-p thumb-file)
                        (telega-create-image
                         (telega--tl-get thumb-file :local :path)
                         (when (fboundp 'imagemagick-types) 'imagemagick) nil
                         :scale 1.0 :ascent 'center
                         :height (telega-chars-xheight 1)))))
      (with-telega-chatbuf chat
        (telega-chatbuf--input-delete)
        (telega-chatbuf-input-insert
         (list :@type "telegaInlineQuery"
               :preview thumb-img
               :caption (substring (plist-get qr :@type) 17)
               :query inline-query
               :via-bot bot
               :hide-via-bot current-prefix-arg
               :query-id (plist-get inline-results :inline_query_id)
               :result-id (telega-tl-str qr :id)))))))

(defun telega-ins--inline-audio (qr)
  "Inserter for `inlineQueryResultAudio' QR."
  (let ((audio (plist-get qr :audio)))
    (telega-ins--audio nil audio (telega-symbol 'audio))
    (telega-ins "\n")))

(defun telega-ins--inline-voice-note (qr)
  "Inserter for `inlineQueryResultVoiceNote' QR."
  (let ((voice-note (plist-get qr :voice_note)))
    (telega-ins (telega-tl-str qr :title) "\n")
    (telega-ins--voice-note nil voice-note)
    (telega-ins "\n")))

(defun telega-ins--inline-sticker (qr)
  "Inserter for `inlineQueryResultSticker' QR."
  (let ((sticker (plist-get qr :sticker)))
    (telega-ins--sticker-image sticker)))

(defun telega-ins--inline-animation (qr)
  "Inserter for `inlineQueryResultAnimation' QR."
  (let ((anim (plist-get qr :animation)))
    (telega-ins--animation-image anim)))

(defun telega-ins--inline-photo (qr)
  "Inserter for `inlineQueryResultPhoto' QR."
  (let ((photo (plist-get qr :photo)))
    (telega-ins--image
     (telega-photo--image photo telega-inline-photo-size-limits))))

(defun telega-ins--inline-document (qr)
  "Inserter for `inlineQueryResultDocument' QR."
  (let* ((doc (plist-get qr :document))
         (thumb (plist-get doc :thumbnail))
         (thumb-img (when thumb
                      (telega-media--image
                       (cons thumb 'telega-thumb--create-image-two-lines)
                       (cons thumb :photo)))))
    (telega-ins--document-header doc)
    (telega-ins "\n")

    ;; documents thumbnail preview (if any)
    (when thumb-img
      (telega-ins--image thumb-img 0))
    (telega-ins " " (telega-tl-str qr :title) "\n")
    (when thumb-img
      (telega-ins--image thumb-img 1))
    (telega-ins " " (telega-tl-str qr :description) "\n")))

(defun telega-ins--inline-article (qr)
  "Inserter for `inlineQueryResultArticle' QR."
  (let* ((thumb (plist-get qr :thumbnail))
         (thumb-img (when thumb
                      (telega-media--image
                       (cons thumb 'telega-thumb--create-image-two-lines)
                       (cons thumb :file)))))
    (when thumb-img
      (telega-ins--image thumb-img 0))
    (telega-ins " " (telega-tl-str qr :title) "\n")
    (when thumb-img
      (telega-ins--image thumb-img 1))
    (telega-ins " " (telega-tl-str qr :description) "\n")
    ))

(defun telega-ins--inline-video (qr)
  "Inserter for `inlineQueryResultVideo' QR."
  (let* ((video (plist-get qr :video))
         (thumb (plist-get video :thumbnail))
         (thumb-img (when thumb
                      (telega-media--image
                       (cons thumb 'telega-thumb--create-image-two-lines)
                       (cons thumb :file)))))
    (when thumb-img
      (telega-ins--image thumb-img 0)
      (telega-ins " "))
    (telega-ins (telega-tl-str qr :title))
    (telega-ins "\n")
    (when thumb-img
      (telega-ins--image thumb-img 1)
      (telega-ins " "))
    (telega-ins-fmt "%dx%d %s"
      (plist-get video :width)
      (plist-get video :height)
      (telega-duration-human-readable (plist-get video :duration)))
    (telega-ins "\n")))

(defun telega-ins--inline-game (qr)
  "Inserter for `inlineQueryResultGame' QR."
  (let* ((game (plist-get qr :game))
         (photo (plist-get game :photo))
         (photo-img (when photo
                      (telega-photo--image photo (list 4 2 4 2)))))
    (when photo-img
      (telega-ins--image photo-img 0)
      (telega-ins " "))
    (telega-ins--with-face 'bold
      (telega-ins (telega-tl-str game :title)))
    (telega-ins "\n")
    (when photo-img
      (telega-ins--image photo-img 1)
      (telega-ins " "))
    (telega-ins (telega-tl-str game :description))
    (telega-ins "\n")))

(defun telega-ins--inline-venue (qr)
  "Inserter for `inlineQueryResultVenue' OR."
  (let* ((venue (plist-get qr :venue))
         (thumb (plist-get qr :thumbnail))
         (thumb-img (when thumb
                      (telega-media--image
                       (cons thumb 'telega-thumb--create-image-three-lines)
                       (cons thumb :photo)))))
    (when thumb-img
      (telega-ins--image thumb-img 0)
      (telega-ins " "))
    (telega-ins--with-face 'bold
      (telega-ins (telega-tl-str venue :title)))
    (telega-ins "\n")
    (when thumb-img
      (telega-ins--image thumb-img 1)
      (telega-ins " "))
    (telega-ins--with-face 'telega-shadow
      (telega-ins (telega-tl-str venue :address)))
    (telega-ins "\n")
    (when thumb-img
      (telega-ins--image thumb-img 2)
      (telega-ins " "))
    (telega-ins--location (plist-get venue :location))
    (telega-ins "\n")))

(defun telega-inline-bot--gen-callback (bot query &optional for-chat)
  "Generate callback for the BOT's QUERY result handling in FOR-CHAT."
  (lambda (reply)
    (if-let ((qr-results (append (plist-get reply :results) nil)))
        (let ((help-window-select telega-inline-query-window-select))
          (with-telega-help-win "*Telegram Inline Results*"
            (visual-line-mode 1)
            ;; NOTE: Non-nil `auto-window-vscroll' make C-n jump to the end
            ;; of the buffer
            (set (make-local-variable 'auto-window-vscroll) nil)

            (setq telega--inline-bot bot)
            (setq telega--inline-query query)
            (setq telega--inline-results reply)
            (setq telega--chat for-chat)

            (dolist (qr qr-results)
              ;; NOTE: possible insert the delimiter, so mixing for
              ;; example Articles and Animations is possible
              (when (memq (telega--tl-type qr)
                          '(inlineQueryResultVideo
                            inlineQueryResultAudio
                            inlineQueryResultArticle
                            inlineQueryResultDocument
                            inlineQueryResultGame
                            inlineQueryResultVenue))
                (unless (or (= (point) (line-beginning-position))
                            (= (point) 1))
                  (telega-ins "\n")
                  (telega-ins--inline-delim)))

              (cl-case (telega--tl-type qr)
                (inlineQueryResultDocument
                 (telega-button--insert 'telega qr
                   :inserter 'telega-ins--inline-document
                   :action 'telega-inline-bot--action
                   'cursor-sensor-functions
                   '(telega-button-highlight--sensor-func))
                 (telega-ins--inline-delim))

                (inlineQueryResultVideo
                 (telega-button--insert 'telega qr
                   :inserter 'telega-ins--inline-video
                   :action 'telega-inline-bot--action
                   'cursor-sensor-functions
                   '(telega-button-highlight--sensor-func))
                 (telega-ins--inline-delim))

                (inlineQueryResultAudio
                 (telega-button--insert 'telega qr
                   :inserter 'telega-ins--inline-audio
                   :action 'telega-inline-bot--action
                   'cursor-sensor-functions
                   '(telega-button-highlight--sensor-func))
                 (telega-ins--inline-delim))

                (inlineQueryResultVoiceNote
                 (telega-button--insert 'telega qr
                   :inserter 'telega-ins--inline-voice-note
                   :action 'telega-inline-bot--action
                   'cursor-sensor-functions
                   '(telega-button-highlight--sensor-func))
                 (telega-ins--inline-delim))

                (inlineQueryResultArticle
                 (telega-button--insert 'telega qr
                   :inserter 'telega-ins--inline-article
                   :action 'telega-inline-bot--action
                   'cursor-sensor-functions
                   '(telega-button-highlight--sensor-func))
                 (telega-ins--inline-delim))

                (inlineQueryResultAnimation
                 (telega-button--insert 'telega qr
                   :inserter 'telega-ins--inline-animation
                   :action 'telega-inline-bot--action
                   'cursor-sensor-functions
                   (list (telega-animation--gen-sensor-func
                          (plist-get qr :animation)))
                   'help-echo (when-let ((title (telega-tl-str qr :title)))
                                (format "GIF title: %s" title))))

                (inlineQueryResultPhoto
                 (telega-button--insert 'telega qr
                   :inserter 'telega-ins--inline-photo
                   :action 'telega-inline-bot--action))

                (inlineQueryResultSticker
                 (telega-button--insert 'telega qr
                   :inserter 'telega-ins--inline-sticker
                   :action 'telega-inline-bot--action))

                (inlineQueryResultGame
                 (telega-button--insert 'telega qr
                   :inserter 'telega-ins--inline-game
                   :action 'telega-inline-bot--action)
                 (telega-ins--inline-delim))

                (inlineQueryResultVenue
                 (telega-button--insert 'telega qr
                   :inserter 'telega-ins--inline-venue
                   :action 'telega-inline-bot--action)
                 (telega-ins--inline-delim))

                (t
                 (telega-ins-fmt "* %S\n" qr))))))

      ;; Not found
      (unless (string-empty-p query)
        (message "telega: @%s Nothing found for %s"
                 (telega-msg-sender-username bot)
                 (propertize query 'face 'bold)))
      )))

(defun telega-inline-bot-query (bot query for-chat)
  "Query BOT for inline results for the QUERY."
  (with-telega-chatbuf for-chat
    ;; Cancel currently active inline-query loading
    (when (telega-server--callback-get telega-chatbuf--inline-query)
      (telega-server--callback-put telega-chatbuf--inline-query 'ignore))

    (message "telega: @%s Searching for %s..."
             (telega-msg-sender-username bot) (propertize query 'face 'bold))
    (setq telega-chatbuf--inline-query
          (telega--getInlineQueryResults bot query for-chat nil nil
            (telega-inline-bot--gen-callback bot query for-chat)))))

(defun telega--recent-inline-bots-fetch ()
  "Update recently used bots."
  (telega--getRecentInlineBots
   (lambda (users)
     (setq telega--recent-inline-bots
           (mapcar (lambda (user)
                     (telega-user-title user 'username))
                   users)))))

(provide 'telega-inline)

;;; telega-inline.el ends here

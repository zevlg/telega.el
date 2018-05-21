;;; telega-msg.el --- Messages for telega

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri May  4 03:49:22 2018
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
(require 'telega-customize)

(defvar telega-msg-button-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map [remap self-insert-command] 'ignore)
    (define-key map (kbd "i") 'telega-msg-info)
    (define-key map (kbd "n") 'telega-msg-next)
    (define-key map (kbd "p") 'telega-msg-prev)
    (define-key map (kbd "r") 'telega-msg-reply)
    (define-key map (kbd "e") 'telega-msg-edit)
    (define-key map (kbd "f") 'telega-msg-forward)
    (define-key map (kbd "DEL") 'telega-msg-delete)
    map))

(define-button-type 'telega-msg
  :supertype 'telega
  'keymap telega-msg-button-keymap
  'action #'telega-msg-reply)

(defsubst telega-msg--chat (msg)
  "Return chat for the MSG."
  (telega-chat--get (plist-get msg :chat_id)))

(defun telega-msg--get (chat-id msg-id)
  "Get message."
  (telega-server--call
   (list :@type "getMessage"
         :chat_id chat-id
         :message_id msg-id)))

(defun telega-msg-chat-title (msg)
  "Title of the message's chat."
  (telega-chat--title (telega-msg--chat msg) 'with-username))

(defun telega-msg-sender-shortname (msg)
  "Short name of the MSG sender."
  (telega-user--short-name
   (telega-user--get (plist-get msg :sender_user_id))))

(defun telega-msg-sender-name (msg)
  "Title of the MSG sender."
  (telega-user--title
   (telega-user--get (plist-get msg :sender_user_id))
   'with-username))

(defun telega-msg-sender-ava-h (msg)
  "High stripe of the MSG sender's avatar."
  ;; TODO: sender avatar low stripe
  "[AH]")

(defun telega-msg-sender-ava-l (msg)
  "Low stripe of the MSG sender's avatar."
  ;; TODO: sender avatar high stripe
  "[AL]")

(defun telega-msg-sender-same-p (msg cmp-with-msg &optional with-via-bot)
  "Return non-nil if MSG has same sender as CMP-WITH-MSG.
For edited messages always return nil."
  (and (zerop (plist-get msg :edit_date))
       (= (plist-get msg :sender_user_id)
          (plist-get cmp-with-msg :sender_user_id))
       (if with-via-bot
           (string= (telega-msg-via-bot msg)
                    (telega-msg-via-bot cmp-with-msg))
         t)))

(defun telega-msg-timestamp (msg)
  "Format MSG date."
  (telega-fmt-timestamp (plist-get msg :date)))

(defun telega-msg-text-with-props (msg)
  "Return formatted text for the MSG."
  (let ((content (plist-get msg :content)))
    (case (telega--tl-type content)
      (messageText
       (let ((text (plist-get content :text)))
         (telega-msg--ents-to-props
          (plist-get text :text)
          (plist-get text :entities))))
      (t (format "<unsupported message %S>" (telega--tl-type content))))))

(defun telega-msg-text-with-props-one-line (msg)
  "Format MSG's text as one line."
  (replace-regexp-in-string "\n" " " (telega-msg-text-with-props msg)))

(defun telega-msg-button--format-error (msg)
  (error "Must set :format explicitly for message buttons."))

(defun telega-msg-button--format-one-line (msg)
  "Format message MSG to be displayed in one line."
  `(telega-msg-sender-shortname
    "> "
    telega-msg-text-with-props-one-line))

(defun telega-msg-sender-admin-status (msg)
  (let ((admins-tl (telega-server--call
                    (list :@type "getChatAdministrators"
                          :chat_id (plist-get msg :chat_id)))))
    (if (cl-find (plist-get msg :sender_user_id)
                 (plist-get admins-tl :user_ids)
                 :test #'=)
        " (admin)"
      "")))

(defun telega-msg-sender-status (msg)
  "Format MSG sender status.
Makes heave online requests without caching, be carefull."
  (let ((ucm (telega-server--call
              (list :@type "getChatMember"
                    :chat_id (plist-get msg :chat_id)
                    :user_id (plist-get msg :sender_user_id)))))
    (telega-fmt-chat-member-status (plist-get ucm :status))))

(defun telega-msg-via-bot (msg)
  (let ((via-bot-id (plist-get msg :via_bot_user_id)))
    (if (zerop via-bot-id)
        ""
      (concat " via @" (plist-get (telega-user--get via-bot-id) :username)))))

(defun telega-msg-edit-date (msg)
  (let ((edit-date (plist-get msg :edit_date)))
    (if (zerop edit-date)
        ""
      (concat " edited at " (telega-fmt-timestamp edit-date)))))

(defun telega-msg-inline-reply (msg fill-prefix)
  (let* ((reply-to-msg-id (plist-get msg :reply_to_message_id))
         (reply-msg (unless (zerop reply-to-msg-id)
                      (telega-msg--get (plist-get msg :chat_id)
                                       reply-to-msg-id))))
    (if reply-msg
        `((("| Reply: " ,(telega-fmt-eval
                          (telega-msg-button--format-one-line reply-msg)
                          reply-msg))
           :max ,(- telega-chat-fill-column (length fill-prefix))
           :face telega-chat-inline-reply)
          "\n" ,fill-prefix)
      '(""))))

(defun telega-msg-button--format-msg (msg fill-prefix)
  `((telega-msg-text-with-props
     :fill left
     :fill-prefix ,fill-prefix
     :fill-column ,telega-chat-fill-column
     :min ,telega-chat-fill-column
     :align left)
    (telega-msg-timestamp :align right :min 10)
    "\n"))

(defun telega-msg-button--format-channel (msg)
  `((telega-msg-chat-title :face telega-chat-user-title)
    telega-msg-via-bot
    " " ,telega-symbol-eye " " ,(plist-get msg :views)
    telega-msg-edit-date
    "\n"
    ,@(telega-msg-button--format-msg msg "")))

(defconst telega-msg-full-prefix "     ")

(defun telega-msg-button--format-full (msg)
  `(telega-msg-sender-ava-h
    " "
    (telega-msg-sender-name :face telega-chat-user-title)
;    (telega-msg-sender-status :face shadow)
    telega-msg-via-bot
    telega-msg-edit-date
    "\n"
    telega-msg-sender-ava-l " "
    ,@(telega-msg-inline-reply msg telega-msg-full-prefix)
    ,@(telega-msg-button--format-msg msg telega-msg-full-prefix)))

(defun telega-msg-button--format-same (msg)
  "Fromat message when previous msg is from the same sender."
  `(,telega-msg-full-prefix
    ,@(telega-msg-inline-reply msg telega-msg-full-prefix)
    ,@(telega-msg-button--format-msg msg telega-msg-full-prefix)))

(defun telega-msg--entity-to-properties (entity)
  )

(defun telega-msg--ents-to-props (text entities)
  "Convert message TEXT with text ENTITIES to propertized string."
  (mapc
   (lambda (ent)
     (let ((props (telega-msg--entity-to-properties ent)))
       (when props
         (add-text-properties
          (plist-get ent :offset)
          (+ (plist-get ent :offset) (plist-get ent :length))
          props text))))
   entities)
  text)

(defun telega-msg--props-to-ents (text)
  "Convert propertiezed TEXT to message with text entities."
  ;; TODO: convert text properties to tl text entities
  (let ((entities))
    (cons text entities)))

(defun telega-msg-format (msg)
  (plist-get (plist-get (plist-get msg :content) :text) :text)
  )

(defun telega-msg--input-content (text &optional markdown)
  "Convert TEXT to tl InputMessageContent.
If MARKDOWN is non-nil then format TEXT as markdown."
  (let ((fmt-text (if markdown
                      (telega-server--call
                       `(:@type "parseTextEntities"
                                :text ,text
                                :parse_mode (:@type "textParseModeMarkdown")))
                    `(:@type "formattedText" :text ,text :entities []))))
    `(:@type "inputMessageText"
             :text ,fmt-text
             :clear_draft t)))


(defun telega-msg-info (msg)
  "Show info about message at point."
  (interactive (list (button-get (button-at (point)) :value)))
  (unless msg
    (error "No message at point"))

  (with-help-window " *Telegram Message Info*"
    (set-buffer standard-output)
    (insert (format "Chat-id: %d\n" (plist-get msg :chat_id)))
    (insert (format "Message-id: %d\n" (plist-get msg :id)))

    (insert (format "\nTODO: %S\n" msg))
    ))

(defun telega-msg-reply (msg)
  "Reply to the message at point."
  (interactive
   (list (button-get (button-at (point)) :value)))
  (telega-chat-msg-reply msg))

(defun telega-msg-edit (msg)
  "Edit message at point."
  (interactive
   (list (button-get (button-at (point)) :value)))
  (telega-chat-msg-edit msg))

(provide 'telega-msg)

;;; telega-msg.el ends here

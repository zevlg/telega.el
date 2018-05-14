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

(defvar telega-msg-button-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "r") 'telega-msg-reply)
    (define-key map (kbd "f") 'telega-msg-forward)
    (define-key map (kbd "e") 'telega-msg-edit)
    (define-key map (kbd "DEL") 'telega-msg-delete)
    map))

(define-button-type 'telega-msg
  :supertype 'telega
  :format #'telega-msg-button--format-error
  'keymap telega-msg-button-keymap
  'action #'telega-msg-reply
  'read-only t)

(defsubst telega-msg--chat (msg)
  "Return chat for the MSG."
  (telega-chat--get (plist-get msg :chat_id)))

(defun telega-msg-sender-name (msg)
  "Short name of the MSG sender."
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

(defun telega-msg-sender-same-p (msg1 msg2 &optional with-via-bot)
  "Return non-nil if MSG1 has same sender as msg2."
  (and (= (plist-get msg1 :sender_user_id)
          (plist-get msg2 :sender_user_id))
       (if with-via-bot
           (string= (telega-msg-via-bot msg1)
                    (telega-msg-via-bot msg2))
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

(defun telega-msg-button--format-error (msg)
  (error "Must set :format explicitly for message buttons."))

(defun telega-msg-button--format-one-line (msg)
  "Format message MSG to be displayed in one line."
  (telega-msg-user-short-name
   "> "
   telega-msg-text-with-props))

(defun telega-msg-sender-status (msg)
  (let ((admins-tl (telega-server--call
                    `(:@type "getChatAdministrators"
                             :chat_id ,(plist-get msg :chat_id)))))
    (if (cl-find (plist-get msg :sender_user_id)
                 (plist-get admins-tl :user_ids)
                 :test #'=)
        " (admin)"
      "")))

(defun telega-msg-via-bot (msg)
  (let ((via_bot_id (plist-get msg :via_bot_user_id)))
    (if (> via_bot_id 0)
        (concat " via @" (plist-get (telega-user--get via_bot_id) :username))
      "")))

(defconst telega-msg-button--format-msg
  `((telega-msg-text-with-props
     :fill left
     :fill-prefix "     " ;(make-string 5 ?\s)
     :fill-column ,telega-chat-fill-column
     :min ,telega-chat-fill-column
;    :face c-nonbreakable-space-face
     :align left)
    (telega-msg-timestamp :align right :min 10)
    "\n"))

(defun telega-msg-button--format-full (msg)
  `(telega-msg-sender-ava-h
    " "
    (telega-msg-sender-name :face telega-chat-user-title)
    (telega-msg-sender-status :face shadow)
    telega-msg-via-bot
    "\n"
    telega-msg-sender-ava-l " "
    ,@telega-msg-button--format-msg))

(defun telega-msg-button--format-same (msg)
  "Fromat message when previous msg is from the same sender."
  `("     "
    ,@telega-msg-button--format-msg))

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

(provide 'telega-msg)

;;; telega-msg.el ends here

;;; telega-ins.el --- Inserters for the telega.

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sat Jul 14 19:06:40 2018
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

;; Inserter is the function that inserts some content.
;; Different inserters accepts different arguments
;; Inserter can examine previously inserted content.
;; Inserter returns non-nil if something was inserted and nil if
;; nothing has been inserted.

;;; Code:
(require 'telega-core)
(require 'telega-customize)

(defmacro telega-ins (&rest args)
  "Insert string in ARGS.
Return `t'."
  `(progn (insert ,@args) t))

(defmacro telega-ins-fmt (fmt &rest args)
  "Insert string formatted by FMT and ARGS.
Return `t'."
  `(telega-ins (format ,fmt ,@args)))

(defmacro telega-ins-propertize (str &rest props)
  `(telega-ins (propertize ,str ,@props)))
(put 'telega-ins-propertize 'lisp-indent-function 1)

(defmacro telega-ins--as-string (&rest body)
  "Execute BODY inserters and return result as a string."
  `(with-temp-buffer
     ,@body
     (buffer-string)))

(defmacro telega-ins--one-lined (&rest body)
  "Execute BODY making insertation one-lined.
It makes one line by replacing all newlines by spaces."
  `(telega-ins
    (replace-regexp-in-string
     "\n" " " (telega-ins--as-string ,@body))))

(defmacro telega-ins--with-attrs (attrs &rest body)
  "Execute inserters applying ATTRS after insertation.
Return `t'."
  (let ((spnt-sym (gensym "pnt"))
        (epnt-sym (gensym "pnt")))
    `(let ((,spnt-sym (point)))
       ,@body
       (let ((,epnt-sym (point)))
         (prog1
             (telega-ins (telega-fmt-eval-attrs
                          (buffer-substring ,spnt-sym ,epnt-sym) ,attrs))
           (delete-region ,spnt-sym ,epnt-sym))))))
(put 'telega-ins--with-attrs 'lisp-indent-function 'defun)

(defmacro telega-ins-prefix (prefix &rest body)
  "In case BODY inserted anything then PREFIX is also inserted before BODY."
  (let ((spnt-sym (gensym "pnt")))
    `(let ((,spnt-sym (point)))
       (when (progn ,@body)
         (save-excursion
           (goto-char ,spnt-sym)
           (insert ,prefix))))))
(put 'telega-ins-prefix 'lisp-indent-function 'defun)

(defun telega-ins--button-format (button)
  "Insert BUTTON using oldstyle `:format' to format the contents."
  (telega-ins
   (telega-fmt-eval (button-get button :format) (button-get button :value))))

(defun telega-ins--actions (actions)
  "Insert chat ACTIONS alist."
  (when actions
    (telega-ins-fmt "TODO: `telega-ins--actions' %S" actions)))


;; Various inserters
(defun telega-ins--filesize (filesize)
  "Insert FILESIZE in human readable format."
  (telega-ins (file-size-human-readable filesize)))

(defun telega-ins--date (date)
  "Insert DATE."
  (unless (zerop date)
    (telega-ins (telega-fmt-timestamp date))))

(defun telega-ins--timestamp-iso8601 (timestamp)
  "Insert TIMESTAMP in ISO8601 format."
  (telega-ins (format-time-string "%FT%T%z" timestamp)))

(defun telega-ins--labeled-text (label text &optional fill-col)
  "Insert TEXT filling it, prefixing first line with LABEL."
  (telega-ins (telega-fmt-labeled-text label text fill-col)))

(defun telega-ins--username (user-id &optional fmt-type)
  "Insert username for user denoted by USER-ID
FMT-TYPE is passed directly to `telega-user--name' (default=`short')."
  (unless (zerop user-id)
    (telega-ins
     (telega-user--name (telega-user--get user-id) (or fmt-type 'short)))))

(defun telega-ins--button-aux (button)
  "Inserter for aux BUTTON."
  (let ((aux-title (button-get button :aux-title))
        (msg (button-get button :value)))
    (telega-ins--with-attrs  (list :max telega-chat-fill-column
                                   :elide t
                                   :face 'telega-chat-prompt)
      (telega-ins "| " aux-title ": ")
      (when (telega-ins--username (plist-get msg :sender_user_id))
        (telega-ins "> "))
      (telega-ins--msg-one-line msg)
      (telega-ins "\n"))))

(defun telega-ins--via-bot (via-bot-user-id)
  "Insert via bot user."
  (unless (zerop via-bot-user-id)
    (telega-ins
     "via "
     (apply 'propertize
            (telega-user--name (telega-user--get via-bot-user-id) 'short)
            (telega-link-props
             'user via-bot-user-id)))))

(defun telega-ins--file-progress (file ptype)
  "Format FILE's progress of PTYPE.
PTYPE is `download' or `upload'."
  (let* ((filesize (plist-get file :size))
         (dsize (if (eq ptype 'download)
                    (telega--tl-get file :local :downloaded_size)
                  (teleg--tl-get file :remote :uploaded_size)))
         (dpart (/ (float dsize) filesize))
         (percents (round (* (/ (float dsize) filesize) 100))))
    (telega-ins-fmt "[%-10s%d%%]"
                    (make-string (round (* dpart 10)) ?\.)
                    (round (* dpart 100)))
    (telega-ins " ")
    (telega-ins
     (apply 'propertize "[Cancel]"
            (telega-link-props
             (intern (format "cancel-%S" ptype))
             (plist-get file :id))))))

(defun telega-ins--msg-special (msg)
  "Format special message MSG.
Special message is one of: `messageContactRegistered',
`messageChatAddMembers', `messageChatJoinByLink',
`messageChatDeleteMember' or `messageChatChangeTitle'."
  (let ((content (plist-get msg :content))
        (sender (telega-user--get (plist-get msg :sender_user_id))))
    (telega-ins--with-attrs (list :min telega-chat-fill-column
                                  :align 'center
                                  :align-symbol "-")
      (telega-ins "--(")
      (cl-case (telega--tl-type content)
        (messageContactRegistered
         (telega-ins (telega-user--name sender) " joined the Telegram"))
        (messageChatAddMembers
         ;; If sender matches
         (let ((user-ids (plist-get content :member_user_ids)))
           (if (and (= 1 (length user-ids))
                    (= (plist-get sender :id) (aref user-ids 0)))
               (telega-ins (telega-user--name sender 'name) " joined the group")
             (telega-ins (telega-user--name sender 'name) " invited "
                         (mapconcat 'telega-user--name
                                    (mapcar 'telega-user--get user-ids)
                                    ", ")))))
        (messageChatJoinByLink
         (telega-ins (telega-user--name sender)
                     " joined the group via invite link"))
        (messageChatDeleteMember
         (telega-ins (telega-user--name
                      (telega-user--get (plist-get content :user_id)))
                     " left the group"))
        (messageChatChangeTitle
         (telega-ins (telega-user--name sender)
                     " renamed group to \"" (plist-get content :title) "\""))
        (telega-ins-fmt
         "<unsupported chat action: %S>" (telega--tl-type content))))
      (telega-ins ")--")

    (telega-ins--with-attrs (list :align 'right :min 10)
      (telega-ins--date (plist-get msg :date)))))

(defun telega-ins--outgoing-status (msg)
  "Insert outgoing status of the message MSG."
  (when (plist-get msg :is_outgoing)
    (let ((sending-state (plist-get (plist-get msg :sending_state) :@type))
          (chat (telega-chat--get (plist-get msg :chat_id))))
      (telega-ins
       (cond ((and (stringp sending-state)
                   (string= sending-state "messageSendingStatePending"))
              telega-symbol-msg-pending)
             ((and (stringp sending-state)
                   (string= sending-state "messageSendingStateFailed"))
              telega-symbol-msg-failed)
             ((>= (plist-get chat :last_read_outbox_message_id)
                  (plist-get msg :id))
              telega-symbol-msg-viewed)
             (t telega-symbol-msg-succeed))))))

(defun telega-ins--text (text)
  "Insert TEXT applying telegram entities."
  (when text
    (telega-ins
     (telega--entities-apply (plist-get text :entities) (plist-get text :text)))))
(defalias 'telega-ins--caption 'telega-ins--text)

(defun telega-ins--web-page (web-page)
  )

(defun telega-ins--content (msg)
  "Insert message's MSG content."
  (let ((content (plist-get msg :content))
        (spoint (point)))
    (cl-case (telega--tl-type content)
      (messageText
       (telega-ins--text (plist-get content :text))
       (telega-ins--web-page (plist-get content :web_page)))
      (messageDocument
       (telega-ins--document (plist-get content :document)))
      (messagePhoto
       (telega-ins--photo (plist-get content :photo) msg))
      (t (telega-ins-fmt "<unsupported message %S>" (telega--tl-type content))
         (telega-ins-prefix "\n"
           (telega-ins--text (plist-get content :caption))))))

  ;; TODO: examine props from SPOINT to POINT finding out files we need to monitor
  (telega-ins--reply-markup (plist-get msg :reply_markup)))

(defun telega-msg-format (msg)
  "Return formatted text for the MSG."
  (concat 
   (let ((content (plist-get msg :content)))
     (cl-case (telega--tl-type content)
       (messageText
        (telega-msg-text msg))
       (messageDocument
        (telega-msg-document msg))
       (messagePhoto
        (telega-msg-photo msg))
       (t (concat (format "<unsupported message %S>" (telega--tl-type content))
                  (telega-prefix "\n" (telega-msg-caption msg))))))

   (telega-msg-reply-markup msg)))

(defun telega-ins--timestamped-msg (msg)
  "Insert message MSG with timestamp and outgoing status."
  (let ((fill-prefix (make-string (- (point) (point-at-bol)) ?\s)))
    (telega-ins--with-attrs (list :fill 'left
                                  :fill-prefix fill-prefix
                                  :fill-column telega-chat-fill-column
                                  :align 'left)
      (telega-ins--content msg)
      (move-to-column telega-chat-fill-column t)
      (unless (eolp)
        (goto-char (point-at-eol)))
      (telega-ins--with-attrs (list :align 'right :min 10)
        (telega-ins--date (plist-get msg :date)))
      (telega-ins--with-attrs (list :face 'telega-msg-status)
        (telega-ins--outgoing-status msg))
      t)))

(defun telega-ins--inline-reply (msg)
  "Insert reply to MSG."
  (let* ((reply-to-msg-id (plist-get msg :reply_to_message_id))
         (reply-msg (unless (zerop reply-to-msg-id)
                      (telega-msg--get (plist-get msg :chat_id)
                                       reply-to-msg-id))))
    (when reply-msg
      (let ((fill-prefix (make-string (- (point) (point-at-bol)) ?\s)))
        (telega-ins--with-attrs (list :max (- telega-chat-fill-column (length fill-prefix))
                                      :elide t
                                      :face 'telega-chat-inline-reply)
          (when (telega-ins--username (plist-get msg :sender_user_id))
            (telega-ins "> "))
          (telega-ins--msg-one-line msg)
          (telega-ins "\n" fill-prefix))))))

(defun telega-ins--channel-msg (msg)
  "Insert MSG received in channel chat."
  (telega-ins--with-attrs (list :face 'telega-chat-user-title)
    (telega-ins (telega-chat--title (telega-msg--chat msg) 'with-username)))
  (telega-ins-prefix " "
    (telega-ins--via-bot (plist-get msg :via_bot_user_id)))
  (telega-ins " " telega-symbol-eye " " (plist-get msg :views))
  (telega-ins-prefix " edited at "
    (telega-ins--date (plist-get msg :edit_date)))
  (telega-ins "\n")
  (telega-ins--inline-reply msg)
  (telega-ins--timestamped-msg msg))

(defun telega-ins--message (msg)
  "Insert message MSG."
  )

(defun telega-ins--message-photo-one-line (msg)
  )

(defun telega-ins--message-one-line (msg)
  "Insert message MSG for one line usage."
  (telega-ins--one-lined
   (case (telega--tl-type (plist-get msg :content))
     (messageText
      (telega-ins--text (telega--tl-get msg :content :text)))
     (messagePhoto
      (telega-ins--message-photo-one-line msg))
     (t (telega-ins--message msg)))))


(defun telega-ins--chat (chat &optional brackets)
  "Insert CHAT into root buffer."
  (let ((title (telega-chat--title chat))
        (unread (plist-get chat :unread_count))
        (mentions (plist-get chat :unread_mention_count))
        (pinned-p (plist-get chat :is_pinned))
        (muted-p (telega-chat--muted-p chat))
        (umwidth 7))
    (telega-ins (or (car brackets) "["))
    (telega-ins
     (truncate-string-to-width
      title (- telega-chat-button-width umwidth) 0
      ?\s telega-symbol-eliding))
    (telega-ins--with-attrs (list :min umwidth
                                  :max umwidth
                                  :elide t
                                  :align 'right)
      (unless (zerop unread)
        (telega-ins-propertize (number-to-string unread)
          'face (if muted-p
                    'telega-muted-count
                  'telega-unmuted-count)))
      (unless (zerop mentions)
        (telega-ins-propertize (format "@%d" mentions)
          'face ' 'telega-mention-count)))
    (telega-ins (or (cdr brackets) "]"))
    (when pinned-p
      (telega-ins telega-symbol-pin))
    (when (eq (telega-chat--type chat 'raw) 'secret)
      (telega-ins telega-symbol-lock)))

  (telega-ins "\n"))

(defun telega-ins--chat-button (chat &optional inserter)
  "Inserter for CHAT as button."
  ;; Insert only visible chat buttons
  ;; See https://github.com/zevlg/telega.el/issues/3
  (let ((visible-p (telega-filter-chats nil (list chat))))
    (when visible-p
      (make-text-button
       (prog1 (point)
         (funcall (or inserter 'telega-ins--chat) chat))
       (point)
       :type 'telega-chat
       :value chat))))

(defun telega-ins--advanced-chat-2nd-line (chat)
  "Second line for chat button advanced inserter."
  (let ((chat-actions (gethash (plist-get chat :id) telega--actions))
        (draft-msg (plist-get chat :draft_message))
        (last-msg (plist-get chat :last_message)))
    (cond (chat-actions
           (telega-debug "CHAT-ACTIONS: %s --> %S" (telega-chat--title chat)
                         chat-actions)
           (telega-ins--with-attrs (list :align 'left
                                         :max telega-filters-fill-column
                                         :elide t)
             (telega-ins--actions chat-actions)))

          (draft-msg
           (let ((inmsg (plist-get draft-msg :input_message_text)))
             (assert (eq (telega--tl-type inmsg) 'inputMessageText)
                     nil "tdlib states that draft must be `inputMessageText'")
             (telega-ins--with-attrs (list :align 'left
                                           :max telega-filters-fill-column
                                           :elide t)
               (telega-ins telega-symbol-draft ": ")
               (telega-ins--one-lined
                (telega-ins--text (plist-get inmsg :text))))))

          (last-msg
           (telega-ins--with-attrs (list :align 'left
                                         :min telega-filters-fill-column
                                         :max telega-filters-fill-column
                                         :elide t)
             (when (telega-ins--username (plist-get last-msg :sender_user_id))
               (telega-ins ": "))
             (telega-ins--message-one-line last-msg))

           (telega-ins--with-attrs (list :align 'right :min 10)
             (telega-ins--date (plist-get last-msg :date)))
           (telega-ins--with-attrs (list :face 'telega-msg-status)
             (telega-ins--outgoing-status last-msg))
           ))
    ))

(defun telega-ins--advanced-chat (chat)
  "Insert CHAT into root buffer using advanced formatting."
  ;; 2 lines formatting
  (telega-ins--chat chat)
  (telega-ins--with-attrs (list :face 'telega-root-advanced-second-line)
    (telega-ins "  ")
    (telega-ins--advanced-chat-2nd-line chat)
    (telega-ins "\n")))

(defun telega-ins--advanced-chat-button (chat)
  "Advanced button inserter for the CHAT.
Could be used as value for `telega-inserter-chat-button'."
  (telega-ins--chat-button chat 'telega-ins--advanced-chat))

(provide 'telega-ins)

;;; telega-ins.el ends here

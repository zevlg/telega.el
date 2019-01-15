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

(defsubst telega-ins (&rest args)
  "Insert all strings in ARGS.
Return non-nil if something has been inserted."
  (< (prog1 (point) (apply 'insert args)) (point)))

(defmacro telega-ins-fmt (fmt &rest args)
  "Insert string formatted by FMT and ARGS.
Return `t'."
  (declare (indent 1))
  `(telega-ins (format ,fmt ,@args)))

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
  (declare (indent 1))
  `(telega-ins
    (telega-fmt-eval-attrs (telega-ins--as-string ,@body) ,attrs)))

(defmacro telega-ins--labeled (label fill-col &rest body)
  "Execute BODY filling it to FILL-COLL, prefixing first line with LABEL."
  (declare (indent 2))
  (let ((prfx-sym (gensym "prefix")))
    `(progn
       (telega-ins ,label)
       (telega-ins--with-attrs
         (list :fill 'left
               :fill-prefix (make-string (- (point) (point-at-bol)) ?\s)
               :fill-column ,fill-col)
         ,@body))))

(defmacro telega-ins--text-buton (props &rest body)
  "Execute BODY creating text button with PROPS."
  (declare (indent 1))
  `(apply 'make-text-button (prog1 (point) ,@body) (point)
          ,props))
  
(defmacro telega-ins--with-props (props &rest body)
  "Execute inserters applying PROPS after insertation.
Return what BODY returns."
  (let ((spnt-sym (gensym "pnt")))
    `(let ((,spnt-sym (point)))
       (prog1
           (progn ,@body)
         (add-text-properties ,spnt-sym (point) ,props)))))
(put 'telega-ins--with-props 'lisp-indent-function 'defun)

(defmacro telega-ins-prefix (prefix &rest body)
  "In case BODY inserted anything then PREFIX is also inserted before BODY."
  (declare (indent 1))
  (let ((spnt-sym (gensym "pnt")))
    `(let ((,spnt-sym (point)))
       (when (progn ,@body)
         (save-excursion
           (goto-char ,spnt-sym)
           (telega-ins ,prefix))))))


;; Various inserters
(defun telega-ins--actions (actions)
  "Insert chat ACTIONS alist."
  (when actions
    ;; NOTE: Display only last action
    (let* ((user (telega-user--get (caar actions)))
           (action (cdar actions)))
      (telega-ins (telega-user--name user 'short) " ")
      (telega-ins
       (propertize (concat "is " (substring (plist-get action :@type) 10))
                   'face 'shadow)))))

(defun telega-ins--filesize (filesize)
  "Insert FILESIZE in human readable format."
  (telega-ins (file-size-human-readable filesize)))

(defun telega-ins--date (timestamp)
  "Insert DATE.
Format is:
- HH:MM      if today
- Mon/Tue/.. if on this week
- DD.MM.YY   otherwise"
  (let* ((dtime (decode-time timestamp))
         (current-ts (time-to-seconds (current-time)))
         (ctime (decode-time current-ts))
         (today00 (telega--time-at00 current-ts ctime)))
    (if (> timestamp today00)
        (telega-ins-fmt "%02d:%02d" (nth 2 dtime) (nth 1 dtime))

      (let* ((week-day (nth 6 ctime))
             (mdays (+ week-day
                       (- (if (< week-day telega-week-start-day) 7 0)
                          telega-week-start-day)))
             (week-start00 (telega--time-at00
                            (- current-ts (* mdays 24 3600)))))
        (if (> timestamp week-start00)
            (telega-ins (nth (nth 6 dtime) telega-week-day-names))

          (telega-ins-fmt "%02d.%02d.%02d"
            (nth 3 dtime) (nth 4 dtime) (- (nth 5 dtime) 2000))))
      )))

(defun telega-ins--date-iso8601 (timestamp &rest args)
  "Insert TIMESTAMP in ISO8601 format."
  (apply 'telega-ins (format-time-string "%FT%T%z" timestamp) args))

(defun telega-ins--date-full (timestamp &rest args)
  "Insert TIMESTAMP in full format - DAY MONTH YEAR."
  (apply 'telega-ins (format-time-string "%d %B %Y" timestamp) args))

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
            (telega-link-props 'user via-bot-user-id)))))

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
     (telega--entities-apply
      (plist-get text :entities) (plist-get text :text)))))
(defalias 'telega-ins--caption 'telega-ins--text)

(defun telega-ins--document (doc)
  "Insert document DOC."
  )

(defun telega-ins--web-page (web-page)
  )

(defun telega-msg-special-p (msg)
  "Return non-nil if MSG is special."
  (memq (telega--tl-type (plist-get msg :content))
        (list 'messageContactRegistered 'messageChatAddMembers
              'messageChatJoinByLink 'messageChatDeleteMember
              'messageChatChangeTitle 'messageSupergroupChatCreate
              'messageBasicGroupChatCreate 'messageCustomServiceAction)))

(defun telega-ins--special (msg)
  "Insert special message MSG.
Special messages are determined with `telega-msg-special-p'."
  (let* ((content (plist-get msg :content))
         (sender-id (plist-get msg :sender_user_id))
         (sender (unless (zerop sender-id) (telega-user--get sender-id))))
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
      (messageSupergroupChatCreate
       (telega-ins (if (plist-get msg :is_channel_post)
                       "Channel" "Supergroup"))
       (telega-ins " \"" (plist-get content :title) "\" created"))
      (messageBasicGroupChatCreate
       (telega-ins "Group \"" (plist-get content :title) "\" created"))
      (messageCustomServiceAction
       (telega-ins (plist-get content :text)))
      (telega-ins-fmt
          "<unsupported special message: %S>" (telega--tl-type content))))
  (telega-ins ")--"))

(defun telega-ins--content (msg)
  "Insert message's MSG content."
  (let ((content (plist-get msg :content)))
    (cl-case (telega--tl-type content)
      (messageText
       (telega-ins--text (plist-get content :text))
       (telega-ins--web-page (plist-get content :web_page)))
      (messageDocument
       (telega-ins--document (plist-get content :document)))
      (messagePhoto
       (telega-ins--photo (plist-get content :photo) msg))
      ;; special message
      ((messageContactRegistered messageChatAddMembers
        messageChatJoinByLink messageChatDeleteMember
        messageChatChangeTitle messageSupergroupChatCreate
        messageBasicGroupChatCreate messageCustomServiceAction)
       (telega-ins--special msg))
      (t (telega-ins-fmt "<TODO: %S>"
                         (telega--tl-type content))))

    (telega-ins-prefix "\n"
      (telega-ins--text (plist-get content :caption))))
  )

(defun telega-ins--timestamped-msg (msg)
  "Insert message MSG with timestamp and outgoing status."
  (let ((fill-prefix (make-string (- (point) (point-at-bol)) ?\s)))
    (telega-ins--with-attrs (list :fill 'left
                                  :fill-prefix fill-prefix
                                  :fill-column telega-chat-fill-column
                                  :align 'left)
      (let ((spoint (point)))
        (if (telega-msg-special-p msg)
            (telega-ins--with-attrs (list :min telega-chat-fill-column
                                          :align 'center
                                          :align-symbol "-")
              (telega-ins--content msg))
          (telega-ins--content msg))
        ;; TODO: examine props from SPOINT to POINT finding out files we
        ;; need to monitor
        )
      (telega-ins--reply-markup (plist-get msg :reply_markup))

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
    (unless (zerop (plist-get msg :edit_date))
      (telega-ins--date (plist-get msg :edit_date))))
  (telega-ins "\n")
  (telega-ins--inline-reply msg)
  (telega-ins--timestamped-msg msg))

(defun telega-ins--message (msg)
  "Insert message MSG."
  )

(defun telega-ins--content-one-line (msg)
  "Insert message's MSG content for one line usage."
  (telega-ins--one-lined
   (let ((content (plist-get msg :content)))
     (case (telega--tl-type content)
       (messageText
        (telega-ins--text (plist-get content :text)))
       (messagePhoto
        (telega-ins telega-symbol-photo " ")
        (or (telega-ins--text (plist-get content :caption))
            ;; I18N: lng_in_dlg_photo or lng_attach_photo
            (telega-ins (propertize "Photo" 'face 'shadow))))
       (messageDocument
        (telega-ins telega-symbol-document " ")
        (or (telega-ins (telega--tl-get content :document :file_name))
            (telega-ins--text (plist-get content :caption))
            (telega-ins (propertize "Document" 'face 'shadow))))
       (messageLocation
        (telega-ins telega-symbol-location " ")
        (let ((loc (plist-get content :location)))
          (telega-ins-fmt "%fN, %fE"
            (plist-get loc :latitude) (plist-get loc :longitude)))

        ;; NOTE: in case of unexpired live location show last update
        ;; time and expiration period
        (let ((live-period (plist-get content :live_period)))
          (unless (zerop live-period)
            (telega-ins " " (propertize "Live" 'face 'shadow))
            (unless (zerop (plist-get content :expires_in))
              (let* ((current-ts (truncate (float-time)))
                     (since (if (zerop (plist-get msg :edit_date))
                                (plist-get msg :date)
                              (plist-get msg :edit_date)))
                     (live-for (- (+ since live-period) current-ts)))
                (when (> live-for 0)
                  (telega-ins-fmt " for %s"
                    (telega-duration-human-readable live-for))
                  (telega-ins-fmt " (updated %s ago)"
                    (telega-duration-human-readable
                     (- current-ts since)))))))))
       (messageAnimation
        (or (telega-ins--text (plist-get content :caption))
            (telega-ins (propertize "GIF" 'face 'shadow))))
       (messageVideo
        (telega-ins telega-symbol-video " ")
        (or (telega-ins--text (plist-get content :caption))
            (telega-ins (propertize "Video" 'face 'shadow)))
        (telega-ins-fmt " (%s)"
          (telega-duration-human-readable
           (telega--tl-get content :video :duration))))
       (messageGame
        (telega-ins telega-symbol-game " ")
        (or (telega-ins (telega--tl-get content :game :title))
            (telega-ins (telega--tl-get content :game :short_name))
            (telega-ins (propertize "Game" 'face 'shadow))))
       (messageSticker
        (telega-ins (telega--tl-get content :sticker :emoji))
        (telega-ins " " (propertize "Sticker" 'face 'shadow)))
       (messageCall
        (telega-ins telega-symbol-phone " ")
        (let* ((reason (telega--tl-type (plist-get content :discard_reason)))
               (label (cond ((plist-get msg :is_outgoing)
                             (if (eq reason 'callDiscardReasonMissed)
                                 "Cancelled call" ;; I18N: lng_call_cancelled
                               "Outgoing call")) ;; I18N: lng_call_outgoing
                            ((eq reason 'callDiscardReasonMissed)
                             "Missed call") ;; I18N: lng_call_missed
                            ((eq reason 'callDiscardReasonDeclined)
                             "Declined call") ;; I18N: lng_call_declined
                            (t
                             "Incoming call")))) ;; I18N: lng_call_incoming
          (telega-ins (propertize label 'face 'shadow)))
        (telega-ins-fmt " (%s)"
          (telega-duration-human-readable
           (plist-get content :duration))))
       (messageVoiceNote
        ;; I18N: lng_in_dlg_audio
        (telega-ins (propertize "Voice message" 'face 'shadow))
        (telega-ins-fmt " (%s)"
          (telega-duration-human-readable
           (telega--tl-get content :voice_note :duration))))
       (messageVideoNote
        ;; I18N: lng_in_dlg_video_message
        (telega-ins (propertize "Video message" 'face 'shadow))
        (telega-ins-fmt " (%s)"
          (telega-duration-human-readable
           (telega--tl-get content :video_note :duration))))
       (messageContact
        ;; I18N: lng_in_dlg_contact
        (telega-ins (propertize "Contact" 'face 'shadow))
        (telega-ins-fmt " %s (%s %s)"
          (telega--tl-get content :contact :phone_number)
          (telega--tl-get content :contact :first_name)
          (telega--tl-get content :contact :last_name)))
       (messageInvoice
        (telega-ins (propertize "Invoice" 'face 'shadow))
        (telega-ins-prefix " "
          (telega-ins (plist-get content :title))))
       (t (telega-ins--content msg))))))


;; Inserter for custom filter button
(defun telega-ins--filter (custom)
  "Inserter for the CUSTOM filter button in root buffer."
  (let* ((name (car custom))
         (chats (telega-filter-chats (cdr custom) telega--filtered-chats))
         (nchats (length chats))
         (unread (apply #'+ (mapcar (telega--tl-prop :unread_count) chats)))
         (mentions (apply #'+ (mapcar
                               (telega--tl-prop :unread_mention_count) chats)))
         (umwidth 7)
         (title-width (- telega-filter-button-width umwidth)))
    (telega-ins "[")
    (telega-ins--with-attrs (list :min title-width
                                  :max title-width
                                  :elide t
                                  :align 'left)
      (telega-ins-fmt "%d:%s" nchats name))
    (telega-ins--with-attrs (list :min umwidth
                                  :max umwidth
                                  :elide t
                                  :align 'right)
      (unless (zerop unread)
        (telega-ins-fmt "%d" unread))
      (unless (zerop mentions)
        (telega-ins-fmt "@%d" mentions)))
    (telega-ins "]")))


(defun telega-ins--chat (chat &optional brackets)
  "Inserter for CHAT button in root buffer.
Return t."
  (let ((title (telega-chat--title chat))
        (unread (plist-get chat :unread_count))
        (mentions (plist-get chat :unread_mention_count))
        (pinned-p (plist-get chat :is_pinned))
        (muted-p (telega-chat--muted-p chat))
        (chat-info (telega-chat--info chat))
        (umwidth 7))
    (when (plist-get chat-info :is_verified)
      (setq title (concat title telega-symbol-verified)))

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
        (telega-ins--with-attrs (list :face (if muted-p
                                                'telega-muted-count
                                              'telega-unmuted-count))
          (telega-ins (number-to-string unread))))
      (unless (zerop mentions)
        (telega-ins--with-attrs (list :face 'telega-mention-count)
          (telega-ins-fmt "@%d" mentions)))
      ;; Mark for chats marked as unread
      (when (and (zerop unread) (zerop mentions)
                 (plist-get chat :is_marked_as_unread))
        (telega-ins--with-attrs (list :face (if muted-p
                                                'telega-muted-count
                                              'telega-unmuted-count))
          (telega-ins telega-symbol-unread)))
      )
    (telega-ins (or (cdr brackets) "]"))
    (when pinned-p
      (telega-ins telega-symbol-pin))
    (when (eq (telega-chat--type chat 'raw) 'secret)
      (telega-ins telega-symbol-lock))
    t))

(defun telega-ins--chat-full (chat)
  "Full status inserter for CHAT button in root buffer."
  (telega-ins--chat chat)
  (telega-ins "  ")

  ;; And the status
  (let ((max-width (- telega-root-fill-column (current-column)))
        (actions (gethash (plist-get chat :id) telega--actions))
        (call (telega-voip--by-user-id (plist-get chat :id)))
        (draft-msg (plist-get chat :draft_message))
        (last-msg (plist-get chat :last_message)))
    (cond (call
           (telega-ins telega-symbol-phone " ")
           (telega-ins-fmt "%s Call (%s)"
             (if (plist-get call :is_outgoing) "Outgoing" "Incoming")
             (substring (telega--tl-get call :state :@type) 9)))

           (actions
           (telega-debug "CHAT-ACTIONS: %s --> %S"
                         (telega-chat--title chat) actions)
           (telega-ins--with-attrs (list :align 'left
                                         :max max-width
                                         :elide t)
             (telega-ins--actions actions)))

          (draft-msg
           (let ((inmsg (plist-get draft-msg :input_message_text)))
             (assert (eq (telega--tl-type inmsg) 'inputMessageText)
                     nil "tdlib states that draft must be `inputMessageText'")
             (telega-ins--with-attrs (list :align 'left
                                           :max max-width
                                           :elide t)
               (telega-ins telega-symbol-draft ": ")
               (telega-ins--one-lined
                (telega-ins--text (plist-get inmsg :text))))))

          (last-msg
           ;; NOTE: date - 10 chars, outgoing-status - 1 char
           (telega-ins--with-attrs (list :align 'left
                                         :min (- max-width 10 1)
                                         :max (- max-width 10 1)
                                         :elide t)
             ;; NOTE: Do not show username for:
             ;;  - Saved Messages
             ;;  - If sent by user in private chat
             ;;  - Special messages
             (unless (or (eq (plist-get last-msg :sender_user_id)
                             (plist-get chat :id))
                         (telega-msg-special-p last-msg))
               (when (telega-ins--username (plist-get last-msg :sender_user_id))
                 (telega-ins ": ")))

             (telega-ins--content-one-line last-msg))

           (telega-ins--with-attrs (list :align 'right :min 10)
             (telega-ins--date (plist-get last-msg :date)))
           (telega-ins--with-attrs (list :face 'telega-msg-status)
             (telega-ins--outgoing-status last-msg))
           ))
    )
  t)

(provide 'telega-ins)

;;; telega-ins.el ends here

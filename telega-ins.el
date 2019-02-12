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

(defun telega-ins (&rest args)
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

(defmacro telega-ins--with-face (face &rest body)
  "Execute BODY highlighting result with FACE."
  (declare (indent 1))
  `(telega-ins--with-attrs (list :face ,face)
     ,@body))

(defmacro telega-ins--column (column fill-col &rest body)
  "Execute BODY at COLUMN filling to FILL-COLL.
If COLUMN is nil or less then current column, then current column is used."
  (declare (indent 2))
  (let ((colsym (gensym "col"))
        (curcol (gensym "curcol")))
    `(let ((,colsym ,column)
           (,curcol (telega-current-column)))
       (when (or (null ,colsym) (< ,colsym ,curcol))
         (setq ,colsym ,curcol))

       (telega-ins (make-string (- ,colsym ,curcol) ?\s))
;       (move-to-column ,colsym t)
       (telega-ins--with-attrs
           (list :fill 'left
                 :fill-prefix (make-string ,colsym ?\s)
                 :fill-column ,fill-col)
         ,@body))))

(defmacro telega-ins--labeled (label fill-col &rest body)
  "Execute BODY filling it to FILL-COLL, prefixing first line with LABEL."
  (declare (indent 2))
  `(progn
     (telega-ins ,label)
     (telega-ins--column nil ,fill-col
       ,@body)))

(defun telega-ins--button (label &rest props)
  "Insert pressable button labeled with LABEL.
If custom face is specified in PROPS, then
`telega-button--sensor-func' is not set as sensor function."
  (declare (indent 1))
  (unless (plist-get props 'face)
    ;; XXX inclose LABEL with shrink version of spaces, so button
    ;; width will be char aligned
    (let* ((box-width (- (or (plist-get (face-attribute 'telega-button :box)
                                        :line-width)
                             0)))
           (space `(space (,(- (frame-char-width) box-width)))))
      (setq label (concat (propertize " " 'display space)
                          label
                          (propertize " " 'display space))))
    (setq props (plist-put props 'face 'telega-button))
    (setq props (plist-put props 'cursor-sensor-functions
                           '(telega-button--sensor-func))))
  (unless (plist-get props 'action)
    (setq props (plist-put props 'action
                           (lambda (button)
                             (funcall (button-get button :action)
                                      (button-get button :value))))))
  (button-at (apply 'insert-text-button label props)))

(defmacro telega-ins--raw-button (props &rest body)
  "Execute BODY creating text button with PROPS."
  (declare (indent 1))
  `(button-at (apply 'make-text-button (prog1 (point) ,@body) (point)
                     ,props)))

(defmacro telega-ins--with-props (props &rest body)
  "Execute inserters applying PROPS after insertation.
Return what BODY returns."
  (declare (indent 1))
  (let ((spnt-sym (gensym "pnt")))
    `(let ((,spnt-sym (point)))
       (prog1
           (progn ,@body)
         (add-text-properties ,spnt-sym (point) ,props)))))

(defmacro telega-ins-prefix (prefix &rest body)
  "In case BODY inserted anything then PREFIX is also inserted before BODY."
  (declare (indent 1))
  (let ((spnt-sym (gensym "pnt")))
    `(let ((,spnt-sym (point)))
       (when (progn ,@body)
         (save-excursion
           (goto-char ,spnt-sym)
           (telega-ins ,prefix))))))

(defun telega-ins--image (img &optional slice-num props)
  "Insert image IMG generated by telega.
Uses internal `:telega-text' to keep correct column.
If SLICE-NUM is specified, then insert N's."
  (let* ((imgsz (image-size img t))
         (slice (when slice-num
                  (let* ((slice-h (frame-char-height))
                         (slices (/ (cdr imgsz) slice-h)))
                    (when (>= slice-num slices)
                      (error "Can't insert %d slice, image has only %d slices"
                             slice-num slices))
                    (list 0 (* slice-num slice-h) (car imgsz) slice-h)))))
    (telega-ins--with-props
        (nconc `(display ,(if slice
                              (list (cons 'slice slice) img)
                            img)
                         rear-nonsticky (display))
               props)
      (telega-ins
       (or (plist-get img :telega-text)
           (make-string (telega-chars-in-width (car imgsz)) ?X))))))

(defun telega-ins--image-slices (image &optional props)
  "Insert sliced IMAGE at current column."
  (let ((img-slices (ceiling (cdr (image-size image)))))
    (telega-ins--column (current-column) nil
      (cl-dotimes (slice-num img-slices)
        (telega-ins--image image slice-num props)
        (unless (= slice-num (1- img-slices))
          (telega-ins "\n"))))))


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

(defun telega-ins--chat-member-status (status)
  "Format chat member STATUS."
  (unless (eq (telega--tl-type status) 'chatMemberStatusMember)
    (telega-ins (downcase (substring (plist-get status :@type) 16)))))

(defun telega-ins--user-status (user)
  "Insert USER's online status."
  ;; TODO: check online's `:expires'
  (let* ((status (telega--tl-type (plist-get user :status)))
         (online-dur (- (telega-time-seconds)
                        (or (plist-get user :telega-last-online) 0))))
    (telega-ins--with-face (if (eq status 'userStatusOnline)
                               'telega-user-online-status
                             'telega-user-non-online-status)
      (telega-ins
       (cond ((eq status 'userStatusOnline)
              ;; I18N: lng_status_online
              "online")
             ((< online-dur 60)
              ;; I18N: lng_status_lastseen_now
              "last seen just now")
             ((< online-dur (* 3 24 60 60))
              (format "last seen in %s"
                      (telega-duration-human-readable online-dur 1)))
             ((eq status 'userStatusRecently)
              ;; I18N: lng_status_recently
              "last seen recently")
             (t
              ;; TODO: other cases
              (symbol-name status)))))))

(defun telega-ins--user (user &optional member)
  "Insert USER, aligning multiple lines at current column."
  (let* ((joined (plist-get member :joined_chat_date))
         (avatar-svg (telega-user-avatar-svg user))
         (off-column (telega-current-column)))
    (telega-ins--image avatar-svg 0)
    (telega-ins (telega-user--name user))
    (when (and member
               (telega-ins-prefix " ("
                 (telega-ins--chat-member-status
                  (plist-get member :status))))
      (telega-ins ")"))
    (telega-ins "\n")
    (telega-ins (make-string off-column ?\s))
    (telega-ins--image avatar-svg 1)
    (telega-ins--user-status user)
    ;; TODO: for member insert join date
    ;;  (unless (zerop joined)
    ;;    (concat " joined at " (telega-fmt-timestamp joined))))))
    ))

(defun telega-ins--chat-member (member)
  "Formatting for the chat MEMBER.
Return COLUMN at which user name is inserted."
  (telega-ins--user
   (telega-user--get (plist-get member :user_id)) member))

(defun telega-ins--chat-members (members)
  "Insert chat MEMBERS list."
  (let ((last-member (unless (zerop (length members))
                       (aref members (1- (length members)))))
        (delim-col 5))
    (seq-doseq (member members)
      (telega-ins " ")
      (telega-button--insert 'telega-member member)

      ;; Insert the delimiter
      (unless (eq member last-member)
        (telega-ins "\n")
        ;; NOTE: to apply `height' property \n must be included
        (telega-ins--with-props
            '(face default display ((space-width 2) (height 0.5)))
          (telega-ins--column delim-col nil
            (telega-ins (make-string 30 ?─) "\n")))))
    (telega-ins "\n")))

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
                  (telega--tl-get file :remote :uploaded_size)))
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
          (chat (telega-chat-get (plist-get msg :chat_id))))
      (telega-ins--with-face 'telega-msg-outgoing-status
        (telega-ins
         (cond ((and (stringp sending-state)
                     (string= sending-state "messageSendingStatePending"))
                telega-symbol-pending)
               ((and (stringp sending-state)
                     (string= sending-state "messageSendingStateFailed"))
                telega-symbol-failed)
               ((>= (plist-get chat :last_read_outbox_message_id)
                    (plist-get msg :id))
                telega-symbol-heavy-checkmark)
               (t telega-symbol-checkmark)))))))

(defun telega-ins--text (text)
  "Insert TEXT applying telegram entities."
  (when text
    (telega-ins
     (telega--entities-apply
      (plist-get text :entities) (plist-get text :text)))))
(defalias 'telega-ins--caption 'telega-ins--text)

(defun telega-ins--photo (photo)
  (telega-ins "TODO: PHOTO")
  )

(defun telega-ins--document (doc)
  "Insert document DOC."
  )

(defun telega-ins--web-page (web-page)
  "Insert WEB-PAGE.
Return `non-nil' if WEB-PAGE has been inserted."
  (let ((sitename (plist-get web-page :site_name))
        (title (plist-get web-page :title))
        (desc (plist-get web-page :description))
        (instant-view-p (plist-get web-page :has_instant_view))
        (photo (plist-get web-page :photo))
        (width (- telega-chat-fill-column 10)))
    (when web-page
      (telega-ins--with-attrs (list :fill-prefix telega-symbol-vertical-bar
                                    :fill-column width
                                    :fill 'left)
        (telega-ins telega-symbol-vertical-bar)
        (when (telega-ins--with-face 'telega-webpage-sitename
                (telega-ins sitename))
          (telega-ins "\n"))
        (when (telega-ins--with-face 'telega-webpage-title
                (telega-ins title))
          (telega-ins "\n"))
        (when (telega-ins desc)
          (telega-ins "\n"))

       ;; (when photo
       ;;   (concat "\n" telega-symbol-vertical-bar
       ;;           (telega-photo-format (plist-get web-page :photo))))
       (cl-case (intern (plist-get web-page :type))
         (photo
          ;; no-op, already displayed above
          )
         (article
          ;; nothing to display
          )
         (t (telega-ins "<unsupported webPage:"
                        (plist-get web-page :type) ">")))
       )

       (when instant-view-p
         (telega-ins--button
             (concat "  " telega-symbol-thunder " INSTANT VIEW  ")
           'action 'telega-msg-button--iv-action)
         (telega-ins "\n"))

       ;; Remove trailing newline, if any
       (when (= (char-before) ?\n)
         (delete-char -1))
       t)))

(defun telega-ins--location (location)
  "Inserter for the LOCATION."
  (telega-ins telega-symbol-location " ")
  (telega-ins-fmt "%fN, %fE"
    (plist-get location :latitude) (plist-get location :longitude)))

(defun telega-ins--contact (contact)
  "Inserter for the CONTACT."
  (telega-ins telega-symbol-contact " ")
  (when (telega-ins (plist-get contact :first_name))
    (telega-ins " "))
  (when (telega-ins (plist-get contact :last_name))
    (telega-ins " "))
  (telega-ins "(" (plist-get contact :phone_number) ")"))

(defun telega-ins--input-file (document &optional attach-symbol)
  "Insert input file."
  (telega-ins (or attach-symbol telega-symbol-attachment) " ")
  (cl-ecase (telega--tl-type document)
    (inputFileLocal
     (telega-ins (abbreviate-file-name (plist-get document :path))))
    (inputFileId
     (let ((preview (get-text-property
                     0 'telega-preview (plist-get document :@type))))
       (when preview
         (telega-ins--image preview)
         (telega-ins " ")))
     (telega-ins-fmt "Id: %d" (plist-get document :id))
     )
    (inputFileRemote
     ;; TODO: getRemoteFile
     (telega-ins-fmt "Remote: %s" (plist-get document :id))
     )
    ))

(defun telega-msg-special-p (msg)
  "Return non-nil if MSG is special."
  (memq (telega--tl-type (plist-get msg :content))
        (list 'messageContactRegistered 'messageChatAddMembers
              'messageChatJoinByLink 'messageChatDeleteMember
              'messageChatChangeTitle 'messageSupergroupChatCreate
              'messageBasicGroupChatCreate 'messageCustomServiceAction
              'messageChatSetTtl 'messageExpiredPhoto
              'messageChatChangePhoto 'messageChatUpgradeFrom)))

(defun telega-ins--special (msg)
  "Insert special message MSG.
Special messages are determined with `telega-msg-special-p'."
  (telega-ins "--(")
  (let* ((content (plist-get msg :content))
         (sender-id (plist-get msg :sender_user_id))
         (sender (unless (zerop sender-id) (telega-user--get sender-id))))
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
       (let ((user (telega-user--get (plist-get content :user_id))))
         (if (eq sender user)
             (telega-ins (telega-user--name user 'name) " left the group")
           (telega-ins (telega-user--name sender 'name)
                       " removed "
                       (telega-user--name user 'name)))))

      (messageChatChangeTitle
       (telega-ins "Renamed to \"" (plist-get content :title) "\"")
       (when sender
         (telega-ins " by " (telega-user--name sender 'short))))

      (messageSupergroupChatCreate
       (telega-ins (if (plist-get msg :is_channel_post)
                       "Channel" "Supergroup"))
       (telega-ins " \"" (plist-get content :title) "\" created"))
      (messageBasicGroupChatCreate
       (telega-ins "Group \"" (plist-get content :title) "\" created"))
      (messageCustomServiceAction
       (telega-ins (plist-get content :text)))
      (messageChatSetTtl
       (telega-ins-fmt "messages TTL set to %s"
         (telega-duration-human-readable (plist-get content :ttl))))
      (messageExpiredPhoto
       ;; I18N: lng_ttl_photo_expired
       (telega-ins "Photo has expired"))
      (messageChatChangePhoto
       (telega-ins "Group photo updated"))
      (messageChatUpgradeFrom
       (telega-ins (telega-user--name sender 'short)
                   " upgraded the group to supergroup"))
      (t (telega-ins-fmt "<unsupported special message: %S>"
           (telega--tl-type content)))))
  (telega-ins ")--"))

(defun telega-ins--content (msg)
  "Insert message's MSG content."
  (let ((content (plist-get msg :content)))
    (pcase (telega--tl-type content)
      ('messageText
       (telega-ins--text (plist-get content :text))
       (telega-ins-prefix "\n"
         (telega-ins--web-page (plist-get content :web_page))))
      ('messageDocument
       (telega-ins--document (plist-get content :document)))
      ('messagePhoto
       (telega-ins--photo (plist-get content :photo)))
      ('messageSticker
       (telega-ins--sticker (plist-get content :sticker)))
      ;; special message
      ((guard (telega-msg-special-p msg))
       (telega-ins--special msg))
      (_ (telega-ins-fmt "<TODO: %S>"
                         (telega--tl-type content))))

    (telega-ins-prefix "\n"
      (telega-ins--text (plist-get content :caption))))
  )

(defun telega-ins--reply-markup (reply-markup)
  "Insert reply markup."
  (when reply-markup
    (telega-ins-fmt "REPLY-MARKUP: %S" reply-markup)))

(defun telega-ins--content-markup-date-status (msg)
  "Insert message MSG with timestamp and outgoing status."
  (let ((fill-prefix (make-string (telega-current-column) ?\s)))
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
      (telega-ins-prefix "\n"
        (telega-ins--reply-markup (plist-get msg :reply_markup))))

    ;; Date/status starts at `telega-chat-fill-column' column
    (let ((slen (- telega-chat-fill-column (telega-current-column))))
      (when (< slen 0) (setq slen 1))
      (telega-ins (make-string slen ?\s)))

    (telega-ins--with-attrs (list :align 'right :min 10)
      (telega-ins--date (plist-get msg :date)))
    (telega-ins--outgoing-status msg)
    t))

(defun telega-ins--aux-msg-inline (title msg face &optional with-username)
  "Insert REPLY-MSG as one-line."
  (when msg
    (telega-ins--with-attrs  (list :max (- telega-chat-fill-column
                                           (telega-current-column))
                                   :elide t
                                   :face face)
      (telega-ins "| " title ": ")
      (when (and with-username
                 (telega-ins--username (plist-get msg :sender_user_id)))
        (telega-ins "> "))
      (telega-ins--content-one-line msg)
      (telega-ins "\n"))))

(defun telega-ins--aux-reply-inline (reply-msg &optional face)
  (telega-ins--aux-msg-inline
   "Reply" reply-msg (or face 'telega-chat-prompt) 'with-username))

(defun telega-ins--aux-edit-inline (edit-msg)
  (telega-ins--aux-msg-inline
   "Edit" edit-msg 'telega-chat-prompt))

(defun telega-ins--aux-fwd-inline (fwd-msg)
  (telega-ins--aux-msg-inline
   "Forward" fwd-msg 'telega-chat-prompt 'with-username))

(defun telega-ins--channel-msg (msg)
  "Insert MSG received in channel chat."
  (let ((chat (telega-msg-chat msg)))
    (telega-ins--with-attrs
        (list :face (if telega-chat-rainbow-users
                        (list :foreground
                              (if (eq (frame-parameter nil 'background-mode) 'light)
                                  (nth 2 (telega-chat-uaprop chat :color))
                                (nth 0 (telega-chat-uaprop chat :color))))
                      'telega-chat-user-title))
      (telega-ins (telega-chat-title chat 'with-username))))
  (telega-ins-prefix " "
    (let ((sign (plist-get msg :author_signature)))
      (unless (string-empty-p sign)
        (telega-ins "--" sign))))
  (telega-ins-prefix " "
    (telega-ins--via-bot (plist-get msg :via_bot_user_id)))
  (telega-ins-fmt " %s %d" telega-symbol-eye (plist-get msg :views))
  (telega-ins-prefix " edited at "
    (unless (zerop (plist-get msg :edit_date))
      (telega-ins--date (plist-get msg :edit_date))))
  (telega-ins "\n")
  (telega-ins--aux-reply-inline
   (telega-msg-reply-msg msg) 'telega-chat-inline-reply)
  (telega-ins--content-markup-date-status msg)
  )

(defun telega-ins--mesage-title ()
  )

(defun telega-ins--message-header (msg &optional no-avatar)
  "Insert message's MSG header, everything except for message content.
If NO-AVATAR is specified, then do not insert avatar."
  (if (plist-get msg :is_channel_post)
      ;; Chat title and author signature for channel posts
      (let ((chat (telega-msg-chat msg)))
        (telega-ins--with-attrs
            (list :face (if telega-chat-rainbow-users
                            (list :foreground
                                  (if (eq (frame-parameter nil 'background-mode) 'light)
                                      (nth 2 (telega-chat-uaprop chat :color))
                                    (nth 0 (telega-chat-uaprop chat :color))))
                          'telega-chat-user-title))
          (telega-ins (telega-chat-title chat 'with-username)))
        (telega-ins-prefix " "
          (let ((sign (plist-get msg :author_signature)))
            (unless (string-empty-p sign)
              (telega-ins "--" sign)))))

    ;; Sender for ordinary messages
    )

  (let ((views (plist-get msg :views)))
    (telega-ins-fmt " %s %d" telega-symbol-eye (plist-get msg :views)))
  (telega-ins-prefix " edited at "
    (unless (zerop (plist-get msg :edit_date))
      (telega-ins--date (plist-get msg :edit_date))))
  (telega-ins "\n")
  )

(defun telega-ins--message (msg)
  "Insert message MSG."
  ;; (let ((chat (telega-msg-chat msg))
  ;;       (channel-post-p (plist-get msg :is_channel_post)))

  (cond ((plist-get msg :is_channel_post)
         (telega-ins--channel-msg msg))
        (t
         (telega-ins-fmt "MSG: %S " (plist-get msg :id))
         (telega-ins--chat-msg-one-line
          (telega-msg-chat msg) msg
          (- telega-chat-fill-column (current-column)))
         ))
  )

(defun telega-ins--input-content-one-line (imc)
  "Insert input message's MSG content for one line usage."
  (telega-ins--one-lined
   (cl-case (telega--tl-type imc)
     (inputMessageLocation
      (telega-ins--location (plist-get imc :location))
      (when (> (or (plist-get imc :live_period) 0) 0)
        (telega-ins " Live for: "
                    (telega-duration-human-readable
                     (plist-get imc :live_period)))))
     (inputMessageContact
      (telega-ins--contact (plist-get imc :contact)))
     (inputMessageDocument
      (telega-ins--input-file (plist-get imc :document)))
     (inputMessagePhoto
      (telega-ins--input-file (plist-get imc :photo) telega-symbol-photo))
     (inputMessageVideo
      (telega-ins--input-file (plist-get imc :video) telega-symbol-video))
     (t
      (telega-ins-fmt "<TODO: %S>" (telega--tl-type imc)))
     )))

(defun telega-ins--content-one-line (msg)
  "Insert message's MSG content for one line usage."
  (telega-ins--one-lined
   (let ((content (plist-get msg :content)))
     (cl-case (telega--tl-type content)
       (messageText
        (telega-ins--text (plist-get content :text)))
       (messagePhoto
        (telega-ins telega-symbol-photo " ")
        (or (telega-ins--text (plist-get content :caption))
            ;; I18N: lng_in_dlg_photo or lng_attach_photo
            (telega-ins (propertize "Photo" 'face 'shadow))))
       (messageDocument
        (telega-ins telega-symbol-attachment " ")
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
         (telega-filters--inhibit-list '(has-order))
         (chats (telega-filter-chats (cdr custom) telega--filtered-chats))
         (active-p (not (null chats)))
         (nchats (length chats))
         (unread (apply #'+ (mapcar (telega--tl-prop :unread_count) chats)))
         (mentions (apply #'+ (mapcar
                               (telega--tl-prop :unread_mention_count) chats)))
         (umwidth 7)
         (title-width (- telega-filter-button-width umwidth)))
    (telega-ins--with-props (list 'inactive (not active-p)
                                  'face (if active-p
                                            'telega-filter-button-active
                                          'telega-filter-button-inactive)
                                  'action (if active-p
                                              'telega-filter-button--action
                                            'ignore))
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
      (telega-ins "]"))))


(defun telega-ins--chat-msg-one-line (chat msg max-width)
  "Insert message for the chat button usage."
  (cl-assert (> max-width 11))
  ;; NOTE: date - 10 chars, outgoing-status - 1 char
  (telega-ins--with-attrs (list :align 'left
                                :min (- max-width 10 1)
                                :max (- max-width 10 1)
                                :elide t)
    ;; NOTE: Do not show username for:
    ;;  - Saved Messages
    ;;  - If sent by user in private/secret chat
    ;;  - Special messages
    (unless (or (eq (plist-get msg :sender_user_id)
                    (plist-get chat :id))
                (telega-chat--secret-p chat)
                (telega-msg-special-p msg))
      (when (telega-ins--username (plist-get msg :sender_user_id))
        (telega-ins ": ")))

    (telega-ins--content-one-line msg))

  (telega-ins--with-attrs (list :align 'right :min 10)
    (telega-ins--date (plist-get msg :date)))
  (telega-ins--outgoing-status msg)
  )

(defun telega-ins--chat (chat &optional brackets)
  "Inserter for CHAT button in root buffer.
Return t."
  (let ((title (telega-chat-title chat))
        (unread (plist-get chat :unread_count))
        (mentions (plist-get chat :unread_mention_count))
        (pinned-p (plist-get chat :is_pinned))
        (custom-order (telega-chat-uaprop chat :order))
        (muted-p (telega-chat--muted-p chat))
        (chat-info (telega-chat--info chat))
        (chat-ava (plist-get chat :telega-avatar-1)))
    (when (plist-get chat-info :is_verified)
      (setq title (concat title telega-symbol-verified)))
    (when (telega-chat--secret-p chat)
      (setq title (propertize title 'face 'telega-secret-title)))

    (telega-ins (or (car brackets) "["))

    ;; 1) First we format unread@mentions as string to find out its
    ;;    final length
    ;; 2) Then we insert the title as wide as possible
    ;; 3) Then insert formatted UNREAD@MENTIONS string
    (let* ((umstring (telega-ins--as-string
                      (unless (zerop unread)
                        (telega-ins--with-face (if muted-p
                                                   'telega-muted-count
                                                 'telega-unmuted-count)
                          (telega-ins (number-to-string unread))))
                      (unless (zerop mentions)
                        (telega-ins--with-face 'telega-mention-count
                          (telega-ins-fmt "@%d" mentions)))
                      ;; Mark for chats marked as unread
                      (when (and (zerop unread) (zerop mentions)
                                 (plist-get chat :is_marked_as_unread))
                        (telega-ins--with-face (if muted-p
                                                   'telega-muted-count
                                                 'telega-unmuted-count)
                          (telega-ins telega-symbol-unread)))
                      ;; For chats searched by
                      ;; `telega--searchPublicChats' insert number of
                      ;; members in the group
                      ;; Basicgroups converted to supergroups
                      ;; does not have username and have "0" order
                      (when (string= "0" (plist-get chat :order))
                        (when (telega-chat-username chat)
                          (telega-ins--with-face 'telega-username
                            (telega-ins "@" (telega-chat-username chat))))
                        (telega-ins--with-face (if muted-p
                                                   'telega-muted-count
                                                 'telega-unmuted-count)
                          (cl-case (telega-chat--type chat 'no-interpret)
                            (basicgroup
                             (telega-ins telega-symbol-contact
                                         (number-to-string
                                          (plist-get chat-info :member_count))))
                            (supergroup
                             (telega-ins telega-symbol-contact
                                         (number-to-string
                                          (plist-get
                                           (telega--full-info chat-info)
                                           :member_count)))))))
                      ))
           (title-width (- telega-chat-button-width (string-width umstring))))
      (telega-ins--with-attrs (list :min title-width
                                    :max title-width
                                    :align 'left
                                    :elide t)
        (when chat-ava
          (telega-ins--image chat-ava))
        (telega-ins title))
      (telega-ins umstring))

    (telega-ins (or (cadr brackets) "]"))
    (when pinned-p
      (telega-ins telega-symbol-pin))
    (when custom-order
      (telega-ins
       (if (< (string-to-number custom-order)
              (string-to-number (plist-get chat :order)))
           (car telega-symbol-custom-order)
         (cdr telega-symbol-custom-order))))
    (when (telega-chat--secret-p chat)
      (telega-ins telega-symbol-lock))
    t))

(defun telega-ins--chat-full (chat)
  "Full status inserter for CHAT button in root buffer."
  (let ((brackets (cdr (seq-find (lambda (bspec)
                                   (telega-filter-chats
                                    (car bspec) (list chat)))
                                 telega-chat-button-brackets))))
    (telega-ins--chat chat brackets))
  (telega-ins "  ")

  ;; And the status
  (let ((max-width (- telega-root-fill-column (current-column)))
        (actions (gethash (plist-get chat :id) telega--actions))
        (call (telega-voip--by-user-id (plist-get chat :id)))
        (draft-msg (plist-get chat :draft_message))
        (last-msg (plist-get chat :last_message))
        (chat-info (telega-chat--info chat)))
    (cond ((and (telega-chat--secret-p chat)
                (memq (telega--tl-type (plist-get chat-info :state))
                      '(secretChatStatePending secretChatStateClosed)))
           ;; Status of the secret chat
           (telega-ins (propertize
                        (substring (telega--tl-get chat-info :state :@type) 15)
                        'face 'shadow)))

          (call
           (let ((state (plist-get call :state)))
             (telega-ins telega-symbol-phone " ")
             (telega-ins-fmt "%s Call (%s)"
               (if (plist-get call :is_outgoing) "Outgoing" "Incoming")
               (substring (plist-get state :@type) 9))

             (when (eq (telega--tl-type state) 'callStateReady)
               (telega-ins " " (telega-voip--call-emojis call)))
             ))

           (actions
           (telega-debug "CHAT-ACTIONS: %s --> %S"
                         (telega-chat-title chat) actions)
           (telega-ins--with-attrs (list :align 'left
                                         :max max-width
                                         :elide t)
             (telega-ins--actions actions)))

          (draft-msg
           (let ((inmsg (plist-get draft-msg :input_message_text)))
             (cl-assert (eq (telega--tl-type inmsg) 'inputMessageText) nil
                        "tdlib states that draft must be `inputMessageText'")
             (telega-ins--with-attrs (list :align 'left
                                           :max max-width
                                           :elide t)
               (telega-ins telega-symbol-draft ": ")
               (telega-ins--one-lined
                (telega-ins--text (plist-get inmsg :text))))))

          (last-msg
           (telega-ins--chat-msg-one-line chat last-msg max-width))

          ((and (telega-chat--secret-p chat)
                (eq (telega--tl-type (plist-get chat-info :state))
                    'secretChatStateReady))
           ;; Status of the secret chat
           (telega-ins (propertize
                        (substring (telega--tl-get chat-info :state :@type) 15)
                        'face 'shadow)))
          ))
  t)

(defun telega-ins--root-msg (msg)
  "Inserter for message MSG shown in `telega-root-messages--ewoc'."
  (let ((chat (telega-msg-chat msg))
        (telega-chat-button-width (* 2 (/ telega-chat-button-width 3))))
    (telega-ins--chat chat)
    (telega-ins " ")
    (let ((max-width (- telega-root-fill-column (current-column))))
      (telega-ins--chat-msg-one-line chat msg max-width))))

(provide 'telega-ins)

;;; telega-ins.el ends here

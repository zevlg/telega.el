;;; telega-ins.el --- Inserters for the telega  -*- lexical-binding:t -*-

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
(require 'format-spec)
(require 'transient)

(require 'telega-core)
(require 'telega-tdlib)
(require 'telega-inline)
(require 'telega-folders)
(require 'telega-topic)
(require 'telega-customize)

;; telega-chat.el depends on telega-ins.el
(declare-function telega-msg-delete0 "telega-chat" (msg &optional revoke))
(declare-function telega-msg-redisplay "telega-chat" (msg &optional node))
(declare-function telega-chat-get "telega-chat" (chat-id &optional offline-p))
(declare-function telega-chat-color "telega-chat" (chat))
(declare-function telega-chat-title "telega-chat" (chat &optional no-badges))
(declare-function telega-chat--goto-msg "telega-chat" (chat msg-id &optional highlight callback))
(declare-function telega-describe-chat "telega-chat" (chat))
(declare-function telega-chat-secret-p "telega-chat" (chat))
(declare-function telega-chat-user "telega-chat" (chat))
(declare-function telega-chat-muted-p "telega-chat" (chat))
(declare-function telega-chat-channel-p "telega-chat" (chat))
(declare-function telega-chat--info "telega-chat" (chat))
(declare-function telega-chat-delete "telega-chat" (chat &optional leave-p))
(declare-function telega-chat-admin-get "telega-chat" (chat user))

(declare-function telega-topic-button-action "telega-root" (chat-topic))

(defun telega-ins--text-button (label &rest props)
  "Insert pressable button labeled with LABEL."
  (declare (indent 1))
  (unless (plist-get props 'action)
    (setq props (plist-put props 'action 'telega-button--action)))
  (telega-ins--raw-button props
    (telega-ins label)))

(defun telega-box-button--endings-func (label)
  "Function to generate endings for the button with LABEL."
  (cond
   ((member label (list " " "  " "✕" telega-symbol-heavy-checkmark))
    ;; NOTE: " " is arguable, incorrect rendering for @chessy_bot
    (cons "" ""))

   ((or (string-equal label "2×")
        (string-equal label telega-symbol-button-close))
    ;; Special case for voice/video notes speedup button
    ;; and for `telega-symbol-button-close'
    (let ((half-space (propertize " " 'display '(space :width 0.5))))
      (cons half-space half-space)))

   (t
    ;; NOTE: In newer Emacs we can use cons as `:line-width` in
    ;; telega-box-button face, so width of the button is not increased
    ;; in contrast with using single negative number for `:line-width'
    ;; However is older Emacs this is not implemented, see
    ;; https://t.me/emacs_telega/22129
    ;;
    ;; We handle both cases, for `:line-width' as cons and as negative
    ;; number

    ;; XXX inclose LABEL with shrink version of spaces, so button
    ;; width will be char aligned

    ;; NOTE: non-breakable space is used, so if line is feeded at the
    ;; beginning of button, it won't loose its leading space
    (let* ((line-width (plist-get (face-attribute 'telega-box-button :box)
                                  :line-width))
           (box-width (if (consp line-width)
                          (car line-width)
                        (- (or line-width 0))))
           (space (when (> box-width 0)
                    `(space (,(- (telega-chars-xwidth 1) box-width)))))
           (end (if space
                    (propertize telega-symbol-nbsp 'display space)
                  telega-symbol-nbsp)))
      (cons end end)))))

(defun telega-box-button--sensor-func (_window oldpos dir)
  "Function to be used in `cursor-sensor-functions' text property.
Activates box button if cursor enter, deactivates if leaves."
  (let ((inhibit-read-only t)
        (button-region (telega--region-with-cursor-sensor
                        (if (eq dir 'entered) (point) oldpos))))
    (when button-region
      (put-text-property (car button-region) (cdr button-region)
                         'face (if (eq dir 'entered)
                                   'telega-box-button-active
                                 'telega-box-button))
      (when (eq dir 'entered)
        (telega-button--help-echo (car button-region)))
      )))

(defun telega-ins--box-button (label &rest props)
  "Insert box button."
  (declare (indent 1))
  (let ((ends (if (functionp telega-box-button-endings)
                  (funcall telega-box-button-endings label)
                telega-box-button-endings)))
    (setq label (concat (or (car ends) "[")
                        label
                        (or (cdr ends) "]"))))
  (setq props (plist-put props 'face 'telega-box-button))
  (setq props (plist-put props 'cursor-sensor-functions
                         '(telega-box-button--sensor-func)))

  ;; NOTE: buttons must not be breakable by filling logic, so we use
  ;; non-breakable spaces instead of regular
  (apply #'telega-ins--text-button
         (replace-regexp-in-string " " telega-symbol-nbsp label) props))

(defun telega-ins--image (img &optional slice-num &rest props)
  "Insert image IMG generated by telega.
Uses internal `:telega-text' to keep correct column.
If SLICE-NUM is specified, then insert single slice.
SLICE-NUM can be a list in form (SLICE-NUM SLICE-Y SLICE-H).

Special property `:no-display-if' is supported in PROPS to
ommit image display if value is for this property is non-nil."
  ;; NOTE: IMG might be nil if `telega-use-images' is nil
  ;; See https://github.com/zevlg/telega.el/issues/274
  (if (or (not img) (not telega-use-images)
          (not (display-graphic-p (telega-x-frame))))
      (telega-ins (or (plist-get props :telega-text)
                      (telega-image--telega-text img slice-num)
                      "<IMAGE>"))

    ;; NOTE: do not check SLICE-NUM
    (let ((slice (cond ((numberp slice-num)
                        (list 0 (telega-chars-xheight slice-num)
                              1.0 (telega-chars-xheight 1)))
                       ((listp slice-num)
                        (prog1
                            (list 0 (nth 1 slice-num)
                                  1.0 (nth 2 slice-num))
                          (setq slice-num (nth 0 slice-num))))
                       (slice-num
                        (error "Invalid slice-num: %S" slice-num)))))
      (telega-ins--with-props
          (nconc (list 'rear-nonsticky '(display))
                 (unless (plist-get props :no-display-if)
                   (list 'display
                         (if slice
                             (list (cons 'slice slice) img)
                           img)))
                 props)
        (telega-ins
         (or (plist-get props :telega-text)
             (telega-image--telega-text img slice-num)
             (make-string (ceiling (car (image-size img nil (telega-x-frame))))
                          ?X)))))))

(defun telega-image--adjust-slice (start end slice-hoff slice-height)
  "Adjust slice of the image display at START END region."
  (when-let ((dsp (get-text-property start 'display)))
    (when (listp dsp)
      (let ((old-slice (nth 0 dsp))
            (img (nth 1 dsp)))
        (unless (and (eq (nth 2 old-slice) slice-hoff)
                     (eq (nth 4 old-slice) slice-height))
          ;; Slice parameters has been changed, need to update
          (set-text-properties start end
                               `(display ((slice 0 ,slice-hoff 1.0 ,slice-height)
                                          ,img))))))))

(defun telega-ins--image-slices (image &optional props slice-func)
  "Insert sliced IMAGE at current column.
PROPS - additional image properties.
SLICE-FUNC - function called after inserting slice. Called with
single argument - slice number, starting from 0."
  (declare (indent 2))
  (if (or (not telega-use-images)
          (not (display-graphic-p (telega-x-frame))))
      (telega-ins "<IMAGE>")

    ;; NOTE: it is okay to call `image-size' now, because image will
    ;; be read anyway, and `image-size' puts image to the image cache
    (let ((nslices (or (plist-get (cdr image) :telega-nslices)
                       (round (cdr (image-size image nil (telega-x-frame)))))))
      (dotimes (slice-num nslices)
        (apply #'telega-ins--image image slice-num props)
        (when slice-func
          (funcall slice-func slice-num))
        ;; NOTE: do not insert newline for the last slice
        (when (< (1+ slice-num) nslices)
          (telega-ins--with-props (list 'line-height t)
            (telega-ins "\n")))))))

(defun telega-ins--actions (actions)
  "Insert chat ACTIONS alist."
  (when actions
    ;; NOTE: Display only first action
    (let* ((first-action (car actions))
           (sender (telega-msg-sender (car first-action)))
           (action (cdr first-action))
           (sender-title (telega-ins--as-string
                          (telega-ins--msg-sender sender :with-avatar-p t)))
           (start (point)))
      (prog1
      (telega-ins--with-face 'telega-shadow
        (telega-ins (telega-symbol 'typing))
        (cl-case (telega--tl-type action)
          (chatActionTyping
           (let ((more-senders
                  (delq nil (mapcar (lambda (spec)
                                      (when (eq (telega--tl-type (cdr spec))
                                                'chatActionTyping)
                                        (telega-msg-sender (car spec))))
                                    (cdr actions)))))
             (cond ((= 0 (length more-senders))
                    (telega-ins-i18n "lng_user_typing"
                      :user sender-title))
                   ((= 1 (length more-senders))
                    (telega-ins-i18n "lng_users_typing"
                      :user sender-title
                      :second_user (telega-ins--as-string
                                    (telega-ins--msg-sender (car more-senders)
                                      :with-avatar-p t))))
                   (t
                    (telega-ins-i18n "lng_many_typing"
                      :count (1+ (length more-senders)))))))
          (chatActionRecordingVideoNote
           (telega-ins-i18n "lng_user_action_record_round"
             :user sender-title))
          (chatActionUploadingVideoNote
           (telega-ins-i18n "lng_user_action_upload_round"
             :user sender-title))
          (chatActionRecordingVoiceNote
           (telega-ins-i18n "lng_user_action_record_audio"
             :user sender-title))
          (chatActionUploadingVoiceNote
           (telega-ins-i18n "lng_user_action_upload_audio"
             :user sender-title))
          (chatActionWatchingAnimations
           (telega-ins sender-title
                       (telega-i18n "lng_user_action_watching_animations"
                         :emoji (telega-tl-str action :emoji))))
          (t
           (telega-ins sender-title "is "
                       (substring (plist-get action :@type)
                                  (eval-when-compile
                                    (length "chatAction"))))))
        (when-let ((progress (plist-get action :progress)))
          (telega-ins-fmt " %d%%" progress))
        )
      (replace-regexp-in-region " " telega-symbol-nbsp start (point))
      ))))

(defun telega-ins--filesize (filesize)
  "Insert FILESIZE in human readable format."
  (telega-ins (file-size-human-readable filesize)))

(defun telega-ins--date (timestamp &optional fmt-type)
  "Insert TIMESTAMP.
Use date format from `telega-date-format-alist' corresponding to FMT-TYPE.
By default FMT-TYPE is determined by TIMESTAMP value.
FMT-TYPE can be a string, directly specifying time format string."
  (unless fmt-type
    (let* ((current-ts (telega-time-seconds))
           (ctime (decode-time current-ts))
           (today00 (telega--time-at00 current-ts ctime)))
      (if (and (> timestamp today00)
               (< timestamp (+ today00 (* 24 60 60))))
          (setq fmt-type 'today)

        (let* ((week-day (nth 6 ctime))
               (mdays (+ week-day
                         (- (if (< week-day telega-week-start-day) 7 0)
                            telega-week-start-day)))
               (week-start00 (telega--time-at00
                              (- current-ts (* mdays 24 3600)))))
          (if (and (> timestamp week-start00)
                   (< timestamp (+ week-start00 (* 7 24 60 60))))
              (setq fmt-type 'this-week)
            (setq fmt-type 'old))))))

  (telega-ins
   (format-time-string (or (and (stringp fmt-type) fmt-type)
                           (cdr (assq fmt-type telega-date-format-alist))
                           "%FT%T%z")
                       timestamp)))

(defun telega-ins--birthdate (birthdate &optional with-years-old-p)
  "Inserter for the BIRTHDATE tl struct.
If WITH-YEARS-OLD-P is specified, insert years old as well."
  (let ((bd-day (plist-get birthdate :day))
        (bd-month (plist-get birthdate :month))
        (bd-year (plist-get birthdate :year)))
    (if with-years-old-p
        (let ((nowdate (decode-time (telega-time-seconds))))
          (telega-ins-i18n "lng_info_birthday_years"
            :date (telega-ins--as-string
                   (telega-ins--birthdate birthdate))
            :count (- (decoded-time-year nowdate)
                      bd-year
                      (if (or (> bd-month (decoded-time-month nowdate))
                              (and (= bd-month (decoded-time-month nowdate))
                                   (> bd-day (decoded-time-day nowdate))))
                          1
                        0))))

      (let* ((bd-decoded (list 0 0 0 bd-day bd-month bd-year))
             (bd-timestamp
              (round (time-to-seconds (apply #'encode-time bd-decoded)))))
        (telega-ins--date bd-timestamp 'date-long)))))

(defun telega-ins--date-relative (timestamp)
  "Insert relative date for the timestamp."
  (let* ((dtime (decode-time timestamp))
         (today00 (telega--time-at00 (telega-time-seconds)))
         (tomorrow00 (+ today00 (* 24 3600)))
         (yesterday00 (- today00 (* 24 3600)))
         (formatted-time (format "%02d:%02d" (nth 2 dtime) (nth 1 dtime))))
    (cond ((and (> timestamp yesterday00)
                (< timestamp today00))
           (telega-ins-i18n "lng_mediaview_yesterday"
             :time formatted-time))
          ((and (> timestamp today00)
                (< timestamp tomorrow00))
           (telega-ins-i18n "lng_mediaview_today"
             :time formatted-time))
          (t
           (telega-ins-i18n "lng_mediaview_date_time"
             :date (telega-ins--as-string
                    (telega-ins--date timestamp 'date-long))
             :time formatted-time)))
    ))

(cl-defun telega-ins--msg-sender (msg-sender &key
                                             with-avatar-p
                                             with-username-p
                                             with-brackets-p
                                             (with-badges-p t)
                                             (with-title-faces-p t)
                                             (trail-delim " ")
                                             trail-inserter)
  "Insert message's sender title.
If WITH-AVATAR-P is 2, then insert 2 lines version of an avatar."
  (declare (indent 1))
  (let* ((chat-p (telega-chat-p msg-sender))
         (title (if chat-p
                    (telega-chat-title msg-sender (not with-badges-p))
                  (cl-assert (telega-user-p msg-sender))
                  (telega-user-title msg-sender 'full-name (not with-badges-p))))
         (title-faces (when with-title-faces-p
                        (telega-msg-sender-title-faces msg-sender)))
         (brackets (when with-brackets-p
                     (telega-msg-sender-brackets msg-sender))))
    (when (and with-avatar-p
               (if chat-p
                   telega-chat-show-avatars
                 telega-user-show-avatars))
       (if (eq with-avatar-p 2)
           (telega-ins--image
            (telega-msg-sender-avatar-image msg-sender) 0)
         (telega-ins--image
          (telega-msg-sender-avatar-image-one-line msg-sender))))
    (when brackets
      (telega-ins (car brackets)))

    (telega-ins--with-face title-faces
      (telega-ins title))

    (when with-username-p
      (when-let ((username (telega-msg-sender-username msg-sender 'with-@)))
        (telega-ins--with-face 'telega-shadow
          (telega-ins telega-symbol-nbsp "•" telega-symbol-nbsp))
        (telega-ins--with-face (if (facep with-username-p)
                                   with-username-p
                                 title-faces)
          (telega-ins username))))

    ;; Trailer before closing bracket
    (when trail-inserter
      (telega-ins-prefix trail-delim
        (funcall trail-inserter msg-sender)))

    (when brackets
      (telega-ins (cadr brackets)))

    (when (eq with-avatar-p 2)
      (telega-ins "\n")
      (telega-ins--image (telega-msg-sender-avatar-image msg-sender) 1))
    t))

(defun telega-ins--chat-member-status (status)
  "Format chat member STATUS."
  (telega-ins
   (cl-case (telega--tl-type status)
     (chatMemberStatusAdministrator
      (or (telega-tl-str status :custom_title)
          (telega-i18n "lng_admin_badge")))
     (chatMemberStatusCreator
      (or (telega-tl-str status :custom_title)
          (telega-i18n "lng_owner_badge")))
     (chatMemberStatusMember
      nil)
     (t
      (downcase (substring (plist-get status :@type) 16))))))

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
              (telega-i18n "lng_status_online"))
             ((< online-dur 60)
              (telega-i18n "lng_status_lastseen_now"))
             ((< online-dur (* 60 60))
              (telega-i18n "lng_status_lastseen_minutes"
                :count (/ online-dur 60)))
             ((< online-dur (* 24 60 60))
              (telega-i18n "lng_status_lastseen_hours"
                :count (/ online-dur (* 60 60))))
             ((< online-dur (* 2 24 60 60))
              (telega-i18n "lng_status_lastseen_yesterday"
                :time (telega-ins--as-string
                       (telega-ins--date (- online-dur (* 24 60 60))))))
             ((eq status 'userStatusRecently)
              (telega-i18n "lng_status_recently"))
             (t
              ;; TODO: other cases
              (symbol-name status)))))))

(defun telega-ins--user-relationship (user)
  "Insert relationship with USER.
🚹←🚹 - if user is contact.
🚹↔🚹 - if user is mutual contact."
  (let ((contact-p (plist-get user :is_contact))
        (mutual-contact-p (plist-get user :is_mutual_contact)))
    (when (or contact-p mutual-contact-p)
      (telega-ins (propertize "🚹" 'face 'telega-shadow)
                  (if mutual-contact-p "↔" "←")
                  (propertize "🚹" 'face 'bold)))))

(defun telega-ins--user-nearby-distance (user)
  "Insert distance to the USER nearby."
  (when-let* ((chat (telega-user-chat user))
              (distance (telega-chat-nearby-distance chat)))
    (telega-ins--with-face 'telega-shadow
      (telega-ins (telega-distance-human-readable distance) " away"))))

(defun telega-ins--user (user &optional member show-phone-p)
  "Insert USER, aligning multiple lines at current column.
MEMBER specifies corresponding \"ChatMember\" object.
If SHOW-PHONE-P is non-nil, then show USER's phone number."
  (let ((avatar (telega-msg-sender-avatar-image user))
        (off-column (telega-current-column)))
    (telega-ins--image avatar 0
                       :no-display-if (not telega-user-show-avatars))
    (telega-ins--msg-sender user
      :with-username-p 'telega-username)
    (telega-ins--with-face 'telega-shadow
      (when (and member
                 (telega-ins-prefix " ("
                   (telega-ins--chat-member-status
                    (plist-get member :status))))
        (telega-ins ")")))

    (when show-phone-p
      (when-let ((phone-number (telega-tl-str user :phone_number)))
        (telega-ins--with-face 'telega-shadow
          (telega-ins telega-symbol-nbsp "•" telega-symbol-nbsp))
        (telega-ins "+" phone-number)))

    ;; Insert (him)in<-->out(me) relationship
    (when (and telega-user-show-relationship
               (not (telega-me-p user)))
      (telega-ins " ")
      (telega-ins--user-relationship user))

    (telega-ins "\n")
    (telega-ins (make-string off-column ?\s))
    (telega-ins--image avatar 1
                       :no-display-if (not telega-user-show-avatars))
    ;; Setup `off-column' for "invited by" string
    (setq off-column (telega-current-column))
    (telega-ins--user-status user)

    (when-let ((join-date (plist-get member :joined_chat_date)))
      (unless (zerop join-date)
        (telega-ins ", joined ")
        (telega-ins--date join-date)))

    (telega-ins-prefix ", "
      (telega-ins--user-nearby-distance user))

    (when-let* ((inviter-id (plist-get member :inviter_user_id))
                (inviter-user (unless (zerop inviter-id)
                                (telega-user-get inviter-id 'local))))
      (telega-ins "\n")
      (telega-ins (make-string off-column ?\s))
      (telega-ins "invited by ")
      (telega-ins--raw-button (telega-link-props 'user inviter-id 'type 'telega)
        (telega-ins--msg-sender inviter-user :with-avatar-p t)))
    t))

(defun telega-ins--chat-member (member)
  "Formatting for the chat MEMBER.
Return COLUMN at which user name is inserted."
  (let ((sender (telega-msg-sender (plist-get member :member_id))))
    (if (telega-user-p sender)
        (telega-ins--user sender member)
      ;; TODO: support for chat as member
      )))

(defun telega-ins--user-list (users &optional button-type)
  "Insert list of the USERS using BUTTON-TYPE.
By default BUTTON-TYPE is `telega-user'."
  (while users
    (telega-button--insert (or button-type 'telega-user) (car users))

    (setq users (cdr users))
    (when users
      (telega-ins "\n")
      ;; NOTE: to apply `height' property \n must be included
      (telega-ins--with-props
          '(face telega-shadow display ((space-width 2) (height 0.5)))
        (telega-ins--column 4 nil
          (telega-ins (make-string 30 ?─) "\n"))))
    t))

(defun telega-ins--chat-members (members)
  "Insert chat MEMBERS list."
  (telega-ins--user-list members 'telega-member)
  (telega-ins "\n"))

(defun telega-ins-progress-bar (progress duration nbars &optional p-char e-char)
  "Insert progress bar for PROGRESS at overall DURATION.
Use NBARS characters for progress bar.
P-CHAR - progress char, default is \".\"
         could be a cons cell, where care is filling char, and cdr is
         trailing char. For example (?= . ?>) to draw ====> progress
         bar.
E-CHAR - empty char, default is non-break space."
  (let ((pbars (ceiling
                (* nbars (/ (or progress 0)
                            (if (zerop duration) 0.1 duration))))))
    (when (> pbars nbars) (setq pbars nbars)) ; check for overflows
    (telega-ins (if (consp p-char)
                    (cond ((= 0 pbars) "")
                          ((= 1 pbars) (char-to-string (cdr p-char)))
                          (t (concat (make-string (1- pbars) (car p-char))
                                     (char-to-string (cdr p-char)))))
                  (make-string pbars (or p-char ?\.)))
                (make-string (- nbars pbars) (or e-char 160)))))

(defun telega-ins--file-progress (msg file)
  "Insert Upload/Download status for the document."
  ;; Downloading status:
  ;;   /link/to-file         if file has been downloaded
  ;;   [Download]            if no local copy
  ;;   [...   20%] [Cancel]  if download in progress
  (cond ((telega-file--uploading-p file)
         (let* ((progress (telega-file--uploading-progress file))
                (progress-100 (round (* progress 100)))
                (nbsp-char 160))
           (telega-ins "[")
           (telega-ins-progress-bar
            progress 1.0 10 telega-symbol-upload-progress nbsp-char)
           (when (< progress-100 10)
             (telega-ins nbsp-char))
           (telega-ins-fmt "%d%%]%c" progress-100 nbsp-char)
           (telega-ins--box-button (telega-i18n "lng_context_cancel_upload")
             'action (lambda (_ignored)
                       (telega-msg-delete0 msg)))))

        ((telega-file--downloading-p file)
         (let* ((progress (telega-file--downloading-progress file))
                (progress-100 (round (* progress 100)))
                (nbsp-char 160))
           (telega-ins "[")
           (telega-ins-progress-bar
            progress 1.0 10 telega-symbol-download-progress nbsp-char)
           (when (< progress-100 10)
             (telega-ins nbsp-char))
           (telega-ins-fmt "%d%%]%c" progress-100 nbsp-char)
           (telega-ins--box-button (telega-i18n "lng_context_cancel_download")
             'action (lambda (_ignored)
                       (telega--cancelDownloadFile file)))))

        ((not (telega-file--downloaded-p file))
         (let* ((progress (telega-file--downloading-progress file))
                (progress-100 (round (* progress 100)))
                (nbsp-char 160))
           (when (> progress 0)
             (telega-ins "[")
             (telega-ins-progress-bar progress 1.0 10 (cons ?= ?⏸) nbsp-char)
             (when (< progress-100 10)
               (telega-ins nbsp-char))
             (telega-ins-fmt "%d%%]%c" progress-100 nbsp-char))
           (telega-ins--box-button
            (if (> progress 0)
                (telega-i18n "telega_media_resume_download")
              (telega-i18n "lng_media_download"))
            'action (lambda (_ignored)
                       (telega-file--download file 32
                         (lambda (_fileignored)
                           (telega-msg-redisplay msg)))))))))

(defun telega-ins--outgoing-status (msg)
  "Insert outgoing status of the message MSG."
  (when (plist-get msg :is_outgoing)
    (let ((sending-state (plist-get (plist-get msg :sending_state) :@type))
          (chat (telega-chat-get (plist-get msg :chat_id))))
      (telega-ins
       (cond ((plist-get msg :scheduling_state)
              (telega-symbol 'alarm))
             ((and (stringp sending-state)
                   (string= sending-state "messageSendingStatePending"))
              (telega-symbol 'pending))
             ((and (stringp sending-state)
                   (string= sending-state "messageSendingStateFailed"))
              (telega-symbol 'failed))
             ((>= (plist-get chat :last_read_outbox_message_id)
                  (plist-get msg :id))
              (telega-symbol 'heavy-checkmark))
             (t
              (telega-symbol 'checkmark)))))))

(defun telega-ins--fmt-text (fmt-text &optional for-msg)
  "Insert formatted TEXT applying telegram entities.
If AS-MARKDOWN is non-nil, then instead of applying faces, apply
markdown syntax to the TEXT.
FOR-MSG is message we are inserting TEXT for."
  (when fmt-text
    (telega-ins
     (telega--desurrogate-apply (telega--fmt-text-faces fmt-text for-msg)))))

(defun telega-ins--photo (photo &optional msg limits show-details)
  "Inserter for the PHOTO.
SHOW-DETAILS - non-nil to show photo details."
  (let* ((hr (telega-photo--highres photo))
         (hr-file (telega-file--renew hr :photo))
         (show-progress
          (and (telega-file--downloading-p hr-file) msg)))
    ;; Show photo details and download progress for highres thumbnail
    (when (or show-details show-progress)
      ;; Monitor downloading progress for the HR-FILE
      (when show-progress
        (telega-file--download hr-file 32
          (lambda (_fileignored)
            (telega-msg-redisplay msg))))

      (telega-ins (telega-symbol 'photo) " ")
      (telega-ins-fmt "(%dx%d %s)"
        (plist-get hr :width) (plist-get hr :height)
        (file-size-human-readable (telega-file--size hr-file)))
      (when-let ((tl-ttl (plist-get msg :self_destruct_type)))
        (telega-ins ", ")
        (telega-ins--self-destruct-type tl-ttl 'short))
      (let ((album-id (plist-get msg :media_album_id)))
        (unless (telega-zerop album-id)
          (telega-ins--with-face 'telega-shadow
            (telega-ins " " "album-id: " album-id))))
      (when show-progress
        (telega-ins " ")
        (telega-ins--file-progress msg hr-file))
      (telega-ins "\n"))

    (let ((msg-content (plist-get msg :content)))
      (cond ((plist-get msg-content :is_secret)
             (let ((ttl-in (plist-get msg :self_destruct_in)))
               (unless (telega-zerop ttl-in)
                 (telega-ins--with-face 'telega-shadow
                   (telega-ins "Self-descruct in "))
                 (telega-ins " " (telega-duration-human-readable ttl-in) "\n"))
               (telega-ins--image-slices
                   (telega-self-destruct-create-svg
                    (plist-get photo :minithumbnail)
                    (telega-symbol
                     (if (plist-get msg :self_destruct_type) 'flames 'lock))))))

            ((and (plist-get msg-content :has_spoiler)
                  (not (plist-get msg-content :telega-spoiler-removed)))
             (telega-ins--image-slices
                 (telega-spoiler-create-svg
                  (plist-get photo :minithumbnail)
                  (plist-get hr :width)
                  (plist-get hr :height)
                  telega-thumbnail-size-limits))
             (telega-ins "\n")
             (telega-ins--box-button (telega-i18n "lng_context_disable_spoiler")
               :action #'telega-msg-remove-media-spoiler)
             (telega-ins " "))

            (t
             (telega-ins--image-slices
                 (telega-photo--image
                  photo (or limits telega-photo-size-limits))))))
    t))

(defun telega-ins--audio (msg &optional audio music-symbol)
  "Insert audio message MSG.
If MUSIC-SYMBOL is specified, use it instead of play/pause."
  (unless audio
    (setq audio (telega--tl-get msg :content :audio)))
  (let* ((dur (plist-get audio :duration))
         (proc (plist-get msg :telega-ffplay-proc))
         (playing-p (telega-ffplay-playing-p proc))
         (played (if (telega-ffplay-playing-p proc)
                     (telega-ffplay-progress proc)
                   (telega-ffplay-paused-p proc)))
         (audio-name (plist-get audio :file_name))
         (audio-file (telega-file--renew audio :audio)))
    ;; Play/pause and downloading status
    (if playing-p
        (telega-ins (or music-symbol (telega-symbol 'pause)))
      (telega-ins (or music-symbol (telega-symbol 'play))))
    (telega-ins " ")

    (telega-ins--with-attrs (list :max (/ telega-chat-fill-column 2)
                                  :elide t
                                  :elide-trail (/ telega-chat-fill-column 4))
      (if (telega-file--downloaded-p audio-file)
          (let ((local-path (telega--tl-get audio-file :local :path)))
            (telega-ins--raw-button
                (telega-link-props 'file local-path 'face 'telega-link)
              (telega-ins (telega-short-filename local-path))))
        (telega-ins audio-name)))
    (telega-ins-fmt " (%s %s)"
      (file-size-human-readable (telega-file--size audio-file))
      (telega-duration-human-readable dur))
    (when msg
      (telega-ins-prefix " "
        (telega-ins--file-progress msg audio-file)))

    ;; Progress and [Stop] button
    (when played
      (telega-ins "\n")
      (unless (zerop dur)
        (telega-ins "[")
        (telega-ins-progress-bar
         played dur (/ telega-chat-fill-column 2) ?\.)
        (telega-ins "]" (telega-duration-human-readable played) " "))
      (if (telega-ffplay-playing-p proc)
          (telega-ins--ffplay-controls msg 'no-2x-button)
        (telega-ins--box-button (telega-i18n "lng_mac_menu_player_stop")
          :value msg
          :action #'telega-msg--vvnote-stop)))

    ;; Title --Performer
    (when-let ((title (telega-tl-str audio :title)))
      (telega-ins "\n")
      (telega-ins--with-face 'bold
        (telega-ins title))
      (when-let ((performer (telega-tl-str audio :performer)))
        (telega-ins " --" performer)))

    ;; Album cover
    (let ((thumb (plist-get audio :album_cover_thumbnail))
          (minithumb (plist-get audio :album_cover_minithumbnail)))
      (when (or minithumb thumb)
        (telega-ins "\n")
        (let ((timg (telega-media--image
                     (cons audio 'telega-audio--create-image)
                     (cons thumb :file))))
          (telega-ins--image-slices timg))
        (telega-ins " ")))
    t))

(defun telega-ins--video (msg &optional video no-thumbnail-p)
  "Insert video message MSG.
If NO-THUMBNAIL-P is non-nil, then do not insert thumbnail."
  (let* ((content (plist-get msg :content))
         (video (or video (plist-get content :video)))
         (video-name (telega-tl-str video :file_name))
         (video-file (telega-file--renew video :video)))
    (telega-ins (telega-symbol 'video) " ")
    (if (telega-file--downloaded-p video-file)
        (let ((local-path (telega--tl-get video-file :local :path)))
          (telega-ins--raw-button
              (telega-link-props 'file local-path 'face 'telega-link)
            (telega-ins (telega-short-filename local-path))))
      (telega-ins (or video-name "")))
    (telega-ins-fmt " (%dx%d %s %s)"
      (plist-get video :width)
      (plist-get video :height)
      (file-size-human-readable (telega-file--size video-file))
      (telega-duration-human-readable (plist-get video :duration)))
    (when-let ((tl-ttl (plist-get msg :self_destruct_type)))
      (telega-ins ", ")
      (telega-ins--self-destruct-type tl-ttl 'short))
    (telega-ins-prefix " "
      (telega-ins--file-progress msg video-file))

    ;; Video's thumbnail, if any
    (unless no-thumbnail-p
      (telega-ins "\n")
      (cond ((plist-get content :is_secret)
             ;; Secret video
             (let ((ttl-in (plist-get msg :self_destruct_in)))
               (unless (telega-zerop ttl-in)
                 (telega-ins--with-face 'telega-shadow
                   (telega-ins "Self-descruct in "))
                 (telega-ins (telega-duration-human-readable ttl-in))
                 (telega-ins "\n"))
               (telega-ins--image-slices
                   (telega-self-destruct-create-svg
                    (plist-get video :minithumbnail)
                    (telega-symbol
                     (if (plist-get msg :self_destruct_type) 'flames 'lock))))))

            ((and (plist-get content :has_spoiler)
                  (not (plist-get content :telega-spoiler-removed)))
             (telega-ins--image-slices
                 (telega-spoiler-create-svg
                  (plist-get video :minithumbnail)
                  (plist-get video :width)
                  (plist-get video :height)
                  telega-thumbnail-size-limits))
             (telega-ins "\n")
             (telega-ins--box-button (telega-i18n "lng_context_disable_spoiler")
               :action #'telega-msg-remove-media-spoiler)
             (telega-ins " "))

            (t
             (let ((thumb (plist-get video :thumbnail))
                   (minithumb (plist-get video :minithumbnail)))
               (when (or thumb minithumb)
                 (telega-ins--image-slices
                     (telega-media--image
                      (cons video 'telega-video--create-image)
                      (cons thumb :file)))
                 (telega-ins " "))))))
    t))

(defun telega-ins--ffplay-controls (msg &optional no-2x-button)
  "Insert controls for voice/video notes.
If NO-2X-BUTTON is specified, then do not display \"2x\" button."
  (telega-ins--box-button (telega-symbol "⏪")
    'action (lambda (_button)
              (telega-msg--vvnote-rewind msg -10)))
  (telega-ins " ")
  (telega-ins--box-button (telega-symbol "⏩")
    'action (lambda (_button)
              (telega-msg--vvnote-rewind msg 10)))
  (telega-ins " ")
  (unless no-2x-button
    (let* ((label2x "2×")
           (ends (if (functionp telega-box-button-endings)
                     (funcall telega-box-button-endings label2x)
                   telega-box-button-endings)))
      (setq label2x (concat (or (car ends) "[") label2x (or (cdr ends) "]")))

      (telega-ins--box-button label2x
        'face (if (eq telega-vvnote-play-speed 1)
                  'telega-box-button
                'telega-box-button-active)
        :value msg
        :action #'telega-msg--vvnote-play-speed-toggle))
    (telega-ins " "))
  (telega-ins--box-button (telega-i18n "lng_mac_menu_player_stop")
    :value msg
    :action #'telega-msg--vvnote-stop))

(defun telega--can-speech-recognize-p (&optional duration)
  "Return non-nil if speech recognition is available."
  (or (telega-user-match-p (telega-user-me) 'is-premium)
      (and telega--speech-recognition-trial
           (> (plist-get telega--speech-recognition-trial :left_count) 0)
           (or (null duration)
               (< duration (plist-get telega--speech-recognition-trial
                                      :max_media_duration))))))

(defun telega-ins--speech-recognition-button (recognition for-msg
                                                          &optional duration)
  "Insert speech recognize button."
  ;; NOTE: if previous recognition results in error, then also show
  ;; the recognize button
  (when (and (or (not recognition)
                 (eq 'speechRecognitionResultError
                     (telega--tl-type recognition)))
             (telega--can-speech-recognize-p duration))
    (telega-ins " ")
    (telega-ins--box-button "🠆A"
      :value for-msg
      :action #'telega--recognizeSpeech
      'help-echo "Recognize speech")))

(defun telega-ins--speech-recognition-text (recognition)
  "Insert results of the voice/video message recognition."
  (cl-assert recognition)
  (cl-ecase (telega--tl-type recognition)
    (speechRecognitionResultPending
     (telega-ins--with-face 'telega-shadow
       (telega-ins (or (telega-tl-str recognition :partial_text)
                       "Recognizing")
                   "...")))
    (speechRecognitionResultText
     (telega-ins--with-face 'telega-shadow
       (telega-ins (telega-tl-str recognition :text))))
    (speechRecognitionResultError
     (telega-ins--with-face 'error
       (telega-ins
        (telega-tl-str (plist-get recognition :error) :message))))))

(defun telega-ins--voice-note (msg &optional voice-note)
  "Insert message MSG with VOICE-NOTE content."
  (let* ((note (or voice-note (telega--tl-get msg :content :voice_note)))
         (dur (plist-get note :duration))
         (proc (plist-get msg :telega-ffplay-proc))
         (playing-p (telega-ffplay-playing-p proc))
         (played (if (telega-ffplay-playing-p proc)
                     (telega-ffplay-progress proc)
                   (telega-ffplay-paused-p proc)))
         (note-file (telega-file--renew note :voice))
         (waveform (plist-get note :waveform))
         (waves (telega-vvnote--waveform-decode waveform)))

    ;; play/pause only for messages
    (when msg
      (if playing-p
          (telega-ins (telega-symbol 'pause))
        (telega-ins (telega-symbol 'play)))
      (telega-ins " "))

    ;; waveform image
    (if telega-use-images
        (telega-ins--image
         (telega-vvnote--waves-svg
          waves (round (telega-chars-xheight
                        telega-vvnote-waves-height-factor))
          dur played))

      ;; tty version
      (telega-ins "[")
      (telega-ins-progress-bar played dur 15 ?\# ?\.)
      (telega-ins "]"))

    ;; Duration / self destruct
    (telega-ins " (" (telega-duration-human-readable dur) ")")
    (when-let ((tl-ttl (plist-get msg :self_destruct_type)))
      (telega-ins ", ")
      (telega-ins--self-destruct-type tl-ttl 'short))

    ;; ffplay controls to seek/2x/stop
    (when (telega-ffplay-playing-p proc)
      (telega-ins " ")
      (telega-ins--ffplay-controls msg)
      (telega-ins " "))

    ;; Show download status/button only if inserted for message
    (when msg
      (when (telega--tl-get msg :content :is_listened)
        (telega-ins (telega-symbol 'eye)))
      (telega-ins-prefix " "
        (telega-ins--file-progress msg note-file)))

    (let ((recognition (plist-get note :speech_recognition_result)))
      (telega-ins--speech-recognition-button recognition msg dur)
      (when recognition
        (telega-ins "\n")
        (telega-ins--speech-recognition-text recognition)))
    ))

(defun telega-ins--video-note (msg &optional video-note)
  "Insert message MSG with VIDEO-NOTE content."
  (let* ((note (or video-note (telega--tl-get msg :content :video_note)))
         (dur (plist-get note :duration))
         (note-file (telega-file--renew note :video))
         (recognition (plist-get note :speech_recognition_result))
         (ffplay-proc (plist-get msg :telega-ffplay-proc))
         (tl-ttl (plist-get msg :self_destruct_type)))
    (telega-ins (propertize "NOTE" 'face 'telega-shadow))
    (telega-ins-fmt " (%dx%d %s %s)"
      (plist-get note :length) (plist-get note :length)
      (file-size-human-readable (telega-file--size note-file))
      (telega-duration-human-readable dur))
    (when (telega--tl-get msg :content :is_viewed)
      (telega-ins (telega-symbol 'eye)))
    (when tl-ttl
      (telega-ins ", ")
      (telega-ins--self-destruct-type tl-ttl 'short))

    ;; ffplay controls to seek/2x/stop
    (when (telega-ffplay-playing-p ffplay-proc)
      (telega-ins " ")
      (telega-ins--ffplay-controls msg)
      (telega-ins " "))

    (when telega-use-images
      (when-let* ((waveform (plist-get note :waveform))
                  (waves (telega-vvnote--waveform-decode waveform)))
        (telega-ins " ")
        (telega-ins--image
         (telega-vvnote--waves-svg
          waves (round (telega-chars-xheight
                        telega-vvnote-waves-height-factor))
          (plist-get note :duration) (telega-ffplay-progress ffplay-proc)))))

    (telega-ins--speech-recognition-button recognition msg dur)

    (telega-ins-prefix " "
      (telega-ins--file-progress msg note-file))

    (let ((thumb (plist-get note :thumbnail))
          (minithumb (plist-get note :minithumbnail)))
      (when-let ((img (or (plist-get msg :telega-ffplay-frame)
                          (when (or minithumb thumb)
                            (telega-media--image
                             (cons note
                                   (if tl-ttl
                                       #'telega-vvnote-video-ttl--create-image
                                     #'telega-vvnote-video--create-image))
                             (cons thumb :file))))))
        (telega-ins "\n")
        (telega-ins--image-slices img)
        (telega-ins " ")))

    (when recognition
      (telega-ins "\n")
      (telega-ins--speech-recognition-text recognition))
    ))

(defun telega-ins--document-header (doc &optional no-attach-symbol)
  "Attach header for the document DOC.
If NO-ATTACH-SYMBOL is specified, then do not insert attachment symbol."
  (let ((fname (telega-tl-str doc :file_name))
        (doc-file (telega-file--renew doc :document)))
    (unless no-attach-symbol
      (telega-ins (telega-symbol 'attachment) " "))

    (if (telega-file--downloaded-p doc-file)
        (let ((local-path (telega--tl-get doc-file :local :path)))
          (telega-ins--raw-button (telega-link-props 'file local-path
                                                     'face 'telega-link)
            (telega-ins (telega-short-filename local-path))))
      (telega-ins fname))
    (telega-ins " (" (file-size-human-readable
                      (telega-file--size doc-file))
                ") ")))

(defun telega-ins--document (msg &optional doc)
  "Insert document DOC."
  (unless doc
    (setq doc (telega--tl-get msg :content :document)))
  (telega-ins--document-header doc)
  (telega-ins--file-progress msg (telega-file--renew doc :document))

  ;; document's thumbnail preview (if any)
  (let ((thumb (plist-get doc :thumbnail))
        (minithumb (plist-get doc :minithumbnail)))
    (when (or thumb minithumb)
      (telega-ins "\n")
      (telega-ins--image-slices
          (telega-media--image
           (cons doc 'telega-thumb-or-minithumb--create-image)
           (cons thumb :file)))
      (telega-ins " "))))

(defun telega-ins--game (msg &optional game-value)
  "Insert GAME."
  (let ((game (or game-value (telega--tl-get msg :content :game))))
    (telega-ins--with-face 'telega-shadow
      (telega-ins (telega-symbol 'game) " "
                  (upcase (telega-i18n "lng_game_tag")) "\n"))
    (telega-ins--line-wrap-prefix (telega-symbol 'vertical-bar)
      (when-let ((photo (plist-get game :photo)))
        (telega-ins--photo photo msg)
        (telega-ins "\n"))
      (telega-ins--with-face 'telega-webpage-sitename
        (telega-ins (telega-tl-str game :title)))
      (telega-ins "\n")
      (unless (telega-ins--fmt-text (plist-get game :text) msg)
        (telega-ins (telega-tl-str game :description)))
      t)))

(defun telega-ins--webpage (msg &optional web-page)
  "Insert WEB-PAGE preview.
Return `non-nil' if WEB-PAGE has been inserted."
  (unless web-page
    (setq web-page (telega--tl-get msg :content :web_page)))
  (when web-page
    (telega-ins--line-wrap-prefix (propertize (telega-symbol 'vertical-bar)
                                              'face 'telega-webpage-sitename)
      (when-let ((sitename (telega-tl-str web-page :site_name)))
        (telega-ins--with-face 'telega-webpage-sitename
          (telega-ins sitename))
        (when (telega-msg-match-p msg '(prop :can_be_edited))
          (telega-ins " ")
          (telega-ins--text-button (telega-symbol 'button-close)
            'face 'telega-link
            :action #'telega-msg-disable-webpage-preview
            'help-echo "telega: Press to disable webpage preview"))
        (telega-ins "\n"))
      (when-let ((title (telega-tl-str web-page :title)))
        (telega-ins--with-face 'telega-webpage-title
          (telega-ins title))
        (telega-ins "\n"))
      (when-let ((desc (telega-tl-str web-page :description)))
        (when (and telega-webpage-preview-description-limit
                   (> (length desc) telega-webpage-preview-description-limit))
          (setq desc (truncate-string-to-width
                      desc telega-webpage-preview-description-limit nil nil
                      (when (> telega-webpage-preview-description-limit 0)
                        telega-symbol-eliding))))
        (when (telega-ins desc)
          (telega-ins "\n")))

      ;; NOTE: animation/video uses it's own thumbnails
      (let ((photo (plist-get web-page :photo)))
        (when (and photo (not (or (plist-get web-page :animation)
                                  (plist-get web-page :video)))
                   telega-webpage-preview-size-limits)
          (telega-ins--photo photo msg telega-webpage-preview-size-limits)
          (telega-ins "\n"))

        ;; See `telega-msg-open-webpage'
        (cond ((plist-get web-page :animation)
               (telega-ins--animation-msg msg (plist-get web-page :animation))
               (telega-ins "\n"))
              ((plist-get web-page :audio)
               (telega-ins--audio msg (plist-get web-page :audio))
               (telega-ins "\n"))
              ((plist-get web-page :document)
               (telega-ins--document msg (plist-get web-page :document))
               (telega-ins "\n"))
              ((plist-get web-page :sticker)
               (telega-ins--sticker-image (plist-get web-page :sticker) 'slices)
               (telega-ins "\n"))
              ((plist-get web-page :video)
               (telega-ins--video
                msg (plist-get web-page :video))
               (telega-ins "\n"))
              ((plist-get web-page :video_note)
               (telega-ins--video-note msg (plist-get web-page :video_note))
               (telega-ins "\n"))
              ((plist-get web-page :voice_note)
               (telega-ins--voice-note msg (plist-get web-page :voice_note))
               (telega-ins "\n"))
              ((and (not (telega-zerop (plist-get web-page :story_sender_chat_id)))
                    (not (telega-zerop (plist-get web-page :story_id))))
               (telega-ins--story-content
                (telega-story-get
                 (plist-get web-page :story_sender_chat_id)
                 (plist-get web-page :story_id)
                 'offline)
                msg)
               (telega-ins "\n"))
              ))

      ;; Stickers/Custom Emojis
      (when-let ((stickers (append (plist-get web-page :stickers) nil))
                 (nslices 1))
        (seq-doseq (sticker stickers)
          (when (telega-custom-emoji-sticker-p sticker)
            (telega-custom-emoji--ensure sticker))
          (when-let ((sslices (car (telega-sticker-size sticker))))
            (when (> sslices nslices)
              (setq nslices sslices))))

        (dotimes (slice-num nslices)
          (seq-doseq (sticker stickers)
            (telega-ins--image (telega-sticker--image sticker) slice-num)
            (telega-ins " "))
          (telega-ins "\n")))
      )

    ;; Additional View button
    (if (zerop (plist-get web-page :instant_view_version))
        (when-let ((title (pcase (plist-get web-page :type)
                            ("telegram_channel"
                             (telega-i18n "lng_view_button_channel"))
                            ("telegram_channel_boost"
                             (telega-i18n "lng_view_button_boost"))
                            ((or "telegram_chat" "telegram_megagroup")
                             (telega-i18n "lng_view_button_group"))
                            ("telegram_bot"
                             (telega-i18n "lng_view_button_bot"))
                            ("telegram_message"
                             (telega-i18n "lng_view_button_message"))
                            ("telegram_background"
                             (telega-i18n "lng_view_button_background"))
                            ("telegram_theme"
                             (telega-i18n "lng_view_button_theme"))
                            ("telegram_user"
                             (telega-i18n "lng_view_button_user"))
                            ("telegram_channel_request"
                             (telega-i18n "lng_view_button_request_join"))
                            ("telegram_livestream"
                             (telega-i18n "lng_view_button_voice_chat_channel"))
                            ("telegram_voicechat"
                             "JOIN AS LISTENER")
                            ("telegram_chatlist"
                             "VIEW CHAT LIST")
                            ("telegram_story"
                             (telega-i18n "lng_view_button_story"))
                            )))
          (telega-ins--box-button (concat "   " (upcase title) "   ")
            'action 'telega-msg-button--action))

      (telega-ins--box-button
          (concat "  " (telega-symbol 'lightning) " INSTANT VIEW  ")
        'action 'telega-msg-button--iv-action))

    ;; Remove trailing newline, if any
    (when (= (char-before) ?\n)
      (delete-char -1))
    t))

(defun telega-ins--location (location)
  "Inserter for the LOCATION."
  (telega-ins (telega-symbol 'location) " ")
  (telega-ins-fmt "%fN, %fE"
    (plist-get location :latitude) (plist-get location :longitude)))

(defun telega-ins--location-live-header (live-for updated-ago)
  "Insert live location header."
  (telega-ins--with-face 'telega-shadow
    (telega-ins "Live"))
  (when (> live-for 0)
    (telega-ins " " (telega-time-ago-human-readable updated-ago)
                " " (telega-symbol 'timer-clock)
                (telega-duration-human-readable
                 live-for (if (> live-for 3600) 2 1)
                 (unless (> live-for 3600) 'long))))
  t)

(defun telega-ins--location-live (msg)
  "Insert live location description for location message MSG."
  ;; NOTE: in case of unexpired live location show last update
  ;; time and expiration period
  (when-let ((live-for-spec (telega-msg-location-live-for msg)))
    (cl-destructuring-bind (live-for updated-ago) live-for-spec
      (telega-ins " ")
      (telega-ins--location-live-header live-for updated-ago)

      (when (plist-get msg :can_be_edited)
        (telega-ins " ")
        (telega-ins--box-button (telega-i18n "telega_stop")
          'action (lambda (_button)
                    (telega--editMessageLiveLocation msg nil)))

        (let ((proximity-radius
               (telega--tl-get msg :content :proximity_alert_radius)))
          (telega-ins "\n")
          (telega-ins "Proximity Alert Radius: ")
          (unless (zerop proximity-radius)
            (telega-ins (telega-distance-human-readable proximity-radius)
                        " "))
          (telega-ins--box-button (if (zerop proximity-radius)
                                      "Set"
                                    "Change")
            'action (lambda (_ignored)
                      (telega--editMessageLiveLocation
                       msg (telega--tl-get msg :content :location)
                       :proximity-alert-radius
                       (read-number "Proximity Alert Radius (meters): "))))))
      )))

(cl-defun telega-ins--contact (contact &key
                                       (with-avatar-p t)
                                       (with-title-faces-p t)
                                       (with-online-status-p t)
                                       (with-username-p t)
                                       (with-phone-p t))
  "Multiple line variant inserter for CONTACT."
  (declare (indent 1))
  (let* ((user (if (eq (telega--tl-type contact) 'user)
                   contact
                 (telega-user-get (plist-get contact :user_id) 'offline)))
         (title-faces (when (and user with-title-faces-p)
                        (telega-msg-sender-title-faces user))))
    (when (and user with-avatar-p)
      (telega-ins--image (telega-msg-sender-avatar-image-one-line user)))

    ;; Use different symbols depending on whether contact has been
    ;; chatted or not
    (if (and user (telega-user-chat user))
        (telega-ins (telega-symbol 'contact))
      (telega-ins (telega-symbol 'member)))
    (telega-ins " ")

    (telega-ins--with-face title-faces
      (telega-ins (telega-user-title contact 'full-name)))
    (when (and user with-online-status-p)
      (telega-ins--user-online-status user))

    (when (and user with-username-p)
      (when-let ((username (telega-msg-sender-username user 'with-@)))
        (telega-ins--with-face 'telega-shadow
          (telega-ins telega-symbol-nbsp "•" telega-symbol-nbsp))
        (telega-ins--with-face 'telega-username
          (telega-ins username))))

    (when-let* ((with-phone-p with-phone-p)
                (phone-number (telega-tl-str contact :phone_number)))
      (telega-ins--with-face 'telega-shadow
        (telega-ins telega-symbol-nbsp "•" telega-symbol-nbsp))
      (telega-ins (unless (string-prefix-p "+" phone-number) "+") phone-number))
    t))

(defun telega-ins--contact-msg (msg)
  "Inserter for contact message MSG."
  ;; Two lines for the contact
  (let* ((content (plist-get msg :content))
         (contact (plist-get content :contact))
         (user-id (plist-get contact :user_id))
         (user (unless (zerop user-id) (telega-user-get user-id)))
         (user-ava (when (and telega-user-show-avatars user)
                     (telega-msg-sender-avatar-image-three-lines user))))
    (when user-ava
      (telega-ins--image user-ava 0))
    (telega-ins--with-face 'telega-shadow
      (telega-ins-i18n "lng_in_dlg_contact"))
    (telega-ins "\n")
    (when user-ava
      (telega-ins--image user-ava 1))
    (telega-ins--contact (plist-get content :contact)
      :with-avatar-p nil)
    (telega-ins "\n")
    (when user-ava
      (telega-ins--image user-ava 2))
    (telega-ins--box-button (concat "   VIEW CONTACT   ")
      'action 'telega-msg-button--action)))

(defun telega-ins--call-msg (msg)
  "Insert call message MSG."
  (let* ((content (plist-get msg :content))
         (video-p (plist-get content :is_video))
         (call-symbol (telega-symbol (if video-p 'video 'phone)))
         (reason (telega--tl-type (plist-get content :discard_reason)))
         (label (cond ((plist-get msg :is_outgoing)
                       (if (eq reason 'callDiscardReasonMissed)
                           (telega-i18n (if video-p
                                            "lng_call_video_cancelled"
                                          "lng_call_cancelled"))
                         (telega-i18n (if video-p
                                          "lng_call_video_outgoing"
                                        "lng_call_outgoing"))))
                      ((eq reason 'callDiscardReasonMissed)
                       (telega-i18n (if video-p
                                        "lng_call_video_missed"
                                      "lng_call_missed")))
                      ((eq reason 'callDiscardReasonDeclined)
                       (telega-i18n (if video-p
                                        "lng_call_video_declined"
                                      "lng_call_declined")))
                      (t
                       (telega-i18n (if video-p
                                        "lng_call_video_incoming"
                                      "lng_call_incoming"))))))
    (telega-ins (cond ((memq reason '(callDiscardReasonMissed
                                      callDiscardReasonDeclined))
                       (propertize call-symbol 'face 'error))
                      ((plist-get msg :is_outgoing)
                       (concat call-symbol "→"))
                      (t
                       (concat call-symbol "←")))
                " ")
    (telega-ins (propertize label 'face 'telega-shadow))
    (telega-ins-fmt " (%s)"
      (telega-duration-human-readable
       (plist-get content :duration)))))

(defun telega-ins--dice-msg-content (content &optional one-line-p)
  "Inserter for the \"messageDice\" MSG."
  (let ((dice-value (plist-get content :value))
        (dice-emoji (telega-tl-str content :emoji)))
    (telega-ins (or dice-emoji (car telega-symbol-dice-list)) " ")
    (telega-ins--with-face 'telega-shadow
      (telega-ins-i18n "telega_random_dice"))
    (telega-ins " " (number-to-string dice-value))

    (unless one-line-p
      (telega-ins "\n")

      ;; TODO: Animated sticker as dice roll, also see
      ;; `:initial_state_sticker'
      (if-let ((dice-isticker nil)) ; (plist-get content :final_state_sticker)))
          (telega-ins--sticker-image dice-isticker 'slices)

        (let ((dice-symbol (or (nth dice-value telega-symbol-dice-list)
                               (number-to-string dice-value))))
          (if telega-emoji-use-images
              (telega-ins--image-slices
                  (telega-emoji-create-svg dice-symbol (car telega-sticker-size)))
            (telega-ins dice-symbol))
          ))
      )))

(defun telega-ins--animated-emoji-msg (msg)
  "Inserter for the \"messageAnimatedEmoji\" MSG."
  (let* ((content (plist-get msg :content))
         (emoji (telega-tl-str content :emoji))
         (animated-emoji (plist-get content :animated_emoji))
         (sticker (plist-get animated-emoji :sticker))
         (fs-sticker (plist-get msg :telega-sticker-fullscreen)))
    (when (and fs-sticker
               (plist-get fs-sticker :telega-ffplay-frame-filename))
      (setq sticker fs-sticker))
    ;; NOTE: sticker might be nil if yet unknown for a custom emoji.
    ;; In this case we insert emoji instead
    (if sticker
        (telega-ins--sticker-image sticker 'slices)
      (telega-ins emoji))))

(defun telega-ins--invoice (invoice)
  "Insert invoice message MSG."
  (let ((title (telega-tl-str invoice :title))
        (desc (telega-tl-str invoice :description))
        (photo (plist-get invoice :photo))
        (currency (telega-tl-str invoice :currency)))
    (telega-ins (telega-symbol 'invoice) " ")
    (telega-ins-fmt "%.2f%s " (/ (plist-get invoice :total_amount) 100.0)
                    (or (cdr (assoc currency telega-currency-symbols-alist))
                        currency))
    (telega-ins--with-face 'telega-shadow
      (telega-ins-i18n (if (plist-get invoice :is_test)
                           "lng_payments_invoice_label_test"
                         "lng_payments_invoice_label")))
    (telega-ins "\n")
    (when photo
      (telega-ins--photo photo)
      (telega-ins "\n"))
    (telega-ins--with-face '(telega-link bold)
      (telega-ins title))
    (telega-ins "\n")
    (telega-ins desc)))

(defun telega-ins--poll (msg)
  "Insert poll message MSG."
  (let* ((content (plist-get msg :content))
         (poll (plist-get content :poll))
         (poll-type (plist-get poll :type))
         (closed-p (plist-get poll :is_closed))
         (anonymous-p (plist-get poll :is_anonymous))
         (options (append (plist-get poll :options) nil))
         (choices (cl-loop for popt in options
                           for popt-id from 0
                           if (plist-get popt :is_chosen)
                           collect popt-id))
         (quiz-p (eq (telega--tl-type poll-type) 'pollTypeQuiz))
         (quiz-popt-id (when quiz-p
                         (plist-get poll-type :correct_option_id)))
         (multiple-answers-p (unless quiz-p
                               (plist-get poll-type :allow_multiple_answers)))
         (option-symbols
          (mapcar #'telega-symbol
                  (cond (quiz-p telega-symbol-quiz-options)
                        (multiple-answers-p telega-symbol-poll-multiple-options)
                        (t telega-symbol-poll-options)))))
    ;; Poll header
    (telega-ins (telega-symbol 'poll) " ")
    (telega-ins--with-face 'telega-shadow
      (telega-ins-i18n (cond ((and anonymous-p quiz-p) "lng_polls_anonymous_quiz")
                             (anonymous-p "lng_polls_anonymous")
                             (quiz-p "lng_polls_public_quiz")
                             (t "lng_polls_public"))))
    (when (and quiz-p (telega-tl-str poll-type :explanation))
      (telega-ins " ")
      (telega-ins--text-button (telega-symbol 'bulp)
        'action (lambda (_button)
                  (message "telega: %s"
                           (telega-tl-str poll-type :explanation)))))
    ;; I18N: polls_votes_count -> {count} votes
    (telega-ins ", " (telega-i18n (if quiz-p
                                      "lng_polls_answers_count"
                                    "lng_polls_votes_count")
                                  :count (plist-get poll :total_voter_count)))
    (when-let ((recent-voters (append (plist-get poll :recent_voter_ids) nil)))
      (telega-ins " ")
      (seq-doseq (rv recent-voters)
        (telega-ins--image (telega-msg-sender-avatar-image-one-line
                            (telega-msg-sender rv)))))
    (when closed-p
      (telega-ins ", ")
      (telega-ins--with-face 'error
        (telega-ins-i18n "lng_polls_closed")))
    (when (and (not closed-p) (plist-get msg :can_be_edited))
      (telega-ins " ")
      (telega-ins--box-button
          (if quiz-p
              "Stop Quiz"
            (telega-i18n "lng_polls_stop"))
        'action (lambda (_ignored)
                  (when (yes-or-no-p (telega-i18n "lng_polls_stop_warning"))
                    (telega--stopPoll msg)))))
    (telega-ins "\n")

    ;; Question and options
    (telega-ins--with-face 'bold
      (telega-ins--fmt-text (plist-get poll :question)))
    (dotimes (popt-id (length options))
      (let ((popt (nth popt-id options)))
        (telega-ins "\n")
        (telega-ins--raw-button
            (list 'action (lambda (_ignore)
                            (if (plist-get popt :is_chosen)
                                (apply #'telega--setPollAnswer msg
                                       (delete popt-id choices))
                              (if multiple-answers-p
                                  (apply #'telega--setPollAnswer msg
                                         (cons popt-id choices))
                                (telega--setPollAnswer msg popt-id))))
                  'keymap (let ((map (make-sparse-keymap)))
                            (set-keymap-parent map telega-msg-button-map)
                            (define-key map (kbd "SPC") 'push-button)
                            map))
          (telega-ins--with-face 'telega-link
            (telega-ins
             (if quiz-p
                 (cond ((eq quiz-popt-id popt-id)
                        (if (plist-get popt :is_chosen)
                            (propertize (nth 0 option-symbols) 'face 'bold)
                          (nth 0 option-symbols)))
                       ((plist-get popt :is_chosen)
                        (propertize (nth 2 option-symbols) 'face 'error))
                       (t
                        (nth 1 option-symbols)))

               (if (plist-get popt :is_chosen)
                   (propertize (nth 1 option-symbols) 'face 'bold)
                 (nth 0 option-symbols)))))

          (telega-ins " ")
          (telega-ins--fmt-text (plist-get popt :text))
          (when (or choices closed-p)
            (telega-ins "\n")
            (telega-ins--line-wrap-prefix "  "
              (telega-ins--with-face 'telega-shadow
                (telega-ins-fmt "%3d%% " (plist-get popt :vote_percentage)))
              (telega-ins--with-face 'telega-link
                (telega-ins--image
                 (telega-poll-create-svg (/ telega-chat-fill-column 8)
                                         (plist-get popt :vote_percentage))))
              (telega-ins " ")
              (telega-ins--with-face 'telega-shadow
                (telega-ins-i18n (if quiz-p
                                     "lng_polls_answers_count"
                                   "lng_polls_votes_count")
                  :count (plist-get popt :voter_count))))))
        ))

    (unless anonymous-p
      (telega-ins "\n")
      (telega-ins--box-button
          (concat "  " (upcase (telega-i18n "lng_polls_view_results")) "  ")
        :value msg
        :action #'telega-msg-open-poll))
    ))

(defun telega-ins--animation-msg (msg &optional animation)
  "Inserter for animation message MSG.
If NO-THUMBNAIL-P is non-nil, then do not insert thumbnail."
  (unless animation
    (setq animation (telega--tl-get msg :content :animation)))
  (let ((anim-file (telega-file--renew animation :animation)))
    (telega-ins (propertize "GIF" 'face 'telega-shadow) " ")
    (if (telega-file--downloaded-p anim-file)
        (let ((local-path (telega--tl-get anim-file :local :path)))
          (telega-ins--raw-button
              (telega-link-props 'file local-path 'face 'telega-link)
            (telega-ins (telega-short-filename local-path))))
      (telega-ins (telega-tl-str animation :file_name)))
    (telega-ins-fmt " (%dx%d %s %s)"
      (plist-get animation :width)
      (plist-get animation :height)
      (file-size-human-readable (telega-file--size anim-file))
      (telega-duration-human-readable (telega--tl-get animation :duration)))
    (telega-ins-prefix " "
      (telega-ins--file-progress msg anim-file))
    (telega-ins "\n")

    (telega-ins--animation-image animation 'sliced)))

(defun telega-ins--location-msg (msg &optional venue-p)
  "Insert content for location message MSG."
  (let* ((venue (when venue-p
                  (telega--tl-get msg :content :venue)))
         (loc (if venue-p
                  (plist-get venue :location)
                (telega--tl-get msg :content :location)))
         (map (plist-get msg :telega-map)))
    (telega-ins--location loc)
    (unless venue-p
      (telega-ins--location-live msg))
    (when telega-my-location
      (telega-ins "\n" "Distance From Me: "
                  (telega-distance-human-readable
                   (telega-location-distance loc telega-my-location))))

    (unless map
      ;; Initial location map creation
      (setq map
            (list :width (telega-chars-xwidth (cdr telega-location-size))
                  :height (telega-chars-xheight (car telega-location-size))
                  :zoom telega-location-zoom
                  :scale telega-location-scale
                  :sender_id (unless venue-p
                               (plist-get msg :sender_id))))
      (plist-put msg :telega-map map))

    ;; NOTE: if location or heading changes (or initial request), then
    ;; redraw map thumbnail.
    (let* ((heading (unless venue-p
                      (telega--tl-get msg :content :heading)))
           (heading-changed-p (not (equal heading (plist-get map :user-heading))))
           (loc-changed-p (not (equal loc (plist-get map :user-location))))
           (need-map-photo-p
            (telega-map--need-new-map-photo-p map (plist-get map :user-location)))
           (alert-radius
            (telega--tl-get msg :content :proximity_alert_radius))
           (alert-radius-changed-p
            (not (equal alert-radius (plist-get map :user-alert-radius)))))
      (when (or heading-changed-p loc-changed-p alert-radius-changed-p
                need-map-photo-p)
        ;; ARGUABLE: Cancel previously pending request?
        ;;   (telega-server--callback-rm (plist-get map :get-map-extra))
        ;;   (telega--cancelDownloadFile (plist-get map :photo))
        ;; TODO: what if some other user shown in map image moved?

        (unless (plist-get map :user-location)
          ;; Inilial map load or zoom has been changed
          (plist-put map :photo nil)
          (cl-assert need-map-photo-p))

        ;; Save location tracks if location moved more then 50 meters
        ;; from last track location
        (when loc-changed-p
          (let ((user-tracks (plist-get map :user-tracks)))
            (when (or (not user-tracks)
                      (> (telega-location-distance (car user-tracks) loc) 50))
              (plist-put map :user-tracks (cons loc user-tracks)))))

        (plist-put map :user-location loc)
        (plist-put map :user-heading heading)
        (plist-put map :user-alert-radius alert-radius)
        (unless (plist-get map :svg-image)
          ;; For initial image creation
          (plist-put map :map-location loc))

        (when need-map-photo-p
          (plist-put map :get-map-extra
                     (telega-map--get-thumbnail-file map loc)))

        (plist-put map :svg-image (telega-map--create-image map))))

    (telega-ins "\n")
    (telega-ins--image-slices
     (plist-get map :svg-image) nil
     ;; Map controls
     (lambda (slice-num)
       (cond ((= slice-num 0)
              (telega-ins-fmt " zoom: %d" (- (plist-get map :zoom) 12)))
             ((= slice-num 1)
              (telega-ins " ")
              (telega-ins--box-button " + "
                'action (lambda (_ignore)
                          (when (telega-map--zoom map 1)
                            (plist-put msg :telega-map
                                       (plist-put map :user-location nil))
                            (telega-msg-redisplay msg)))))
             ((= slice-num 2)
              (telega-ins " ")
              (telega-ins--box-button " - "
                'action (lambda (_ignore)
                          (when (telega-map--zoom map -1)
                            (plist-put msg :telega-map
                                       (plist-put map :user-location nil))
                            (telega-msg-redisplay msg))))))))

    (when venue-p
      (telega-ins "\n")
      (telega-ins--with-face 'bold
        (telega-ins (telega-tl-str venue :title)))
      (telega-ins "\n")
      (telega-ins--with-face 'telega-shadow
        (telega-ins (telega-tl-str venue :address))))
    ))

(defun telega-ins--giveaway-msg-content (content)
  "Insert CONTENT for the premium giveaway message."
  (telega-ins--with-face '(telega-shadow bold)
    (telega-ins-i18n "lng_prizes_title"
      :count 2))
  (telega-ins "\n")
  (when-let ((sticker (plist-get content :sticker)))
    (telega-ins--sticker-image sticker)
    (telega-ins "\n"))
  (telega-ins-i18n "lng_prizes_about"
    :count (plist-get content :winner_count)
    :duration (telega-i18n "lng_premium_gift_duration_months"
                :count (plist-get content :month_count)))
  (telega-ins "\n")

  (let ((ga-params (plist-get content :parameters)))
    (telega-ins--with-face 'bold
      (telega-ins (telega-i18n "lng_prizes_participants") "\n"))
    (telega-ins--line-wrap-prefix "  "
      (telega-ins-i18n (if (plist-get ga-params :only_new_members)
                           "lng_prizes_participants_new"
                         "lng_prizes_participants_all")
        :count (1+ (length (plist-get ga-params :additional_chat_ids))))
      (telega-ins "\n")
      (let ((boosted-chat (telega-chat-get
                           (plist-get ga-params :boosted_chat_id))))
        (telega-ins--raw-button
            (telega-link-props 'sender boosted-chat 'type 'telega)
          (telega-ins--msg-sender boosted-chat
            :with-avatar-p t
            :with-username-p t
            :with-brackets-p t)))
      (seq-doseq (add-chat-id (plist-get ga-params :additional_chat_ids))
        (let ((addition-chat (telega-chat-get add-chat-id)))
          (telega-ins "\n")
          (telega-ins--raw-button
              (telega-link-props 'sender addition-chat 'type 'telega)
            (telega-ins--msg-sender addition-chat
              :with-avatar-p t
              :with-username-p t
              :with-brackets-p t))))
      (telega-ins "\n")

      (let ((countries (plist-get ga-params :country_codes)))
        (unless (seq-empty-p countries)
          (telega-ins-i18n "lng_prizes_countries"
            :countries (mapconcat #'identity countries ", "))
          (telega-ins "\n"))))

    (telega-ins--with-face 'bold
      (telega-ins (telega-i18n "lng_prizes_date") "\n"))
    (telega-ins--line-wrap-prefix "  "
      (telega-ins--date
       (plist-get ga-params :winners_selection_date) 'date-time))

    (when (> (telega-time-seconds)
             (plist-get ga-params :winners_selection_date))
      (telega-ins--with-face 'telega-shadow
        (telega-ins " (" (telega-i18n "lng_prizes_end_title") ")")))

    (telega-ins "\n")
    (telega-ins--box-button
        (concat "  " (telega-i18n "lng_prizes_how_works") "  ")
      'action 'telega-msg-button--action)
    ))

(defun telega-ins--input-file (document &optional attach-symbol trailing-text)
  "Insert input file."
  (telega-ins (or attach-symbol (telega-symbol 'attachment)) " ")
  (when-let ((preview (get-text-property
                       0 'telega-preview (plist-get document :@type))))
    (telega-ins--image preview)
    (telega-ins " "))
  (cl-ecase (telega--tl-type document)
    (inputFileLocal
     (let ((filename (plist-get document :path)))
       (telega-ins (abbreviate-file-name filename)
                   " ("
                   (file-size-human-readable
                    (file-attribute-size (file-attributes filename)))
                   ")")))
    (inputFileId
     (telega-ins-fmt "Id: %d" (plist-get document :id)))
    (inputFileRemote
     ;; TODO: getRemoteFile
     (telega-ins-fmt "Remote: %s" (plist-get document :id))
     ))
  (when trailing-text
    (telega-ins trailing-text))
  t)

(defun telega-msg-special-p (msg)
  "Return non-nil if MSG is special."
  (memq (telega--tl-type (plist-get msg :content))
        '(messageContactRegistered
          messageChatAddMembers
          messageChatJoinByLink
          messageChatJoinByRequest
          messageChatDeleteMember
          messageChatChangeTitle
          messageSupergroupChatCreate
          messageBasicGroupChatCreate
          messageCustomServiceAction
          messageChatSetTtl
          messageExpiredPhoto
          messageExpiredVideo
          messageExpiredVoiceNote
          messageExpiredVideoNote
          messageChatChangePhoto
          messageChatDeletePhoto
          messageChatUpgradeTo
          messageChatUpgradeFrom
          messagePinMessage
          messageScreenshotTaken
          messageGameScore
          messageProximityAlertTriggered
          messageVideoChatScheduled
          messageVideoChatStarted
          messageVideoChatEnded
          messageInviteVideoChatParticipants
          messageChatSetTheme
          messageChatSetMessageAutoDeleteTime
          messagePaymentSuccessful
          messageForumTopicCreated
          messageForumTopicEdited
          messageForumTopicIsClosedToggled
          messagePremiumGiveawayCreated
          messagePremiumGiveawayCompleted
          messageGiftedPremium
          messageChatSetBackground
          messageBotWriteAccessAllowed
          messageChatBoost
          messageUsersShared
          messageChatShared
          telegaInternal)))

(defun telega-ins--special-replied-msg (msg &optional _attrs)
  "Inserter for the MSG's dependent message in context of special message."
  (let ((replied-msg (telega-msg--replied-message msg)))
    (cond ((or (null replied-msg) (eq replied-msg 'loading))
           ;; NOTE: if replied message is not available right now, it
           ;; will be fetched by `telega-msg--replied-message-fetch' later
           (telega-ins-i18n "lng_profile_loading"))
          ((telega--tl-error-p replied-msg)
           (telega-ins--with-face 'error
             (telega-ins (telega-i18n "lng_deleted_message"))))
          (t
           (telega-ins--with-attrs
               (list :max 20 :align 'left :elide t)
             (telega-ins--content-one-line replied-msg))))))

(defun telega-ins--special (msg)
  "Insert special message MSG.
Special messages are determined with `telega-msg-special-p'."
  (telega-ins (telega-symbol 'horizontal-bar)
              (telega-symbol 'horizontal-bar)
              "(")
  (let* ((content (plist-get msg :content))
         (sender (telega-msg-sender msg))
         (sender-name (when sender
                        (telega-msg-sender-title--special sender))))
    (cl-case (telega--tl-type content)
      (messageContactRegistered
       ;; I18N: action_user_registered -> joined Telegram
       (telega-ins-i18n "lng_action_user_registered" :from sender-name))
      (messageChatAddMembers
       ;; If sender matches
       (let ((user-ids (plist-get content :member_user_ids)))
         (if (and (= 1 (length user-ids))
                  (= (plist-get sender :id) (aref user-ids 0)))
             (telega-ins-i18n "lng_action_user_joined"
               :from sender-name)

           (telega-ins-i18n "lng_action_add_user"
             :from sender-name
             :user (mapconcat #'telega-msg-sender-title--special
                              (mapcar #'telega-user-get user-ids)
                              ", ")))))
      (messageChatJoinByLink
       (telega-ins-i18n "lng_action_user_joined_by_link" :from sender-name))
      (messageChatJoinByRequest
       (telega-ins-i18n "lng_action_user_joined_by_request" :from sender-name))
      (messageChatDeleteMember
       (let ((user (telega-user-get (plist-get content :user_id))))
         (if (eq sender user)
             (telega-ins-i18n "lng_action_user_left" :from sender-name)
           (telega-ins-i18n "lng_action_kick_user"
             :from sender-name
             :user (telega-msg-sender-title--special user)))))

      (messageChatChangeTitle
       ;; I18N:
       ;; action_changed_title_channel -> Channel renamed to "{title}"
       ;; action_changed_title         -> {from} renamed group to "{title}"
       (if (plist-get msg :is_channel_post)
           (telega-ins-i18n "lng_action_changed_title_channel"
             :title (telega-tl-str content :title))
         (telega-ins-i18n "lng_action_changed_title"
           :from sender-name
           :title (telega-tl-str content :title))))

      (messageSupergroupChatCreate
       ;; TODO: I18N
       (telega-ins (if (plist-get msg :is_channel_post)
                       "Channel" "Supergroup"))
       (telega-ins " \"" (telega-tl-str content :title) "\" created"))
      (messageBasicGroupChatCreate
       (telega-ins-i18n "lng_action_created_chat"
         :from sender-name :title (telega-tl-str content :title)))
      (messageCustomServiceAction
       (telega-ins (telega-tl-str content :text)))
      (messageChatSetTtl
       (telega-ins-fmt "messages TTL set to %s"
         (telega-duration-human-readable (plist-get content :ttl))))
      (messageExpiredPhoto
       (telega-ins-i18n "lng_ttl_photo_expired"))
      (messageExpiredVideo
       (telega-ins-i18n "lng_ttl_video_expired"))
      (messageExpiredVoiceNote
       (telega-ins-i18n "lng_ttl_voice_expired"))
      (messageExpiredVideoNote
       (telega-ins-i18n "lng_ttl_round_expired"))
      (messageChatChangePhoto
       (let ((animated-p (telega--tl-get content :photo :animation)))
         (if (plist-get msg :is_channel_post)
             (telega-ins-i18n (if animated-p
                                  "lng_action_changed_video_channel"
                                "lng_action_changed_photo_channel"))
           (telega-ins-i18n (if animated-p
                                "lng_action_changed_video"
                              "lng_action_changed_photo")
             :from sender-name))
         (telega-ins--photo (plist-get content :photo) msg
                            '(3 1 3 1))))
      (messageChatDeletePhoto
       (if (plist-get msg :is_channel_post)
           (telega-ins-i18n "lng_action_removed_photo_channel")
         (telega-ins-i18n "lng_action_removed_photo"
           :from sender-name)))
      (messageChatUpgradeTo
       (telega-ins-i18n "telega_action_group_upgrade_to"
         :from sender-name))
      (messageChatUpgradeFrom
       (telega-ins-i18n "telega_action_group_upgrade_from"
         :from sender-name))
      (messageScreenshotTaken
       (cl-assert sender)
       (if (telega-me-p sender)
           (telega-ins-i18n "lng_action_you_took_screenshot")
         (telega-ins-i18n "lng_action_took_screenshot"
           :from sender-name)))
      (messagePinMessage
       (telega-ins-i18n "lng_action_pinned_message"
         :from (or sender-name "Message")
         :text (telega-ins--as-string
                (telega-ins--special-replied-msg msg))))
      (messageGameScore
       ;; NOTE: if game message is not available right now, it will
       ;; be fetched by `telega-msg--replied-message-fetch' later
       (let* ((game-msg (telega-msg--replied-message msg))
              (game-title (unless (eq game-msg 'loading)
                            (telega-tl-str
                             (telega--tl-get game-msg :content :game)
                             :title))))
         (if game-title
             (if (telega-me-p sender)
                 (telega-ins-i18n "lng_action_game_you_scored"
                   :count (plist-get content :score)
                   :game (propertize game-title 'face 'bold))
               (telega-ins-i18n "lng_action_game_score"
                 :from sender-name
                 :count (plist-get content :score)
                 :game (propertize game-title 'face 'bold)))
           (if (telega-me-p sender)
               (telega-ins-i18n "lng_action_game_you_scored_no_game"
                 :count (plist-get content :score))
             (telega-ins-i18n "lng_action_game_score_no_game"
               :from sender-name
               :count (plist-get content :score))))))
      (messageProximityAlertTriggered
       (let ((traveler (telega-msg-sender (plist-get content :traveler_id)))
             (watcher (telega-msg-sender (plist-get content :watcher_id)))
             (distance (plist-get content :distance)))
         (telega-ins-i18n "lng_action_proximity_reached"
           :from (telega-msg-sender-title--special traveler)
           :user (telega-msg-sender-title--special watcher)
           :distance (telega-i18n (if (> distance 1000)
                                      "lng_action_proximity_distance_km"
                                    "lng_action_proximity_distance_m")
                       :count (if (> distance 1000)
                                  (/ (float distance) 1000)
                                distance)))
         ))
      (messageVideoChatScheduled
       (telega-ins-i18n (if (plist-get msg :is_channel_post)
                            "lng_action_group_call_scheduled_channel"
                          "lng_action_group_call_scheduled_group")
         :from sender-name
         :date (telega-ins--as-string
                (telega-ins--date (plist-get content :start_date) 'date-time))))
      (messageVideoChatStarted
       (telega-ins-i18n "lng_action_group_call_started_group"
         :from sender-name))
      (messageVideoChatEnded
       (telega-ins-i18n "lng_action_group_call_finished_group"
         :from sender-name
         :duration (telega-duration-human-readable
                    (plist-get content :duration))))
      (messageInviteVideoChatParticipants
       (telega-ins-i18n "lng_action_invite_users_many"
         :from sender-name
         :users (mapconcat #'telega-msg-sender-title--special
                           (mapcar #'telega-user-get
                                   (plist-get content :user_ids))
                           ", ")
         :chat (telega-i18n "lng_action_invite_user_chat")))
      (messageChatSetTheme
       (let ((theme (telega-tl-str content :theme_name))
             (sender-me-p (telega-me-p sender)))
         (cond ((and sender-me-p theme)
                (telega-ins-i18n "lng_action_you_theme_changed"
                  :emoji theme))
               (sender-me-p
                (telega-ins-i18n "lng_action_you_theme_disabled"))
               (theme
                (telega-ins-i18n "lng_action_theme_changed"
                  :from sender-name :emoji theme))
               (t
                (telega-ins-i18n "lng_action_theme_disabled"
                  :from sender-name)))))
      (messageChatSetMessageAutoDeleteTime
       (let* ((sender-me-p (telega-me-p sender))
              (auto-del-time (plist-get content :message_auto_delete_time)))
         (if (zerop auto-del-time)
             (if sender-me-p
                 (telega-ins-i18n "lng_action_ttl_removed_you")
               (telega-ins-i18n "lng_action_ttl_removed"
                 :from sender-name))
           (if sender-me-p
               (telega-ins-i18n "lng_action_ttl_changed_you"
                 :duration (telega-duration-human-readable
                            auto-del-time 1 'long))
             (telega-ins-i18n "lng_action_ttl_changed"
               :from sender-name
               :duration (telega-duration-human-readable
                          auto-del-time 1 'long))))))
      (messagePaymentSuccessful
       (telega-ins-i18n (if (plist-get content :is_recurring)
                            "lng_action_payment_init_recurring"
                          "lng_action_payment_done")
         :amount (let ((currency (telega-tl-str content :currency)))
                   (propertize
                    (format "%.2f%s" (/ (plist-get content :total_amount) 100.0)
                            (or (cdr (assoc currency
                                            telega-currency-symbols-alist))
                                currency))
                    'face 'bold))
         :user (let ((from-user (telega-chat-user
                                 (telega-chat-get
                                  (plist-get content :invoice_chat_id)))))
                 (telega-msg-sender-title--special from-user))))
      (messageForumTopicCreated
       (telega-ins sender-name (telega-symbol 'right-arrow))
       (telega-ins-i18n "lng_action_topic_created"
         :topic (telega-ins--as-string
                 (telega-ins--content-one-line msg))))
      (messageForumTopicIsClosedToggled
       (telega-ins sender-name (telega-symbol 'right-arrow))
       (telega-ins-i18n (if (plist-get content :is_closed)
                            "lng_action_topic_closed"
                          "lng_action_topic_reopened")
         :topic (telega-ins--as-string
                 (telega-ins--special-replied-msg msg))))
      (messageForumTopicEdited
       (let* ((edit-icon-p (plist-get content :edit_icon_custom_emoji_id))
              (new-icon-sticker (when edit-icon-p
                                  (telega-custom-emoji-get
                                   (plist-get content :icon_custom_emoji_id))))
              (new-name (telega-tl-str content :name)))
         (cond (new-name
                (telega-ins-i18n "lng_action_topic_renamed"
                  :from sender-name
                  :link (telega-ins--as-string
                         (telega-ins--special-replied-msg msg))
                  :title (telega-ins--as-string
                          (when new-icon-sticker
                            (telega-ins--sticker-image new-icon-sticker))
                          (telega-ins new-name))))
               (edit-icon-p
                (telega-ins-i18n "lng_action_topic_icon_changed"
                  :from sender-name
                  :link (telega-ins--as-string
                         (telega-ins--special-replied-msg msg))
                  :emoji (telega-ins--as-string
                          (when new-icon-sticker
                            (telega-ins--sticker-image new-icon-sticker)))))
               (t
                (telega-ins "unsupported topic edit message")))))
      (messagePremiumGiveawayCreated
       (telega-ins-i18n "lng_action_giveaway_started"
         :from sender-name))
      (messagePremiumGiveawayCompleted
       (telega-ins-i18n "lng_action_giveaway_results"
         :count (plist-get content :winner_count)))
      (messageGiftedPremium
       (let* ((gifter-user-id (plist-get content :gifter_user_id))
              (gifter (unless (telega-zerop gifter-user-id)
                        (telega-user-get gifter-user-id)))
              (sticker (plist-get content :sticker))
              (currency (plist-get content :currency))
              (cost
               (format "%.2f%s " (/ (plist-get content :amount) 100.0)
                       (or (cdr (assoc currency telega-currency-symbols-alist))
                           currency))))
         (when sticker
           (telega-ins--image
            (telega-sticker--image sticker
                                   #'telega-sticker--create-image-one-line
                                   :telega-sticker-gift-1)))
         (if (telega-me-p sender)
             (telega-ins-i18n "lng_action_gift_received_me"
               :user (telega-msg-sender-title--special
                      (telega-chat-user (telega-msg-chat msg)))
               :cost (propertize cost 'face 'bold))
           (telega-ins-i18n "lng_action_gift_received"
             :user sender-name
             :cost (propertize cost 'face 'bold)))))
      (messageChatSetBackground
       (let ((only-self-p (plist-get content :only_for_self))
             (sender-me-p (telega-me-p sender)))
         (cond ((and sender-me-p only-self-p)
                (telega-ins-i18n "lng_action_set_wallpaper_me"))
               (sender-me-p
                (telega-ins-i18n "lng_action_set_wallpaper_both_me"
                  :user (let ((user (telega-chat-user (telega-msg-chat msg))))
                          (telega-msg-sender-title--special user))))
               (t
                (telega-ins-i18n "lng_action_set_wallpaper"
                  :user sender-name)))))

      (messageBotWriteAccessAllowed
       (let ((reason (plist-get content :reason)))
         (cl-ecase (telega--tl-type reason)
           (botWriteAccessAllowReasonConnectedWebsite
            (telega-ins-i18n "lng_action_bot_allowed_from_domain"
              :domain (propertize (telega-tl-str reason :domain_name)
                                  'face 'bold)))
           (botWriteAccessAllowReasonAddedToAttachmentMenu
            (telega-ins-i18n "lng_action_attach_menu_bot_allowed"))
           (botWriteAccessAllowReasonLaunchedWebApp
            (let ((web-app (plist-get reason :web_app)))
              (telega-ins-i18n "lng_action_bot_allowed_from_app"
                :app (propertize (or (telega-tl-str web-app :title)
                                     (telega-tl-str web-app :short_name))
                                 'face 'bold))))
           (botWriteAccessAllowReasonAcceptedRequest
            (telega-ins "You allowed this bot to message you"))
            )))
      (messageChatBoost
       (telega-ins-i18n "lng_action_boost_apply"
         :count (plist-get content :boost_count)
         :from sender-name))
      (messageUsersShared
       (let ((shared-users
              (mapcar #'telega-user-get
                      (mapcar (telega--tl-prop :user_id)
                              (plist-get content :users)))))
         (telega-ins-i18n "lng_action_shared_chat_with_bot"
           :chat (mapconcat #'telega-msg-sender-title--special
                            shared-users ", ")
           :bot (telega-msg-sender-title--special
                 (telega-chat-user (telega-msg-chat msg))))))
      (messageChatShared
       (let ((shared-chat
              (telega-chat-get (telega--tl-get content :chat :chat_id))))
         (telega-ins-i18n "lng_action_shared_chat_with_bot"
           :chat (telega-msg-sender-title--special shared-chat
                   :with-brackets-p t)
           :bot (telega-msg-sender-title--special
                 (telega-chat-user (telega-msg-chat msg))))))

      (telegaInternal
       (telega-ins--fmt-text (plist-get content :text)))

      (t (telega-ins-fmt "<unsupported special message: %S>"
           (telega--tl-type content)))))
  (telega-ins ")"
              (telega-symbol 'horizontal-bar)
              (telega-symbol 'horizontal-bar)))

(defun telega-ins--content (msg)
  "Insert message's MSG content."
  (when-let ((scheduled (plist-get msg :scheduling_state)))
    (telega-ins (telega-symbol 'alarm) " ")
    (telega-ins--with-face 'telega-shadow
      (telega-ins-i18n "telega_scheduled"))
    (telega-ins " ")
    (if-let ((send-date (plist-get scheduled :send_date)))
        (telega-ins-i18n "telega_scheduled_at_date"
          :date (telega-ins--as-string
                 (telega-ins--date send-date 'date-time)))
      (telega-ins-i18n "telega_scheduled_when_online"))
    (telega-ins "\n"))

  (let* ((telega-msg--current msg)
         (content (plist-get msg :content))
         (translated (plist-get msg :telega-translated))
         (translated-replaces-p (and translated
                                     (not (plist-get translated :loading))
                                     (with-telega-chatbuf (telega-msg-chat msg)
                                       telega-translate-replace-content))))
    (pcase (telega--tl-type content)
      ('messageText
       ;; NOTE: if text message is emojis only and no webpage is
       ;; attached, then display enlarged version according to
       ;; `telega-emoji-large-height'.
       (let ((web-page (telega--tl-get content :web_page))
             (emojis-text (when (and telega-emoji-use-images
                                     telega-emoji-large-height
                                     (telega-msg-emojis-only-p msg)
                                     (not (plist-get content :web_page)))
                            (telega--desurrogate-apply
                             (telega--tl-get content :text :text)))))
         (when (and web-page (plist-get web-page :show_above_text))
           (telega-ins--webpage msg web-page)
           (telega-ins "\n"))

         (cond (emojis-text
                (telega-ins--image-slices
                    (telega-emoji-create-svg
                     emojis-text telega-emoji-large-height)))
               ((and translated translated-replaces-p)
                (telega-ins (telega-tl-str translated :text)))
               (t
                (telega-ins--fmt-text (plist-get content :text) msg)))

         (when (and web-page (not (plist-get web-page :show_above_text)))
           (telega-ins "\n")
           (telega-ins--webpage msg web-page))))
      ('messageDocument
       (telega-ins--document msg))
      ('messageGame
       (telega-ins--game msg))
      ('messagePhoto
       (telega-ins--photo (plist-get content :photo)
                          msg nil telega-photo-show-details))
      ('messageSticker
       (let ((sticker (plist-get content :sticker)))
         (when (plist-get content :is_premium)
           (telega-ins (telega-symbol 'premium)))
         (cl-case (telega--tl-type (plist-get sticker :format))
           (stickerFormatTgs
            (telega-ins--with-face 'telega-shadow
              (telega-ins "Animated Sticker\n")))
           (stickerFormatWebm
            (telega-ins--with-face 'telega-shadow
              (telega-ins "Video Sticker\n"))))
         (telega-ins--sticker-image sticker 'slices)))
      ('messageAudio
       (telega-ins--audio msg))
      ('messageVideo
       (telega-ins--video msg))
      ('messageVoiceNote
       (telega-ins--voice-note msg))
      ('messageVideoNote
       (telega-ins--video-note msg))
      ('messageInvoice
       (telega-ins--invoice content))
      ('messagePoll
       (telega-ins--poll msg))
      ('messageAnimation
       (telega-ins--animation-msg msg))
      ('messageLocation
       (telega-ins--location-msg msg))
      ('messageVenue
       (telega-ins--location-msg msg 'venue))
      ('messageContact
       (telega-ins--contact-msg msg))
      ('messageCall
       (telega-ins--call-msg msg))
      ('messageDice
       (telega-ins--dice-msg-content content))
      ('messageAnimatedEmoji
       (telega-ins--animated-emoji-msg msg))
      ('messageStory
       (telega-ins--story-msg msg))
      ('messagePremiumGiveaway
       (telega-ins--giveaway-msg-content content))

      ;; special message
      ((guard (telega-msg-special-p msg))
       (telega-ins--special msg))

      (_ (telega-ins-fmt "<TODO: %S>"
                         (telega--tl-type content))))

    (when-let ((caption (plist-get content :caption)))
      (telega-ins-prefix "\n"
        (if (and translated translated-replaces-p)
            (telega-tl-str translated :text)
          (telega-ins--fmt-text caption msg))))

    ;; Translation
    (when (and translated (not translated-replaces-p))
      (telega-ins--with-face 'telega-shadow
        (telega-ins "\n")
        (telega-ins "--- Translation to "
                    (plist-get translated :to_language_code)
                    " ---\n")
        (if (plist-get translated :loading)
            (telega-ins "Translating...")
          (telega-ins (plist-get translated :text)))))
    ))

(defun telega-ins--keyboard-button (kbd-button msg &optional forced-width
                                               additional-action)
  "Insert inline KBD-BUTTON for the MSG.
If FORCED-WIDTH is used, then enlarge/shrink button to FORCED-WIDTH chars.
ADDITIONAL-ACTION function is called when button is pressed.
ADDITIONAL-ACTION is called with two args kbd-button and message."
  (declare (indent 3))
  (let* ((text (or (telega--tl-get kbd-button :telega-translated :text)
                   (telega-tl-str kbd-button :text)))
         (kbdb-text (if forced-width
                        (telega-ins--as-string
                         (telega-ins--with-attrs (list :min forced-width
                                                       :align 'center
                                                       :max forced-width)
                           (telega-ins text)))
                      text)))
    ;; NOTE: for "buy" buttons add credit card symbol
    (when (eq 'inlineKeyboardButtonTypeBuy
              (telega--tl-type (plist-get kbd-button :type)))
      (setq kbdb-text
            (concat kbdb-text
                    (propertize telega-symbol-credit-card
                                'display '((raise 0.5) (height 0.5))))))

    (telega-ins--box-button kbdb-text
      'action (lambda (_ignored)
                (telega-inline--callback kbd-button msg)
                (when additional-action
                  (funcall additional-action kbd-button msg)))
      :help-echo (cl-case (telega--tl-type kbd-button)
                   (inlineKeyboardButton
                    (telega-inline--help-echo kbd-button msg))
                   (keyboardButton
                    (substring (telega--tl-get kbd-button :type :@type) 18))))
    ))

(defun telega-ins--invoice-show-receipt (msg)
  "Insert [SHOW RECEIPT] button."
  (telega-ins--box-button "SHOW RECEIPT"
    'action (lambda (_ignored)
              (let* ((receipt-msg-id
                      (telega--tl-get msg :content :receipt_message_id))
                     (receipt
                      (telega-server--call
                       (list :@type "getPaymentReceipt"
                             :chat_id (plist-get msg :chat_id)
                             :message_id receipt-msg-id))))
                (message "receipt: %S" receipt)))))

(defun telega-ins--reply-markup (msg &optional force-keyboard)
  "Insert reply markup.
If FORCE-KEYBOARD is non-nil, then show reply markup even if it
has `replyMarkupShowKeyboard' type."
  ;; NOTE: for invoice messages with non-0 :receipt_message_id, show
  ;; [SHOW RECEIPT] button instead of markup
  (if (let ((content (plist-get msg :content)))
        (and (eq 'messageInvoice (telega--tl-type content))
             (not (zerop (or (plist-get content :receipt_message_id) 0)))))
      (telega-ins--invoice-show-receipt msg)

    (when-let ((reply-markup (plist-get msg :reply_markup))
               (reply-markup-type (telega--tl-type reply-markup)))
      (cl-assert (memq reply-markup-type '(replyMarkupForceReply
                                           replyMarkupInlineKeyboard
                                           replyMarkupShowKeyboard)))
      (when (or (eq reply-markup-type 'replyMarkupInlineKeyboard)
                (and force-keyboard
                     (eq reply-markup-type 'replyMarkupShowKeyboard)))
        (let ((rows (append (plist-get reply-markup :rows) nil)))
          (while rows
            (telega-ins--move-to-column 4)
            (let* ((buttons-row (car rows))
                   (forced-width (when (plist-get reply-markup :resize_keyboard)
                                   (/ (- telega-chat-fill-column 10
                                         (telega-current-column)
                                         (length buttons-row))
                                      (length buttons-row)))))
              (seq-doseq (kbd-button buttons-row)
                (telega-ins--keyboard-button kbd-button msg forced-width
                  (when (plist-get reply-markup :one_time)
                    (lambda (_kbdbutton _kbdmsg)
                      (telega--deleteChatReplyMarkup msg))))
                (telega-ins " ")))
            (when (setq rows (cdr rows))
              (telega-ins "\n"))))
        t))))

(defmacro telega-ins--aux-inline (prefix title suffix face &rest body)
  "Execute BODY inserters prefixing with TITLE.
PREFIX is displayed before TITLE.
SUFFIX is displayed after TITLE.
Display text using FACE."
  (declare (indent 4))
  `(telega-ins--with-attrs  (list :max (- telega-chat-fill-column
                                          (telega-current-column))
                                  :elide t
                                  :face ,face)
     (telega-ins ,prefix ,title ,suffix)
     (progn ,@body)
     (telega-ins "\n")))

(cl-defun telega-ins--aux-msg-topic-one-line (msg &key prefix)
  "Insert topic icon in case MSG belongs to a topic."
  (when-let ((topic (telega-msg-topic msg))
             (show-topic-p
              (or telega-msg-always-show-topic-info
                  (not (eq topic (telega-chatbuf--thread-topic))))))
    (telega-ins--with-face 'telega-shadow
      (telega-ins prefix (telega-symbol 'topic))
      (telega-ins--topic-icon topic))))

(cl-defun telega-ins--aux-msg-one-line (msg &key with-username
                                            username-face remove)
  "Insert contents for aux message MSG as one line.
If WITH-USERNAME is non-nil then insert MSG sender as well.
If WITH-USERNAME is `unread-mention', then outline sender with
`telega-mention-count' face.
If WITH-USERNAME is a string, then use it as title of the MSG sender.
USERNAME-FACE specifies face to use for sender's title.
If REMOVE is `message', then do not insert the message MSG content.
If REMOVE is `caption', then do not insert message's MSG caption."
  (declare (indent 1))
  (when (and with-username
             (telega-ins--with-face username-face
               (let ((sender (telega-msg-sender msg)))
                 (telega-ins (or (when (stringp with-username)
                                   with-username)
                                 (telega-msg-sender-username sender 'with-@)
                                 (telega-msg-sender-title sender))))))
    (telega-ins--aux-msg-topic-one-line msg :prefix (telega-symbol 'right-arrow))
    (telega-ins (telega-symbol 'sender-and-text-delim) " "))
  (unless (eq remove 'message)
    (telega-ins--content-one-line msg
      :remove-caption remove)))

(defun telega-ins--msg-interaction-info (msg &optional msg-chat)
  "Insert interaction info for message MSG.
MSG-CHAT is already calculated chat of the message, used for
performance."
  (unless msg-chat
    (setq msg-chat (telega-msg-chat msg)))

  (let* ((msg-ii (plist-get msg :interaction_info))
         (view-count (plist-get msg-ii :view_count))
         (fwd-count (plist-get msg-ii :forward_count))
         (reply-count (telega-msg-replies-count msg)))
    (when (and view-count (not (zerop view-count)))
      (telega-ins " " (telega-symbol 'eye)
                  (telega-number-human-readable view-count "%d")))
    (when (and fwd-count (not (zerop fwd-count)))
      (telega-ins " ")
      (let ((fwd-count-label (concat (telega-symbol 'forward)
                                     (int-to-string fwd-count))))
        (if (and (telega-chat-channel-p msg-chat)
                 (telega-chat-match-p msg-chat '(me-is-owner or-admin)))
            (telega-ins--text-button fwd-count-label
              'face 'telega-link
              :value msg
              :action #'telega-msg-public-forwards)
          (telega-ins fwd-count-label))))
    (when (and (plist-get msg :can_get_message_thread)
               (> reply-count 0))
      (telega-ins " ")
      (telega-ins--text-button
          (format "%s%d" (telega-symbol 'reply) reply-count)
        'face 'telega-link
        :action #'telega-msg-open-thread-or-topic
        :help-echo "Show message thread"))
    t))

(defun telega-ins--msg-comments (msg &optional msg-chat)
  "Insert \"Comments\" section for the message MSG."
  (when (and (plist-get msg :can_get_message_thread)
             (telega-chat-channel-p (or msg-chat (telega-msg-chat msg))))
    (let* ((msg-ri (telega--tl-get msg :interaction_info :reply_info))
           (reply-count (or (plist-get msg-ri :reply_count) 0))
           (recent-repliers (plist-get msg-ri :recent_replier_ids)))
      (telega-ins--box-button
          (telega-ins--as-string
           (if (zerop (length recent-repliers))
               (telega-ins (telega-symbol 'leave-comment))
             (seq-doseq (rr recent-repliers)
               (telega-ins--image (telega-msg-sender-avatar-image-one-line
                                   (telega-msg-sender rr)))))
           (telega-ins " " (if (zerop reply-count)
                               (telega-i18n "lng_comments_open_none")
                             (concat (telega-i18n "lng_comments_open_count"
                                       :count reply-count)
                                     (when (telega-msg-replies-has-unread-p msg)
                                       "•")))))
        ;; Use custom :action for clickable comments button
        :action #'telega-msg-open-thread-or-topic
        :help-echo (telega-i18n "lng_profile_view_discussion")))))

(defun telega-ins--message-date-and-status (msg)
  "Insert message's date and outgoing status."
  ;; NOTE: telegaInternal messages has no `:date' property
  (when-let ((date (or (telega--tl-get msg :scheduling_state :send_date)
                       (plist-get msg :date))))
    (telega-ins--date date)
    ;; NOTE: outgoing messages always has `:date' property
    (telega-ins--outgoing-status msg)
    t))

(defun telega-ins--message-header (msg &optional msg-chat msg-sender
                                       addon-inserter)
  "Insert message's MSG header, everything except for message content.
MSG-CHAT - Chat for which to insert message header.
MSG-SENDER - Sender of the message.
If ADDON-INSERTER function is specified, it is called with one
argument - MSG to insert additional information after header."
  (let* ((date-and-status (telega-ins--as-string
                           (when telega-msg-heading-with-date-and-status
                             (telega-ins--message-date-and-status msg))))
         (dwidth (- telega-chat-fill-column
                    (string-width date-and-status)))
         (chat (or msg-chat (telega-msg-chat msg)))
         (sender (or msg-sender (telega-msg-sender msg))))
    (cl-assert sender)
    (telega-ins--with-props
        (list 'action (lambda (button)
                        ;; NOTE: check for custom message :action first
                        ;; - [RESEND] button uses :action
                        ;; - via @bot link uses :action
                        (or (telega-button--action button)
                            (telega-describe-msg-sender sender))))
      (telega-ins--with-face 'telega-msg-heading
        (telega-ins--with-attrs (list :max (- dwidth (telega-current-column))
                                      :align 'left
                                      :elide t
                                      :elide-trail 20)
          ;; NOTE: if channel post has a signature, then use it instead
          ;; of username to shorten message header
          (let ((signature (telega-tl-str msg :author_signature)))
            (telega-ins--msg-sender sender
              :with-username-p (not signature))
            (when signature
              (telega-ins--with-face (telega-msg-sender-title-faces sender)
                (telega-ins " --" signature))))

          ;; Admin badge if any
          (when (telega-user-p sender)
            (when-let ((admin (telega-chat-admin-get chat sender)))
              (telega-ins--with-face 'telega-shadow
                (telega-ins " ("
                            (or (telega-tl-str admin :custom_title)
                                (if (plist-get admin :is_owner)
                                    (telega-i18n "lng_owner_badge")
                                  (telega-i18n "lng_admin_badge")))
                            ")"))))

          ;; via <bot>
          (when-let* ((via-bot-user-id (plist-get msg :via_bot_user_id))
                      (via-bot (unless (zerop via-bot-user-id)
                                 (telega-user-get via-bot-user-id)))
                      (bot-title (telega-ins--as-string
                                  ;; Use custom :action for clickable @bot link
                                  (telega-ins--text-button
                                      (telega-user-title via-bot 'username)
                                    'face 'telega-username
                                    :action (lambda (_msg_ignored)
                                              (telega-describe-user via-bot))))))
            (telega-ins " " (telega-i18n "lng_inline_bot_via"
                              :inline_bot bot-title)))

          ;; Edited date
          (let ((edited-date (plist-get msg :edit_date)))
            (unless (zerop edited-date)
              (telega-ins " " (telega-i18n "lng_edited") " ")
              (telega-ins--date (plist-get msg :edit_date))))

          ;; Interaction info
          (telega-ins--msg-interaction-info msg chat)

          (when-let ((fav (telega-msg-favorite-p msg)))
            (telega-ins " " (telega-symbol 'favorite))
            ;; Also show comment to the favorite message
            (telega-ins--with-face 'telega-shadow
              (telega-ins-prefix "("
                (when (telega-ins (plist-get fav :comment))
                  (telega-ins ")")))))

          ;; Maybe pinned message?
          (when (plist-get msg :is_pinned)
            (telega-ins " " (telega-symbol 'pin)))

          ;; Copyright if can't be saved
          (unless (plist-get msg :can_be_saved)
            (telega-ins " " (telega-symbol 'copyright)))

          ;; message auto-deletion time
          (let ((auto-delete-in (plist-get msg :auto_delete_in)))
            (unless (telega-zerop auto-delete-in)
              (telega-ins " " (telega-symbol 'flames)
                          (telega-duration-human-readable auto-delete-in 1))))

          ;; Show language code if translation replaces message's content
          (when-let ((translated (plist-get msg :telega-translated)))
            (when (with-telega-chatbuf chat
                    telega-translate-replace-content)
              (telega-ins--with-face 'telega-shadow
                (telega-ins " ["
                            (telega-symbol 'right-arrow)
                            (plist-get translated :to_language_code)
                            "]"))))

          (when (numberp telega-debug)
            (telega-ins-fmt " (ID=%d)" (plist-get msg :id)))

          ;; Resend button in case message sent failed
          ;; Use custom :action to resend message
          (when-let ((send-state (plist-get msg :sending_state)))
            (when (and (eq (telega--tl-type send-state)
                           'messageSendingStateFailed)
                       (plist-get send-state :can_retry))
              (telega-ins " ")
              (telega-ins--box-button "RESEND"
                :action #'telega--resendMessages)))

          (when addon-inserter
            (cl-assert (functionp addon-inserter))
            (funcall addon-inserter msg))

          ;; Message's topic aligned to the right
          (when-let* ((topic (telega-msg-topic msg))
                      (show-topic-p (or telega-msg-always-show-topic-info
                                        (not (telega-chatbuf--thread-topic))))
                      (topic-title (telega-ins--as-string
                                    (telega-ins (telega-symbol 'right-arrow)
                                                (telega-symbol 'topic))
                                    (telega-ins--topic-icon topic)
                                    (telega-ins--topic-title topic))))
            (telega-ins--move-to-column
             (- dwidth (string-width topic-title)))
            (telega-ins--with-props
                (list 'face 'telega-topic-button
                      :action #'telega-msg-show-topic-info
                      :help-echo "Show topic info")
              (telega-ins topic-title))))

        (when date-and-status
          (telega-ins--move-to-column dwidth)
          (telega-ins date-and-status))

        (telega-ins "\n")))))

(defun telega--msg-origin-sender (origin)
  "Return message sender extracted from the ORIGIN.
Return user, chat or string with the sender title."
  (cl-ecase (telega--tl-type origin)
    (messageOriginChat
     (telega-chat-get (plist-get origin :sender_chat_id)))

    (messageOriginUser
     (telega-user-get (plist-get origin :sender_user_id)))

    (messageOriginHiddenUser
     (telega-tl-str origin :sender_name))

    (messageOriginChannel
     (telega-chat-get (plist-get origin :chat_id)))))

(cl-defun telega-ins--msg-sender-chat-date (sender &key from-chat-id
                                                   topic date signature)
  "Insert SENDER --SIGNATURE → CHAT#TOPIC at DATE."
  (declare (indent 1))

  (if (stringp sender)
      (telega-ins--with-face 'telega-shadow
        (telega-ins sender))
    (telega-ins--msg-sender sender
      :with-avatar-p t
      :with-username-p t
      :with-brackets-p t))

  (when signature
    (telega-ins " --" signature))

  (unless (telega-zerop from-chat-id)
    (let ((from-chat (telega-chat-get from-chat-id)))
      (unless (or (eq sender from-chat)
                  (eq sender (telega-chat-user from-chat)))
        (telega-ins (telega-symbol 'right-arrow))
        (if telega-chat-show-avatars
            (telega-ins--image
             (telega-msg-sender-avatar-image-one-line from-chat))
          (telega-ins--msg-sender from-chat
            :with-avatar-p t
            :with-username-p t
            :with-brackets-p t)))))

  (when topic
    (telega-ins--with-face 'telega-shadow
      (telega-ins (telega-symbol 'topic))
      (telega-ins--topic-icon topic)))

  (unless (telega-zerop date)
    (telega-ins " " (telega-i18n "lng_schedule_at") " ")
    (telega-ins--date date))
  t)

(defun telega--fwd-info-action (fwd-info)
  "Action to take when button with FWD-INFO displayed is clicked."
  (let* ((origin (plist-get fwd-info :origin))
         (origin-chat-id (plist-get origin :chat_id))
         (origin-msg-id (plist-get origin :message_id))
         (origin-sender-id (plist-get origin :sender_user_id))
         (from-chat-id (telega--tl-get fwd-info :source :chat_id))
         (chat-id (if (not (telega-zerop from-chat-id))
                      from-chat-id
                    origin-chat-id))
         (from-msg-id (telega--tl-get fwd-info :source :message_id))
         (msg-id (if (not (telega-zerop from-msg-id))
                     from-msg-id
                   origin-msg-id)))
    (cond ((and chat-id msg-id (not (zerop chat-id)) (not (zerop msg-id)))
           ;; NOTE: if we have active chatbuffer for private channel -
           ;; try goto message, it could work in this case
           (let ((chat (telega-chat-get chat-id)))
             (if (and (not (with-telega-chatbuf chat t))
                      (telega-chat-match-p chat
                        '(and (type channel) (not is-public))))
                 (error (concat
                         "Telega: " (telega-i18n "lng_channel_not_accessible")))
               (telega-chat--goto-msg chat msg-id t))))
          ((and origin-sender-id (not (zerop origin-sender-id)))
           (telega-describe-user (telega-user-get origin-sender-id))))))

(defun telega-ins--fwd-info-inline (fwd-info)
  "Insert forward info FWD-INFO as one liner."
  (when fwd-info
    (telega-ins--with-props
        ;; When pressed, then jump to original message or show info
        ;; about original sender
        (list 'action
              (lambda (_button) (telega--fwd-info-action fwd-info))
              'help-echo "RET to goto original message")
      (telega-ins--with-attrs  (list :max (- telega-chat-fill-column
                                             (telega-current-column))
                                     :elide t
                                     :elide-trail 8
                                     :face 'telega-msg-inline-forward)
        ;; | Forwarded From:
        (telega-ins (telega-symbol 'vertical-bar))
        (if (memq 'forward telega-chat-aux-inline-symbols)
            (telega-ins (telega-symbol 'forward) " ")
          (telega-ins-i18n "lng_forwarded" :user ""))
        (let ((origin (plist-get fwd-info :origin))
              (source (plist-get fwd-info :source)))
          (telega-ins--msg-sender-chat-date
              (or (when-let ((src-sender (plist-get source :sender_id)))
                    (telega-msg-sender src-sender))
                  (telega--msg-origin-sender origin))
            :from-chat-id (plist-get source :chat_id)
            :signature (telega-tl-str origin :author_signature)
            :date (or (plist-get source :date)
                      (plist-get fwd-info :date))))
        (telega-ins "\n")))))

(defun telega-ins--msg-reply-to-message-inline (msg &optional reply-to)
  "Inline reply to a message."
  (unless reply-to
    (setq reply-to (plist-get msg :reply_to)))

  ;; If replied message is not instantly available, it will be fetched
  ;; later by the `telega-msg--replied-message-fetch'
  (let* ((replied-msg (telega-msg--replied-message msg))
         (reply-quote (plist-get reply-to :quote))
         (content (or (plist-get reply-to :content)
                      (plist-get replied-msg :content)))
         (origin (plist-get reply-to :origin)))
    ;; Sender and content part
    (telega-ins--aux-inline
        (telega-symbol 'vertical-bar)
        (telega-chat--aux-inline-reply-symbol
         (plist-get reply-quote :is_manual))
        " "
        'telega-msg-inline-reply
      (cond (origin
             (telega-ins--msg-sender-chat-date (telega--msg-origin-sender origin)
               :from-chat-id (plist-get reply-to :chat_id)
               :signature (telega-tl-str origin :signature)
               :topic (telega-msg-topic replied-msg)
               :date (plist-get reply-to :origin_send_date)))
            ((or (null replied-msg) (eq replied-msg 'loading))
             ;; NOTE: replied message will be fetched by the
             ;; `telega-msg--replied-message-fetch'
             (telega-ins-i18n "lng_profile_loading"))
            ((telega--tl-error-p replied-msg)
             (telega-ins--with-face 'telega-shadow
               (telega-ins (telega-i18n "lng_deleted_message"))))
            ((telega-msg-match-p replied-msg 'ignored)
             (telega-ins--message-ignored replied-msg)
             ;; NOTE: Never show content and/or quote from ignored message
             (setq reply-quote nil
                   content nil))
            (t
             ;; NOTE: If forwarded message replies to a forwarded
             ;; message, then use fwd-info origin sender to resemble
             ;; origin thread
             (let* ((fwd-origin
                     (when (plist-get msg :forward_info)
                       (telega--tl-get replied-msg :forward_info :origin)))
                    (sender
                     (if fwd-origin
                         (telega--msg-origin-sender fwd-origin)
                       (telega-msg-sender replied-msg)))
                    (sender-faces
                     (if (stringp sender)
                         (list 'telega-msg-user-title)
                       (telega-msg-sender-title-faces sender))))
               ;; Add special face if message contains unread mention
               (when (and (not (stringp sender))
                          (telega-sender-match-p sender 'me)
                          (plist-get replied-msg :contains_unread_mention))
                 (setq sender-faces (append sender-faces
                                            '(telega-entity-type-mention))))
               (telega-ins--with-face sender-faces
                 (telega-ins (or (when (stringp sender) sender)
                                 (telega-msg-sender-username sender 'with-@)
                                 (telega-msg-sender-title sender)))))

             ;; If message is replied in the same topic, then there is
             ;; no need show topic icon
             (unless (eq (telega-msg-topic msg)
                         (telega-msg-topic replied-msg))
               (telega-ins--aux-msg-topic-one-line replied-msg))))

      (when content
        (telega-ins (telega-symbol 'sender-and-text-delim) " ")
        (telega-ins--content-one-line replied-msg
          :content content)))

    (when reply-quote
      (telega-ins--line-wrap-prefix (telega-symbol 'vertical-bar)
        (telega-ins--with-face 'telega-entity-type-blockquote
          (telega-ins--fmt-text (plist-get reply-quote :text) replied-msg))
        (telega-ins "\n")))
    t))

(defun telega-ins--msg-reply-inline (msg)
  "For message MSG insert reply header in case MSG is replying to some message."
  (when-let ((reply-to (plist-get msg :reply_to)))
    (cl-ecase (telega--tl-type reply-to)
      (messageReplyToMessage
       ;; NOTE: Do not show reply header in case message replied to the
       ;; thread's root message
       (unless (when-let ((thread-msg (telega-chatbuf--thread-msg)))
                 (and (eq (plist-get thread-msg :chat_id)
                          (plist-get reply-to :chat_id))
                      (eq (plist-get thread-msg :id)
                          (plist-get reply-to :message_id))))
         (telega-ins--with-props '(:action telega-msg-goto-reply-to-message)
           (telega-ins--msg-reply-to-message-inline msg reply-to))))

      (messageReplyToStory
       ;; NOTE: If replied story is not instantly available, it will
       ;; be fetched later by the `telega-msg--replied-story-fetch'
       (telega-ins--aux-inline
           (telega-symbol 'vertical-bar)
           (telega-chat--aux-inline-reply-symbol)
           " "
           'telega-msg-inline-reply
         (let ((replied-story (telega-msg--replied-story msg)))
           (cond ((or (null replied-story) (eq replied-story 'loading))
                  ;; NOTE: replied story will be fetched by the
                  ;; `telega-msg--replied-story-fetch'
                  (telega-ins-i18n "lng_profile_loading"))
                 ((or (telega--tl-error-p replied-story)
                      (telega-story-deleted-p replied-story))
                  (telega-ins--with-face 'telega-shadow
                    (telega-ins (telega-i18n "lng_deleted_story"))))
                 (t
                  (telega-ins--with-props
                      ;; When pressed, open the replied story
                      (list 'action
                            (lambda (_button)
                              (telega-story-open replied-story msg)))
                    (telega-ins--my-story-one-line replied-story msg)))
                 )))))
    ))

(defun telega-ins--msg-sending-state-failed (msg)
  "Insert sending state failure reason for message MSG."
  (when-let ((send-state (plist-get msg :sending_state)))
    (when (eq 'messageSendingStateFailed (telega--tl-type send-state))
      (telega-ins (telega-symbol 'failed))
      (telega-ins--with-face 'error
        (telega-ins "Failed to send: "
                    (telega-tl-str (plist-get send-state :error) :message)))
      (cond ((and (telega-msg-match-p msg '(type Photo))
                  (equal (telega-tl-str (plist-get send-state :error) :message)
                         "PHOTO_INVALID_DIMENSIONS"))
             ;; NOTE: Resending as file will accomplish without errors
             (when-let ((photofile
                         (plist-get (cl-find "i" (telega--tl-get
                                                  msg :content :photo :sizes)
                                             :test #'equal
                                             :key (telega--tl-prop :type))
                                    :photo))
                        (caption (telega--tl-get msg :content :caption)))
               (telega-ins " ")
               (telega-ins--box-button "RESEND as file"
                 'action (lambda (_button)
                           (message "TODO: resend as file")))))
            )
      t)))

(cl-defun telega-ins--message0 (msg &key no-header sender addon-header-inserter)
  "Insert message MSG.
If NO-HEADER is non-nil, then do not display message header
unless message is edited.
ADDON-HEADER-INSERTER is passed directly to `telega-ins--message-header'."
  (declare (indent 1))
  (if (telega-msg-special-p msg)
      ;; Align message at the center using `horizontal-bar' symbol
      (let ((start-pos (point)))
        (telega-ins--content msg)
        (let* ((end-column (telega-current-column))
               (align-nchars (/ (- telega-chat-fill-column
                                   end-column)
                                2)))
          (save-excursion
            (goto-char start-pos)
            (dotimes (_ align-nchars)
              (telega-ins (telega-symbol 'horizontal-bar))))
          (dotimes (_ align-nchars)
            (telega-ins (telega-symbol 'horizontal-bar)))
          ))

    ;; Message header needed
    (let* ((chat (telega-msg-chat msg))
           ;; NOTE: `telega-msg--current' is used
           ;; inside `telega--entity-type-to-text-props'
           (telega-msg--current msg)
           (fwd-info (plist-get msg :forward_info))
           (sender (or sender
                       ;; For "Replies" chat we need to show original sender.
                       ;; Workaround for case when `:forward_info' is unset (for
                       ;; outgoing messages [what?] for example)
                       (if (and (telega-replies-p chat) fwd-info)
                           (telega--msg-origin-sender
                            (plist-get fwd-info :origin))
                         (telega-msg-sender msg))))
           (avatar (telega-msg-sender-avatar-image sender))
           ;; NOTE: Text for the first slice might be customized via
           ;; `telega-avatar-text-function', so we use text for the
           ;; second slice to measure avatar width
           (awidth (length (telega-image--telega-text avatar 1)))
           (gaps-workaround-p
            (telega-chat-match-p chat telega-avatar-workaround-gaps-for))
           (unread-mention-p
            (plist-get msg :contains_unread_mention))
           (l1width (if unread-mention-p
                        (string-width (telega-symbol 'mention-mark))
                      0))
           (header-prefix (when unread-mention-p
                            (telega-symbol 'mention-mark)))
           (content-prefix (make-string l1width ?\s))
           (content-wrap (make-string (+ awidth l1width) ?\s)))

      (if (and no-header
               (zerop (plist-get msg :edit_date))
               (zerop (plist-get msg :via_bot_user_id)))
          (setq content-prefix (concat header-prefix (make-string awidth ?\s)))

        ;; Header is required, set it up
        (setq header-prefix
              (concat header-prefix
                      (telega-ins--as-string
                       (telega-ins--image
                        avatar (if gaps-workaround-p
                                   (list 0 0 (telega-chars-xheight 2))
                                 0)
                        :no-display-if (not telega-chat-show-avatars))))
              content-prefix
              (if gaps-workaround-p
                  wrap-prefix
                (concat content-prefix
                        (telega-ins--as-string
                         (telega-ins--image
                          avatar 1
                          :no-display-if (not telega-chat-show-avatars))))))
        (telega-ins--line-wrap-prefix (cons header-prefix nil)
          (telega-ins--message-header msg chat sender addon-header-inserter)))

      ;; Message Content
      (telega-ins--line-wrap-prefix (cons content-prefix content-wrap)
        (telega-ins--fwd-info-inline fwd-info)
        (telega-ins--msg-reply-inline msg)
        (telega-ins--content msg)

        (telega-ins-prefix "\n"
          (telega-ins--msg-sending-state-failed msg)))

      (when (telega-msg-match-p msg telega-msg-temex-show-reactions)
        (setq content-prefix
              (if (telega-msg-match-p msg 'unread-reactions)
                  (let ((reaction-prefix
                         (propertize (telega-symbol 'reaction-mark)
                                     'face (if (telega-chat-muted-p chat)
                                               'telega-muted-count
                                             'telega-unmuted-count))))
                    (concat reaction-prefix
                            (substring content-wrap
                                       (string-width reaction-prefix))))
                content-wrap))
        (telega-ins-prefix "\n"
          (telega-ins--line-wrap-prefix (cons content-prefix content-wrap)
            (if (telega-msg-match-p msg '(chat saved-messages))
                (telega-ins--msg-saved-messages-tags msg)
              (telega-ins--msg-reaction-list msg)))))

      (telega-ins--line-wrap-prefix content-wrap
        (telega-ins-prefix "\n"
          (telega-ins--reply-markup msg))
        (telega-ins-prefix "\n"
          (telega-ins--msg-comments msg chat))
        ))

    (unless telega-msg-heading-with-date-and-status
      (let* ((date-and-status (telega-ins--as-string
                               (telega-ins--message-date-and-status msg)))
             (dswidth (string-width date-and-status))
             (dsoffset 2))              ;XXX
        (when (> (telega-current-column)
                 (- telega-chat-fill-column dswidth dsoffset))
          (telega-ins "\n"))
        (telega-ins--move-to-column (- telega-chat-fill-column dswidth))
        (telega-ins date-and-status))))
  t)

(defun telega-ins--message-media-compact (msg &rest _ignored)
  "Insert for compact view of media messages."
  (let ((content (plist-get msg :content)))
    (cl-ecase (telega--tl-type content)
      (telegaInternal
       (telega-ins "\n")
       (telega-ins--message0 msg)
       (telega-ins "\n"))

      (messagePhoto
       (telega-ins--image
        (telega-photo--image
         (plist-get content :photo) '(10 10 10 10))))
      )))

(defun telega-ins--message (msg &rest args)
  "Inserter for the message MSG.
Pass all ARGS directly to `telega-ins--message0'."
  (declare (indent 1))
  (if (telega-msg-marked-p msg)
      (telega-ins--line-wrap-prefix (telega-symbol 'mark)
        (apply #'telega-ins--message0 msg args))

    (apply #'telega-ins--message0 msg args)))

(defun telega-ins--message-no-header (msg)
  "Insert message MSG without header."
  (funcall telega-inserter-for-msg-button msg :no-header t))

(defun telega-ins--message-deleted (msg)
  "Inserter for deleted message MSG."
  (telega-ins--with-props (list 'face 'telega-msg-deleted)
    ;; NOTE: For ignored MSG use `telega-ins--message-ignored'
    ;; inserter.  See https://github.com/zevlg/telega.el/issues/342
    (if (telega-msg-match-p msg 'ignored)
        (when telega-ignored-messages-visible
          (telega-ins--message-ignored msg))

      (funcall telega-inserter-for-msg-button msg
               :addon-header-inserter
               (lambda (_ignoredmsg)
                 (telega-ins " ")
                 (telega-ins--with-face 'error
                   (telega-ins (telega-i18n "lng_deleted_message"))))))))

(defun telega-ins--message-ignored (msg)
  "Inserter for ignored message MSG in chatbuf."
  (if (functionp telega-ignored-messages-visible)
      (progn
        (cl-assert (not (eq telega-ignored-messages-visible
                            #'telega-ins--message-ignored)))
        (funcall telega-ignored-messages-visible msg))
    (telega-ins (propertize "<Ignored Message>" 'face 'telega-shadow))))

(defun telega-ins--message-with-chat-header (msg)
  "Inserter for message MSG showing chat header."
  (let ((telega-chat-fill-column telega-root-fill-column))
    (telega-ins--with-face 'telega-msg-heading
      (telega-ins--with-attrs (list :max telega-root-fill-column
                                    :align 'left)
        (telega-ins--date (plist-get msg :date))
        (telega-ins " ")
        (telega-ins--chat (telega-msg-chat msg))
        (telega-ins "\n")))

    (telega-ins--message msg
      :no-header (telega-msg-match-p msg '(chat (type channel))))))

(defun telega-ins--self-destruct-type (tl-ttl &optional short-p)
  "Inserter for the MessageSelfDestructType.
If SHORT-P is non-nil then use short version."
  (if short-p
      (telega-ins (telega-symbol 'flames))
    (telega-ins "self-destruct"))
  (unless short-p
    (telega-ins " "))
  (cl-ecase (telega--tl-type tl-ttl)
    (messageSelfDestructTypeImmediately
     (if short-p
         (telega-ins (telega-duration-human-readable 0))
       (telega-ins "immediately")))
    (messageSelfDestructTypeTimer
     (telega-ins (unless short-p "in ")
                 (telega-duration-human-readable
                  (plist-get tl-ttl :self_destruct_time))))))

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
      (telega-ins--contact (plist-get imc :contact)
        :with-avatar-p telega-user-show-avatars))
     (inputMessageDocument
      (telega-ins--input-file (plist-get imc :document)))
     (inputMessagePhoto
      (telega-ins--input-file
       (plist-get imc :photo) (telega-symbol 'photo)
       (let ((width (plist-get imc :width))
             (height (plist-get imc :height)))
         (concat " "
                 (when (and width height)
                   (format "%dx%d" width height))
                 (when-let ((tl-ttl (telega--tl-get imc :self_destruct_type)))
                   (telega-ins--as-string
                    (when (and width height)
                      (telega-ins ", "))
                    (telega-ins--self-destruct-type tl-ttl 'short)))))))
     (inputMessageAudio
      (let* ((title (plist-get imc :title))
             (artist (plist-get imc :performer))
             (audio-description (concat (when title
                                          (propertize title 'face 'bold))
                                        (when artist
                                          (concat (when title " ")
                                                  "--" artist)))))
        (telega-ins (telega-symbol 'audio) " ")
        (when (telega-ins audio-description)
          (telega-ins ","))
        (telega-ins--input-file (plist-get imc :audio) "")))
     (inputMessageVideo
      (let ((duration (or (plist-get imc :duration) 0))
            (width (plist-get imc :width))
            (height (plist-get imc :height)))
        (telega-ins--input-file
         (plist-get imc :video) (telega-symbol 'video)
         (concat " "
                 (when (and width height)
                   (format "%dx%d " width height))
                 (telega-duration-human-readable duration)
                 (when-let ((tl-ttl (telega--tl-get imc :self_destruct_type)))
                   (telega-ins--as-string
                    (when (and width height)
                      (telega-ins ", "))
                    (telega-ins--self-destruct-type tl-ttl 'short)))
                 ))))
     (inputMessageVoiceNote
      (let ((duration (or (plist-get imc :duration) 0))
            (waveform (plist-get imc :waveform)))
        (telega-ins "VoiceNote ")
        (when (and telega-use-images waveform)
          (telega-ins--image
           (telega-vvnote--waves-svg
            (telega-vvnote--waveform-decode waveform)
            (round (telega-chars-xheight
                    telega-vvnote-waves-height-factor))
            duration)))
        (telega-ins " (" (telega-duration-human-readable duration) ")")))
     (inputMessageVideoNote
      (telega-ins "VideoNote")
      (let ((duration (or (plist-get imc :duration) 0))
            (thumb-filename (telega--tl-get imc :thumbnail :thumbnail :path)))
        (when (and telega-use-images thumb-filename)
          (telega-ins " ")
          (telega-ins--image
           (let ((telega-video-note-height 1))
             (telega-vvnote-video--svg thumb-filename))))
        (telega-ins " (" (telega-duration-human-readable duration) ")")))
     (inputMessageSticker
      (telega-ins--input-file (plist-get imc :sticker) "Sticker"))
     (inputMessageAnimation
      (let ((duration (or (plist-get imc :duration) 0))
            (width (plist-get imc :width))
            (height (plist-get imc :height)))
        (telega-ins--input-file
         (plist-get imc :animation) "GIF"
         (concat " (" (when (and width height)
                       (format "%dx%d " width height))
                 (telega-duration-human-readable duration)
                 ")"))))
     (inputMessagePoll
      (telega-ins (telega-symbol 'poll) " ")
      (telega-ins (telega-tl-str imc :question))
      (telega-ins--with-face 'telega-shadow
        (telega-ins-fmt " (%d options)" (length (plist-get imc :options)))))
     (inputMessageDice
      (telega-ins (or (telega-tl-str imc :emoji)
                      (car telega-symbol-dice-list))
                  " " (telega-i18n "telega_random_dice")))

     ;; Special IMC for inline query results
     (telegaInlineQuery
      (telega-ins telega-symbol-inline " ")
      (let ((preview (plist-get imc :preview))
            (caption (plist-get imc :caption))
            (query (plist-get imc :query))
            (bot (plist-get imc :via-bot))
            (hide-via-bot (plist-get imc :hide-via-bot)))
        (when caption
          (telega-ins (propertize caption 'face 'telega-shadow) " "))
        (when preview
          (telega-ins--image preview)
          (telega-ins " "))
        (when query
          (telega-ins query " "))
        (when bot
          ;; NOTE: hide-via-bot can be only applied to @gif, @pic and @venue
          (unless (and hide-via-bot
                       (member (telega-msg-sender-username bot)
                               (mapcar (lambda (botopt)
                                         (plist-get telega--options botopt))
                                       '(:animation_search_bot_username
                                         :photo_search_bot_username
                                         :venue_search_bot_username))))
            (telega-ins "via " (telega-user-title bot 'username))))))
     (telegaForwardMessage
      (telega-ins (telega-symbol 'forward) "Fwd")
      (when (plist-get imc :send_copy)
        (telega-ins " Copy"))
      (when (plist-get imc :remove_caption)
        (telega-ins " NewCap"))
      (telega-ins ": ")
      (telega-ins--with-attrs (list :align 'left
                                    :max 20
                                    :elide-trail 1 ;closing bracket
                                    :elide t)
        (telega-ins--msg-sender (telega-msg-sender (plist-get imc :message))
          :with-avatar-p t
          :with-username-p t
          :with-brackets-p t))
      (telega-ins " ")
      (telega-ins--with-attrs (list :align 'left
                                    :max 20
                                    :elide t)
        (telega-ins--content-one-line (plist-get imc :message)
          :remove-caption (plist-get imc :remove_caption))))
     (telegaScheduledMessage
      (telega-ins (telega-symbol 'alarm) " " (telega-i18n "telega_scheduled") " ")
      (if-let ((timestamp (plist-get imc :timestamp)))
          (telega-ins-i18n "telega_scheduled_at_date"
            :date (telega-ins--as-string (telega-ins--date timestamp)))
        (telega-ins-i18n "telega_scheduled_when_online")))
     (telegaDisableNotification
      (telega-ins-i18n (if (plist-get imc :disable_notification)
                           "telega_disable_notification"
                         "telega_enable_notification")))
     (telegaLinkPreviewOptions
      (telega-ins "LinkPreviewOptions")
      (telega-ins-fmt " %S" (plist-get imc :options)))
     (telegaChatTheme
      (telega-ins "Theme: " (or (telega-tl-str imc :name)
                                "disable")))
     (telegaDelimiter
      (telega-ins "Delimiter"))
     (t
      (telega-ins-fmt "<TODO: %S>" (telega--tl-type imc)))
     ))
  t)

(cl-defun telega-ins--content-one-line (msg &key content remove-caption)
  "Insert message's MSG content for one line usage.
If REMOVE-CAPTION is specified, then do not insert caption."
  (declare (indent 1))
  (telega-ins--one-lined
   (let ((telega-msg--current msg)
         (content (or content (plist-get msg :content))))
     (cl-case (telega--tl-type content)
       (messageText
        (telega-ins--fmt-text (plist-get content :text) msg))
       (messagePhoto
        (if-let ((preview-img (telega-photo-preview--create-image-one-line
                               (plist-get content :photo)
                               (telega-msg-chat msg 'offline))))
            (telega-ins--image preview-img)
          (telega-ins (telega-symbol 'photo)))
        (telega-ins " ")
        (or (telega-ins--fmt-text
             (unless remove-caption (plist-get content :caption)) msg)
            (telega-ins--with-face 'telega-shadow
              (telega-ins-i18n "lng_in_dlg_photo"))))
       (messageDocument
        (telega-ins (telega-symbol 'attachment) " ")
        (or (telega-ins (telega--tl-get content :document :file_name))
            (telega-ins--fmt-text
             (unless remove-caption (plist-get content :caption)) msg)
            (telega-ins--with-face 'telega-shadow
              (telega-ins-i18n "lng_in_dlg_file"))))
       (messageLocation
        (telega-ins--location (plist-get content :location))
        (telega-ins--location-live msg))
       (messageVenue
        (telega-ins--location (telega--tl-get content :venue :location)))
       (messageAnimation
        (telega-ins--with-face 'telega-shadow
          (telega-ins "GIF"))
        (telega-ins-prefix " "
          (or (telega-ins--fmt-text
               (unless remove-caption (plist-get content :caption)) msg)
              (telega-ins
               (telega-tl-str (plist-get content :animation) :file_name)))))
       (messageAudio
        (telega-ins (telega-symbol 'audio) " ")
        (or (telega-ins--fmt-text
             (unless remove-caption (plist-get content :caption)) msg)
            (telega-ins--with-face 'telega-shadow
              (telega-ins-i18n "lng_in_dlg_audio_file")))
        (telega-ins-fmt " (%s)"
          (telega-duration-human-readable
           (telega--tl-get content :audio :duration))))
       (messageVideo
        (if-let ((preview-img (telega-video-preview--create-image-one-line
                               (telega--tl-get msg :content :video)
                               (telega-msg-chat msg 'offline))))
            (telega-ins--image preview-img)
          (telega-ins (telega-symbol 'video)))
        (telega-ins " ")
        (or (telega-ins--fmt-text
             (unless remove-caption (plist-get content :caption)) msg)
            (telega-ins--with-face 'telega-shadow
              (telega-ins-i18n "lng_in_dlg_video")))
        (telega-ins-fmt " (%s)"
          (telega-duration-human-readable
           (telega--tl-get content :video :duration))))
       (messageGame
        (telega-ins (telega-symbol 'game) " ")
        (let ((game (plist-get content :game)))
          (telega-ins (or (telega-tl-str game :title)
                          (telega-tl-str game :short_name)
                          (propertize "Game" 'face 'telega-shadow)))))
       (messageSticker
        (telega-ins (telega-sticker-emoji (plist-get content :sticker)))
        (telega-ins " " (propertize (telega-i18n "lng_in_dlg_sticker")
                                    'face 'telega-shadow)))
       (messageVoiceNote
        (telega-ins (propertize (telega-i18n "lng_in_dlg_audio")
                                'face 'telega-shadow))
        (telega-ins-fmt " (%s)"
          (telega-duration-human-readable
           (telega--tl-get content :voice_note :duration))))
       (messageVideoNote
        (telega-ins (propertize (telega-i18n "lng_in_dlg_video_message")
                                'face 'telega-shadow))
        (telega-ins-fmt " (%s)"
          (telega-duration-human-readable
           (telega--tl-get content :video_note :duration))))
       (messageContact
        (telega-ins--with-face 'telega-shadow
          (telega-ins-i18n "lng_in_dlg_contact"))
        (telega-ins--contact (plist-get content :contact)))
       (messageInvoice
        (telega-ins (telega-symbol 'invoice) " ")
        (let ((currency (plist-get content :currency)))
          (telega-ins-fmt "%.2f%s " (/ (plist-get content :total_amount) 100.0)
                          (or (cdr (assoc currency telega-currency-symbols-alist))
                              currency)))
        (telega-ins--with-face 'telega-shadow
          (telega-ins-i18n (if (plist-get content :is_test)
                               "lng_payments_invoice_label_test"
                             "lng_payments_invoice_label")))
        (when-let ((title (telega-tl-str content :title)))
          (telega-ins " ")
          (telega-ins--with-face '(telega-link bold)
            (telega-ins title))))
       (messagePoll
        (telega-ins (telega-symbol 'poll) " ")
        (let ((poll (plist-get content :poll)))
          (telega-ins (telega-tl-str poll :question))
          (telega-ins " (" (telega-i18n "lng_polls_votes_count"
                             :count (plist-get poll :total_voter_count))
                      ")")))
       (messageDice
        (telega-ins--dice-msg-content content 'one-line))
       (messageAnimatedEmoji
        (telega-ins (telega-tl-str content :emoji)))
       (messageForumTopicCreated
        (let* ((topic-icon (plist-get content :icon))
               (icon-sticker (telega-custom-emoji-get
                              (plist-get topic-icon :custom_emoji_id)))
               (topic-name (telega-tl-str content :name)))
          (when icon-sticker
            (telega-ins--sticker-image icon-sticker))
          (telega-ins topic-name)))
       (messageStory
        (telega-ins--story-msg-forwarded-from
         (telega-chat-get (plist-get content :story_sender_chat_id))))
       (messagePremiumGiveaway
        (telega-ins--with-face '(telega-shadow bold)
          (telega-ins-i18n "lng_prizes_title"
            :count 2)))
       (t (telega-ins--content msg)))
     t)))


;;; Inserters for CONTACTS ewoc buttons
(defun telega-ins--root-contact (user)
  "Inserter for USER, used for contacts ewoc in rootbuf."
  (telega-ins--contact user
    :with-avatar-p telega-root-show-avatars))

(defun telega-ins--root-contact-2lines (user)
  "Two lines inserter for USER, used for contacts ewoc in rootbuf."
  (let ((avatar (telega-msg-sender-avatar-image user)))
    ;; first line
    (telega-ins--image avatar 0
                       :no-display-if (not telega-user-show-avatars))
    (let ((telega-root-show-avatars nil))
      (telega-ins--root-contact user))

    ;; second line
    (telega-ins "\n")
    (telega-ins--image avatar 1
                       :no-display-if (not telega-user-show-avatars))

    (telega-ins--user-status user)
    (telega-ins-prefix ", "
      (telega-ins--user-nearby-distance user))
    t))


(defun telega-ins--chat-msg-one-line (chat msg)
  "Insert message for the chat button usage."
;  (cl-assert (> max-width 11))
  (let* ((date-and-status (telega-ins--as-string
                           (telega-ins--message-date-and-status msg)))
         (dwidth (- telega-root-fill-column (string-width date-and-status))))
    (telega-ins--with-attrs (list :align 'left
                                  :max (- dwidth (telega-current-column))
                                  :elide t)
      ;; NOTE: Do not show username for:
      ;;;  - Saved Messages
      ;;;  - Channel posts
      ;;;  - Special messages
      ;;;  - If sent by peer in private/secret chat
      (let ((sender
             (unless (or (telega-me-p chat)
                         (telega-chat-channel-p chat)
                         (telega-msg-special-p msg)
                         (and (telega-chat-match-p chat '(type private secret))
                              (not (telega-msg-match-p msg 'outgoing))))
               (telega-msg-sender msg))))
        (telega-ins--aux-msg-one-line msg
          :with-username (when sender t)
          :username-face (when sender
                           (telega-msg-sender-title-faces sender)))))

    (telega-ins--move-to-column dwidth)
    (telega-ins date-and-status)))

(defun telega-ins--user-online-status (user)
  "Insert USER's online status."
  (when (and user
             (not (telega-user-bot-p user))
             (not (telega-me-p user))
             (telega-user-online-p user))
    (telega-ins (telega-symbol 'online-status))))

(defun telega-ins--chat-members-trail (chat)
  "Insert number of CHAT members at chat trail."
  ;; For chats searched by
  ;; `telega--searchPublicChats' insert number of
  ;; members in the group
  ;; Basicgroups converted to supergroups
  ;; does not have username and have "0" order
  (when (telega-chat-match-p chat '(type basicgroup supergroup channel))
    (telega-ins--with-face (if (telega-chat-muted-p chat)
                               'telega-muted-count
                             'telega-unmuted-count)
      (telega-ins (telega-number-human-readable
                   (plist-get (telega-chat--info chat) :member_count))
                  (telega-symbol 'member)))))

(defun telega-ins--chopic-unread-trail (chopic)
  "Insert chat or topic CHOPIC's unread status."
  (let* ((unread (plist-get chopic :unread_count))
         (mentions (plist-get chopic :unread_mention_count))
         (reactions (plist-get chopic :unread_reaction_count))
         (chat-p (telega-chat-p chopic))
         (muted-p (if chat-p
                      (telega-chat-muted-p chopic)
                    (telega-topic-muted-p chopic)))
         (disable-mention-notifications-p
          (if chat-p
              (telega-chat-notification-setting
               chopic :disable_mention_notifications)
            (telega-topic-notification-setting
               chopic :disable_mention_notifications))))
    (unless (zerop unread)
      (telega-ins--with-face (if muted-p
                                 'telega-muted-count
                               'telega-unmuted-count)
        (telega-ins (telega-number-human-readable unread))))

    (unless (zerop mentions)
      (telega-ins--with-face
          (if disable-mention-notifications-p
              '(telega-muted-count bold)
            'telega-mention-count)
        (telega-ins-fmt "@%d" mentions)))

    (unless (zerop reactions)
      (telega-ins--with-face (if muted-p
                                 'telega-muted-count
                               'telega-unmuted-count)
        (telega-ins (telega-symbol 'reaction)
                    ;; (format "%d" reactions)
                    )))

    ;; Mark for chats marked as unread
    (when (and chat-p (zerop unread) (zerop mentions)
               (plist-get chopic :is_marked_as_unread))
      (telega-ins--with-face (if muted-p
                                 'telega-muted-count
                               'telega-unmuted-count)
        (telega-ins (telega-symbol 'unread))))
    t))

(defun telega-ins--chopic-pinned-trail (chopic)
  "Trail inserter for pin status of the chat or topic CHOPIC."
  (when (if (telega-chat-p chopic)
            (plist-get (telega-chat-position chopic) :is_pinned)
          (plist-get chopic :is_pinned))
    (telega-ins (telega-symbol 'pin))))

(defun telega-ins--chat-status-icons-trail (chat)
  "Inserter for status icons for the CHAT button."
  (telega-ins--chopic-pinned-trail chat)
  (when (telega-chat-match-p chat 'is-forum)
    (telega-ins (telega-symbol 'forum)))
  (when (telega-chat-match-p chat 'has-video-chat)
    (telega-ins (telega-symbol
                 (if (telega-chat-match-p chat '(has-video-chat non-empty))
                     'video-chat-active
                   'video-chat-passive))))
  (when (telega-chat-match-p chat 'has-scheduled-messages)
    (telega-ins (telega-symbol 'alarm)))
  (when-let ((custom-order (telega-chat-uaprop chat :order)))
    (telega-ins
     (if (< (string-to-number custom-order)
            (string-to-number (telega-chat-order chat 'raw)))
         (car telega-symbol-custom-order)
       (cdr telega-symbol-custom-order))))
  (when (telega-chat-secret-p chat)
    (telega-ins (telega-symbol 'lock)))
  (when (telega-chat-match-p chat 'has-protected-content)
    (telega-ins (telega-symbol 'copyright)))
  )

(defun telega-ins--chat (chat)
  "Inserter for CHAT button in root buffer.
CHAT is formatted according to `telega-chat-button-format-temex' or
`telega-chat-button-format-plist'.
Return t."
  (let* ((brackets (telega-msg-sender-brackets chat))
         (fmt-plist
          (or (telega-chat-match-p chat telega-chat-button-format-temex)
              telega-chat-button-format-plist))
         (inside-trail
          (telega-ins--as-string
           (when (plist-get fmt-plist :with-unread-trail-p)
             (telega-ins--chopic-unread-trail chat))
           (when (plist-get fmt-plist :with-members-trail-p)
             (telega-ins--chat-members-trail chat))))
         (chat-button-width
          (telega-canonicalize-number telega-chat-button-width
                                      telega-root-fill-column))
         (curr-column (when chat-button-width
                        (telega-current-column)))
         (title-width
          (when chat-button-width
            (- chat-button-width (string-width inside-trail)
               ;; NOTE: Do *not* include brackets into
               ;; `chat-button-width' to avoid additional calls to
               ;; `string-width'
               ;; (string-width (car brackets))
               ;; (string-width (cadr brackets))
               ))))

    (when telega-root-show-avatars
      (telega-ins--image (telega-msg-sender-avatar-image-one-line chat)))
    (telega-ins (car brackets))

    ;; Title
    (telega-ins--with-attrs (list :max title-width
                                  :align 'left
                                  :elide t)
      (when-let* ((folders-insexp (plist-get fmt-plist :with-folders-insexp))
                  (telega-chat-folders
                   (seq-difference (telega-chat-folders chat)
                                   telega-chat-folders-exclude)))
        (telega-ins--insexp folders-insexp))

      (telega-ins--with-face (when (telega-chat-secret-p chat)
                               'telega-secret-title)
        (telega-ins--msg-sender chat
          :with-username-p (plist-get fmt-plist :with-username-p)
          :with-title-faces-p (plist-get fmt-plist :with-title-faces-p)))

      (telega-ins--user-online-status (telega-chat-user chat)))

    (cond ((and curr-column title-width)
           (telega-ins--move-to-column (+ curr-column 3 1 title-width)))
          ((not (string-empty-p inside-trail))
           (telega-ins " ")))
    (telega-ins inside-trail)
    (telega-ins (cadr brackets))

    (when (plist-get fmt-plist :with-status-icons-trail-p)
      (telega-ins--chat-status-icons-trail chat))
    t))

(defun telega-ins--chat-status (chat &optional topic)
  "Insert CHAT status, limiting it to MAX-WIDTH.
If TOPIC is given, insert chat status for the TOPIC."
  (let ((actions (telega-chat--actions
                  chat (when topic (telega-topic-msg-thread-id topic))))
        (call (unless topic
                (telega-voip--by-user-id (plist-get chat :id))))
        (draft-msg (plist-get (or topic chat) :draft_message))
        (last-msg (plist-get (or topic chat) :last_message))
        (chat-info (telega-chat--info chat))
        (max-width  (- telega-root-fill-column
                       (telega-current-column))))
    (cond ((and (telega-chat-secret-p chat)
                (memq (telega--tl-type (plist-get chat-info :state))
                      '(secretChatStatePending secretChatStateClosed)))
           ;; Status of the secret chat
           (telega-ins (propertize
                        (substring (telega--tl-get chat-info :state :@type) 15)
                        'face 'telega-shadow)))

          (call
           (let ((state (plist-get call :state)))
             (telega-ins (telega-symbol (if (plist-get call :is_video)
                                            'video
                                          'phone))
                         " "
                         (cond ((and (plist-get call :is_outgoing)
                                     (plist-get call :is_video))
                                (telega-i18n "lng_call_video_outgoing"))
                               ((plist-get call :is_outgoing)
                                (telega-i18n "lng_call_outgoing"))
                               ((plist-get call :is_video)
                                (telega-i18n "lng_call_video_incoming"))
                               (t
                                (telega-i18n "lng_call_incoming")))
                         " (" (substring (plist-get state :@type) 9) ")")

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
               (telega-ins--with-face 'error
                 (telega-ins (telega-i18n "lng_from_draft") ": "))
               (telega-ins--one-lined
                (telega-ins--fmt-text (plist-get inmsg :text))))))

          (last-msg
           (if (telega-msg-match-p last-msg 'ignored)
               (telega-ins--one-lined (telega-ins--message-ignored last-msg))
             (telega-ins--chat-msg-one-line chat last-msg)))

          ((and (telega-chat-secret-p chat)
                (eq (telega--tl-type (plist-get chat-info :state))
                    'secretChatStateReady))
           ;; Status of the secret chat
           (telega-ins--with-face 'telega-shadow
             (telega-ins
              (substring (telega--tl-get chat-info :state :@type) 15))))
          )))

(defun telega-ins--chat-compact (chat)
  "Inserter for compact CHAT button in the root buffer.
Short version."
  (telega-ins--chat chat)
  t)

(defun telega-ins--chat-full (chat)
  "Full status inserter for CHAT button in root buffer."
  (telega-ins--chat chat)
  (telega-ins "  ")
  (telega-ins--chat-status chat)
  t)

(defun telega-ins--chat-full-2lines (chat)
  "Two lines inserter for the CHAT button in rootbuf."
  (let ((avatar (telega-msg-sender-avatar-image chat)))
    (when telega-root-show-avatars
      (telega-ins--image avatar 0))
    (let ((telega-root-show-avatars nil))
      (telega-ins--chat chat))
    (telega-ins "\n")
    (when telega-root-show-avatars
      (telega-ins--image avatar 1)))

  (telega-ins " ")
  (telega-ins--chat-status chat)
  t)

(defun telega-ins--chat-nearby-2lines (chat)
  "Two lines inserter for the nearby CHAT in rootbuf."
  (let ((avatar (telega-msg-sender-avatar-image chat)))
    (telega-ins--image avatar 0)
    (let ((telega-root-show-avatars nil))
      (telega-ins--chat chat))
    (telega-ins "\n")
    (telega-ins--image avatar 1))

  (let ((distance (telega-chat-nearby-distance chat)))
    (telega-ins--with-face 'telega-shadow
      (telega-ins (telega-distance-human-readable distance) " away")))

  ;; Online status for private chats
  (when-let ((user (telega-chat-user chat)))
    (telega-ins ", ")
    (telega-ins--user-status user))
  t)

(defun telega-ins--chat-last-message (chat)
  "Inserter for last message in CHAT."
  (let ((last-msg (plist-get chat :last_message)))
    (cl-assert last-msg)
    (telega-ins--message-with-chat-header last-msg)))

(defun telega-ins--root-msg (msg)
  "Inserter for message MSG shown in `telega-root-messages--ewoc'."
  (let ((chat (telega-msg-chat msg))
        (telega-chat-button-width
         (round (* (telega-canonicalize-number telega-chat-button-width
                                               telega-root-fill-column)
                   (/ 2.0 3)))))
    (telega-ins--chat chat)
    (telega-ins "  ")
    (telega-ins--chat-msg-one-line chat msg)))

(defun telega-ins--root-msg-call (msg)
  "Inserter for call message MSG in rootbuf."
  (let ((telega-chat-fill-column telega-root-fill-column)
        (telega-msg-heading-with-date-and-status t))
    (telega-ins--message msg
      :sender (when (telega-msg-match-p msg 'outgoing)
                (telega-chat-user (telega-msg-chat msg))))))

(defun telega-ins--chat-my-restrictions (chat)
  "Insert my restrictions (if any) in the CHAT.
Return non-nil if restrictions has been inserted."
  (when-let ((my-status (telega-chat-member-my-status chat)))
    (when (eq (telega--tl-type my-status) 'chatMemberStatusRestricted)
      (let* ((until (plist-get my-status :restricted_until_date))
             (until-date (unless (zerop until)
                           (telega-ins--as-string
                            (telega-ins--date until 'date))))
             (until-time (unless (zerop until)
                           (telega-ins--as-string
                            (telega-ins--date until 'time))))
             (perms (plist-get my-status :permissions)))
        (cond ((not (plist-get perms :can_send_basic_messages))
               (if (and until-date until-time)
                   (telega-ins-i18n "lng_restricted_send_message_until"
                     :date until-date :time until-time)
                 (telega-ins-i18n "lng_restricted_send_message")))
              ((or (not (plist-get perms :can_send_audios))
                   (not (plist-get perms :can_send_documents))
                   (not (plist-get perms :can_send_photos))
                   (not (plist-get perms :can_send_videos))
                   (not (plist-get perms :can_send_video_notes))
                   (not (plist-get perms :can_send_voice_notes)))
               (if (and until-date until-time)
                   (telega-ins-i18n "lng_restricted_send_media_until"
                     :date until-date :time until-time)
                 (telega-ins-i18n "lng_restricted_send_media")))
              ((not (plist-get perms :can_send_polls))
               (if (and until-date until-time)
                   (telega-ins-i18n "lng_restricted_send_polls_until"
                     :date until-date :time until-time)
                 (telega-ins-i18n "lng_restricted_send_polls")))
              (t
               (if (and until-date until-time)
                   (telega-ins-i18n "lng_restricted_send_message_until"
                     :date until-date :time until-time)
                 (telega-ins-i18n "lng_restricted_send_message")))
              )
        t))))

(defun telega-ins--sponsored-message (sponsored-msg)
  "Inserter for the SPONSORED-MSG."
  (let* ((sponsor (plist-get sponsored-msg :sponsor))
         (sponsor-type (plist-get sponsor :type))
         (chat-photo-info (plist-get sponsor :photo)))
    (when chat-photo-info
      (telega-ins--image
       (telega-chat-photo-info-image-one-line chat-photo-info)))

    (cl-assert sponsor)
    (cl-ecase (telega--tl-type sponsor-type)
      (messageSponsorTypeBot
       (let ((user (telega-user-get (plist-get sponsor-type :bot_user_id))))
         (telega-ins--msg-sender user
           :with-avatar-p nil
           :with-username-p t
           :with-brackets-p nil)))

      (messageSponsorTypeWebApp
       (telega-ins (telega-tl-str sponsor-type :web_app_title)))

      (messageSponsorTypePublicChannel
       (let ((chat (telega-chat-get (plist-get sponsor-type :chat_id))))
         (telega-ins--msg-sender chat
           :with-avatar-p nil
           :with-username-p 'telega-username
           :with-brackets-p nil)))

      (messageSponsorTypePrivateChannel
       (telega-ins (telega-tl-str sponsor-type :title)))

      (messageSponsorTypeWebsite
       (telega-ins--with-face 'telega-webpage-sitename
         (telega-ins (telega-tl-str sponsor-type :name)))))
    (telega-ins "\n")

    (telega-ins--with-face 'telega-msg-sponsored
      (telega-ins--content sponsored-msg))
    (telega-ins "\n")

    ;; Link
    (cl-ecase (telega--tl-type sponsor-type)
      (messageSponsorTypeBot
       (telega-ins--box-button (telega-i18n "lng_view_button_bot")
         'action 'telega-msg-open-sponsored))

      (messageSponsorTypeWebApp
       (telega-ins--box-button (telega-i18n "lng_open_link")
         'action 'telega-msg-open-sponsored))

      ((messageSponsorTypePublicChannel
        messageSponsorTypePrivateChannel)
       (telega-ins--box-button (telega-i18n "lng_view_button_channel")
         'action 'telega-msg-open-sponsored))

      (messageSponsorTypeWebsite
       (telega-ins--box-button (telega-i18n "lng_view_button_external_link")
         'action 'telega-msg-open-sponsored)))
    (telega-ins "\n")))

(defun telega-ins--msg-reaction-type (reaction-type)
  "Insert REACTION-TYPE.
REACTION-TYPE. is the `ReactionType' TDLib object."
  (cl-case (telega--tl-type reaction-type)
    (reactionTypeEmoji
     (telega-ins (telega-tl-str reaction-type :emoji)))
    (reactionTypeCustomEmoji
     (when-let ((sticker (telega-custom-emoji-get
                          (plist-get reaction-type :custom_emoji_id))))
       (telega-ins--sticker-image sticker)))))

(defun telega-ins--msg-reaction (msg-reaction)
  "Insert MSG-REACTION.
MSG-REACTION is the `messageReaction' TDLib object."
  (telega-ins--with-face (if (plist-get msg-reaction :is_chosen)
                             'telega-box-button-active
                           'telega-shadow)
    (telega-ins-fmt "%d" (plist-get msg-reaction :total_count))
    (telega-ins--msg-reaction-type (plist-get msg-reaction :type))
    (seq-doseq (rs (plist-get msg-reaction :recent_sender_ids))
      (telega-ins--image
       (telega-msg-sender-avatar-image-one-line (telega-msg-sender rs))))
    t))

(defun telega-ins--msg-reaction-list (msg)
  "Inserter for the message's MSG reactions."
  (let ((reactions (telega--tl-get msg :interaction_info :reactions
                                   :reactions))
        ret)
    (seq-doseq (msg-reaction reactions)
      (when ret
        (telega-ins "  "))
      (telega-ins--raw-button
          (list 'action (lambda (_button)
                          (let ((rtype (plist-get msg-reaction :type))
                                (big-p current-prefix-arg))
                            (if (plist-get msg-reaction :is_chosen)
                                (telega--removeMessageReaction msg rtype)
                              (telega--addMessageReaction msg rtype big-p)))))
        (funcall telega-inserter-for-msg-reaction msg-reaction))
      (setq ret t))
    ret))

(defun telega-ins--available-reaction-list (av-reactions custom-action)
  "Insert available reactions.
AV-REACTIONS - list of `availableReaction' TDLib objects.
When some reaction is chosen, CUSTOM-ACTION is called with the single
argument of `ReactionType' type."
  (seq-doseq (av-reaction av-reactions)
    (let ((reaction-type (plist-get av-reaction :type)))
      (telega-button--insert 'telega reaction-type
        :inserter #'telega-ins--msg-reaction-type
        :action custom-action
        'cursor-sensor-functions
        (when-let ((sticker
                    (when (eq (telega--tl-type reaction-type)
                              'reactionTypeCustomEmoji)
                      (telega-custom-emoji-get
                       (plist-get reaction-type :custom_emoji_id)))))
          (when (and (not (telega-sticker-static-p sticker))
                     telega-sticker-animated-play)
            (list (telega-sticker--gen-sensor-func sticker))))))))

(defun telega-ins--saved-messages-tag (tag)
  "Inserter for the Saved Messages TAG."
  (telega-ins--with-face 'telega-box-button-active
    (telega-ins " ")
    (telega-ins--msg-reaction-type (plist-get tag :tag))
    (telega-ins (telega-tl-str tag :label))
    (telega-ins " "))
  (telega-ins--with-face
      (list :foreground (face-background 'telega-box-button-active))
    (telega-ins (telega-symbol 'saved-messages-tag-end)))
  t)

(defun telega-ins--msg-saved-messages-tags (msg)
  "Inserter for Saved Messages tags for the message MSG."
  (let ((reactions (telega--tl-get msg :interaction_info :reactions
                                   :reactions))
        ret)
    (seq-doseq (msg-reaction reactions)
      (when ret
        (telega-ins "  "))
      (if-let ((tag (telega-saved-messages-find-tag
                     (plist-get msg-reaction :type))))
          (telega-ins--raw-button
              (list 'action (lambda (_button)
                              (transient-setup
                               'telega-saved-messages-tag-commands nil nil
                               :scope (cons tag msg))))
            (telega-ins--saved-messages-tag tag))
        ;; NOTE: if Saved Messages are not yet loaded we will
        ;; fallback to `telega-ins--msg-reaction'
        (funcall telega-inserter-for-msg-reaction msg-reaction))
      (setq ret t))
    ret))


;;; Stories
(defun telega-ins--story-msg-forwarded-from (chat)
 "Inserter for the header of message story forwarded from CHAT."
 (let ((sender chat))
   (telega-ins--with-face 'telega-shadow
     (telega-ins-i18n "lng_forwarded_story"
       :user (telega-ins--as-string
              (telega-ins--raw-button
                  (list 'action (lambda (_button)
                                  (telega-describe-msg-sender sender)))
                (telega-ins--msg-sender sender
                  :with-avatar-p t
                  :with-username-p t
                  :with-brackets-p t)))))))

(defun telega-ins--my-story-one-line (story &optional for-msg remove-caption)
  "Insert contents for my STORY as one line.
FOR-MSG is the message replying to me STORY.
FOR-MSG and REMOVE-CAPTION are passed directly to
`telega-ins--story-content-one-line'."
  (declare (indent 1))
  (telega-ins--with-face 'telega-shadow
    (telega-ins-i18n "lng_stories_my_name"))
  (telega-ins " ")
  (telega-ins--story-content-one-line story for-msg remove-caption))

(defun telega-ins--story-msg (msg)
  "Inserter for the \"messageStory\" MSG."
  (let* ((content (plist-get msg :content))
         (chat-id (plist-get content :story_sender_chat_id)))
    (telega-ins--story-msg-forwarded-from (telega-chat-get chat-id))
    (telega-ins "\n")
    (telega-ins--story-content
     (telega-story-get chat-id (plist-get content :story_id) 'offline)
     msg)))

(defun telega-ins--story-content (story &optional for-msg remove-caption)
  "Inserter for the STORY content."
  (cond ((null story)
         (telega-ins-i18n "lng_profile_loading"))

        ((telega-story-deleted-p story)
         (telega-ins--with-face 'shadow
           (telega-ins-i18n "lng_forwarded_story_expired")))

        (t
         (let ((content (plist-get story :content)))
           (cl-ecase (telega--tl-type content)
             (storyContentPhoto
              (telega-ins--photo (plist-get content :photo)
                                 for-msg nil telega-photo-show-details))
             (storyContentVideo
              (telega-ins--video for-msg (plist-get content :video)))
             (storyContentUnsupported
              (telega-ins "<unsupported story content>")))

           (unless remove-caption
             (when-let ((caption (plist-get story :caption)))
               (telega-ins-prefix "\n"
                 (telega-ins--fmt-text caption for-msg))))
           t))))

(defun telega-ins--story-content-one-line (story &optional for-msg
                                                 remove-caption)
  "One line inserter for the STORY content.
If REMOVE-CAPTION is specified, then do not insert caption."
  (cond ((null story)
         (telega-ins--image
          (telega-svg-image
           (let* ((xw (telega-chars-xwidth 2))
                  (xh (min xw (telega-chars-xheight 1)))
                  (svg (telega-svg-create xh xh)))
             (telega-svg-story-icon-with-symbol svg xh telega-symbol-pending))
           :scale 1.0
           :ascent 'center
           :telega-text telega-symbol-story))
         (unless remove-caption
           (telega-ins--with-face 'shadow
             (telega-ins-i18n "lng_profile_loading"))))

        ((telega-story-deleted-p story)
         (telega-ins--image
          (telega-svg-image
           (let* ((xw (telega-chars-xwidth 2))
                  (svg (telega-svg-create xw xw)))
             (telega-svg-story-icon-with-symbol svg xw telega-symbol-flames))
           :scale 1.0
           :ascent 'center
           :telega-text telega-symbol-story))
         (unless remove-caption
           (telega-ins--with-face 'shadow
             (telega-ins-i18n "lng_forwarded_story_expired"))))

        (t
         (if-let ((story-img (telega-story-preview--create-image-one-line story)))
             (telega-ins--image story-img)

           ;; No preview image, use symbol as preview icon
           (cond ((telega-story-match-p story 'is-photo)
                  (telega-ins (telega-symbol 'photo)))
                 ((telega-story-match-p story 'is-video)
                  (telega-ins (telega-symbol 'video)))
                 (t
                  (telega-ins (telega-symbol 'story)))))

         (unless remove-caption
           (telega-ins--one-lined
            (telega-ins " ")
            (or (telega-ins--fmt-text (plist-get story :caption) for-msg)
                (telega-ins--with-face 'telega-shadow
                  (cond ((telega-story-match-p story 'is-photo)
                         (telega-ins-i18n "lng_in_dlg_photo"))
                        ((telega-story-match-p story 'is-video)
                         (telega-ins-i18n "lng_in_dlg_video")))))))
         )))

(defun telega-ins--story-one-line (story)
  "Insert a one line STORY content without caption."
  (telega-ins--story-content-one-line story nil 'no-caption))

(defun telega-ins--button-story-one-line-no-caption (story)
  "One line inserter for the one line story button without caption."
  (telega-button--insert 'telega story
    :inserter #'telega-ins--story-one-line
    :action #'telega-story-open
    'help-echo
    (telega-ins--as-string
     (telega-ins--one-lined
      (telega-ins--fmt-text (plist-get story :caption))))))


;;; Topics
(defun telega-ins--topic-icon (topic)
  "Insert TOPIC's icon."
  (when-let ((topic-icon-image (telega-topic-avatar-image topic 1)))
    (telega-ins--image topic-icon-image)))

(defun telega-ins--topic-title (topic &optional with-icon-p)
  "Inserter for the chat TOPIC title."
  (when with-icon-p
    (telega-ins--topic-icon topic))
  (telega-ins (telega-tl-str (plist-get topic :info) :name))
  )

(defun telega-ins--topic-status (topic)
  (let ((telega-root-fill-column (- telega-root-fill-column 2)))
    (telega-ins--chat-status (telega-topic-chat topic) topic)))

(defun telega-ins--topic (topic)
  "Inserter for the TOPIC button."
  (let* ((fmt-plist
          (or (telega-topic-match-p topic telega-topic-button-format-temex)
              telega-topic-button-format-plist))
         (inside-trail
          (telega-ins--as-string
           (when (plist-get fmt-plist :with-unread-trail-p)
             (telega-ins--chopic-unread-trail topic))))
         (prefix-space
            (or (plist-get fmt-plist :prefix-space) ""))
         (topic-button-width
          (telega-canonicalize-number telega-chat-button-width
                                      telega-root-fill-column))
         (curr-column (when topic-button-width
                        (telega-current-column)))
         (title-width
          (when topic-button-width
            (- topic-button-width (string-width inside-trail)
               1                        ; "#" symbol
               (string-width prefix-space)))))
    (telega-ins--with-face
        (cons 'telega-topic-button
              (when (telega-topic-match-p topic 'is-most-recent) (list 'bold)))
      (telega-ins prefix-space)
      (telega-ins (telega-symbol 'topic))
      (telega-ins--topic-icon topic)
      (telega-ins (car telega-symbol-topic-brackets))
      (telega-ins--with-attrs (list :max title-width
                                    :align 'left
                                    :elide t)
        (telega-ins--topic-title topic))

      (cond ((and curr-column title-width)
             (telega-ins--move-to-column
              (+ curr-column 3 1 1 (string-width prefix-space) title-width)))
            ((not (string-empty-p inside-trail))
             (telega-ins " ")))
      (telega-ins inside-trail)
      (telega-ins (cdr telega-symbol-topic-brackets)))
    ))

(defun telega-ins--topic-full (topic)
  "Full status inserter for TOPIC button in root buffer."
  (telega-ins--topic topic)
  (telega-ins "  ")
  (telega-ins--topic-status topic)
  t)

(defun telega-ins--topic-full-2lines (topic)
  "Two lines inserter for CHAT TOPIC in the rootbuf."
  (let ((avatar (telega-topic-avatar-image topic)))
    (when telega-root-show-avatars
      (telega-ins--image avatar 0))
    (let ((telega-root-show-avatars nil))
      (telega-ins--topic topic))
    (telega-ins "\n")
    (when telega-root-show-avatars
      (telega-ins--image avatar 1)))

  (telega-ins " ")
  (telega-ins--topic-status topic)
  t)

(provide 'telega-ins)

;;; telega-ins.el ends here

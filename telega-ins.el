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

(require 'telega-core)
(require 'telega-tdlib)
(require 'telega-inline)
(require 'telega-folders)
(require 'telega-customize)

;; telega-chat.el depends on telega-ins.el
(declare-function telega-msg-delete0 "telega-chat" (msg &optional revoke))
(declare-function telega-msg-redisplay "telega-chat" (msg &optional node))
(declare-function telega-chat-get "telega-chat" (chat-id &optional offline-p))
(declare-function telega-chat-color "telega-chat" (chat))
(declare-function telega-chat-title "telega-chat" (chat &optional with-username))
(declare-function telega-chat-title-with-brackets "telega-chat" (chat &optional with-username))
(declare-function telega-chat--goto-msg "telega-chat" (chat msg-id &optional highlight))
(declare-function telega-describe-chat "telega-chat" (chat))
(declare-function telega-chat-secret-p "telega-chat" (chat))
(declare-function telega-chat-user "telega-chat" (chat))
(declare-function telega-chat-brackets "telega-chat" (chat))
(declare-function telega-chat-muted-p "telega-chat" (chat))
(declare-function telega-chat--type "telega-chat" (chat))
(declare-function telega-chat-channel-p "telega-chat" (chat))
(declare-function telega-chat--info "telega-chat" (chat))
(declare-function telega-chat-pinned-msg "telega-chat" (chat &optional offline-p callback))
(declare-function telega-chat-delete "telega-chat" (chat &optional leave-p))
(declare-function telega-chat-admin-get "telega-chat" (chat user))
(declare-function telega-chat--update-pinned-message "telega-chat" (chat &optional offline-p old-pin-msg-id))

(defun telega-button--endings-func (label)
  "Function to generate endings for the button with LABEL."
  (cond
   ((member label (list " " "  " "✕" telega-symbol-heavy-checkmark))
    ;; NOTE: " " is arguable, incorrect rendering for @chessy_bot
    (cons "" ""))

   ((string-equal label "2×")
    ;; Special case for voice/video notes speedup button
    (let ((half-space (propertize " " 'display '(space :width 0.5))))
      (cons half-space half-space)))

   (t
    ;; NOTE: In newer Emacs we can use cons as `:line-width` in
    ;; telega-button face, so width of the button is not increased in
    ;; contrast with using single negative number for `:line-width'
    ;; However is older Emacs this is not implemented, see
    ;; https://t.me/emacs_telega/22129
    ;;
    ;; We handle both cases, for `:line-width' as cons and as negative
    ;; number

    ;; XXX inclose LABEL with shrink version of spaces, so button
    ;; width will be char aligned

    ;; NOTE: non-breakable space is used, so if line is feeded at the
    ;; beginning of button, it won't loose its leading space
    (let* ((line-width (plist-get (face-attribute 'telega-button :box)
                                  :line-width))
           (box-width (- (if (consp line-width)
                             (car line-width)
                           (or line-width 0))))
           (space (when (> box-width 0)
                    `(space (,(- (telega-chars-xwidth 1) box-width)))))
           (end (if space
                    (propertize "\u00A0" 'display space)
                  "\u00A0")))
      (cons end end)))))

(defun telega-ins--button (label &rest props)
  "Insert pressable button labeled with LABEL.
If custom face is specified in PROPS, then
`telega-button--sensor-func' is not set as sensor function."
  (declare (indent 1))
  (unless (plist-get props 'face)
    (let ((ends (if (functionp telega-button-endings)
                    (funcall telega-button-endings label)
                  telega-button-endings)))
      (setq label (concat (or (car ends) "[")
                          label
                          (or (cdr ends) "]"))))
    (setq props (plist-put props 'face 'telega-button))
    (setq props (plist-put props 'cursor-sensor-functions
                           '(telega-button--sensor-func))))

  (unless (plist-get props 'action)
    (setq props (plist-put props 'action 'telega-button--action)))
  (button-at (apply 'insert-text-button label props)))

(defun telega-ins--image (img &optional slice-num &rest props)
  "Insert image IMG generated by telega.
Uses internal `:telega-text' to keep correct column.
If SLICE-NUM is specified, then insert single slice.
SLICE-NUM can be a list in form (SLICE-NUM SLICE-Y SLICE-H).

Special property `:no-display-if' is supported in PROPS to
ommit image display if value is for this property is non-nil."
  ;; NOTE: IMG might be nil if `telega-use-images' is nil
  ;; See https://github.com/zevlg/telega.el/issues/274
  (if (not img)
      (telega-ins "<IMAGE>")

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
             ;; Otherwise use slow `image-size' to get correct
             ;; `:telega-text'
             (make-string (telega-chars-in-width
                           (or (plist-get (cdr img) :width)
                               (progn
                                 (telega-debug "WARN: `image-size' used for %S" img)
                                 (cl-assert img)
                                 (car (image-size img t (telega-x-frame))))))
                          ?X)))))))

(defun telega-ins--image-slices (image &optional props slice-func)
  "Insert sliced IMAGE at current column.
PROPS - additional image properties.
SLICE-FUNC - function called after inserting slice. Called with
single argument - slice number, starting from 0."
  (declare (indent 2))
  (if (or (not telega-use-images)
          (not (display-graphic-p (telega-x-frame))))
      (telega-ins "<IMAGE>")

    (let* ((img-xheight (* (plist-get (cdr image) :height)
                           (plist-get (cdr image) :scale)))
           (img-slices (if img-xheight
                           (telega-chars-in-height img-xheight)
                         (ceiling
                          (progn
                            (warn "`image-size' used for %S" image)
                            (cdr (image-size image nil (telega-x-frame))))))))
      (telega-ins--column (current-column) nil
        (dotimes (slice-num img-slices)
          (apply #'telega-ins--image image slice-num props)
          (when slice-func
            (funcall slice-func slice-num))
          (unless (= slice-num (1- img-slices))
            (telega-ins--with-props (list 'line-height t)
              (telega-ins "\n"))))))))

(defun telega-ins--actions (actions)
  "Insert chat ACTIONS alist."
  (when actions
    ;; NOTE: Display only last action
    (let* ((first-action (car actions))
           (sender (telega-msg-sender (car first-action)))
           (action (cdr first-action)))
      (telega-ins--with-attrs
          (list :face (telega-msg-sender-title-faces sender))
        (telega-ins (telega-msg-sender-title sender t)))
      (telega-ins " ")
      (telega-ins--with-face 'shadow
        (telega-ins "is " (substring (plist-get action :@type) 10))
        (when-let ((progress (plist-get action :progress)))
          (telega-ins-fmt " %d%%" progress))
        (when-let ((emoji (telega-tl-str action :emoji)))
          (telega-ins emoji))
        ))))

(defun telega-ins--filesize (filesize)
  "Insert FILESIZE in human readable format."
  (telega-ins (file-size-human-readable filesize)))

(defun telega-ins--date (timestamp)
  "Insert TIMESTAMP.
Format is:
- HH:MM      if today
- Mon/Tue/.. if on this week
- DD.MM.YY   otherwise (uses `telega-old-date-format')"
  (let* ((dtime (decode-time timestamp))
         (current-ts (telega-time-seconds))
         (ctime (decode-time current-ts))
         (today00 (telega--time-at00 current-ts ctime)))
    (if (and (> timestamp today00)
             (< timestamp (+ today00 (* 24 60 60))))
        (telega-ins-fmt "%02d:%02d" (nth 2 dtime) (nth 1 dtime))

      (let* ((week-day (nth 6 ctime))
             (mdays (+ week-day
                       (- (if (< week-day telega-week-start-day) 7 0)
                          telega-week-start-day)))
             (week-start00 (telega--time-at00
                            (- current-ts (* mdays 24 3600)))))
        (if (and (> timestamp week-start00)
                 (< timestamp (+ week-start00 (* 7 24 60 60))))
            (telega-ins (nth (nth 6 dtime) telega-i18n-weekday-names))

          (telega-ins
           (format-spec telega-old-date-format
                        (format-spec-make
                         ?D (format "%02d" (nth 3 dtime))
                         ?M (format "%02d" (nth 4 dtime))
                         ?Y (format "%02d" (- (nth 5 dtime) 2000)))))))
      )))

(defun telega-ins--date-iso8601 (timestamp)
  "Insert TIMESTAMP in ISO8601 format."
  (telega-ins (format-time-string "%FT%T%z" timestamp)))

(defun telega-ins--date-full (timestamp)
  "Insert TIMESTAMP in full format - DAY MONTH YEAR."
  (cl-destructuring-bind (day month year)
      (seq-subseq (decode-time timestamp) 3 6)
    (telega-ins-fmt "%d %s %d"
      day (nth month (assq 'full telega-i18n-month-names)) year)))

(defun telega-ins--msg-sender (msg-sender &optional with-avatar-p with-username-p)
  "Insert message's sender title."
  (telega-ins--with-face (telega-msg-sender-title-faces msg-sender)
    (telega-ins (telega-msg-sender-title msg-sender with-avatar-p)))
  (when with-username-p
    (when-let ((username (telega-msg-sender-username msg-sender)))
      (telega-ins--with-face 'shadow
        (telega-ins " • "))
      (telega-ins--with-face 'telega-username
        (telega-ins "@" username)))))

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

(defun telega-ins--user-relationship (user)
  "Insert relationship with USER.
🚹←🚹 - if user is contact.
🚹↔🚹 - if user is mutual contact."
  (let ((contact-p (plist-get user :is_contact))
        (mutual-contact-p (plist-get user :is_mutual_contact)))
    (when (or contact-p mutual-contact-p)
      (telega-ins (propertize "🚹" 'face 'shadow)
                  (if mutual-contact-p "↔" "←")
                  (propertize "🚹" 'face 'bold)))))

(defun telega-ins--user-nearby-distance (user)
  "Insert distance to the USER nearby."
  (when-let* ((user-chat (telega-chat-get (plist-get user :id) 'offline))
              (distance (telega-chat-nearby-distance user-chat)))
    (telega-ins--with-face 'shadow
      (telega-ins (telega-distance-human-readable distance) " away"))))

(defun telega-ins--user (user &optional member show-phone-p)
  "Insert USER, aligning multiple lines at current column.
MEMBER specifies corresponding \"ChatMember\" object.
If SHOW-PHONE-P is non-nil, then show USER's phone number."
  (let ((avatar (telega-msg-sender-avatar-image user))
        (off-column (telega-current-column)))
    (telega-ins--image avatar 0
                       :no-display-if (not telega-user-show-avatars))
    (telega-ins--msg-sender user nil 'with-username)
    (telega-ins--with-face 'shadow
      (when (and member
                 (telega-ins-prefix " ("
                   (telega-ins--chat-member-status
                    (plist-get member :status))))
        (telega-ins ")")))

    (when show-phone-p
      (when-let ((phone-number (telega-tl-str user :phone_number)))
        (telega-ins--with-face 'shadow
          (telega-ins " • "))
        (telega-ins "+" phone-number)))

    ;; Insert (him)in<-->out(me) relationship
    (when (and telega-user-show-relationship
               (not (telega-me-p user)))
      (telega-ins " ")
      (telega-ins--user-relationship user))
    ;; Block/scam mark, without requesting
    (when (telega-msg-sender-blocked-p user 'locally)
      (telega-ins " " (telega-symbol 'blocked)))

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
      (telega-ins--raw-button (telega-link-props 'user inviter-id)
        (telega-ins--msg-sender inviter-user 'with-avatar)))
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
    (telega-ins " ")
    (telega-button--insert (or button-type 'telega-user) (car users))

    (setq users (cdr users))
    (when users
      (telega-ins "\n")
      ;; NOTE: to apply `height' property \n must be included
      (telega-ins--with-props
          '(face default display ((space-width 2) (height 0.5)))
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
         (let ((progress (telega-file--uploading-progress file))
               (nbsp-char 160))
           (telega-ins "[")
           (telega-ins-progress-bar
            progress 1.0 10 telega-symbol-upload-progress nbsp-char)
           (telega-ins-fmt "%d%%]%c" (round (* progress 100)) nbsp-char)
           (telega-ins--button "Cancel"
             'action (lambda (_ignored)
                       (telega-msg-delete0 msg)))))

        ((telega-file--downloading-p file)
         (let ((progress (telega-file--downloading-progress file))
               (nbsp-char 160))
           (telega-ins "[")
           (telega-ins-progress-bar
            progress 1.0 10 telega-symbol-download-progress nbsp-char)
           (telega-ins-fmt "%d%%]%c" (round (* progress 100)) nbsp-char)
           (telega-ins--button (telega-i18n "lng_media_cancel")
             'action (lambda (_ignored)
                       (telega--cancelDownloadFile file)))))

        ((not (telega-file--downloaded-p file))
         (let ((progress (telega-file--downloading-progress file))
               (nbsp-char 160))
           (when (> progress 0)
             (telega-ins "[")
             (telega-ins-progress-bar progress 1.0 10 (cons ?= ?⏸) nbsp-char)
             (telega-ins-fmt "%d%%]%c" (round (* progress 100)) nbsp-char))
           (telega-ins--button (if (> progress 0)
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
      (telega-ins--with-face 'telega-msg-outgoing-status
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
                telega-symbol-heavy-checkmark)
               (t telega-symbol-checkmark)))))))

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
      (when show-progress
        (telega-ins " ")
        (telega-ins--file-progress msg hr-file))
      (telega-ins "\n"))

    (if (telega--tl-get msg :content :is_secret)
        (let ((ttl-in (or (plist-get msg :ttl_expires_in) 0)))
          (when (> ttl-in 0)
            (telega-ins (propertize "Self-descruct in" 'face 'shadow) " "
                        (telega-duration-human-readable ttl-in) "\n"))
          (telega-ins--image-slices
           (telega-self-destruct-create-svg
            (plist-get photo :minithumbnail)
            (telega-symbol (if (> ttl-in 0) 'flames 'lock)))))

      (telega-ins--image-slices
       (telega-photo--image photo (or limits telega-photo-size-limits)))
      )))

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
                (telega-link-props 'file local-path 'telega-link)
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
        (telega-ins--button (telega-i18n "lng_mac_menu_player_stop")
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
  (unless video
    (setq video (telega--tl-get msg :content :video)))
  (let ((video-name (telega-tl-str video :file_name))
        (video-file (telega-file--renew video :video)))
    (telega-ins (telega-symbol 'video) " ")
    (if (telega-file--downloaded-p video-file)
        (let ((local-path (telega--tl-get video-file :local :path)))
          (telega-ins--raw-button
              (telega-link-props 'file local-path 'telega-link)
            (telega-ins (telega-short-filename local-path))))
      (telega-ins (or video-name "")))
    (telega-ins-fmt " (%dx%d %s %s)"
      (plist-get video :width)
      (plist-get video :height)
      (file-size-human-readable (telega-file--size video-file))
      (telega-duration-human-readable (plist-get video :duration)))
    (telega-ins-prefix " "
      (telega-ins--file-progress msg video-file))

    ;; Video's thumbnail, if any
    (unless no-thumbnail-p
      (if (telega--tl-get msg :content :is_secret)
          ;; Secret video
          (let ((ttl-in (or (plist-get msg :ttl_expires_in) 0)))
            (when (> ttl-in 0)
              (telega-ins "\n"
                          (propertize "Self-descruct in" 'face 'shadow) " "
                          (telega-duration-human-readable ttl-in)
                          "\n"))
            (telega-ins--image-slices
             (telega-self-destruct-create-svg
              (plist-get video :minithumbnail)
              (telega-symbol (if (> ttl-in 0) 'flames 'lock)))))

        (let ((thumb (plist-get video :thumbnail))
              (minithumb (plist-get video :minithumbnail)))
          (when (or thumb minithumb)
            (telega-ins "\n")
            (telega-ins--image-slices
             (telega-media--image
              ;; TODO: use `telega-video--create-svg'
              (cons video 'telega-thumb-or-minithumb--create-image)
              (cons thumb :file)))
            (telega-ins " ")))))
    t))

(defun telega-ins--ffplay-controls (msg &optional no-2x-button)
  "Insert controls for voice/video notes.
If NO-2X-BUTTON is specified, then do not display \"2x\" button."
  (telega-ins--button (telega-symbol "⏪")
    'action (lambda (_button)
              (telega-msg--vvnote-rewind msg -10)))
  (telega-ins " ")
  (telega-ins--button (telega-symbol "⏩")
    'action (lambda (_button)
              (telega-msg--vvnote-rewind msg 10)))
  (telega-ins " ")
  (unless no-2x-button
    (let* ((label2x "2×")
           (ends (if (functionp telega-button-endings)
                     (funcall telega-button-endings label2x)
                   telega-button-endings)))
      (setq label2x (concat (or (car ends) "[") label2x (or (cdr ends) "]")))

      (telega-ins--button label2x
        'face (if (eq telega-vvnote-play-speed 1)
                  'telega-button
                'telega-button-active)
        :value msg
        :action #'telega-msg--vvnote-play-speed-toggle
        'action #'telega-button--action))
    (telega-ins " "))
  (telega-ins--button (telega-i18n "lng_mac_menu_player_stop")
    :value msg
    :action #'telega-msg--vvnote-stop))

(defun telega-ins--speech-recognition-button (recognition for-msg)
  "Insert speech recognize button."
  ;; NOTE: if previous recognition results in error, then also show
  ;; the recognize button
  (when (and (or (not recognition)
                 (eq 'speechRecognitionResultError
                     (telega--tl-type recognition)))
             (telega-user-match-p (telega-user-me) 'is-premium))
    (telega-ins " ")
    (telega-ins--button (concat "🠆A" (telega-symbol 'premium))
      :value for-msg
      :action #'telega--recognizeSpeech)))

(defun telega-ins--speech-recognition-text (recognition)
  "Insert results of the voice/video message recognition."
  (cl-assert recognition)
  (cl-ecase (telega--tl-type recognition)
    (speechRecognitionResultPending
     (telega-ins--with-face 'shadow
       (telega-ins (or (telega-tl-str recognition :partial_text)
                       "Recognizing")
                   "...")))
    (speechRecognitionResultText
     (telega-ins--with-face 'shadow
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

    ;; Duration
    (telega-ins " (" (telega-duration-human-readable dur) ")")

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
      (telega-ins--speech-recognition-button recognition msg)
      (when recognition
        (telega-ins "\n")
        (telega-ins--speech-recognition-text recognition)))
    ))

(defun telega-ins--video-note (msg &optional video-note)
  "Insert message MSG with VIDEO-NOTE content."
  (let* ((note (or video-note (telega--tl-get msg :content :video_note)))
         (note-file (telega-file--renew note :video))
         (recognition (plist-get note :speech_recognition_result))
         (ffplay-proc (plist-get msg :telega-ffplay-proc)))
    (telega-ins (propertize "NOTE" 'face 'shadow))
    (telega-ins-fmt " (%dx%d %s %s)"
      (plist-get note :length) (plist-get note :length)
      (file-size-human-readable (telega-file--size note-file))
      (telega-duration-human-readable (plist-get note :duration)))
    (when (telega--tl-get msg :content :is_viewed)
      (telega-ins (telega-symbol 'eye)))

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

    (telega-ins--speech-recognition-button recognition msg)

    (telega-ins-prefix " "
      (telega-ins--file-progress msg note-file))

    (let ((thumb (plist-get note :thumbnail))
          (minithumb (plist-get note :minithumbnail)))
      (when-let ((img (or (plist-get msg :telega-ffplay-frame)
                          (when (or minithumb thumb)
                            (telega-media--image
                             (cons note 'telega-vvnote-video--create-image)
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
        (telega-ins--with-face 'telega-link
          (telega-ins (telega-short-filename
                       (telega--tl-get doc-file :local :path))))
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
    (telega-ins (telega-symbol 'game) " "
                (propertize "GAME" 'face 'shadow)
                "\n")
    (telega-ins (telega-symbol 'vertical-bar))
    (telega-ins--with-attrs (list :fill-prefix (telega-symbol 'vertical-bar)
                                  :fill-column (- telega-chat-fill-column 4)
                                  :fill 'left)
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
    (telega-ins (telega-symbol 'vertical-bar))
    (telega-ins--with-attrs (list :fill-prefix (telega-symbol 'vertical-bar)
                                  :fill-column (- telega-chat-fill-column 4)
                                  :fill 'left)
      (when-let ((sitename (telega-tl-str web-page :site_name)))
        (telega-ins--with-face 'telega-webpage-sitename
          (telega-ins sitename))
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

      ;; NOTE: animation uses it's own thumbnails
      (let ((photo (plist-get web-page :photo)))
        (when (and photo (not (plist-get web-page :animation))
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
                msg (plist-get web-page :video) (when photo 'no-thumbnail))
               (telega-ins "\n"))
              ((plist-get web-page :video_note)
               (telega-ins--video-note msg (plist-get web-page :video_note))
               (telega-ins "\n"))
              ((plist-get web-page :voice_note)
               (telega-ins--voice-note msg (plist-get web-page :voice_note))
               (telega-ins "\n"))
              )))

    ;; Additional View button
    (if (zerop (plist-get web-page :instant_view_version))
        (when-let ((title (pcase (plist-get web-page :type)
                            ("telegram_channel"
                             (telega-i18n "lng_view_button_channel"))
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
                             "VIEW CHAT")
                            ("telegram_voicechat"
                             "JOIN AS LISTENER")
                            )))
          (telega-ins--button (concat "   " (upcase title) "   ")
            'action 'telega-msg-button--action))

      (telega-ins--button
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

(defun telega-ins--location-live (msg)
  "Insert live location description for location message MSG."
  ;; NOTE: in case of unexpired live location show last update
  ;; time and expiration period
  (when-let ((live-for-spec (telega-msg-location-live-for msg)))
    (cl-destructuring-bind (live-for updated-ago) live-for-spec
      (telega-ins " " (propertize "Live" 'face 'shadow))
      (when (> live-for 0)
        (telega-ins-fmt " for %s"
          (telega-duration-human-readable live-for 1))
        (telega-ins-fmt " (updated %s ago)"
          (telega-duration-human-readable updated-ago 1))
        (telega-ins " ")
        (telega-ins--button (telega-i18n "telega_stop_live_location" :count 1)
          'action (lambda (_button)
                    (telega--editMessageLiveLocation msg nil))))

      (when (plist-get msg :can_be_edited)
        (let ((proximity-radius
               (telega--tl-get msg :content :proximity_alert_radius)))
          (telega-ins "\n")
          (telega-ins "Proximity Alert Radius: ")
          (unless (zerop proximity-radius)
            (telega-ins (telega-distance-human-readable proximity-radius)
                        " "))
          (telega-ins--button (if (zerop proximity-radius)
                                  "Set"
                                "Change")
            'action (lambda (_ignored)
                      (telega--editMessageLiveLocation
                       msg (telega--tl-get msg :content :location)
                       :proximity-alert-radius
                       (read-number "Proximity Alert Radius (meters): "))))))
      )))

(defun telega-ins--contact (contact &optional no-phone)
  "One line variant inserter for CONTACT."
  (telega-ins (telega-symbol 'contact) " ")
  (telega-ins (telega-user-title contact 'name))
  (when (eq (telega--tl-type contact) 'user)
    (telega-ins--user-online-status contact))

  (unless no-phone
    (telega-ins " (" (plist-get contact :phone_number) ")")))

(defun telega-ins--contact-msg (msg)
  "Inserter for contact message MSG."
  ;; Two lines for the contact
  (let* ((content (plist-get msg :content))
         (contact (plist-get content :contact))
         (user-id (plist-get contact :user_id))
         (user (unless (zerop user-id) (telega-user-get user-id)))
         (user-ava (when (and telega-user-show-avatars user)
                     (telega-msg-sender-avatar-image user))))
    (when user-ava
      (telega-ins--image user-ava 0))
    (telega-ins--contact (plist-get content :contact))
    (telega-ins "\n")
    (when user-ava
      (telega-ins--image user-ava 1))
    (telega-ins--button (concat "   VIEW CONTACT   ")
      'action 'telega-msg-button--action)))

(defun telega-ins--call-msg (msg)
  "Insert call message MSG."
  (let* ((content (plist-get msg :content))
         (video-p (plist-get content :is_video))
         (call-symbol (telega-symbol (if video-p 'video 'phone)))
         (reason (telega--tl-type (plist-get content :discard_reason)))
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
    (telega-ins (cond ((memq reason '(callDiscardReasonMissed
                                      callDiscardReasonDeclined))
                       (propertize call-symbol 'face 'error))
                      ((plist-get msg :is_outgoing)
                       (concat call-symbol "→"))
                      (t
                       (concat call-symbol "←")))
                " ")
    (telega-ins (propertize label 'face 'shadow))
    (telega-ins-fmt " (%s)"
      (telega-duration-human-readable
       (plist-get content :duration)))))

(defun telega-ins--dice-msg (msg &optional one-line-p)
  "Inserter for the \"messageDice\" MSG."
  (let* ((content (plist-get msg :content))
         (dice-value (plist-get content :value))
         (dice-emoji (telega-tl-str content :emoji)))
    (telega-ins (or dice-emoji (car telega-symbol-dice-list)) " ")
    (telega-ins--with-face 'shadow
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
    (telega-ins--with-face 'shadow
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
         (max-opt-len (apply #'max
                             (mapcar #'length
                                     (mapcar (telega--tl-prop :text)
                                             options))))
         (option-symbols (if quiz-p
                             telega-symbol-quiz-options
                           (if multiple-answers-p
                               telega-symbol-poll-multiple-options
                             telega-symbol-poll-options)))
         (opt-sym-len (apply #'max (mapcar #'string-width option-symbols))))
    ;; Poll header
    ;; NOTE: Currently all polls are anonymous, this might be changed
    ;; in future See https://telegram.org/blog/polls
    (telega-ins (telega-symbol 'poll) " ")
    (telega-ins--with-face 'shadow
      (telega-ins-i18n (cond ((and anonymous-p quiz-p) "lng_polls_anonymous_quiz")
                             (anonymous-p "lng_polls_anonymous")
                             (quiz-p "lng_polls_public_quiz")
                             (t "lng_polls_public"))))
    (when (and quiz-p (plist-get poll-type :explanation))
      (telega-ins " " (telega-symbol 'bulp)))
    ;; I18N: polls_votes_count -> {count} votes
    (telega-ins ", " (telega-i18n (if quiz-p
                                      "lng_polls_answers_count"
                                    "lng_polls_votes_count")
                                  :count (plist-get poll :total_voter_count)))
    (when closed-p
        (telega-ins ", " (propertize "closed" 'face 'error)))
    (when (and (not closed-p) (plist-get msg :can_be_edited))
      (telega-ins " ")
      (telega-ins--button (concat "Close " (if quiz-p "Quiz" "Poll"))
        'action (lambda (_ignored)
                  (when (yes-or-no-p (telega-i18n "lng_polls_stop_warning"))
                    (telega--stopPoll msg)))))
    (telega-ins "\n")

    ;; Question and options
    (telega-ins--with-face 'bold
      (telega-ins (telega-tl-str poll :question)))
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
                            (set-keymap-parent map button-map)
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

          (telega-ins " " (telega-tl-str popt :text)))
        (when (or choices closed-p)
          (telega-ins "\n")
          (telega-ins (make-string opt-sym-len ?\s) " ")
          (telega-ins-fmt "%2d%% " (plist-get popt :vote_percentage))
          (telega-ins--image
           (telega-poll-create-svg
            max-opt-len (plist-get popt :vote_percentage)))
          (telega-ins--with-face 'shadow
            (telega-ins " ")
            (telega-ins-i18n (if quiz-p
                                 "lng_polls_answers_count"
                               "lng_polls_votes_count")
              :count (plist-get popt :voter_count))))
        ))

    (unless anonymous-p
      (telega-ins "\n")
      (telega-ins--button "  VIEW RESULTS  "
        :value msg
        :action #'telega-msg-open-poll))
    ))

(defun telega-ins--animation-msg (msg &optional animation)
  "Inserter for animation message MSG.
If NO-THUMBNAIL-P is non-nil, then do not insert thumbnail."
  (unless animation
    (setq animation (telega--tl-get msg :content :animation)))
  (let ((anim-file (telega-file--renew animation :animation)))
    (telega-ins (propertize "GIF" 'face 'shadow) " ")
    (if (telega-file--downloaded-p anim-file)
        (let ((local-path (telega--tl-get anim-file :local :path)))
          (telega-ins--raw-button
              (telega-link-props 'file local-path 'telega-link)
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
                  :sender (unless venue-p
                            (plist-get msg :sender))))
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
              (telega-ins--button " + "
                'action (lambda (_ignore)
                          (when (telega-map--zoom map 1)
                            (plist-put msg :telega-map
                                       (plist-put map :user-location nil))
                            (telega-msg-redisplay msg)))))
             ((= slice-num 2)
              (telega-ins " ")
              (telega-ins--button " - "
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
      (telega-ins--with-face 'shadow
        (telega-ins (telega-tl-str venue :address))))
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
     (telega-ins (abbreviate-file-name (plist-get document :path))))
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
        (list 'messageContactRegistered 'messageChatAddMembers
              'messageChatJoinByLink 'messageChatDeleteMember
              'messageChatChangeTitle 'messageSupergroupChatCreate
              'messageBasicGroupChatCreate 'messageCustomServiceAction
              'messageChatSetTtl 'messageExpiredPhoto 'messageExpiredVideo
              'messageChatChangePhoto 'messageChatDeletePhoto
              'messageChatUpgradeTo 'messageChatUpgradeFrom
              'messagePinMessage 'messageScreenshotTaken
              'messageGameScore
              'messageProximityAlertTriggered
              'messageVideoChatScheduled
              'messageVideoChatStarted
              'messageVideoChatEnded
              'messageInviteVideoChatParticipants
              'messageWebsiteConnected
              'messageChatSetTheme
              'messagePaymentSuccessful
              'messageForumTopicCreated
              'messageForumTopicEdited
              'messageForumTopicIsClosedToggled
              'telegaInternal)))

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
         (sender-name
          (when sender
            (telega-ins--as-string
             (telega-ins--raw-button
                 (list 'action (lambda (_button)
                                 (telega-describe-msg-sender sender)))
               (telega-ins--with-face 'bold
                 (telega-ins (telega-msg-sender-title sender t))))))))
    (cl-case (telega--tl-type content)
      (messageWebsiteConnected
       (telega-ins-i18n "lng_action_bot_allowed_from_domain"
         :domain (telega-tl-str content :domain_name)))
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
             :user (mapconcat (lambda (user)
                                (propertize (telega-msg-sender-title user t)
                                            'face 'bold))
                              (mapcar #'telega-user-get user-ids)
                              ", ")))))
      (messageChatJoinByLink
       (telega-ins-i18n "lng_action_user_joined_by_link" :from sender-name))
      (messageChatDeleteMember
       (let* ((user (telega-user-get (plist-get content :user_id)))
              (user-name (propertize (telega-msg-sender-title user t)
                                     'face 'bold)))
         (if (eq sender user)
             (telega-ins-i18n "lng_action_user_left" :from sender-name)
           (telega-ins-i18n "lng_action_kick_user"
             :from sender-name :user user-name))))

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
           :from (propertize (telega-msg-sender-title traveler t) 'face 'bold)
           :user (propertize (telega-msg-sender-title watcher t) 'face 'bold)
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
                (telega-ins--date-iso8601 (plist-get content :start_date)))))
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
         :users (mapconcat (lambda (user)
                             (propertize (telega-msg-sender-title user t)
                                         'face 'bold))
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
                 (propertize (telega-msg-sender-title from-user t) 'face 'bold))))
      (messageForumTopicCreated
       (telega-ins sender-name "→")
       (telega-ins-i18n "lng_action_topic_created"
         :topic (telega-ins--as-string
                 (telega-ins--content-one-line msg))))
       (messageForumTopicIsClosedToggled
        (telega-ins sender-name "→")
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
    (telega-ins--with-face 'shadow
      (telega-ins-i18n "telega_scheduled"))
    (telega-ins " ")
    (if-let ((send-date (plist-get scheduled :send_date)))
        (telega-ins-i18n "telega_scheduled_at_date"
          :date (telega-ins--as-string (telega-ins--date-iso8601 send-date)))
      (telega-ins-i18n "telega_scheduled_when_online"))
    (telega-ins "\n"))

  (let* ((content (plist-get msg :content))
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
       (let ((emojis-text (when (and telega-emoji-use-images
                                     telega-emoji-large-height
                                     (telega-msg-emojis-only-p msg)
                                     (not (plist-get content :web_page)))
                            (telega--desurrogate-apply
                             (telega--tl-get content :text :text)))))
         (cond (emojis-text
                (telega-ins--image-slices
                    (telega-emoji-create-svg
                     emojis-text telega-emoji-large-height)))
               ((and translated translated-replaces-p)
                (telega-ins (telega-tl-str translated :text)))
               (t
                (telega-ins--fmt-text (plist-get content :text) msg))))
       (telega-ins-prefix "\n"
         (telega-ins--webpage msg)))
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
            (telega-ins--with-face 'shadow
              (telega-ins "Animated Sticker\n")))
           (stickerFormatWebm
            (telega-ins--with-face 'shadow
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
       (telega-ins--dice-msg msg))
      ('messageAnimatedEmoji
       (telega-ins--animated-emoji-msg msg))
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
      (telega-ins--with-face 'shadow
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

    (telega-ins--button kbdb-text
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
  (telega-ins--button "SHOW RECEIPT"
    'action (lambda (_ignored)
              (let* ((receipt-msg-id (telega--tl-get msg :content :receipt_message_id))
                     (receipt (telega-server--call
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

(defmacro telega-ins--aux-inline (title face &rest body)
  "Execute BODY inserters prefixing with TITLE.
Display text using FACE."
  (declare (indent 2))
  `(progn
     (telega-ins--with-attrs  (list :max (- telega-chat-fill-column
                                            (telega-current-column))
                                    :elide t
                                    :face ,face)
       (telega-ins "| " ,title ": ")
       (progn ,@body)
       (when telega-msg-heading-whole-line
         (telega-ins "\n")))
     (unless telega-msg-heading-whole-line
       (telega-ins "\n"))))

(defmacro telega-ins--aux-inline-reply (&rest body)
  `(telega-ins--aux-inline
       (telega-i18n "lng_in_reply_to") 'telega-msg-inline-reply
     ,@body))

(cl-defun telega-ins--aux-msg-one-line (msg &key with-username
                                            username-face remove-caption)
  "Insert contents for aux message MSG as one line.
If WITH-USERNAME is non-nil then insert MSG sender as well.
USERNAME-FACE specifies face to use for sender's title.
if WITH-USERNAME is `unread-mention', then outline sender with
`telega-mention-count' face.
REMOVE-CAPTION is passed directly to `telega-ins--content-one-line'."
  (declare (indent 1))
  (when (and with-username
             (telega-ins--with-face username-face
               (let ((sender (telega-msg-sender msg)))
                 (telega-ins (or (telega-msg-sender-username sender 'with-@)
                                 (telega-msg-sender-title sender))))))
    (telega-ins "> "))
  (telega-ins--content-one-line msg remove-caption))

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
      (telega-ins-fmt " %s%d" (telega-symbol 'eye) view-count))
    (when (and fwd-count (not (zerop fwd-count)))
      (telega-ins " ")
      (let ((fwd-count-label (concat (telega-symbol 'forward)
                                     (int-to-string fwd-count))))
        (if (and (telega-chat-channel-p msg-chat)
                 (telega-chat-match-p msg-chat '(me-is-owner or-admin)))
            (telega-ins--button fwd-count-label
              'face 'telega-link
              :value msg
              :action #'telega-msg-public-forwards)
          (telega-ins fwd-count-label))))
    (when (and (plist-get msg :can_get_message_thread)
               (> reply-count 0))
      (telega-ins " ")
      (telega-ins--button
          (format "%s%d" (telega-symbol 'reply) reply-count)
        'face 'telega-link
        :action #'telega-msg-open-thread
        :help-echo "Show message thread"))
    t))

(defun telega-ins--msg-comments (msg &optional msg-chat)
  "Insert \"Comments\" section for the message MSG."
  (when (and (plist-get msg :can_get_message_thread)
             (telega-chat-channel-p (or msg-chat (telega-msg-chat msg))))
    (let* ((msg-ri (telega--tl-get msg :interaction_info :reply_info))
           (reply-count (or (plist-get msg-ri :reply_count) 0))
           (recent-repliers (append (plist-get msg-ri :recent_replier_ids) nil)))
      (telega-ins--button
          (telega-ins--as-string
           (if recent-repliers
               (seq-doseq (rr recent-repliers)
                 (telega-ins--image (telega-msg-sender-avatar-image-one-line
                                     (telega-msg-sender rr))))
             (telega-ins (telega-symbol 'leave-comment)))
           (telega-ins " " (if (zerop reply-count)
                               (telega-i18n "lng_comments_open_none")
                             (concat (telega-i18n "lng_comments_open_count"
                                       :count reply-count)
                                     (when (telega-msg-replies-has-unread-p msg)
                                       "•")))))
        ;; Use custom :action for clickable comments button
        :action #'telega-msg-open-thread
        :help-echo "Open comments in discussion group"))))

(defun telega-ins--message-header (msg &optional msg-chat msg-sender
                                       addon-inserter)
  "Insert message's MSG header, everything except for message content.
MSG-CHAT - Chat for which to insert message header.
MSG-SENDER - Sender of the message.
If ADDON-INSERTER function is specified, it is called with one
argument - MSG to insert additional information after header."
  ;; twidth including 10 chars of date
  (let* ((fwidth (- telega-chat-fill-column (telega-current-column)))
         (twidth (+ 10 fwidth))
         (chat (or msg-chat (telega-msg-chat msg)))
         (sender (or msg-sender (telega-msg-sender msg))))
    (cl-assert sender)
    (telega-ins--with-face 'telega-msg-heading
      (telega-ins--with-attrs (list :max twidth :align 'left :elide t)
        (unless (plist-get msg :can_be_saved)
          (telega-ins (telega-symbol 'copyright)))
        (telega-ins--with-face (telega-msg-sender-title-faces sender)
          ;; Message title itself
          (telega-ins (telega-msg-sender-title sender))
          (telega-ins-prefix " @"
            (telega-ins (telega-msg-sender-username sender)))

          ;; Admin badge if any
          (when (telega-user-p sender)
            (when-let ((admin (telega-chat-admin-get chat sender)))
              (telega-ins--with-face 'shadow
                (telega-ins " ("
                            (or (telega-tl-str admin :custom_title)
                                (if (plist-get admin :is_owner)
                                    (telega-i18n "lng_owner_badge")
                                  (telega-i18n "lng_admin_badge")))
                            ")"))))

          ;; Signature for channel posts and anonymous admin messages
          (when-let ((signature (telega-tl-str msg :author_signature)))
            (telega-ins " --" signature)))

        ;; via <bot>
        (let* ((via-bot-user-id (plist-get msg :via_bot_user_id))
               (via-bot (unless (zerop via-bot-user-id)
                          (telega-user-get via-bot-user-id))))
          (when via-bot
            (telega-ins " via ")
            ;; Use custom :action for clickable @bot link
            (telega-ins--button (telega-user-title via-bot 'short)
              'face 'telega-link          ;no button outline please
              :action (lambda (_msg_ignored)
                        (telega-describe-user via-bot)))))

        ;; Edited date
        (telega-ins-prefix " edited at "
          (unless (zerop (plist-get msg :edit_date))
            (telega-ins--date (plist-get msg :edit_date))))

        ;; Interaction info
        (telega-ins--msg-interaction-info msg chat)

        (when-let ((fav (telega-msg-favorite-p msg)))
          (telega-ins " " (telega-symbol 'favorite))
          ;; Also show comment to the favorite message
          (telega-ins--with-face 'shadow
            (telega-ins-prefix "("
              (when (telega-ins (plist-get fav :comment))
                (telega-ins ")")))))

        ;; Maybe pinned message?
        (when (plist-get msg :is_pinned)
          (telega-ins " " (telega-symbol 'pin)))

        ;; Show language code if translation replaces message's content
        (when-let ((translated (plist-get msg :telega-translated)))
          (when (with-telega-chatbuf chat
                  telega-translate-replace-content)
            (telega-ins--with-face 'shadow
              (telega-ins " [→" (plist-get translated :to_language_code) "]"))))

        (when (numberp telega-debug)
          (telega-ins-fmt " (ID=%d)" (plist-get msg :id)))

        ;; Resend button in case message sent failed
        ;; Use custom :action to resend message
        (when-let ((send-state (plist-get msg :sending_state)))
          (when (and (eq (telega--tl-type send-state) 'messageSendingStateFailed)
                     (plist-get send-state :can_retry))
            (telega-ins " ")
            (telega-ins--button "RESEND"
              :action 'telega--resendMessage)))

        (when addon-inserter
          (cl-assert (functionp addon-inserter))
          (funcall addon-inserter msg)))

      (if telega-msg-heading-whole-line
          (telega-ins "\n")
        (telega-ins--move-to-column (+ 10 1 telega-chat-fill-column))))

    (unless telega-msg-heading-whole-line
      (telega-ins "\n"))))

(defun telega--fwd-info-action (fwd-info)
  "Action to take when button with FWD-INFO displayed is clicked."
  (let* ((origin (plist-get fwd-info :origin))
         (origin-chat-id (plist-get origin :chat_id))
         (origin-msg-id (plist-get origin :message_id))
         (origin-sender-id (plist-get origin :sender_user_id))
         (from-chat-id (plist-get fwd-info :from_chat_id))
         (from-msg-id (plist-get fwd-info :from_message_id))
         (chat-id (if (and from-chat-id (not (zerop from-chat-id)))
                      from-chat-id
                    origin-chat-id))
         (msg-id (if (and from-msg-id (not (zerop from-msg-id)))
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
                                     :face 'telega-msg-inline-forward)
        (telega-ins "| Forwarded From: ")
        (let* ((origin (plist-get fwd-info :origin))
               (sender nil)
               (from-chat-id (plist-get fwd-info :from_chat_id))
               (from-chat (when (and from-chat-id (not (zerop from-chat-id)))
                            (telega-chat-get from-chat-id))))
          ;; Insert forward origin first
          (cl-ecase (telega--tl-type origin)
            (messageForwardOriginChat
             (setq sender (telega-chat-get (plist-get origin :sender_chat_id)))
             (when telega-chat-show-avatars
               (telega-ins--image
                (telega-msg-sender-avatar-image-one-line sender)))
             (telega-ins "[" (telega-msg-sender-title sender) "]"))

            (messageForwardOriginUser
             (setq sender (telega-user-get (plist-get origin :sender_user_id)))
             (when telega-user-show-avatars
               (telega-ins--image
                (telega-msg-sender-avatar-image-one-line sender)))
             (telega-ins "{" (telega-user-title sender 'full) "}"))

            ((messageForwardOriginHiddenUser messageForwardOriginMessageImport)
             (telega-ins (telega-tl-str origin :sender_name)))

            (messageForwardOriginChannel
             (setq sender (telega-chat-get (plist-get origin :chat_id)))
             (when telega-chat-show-avatars
               (telega-ins--image
                (telega-msg-sender-avatar-image-one-line sender)))
             (telega-ins (telega-chat-title-with-brackets sender " "))))

          (when-let ((signature (telega-tl-str origin :author_signature)))
            (telega-ins " --" signature))

          (when (and from-chat
                     (not (or (eq sender from-chat)
                              (and (telega-user-p sender)
                                   (eq sender (telega-chat-user from-chat))))))
            (telega-ins "→")
            (if telega-chat-show-avatars
                (telega-ins--image
                 (telega-msg-sender-avatar-image-one-line from-chat))
              (telega-ins (telega-chat-title-with-brackets from-chat " ")))))

        (let ((date (plist-get fwd-info :date)))
          (unless (zerop date)
            (telega-ins " at ")
            (telega-ins--date date)))
        (when telega-msg-heading-whole-line
          (telega-ins "\n")))
      (unless telega-msg-heading-whole-line
        (telega-ins "\n")))
    t))

(defun telega-ins--msg-reply-inline (msg)
  "For message MSG insert reply header in case MSG is replying to some message."
  ;; NOTE: Do not show reply inline if replying to thread's root message.
  ;; If replied message is not instantly available, it will be fetched
  ;; later by the `telega-msg--replied-message-fetch'
  (unless (or (zerop (plist-get msg :reply_to_message_id))
              (eq (plist-get telega-chatbuf--thread-msg :id)
                  (plist-get msg :reply_to_message_id)))
    (let ((replied-msg (telega-msg--replied-message msg)))
      (cond ((or (null replied-msg) (eq replied-msg 'loading))
             ;; NOTE: replied message will be fetched by the
             ;; `telega-msg--replied-message-fetch'
             (telega-ins--aux-inline-reply
              (telega-ins-i18n "lng_profile_loading")))
            ((telega--tl-error-p replied-msg)
             (telega-ins--aux-inline-reply
              (telega-ins--with-face 'shadow
                (telega-ins (telega-i18n "lng_deleted_message")))))
            ((telega-msg-match-p replied-msg 'ignored)
             (telega-ins--aux-inline-reply
              (telega-ins--message-ignored replied-msg)))
            (t
             (telega-ins--with-props
                 ;; When pressed, then jump to the REPLIED-MSG message
                 (list 'action
                       (lambda (_button)
                         (telega-msg-goto-highlight replied-msg)))
               (telega-ins--aux-inline-reply
                (telega-ins--aux-msg-one-line replied-msg
                  :with-username t
                  :username-face
                  (let* ((sender (telega-msg-sender replied-msg))
                         (sender-faces (telega-msg-sender-title-faces sender)))
                    (if (and (telega-sender-match-p sender 'me)
                             (plist-get msg :contains_unread_mention))
                        (append sender-faces '(telega-entity-type-mention))
                      sender-faces))))
               ))))))

(defun telega-ins--msg-sending-state-failed (msg)
  "Insert sending state failure reason for message MSG."
  (when-let ((send-state (plist-get msg :sending_state)))
    (when (eq 'messageSendingStateFailed (telega--tl-type send-state))
      (telega-ins (telega-symbol 'failed))
      (telega-ins--with-face 'error
        (telega-ins "Failed to send: "
                    (telega-tl-str send-state :error_message)))
      (cond ((and (telega-msg-match-p msg '(type Photo))
                  (equal (telega-tl-str send-state :error_message)
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
               (telega-ins--button "RESEND as file"
                 'action (lambda (_button)
                           (message "TODO: resend as file")))))
            )
      t)))

(defun telega-ins--message0 (msg &optional no-header
                                 addon-header-inserter no-footer)
  "Insert message MSG.
If NO-HEADER is non-nil, then do not display message header
unless message is edited.
ADDON-HEADER-INSERTER is passed directly to `telega-ins--message-header'."
  (declare (indent 2))
  (if (telega-msg-special-p msg)
      (telega-ins--with-attrs (list :min (- telega-chat-fill-column
                                            (telega-current-column))
                                    :align 'center
                                    :align-symbol 'horizontal-bar)
        (telega-ins--content msg))

    ;; Message header needed
    (let* ((chat (telega-msg-chat msg))
           ;; Is formatting done for "Replies" chat?
           ;; Workaround for case when `:forward_info' is unset (for
           ;; outgoing messages [what?] for example)
           (msg-for-replies-p (and (telega-replies-p chat)
                                   (plist-get msg :forward_info)))
           (sender (if msg-for-replies-p
                       (telega-replies-msg-sender msg)
                     (telega-msg-sender msg)))
           (avatar (if msg-for-replies-p
                       (telega-msg-sender-avatar-image-three-lines sender)
                     (telega-msg-sender-avatar-image sender)))
           (awidth (length (telega-image--telega-text avatar 0)))
           ;; NOTE: `telega-msg-contains-unread-mention' is used
           ;; inside `telega--entity-to-properties'
           (telega-msg-contains-unread-mention
            (plist-get msg :contains_unread_mention))
           ccol)
      (if (and no-header
               (zerop (plist-get msg :edit_date))
               (zerop (plist-get msg :via_bot_user_id)))
          (telega-ins (make-string awidth ?\s))

        ;; Show user profile when clicked on avatar, header
        (telega-ins--with-props
            (list 'action (lambda (button)
                            ;; NOTE: check for custom message :action first
                            ;; - [RESEND] button uses :action
                            ;; - via @bot link uses :action
                            (or (telega-button--action button)
                                (telega-describe-msg-sender sender))))
          (telega-ins--image avatar 0
                             :no-display-if (not telega-chat-show-avatars))
          (telega-ins--message-header msg chat sender addon-header-inserter)
          (telega-ins--image avatar 1
                             :no-display-if (not telega-chat-show-avatars))))

      (setq ccol (telega-current-column))
      (telega-ins--fwd-info-inline (plist-get msg :forward_info))
      ;; NOTE: Three lines avatars in "Replies" chat
      (when msg-for-replies-p
        (telega-ins--image avatar 2
                           :no-display-if (not telega-chat-show-avatars)))
      (telega-ins--msg-reply-inline msg)
      (telega-ins--column ccol telega-chat-fill-column
        (telega-ins--content msg)

        (telega-ins-prefix "\n"
          (telega-ins--msg-sending-state-failed msg))
        (when (telega-msg-match-p msg telega-msg-temex-show-reactions)
          (telega-ins-prefix "\n"
            (telega-ins--msg-reaction-list msg)))
        (telega-ins-prefix "\n"
          (telega-ins--reply-markup msg))
        (telega-ins-prefix "\n"
          (telega-ins--msg-comments msg chat))
        )))

  (unless no-footer
    ;; Footer: Date/status starts at `telega-chat-fill-column' column
    (telega-ins--move-to-column telega-chat-fill-column)
    (telega-ins--with-attrs (list :align 'right :min 10)
      ;; NOTE: telegaInternal messages has no `:date' property
      (when-let ((date (or (telega--tl-get msg :scheduling_state :send_date)
                           (plist-get msg :date))))
        (telega-ins--date date)))
    (telega-ins--outgoing-status msg))
  t)

(defun telega-ins--message-media-compact (msg &rest _ignored)
  "Insert for compact view of media messages."
  (let ((content (plist-get msg :content)))
    (cl-ecase (telega--tl-type content)
      (messagePhoto
       (telega-ins--image
        (telega-photo--image
         (plist-get content :photo) '(10 10 10 10))))
      )))

(defun telega-ins--message (msg &rest args)
  "Inserter for the message MSG.
Pass all ARGS directly to `telega-ins--message0'."
  (if (telega-msg-marked-p msg)
      (progn
        (telega-ins telega-symbol-mark)
        (telega-ins--with-attrs (list :fill-prefix telega-symbol-mark)
          (apply #'telega-ins--message0 msg args)))

    (when (plist-get msg :contains_unread_mention)
      (telega-ins telega-symbol-mention-mark))
    (when (telega-msg-match-p msg 'unread-reactions)
      (telega-ins telega-symbol-reaction-mark))
    (apply #'telega-ins--message0 msg args)))

(defun telega-ins--message-no-header (msg)
  "Insert message MSG without header."
  (funcall telega-inserter-for-msg-button msg :no-header))

(defun telega-ins--message-deleted (msg)
  "Inserter for deleted message MSG."
  (telega-ins--with-props (list 'face 'custom-invalid)
    ;; NOTE: For ignored MSG use `telega-ins--message-ignored'
    ;; inserter.  See https://github.com/zevlg/telega.el/issues/342
    (if (telega-msg-match-p msg 'ignored)
        (when telega-ignored-messages-visible
          (telega-ins--message-ignored msg))

      (funcall telega-inserter-for-msg-button msg nil
               (lambda (_ignoredmsg)
                 (telega-ins " ")
                 (telega-ins--with-face 'error
                   (telega-ins (telega-i18n "lng_deleted_message"))))))))

(defun telega-ins--message-ignored (_msg)
  "Inserter for ignored message MSG in chatbuf."
  (telega-ins (propertize "<Ignored Message>" 'face 'shadow)))

(defun telega-ins--message-with-chat-header (msg)
  "Inserter for message MSG showing chat header."
  (let ((telega-chat-fill-column (- telega-root-fill-column 10 1)))
    (telega-ins--with-attrs (list :max telega-root-fill-column
                                  :face 'telega-msg-heading
                                  :align 'left)
      (telega-ins--date (plist-get msg :date))
      (telega-ins " ")
      (telega-ins--chat (telega-msg-chat msg))
      (if telega-msg-heading-whole-line
          (telega-ins "\n")
        (telega-ins--move-to-column telega-root-fill-column)))
    (unless telega-msg-heading-whole-line
      (telega-ins "\n"))

    (telega-ins--message msg (telega-chat-channel-p (telega-msg-chat msg))
                         nil :no-footer)))

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
      (let* ((contact (plist-get imc :contact))
             (user-id (plist-get contact :user_id))
             (user (when user-id (telega-user-get user-id))))
        (when (and user telega-user-show-avatars)
          (telega-ins--image (telega-msg-sender-avatar-image-one-line user)))
        (telega-ins--contact contact)))
     (inputMessageDocument
      (telega-ins--input-file (plist-get imc :document)))
     (inputMessagePhoto
      (let ((ttl-text (when (plist-get imc :ttl)
                        (format ", self-destruct in: %s"
                                (telega-duration-human-readable
                                 (plist-get imc :ttl))))))
        (telega-ins--input-file (plist-get imc :photo) (telega-symbol 'photo)
                                ttl-text)))
     (inputMessageAudio
      (let* ((title (plist-get imc :title))
             (artist (plist-get imc :performer))
             (audio-description (concat (when title
                                          (propertize title 'face 'bold))
                                        (when artist
                                          (concat (when title " ")
                                                  "--" artist)))))
        (telega-ins telega-symbol-audio " ")
        (when (telega-ins audio-description)
          (telega-ins ","))
        (telega-ins--input-file (plist-get imc :audio) "")))
     (inputMessageVideo
      (let ((duration (or (plist-get imc :duration) 0))
            (width (plist-get imc :width))
            (height (plist-get imc :height))
            (ttl (plist-get imc :ttl)))
        (telega-ins--input-file
         (plist-get imc :video) (telega-symbol 'video)
         (concat " (" (when (and width height)
                        (format "%dx%d " width height))
                 (telega-duration-human-readable duration)
                 (when ttl
                   (format ", self-destruct in: %s"
                           (telega-duration-human-readable ttl)))
                 ")"))))
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
             (telega-vvnote-video--svg thumb-filename nil nil 'png))))
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
      (telega-ins--with-face 'shadow
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
          (telega-ins (propertize caption 'face 'shadow) " "))
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
            (telega-ins "via " (telega-user-title bot 'short))))))
     (telegaForwardMessage
      (telega-ins (telega-symbol 'forward) "Fwd")
      (when (plist-get imc :send_copy)
        (telega-ins " Copy"))
      (when (plist-get imc :remove_caption)
        (telega-ins " NewCap"))
      (telega-ins ": ")
      (telega-ins--with-attrs (list :align 'left
                                    :max 20
                                    :elide t)
        (telega-ins--content-one-line
         (plist-get imc :message) (plist-get imc :remove_caption))))
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
     (telegaDisableWebpagePreview
      (telega-ins-i18n "telega_disable_webpage_preview"))
     (telegaChatTheme
      (telega-ins "Theme: " (or (telega-tl-str imc :name)
                                "disable")))
     (t
      (telega-ins-fmt "<TODO: %S>" (telega--tl-type imc)))
     )))

(defun telega-ins--content-one-line (msg &optional remove-caption)
  "Insert message's MSG content for one line usage.
If REMOVE-CAPTION is specified, then do not insert caption."
  (telega-ins--one-lined
   (let ((content (plist-get msg :content))
         (telega-msg-contains-unread-mention
          (plist-get msg :contains_unread_mention)))
     (cl-case (telega--tl-type content)
       (messageText
        (telega-ins--fmt-text (plist-get content :text) msg))
       (messagePhoto
        (if-let ((preview-img (telega-msg--preview-photo-image msg)))
            (telega-ins--image preview-img)
          (telega-ins (telega-symbol 'photo)))
        (telega-ins " ")
        (or (telega-ins--fmt-text
             (unless remove-caption (plist-get content :caption)) msg)
            ;; I18N: lng_in_dlg_photo or lng_attach_photo
            (telega-ins (propertize "Photo" 'face 'shadow))))
       (messageDocument
        (telega-ins (telega-symbol 'attachment) " ")
        (or (telega-ins (telega--tl-get content :document :file_name))
            (telega-ins--fmt-text
             (unless remove-caption (plist-get content :caption)) msg)
            (telega-ins (propertize "Document" 'face 'shadow))))
       (messageLocation
        (telega-ins--location (plist-get content :location))
        (telega-ins--location-live msg))
       (messageVenue
        (telega-ins--location (telega--tl-get content :venue :location)))
       (messageAnimation
        (telega-ins (propertize "GIF" 'face 'shadow))
        (telega-ins-prefix " "
          (or (telega-ins--fmt-text
               (unless remove-caption (plist-get content :caption)) msg)
              (telega-ins
               (telega-tl-str (plist-get content :animation) :file_name)))))
       (messageAudio
        (telega-ins telega-symbol-audio " ")
        (or (telega-ins--fmt-text
             (unless remove-caption (plist-get content :caption)) msg)
            (telega-ins (propertize "Audio" 'face 'shadow)))
        (telega-ins-fmt " (%s)"
          (telega-duration-human-readable
           (telega--tl-get content :audio :duration))))
       (messageVideo
        (if-let ((preview-img (telega-msg--preview-video-image msg)))
            (telega-ins--image preview-img)
          (telega-ins (telega-symbol 'video)))
        (telega-ins " ")
        (or (telega-ins--fmt-text
             (unless remove-caption (plist-get content :caption)) msg)
            (telega-ins (propertize "Video" 'face 'shadow)))
        (telega-ins-fmt " (%s)"
          (telega-duration-human-readable
           (telega--tl-get content :video :duration))))
       (messageGame
        (telega-ins (telega-symbol 'game) " ")
        (let ((game (plist-get content :game)))
          (telega-ins (or (telega-tl-str game :title)
                          (telega-tl-str game :short_name)
                          (propertize "Game" 'face 'shadow)))))
       (messageSticker
        (telega-ins (telega-sticker-emoji (plist-get content :sticker)))
        (telega-ins " " (propertize "Sticker" 'face 'shadow)))
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
        (telega-ins (telega-symbol 'invoice) " ")
        (let ((currency (plist-get content :currency)))
          (telega-ins-fmt "%.2f%s " (/ (plist-get content :total_amount) 100.0)
                          (or (cdr (assoc currency telega-currency-symbols-alist))
                              currency)))
        (telega-ins--with-face 'shadow
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
          ;; I18N: polls_votes_count -> {count} votes
          (telega-ins " (" (telega-i18n "lng_polls_votes_count"
                             :count (plist-get poll :total_voter_count))
                      ")")))
       (messageDice
        (telega-ins--dice-msg msg 'one-line))
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
       (t (telega-ins--content msg))))))


;;; Inserters for CONTACTS ewoc buttons
(defun telega-ins--root-contact (user)
  "Inserter for USER, used for contacts ewoc in rootbuf."
  ;; Show `telegram' symbol if user ever has been chatted with
  (when telega-root-show-avatars
    (telega-ins--image (telega-msg-sender-avatar-image-one-line user)))

  (if-let ((user-chat (telega-chat-get (plist-get user :id) 'offline)))
      (telega-ins (telega-symbol 'contact))
    (telega-ins (telega-symbol 'member)))
  (telega-ins " ")

  (telega-ins (telega-user-title user 'name))
  (telega-ins--user-online-status user)

  (telega-ins-prefix " "
    (telega-ins--with-face 'telega-username
      (telega-ins (telega-msg-sender-username user 'with-@))))

  (telega-ins-prefix " +"
    (telega-ins (plist-get user :phone_number)))

  (telega-ins-prefix " "
    (when (telega-msg-sender-blocked-p user 'offline)
      (telega-ins--with-face 'error
        (telega-ins telega-symbol-blocked "BLOCKED"))))
  )

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


(defun telega-ins--chat-msg-one-line (chat msg max-width)
  "Insert message for the chat button usage."
;  (cl-assert (> max-width 11))
  (let* ((trail (telega-ins--as-string
                 (telega-ins--date (plist-get msg :date))))
         (trail-width (string-width trail)))
    (telega-ins--with-attrs (list :align 'left
                                  :max (- max-width trail-width 1)
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

    (telega-ins--move-to-column (- telega-root-fill-column trail-width))
    (telega-ins trail)
    (telega-ins--outgoing-status msg)))

(defun telega-ins--chat-pin-msg-one-line (pin-msg)
  "Inserter for pinned message PIN-MSG."
  (telega-ins (telega-symbol 'pin) " ")
  (telega-ins--chat-msg-one-line
   (telega-msg-chat pin-msg) pin-msg (+ 8 telega-chat-fill-column)))

(defun telega-ins--user-online-status (user)
  "Insert USER's online status."
  (when (and user
             (not (telega-user-bot-p user))
             (not (telega-me-p user))
             (telega-user-online-p user))
    (telega-ins (telega-symbol 'online-status))))

(defun telega-ins--user-emoji-status (user)
  "Inserter for the EMOJI-STATUS."
  (when-let ((emoji-status (plist-get user :emoji_status)))
    (telega-ins--image
     (telega-emoji-status--image emoji-status) nil
     :telega-text telega-symbol-premium)))

(defun telega-ins--chat (chat &optional brackets ignore-width-p)
  "Inserter for CHAT button in root buffer.
BRACKETS is cons cell of open-close brackets to use.
By default BRACKETS is choosen according to `telega-chat-button-brackets'.
If IGNORE-WIDTH-P is specified, then ignore `telega-chat-button-width'
and insert chat as is.
Return t."
  (unless brackets
    (setq brackets (telega-chat-brackets chat)))

  (let ((curr-column (unless ignore-width-p (telega-current-column)))
        (title (telega-chat-title chat))
        (unread (plist-get chat :unread_count))
        (mentions (plist-get chat :unread_mention_count))
        (reactions (plist-get chat :unread_reaction_count))
        (custom-order (telega-chat-uaprop chat :order))
        (muted-p (telega-chat-muted-p chat))
        (chat-type (telega-chat--type chat))
        (chat-info (telega-chat--info chat)))
    (when (telega-chat-secret-p chat)
      (setq title (propertize title 'face 'telega-secret-title)))

    ;; Check for chat folder:
    ;; 1) Show icon / folder name if chat belongs exactly to single
    ;;    folder
    ;; 2) Use `telega-symbol-multiple-folders' as fake folder name if
    ;;    chat belongs to multiple folders
    (when telega-chat-folder-format
      (when-let* ((folders (telega-chat-folders chat))
                  (folders-inc (seq-difference folders
                                               telega-chat-folders-exclude))
                  (folder-name (if (= 1 (length folders-inc))
                                   (car folders-inc)
                                 (cl-assert (> (length folders-inc) 1))
                                 (telega-symbol 'multiple-folders))))
        (setq title (concat (telega-folder-format
                             telega-chat-folder-format
                             folder-name
                             ;; NOTE: fake filter-info in case chat
                             ;; belongs to multiple folders
                             (when (> (length folders-inc) 1)
                               (list :title (telega-symbol 'multiple-folders)
                                     :icon_name "non-existing-icon-name")))
                            title))))

    (when telega-root-show-avatars
      (telega-ins--image (telega-msg-sender-avatar-image-one-line chat)))
    (telega-ins (or (car brackets) "["))

    ;; 1) First we format unread@mentions as string to find out its
    ;;    final length
    ;; 2) Then we insert the title as wide as possible
    ;; 3) Then insert formatted UNREAD@MENTIONS@REACTIONS string
    (let* ((umstring (telega-ins--as-string
                      (unless (zerop unread)
                        (telega-ins--with-face (if muted-p
                                                   'telega-muted-count
                                                 'telega-unmuted-count)
                          (telega-ins (telega-number-human-readable unread))))
                      (unless (zerop mentions)
                        (telega-ins--with-face
                            (if (telega-chat-notification-setting
                                 chat :disable_mention_notifications)
                                '(telega-muted-count bold)
                              'telega-mention-count)
                          (telega-ins-fmt "@%d" mentions)))
                      (unless (zerop reactions)
                        (telega-ins--with-face (if muted-p
                                                   'telega-muted-count
                                                 'telega-unmuted-count)
                          (telega-ins (telega-symbol 'reaction)
;                                      (format "%d" reactions)
                                      )))
                      ;; Mark for chats marked as unread
                      (when (and (zerop unread) (zerop mentions)
                                 (plist-get chat :is_marked_as_unread))
                        (telega-ins--with-face (if muted-p
                                                   'telega-muted-count
                                                 'telega-unmuted-count)
                          (telega-ins (telega-symbol 'unread))))
                      ;; For chats searched by
                      ;; `telega--searchPublicChats' insert number of
                      ;; members in the group
                      ;; Basicgroups converted to supergroups
                      ;; does not have username and have "0" order
                      (when (string= "0" (telega-chat-order chat 'raw))
                        (when (telega-chat-username chat)
                          (telega-ins--with-face 'telega-username
                            (telega-ins "@" (telega-chat-username chat))))
                        (telega-ins--with-face (if muted-p
                                                   'telega-muted-count
                                                 'telega-unmuted-count)
                          (when (memq chat-type '(basicgroup supergroup channel))
                            (telega-ins (telega-symbol 'member)
                                        (telega-number-human-readable
                                         (plist-get chat-info :member_count))))))
                      ))
           (chat-button-width
            (telega-canonicalize-number telega-chat-button-width
                                        telega-root-fill-column))
           (title-width
            (- chat-button-width (string-width umstring)
               ;; NOTE: Do *not* include brackets into
               ;; `chat-button-width' to avoid additional calls to
               ;; `string-width'
               ;; (string-width (or (car brackets) "["))
               ;; (string-width (or (cadr brackets) "]"))
               )))
      (telega-ins--with-attrs (list :max (unless ignore-width-p title-width)
                                    :align 'left
                                    :elide t)
        (telega-ins title)
        (telega-ins--user-online-status (telega-chat-user chat)))
      (if ignore-width-p
          (telega-ins-prefix " "
            (telega-ins umstring))

        (cl-assert (and curr-column title-width))
        (telega-ins--move-to-column (+ curr-column 3 1 title-width))
        (telega-ins umstring)))

    (telega-ins (or (cadr brackets) "]"))
    (when (plist-get (telega-chat-position chat) :is_pinned)
      (telega-ins (telega-symbol 'pin)))
    (when (telega-chat-match-p chat 'has-video-chat)
      (telega-ins (telega-symbol
                   (if (telega--tl-get chat :video_chat :has_participants)
                       'video-chat-active
                     'video-chat-passive))))
    (when (plist-get chat :has_scheduled_messages)
      (telega-ins (telega-symbol 'alarm)))
    (when custom-order
      (telega-ins
       (if (< (string-to-number custom-order)
              (string-to-number (telega-chat-order chat 'raw)))
           (car telega-symbol-custom-order)
         (cdr telega-symbol-custom-order))))
    (when (telega-chat-secret-p chat)
      (telega-ins (telega-symbol 'lock)))
    (when (telega-chat-match-p chat 'has-protected-content)
      (telega-ins (telega-symbol 'copyright)))
    t))

(defun telega-ins--chat-status (chat &optional max-width)
  "Insert CHAT status, limiting it to MAX-WIDTH."
  (let ((actions (gethash (plist-get chat :id) telega--actions))
        (call (telega-voip--by-user-id (plist-get chat :id)))
        (draft-msg (plist-get chat :draft_message))
        (last-msg (plist-get chat :last_message))
        (chat-info (telega-chat--info chat)))
    (cond ((and (telega-chat-secret-p chat)
                (memq (telega--tl-type (plist-get chat-info :state))
                      '(secretChatStatePending secretChatStateClosed)))
           ;; Status of the secret chat
           (telega-ins (propertize
                        (substring (telega--tl-get chat-info :state :@type) 15)
                        'face 'shadow)))

          (call
           (let ((state (plist-get call :state)))
             (telega-ins (telega-symbol (if (plist-get call :is_video)
                                            'video
                                          'phone))
                         " ")
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
                (telega-ins--fmt-text (plist-get inmsg :text))))))

          (last-msg
           (if (telega-msg-match-p last-msg 'ignored)
               (telega-ins--one-lined (telega-ins--message-ignored last-msg))
             (telega-ins--chat-msg-one-line chat last-msg max-width)))

          ((and (telega-chat-secret-p chat)
                (eq (telega--tl-type (plist-get chat-info :state))
                    'secretChatStateReady))
           ;; Status of the secret chat
           (telega-ins (propertize
                        (substring (telega--tl-get chat-info :state :@type) 15)
                        'face 'shadow)))
          )))

(defun telega-ins--chat-full (chat)
  "Full status inserter for CHAT button in root buffer."
  (telega-ins--chat chat)
  (telega-ins "  ")
  (telega-ins--chat-status
   chat (- telega-root-fill-column (current-column)))
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
  (telega-ins--chat-status
   chat (- telega-root-fill-column (current-column)))
  (telega-ins "\n")
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
    (telega-ins--with-face 'shadow
      (telega-ins (telega-distance-human-readable distance) " away")))

  ;; Online status for private chats
  (when-let ((user (telega-chat-user chat)))
    (telega-ins ", ")
    (telega-ins--user-status user))
  (telega-ins "\n")
  t)

(defun telega-ins--chat-last-message (chat)
  "Inserter for last message in CHAT."
  (let ((last-msg (plist-get chat :last_message)))
    (cl-assert last-msg)
    (telega-ins--message-with-chat-header last-msg)))

(defun telega-ins--chat-pinned-message (chat)
  "Inserter for the pinned message in the CHAT."
  (if-let ((pin-msg (telega-chat-pinned-msg chat 'offline)))
      (telega-ins--message-with-chat-header pin-msg)

    (with-telega-chatbuf chat
      (telega-chatbuf--pinned-messages-fetch))
    (telega-ins-i18n "lng_profile_loading")))

(defun telega-ins--root-msg (msg)
  "Inserter for message MSG shown in `telega-root-messages--ewoc'."
  (let ((chat (telega-msg-chat msg))
        (telega-chat-button-width
         (round (* (telega-canonicalize-number telega-chat-button-width
                                               telega-root-fill-column)
                   (/ 2.0 3)))))
    (telega-ins--chat chat)
    (telega-ins "  ")
    (let ((max-width (- telega-root-fill-column (current-column))))
      (telega-ins--chat-msg-one-line chat msg max-width))))

(defun telega-ins--root-msg-call (msg)
  "Inserter for call message MSG in rootbuf."
  (let ((telega-chat-fill-column (- telega-root-fill-column 10 1)))
    (telega-ins--message msg nil (lambda (msg)
                                   (telega-ins--move-to-column
                                    (/ telega-chat-fill-column 2))
                                   (telega-ins " ")
                                   (telega-ins--with-attrs
                                       (list :align 'right :min 10)
                                     (telega-ins--date (plist-get msg :date))))
                         :no-footer)
    ))

(defun telega-ins--chat-action-bar-button (chat kind)
  "Insert CHAT action bar button specified by KIND.
KIND is one of: `spam', `location', `add', `block', `share' and
`remove-bar', `invite'."
  (cl-ecase kind
    (spam
     (telega-ins--button
         (if (memq (telega-chat--type chat)
                   '(basicgroup supergroup channel))
             (telega-i18n "lng_report_spam_and_leave")
           (telega-i18n "lng_report_spam"))
       'action (lambda (_ignore)
                 (telega--reportChat chat "Spam")
                 (telega-chat-delete chat))))
    (location
     (telega-ins--button (telega-i18n "lng_report_location")
       'action (lambda (_ignore)
                 (telega--reportChat chat "UnrelatedLocation"))))
    (add
     (let ((user (telega-chat-user chat)))
       (telega-ins--button (telega-i18n "lng_new_contact_add")
         'action (lambda (_ignore)
                   (telega--addContact (telega-user-as-contact user))))))
    (block
     (let ((user (telega-chat-user chat)))
       (telega-ins--button (telega-i18n "lng_new_contact_block")
         'action (lambda (_ignore)
                   (telega-user-block user 'block)))))
    (share
     (let ((user (telega-chat-user chat)))
       (telega-ins--button (telega-i18n "lng_new_contact_share")
         'action (lambda (_ignore)
                   (telega--sharePhoneNumber user)))))
    (remove-bar
     (telega-ins--button "✕"
       'action (lambda (_ignored)
                 (telega--removeChatActionBar chat)
                 (with-telega-chatbuf chat
                   (goto-char (point-max))))))
    (invite
     (telega-ins--button "Invite Users"
       'action (lambda (_ignored)
                 (let ((new-users (telega-completing-read-user-list
                                      "Invite new users")))
                   (dolist (user new-users)
                     (telega-chat-add-member chat user))))))
    ))

(defun telega-ins--chat-action-bar (chat)
  "Inserter for CHAT's action bar."
  (when-let ((action-bar (plist-get chat :action_bar)))
    (telega-ins--chat-action-bar-button chat 'remove-bar)
    (telega-ins " ActionBar: ")
    (cl-ecase (telega--tl-type action-bar)
      (chatActionBarReportUnrelatedLocation
       (telega-ins--chat-action-bar-button chat 'location))

      (chatActionBarReportSpam
       (telega-ins--chat-action-bar-button chat 'spam))

      (chatActionBarAddContact
       (telega-ins--chat-action-bar-button chat 'add))

      (chatActionBarReportAddBlock
       (telega-ins--chat-action-bar-button chat 'spam)
       (telega-ins " ")
       (telega-ins--chat-action-bar-button chat 'add)
       (telega-ins " ")
       (telega-ins--chat-action-bar-button chat 'block))

      (chatActionBarSharePhoneNumber
       (telega-ins--chat-action-bar-button chat 'share))

      (chatActionBarInviteMembers
       (telega-ins--chat-action-bar-button chat 'invite)))
    t))

(defun telega-ins--chat-my-restrictions (chat)
  "Insert my restrictions (if any) in the CHAT.
Return non-nil if restrictions has been inserted."
  (when-let ((my-status (telega-chat-member-my-status chat)))
    (when (eq (telega--tl-type my-status) 'chatMemberStatusRestricted)
      (let* ((until (plist-get my-status :restricted_until_date))
             (until-date (unless (zerop until)
                           (format-time-string
                            (downcase telega-old-date-format) until)))
             (until-time (unless (zerop until)
                           (format-time-string "%H:%M" until)))
             (perms (plist-get my-status :permissions)))
        (cond ((not (plist-get perms :can_send_messages))
               (if (and until-date until-time)
                   (telega-ins-i18n "lng_restricted_send_message_until"
                     :date until-date :time until-time)
                 (telega-ins-i18n "lng_restricted_send_message")))
              ((not (plist-get perms :can_send_media_messages))
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
  (when-let ((schat (telega-chat-get
                     (plist-get sponsored-msg :sponsor_chat_id) t)))
    (let ((telega-chat-button-width
           (round (* (telega-canonicalize-number
                      telega-chat-button-width telega-chat-fill-column)
                     2))))
      (telega-ins--chat schat))
    (telega-ins "\n"))
  (telega-ins--column 2 telega-chat-fill-column
    (telega-ins--content sponsored-msg))
  (telega-ins "\n"))

(defun telega-ins--chat-sponsored-message (chat sponsored-msg)
  "For the CHAT insert sponsored message."
  (telega-ins--message
   (telega-msg-create-internal
    chat (telega-fmt-text
          (telega-i18n (if (plist-get sponsored-msg :is_recommended)
                           "lng_recommended"
                         "lng_sponsored")))))
  (telega-ins "\n")
  (telega-ins--with-face 'telega-msg-sponsored
    (telega-button--insert 'telega sponsored-msg
      :inserter #'telega-ins--sponsored-message
      :action #'telega-msg-open-sponsored))
  t)

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
                             'telega-button-active
                           'shadow)
    (telega-ins-fmt "%d" (plist-get msg-reaction :total_count))
    (telega-ins--msg-reaction-type (plist-get msg-reaction :type))
    (seq-doseq (rs (plist-get msg-reaction :recent_sender_ids))
      (telega-ins--image
       (telega-msg-sender-avatar-image-one-line (telega-msg-sender rs))))
    t))

(defun telega-ins--msg-reaction-list (msg)
  "Inserter for the message's MSG reactions."
  (let (ret)
    (seq-doseq (msg-reaction (telega--tl-get msg :interaction_info :reactions))
      (when ret
        (telega-ins "  "))
      (telega-ins--raw-button
          (list 'action (lambda (_button)
                          (let ((rtype (plist-get msg-reaction :type))
                                (big-p current-prefix-arg))
                            (if (plist-get msg-reaction :is_chosen)
                                (telega--removeMessageReaction msg rtype)
                              (telega--addMessageReaction msg rtype big-p)))))
        (telega-ins--msg-reaction msg-reaction))
      (setq ret t))
    ret))

(defun telega-ins--available-reaction-list (av-reactions custom-action &optional column)
  "Insert available reactions.
AV-REACTIONS - list of `availableReaction' TDLib objects.
When some reaction is chosen, CUSTOM-ACTION is called with the single
argument of `ReactionType' type."
  (seq-doseq (av-reaction av-reactions)
    (telega-ins-prefix (unless (bolp) "\n")
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
              (list (telega-sticker--gen-sensor-func sticker))))))

      (> (current-column) (or column (current-fill-column))))))

(provide 'telega-ins)

;;; telega-ins.el ends here

;;; telega-msg.el --- Messages for telega  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2019 by Zajcev Evgeny.

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
(require 'format-spec)

(require 'telega-core)
(require 'telega-tdlib)
(require 'telega-customize)
(require 'telega-media)
(require 'telega-ffplay)                ; telega-ffplay-run
(require 'telega-vvnote)
(require 'telega-util)
(require 'telega-tme)

(declare-function telega-chat-get "telega-chat" (chat-id &optional offline-p))
(declare-function telega-chatbuf--msg "telega-chat" (msg-id &optional with-node))
(declare-function telega-chat--goto-msg "telega-chat" (chat msg-id &optional highlight))
(declare-function telega-msg-redisplay "telega-chat" (msg &optional node))
(declare-function telega-msg-activate-voice-note "telega-chat" (msg &optional for-chat))
(declare-function telega-chatbuf--next-msg "telega-chat" (msg predicate &optional backward))
(declare-function telega-chat-title "telega-chat" (chat &optional with-username))
(declare-function telega-chatbuf--node-by-msg-id "telega-chat" (msg-id))
(declare-function telega-chatbuf-mode-line-update "telega-chat" ())
(declare-function telega-chat-public-p "telega-chat" (chat &optional chat-type))
(declare-function telega-chat--type "telega-chat" (chat &optional no-interpret))
(declare-function telega-chatevent-log-filter "telega-chat" (&rest filters))
(declare-function telega-chat--pop-to-buffer "telega-chat" (chat))

(declare-function telega-browse-url "telega-webpage" (url &optional in-web-browser))


(defvar telega-msg-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map [remap self-insert-command] 'ignore)
    (define-key map (kbd "n") 'telega-button-forward)
    (define-key map (kbd "p") 'telega-button-backward)

    ;; Marking, `telega-msg-forward' and `telega-msg-delete' can work
    ;; on list of marked messages
    (define-key map (kbd "m") 'telega-msg-mark-toggle)
    (define-key map (kbd "U") 'telega-chatbuf-unmark-all)

    (define-key map (kbd "i") 'telega-describe-message)
    (define-key map (kbd "r") 'telega-msg-reply)
    (define-key map (kbd "e") 'telega-msg-edit)
    (define-key map (kbd "f") 'telega-msg-forward-marked-or-at-point)
    (define-key map (kbd "d") 'telega-msg-delete-marked-or-at-point)
    (define-key map (kbd "k") 'telega-msg-delete-marked-or-at-point)
    (define-key map (kbd "l") 'telega-msg-redisplay)
    (define-key map (kbd "=") 'telega-msg-diff-edits)
    (define-key map (kbd "R") 'telega-msg-resend)
    (define-key map (kbd "S") 'telega-msg-save)

    (define-key map (kbd "DEL") 'telega-msg-delete-marked-or-at-point)
    map))

(define-button-type 'telega-msg
  :supertype 'telega
  :inserter telega-inserter-for-msg-button
  'read-only t
  'keymap telega-msg-button-map
  'action 'telega-msg-button--action)

(defun telega-msg-button--action (button)
  "Action to take when chat BUTTON is pressed."
  (let ((msg (telega-msg-at button))
        ;; If custom `:action' is used for the button, then use it,
        ;; otherwise open content
        (custom-action (button-get button :action)))
    (cl-assert msg)
    (if custom-action
        (funcall custom-action msg)
      (telega-msg-open-content msg))))

(defun telega-msg--pp (msg)
  "Pretty printer for MSG button."
  (let* ((chat (telega-msg-chat msg))
         (msg-inserter
          (cond ((and (telega-chat-match-p
                       chat telega-chat-show-deleted-messages-for)
                      (plist-get msg :telega-is-deleted-message))
                 'telega-ins--message-deleted)

                ((telega-msg-ignored-p msg)
                 'telega-ins--message-ignored)

                ;; NOTE: check for messages grouping by sender
                ((and (telega-chat-match-p chat telega-chat-group-messages-for)
                      (> (point) 3)
                      (let ((prev-msg (telega-msg-at (- (point) 2))))
                        (and prev-msg
                             (not (telega-msg-special-p prev-msg))
                             (eq (plist-get msg :sender_user_id)
                                 (plist-get prev-msg :sender_user_id))
                             (< (- (plist-get msg :date)
                                   (plist-get prev-msg :date))
                                telega-chat-group-messages-timespan))))
                 'telega-ins--message-no-header)

                (t 'telega-ins--message))))
    (telega-button--insert 'telega-msg msg
      :inserter msg-inserter)
    (telega-ins "\n")))

(defun telega-msg-root--pp (msg)
  "Pretty printer for MSG button shown in root buffer."
  (when (telega-filter-chats (list (telega-msg-chat msg))) ; visible-p
    (telega-button--insert 'telega-msg msg
      :inserter 'telega-ins--root-msg
      :action 'telega-msg-goto-highlight)
    (telega-ins "\n")))

(defun telega-msg--get (chat-id msg-id &optional locally-p callback)
  "Get message by CHAT-ID and MSG-ID pair.
If LOCALLY-P is non-nil, then do not perform request to telega-server.
If CALLBACK is specified and message is not available at the
moment, then fetch message asynchronously and call the CALLBACK
function with one argument - message."
  (declare (indent 3))
  (let ((cached-msg (with-telega-chatbuf (telega-chat-get chat-id)
                      (telega-chatbuf--msg msg-id))))
    (if (or locally-p cached-msg)
        cached-msg
      (telega--getMessage chat-id msg-id callback))))

(defun telega-msg-at (&optional pos)
  "Return current message at point."
  (let ((button (button-at (or pos (point)))))
    (when (and button (eq (button-type button) 'telega-msg))
      (button-get button :value))))

(defun telega-msg-type-p (msg-type msg)
  "Return non-nil if MSG is of MSG-TYPE."
  (eq (telega--tl-type (plist-get msg :content)) msg-type))

(defun telega-msg-chat (msg &optional offline-p)
  "Return chat for the MSG.
Return nil for deleted messages."
  (telega-chat-get (plist-get msg :chat_id) offline-p))

(defun telega-msg-reply-msg (msg &optional locally-p callback)
  "Return message MSG replying to.
If LOCALLY-P is non-nil, then do not perform any requests to telega-server.
If CALLBACK is specified, then get reply message asynchronously."
  (declare (indent 2))
  (let ((reply-to-msg-id (plist-get msg :reply_to_message_id)))
    (unless (zerop reply-to-msg-id)
      (telega-msg--get (plist-get msg :chat_id) reply-to-msg-id locally-p
        callback))))

(defsubst telega-msg-goto (msg &optional highlight)
  "Goto message MSG."
  (telega-chat--goto-msg
   (telega-msg-chat msg) (plist-get msg :id) highlight))

(defsubst telega-msg-goto-highlight (msg)
  "Goto message MSG and hightlight it."
  (telega-msg-goto msg 'hightlight))

(defun telega--openMessageContent (msg)
  "Open content of the message MSG."
  (telega-server--send
   (list :@type "openMessageContent"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id))))

(defun telega-msg-open-sticker (msg)
  "Open content for sticker message MSG."
  (let ((sset-id (telega--tl-get msg :content :sticker :set_id)))
    (if (string= "0" sset-id)
        (message "Sticker has no associated stickerset")

      (if-let ((sset (telega-stickerset-get sset-id 'locally)))
          (telega-describe-stickerset sset (telega-msg-chat msg))

        (with-telega-help-win "*Telegram Sticker Set*"
          (telega-ins "Loading stickerset..."))
        (telega-stickerset-get sset-id nil
          (lambda (stickerset)
            (telega-describe-stickerset
             stickerset (telega-msg-chat msg))))))))

;; TODO: revise the code, too much similar stuff
(defun telega-msg-open-video (msg &optional video)
  "Open content for video message MSG."
  (let* ((video (or video (telega--tl-get msg :content :video)))
         (video-file (telega-file--renew video :video)))
    ;; NOTE: `telega-file--download' triggers callback in case file is
    ;; already downloaded
    (telega-file--download video-file 32
      (lambda (file)
        (telega-msg-redisplay msg)
        (when (telega-file--downloaded-p file)
          (apply 'telega-ffplay-run
                 (telega--tl-get file :local :path) nil
                 telega-video-ffplay-args))))))

(defun telega-msg-open-audio (msg)
  "Open content for audio message MSG."
  ;; - If already playing, then pause
  ;; - If paused, start from paused position
  ;; - If not start, start playing
  (let* ((audio (telega--tl-get msg :content :audio))
         (audio-file (telega-file--renew audio :audio))
         (proc (plist-get msg :telega-ffplay-proc)))
    (cl-case (and (process-live-p proc) (process-status proc))
      (run (telega-ffplay-pause proc))
      (stop (telega-ffplay-resume proc))
      (t (telega-file--download audio-file 32
          (lambda (file)
            (telega-msg-redisplay msg)
            (when (telega-file--downloaded-p file)
              (plist-put msg :telega-ffplay-proc
                         (telega-ffplay-run
                          (telega--tl-get file :local :path)
                          (lambda (_proc)
                            (telega-msg-redisplay msg))
                          "-nodisp")))))))))

(defun telega-msg-voice-note--ffplay-callback (msg)
  "Return callback to be used in `telega-ffplay-run'."
  (lambda (proc)
    (telega-msg-redisplay msg)

    (unless (process-live-p proc)
      (telega-msg-activate-voice-note nil (telega-msg-chat msg)))

    (when (and (eq (process-status proc) 'exit)
               telega-vvnote-voice-play-next)
      ;; NOTE: callback might be called *twice* with 'exit status,
      ;; this might cause problems if not handled, to handle this, we
      ;; simple unset the callback on proc
      (set-process-plist proc nil)

      ;; ffplay exited normally (finished playing), try to play next
      ;; voice message if any
      (when-let ((next-voice-msg
                  (telega-chatbuf--next-msg
                   msg (apply-partially #'telega-msg-type-p 'messageVoiceNote))))
        (telega-msg-open-content next-voice-msg)))))

(defun telega-msg-open-voice-note (msg)
  "Open content for voiceNote message MSG."
  ;; - If already playing, then pause
  ;; - If paused, start from paused position
  ;; - If not start, start playing
  (let* ((note (telega--tl-get msg :content :voice_note))
         (note-file (telega-file--renew note :voice))
         (proc (plist-get msg :telega-ffplay-proc)))
    (cl-case (and (process-live-p proc) (process-status proc))
      (run (telega-ffplay-pause proc))
      (stop (telega-ffplay-resume proc))
      (t (telega-file--download note-file 32
           (lambda (file)
             (telega-msg-redisplay msg)
             (when (telega-file--downloaded-p file)
               (plist-put msg :telega-ffplay-proc
                          (telega-ffplay-run
                           (telega--tl-get file :local :path)
                           (telega-msg-voice-note--ffplay-callback msg)
                           "-nodisp"))
               (telega-msg-activate-voice-note msg))))))))

(defun telega-msg-video-note--callback (proc filename msg)
  "Callback for video note playback."
  (let* (;(proc (plist-get msg :telega-ffplay-proc))
         (proc-plist (process-plist proc))
         (nframes (or (float (plist-get proc-plist :nframes))
                      (* 30.0 (telega--tl-get
                               msg :content :video_note :duration))))
         (frame-num (plist-get proc-plist :frame-num))
         (progress (/ frame-num nframes)))
    (plist-put msg :telega-ffplay-frame
               (when filename
                 (telega-vvnote-video--svg filename progress)))
    (telega-msg-redisplay msg)))

(defun telega-msg-open-video-note (msg)
  "Open content for videoNote message MSG.
If called with `\\[universal-argument]' prefix, then open with
external player even if `telega-video-note-play-inline' is
non-nil."
  (let* ((note (telega--tl-get msg :content :video_note))
         (note-file (telega-file--renew note :video))
         (proc (plist-get msg :telega-ffplay-proc)))
    (cl-case (and (process-live-p proc) (process-status proc))
      (run (telega-ffplay-pause proc))
      (stop (telega-ffplay-resume proc))
      (t (telega-file--download note-file 32
           (lambda (file)
             (telega-msg-redisplay msg)
             (when (telega-file--downloaded-p file)
               (let ((filepath (telega--tl-get file :local :path)))
                 (if (and telega-video-note-play-inline
                          (not current-prefix-arg))
                     (plist-put msg :telega-ffplay-proc
                                (telega-ffplay-to-png filepath
                                    (list "-vf" "scale=120:120"
                                          "-f" "alsa" "default" "-vsync" "0")
                                  'telega-msg-video-note--callback msg))
                   (telega-ffplay-run filepath nil))))))))))

(defun telega-msg-open-photo (msg &optional photo)
  "Open content for photo message MSG."
  (telega-photo--open (or photo (telega--tl-get msg :content :photo)) msg))

(defun telega-animation--ffplay-callback (_proc filename anim)
  "Callback for inline animation playback."
  (plist-put anim :telega-ffplay-frame-filename filename)
  ;; NOTE: just redisplay the image, not redisplaying full message
  (telega-media--image-update
   (cons anim 'telega-animation--create-image) nil)
  (force-window-update)
  )

(defun telega-msg-open-animation (msg &optional animation)
  "Open content for animation message MSG.
If called with `\\[universal-argument]' prefix, then open with
external player even if `telega-animation-play-inline' is
non-nil."
  (let* ((anim (or animation (telega--tl-get msg :content :animation)))
         (anim-file (telega-file--renew anim :animation))
         (proc (plist-get msg :telega-ffplay-proc)))
    (cl-case (and (process-live-p proc) (process-status proc))
      (run (telega-ffplay-pause proc))
      (stop (telega-ffplay-resume proc))
      (t (telega-file--download anim-file 32
           (lambda (file)
             (telega-msg-redisplay msg)
             (when (telega-file--downloaded-p file)
               (let ((filename (telega--tl-get file :local :path)))
                 (if (and telega-animation-play-inline
                          (not current-prefix-arg))
                     (plist-put msg :telega-ffplay-proc
                                (telega-ffplay-to-png filename nil
                                  'telega-animation--ffplay-callback anim))
                   (telega-ffplay-run filename nil "-loop" "0"))))))))))

(defun telega-msg-open-document (msg &optional document)
  "Open content for document message MSG."
  (let* ((doc (or document (telega--tl-get msg :content :document)))
         (doc-file (telega-file--renew doc :document)))
    (telega-file--download doc-file 32
      (lambda (file)
        (telega-msg-redisplay msg)
        (when (telega-file--downloaded-p file)
          (find-file (telega--tl-get file :local :path)))))))

(defun telega-msg-open-location (msg)
  "Open content for location message MSG."
  (let* ((loc (telega--tl-get msg :content :location))
         (lat (plist-get loc :latitude))
         (lon (plist-get loc :longitude))
         (url (format-spec telega-location-url-format
                           (format-spec-make ?N lat ?E lon))))
    (telega-browse-url url 'in-web-browser)))

(defun telega-msg-open-contact (msg)
  "Open content for contact message MSG."
  (telega-describe-contact
   (telega--tl-get msg :content :contact)))

(defun telega-msg-open-webpage (msg &optional web-page)
  "Open content for message with webpage message MSG."
  (unless web-page
    (setq web-page (telega--tl-get msg :content :web_page)))

  ;; NOTE: "document" webpage might contain :video instead of :document
  ;; see https://t.me/c/1347510619/43
  (cond ((plist-get web-page :video)
         (telega-msg-open-video msg (plist-get web-page :video)))
        ((plist-get web-page :animation)
         (telega-msg-open-animation msg (plist-get web-page :animation)))
        ((plist-get web-page :document)
         (telega-msg-open-document msg (plist-get web-page :document)))
        ((and (string= "photo" (plist-get web-page :type))
              (plist-get web-page :photo))
         (telega-msg-open-photo msg (plist-get web-page :photo)))
        (t (when-let ((url (plist-get web-page :url)))
             (telega-browse-url url)))))

(defun telega-msg-open-game (msg)
  "Open content for the game message MSG."
  (telega--getCallbackQueryAnswer
   msg (list :@type "callbackQueryPayloadGame"
             :game_short_name (telega--tl-get msg :content :game :short_name))))

(defun telega-msg-open-content (msg)
  "Open message MSG content."
  (telega--openMessageContent msg)

  (cl-case (telega--tl-type (plist-get msg :content))
    (messageDocument
     (telega-msg-open-document msg))
    (messageSticker
     (telega-msg-open-sticker msg))
    (messageVideo
     (telega-msg-open-video msg))
    (messageAudio
     (telega-msg-open-audio msg))
    (messageAnimation
     (telega-msg-open-animation msg))
    (messageVoiceNote
     (telega-msg-open-voice-note msg))
    (messageVideoNote
     (telega-msg-open-video-note msg))
    ((messagePhoto messageChatChangePhoto)
     (telega-msg-open-photo msg))
    (messageLocation
     (telega-msg-open-location msg))
    (messageContact
     (telega-msg-open-contact msg))
    (messageText
     (when-let ((web-page (telega--tl-get msg :content :web_page)))
       (telega-msg-open-webpage msg web-page)))
    (messagePoll
     ;; no-op
     )
    (messageGame
     (telega-msg-open-game msg))

    (messageChatUpgradeTo
     (let* ((sg-id (telega--tl-get msg :content :supergroup_id))
            (sg-chat (telega--createSupergroupChat sg-id 'force)))
       (telega-chat--pop-to-buffer sg-chat)))
    (messageChatUpgradeFrom
     ;; Open corresponding basicgroup chat
     (let* ((bg-id (telega--tl-get msg :content :basic_group_id))
            (bg-chat (telega--createBasicGroupChat bg-id 'force)))
       (telega-chat--pop-to-buffer bg-chat)))

    (t (message "TODO: `open-content' for <%S>"
                (telega--tl-type (plist-get msg :content))))))

(defun telega-msg--track-file-uploading-progress (msg)
  "Track uploading progress for the file associated with MSG."
  (let ((msg-file (telega-file--used-in-msg msg)))
    (when (and msg-file (telega-file--uploading-p msg-file))
      (telega-file--upload-internal msg-file
        (lambda (_filenotused)
          (telega-msg-redisplay msg))))))

(defun telega-msg-chat-title (msg)
  "Title of the message's chat."
  (telega-chat-title (telega-msg-chat msg) 'with-username))

(defsubst telega-msg-sender (msg)
  "Return sender (if any) for message MSG."
  (let ((sender-uid (plist-get msg :sender_user_id)))
    (unless (zerop sender-uid)
      (telega-user--get sender-uid))))

(defsubst telega-msg-by-me-p (msg)
  "Return non-nil if sender of MSG is me."
  (= (plist-get msg :sender_user_id) telega--me-id))

(defsubst telega-msg-seen-p (msg &optional chat)
  "Return non-nil if MSG has been already read in CHAT."
  (unless chat (setq chat (telega-msg-chat msg)))
  (<= (plist-get msg :id) (plist-get chat :last_read_inbox_message_id)))

(defsubst telega-msg-marked-p (msg)
  "Return non-nil if message MSG is marked."
  (with-telega-chatbuf (telega-msg-chat msg)
    (memq msg telega-chatbuf--marked-messages)))

(defun telega-msg-observable-p (msg &optional chat node)
  "Return non-nil if MSG is observable in chatbuffer.
CHAT - chat to search message for.
NODE - ewoc node, if known."
  (unless chat (setq chat (telega-msg-chat msg)))
  (with-telega-chatbuf chat
    (unless node
      (setq node (telega-chatbuf--node-by-msg-id (plist-get msg :id))))
    (when node
      (telega-button--observable-p (ewoc-location node)))))

(defun telega-msg-contains-unread-mention-p (msg)
  "Return non-nil if MSG has unread mention."
  (plist-get msg :contains_unread_mention))

;; DEPRECATED ???
(defun telega-msg-sender-admin-status (msg)
  (let ((admins-tl (telega-server--call
                    (list :@type "getChatAdministrators"
                          :chat_id (plist-get msg :chat_id)))))
    (when (cl-find (plist-get msg :sender_user_id)
                   (plist-get admins-tl :user_ids)
                   :test #'=)
      " (admin)")))

(defun telega--parseTextEntities (text parse-mode)
  "Parse TEXT using PARSE-MODE.
PARSE-MODE is one of:
  (list :@type \"textParseModeMarkdown\" :version 0|1|2)
or
  (list :@type \"textParseModeHTML\")"
  (let ((fmt-text (telega-server--call
                   (list :@type "parseTextEntities"
                         :text text
                         :parse_mode parse-mode))))
    (plist-put fmt-text :text (or (telega-tl-str fmt-text :text 'no-props) ""))))

(defun telega--formattedText (text &optional markdown-version)
  "Convert TEXT to `formattedTex' type.
If MARKDOWN is non-nil then format TEXT as markdown."
  (if markdown-version
      ;; For markdown mode, escape underscores in urls
      ;; See https://github.com/tdlib/td/issues/672
      ;; See https://github.com/zevlg/telega.el/issues/143
      (telega--parseTextEntities
       (telega-escape-underscores text)
       (list :@type "textParseModeMarkdown"
             :version markdown-version))

    (list :@type "formattedText"
          :text (substring-no-properties text) :entities [])))

(defun telega--stopPoll (msg)
  "Stops a poll."
  (telega-server--send
   (list :@type "stopPoll"
         :chat_id (plist-get (telega-msg-chat msg) :id)
         :message_id (plist-get msg :id))))

(defun telega--setPollAnswer (msg &rest option-ids)
  "Changes user answer to a poll.
OPTION-IDS - 0-based identifiers of option, chosen by the user.
If OPTION-IDS is not specified, then retract the voice."
  (telega-server--send
   (list :@type "setPollAnswer"
         :chat_id (plist-get (telega-msg-chat msg) :id)
         :message_id (plist-get msg :id)
         :option_ids (apply 'vector option-ids))))

;;; Ignoring messages
(defun telega-msg-ignored-p (msg)
  "Return non-nil if MSG is ignored."
  (or (plist-get msg :ignored-p)
      ;; Search message with same ID in ignored ring
      (catch 'found
        (dotimes (ind (ring-length telega--ignored-messages-ring))
          (when (eq (plist-get msg :id)
                    (plist-get (ring-ref telega--ignored-messages-ring ind) :id))
            (throw 'found ind))))))

(defun telega-msg-ignore (msg)
  "Mark message MSG to be ignored (not viewed, notified about) in chats.
By side effect adds MSG into `telega--ignored-messages-ring' to be viewed
with `M-x telega-ignored-messages RET'."
  (plist-put msg :ignored-p t)
  (ring-insert telega--ignored-messages-ring msg)
  (telega-debug "IGNORED msg: %S" msg))

(defun telega-msg-ignore-blocked-sender (msg &rest _ignore)
  "Function to be used as `telega-chat-pre-message-hook'.
Add it to `telega-chat-pre-message-hook' to ignore messages from
blocked users."
  (let ((sender-uid (plist-get msg :sender_user_id)))
    (when (and (not (zerop sender-uid))
               (plist-get
                (telega--full-info (telega-user--get sender-uid))
                :is_blocked))
      (telega-msg-ignore msg))))


(defun telega-msg-unmark (msg)
  "Unmark message MSG."
  (with-telega-chatbuf (telega-msg-chat msg)
    (when (telega-msg-marked-p msg)
      (setq telega-chatbuf--marked-messages
            (delq msg telega-chatbuf--marked-messages))
    (telega-chatbuf-mode-line-update)
    (telega-msg-redisplay msg))))

(defun telega-msg-mark-toggle (msg)
  "Toggle mark of the message MSG."
  (interactive (list (telega-msg-at (point))))
  (with-telega-chatbuf (telega-msg-chat msg)
    (if (memq msg telega-chatbuf--marked-messages)
        (setq telega-chatbuf--marked-messages
              (delq msg telega-chatbuf--marked-messages))
      (setq telega-chatbuf--marked-messages
            (push msg telega-chatbuf--marked-messages)))
    (telega-chatbuf-mode-line-update)

    (telega-msg-redisplay msg)
    (telega-button-forward 1 'telega-msg-at)))

(defun telega-msg-pin (msg)
  "Pin message MSG."
  (interactive (list (telega-msg-at (point))))
  (telega--pinChatMessage msg))

(defun telega-msg-save (msg)
  "Save messages's MSG media content to a file."
  (interactive (list (telega-msg-at (point))))
  (let* ((content (plist-get msg :content))
         (file (cl-case (telega--tl-type content)
                 (messageDocument
                  (let ((doc (telega--tl-get msg :content :document)))
                    (telega-file--renew doc :document)))
                 (messagePhoto
                  (let ((hr (telega-photo--highres (plist-get content :photo))))
                    (telega-file--renew hr :photo)))
                 (messageAudio
                  (let ((audio (telega--tl-get msg :content :audio)))
                    (telega-file--renew audio :audio)))
                 (messageVideo
                  (let ((video (telega--tl-get msg :content :video)))
                    (telega-file--renew video :video)))
                 (messageVoiceNote
                  (let ((note (telega--tl-get msg :content :voice_note)))
                    (telega-file--renew note :voice)))
                 (messageVideoNote
                  (let ((note (telega--tl-get msg :content :video_note)))
                    (telega-file--renew note :video)))
                 (messageAnimation
                  (let ((anim (telega--tl-get msg :content :animation)))
                    (telega-file--renew anim :animation)))
                 (t (let ((web-page (plist-get content :web_page)))
                      (if web-page
                          (error "TODO: Save web-page")
                        (user-error "No file associated with message")))))))
    (cl-assert file)
    (telega-file--download file 32
      (lambda (dfile)
        (telega-msg-redisplay msg)
        (when (telega-file--downloaded-p dfile)
          ;; TODO: This might be executed in process filter, so
          ;; pressing C-g will trigger "error in process filter: Quit"
          ;; Need to execute this outside of process filter
          (let* ((fpath (telega--tl-get dfile :local :path))
                 (fname (file-name-nondirectory fpath))
                 (new-fpath (read-file-name "Save to file: " default-directory
                                            nil nil fname nil)))
            ;; See https://github.com/tdlib/td/issues/379
            (copy-file fpath new-fpath)
            (message (format "Wrote %s" new-fpath))))))
    ))

(defun telega-describe-message (msg)
  "Show info about message at point."
  (interactive (list (telega-msg-at (point))))
  (with-telega-help-win "*Telegram Message Info*"
    (let ((chat-id (plist-get msg :chat_id))
          (msg-id (plist-get msg :id)))
      (telega-ins "Date(ISO8601): ")
      (telega-ins--date-iso8601 (plist-get msg :date) "\n")
      (telega-ins-fmt "Chat-id: %d\n" chat-id)
      (telega-ins-fmt "Message-id: %d\n" msg-id)
      (let ((sender-uid (plist-get msg :sender_user_id)))
        (unless (zerop sender-uid)
          (telega-ins "Sender: ")
          (insert-text-button (telega-user--name (telega-user--get sender-uid))
                              :telega-link (cons 'user sender-uid))
          (telega-ins "\n")))
      ;; Link to the message
      (let* ((chat (telega-chat-get chat-id))
             (link (cond ((telega-chat-public-p chat 'supergroup)
                          (telega--getPublicMessageLink chat-id msg-id))
                         ((eq (telega-chat--type chat 'no-interpret) 'supergroup)
                          ;; Only for supergroups and channels
                          (telega--getMessageLink chat-id msg-id)))))
        (when link
          (telega-ins "Link: ")
          (telega-ins--raw-button (telega-link-props 'url link)
            (telega-ins link))
          (telega-ins "\n")))

      (telega-ins "Internal Link: ")
      (let ((internal-link (telega-tme-internal-link-to msg)))
        (apply 'insert-text-button internal-link
               (telega-link-props 'url internal-link 'link)))
      (telega-ins "\n")

      (when telega-debug
        (telega-ins-fmt "MsgSexp: (telega-msg--get %d %d)\n" chat-id msg-id))

      (when telega-debug
        (let ((print-length nil))
          (telega-ins-fmt "\nMessage: %S\n" msg)))
      )))

(defun telega-ignored-messages ()
  "Display all messages that has been ignored."
  (interactive)
  (with-help-window " *Telegram Ignored Messages*"
    (set-buffer standard-output)
    (dolist (msg (ring-elements telega--ignored-messages-ring))
      (telega-button--insert 'telega-msg msg
        :inserter 'telega-ins--message-ignored-list))))


;; Viewing messages diffs
(defun telega-msg-diff-edits (msg)
  "Display edits to MSG user did."
  (interactive (list (telega-msg-at (point))))

  (when (zerop (plist-get msg :edit_date))
    (user-error "Message was not edited"))

  (cl-flet ((find-msg (accesor events)
                      (telega--tl-get
                       (cl-find (plist-get msg :id) events
                                :key (telega--tl-prop :action accesor :id))
                       :action accesor)))
    (let* ((events (telega--getChatEventLog
                    (telega-msg-chat msg) nil nil 50
                    (telega-chatevent-log-filter :message_edits)
                    (list (telega-msg-sender msg))))
           (msg-new (find-msg :new_message events))
           (msg-old (find-msg :old_message (nreverse events))))
      (unless (and msg-old msg-new)
        (user-error "Can't find message edit in last 50 edits"))

      (with-telega-help-win "*Telega Message Diff*"
        (telega-ins--with-face (ansi-color-get-face-1 31)
          (telega-ins "Orig"))
        (telega-ins " message at: ")
        (telega-ins--date-iso8601 (plist-get msg-old :date))
        (telega-ins "\n")

        (telega-ins--with-face (ansi-color-get-face-1 32)
          (telega-ins "Edit"))
        (telega-ins " message at: ")
        (telega-ins--date-iso8601 (plist-get msg-new :edit_date))
        (telega-ins "\n")

        (telega-ins "-- Diff --\n")
        (telega-ins
         (telega-diff-wordwise (telega-ins--as-string
                                (telega-ins--content msg-old))
                               (telega-ins--as-string
                                (telega-ins--content msg-new))
                               'colorize))
        ))))

(provide 'telega-msg)

;;; telega-msg.el ends here

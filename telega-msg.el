;; telega-msg.el --- Messages for telega  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2024 by Zajcev Evgeny.

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
(require 'easymenu)

(require 'telega-core)
(require 'telega-tdlib)
(require 'telega-customize)
(require 'telega-i18n)
(require 'telega-media)
(require 'telega-ffplay)                ; telega-ffplay-run
(require 'telega-vvnote)
(require 'telega-util)
(require 'telega-tme)
(require 'telega-webpage)
(require 'telega-story)

(declare-function telega-root-view--update "telega-root" (on-update-prop &rest args))
(declare-function telega-chat-get "telega-chat" (chat-id &optional offline-p))
(declare-function telega-chat--goto-msg "telega-chat" (chat msg-id &optional highlight callback))
(declare-function telega-msg-redisplay "telega-chat" (msg &optional node))
(declare-function telega-chatbuf--manage-point "telega-chat" (&optional point only-prompt-p))
(declare-function telega-chatbuf--next-msg "telega-chat" (msg msg-temex &optional backward))
(declare-function telega-chatbuf--activate-vvnote-msg "telega-chat" (msg))
(declare-function telega-chat-title "telega-chat" (chat &optional no-badges))
(declare-function telega-chatbuf--node-by-msg-id "telega-chat" (msg-id))
(declare-function telega-chatbuf--chat-update "telega-chat" (&rest dirtiness))
(declare-function telega-chat--type "telega-chat" (chat))
(declare-function telega-chatevent-log-filter "telega-chat" (&rest filters))
(declare-function telega-chat--pop-to-buffer "telega-chat" (chat))

(declare-function telega--full-info "telega-info" (tlobj &optional _callback))


;; Menu for right-mouse on message
(easy-menu-define telega-msg-button-menu nil
  "Menu for the message."
  '("Telega Message"
    ["describe" telega-describe-message
     :key (kbd "i")
     :label (telega-i18n "lng_info_about_label")]
    "---"
    ["reply" telega-msg-reply
     :label (telega-i18n "lng_context_reply_msg")]
    ["reply-in-another-chat" telega-msg-reply-in-another-chat
      :label (telega-i18n "lng_reply_in_another_chat")
      :visible (telega-msg-match-p (telega-msg-for-interactive)
                 '(message-property :can_be_replied_in_another_chat))]
    ["edit" telega-msg-edit
     :label (telega-i18n "lng_context_edit_msg")
     :visible (telega-msg-match-p (telega-msg-for-interactive)
                '(and (not (type Poll))
                      (message-property :can_be_edited)))]
    ["stop-poll" telega-msg-stop-poll
     :label (if (eq 'pollTypeQuiz
                    (telega--tl-type
                     (plist-get (telega-msg-match-p (telega-msg-for-interactive)
                                  '(type Poll))
                                :poll)))
                "Stop Quiz"
              (telega-i18n "lng_polls_stop"))
     :visible (telega-msg-match-p (telega-msg-for-interactive)
                '(and (type Poll)
                      (message-property :can_be_edited)))]
    ["forward" telega-msg-forward-dwim
     :label (telega-i18n "lng_context_forward_msg")
     :enable (telega-msg-match-p (telega-msg-for-interactive)
               '(message-property :can_be_forwarded))]
    ["add-tag" telega-msg-add-reaction
     :label (telega-i18n "lng_add_tag_button")
     :visible (telega-msg-match-p (telega-msg-for-interactive)
                '(chat saved-messages))]
    ["translate" telega-msg-translate
     :label (telega-i18n "lng_context_translate")
     :visible (telega-msg-content-text (telega-msg-for-interactive))]
    "---"
    ["pin" telega-msg-pin-toggle
     :label (telega-i18n "lng_context_pin_msg")
     :style toggle
     :selected (telega-msg-match-p (telega-msg-for-interactive)
                 '(prop :is_pinned))
     :visible (telega-msg-match-p (telega-msg-for-interactive)
                '(message-property :can_be_pinned))]
    ["copy" telega-msg-copy-text
     :label (telega-i18n "lng_context_copy_text")
     :visible (telega-msg-content-text
               (telega-msg-for-interactive) 'with-voice-note)]
    ["copy-link" telega-msg-copy-link
     :label (telega-i18n "lng_context_copy_link")]
    ["save" telega-msg-save
     :label (if (telega-msg-match-p (telega-msg-for-interactive)
                  '(type Animation))
                (telega-i18n "lng_context_save_gif")
              (telega-i18n "lng_context_save_file"))
     :visible (telega-msg-match-p (telega-msg-for-interactive)
                '(not (type Audio)))]
    ("save-music-to"
     :label (telega-i18n "lng_context_save_music_to")
     :visible (telega-msg-match-p (telega-msg-for-interactive)
                '(type Audio))
     ["profile" telega-msg-save-audio-to-profile
      :label (telega-i18n "lng_context_save_music_profile")]
     ["saved" telega-msg-save-to-saved-messages
      :label (telega-i18n "lng_context_save_music_saved")]
     ["downloads" telega-msg-save-to-downloads
      :label (telega-i18n "lng_context_save_music_folder")]
     ["file" telega-msg-save
      :label (telega-i18n "lng_context_save_file")])

    ["favorite" telega-msg-favorite-toggle
     :label "Toggle Favorite"
     :style toggle
     :selected (telega-msg-favorite-p (telega-msg-for-interactive))]
    "---"
    ["delete" telega-msg-delete-dwim
     :label (propertize (telega-i18n "lng_context_delete_msg") 'face 'error)
     :visible (telega-msg-match-p (telega-msg-for-interactive)
                '(or (message-property :can_be_deleted_for_all_users)
                     (message-property :can_be_deleted_only_for_self)))]
    ["ban-sender" telega-msg-ban-sender
     :label (propertize "Ban Sender" 'face 'error)
     :visible (telega-msg-match-p (telega-msg-for-interactive)
                '(chat (my-permission :can_restrict_members)))]
    ["report-spam" telega-msg-report-dwim
     :label (telega-i18n "lng_report_spam")
      :visible (telega-msg-match-p (telega-msg-for-interactive)
                 '(message-property :can_report_supergroup_spam))]
    "---"
    ["view-thread" telega-msg-open-thread-or-topic
     :label (telega-i18n "lng_replies_view_thread")
     :visible (telega-msg-match-p (telega-msg-for-interactive)
                'is-thread)]
    ["view-topic" telega-msg-open-thread-or-topic
     :label (telega-i18n "lng_replies_view_topic")
     :visible (telega-msg-match-p (telega-msg-for-interactive)
                'is-forum-topic)]
    "---"
    ["mark" telega-msg-mark-toggle
     :label (telega-i18n "lng_context_select_msg")
     :style toggle
     :selected (telega-msg-marked-p (telega-msg-for-interactive))]
    ))

(defvar telega-msg-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map [remap self-insert-command] #'undefined)

    (define-key map (kbd "SPC") 'scroll-up-command)
    (define-key map (kbd "c") 'telega-msg-copy-dwim)
    (define-key map (kbd "d") 'telega-msg-delete-dwim)
    (define-key map (kbd "e") 'telega-msg-edit)
    (define-key map (kbd "f") 'telega-msg-forward-dwim)
    (define-key map (kbd "i") 'telega-describe-message)
    (define-key map (kbd "l") 'telega-msg-copy-link)
    ;; Marking, `telega-msg-forward' and `telega-msg-delete' can work
    ;; on list of marked messages
    (define-key map (kbd "m") 'telega-msg-mark-toggle)
    (define-key map (kbd "n") 'telega-msg-next)
    (define-key map (kbd "<tab>") 'telega-chatbuf-next-link)
    (define-key map (kbd "p") 'telega-msg-previous)
    (define-key map (kbd "<backtab>") 'telega-chatbuf-prev-link)
    (define-key map (kbd "r") 'telega-msg-reply)
    (define-key map (kbd "t") 'telega-msg-translate)

    (define-key map (kbd "B") 'telega-msg-ban-sender)
    (define-key map (kbd "F") 'telega-msg-forward-dwim-to-many)
    (define-key map (kbd "L") 'telega-msg-redisplay)
    (define-key map (kbd "P") 'telega-msg-pin-toggle)
    (define-key map (kbd "R") 'telega-msg-resend)
    (define-key map (kbd "S") 'telega-msg-save)
    (define-key map (kbd "T") 'telega-msg-open-thread-or-topic)
    (define-key map (kbd "U") 'telega-chatbuf-msg-marks-toggle)

    (define-key map (kbd "!") 'telega-msg-add-reaction)
    (define-key map (kbd "=") 'telega-msg-diff-edits)
    (define-key map (kbd "^") 'telega-msg-pin-toggle)
    (define-key map (kbd "DEL") 'telega-msg-delete-dwim)

    (define-key map (kbd "*") 'telega-msg-favorite-toggle)

    ;; Menu for right mouse on a message
    (define-key map [down-mouse-3] telega-msg-button-menu)
    (define-key map [mouse-3] #'ignore)

    ;; ffplay media controls for some media messages
    (define-key map (kbd ",") 'telega-msg--vvnote-rewind-10-backward)
    (define-key map (kbd "<") 'telega-msg--vvnote-rewind-10-backward)
    (define-key map (kbd ".") 'telega-msg--vvnote-rewind-10-forward)
    (define-key map (kbd ">") 'telega-msg--vvnote-rewind-10-forward)
    (define-key map (kbd "x") 'telega-msg--vvnote-play-speed-toggle)

    (define-key map (kbd "0") 'telega-msg--vvnote-stop)
    (define-key map (kbd "1") 'telega-msg--vvnote-rewind-part)
    (define-key map (kbd "2") 'telega-msg--vvnote-rewind-part)
    (define-key map (kbd "3") 'telega-msg--vvnote-rewind-part)
    (define-key map (kbd "4") 'telega-msg--vvnote-rewind-part)
    (define-key map (kbd "5") 'telega-msg--vvnote-rewind-part)
    (define-key map (kbd "6") 'telega-msg--vvnote-rewind-part)
    (define-key map (kbd "7") 'telega-msg--vvnote-rewind-part)
    (define-key map (kbd "8") 'telega-msg--vvnote-rewind-part)
    (define-key map (kbd "9") 'telega-msg--vvnote-rewind-part)

    map))

(define-button-type 'telega-msg
  :supertype 'telega
  :inserter telega-inserter-for-msg-button
  :predicate (lambda (button)
               (when-let ((msg (telega-msg-at button)))
                 (not (telega-msg-internal-p msg))))

  ;; NOTE: To make input method works under message buttons,
  ;; See `quail-input-method' for details
  'read-only t
  'front-sticky t

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
      (telega-msg-open-content msg 'clicked))))

(defun telega-msg-create-internal (chat fmt-text &rest props)
  "Create message for internal use.
Used to add content to chatbuf that is not a regular message.
FMT-TEXT is formatted text, can be created with `telega-fmt-text'.
PROPS are additional properties to the internal message."
  (declare (indent 1))
  (nconc (list :@type "message"
               :id -1
               :chat_id (plist-get chat :id)
               :content (list :@type "telegaInternal"
                              :text fmt-text))
         props))

(defun telega-msg-p (obj)
  "Return non-nil if OBJ is a message object."
  (and (listp obj) (equal "message" (plist-get obj :@type))))

(defun telega-msg-internal-p (msg)
  "Return non-nil if MSG is internal, created with `telega-msg-create-internal'."
  (eq -1 (plist-get msg :id)))

(defun telega-msg-from-history-p (msg)
  "Return non-nil if MSG belongs to chat history."
  ;; NOTE: Ignore scheduled/internal messages, see
  ;; https://github.com/zevlg/telega.el/issues/250
  (and msg
       (not (telega-msg-internal-p msg))
       (not (plist-get msg :sending_state))
       (not (plist-get msg :scheduling_state))))

(defun telega-msg-get (chat msg-id &optional callback)
  "Get message by CHAT-ID and MSG-ID pair.
If CALLBACK is not specified, then do not perform request to
telega-server, check only in messages cache.  If CALLBACK
is specified, it should accept two arguments - MESSAGE and
optional OFFLINE-P, non-nil OFFLINE-P means no request to the
telega-server has been made.

Return a message or nil if CALLBACK is not specified.
Return nil if CALLBACK is specified and message is found without
requests to telega-server, and return `:@extra' value of async request
if request is made."
  (declare (indent 2))
  ;; - Search in the messages cache
  ;; - [DON'T] Search in chatbuf messages, because message could be
  ;;           offloaded from the cache and will be outdated
  ;; - [DON'T] Check pinned messages in the chatbuf
  (let* ((chat-id (plist-get chat :id))
         (msg (gethash (cons chat-id msg-id) telega--cached-messages)))
    (if (or msg (null callback))
        (if callback
            (progn
              (funcall callback msg 'offline-p)
              nil)
          msg)

      (cl-assert callback)
      (telega--getMessage chat-id msg-id callback))))

(defun telega-msg-at-down-mouse-3 ()
  "Return message at down-mouse-3 press.
Return nil if there is no `down-mouse-3' keys in `this-command-keys'."
  (when-let* ((ev-key (assq 'down-mouse-3 (append (this-command-keys) nil)))
              (ev-start (cadr ev-key))
              (ev-point (posn-point ev-start)))
    (telega-msg-at ev-point)))

(defun telega-msg-at (&optional pos msg-predicate)
  "Return current message at POS point.
If POS is ommited, then return massage at current point.
For interactive commands acting on message at point/mouse-event
use `telega-msg-for-interactive' instead.
If MSG-PREDICATE is specified, return non-nil only if resulting
message matches MSG-PREDICATE."
  (let* ((button (button-at (or pos (point))))
         (msg (when (and button (eq (button-type button) 'telega-msg))
                (button-get button :value))))
    (when (or (null msg-predicate)
              (and msg (funcall msg-predicate msg)))
      msg)))

(defun telega-msg-for-interactive ()
  "Return message at mouse event or at current point.
Raise an error if resulting message is telega internal message.
For use by interactive commands."
  (when-let ((msg (or (telega-msg-at-down-mouse-3)
                      (telega-msg-at (point)))))
    (when (telega-msg-internal-p msg)
      (user-error "Can't operate on internal message"))
    msg))

(defun telega-msg-chosen-reaction-types (msg)
  "Return reaction chosen by me for the message MSG."
  (mapcar (telega--tl-prop :type)
          (seq-filter (telega--tl-prop :is_chosen)
                      (telega--tl-get msg :interaction_info :reactions
                                      :reactions))))

(defun telega-msg-chat (msg &optional offline-p)
  "Return chat for the MSG.
Return nil for deleted messages."
  (telega-chat-get (plist-get msg :chat_id) offline-p))

(defun telega-msg-replies-count (msg)
  "Return number of replies to the message MSG."
  (or (telega--tl-get msg :interaction_info :reply_info :reply_count) 0))

(defun telega-msg-replies-has-unread-p (msg)
  "Return non-nil if some replies to MSG has been read and there are new unread."
  (let* ((reply-info (telega--tl-get msg :interaction_info :reply_info))
         (last-msg-id (plist-get reply-info :last_message_id))
         (last-read-msg-id (plist-get reply-info :last_read_inbox_message_id)))
    (and last-msg-id last-read-msg-id (not (zerop last-read-msg-id))
         (< last-read-msg-id last-msg-id))))

(defun telega-msg-goto (msg &optional highlight-p)
  "Goto message MSG."
  ;; Ensure MSG is in the cache, to show it in the chatbuf as fast as
  ;; possible
  (let ((msg-cache-key (cons (plist-get msg :chat_id) (plist-get msg :id))))
    (unless (gethash msg-cache-key telega--cached-messages)
      (puthash msg-cache-key msg telega--cached-messages)))

  (telega-chat--goto-msg (telega-msg-chat msg) (plist-get msg :id) highlight-p))

(defun telega-msg-goto-highlight (msg)
  "Goto message MSG, highlight it."
  (telega-msg-goto msg 'highlight))

(defun telega-msg-goto-reply-to-message (msg)
  "Goto message denoted by `:reply_to' field of the message MSG."
  (let* ((reply-to (plist-get msg :reply_to))
         (chat-id (plist-get reply-to :chat_id))
         (msg-id (plist-get reply-to :message_id))
         (reply-quote (plist-get reply-to :quote)))
    (unless (or (telega-zerop chat-id) (telega-zerop msg-id))
      (telega-chat--goto-msg (telega-chat-get chat-id) msg-id
                             (unless reply-to 'highlight)
        ;; Possibly jump to the beginning of the reply quote
        (when reply-quote
          (lambda ()
            (when (telega-chatbuf--goto-msg-content
                   (plist-get reply-quote :position))
              (with-no-warnings
                (pulse-momentary-highlight-region
                 (point) (+ (point) (length (telega-tl-str reply-quote :text))))))
            ))))))

(defun telega-msg-open-sticker (msg &optional sticker)
  "Open content for sticker message MSG."
  (unless sticker
    (setq sticker (telega--tl-get msg :content :sticker)))

  (if (and (not (telega-sticker-static-p sticker))
           telega-sticker-animated-play
           (not current-prefix-arg))
      (telega-sticker--animate sticker)

    (let ((sset-id (plist-get sticker :set_id)))
      (if (string= "0" sset-id)
          (message "Sticker has no associated stickerset")

        (if-let ((sset (telega-stickerset-get sset-id 'locally)))
            (telega-describe-stickerset sset (telega-msg-chat msg))

          (with-telega-help-win "*Telegram Sticker Set*"
            (telega-ins "Loading stickerset..."))
          (telega-stickerset-get sset-id nil
            (lambda (stickerset)
              (telega-describe-stickerset
               stickerset (telega-msg-chat msg)))))))))

(defun telega-msg-open-animated-emoji (msg &optional clicked-p)
  "Open content for animated emoji message MSG."
  ;; NOTE: Sticker may be nil if yet unknown for a custom emoji
  (when-let ((sticker (telega--tl-get msg :content :animated_emoji :sticker)))
    ;; NOTE: animated message could be a static sticker, such as in
    ;; the https://t.me/tgbetachat/1319907
    (when (and (not (telega-sticker-static-p sticker))
               telega-sticker-animated-play)
      (telega-sticker--animate sticker msg)))

  ;; Try fullscreen animated emoji sticker as well
  (when clicked-p
    (when (eq telega-emoji-animated-play 'with-sound)
      (when-let ((sfile (telega--tl-get msg :content :animated_emoji :sound)))
        (when (telega-file--downloaded-p sfile)
          (telega-ffplay-run (telega-file--path sfile)
              (cdr (assq 'animated-emoji telega-open-message-ffplay-args))))))

    (telega--clickAnimatedEmojiMessage msg
      (lambda (fs-sticker)
        (when (and (not (telega--tl-error-p fs-sticker))
                   (not (telega-sticker-static-p fs-sticker))
                   telega-sticker-animated-play)
          (plist-put msg :telega-sticker-fullscreen fs-sticker)
          ;; TODO: Use tgsplay tool to play fullscreen sticker
          (telega-sticker--animate fs-sticker msg))))))

(defun telega-msg-open-story (msg)
  "Open content for the forwarded story message MSG."
  (let* ((content (plist-get msg :content))
         (chat-id (plist-get content :story_poster_chat_id))
         (story-id (plist-get content :story_id))
         (story (telega-story-get chat-id story-id)))
    (telega-story-open story msg)))

(defun telega-msg--play-video (msg file &optional done-callback)
  "Start playing video FILE for MSG."
  (declare (indent 2))
  (if (memq 'video telega-open-message-as-file)
      (progn
        (cl-assert (telega-file--downloaded-p file))
        (telega-open-file (telega-file--path file) msg))

    (let* ((start-timestamp
            (or (telega--tl-get msg :content :start_timestamp)
                (telega--tl-get msg :content :link_preview
                                :type :start_timestamp)))
           (telega-ffplay-media-timestamp
            (unless (telega-zerop start-timestamp)
              start-timestamp)))
      (telega-video-player-run (telega-file--path file) msg done-callback))))

(defun telega-msg--play-video-incrementally (msg video video-file)
  "For massage MSG start playing VIDEO file, while still downloading it."
  ;; NOTE: search `moov' atom at the beginning or ending
  ;; Considering 2k of index data per second
  (cond ((and (telega-file--downloaded-p video-file)
              (plist-get video :telega-video-pending-open))
         (telega-msg--play-video msg video-file))

        ((not (telega-file--downloading-p video-file))
         ;; Some chunk has been downloaded and no other downloading
         ;; is started yet, filename is locked, so we try to start
         ;; playing it incrementally

         ;; File is partially downloaded, start playing incrementally if:
         ;; 1) At least 3 seconds of the video is downloaded
         ;; 2) `moov' atom is available, we use
         ;;    `telega-ffplay-get-resolution' function to check this
         ;; 3) Time to download file to the end is less than it takes
         ;;    to play it
         (let* ((fsize (telega-file--size video-file))
                (dsize (telega-file--downloaded-size video-file))
                (duration (or (plist-get video :duration) 50))
                (probe-size (plist-get video :telega-video-probe-size))
                (open-time (plist-get video :telega-video-pending-open))
                ;; Downloaded duration
                (ddur (* duration (/ (float dsize) fsize))))
           (when (and
                  ;; Check for 1)
                  probe-size (> dsize probe-size) (> ddur 3)
                  ;; Check for 3)
                  open-time
                  (let* ((dtime (- (time-to-seconds) open-time))
                         (dspeed (/ ddur (if (zerop dtime) 1 dtime)))
                         (left-dtime (/ (- duration ddur 3) dspeed)))
                    (< left-dtime duration)))
             ;; Check for 2)
             (if (not (telega-ffplay-get-resolution
                       (telega-file--path video-file)))
                 ;; NOTE: Can't play this file incrementally, so start
                 ;; playing it after full download
                 (plist-put video :telega-video-probe-size nil)

               (plist-put video :telega-video-pending-open nil)
               (telega-msg--play-video msg video-file
                 (lambda ()
                   (telega-file--cancel-download video-file))))
             )))))

(defun telega-msg-open-video (msg &optional video)
  "Open content for video message MSG."
  (cl-assert (or (and msg (not video))
                 (eq video (telega--tl-get
                            msg :content :link_preview :type :video))))

  (let* ((video (or video (telega--tl-get msg :content :video)))
         ;; NOTE: always actualize info about file, because file state
         ;; might not be updated properly due to various reasons
         ;;   (telega--getFile (telega--tl-get video :video :id)))
         (video-file (telega-file--renew video :video))
         (video-file-size (telega-file--size video-file))
         (incremental-size
          (and telega-video-play-incrementally
               ;; NOTE: non-nil `video.supports_streaming' means there
               ;; is a moov atom in the video file.  We try both -
               ;; moov at the beginning and moov at the end of the
               ;; file
               (plist-get video :supports_streaming)
               (not (memq 'video telega-open-message-as-file))
               (let ((moov-size (* 2 1024 (or (plist-get video :duration) 50))))
                 (when (> video-file-size (* 2 moov-size))
                   moov-size)))))
    (when (telega--tl-get msg :content :is_secret)
      (telega--openMessageContent msg))

    (cond ((telega-file--downloaded-p video-file)
           (telega-msg--play-video msg video-file))

          (incremental-size
           ;; Play video incrementally
           (plist-put video :telega-video-pending-open (time-to-seconds))
           (plist-put video :telega-video-probe-size (* 2 incremental-size))

           ;; NOTE: if part of the file is already downloaded, start
           ;; playing it right away before starting to download it
           (unless (zerop (telega-file--downloaded-size video-file))
             (telega-msg--play-video-incrementally msg video video-file))

           (telega-file--download-incrementally video-file
               (nconc (list (cons 0 incremental-size)
                            (cons (- video-file-size incremental-size)
                                  incremental-size))
                      (telega-file--split-to-parts
                       video-file (* 256 1024)
                       incremental-size (- video-file-size incremental-size)))
             :update-callback
             (lambda (dfile &optional chunk-done-p)
               (if chunk-done-p
                   (telega-msg--play-video-incrementally msg video dfile)
                 (telega-msg-redisplay msg)))))

          (t
           (telega-file--download video-file
             :priority 32
             :update-callback
             (lambda (dfile)
               (telega-msg-redisplay msg)
               (when (telega-file--downloaded-p dfile)
                 (telega-msg--play-video msg dfile))))))))

(defun telega-msg-open-audio (msg &optional audio)
  "Open content for audio message MSG."
  (cl-assert (or (not audio)
                 (eq audio (telega--tl-get
                            msg :content :link_preview :type :audio))))

  ;; - If already playing, then pause
  ;; - If paused, start from paused position
  ;; - If not started, start playing
  (let* ((audio (or audio (telega--tl-get msg :content :audio)))
         (audio-file (telega-file--renew audio :audio))
         (proc (plist-get msg :telega-ffplay-proc))
         (paused-p (or telega-ffplay-media-timestamp
                       (telega-ffplay-paused-p proc))))
    (if (telega-ffplay-playing-p proc)
        (telega-ffplay-pause proc)
      (telega-file--download audio-file
        :priority 32
        :update-callback
        (lambda (file)
          (telega-msg-redisplay msg)
          (when (telega-file--downloaded-p file)
            (if (memq 'audio telega-open-message-as-file)
                (telega-open-file (telega-file--path file) msg)
              (plist-put msg :telega-ffplay-proc
                         (telega-ffplay-run (telega-file--path file)
                             (concat
                              (when paused-p
                                (format "-ss %.2f " paused-p))
                              (cdr (assq 'audio telega-open-message-ffplay-args)))
                           (lambda (_proc)
                             (telega-msg-redisplay msg))
                           paused-p)))))))))

(defun telega-msg-voice-note--ffplay-callback (proc msg &optional
                                                    no-progress-adjust)
  "Callback for voice/video note.
Adjust progress according to the `telega-vvnote-play-speed'.
Also, start playing next voice/video note when active voice/video
note finishes."
  ;; NOTE: adjust `:progress' with `telega-vvnote-play-speed'
  ;; since ffplay reports progress disreguarding atempo
  (unless no-progress-adjust
    (when-let* ((proc-plist (process-plist proc))
                (progress (plist-get proc-plist :progress))
                (resumed-at (or (plist-get msg :telega-ffplay-resumed-at) 0)))
      (cl-assert (numberp progress))
      (unless (equal 1 telega-vvnote-play-speed)
        (setq progress (+ resumed-at (* (- progress resumed-at)
                                        telega-vvnote-play-speed)))
        (set-process-plist proc (plist-put proc-plist :progress progress)))))

  (telega-msg-redisplay msg)

  (unless (process-live-p proc)
    ;; NOTE: another message could be already activated
    (with-telega-chatbuf (telega-msg-chat msg)
      (when (eq telega-chatbuf--vvnote-msg msg)
        (telega-chatbuf--activate-vvnote-msg nil))))

  (when (and telega-vvnote-play-next
             (eq 'finished (telega-ffplay-stop-reason proc)))
    ;; NOTE: ffplay exited normally (finished playing), try to play
    ;; next voice/video message if any
    (when-let ((next-vvnote-msg (telega-chatbuf--next-msg msg
                                  '(type VoiceNote VideoNote))))
      (with-telega-chatbuf (telega-msg-chat next-vvnote-msg)
        (telega-chatbuf--goto-msg (plist-get next-vvnote-msg :id) 'highlight))
      (telega-msg-open-content next-vvnote-msg))))

(defun telega-msg-open-voice-note (msg)
  "Open content for voiceNote message MSG."
  ;; - If already playing, then pause
  ;; - If paused, start from paused position
  ;; - If not started, start playing
  (let* ((note (or (telega--tl-get msg :content :voice_note)
                   (telega--tl-get msg :content :link_preview :type :voice_note)))
         (note-file (progn
                      (cl-assert note)
                      (telega-file--renew note :voice)))
         (proc (plist-get msg :telega-ffplay-proc))
         (paused-p (or telega-ffplay-media-timestamp
                       (telega-ffplay-paused-p proc))))
    (if (telega-ffplay-playing-p proc)
        (telega-ffplay-pause proc)
      ;; Start playing or resume from the paused moment
      (telega-file--download note-file
        :priority 32
        :update-callback
        (lambda (file)
          (cond
           ((not (telega-file--downloaded-p file))
            ;; no-op
            )

           ((memq 'voice-note telega-open-message-as-file)
            (telega-open-file (telega-file--path file) msg))

           (t
            ;; NOTE: Set moment we resumed for `:progress' correction
            ;; in the `telega-msg-voice-note--ffplay-callback'
            (plist-put msg :telega-ffplay-resumed-at paused-p)
            (plist-put msg :telega-ffplay-proc
                       (telega-ffplay-run (telega-file--path file)
                           (concat
                            (when paused-p
                              (format "-ss %.2f " paused-p))
                            (unless (equal telega-vvnote-play-speed 1)
                              (format "-af atempo=%.2f "
                                      telega-vvnote-play-speed))
                            (cdr (assq 'voice-note
                                       telega-open-message-ffplay-args)))
                         (lambda (proc)
                           (telega-msg-voice-note--ffplay-callback proc msg))
                         paused-p))
            (with-telega-chatbuf (telega-msg-chat msg)
              (telega-chatbuf--activate-vvnote-msg msg))))

          ;; NOTE: always redisplay the message to actualize
          ;; downloading progress
          (telega-msg-redisplay msg))))))

(defun telega-msg-video-note--ffplay-callback (proc frame msg)
  "Callback for video note playback."
  (let* ((proc-plist (process-plist proc))
         (note (or (telega--tl-get msg :content :video_note)
                   (telega--tl-get msg :content :link_preview :type :video_note)))
         (duration (plist-get note :duration))
         (nframes (or (float (plist-get proc-plist :nframes))
                      (* 30.0 duration)))
         (played (when frame
                   (+ (or (plist-get msg :telega-ffplay-resumed-at) 0)
                      (* (/ (car frame) nframes) duration))))
         (normalized-progress
          (when frame
            (/ (or played 0) (if (zerop duration) 0.1 duration))))
         (ffplay-frame
          (when frame
            (telega-vvnote-video--svg (cdr frame)
              :progress (if (telega-ffplay-paused-p proc)
                            (cons 'paused normalized-progress)
                          normalized-progress)))))
    ;; NOTE: Update proc's `:progress' property to start from correct
    ;; place if [x2] button is pressed
    (set-process-plist proc (plist-put proc-plist :progress played))

    ;; NOTE: Scale frame when starting to play, simulating Video
    ;; Messages 2.0 interface in official client
    (when (and ffplay-frame (consp telega-video-note-height))
      (let* ((max-scale (/ (float (cdr telega-video-note-height))
                           (float (car telega-video-note-height))))
             (scale (if (plist-get msg :telega-ffplay-resumed-at)
                        max-scale
                      (min max-scale (+ 1.0 (/ (float (car frame)) 10))))))
        (plist-put (cdr ffplay-frame) :scale scale)))
    (plist-put msg :telega-ffplay-frame ffplay-frame)

    (telega-msg-voice-note--ffplay-callback proc msg 'no-progress-adjust)))

(defun telega-msg-open-video-note (msg)
  "Open content for videoNote message MSG.
If called with `\\[universal-argument]' prefix, then open with
external player even if `telega-video-note-play-inline' is
non-nil."
  (let* ((note (or (telega--tl-get msg :content :video_note)
                   (telega--tl-get msg :content :link_preview :type :video_note)))
         (note-file (telega-file--renew note :video))
         (proc (plist-get msg :telega-ffplay-proc))
         (paused-p (or telega-ffplay-media-timestamp
                       (telega-ffplay-paused-p proc)))
         (saved-this-command this-command)
         (saved-current-prefix-arg current-prefix-arg))
    (if (telega-ffplay-playing-p proc)
        (telega-ffplay-pause proc)
      (telega-file--download note-file
        :priority 32
        :update-callback
        (lambda (file)
          (cond
           ((not (telega-file--downloaded-p file))
            ;; no-op
            )

           ((memq 'video-note telega-open-message-as-file)
            (telega-open-file (telega-file--path file) msg))

           ((and telega-video-note-play-inline
                 ;; *NOT* called interactively
                 (or (not saved-this-command)
                     (not saved-current-prefix-arg)))
            ;; NOTE: Set moment we resumed for `:progress' correction
            ;; in the `telega-msg-video-note--ffplay-callback'
            (plist-put msg :telega-ffplay-resumed-at paused-p)
            (plist-put msg :telega-ffplay-proc
                       (telega-ffplay-to-png
                           (telega-file--path file)
                           (concat
                            "-vf scale=120:120"
                            (unless (equal telega-vvnote-play-speed 1)
                              (format " -af atempo=%.2f"
                                      telega-vvnote-play-speed))
                            (concat " -f " (car telega-vvnote--has-audio-inputs))
                            " default -vsync 0")
                         (list #'telega-msg-video-note--ffplay-callback msg)
                         :seek paused-p :speed telega-vvnote-play-speed))
            (with-telega-chatbuf (telega-msg-chat msg)
              (telega-chatbuf--activate-vvnote-msg msg)))

           (t
            (telega-ffplay-run (telega-file--path file)
                (cdr (assq 'video-note telega-open-message-ffplay-args)))))

          ;; NOTE: always redisplay the message to actualize
          ;; downloading progress
          (telega-msg-redisplay msg))))))

(defun telega-msg-open-photo (msg &optional photo)
  "Open content for photo message MSG."
  (telega-photo--open (or photo (telega--tl-get msg :content :photo)) msg))

(defun telega-animation--ffplay-callback (_proc frame anim)
  "Callback for inline animation playback."
  (plist-put anim :telega-ffplay-frame-filename (cdr frame))
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
  (let* ((anim (or animation
                   (telega--tl-get msg :content :animation)
                   (telega--tl-get msg :content :link_preview :type :animation)))
         (anim-file (telega-file--renew anim :animation))
         (proc (plist-get msg :telega-ffplay-proc))
         (saved-this-command this-command)
         (saved-current-prefix-arg current-prefix-arg))
    (if (telega-ffplay-playing-p proc)
        (telega-ffplay-stop proc)
      (telega-file--download anim-file
        :priority 32
        :update-callback
        (lambda (file)
          (cond
           ((not (telega-file--downloaded-p file))
            ;; no-op
            )

           ((and (telega-animation-play-inline-p anim)
                 (or (not saved-this-command) ; *NOT* called interactively
                     (not saved-current-prefix-arg)))
            (plist-put msg :telega-ffplay-proc
                       ;; NOTE: "-an" for no sound
                       (telega-ffplay-to-png
                           (telega-file--path file) "-an"
                         (list #'telega-animation--ffplay-callback anim))))

           ((memq 'animation telega-open-message-as-file)
            (telega-open-file (telega-file--path file) msg))

           (t
            (telega-ffplay-run (telega-file--path file)
                (cdr (assq 'animation telega-open-message-ffplay-args)))))

          ;; NOTE: always redisplay the message to actualize
          ;; downloading progress
          (telega-msg-redisplay msg))))))

(defun telega-msg-open-document (msg &optional document)
  "Open content for document message MSG."
  (let* ((doc (or document (telega--tl-get msg :content :document)))
         (doc-file (telega-file--renew doc :document)))
    (telega-file--download doc-file
      :priority 32
      :update-callback
      (lambda (file)
        (telega-msg-redisplay msg)
        (when (telega-file--downloaded-p file)
          (telega-open-file (telega-file--path file) msg))))))

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

(defun telega-msg-open-link-preview (msg &optional link-preview)
  "Open content for message with webpage message MSG."
  (unless link-preview
    (setq link-preview (telega--tl-get msg :content :link_preview)))

  (if (telega-zerop (plist-get link-preview :instant_view_version))
      (let ((lp-type (plist-get link-preview :type)))
        (cl-case (telega--tl-type lp-type)
          (linkPreviewTypePhoto
           (telega-msg-open-photo msg (plist-get lp-type :photo)))
          (linkPreviewTypeVideo
           (telega-msg-open-video msg (plist-get lp-type :video)))
          (linkPreviewTypeVoiceNote
           (telega-msg-open-voice-note msg))
          (linkPreviewTypeVideoNote
           (telega-msg-open-video-note msg))
          ((linkPreviewTypeBackground
            linkPreviewTypeDocument)
           (telega-msg-open-document msg (plist-get lp-type :document)))
          ((linkPreviewTypeEmbeddedAnimationPlayer
            linkPreviewTypeEmbeddedAudioPlayer
            linkPreviewTypeEmbeddedVideoPlayer
            linkPreviewTypeExternalAudio
            linkPreviewTypeExternalVideo
            linkPreviewTypeArticle)
           ;; External link
           (telega-browse-url (plist-get link-preview :url)))
          (t
           ;; Internal link
           (telega-tme-open (plist-get link-preview :url)))))
    (telega-webpage--instant-view
     (telega-tl-str link-preview :url) (plist-get link-preview :site_name))))

  ;; ;; NOTE: "document" webpage might contain :video instead of :document
  ;; ;; see https://t.me/c/1347510619/43
  ;; (cond ((plist-get link-preview :animation)
  ;;        (telega-msg-open-animation msg (plist-get link-preview :animation)))
  ;;       ((plist-get link-preview :audio)
  ;;        (telega-msg-open-audio msg (plist-get link-preview :audio)))
  ;;       ((plist-get link-preview :document)
  ;;        (telega-msg-open-document msg (plist-get link-preview :document)))
  ;;       ((plist-get link-preview :sticker)
  ;;        (telega-msg-open-sticker msg (plist-get link-preview :sticker)))
  ;;       ((plist-get link-preview :video)
  ;;        (telega-msg-open-video msg (plist-get link-preview :video)))
  ;;       ((plist-get link-preview :video_note)
  ;;        (telega-msg-open-video-note msg (plist-get link-preview :video_note)))
  ;;       ((and (string= "photo" (plist-get link-preview :type))
  ;;             (plist-get link-preview :photo))
  ;;        (telega-msg-open-photo msg (plist-get link-preview :photo)))
  ;;       (t
  ;;        (when-let ((url (telega-tl-str link-preview :url)))
  ;;          (telega-browse-url url)))))

(defun telega-msg-open-game (msg)
  "Open content for the game message MSG."
  (telega--getCallbackQueryAnswer
   msg (list :@type "callbackQueryPayloadGame"
             :game_short_name (telega--tl-get msg :content :game :short_name))))

(defun telega-msg-open-poll (msg)
  "Open content for the poll MSG."
  (let ((poll (telega--tl-get msg :content :poll)))
    (unless (plist-get poll :is_anonymous)
      (with-telega-help-win "*Telega Poll Results*"
        (telega-ins--with-face 'bold
          (telega-ins--fmt-text (plist-get poll :question))
          (telega-ins " (" (telega-i18n "lng_polls_votes_count"
                             :count (plist-get poll :total_voter_count))
                      ")"))
        (telega-ins "\n")
        ;; Quiz explanation goes next
        (when-let ((explanation (telega--tl-get poll :type :explanation))
                   (label (propertize
                           (concat (telega-i18n "lng_polls_solution_title") ": ")
                           'face 'telega-shadow)))
          (telega-ins--labeled label nil
            (telega-ins--fmt-text explanation)
            (telega-ins "\n")))
        (telega-ins "\n")

        (let ((options (plist-get poll :options)))
          (dotimes (popt-id (length options))
            (let ((popt (seq-elt options popt-id)))
              (telega-ins--fmt-text (plist-get popt :text))
              (telega-ins--with-face 'telega-shadow
                (telega-ins-fmt " — %d%% (%s)\n"
                  (plist-get popt :vote_percentage)
                  (telega-i18n "lng_polls_votes_count"
                    :count (plist-get popt :voter_count))))
              (when-let* ((voters-reply (telega--getPollVoters msg popt-id))
                          (voters (mapcar #'telega-msg-sender
                                          (plist-get voters-reply :senders))))
                (telega-ins--line-wrap-prefix "  "
                  (seq-doseq (voter voters)
                    (telega-ins--raw-button
                        (telega-link-props 'sender voter 'type 'telega)
                      (telega-ins--msg-sender voter
                        :with-avatar-p t
                        :with-username-p 'telega-username)
                      (telega-ins "\n")))))
              (telega-ins "\n"))))))))

(defun telega-describe--giveaway-info (msg ga-info)
  (with-telega-help-win "*Telegram Giveaway Info*"
    (let* ((hard-newline (propertize "\n" 'hard t))
           (content (plist-get msg :content))
           (prize (plist-get content :prize))
           (ga-params (plist-get content :parameters))
           (boosted-chat (telega-chat-get
                          (plist-get ga-params :boosted_chat_id)))
           (channel-button
            (telega-ins--as-string
             (telega-ins--raw-button
                 (telega-link-props 'sender boosted-chat 'type 'telega)
               (telega-ins--msg-sender boosted-chat
                 :with-avatar-p t
                 :with-username-p t
                 :with-brackets-p t))))
           (many-p
            (not (seq-empty-p (plist-get ga-params :additional_chat_ids)))))
      (cl-ecase (telega--tl-type ga-info)
        (giveawayInfoCompleted
         (telega-ins--with-face 'bold
           (telega-ins (telega-i18n "lng_prizes_end_title")))
         (telega-ins hard-newline)
         (telega-ins--with-face 'bold
           (if-let ((winner-p (telega-tl-str ga-info :gift_code)))
               (telega-ins-i18n "lng_prizes_you_won"
                 :cup "🏆")
             (telega-ins-i18n "lng_prizes_you_didnt")))
         (telega-ins hard-newline hard-newline)
         )

        (giveawayInfoOngoing
         (telega-ins--with-face 'bold
           (telega-ins (telega-i18n "lng_prizes_how_title")))
         (when (plist-get ga-info :is_ended)
           (telega-ins--with-face 'telega-shadow
             (telega-ins " (" (telega-i18n "lng_prizes_end_title") ")")))
         (telega-ins "\n\n")

         (telega-ins
          (telega-i18n "lng_prizes_how_text"
            :admins (telega-i18n "lng_prizes_admins"
                      :channel channel-button
                      :count (plist-get content :winner_count)
                      :duration (telega-i18n "lng_premium_gift_duration_months"
                                  :count (plist-get prize :month_count)))))
         (telega-ins "\n\n")
         (telega-ins-i18n "lng_prizes_how_when_finish"
           :date (telega-ins--as-string
                  (telega-ins--date
                   (plist-get ga-params :winners_selection_date) 'date-time))
           :winners (telega-i18n (if (plist-get ga-params :only_new_members)
                                     (if many-p
                                         "lng_prizes_winners_new_of_many"
                                       "lng_prizes_winners_new_of_one")
                                   (if many-p
                                       "lng_prizes_winners_all_of_many"
                                     "lng_prizes_winners_all_of_one"))
                      :count (plist-get content :winner_count)
                      :channel channel-button
                      :start_date (telega-ins--as-string
                                   (telega-ins--date
                                    (plist-get ga-info :creation_date)
                                    'date-time))))
         (telega-ins "\n\n")

         (let ((status (plist-get ga-info :status)))
           (cl-ecase (telega--tl-type status)
             (giveawayParticipantStatusDisallowedCountry
              (telega-ins-i18n "lng_prizes_how_no_country"))
             (giveawayParticipantStatusAdministrator
              (telega-ins-i18n "lng_prizes_how_no_admin"
                :channel channel-button))
             (giveawayParticipantStatusAlreadyWasMember
              (telega-ins-i18n (if (telega-chat-channel-p boosted-chat)
                                   "lng_prizes_how_no_joined"
                                 "lng_prizes_how_no_joined_group")
                ;; TODO: `:date'
                ))
             (giveawayParticipantStatusParticipating
              (telega-ins-i18n (if many-p
                                   "lng_prizes_how_yes_joined_many"
                                 "lng_prizes_how_yes_joined_one")
                :channel channel-button))
             (giveawayParticipantStatusEligible
              (telega-ins
               "You are not eligible to participate in this giveaway."))))
         )))

    ;; NOTE: -1 as fill-column is not suitable for filling
    (kill-local-variable 'fill-column)
    (fill-region (point-min) (point-max) 'center)
    ))

(defun telega-msg-open-giveaway (msg)
  "Open show more details about giveaway message MSG."
  (message "telega: Fetching giveaway info...")
  (telega--getGiveawayInfo msg
    (lambda (result)
      (message "")
      (telega-describe--giveaway-info msg result))))

(defun telega-msg-open-gift (msg)
  "Open show more details about gift message MSG."
  (unless (telega-msg-match-p msg 'is-outgoing)
    (message "TODO: `telega-msg-open-gift'")
    ))

(defun telega-msg-emojis-only-p (msg)
  "Return non-nil if text message MSG contains only emojis."
  (when (telega-msg-match-p msg '(type Text))
    (let* ((content-text (telega--tl-get msg :content :text))
           (text (plist-get content-text :text)))
      ;; NOTE: Should not contain custom emojis or other entities
      (and (seq-empty-p (plist-get content-text :entities))
           (not (text-property-not-all
                 0 (length text) 'telega-emoji-p t text))))))

(defun telega-msg-open-content (msg &optional clicked-p)
  "Open message MSG content.
non-nil CLICKED-P means message explicitly has been clicked by user."
  ;; NOTE: openMessageContent for is_secret content only after
  ;; downloading completed
  (unless (telega--tl-get msg :content :is_secret)
    (telega--openMessageContent msg))

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
     (when-let ((link-preview (telega--tl-get msg :content :link_preview)))
       (telega-msg-open-link-preview msg link-preview)))
    (messagePoll
     (telega-msg-open-poll msg))
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
    (messagePinMessage
     (let ((pin-msg-id (telega--tl-get msg :content :message_id)))
       (telega-chat--goto-msg (telega-msg-chat msg) pin-msg-id 'hightlight)))
    (messageChatDeleteMember
     (telega-describe-user
      (telega-user-get (telega--tl-get msg :content :user_id))))
    (messageAnimatedEmoji
     (telega-msg-open-animated-emoji msg clicked-p))
    (messageStory
     (telega-msg-open-story msg))
    (messageGiveaway
     (telega-msg-open-giveaway msg))
    (messageGiveawayCompleted
     (telega-chat--goto-msg (telega-msg-chat msg)
         (telega--tl-get msg :content :giveaway_message_id) 'hightlight))
    (messageGift
     (telega-msg-open-gift msg))

    (t (message "TODO: `open-content' for <%S>"
                (telega--tl-type (plist-get msg :content))))))

(defun telega-msg-open-thread-or-topic (msg)
  "View message MSG in the topic or thread.
MSG could be a channel post, in this case open thread in the
discussion group.
Or MSG could be in supergroup, then filter messages to the
corresponding thread or topic."
  (interactive (list (telega-msg-for-interactive)))
  (cond ((telega-msg-match-p msg 'is-forum-topic)
         (telega-topic-goto (telega-msg-topic msg)
                            (plist-get msg :id)))

        ((telega-msg-match-p msg 'post-with-comments)
         (telega-chat--goto-thread (telega-msg-chat msg 'offline)
                                   (plist-get msg :id)))

        ((telega-msg-match-p msg 'is-thread)
         (telega-chat--goto-thread
          (telega-msg-chat msg 'offline)
          (telega--tl-get msg :topic_id :message_thread_id)
          (plist-get msg :id)))))

(defun telega-msg-can-open-media-timestamp-p (msg)
  "Return non-nil if MSG can be opened with custom media timestamp.
Only video, audio, video-note, voice-note or a message with link
preview, having media content, can be opened with media timestamp."
  (telega-msg-match-p msg
    '(or (type Audio Video VideoNote VoiceNote)
         (link-preview Audio Video VideoNote VoiceNote))))

(defun telega-msg-open-media-timestamp (msg timestamp &optional error-p)
  "Open media message MSG (or replied to) at a given TIMESTAMP."
  (cl-assert (and (telega-msg-p msg) (numberp timestamp)))
  (if (telega-msg-can-open-media-timestamp-p msg)
      (let ((telega-ffplay-media-timestamp timestamp))
        (telega-msg-open-content msg))

    (when error-p
      (error "telega: Message ID=%d does not have media content"
             (plist-get msg :id)))

    ;; NOTE: timestamp might refer media in the reply to message
    (let* ((reply-to (plist-get msg :reply_to))
           (reply-to-msg-id (plist-get reply-to :message_id))
           (reply-in-chat-id (or (plist-get reply-to :chat_id)
                                 (plist-get msg :chat_id))))
      (unless (and reply-in-chat-id reply-to-msg-id)
        (error "telega: no media message is associated with timestamp"))

      (telega-chat--goto-msg
          (telega-chat-get reply-in-chat-id) reply-to-msg-id 'hightlight
        (lambda ()
          (telega-msg-open-media-timestamp
           (telega-msg-at (point)) timestamp t))))))

(defun telega-msg--track-file-uploading-progress (msg)
  "Track uploading progress for the file associated with MSG."
  (let ((msg-file (telega-msg--content-file msg)))
    (when (and msg-file (telega-file--uploading-p msg-file))
      (telega-file--add-update-callback (plist-get msg-file :id)
        (lambda (ufile)
          (telega-msg-redisplay msg)
          ;; Remove update callback when uploading is completed
          (telega-file--uploading-p ufile))))))


;; Msg Sender
(defun telega-msg-sender (tl-obj)
  "Convert given TL-OBJ to message sender (a chat or a user).
TL-OBJ could be a \"message\", \"chatMember\", \"messageSender\" or
\"chatMessageSender\".
Return a user or a chat."
  (let ((sender (cl-ecase (telega--tl-type tl-obj)
                  (chatMessageSender
                   (when (or (not (plist-get tl-obj :needs_premium))
                             (plist-get telega--options :is_premium))
                     (plist-get tl-obj :sender)))
                  (message (plist-get tl-obj :sender_id))
                  (chatMember (plist-get tl-obj :member_id))
                  ((messageSenderUser messageSenderChat) tl-obj))))
    ;; NOTE: sender could be `nil' for internal telega messages, see
    ;; `telega-msg-create-internal'.
    (when sender
      (if (eq 'messageSenderUser (telega--tl-type sender))
          (telega-user-get (plist-get sender :user_id))
        (cl-assert (eq 'messageSenderChat (telega--tl-type sender)))
        (telega-chat-get (plist-get sender :chat_id))))))

(defun telega-msg-sender-brackets (msg-sender)
  "Return MSG-SENDER's brackets from `telega-brackets'."
  (or (cdr (seq-find (lambda (bspec)
                       (telega-sender-match-p msg-sender (car bspec)))
                     telega-brackets))
      ;; Fallback to default brackets
      (and (telega-chat-p msg-sender) '("[" "]"))
      (progn (cl-assert (telega-user-p msg-sender)) '("{" "}"))))

(defun telega-msg-sender-username (msg-sender &optional with-prefix-p)
  "Return username for the message sender MSG-SENDER.
If WITH-PREFIX-P is non-nil, then prefix username with \"@\" char."
  (when-let* ((usernames
               (if (telega-user-p msg-sender)
                   (plist-get msg-sender :usernames)
                 (cl-assert (telega-chat-p msg-sender))
                 (plist-get (telega-chat--info msg-sender 'local) :usernames)))
              (active-usernames
               (plist-get usernames :active_usernames))
              (username
               (when (> (length active-usernames) 0)
                 ;; NOTE: from TDLib docs - the first one must be shown as the
                 ;; primary username.
                 (aref active-usernames 0))))
    (concat (when with-prefix-p "@") username)))
(defalias 'telega-chat-username 'telega-msg-sender-username)

(defun telega-msg-sender-initials (msg-sender)
  "Return MSG-SENDER's initials for avatar image."
  (if-let ((user (if (telega-user-p msg-sender)
                     msg-sender
                   (telega-chat-user msg-sender))))
      (let ((first-name (telega-tl-str user :first_name))
            (last-name (telega-tl-str user :last_name)))
        (concat (when first-name (substring first-name 0 1))
                (when last-name (substring last-name 0 1))))

    (substring (telega-chat-title msg-sender 'no-badges) 0 1)))

(defun telega-msg-sender--verification-badges (v-status)
  "Return verification status bages string."
  (when v-status
    (concat
     (when (plist-get v-status :is_scam)
       (propertize (telega-i18n "lng_scam_badge") 'face 'error))
     (when (plist-get v-status :is_fake)
       (propertize (telega-i18n "lng_fake_badge") 'face 'error))
     (if (plist-get v-status :is_verified)
         (telega-symbol 'verified)
       (let ((v-custom-emoji-id
              (plist-get v-status :bot_verification_icon_custom_emoji_id)))
         (unless (telega-zerop v-custom-emoji-id)
           (telega-symbol
            'checkmark
            (when-let ((v-sticker (telega-custom-emoji-get v-custom-emoji-id)))
              (telega-sticker--image v-sticker)))))))))

(defun telega-msg-sender-title (msg-sender &rest args)
  "Return title for the message sender MSG-SENDER.
ARGS are passed directly to `telega-ins--msg-sender'."
  (declare (indent 1))
  (telega-ins--as-string
   (apply #'telega-ins--msg-sender msg-sender args)))

(defun telega-msg-sender-title--special (msg-sender &rest args)
  "Return title for message sender MSG-SENDER to be used in special messages.
ARGS are passed directly to `telega-ins--msg-sender'."
  (declare (indent 1))
  (telega-ins--as-string
   (telega-ins--raw-button
       (list 'action (lambda (_button)
                       (telega-describe-msg-sender msg-sender)))
     (telega-ins--with-face 'bold
       (apply #'telega-ins--msg-sender msg-sender :with-avatar-p t args)))))

(defun telega-msg-sender-palette (msg-sender)
  "Return palette for the message sender MSG-SENDER."
  (unless (memq telega-palette-context telega-palette-context-ignore-list)
    (or (plist-get msg-sender :telega-palette)
        (let ((palette (telega-palette-by-color-id
                        (plist-get msg-sender :accent_color_id))))
          (plist-put msg-sender :telega-palette palette)
          palette))))

(defun telega-msg-sender-title-faces (msg-sender &optional palette)
  "Compute faces list to use for MSG-SENDER title."
  (unless palette
    (setq palette (let ((telega-palette-context 'title))
                    (telega-msg-sender-palette msg-sender))))

  (cons (if (telega-me-p msg-sender)
            'telega-msg-self-title
          'telega-msg-user-title)
        (when palette
          (list (assq :foreground palette)))))

(defun telega-msg-sender-block (msg-sender &optional callback)
  "Block the message sender MSG-SENDER."
  (telega--setMessageSenderBlockList msg-sender 'blockListMain callback))

(defun telega-msg-sender-unblock (msg-sender &optional callback)
  "Unblock the MSG-SENDER."
  (telega--setMessageSenderBlockList msg-sender nil callback))

(defun telega-describe-msg-sender (sender)
  "Describe a message SENDER."
  (if (telega-user-p sender)
      (telega-describe-user sender)
    (cl-assert (telega-chat-p sender))
    (telega-describe-chat sender)))

(defsubst telega-msg-by-me-p (msg)
  "Return non-nil if sender of MSG is me."
  (telega-me-p (telega-msg-sender msg)))

(defsubst telega-msg-seen-p (msg &optional chat)
  "Return non-nil if MSG has been already read in CHAT."
  (unless chat (setq chat (telega-msg-chat msg)))
  (<= (plist-get msg :id)
      (or (with-telega-chatbuf chat
            (telega-chatbuf--last-read-inbox-msg-id))
          (plist-get chat :last_read_inbox_message_id))))

(defsubst telega-msg-marked-p (msg)
  "Return non-nil if message MSG is marked."
  (with-telega-chatbuf (telega-msg-chat msg)
    (memq msg telega-chatbuf--marked-messages)))

(defun telega-msg-observable-p (msg &optional chat)
  "Return non-nil if MSG is observable in a chatbuffer."
  (with-telega-chatbuf (or chat (telega-msg-chat msg))
    (telega-chatbuf--msg-observable-p msg)))

;;; Ignoring messages
(defun telega--ignored-messages-ring-index (msg)
  "Return ignored MSG index inside `telega--ignored-messages-ring'."
  (catch 'found
    (dotimes (ind (ring-length telega--ignored-messages-ring))
      (let ((ring-msg (ring-ref telega--ignored-messages-ring ind)))
        (when (and (eq (plist-get msg :chat_id)
                       (plist-get ring-msg :chat_id))
                   (eq (plist-get msg :id)
                       (plist-get ring-msg :id)))
          (throw 'found ind))))))

(defun telega-msg-ignore (msg &optional ignored-by)
  "Mark message MSG to be ignored (not viewed, notified about) in chats.
By side effect adds MSG into `telega--ignored-messages-ring' to be viewed
with `M-x telega-ignored-messages RET'.
IGNORED-BY specifies function by which message is ignored."
  (plist-put msg :ignored-p (or ignored-by t))

  ;; Remove message with same chat_id/id
  (when-let ((ind (telega--ignored-messages-ring-index msg)))
    (ring-remove telega--ignored-messages-ring ind))

  (ring-insert telega--ignored-messages-ring msg)
  (telega-debug "IGNORED msg%s: %S"
                (if ignored-by (format " (by `%S')" ignored-by) "") msg))

(defun telega-msg-run-ignore-predicates (msg &optional last-msg-p)
  "Run `telega-msg-ignore-predicates' over the MSG.
Do not ignore outgoing messages.
If any of function from `telega-msg-ignore-predicates' return non-nil,
then mark MSG as ignored.
If LAST-MSG-P is specified, then also update `:telega-last-ignored'
chat's property denoting last ignored message.
Return function by which MSG has been ignored."
  (let ((ignored-p
         (cond ((plist-get msg :is_outgoing)
                nil)
               ((plist-member msg :ignored-p)
                (plist-get msg :ignored-p))
               (msg
                (when-let ((ignored-by (cl-some (lambda (predicate)
                                                  (when (funcall predicate msg)
                                                    predicate))
                                                telega-msg-ignore-predicates)))
                  (telega-msg-ignore msg ignored-by)
                  ignored-by)))))
    (when (and ignored-p last-msg-p)
      (plist-put (telega-msg-chat msg) :telega-last-ignored
                 (cons (plist-get msg :id) ignored-p)))
    ignored-p))


(defun telega-msg-next (n)
  "Goto next N messages."
  (interactive "p")
  (let ((button (telega-button-forward n
                  (button-type-get 'telega-msg :predicate))))
    (when (and button telega-msg-goto-content)
      (telega-chatbuf--goto-msg-content))
    button))

(defun telega-msg-previous (n)
  "Goto N previous messages."
  (interactive "p")
  (telega-msg-next (- n)))

(defun telega-msg-unmark (msg)
  "Unmark message MSG."
  (with-telega-chatbuf (telega-msg-chat msg)
    (when (telega-msg-marked-p msg)
      (setq telega-chatbuf--marked-messages
            (delq msg telega-chatbuf--marked-messages))
    (telega-msg-redisplay msg)
    (telega-chatbuf--chat-update "marked-messages"))))

(defun telega-msg-mark-toggle (msg)
  "Toggle mark of the message MSG."
  (interactive (list (telega-msg-for-interactive)))
  (with-telega-chatbuf (telega-msg-chat msg)
    (if (memq msg telega-chatbuf--marked-messages)
        (setq telega-chatbuf--marked-messages
              (delq msg telega-chatbuf--marked-messages))
      (setq telega-chatbuf--marked-messages
            (push msg telega-chatbuf--marked-messages)))
    (telega-msg-redisplay msg)
    ;; NOTE: on last message don't move point to the prompt, because
    ;; after marking messages some message command is expected (for
    ;; example forwarwarding)
    (unless (eq msg (telega-chatbuf--last-msg))
      (telega-msg-next 1))

    (telega-chatbuf--chat-update "marked-messages")))

(defun telega-msg-pin-toggle (msg)
  "Toggle pin state of the message MSG.
For interactive use only."
  (interactive (list (telega-msg-for-interactive)))

  (unless (telega-msg-match-p msg '(chat (my-permission :can_pin_messages)))
    (user-error "telega: No permissions to pin/unpin messages"))

  (if (plist-get msg :is_pinned)
      (telega--unpinChatMessage msg)

    (let* ((chat (telega-msg-chat msg))
           (chat-private-p (telega-chat-private-p chat))
           (for-self-only
            (or (telega-me-p (telega-msg-chat msg))
                (when chat-private-p
                  (not (y-or-n-p (concat
                                  (telega-i18n "lng_pinned_also_for_other"
                                    :user (telega-ins--as-string
                                           (telega-ins--msg-sender chat
                                             :with-avatar-p t)))
                                  "? "))))))
           ;; NOTE: always notify on private chats
           (notify (or chat-private-p
                       (unless for-self-only
                         (y-or-n-p (concat "Pin message.  "
                                           (telega-i18n "lng_pinned_notify")
                                           "? "))))))
      (telega--pinChatMessage msg (not notify) for-self-only))))

(defun telega-msg-favorite-p (msg)
  "Return non-nil if MSG is favorite in the chat."
  (let ((chat-id (plist-get msg :chat_id))
        (msg-id (plist-get msg :id)))
    (seq-find (lambda (fav)
                (and (eq chat-id (plist-get fav :chat_id))
                     (eq msg-id (plist-get fav :id))))
              telega--favorite-messages)))

(defun telega-msg--favorite-messages-file-fetch ()
  "Asynchronously fetch file storing list of favorite messages."
  (when-let ((chat-me (telega-chat-me)))
    (telega--searchChatMessages chat-me
        '(:@type "searchMessagesFilterDocument") 0 0
      :query "#telega_favorite_messages"
      :limit 1
      :callback
      (lambda (reply)
        (let ((messages (plist-get reply :messages)))
          (setq telega--favorite-messages-storage-message
                (unless (seq-empty-p messages)
                  (seq-first messages)))
          (when telega--favorite-messages-storage-message
            (let ((file (telega-msg--content-file
                         telega--favorite-messages-storage-message)))
              (cl-assert file)
              ;; And now load associated file asynchronously
              (telega-file--download file
                :priority 32
                :update-callback
                (lambda (tl-file)
                  (when (telega-file--downloaded-p tl-file)
                    (setq telega--favorite-messages
                          (with-temp-buffer
                            (insert-file-contents
                             (telega--tl-get tl-file :local :path))
                            (goto-char (point-min))
                            (read (current-buffer))))
                    (telega-debug "Loaded %d favorite messages"
                                  (length telega--favorite-messages)))
                  )))))))))

(defun telega-msg--favorite-messages-file-store ()
  "Upload `telega--favorite-messages' value as file into \"Saved Messages\"."
  (when (eq 'not-yet-fetched telega--favorite-messages-storage-message)
    (user-error "telega: fetch favorite messages file first"))

  (let ((fav-msgs-filename
         (expand-file-name "favorite-messages.txt" telega-temp-dir)))
    (write-region (prin1-to-string telega--favorite-messages)
                  nil fav-msgs-filename nil 'quiet)

    (if (null telega--favorite-messages-storage-message)
        ;; No file with favorite messages yet exists, create a new one
        (when (y-or-n-p "telega: No favorite messages storage yet, create? ")
          (telega--sendMessage
           (telega-chat-me)
           (list :@type "inputMessageDocument"
                 :document (telega-chatbuf--gen-input-file
                            fav-msgs-filename 'Document)
                 :caption (telega-fmt-text "#telega_favorite_messages")
                 :disable_content_type_detection t)
           nil nil
           :callback (lambda (msg)
                       (setq telega--favorite-messages-storage-message msg)
                       ;; Wait for "updateMessageSendSucceeded" event
                       ;; to update it
                       )))

      (when (plist-get telega--favorite-messages-storage-message :sending_state)
        (error "telega: Storage for the favorite messages is not yet created"))
      (with-current-buffer (find-file-noselect fav-msgs-filename nil 'raw)
        (setq telega--help-win-param telega--favorite-messages-storage-message)
        (telega-edit-file-save-buffer)
        (kill-buffer)))
      ))

(defun telega-msg-favorite-toggle (msg &optional with-comment-p)
  "Toggle MSG being favorite in the chat.
`\\[universal-argument]' let you to specify or change comment to the
favorite message."
  (interactive (list (telega-msg-for-interactive)
                     current-prefix-arg))
  (let* ((fav (telega-msg-favorite-p msg))
         (comment (when with-comment-p
                    (read-string "Comment for the message: "))))
    (when fav
      (setq telega--favorite-messages
            (delq fav telega--favorite-messages)))

    (when (or (not fav) comment)
      (setq telega--favorite-messages
            (cons (nconc (list :chat_id (plist-get msg :chat_id)
                               :id (plist-get msg :id)
                               :timestamp (telega-time-seconds))
                         (when comment
                           (list :comment comment)))
                  telega--favorite-messages))))
  (telega-msg--favorite-messages-file-store)
  (telega-msg-redisplay msg)
  (telega-root-view--update :on-message-update msg))

(defun telega-msg--content-file (msg)
  "For message MSG return its content file as TDLib object."
  (let* ((content (plist-get msg :content))
         (lp-type (telega--tl-get content :link_preview :type))
         (file-accessor
          (cl-some (lambda (accessor)
                     (when-let ((place (or (plist-get content (car accessor))
                                           (plist-get lp-type (car accessor)))))
                       (cons place (cdr accessor))))
                   '((:document   . :document)
                     (:video      . :video)
                     (:photo      . :photo)
                     (:audio      . :audio)
                     (:voice_note . :voice)
                     (:video_note . :video)
                     (:animation  . :animation)
                     (:sticker    . :sticker)))))
    (when file-accessor
      ;; NOTE: special case for the `:photo' accessor, use highest
      ;; resolution photo
      (telega-file--renew (if (eq :photo (cdr file-accessor))
                              (telega-photo--highres (car file-accessor))
                            (car file-accessor))
                          (cdr file-accessor)))))

(defun telega-msg-resend (msg)
  "Try to resend message MSG."
  (interactive (list (telega-msg-for-interactive)))
  (let* ((state (plist-get msg :sending_state))
         (pay-stars
          (let ((required-stars
                 (plist-get state :required_paid_message_star_count)))
            (when (and (not (telega-zerop required-stars))
                       (yes-or-no-p
                        (telega-i18n "lng_payment_confirm_sure"
                          :amount (telega-ins--as-string
                                   (telega-ins (telega-symbol 'telegram-star))
                                   (telega-ins-fmt "%d" required-stars))
                          :count 1)))
              required-stars))))
    (telega--resendMessages (list msg) nil pay-stars)))

(defun telega-msg-save (msg &optional to-saved-messages-p)
  "Save messages's MSG media content to a file.
If MSG is an animation message, then possibly add animation to
the saved animations list.
If MSG is a sticker message, then possibly add sticker to
the favorite stickers list.
If `\\[universal-argument]' is specified, then save message to the
Saved Messages."
  (interactive (list (telega-msg-for-interactive)
                     current-prefix-arg))
  (let ((file (telega-msg--content-file msg)))
    (cond
     (to-saved-messages-p
      (unless (telega-msg-match-p msg '(message-property :can_be_copied))
        (user-error (concat "telega: "
                            (telega-i18n "lng_error_noforwards_group"))))
      (let ((echo-msg (telega-ins--as-string
                       (telega-ins "Forwarding to ")
                       (telega-ins--msg-sender (telega-chat-me)
                         :with-avatar-p t
                         :with-brackets-p t)
                       (telega-ins "..."))))
        (message echo-msg)
        (telega--forwardMessages
         (telega-chat-me) (telega-msg-chat msg) (list msg)
         nil nil nil
         :callback (lambda (_reply)
                     (message (concat echo-msg "DONE"))))))
     ((not file)
      (user-error "telega: No file associated with message"))
     ((and (telega-msg-match-p msg '(type Animation))
           (y-or-n-p "Add animation to Saved Animations? "))
      (telega--addSavedAnimation
       (list :@type "inputFileId" :id (plist-get file :id)))
      (message "telega: saved new animation"))

     ((and (telega-msg-match-p msg '(type Sticker))
           (y-or-n-p "Add sticker to Favorite Stickers? "))
      (telega--addFavoriteSticker
       (list :@type "inputFileId" :id (plist-get file :id))))

     (t
      (unless (telega-msg-match-p msg '(message-property :can_be_saved))
        (user-error (concat "telega: "
                            (telega-i18n "lng_error_nocopy_group"))))

      ;; NOTE: Start downloading file in the background while
      ;; reading directory to save file to
      (unless (telega-file--downloaded-p file)
        (telega-file--download file
          :priority 32
          :update-callback
          (lambda (_dfile)
            (telega-msg-redisplay msg))))

      (let ((save-dir (or telega-msg-save-dir
                          (read-directory-name
                           (concat (telega-i18n "lng_save_file") ": ")))))
        ;; NOTE: Ensure corresponding directory exists
        (unless (file-exists-p save-dir)
          (if (y-or-n-p
               (format-message
                "Directory `%s' does not exist; create? " save-dir))
              (make-directory save-dir t)
            (error "Canceled")))

        ;; See https://github.com/tdlib/td/issues/379
        (telega-file--download file
          :priority 32
          :update-callback
          (lambda (dfile)
            (when (telega-file--downloaded-p dfile)
              (let* ((dfile-name (telega--tl-get dfile :local :path))
                     (new-fpath (expand-file-name
                                 (file-name-nondirectory dfile-name)
                                 save-dir)))
                (copy-file dfile-name new-fpath 1)
                (message (format "Wrote %s" new-fpath)))))))))))

(defun telega-msg-save-audio-to-profile (msg)
  "Save audio message MSG into profile."
  (interactive (list (telega-msg-for-interactive)))
  (user-error "TODO: save to profile")
  )

(defun telega-msg-save-to-saved-messages (msg)
  "Save a message MSG into Saved Messages."
  (interactive (list (telega-msg-for-interactive)))
  (user-error "TODO: save to saved messages")
  )

(defun telega-msg-save-to-downloads (msg)
  "Add message's MSG file into Downloads folder."
  (interactive (list (telega-msg-for-interactive)))
  (user-error "TODO: save to downloads")
  )

(defun telega-msg-copy-link (msg &optional for-thread-p)
  "Copy link to message to kill ring.
Use \\[yank] command to paste a link."
  (interactive (list (telega-msg-for-interactive)
                     (when telega-chatbuf--topic t)))
  (let* ((chat (telega-msg-chat msg 'offline))
         (media-timestamp
          (when (telega-msg-can-open-media-timestamp-p msg)
            (when-let ((proc (plist-get msg :telega-ffplay-proc)))
              (floor (telega-ffplay-progress proc)))))
         ;; NOTE: `getMessageLink' is available only for already sent
         ;; messages in supergroups and channels, or if
         ;; message.can_get_media_timestamp_links and a media
         ;; timestamp link is generated. (TDLib docs)
         (link (if (and (or (telega-chat-match-p chat '(type supergroup channel))
                            (and (telega-msg-match-p msg
                                   '(message-property
                                     :can_get_media_timestamp_links))
                                 media-timestamp))
                        (not (plist-get msg :sending_state))
                        (not (plist-get msg :scheduling_state)))
                   (telega--getMessageLink msg
                     :media-timestamp media-timestamp
                     :for-thread-p for-thread-p)
                 (telega-tme-internal-link-to msg))))
    (kill-new link)
    (message "Copied link: %s" link)))

(defun telega-msg-content-text (msg &optional with-speech-recognition-p)
  "Return message's content text or a caption.
Return nil if message has no associated text.
If WITH-SPEECH-RECOGNITION-P is non-nil, also examine speech
recognition text if message is a VoiceNote message."
  (let ((content (plist-get msg :content))
        (telega-inhibit-telega-display-by t))
    (or (telega-tl-str content :text)
        (telega-tl-str content :caption)
        ;; See FR https://t.me/emacs_telega/34839
        (and with-speech-recognition-p
             (telega-msg-match-p msg '(type VoiceNote))
             (telega-tl-str
              (telega--tl-get content :voice_note :speech_recognition_result)
              :text)))))

(defun telega-msg-copy-text (msg &optional no-properties)
  "Copy a text of the message MSG.
If `\\[universal-argument]' is supplied, then copy without text properties."
  (interactive (list (telega-msg-for-interactive)
                     current-prefix-arg))

  (unless (telega-msg-match-p msg '(message-property :can_be_saved))
    (user-error "telega: %s" (telega-i18n (if (plist-get msg :is_channel_post)
                                              "lng_error_nocopy_channel"
                                            "lng_error_nocopy_group"))))

  (let* ((translated (plist-get msg :telega-translated))
         (msg-text
          (if (and translated (y-or-n-p "telega: Copy translation text? "))
              (plist-get translated :text)
            (telega-msg-content-text msg 'with-voice-note))))
    (unless msg-text
      (user-error "Nothing to copy"))
    (kill-new (if no-properties
                  (substring-no-properties msg-text)
                msg-text))
    (message "Copied message text (%d chars)" (length msg-text))))

(defun telega-msg--tl-entity-text (msg tl-entity)
  "Return MSG text at point limited by TL-ENTITY."
  (when-let ((msg-fmt-text (or (telega--tl-get msg :content :text)
                               (telega--tl-get msg :content :caption))))
    (telega-tl-str
     (telega-fmt-text-substring
      msg-fmt-text
      (plist-get tl-entity :offset)
      (+ (plist-get tl-entity :offset) (plist-get tl-entity :length))))))

(defun telega-msg-copy-dwim (msg &optional no-properties)
  "Copy text in DWYM manner.
If region is selected copy a region.
If point is under url, copy this url.
If point is inside code block, copy code from this code block.
Otherwise copy message's text.
If `\\[universal-argument]' is supplied, then copy without text properties."
  (interactive (list (telega-msg-for-interactive)
                     current-prefix-arg))

  (let* ((entities (get-text-property (point) :tl-entities))
         (telega-link (get-text-property (point) :telega-link))
         (telega-inhibit-telega-display-by t)
         (ent nil)
         (ctext (cond ((region-active-p)
                       (prog1
                           (buffer-substring (region-beginning) (region-end))
                         (deactivate-mark)))

                      ((memq (car telega-link) '(file url))
                       (cdr telega-link))

                      ((setq ent (telega--tl-entity-get
                                  entities 'textEntityTypeUrl))
                       (telega-msg--tl-entity-text msg ent))

                      ((setq ent (telega--tl-entity-get
                                  entities 'textEntityTypeTextUrl))
                       (telega-tl-str (plist-get ent :type) :url))

                      ((setq ent (telega--tl-entity-get
                                  entities 'textEntityTypePreCode))
                       (telega-msg--tl-entity-text msg ent))

                      (t
                       (call-interactively #'telega-msg-copy-text)
                       ;; NOTE: `telega-msg-copy-text' does all the
                       ;; job by itself, so no need to do anything
                       ;; after it
                       nil))))
    (when ctext
      (setq ctext (if no-properties
                      (substring-no-properties ctext)
                    ctext))
      (kill-new ctext)
      (message "%s (%s)" (telega-i18n "lng_text_copied") ctext))
    ))

(defun telega-msg-ban-sender (msg)
  "Ban forever MSG sender in the chat.
Also query about:
- Report MSG sender as spammer
- Delete all messages from the MSG sender
- Delete only MSG

Requires administrator rights in the chat."
  (interactive (list (telega-msg-for-interactive)))
  (let ((chat (telega-msg-chat msg))
        (sender (telega-msg-sender msg)))
    (unless (plist-get (telega-chat-member-my-permissions chat)
                       :can_restrict_members)
      (user-error "Can't restrict users in this chat"))
    (unless (telega-user-p sender)
      (user-error "Can't ban anonymous message sender"))

    (let* ((report-p
            (when (telega-chat-match-p chat
                    '(type supergroup channel))
              (y-or-n-p (concat (telega-i18n "lng_report_spam") "? "))))
           (delete-all-p
            (when (telega-chat-match-p chat
                    '(and (type supergroup channel)
                          (my-permission :can_delete_messages)))
              (y-or-n-p (concat (telega-i18n "lng_delete_all_from") "? "))))
           (delete-msg-p
            (when (and (not delete-all-p)
                       (telega-chat-match-p chat
                         '(my-permission :can_delete_messages)))
              (y-or-n-p (concat (telega-i18n "lng_deleted_message") "? ")))))
      (when report-p
        (telega--reportSupergroupSpam (telega-chat--supergroup chat) msg))
      (when delete-all-p
        (telega--deleteChatMessagesBySender chat sender))
      (when delete-msg-p
        (telega--deleteMessages msg 'revoke))

      ;; Ban forever
      (telega--setChatMemberStatus
       chat sender
       (list :@type "chatMemberStatusBanned"
             :banned_until_date 0)))))

(defun telega-msg-stop-poll (msg)
  "Stop poll message MSG."
  (interactive (list (telega-msg-for-interactive)))
  (when (yes-or-no-p (telega-i18n "lng_polls_stop_warning"))
    (telega--stopPoll msg)))

(defun telega-ins--message-read-date (msg-read-date)
  "Inserter for the MessageReadDate structure."
  (cl-ecase (telega--tl-type msg-read-date)
    (messageReadDateRead
     (telega-ins--date (plist-get msg-read-date :read_date) 'date-time))
    (messageReadDateUnread
     (telega-ins "Unread"))
    (messageReadDateTooOld
     (telega-ins "Too Old"))
    (messageReadDateUserPrivacyRestricted
     (telega-ins "User restricted"))
    (messageReadDateMyPrivacyRestricted
     (telega-ins "You restricted"))
     ))

(defun telega-describe-message (msg &optional for-thread-p)
  "Show info about message at point."
  (interactive (list (telega-msg-for-interactive)
                     (when telega-chatbuf--topic t)))
  (with-telega-help-win "*Telegram Message Info*"
    (let ((chat-id (plist-get msg :chat_id))
          (msg-id (plist-get msg :id)))
      ;; We use `telega--help-win-param', to make ensure message won't
      ;; change on async requests
      (setq telega--help-win-param msg-id)

      (when (telega-msg-match-p msg 'is-deleted)
        (telega-ins--with-face 'error
          (telega-ins (telega-i18n "lng_deleted_message") "\n")))
      (telega-ins-describe-item (telega-i18n "lng_sent_date" :date "")
        (telega-ins--date (plist-get msg :date) 'date-time))
      (when (telega-msg-match-p msg
              '(and (not is-deleted) (message-property :can_get_read_date)))
        (telega-ins-describe-item "Read Date"
          (telega--getMessageReadDate msg
            (telega--gen-ins-continuation-callback 'loading
              #'telega-ins--message-read-date))))
      (telega-ins-describe-item "Chat-Id"
        (telega-ins-fmt "%d" chat-id))
      (telega-ins-describe-item "Message-Id"
        (telega-ins-fmt "%d" msg-id))
      (let ((thread-id (plist-get msg :message_thread_id)))
        (unless (telega-zerop thread-id)
          (telega-ins-describe-item "Thread-Id"
            (telega-ins-fmt "%d" thread-id))))
      (let ((album-id (plist-get msg :media_album_id)))
        (unless (telega-zerop album-id)
          (telega-ins-describe-item "Media-Album-Id"
            (telega-ins-fmt "%s" album-id))))
      (when-let ((topic (telega-msg-topic msg)))
        (telega-ins-describe-item "Topic"
          (telega-ins--topic-full topic)))

      (when-let ((sender (telega-msg-sender msg)))
        (telega-ins-describe-item "Sender"
          (telega-ins--raw-button
              (telega-link-props 'sender sender 'type 'telega)
            (telega-ins--msg-sender sender
              :with-avatar-p t
              :with-username-p 'telega-username
              :with-brackets-p t))
          (let ((sender-boosts (plist-get msg :sender_boost_count)))
            (unless (telega-zerop sender-boosts)
              (telega-ins-fmt " / %d boosts" sender-boosts)))))

      (when-let ((ignored-by (telega-msg-match-p msg 'ignored)))
        (telega-ins-describe-item "Ignored By"
          (telega-ins-fmt "%S" ignored-by)))

      ;; Link to the message
      (when-let ((link (ignore-errors
                         ;; NOTE: we ignore any errors such as
                         ;;   - error=6: Public message links are available
                         ;;              only for messages in supergroups
                         ;;   - error=6: Message is scheduled
                         ;;   ...
                         (telega--getMessageLink msg
                           :for-thread-p for-thread-p))))
        (telega-ins-describe-item "Link"
          (telega-ins--raw-button (telega-link-props 'url link 'face 'link)
            (telega-ins link))))

      (telega-ins-describe-item "Internal Link"
        (let ((internal-link (telega-tme-internal-link-to msg)))
          (telega-ins--raw-button
              (telega-link-props 'url internal-link 'face 'link)
            (telega-ins internal-link))))

      ;; Custom emoji stickersets
      (when-let* ((custom-emojis
                   (mapcar #'telega-custom-emoji-get
                           (telega-custom-emoji--ids-for-msg msg)))
                  (sset-id-list
                   (seq-uniq (mapcar (telega--tl-prop :set_id) custom-emojis))))
        (telega-ins-describe-item "Custom Emoji"
          (seq-doseq (sset-id (delq nil sset-id-list))
            (telega-stickerset-get sset-id nil
              (telega--gen-ins-continuation-callback 'loading
                (lambda (sset)
                  (telega-ins--box-button (telega-stickerset-title sset)
                    :value sset
                    :action #'telega-describe-stickerset))
                msg-id))
            (telega-ins " "))))

      (when-let ((translated (plist-get msg :telega-translated)))
        (when (with-telega-chatbuf (telega-msg-chat msg)
                telega-translate-replace-content)
          (telega-ins-describe-item "Original Content"
            (telega-ins--column 2 nil
              (telega-ins (telega-msg-content-text msg 'with-speech))))))

      (let ((msg-reactions (telega--tl-get msg :interaction_info :reactions)))
        (cond ((plist-get msg-reactions :can_get_added_reactions)
               (telega-ins-describe-item "Message Reactions"
                 ;; Asynchronously fetch added message reactions
                 (telega--getMessageAddedReactions msg
                   :callback
                   (telega--gen-ins-continuation-callback 'loading
                     (lambda (reply)
                       (let ((added-reactions (plist-get reply :reactions)))
                         (telega-ins-fmt "%d (%d shown)"
                           (plist-get reply :total_count)
                           (length added-reactions))
                         (seq-doseq (ar added-reactions)
                           (telega-ins "\n")
                           (telega-ins--line-wrap-prefix "  "
                             (telega-ins--msg-reaction-type (plist-get ar :type))
                             (telega-ins " ")
                             (telega-ins--msg-sender
                                 (telega-msg-sender (plist-get ar :sender_id))
                               :with-avatar-p t
                               :with-username-p t
                               :with-brackets-p t)
                             (telega-ins--move-to-column 42)
                             (telega-ins " ")
                             (telega-ins--date-relative (plist-get ar :date)))
                           )))
                     msg-id)))
               )

              ((not (seq-empty-p (plist-get msg-reactions :reactions)))
               ;; NOTE: For basicgroups all reactions are listed in the
               ;; interaction info, and `getMessageAddedReactions' is not
               ;; available
               ;; See https://github.com/zevlg/telega.el/issues/460
               (let ((reactions (plist-get msg-reactions :reactions)))
                 (telega-ins-describe-item "Message Reactions"
                   (telega-ins-fmt "%d"
                     (apply #'+ (mapcar (telega--tl-prop :total_count) reactions)))
                   (seq-doseq (reaction reactions)
                     (seq-doseq (sender (plist-get reaction :recent_sender_ids))
                       (telega-ins "\n")
                       (telega-ins--line-wrap-prefix "  "
                         (telega-ins--msg-reaction-type
                          (plist-get reaction :type))
                         (telega-ins " ")
                         (telega-ins--msg-sender (telega-msg-sender sender)
                           :with-avatar-p t
                           :with-username-p t
                           :with-brackets-p t)))))))))

      (when (telega-msg-match-p msg
              '(and (not is-deleted) (message-property :can_get_viewers)))
        (telega-ins-describe-item
            (telega-i18n "lng_stats_overview_message_views")
          ;; Asynchronously fetch message viewers
          (telega--getMessageViewers msg
            (telega--gen-ins-continuation-callback 'loading
              (lambda (viewers)
                (telega-ins-fmt "%d" (length viewers))
                (seq-doseq (viewer viewers)
                  (telega-ins "\n")
                  (telega-ins--line-wrap-prefix "  "
                    (telega-ins--msg-sender
                        (telega-user-get (plist-get viewer :user_id))
                      :with-avatar-p t
                      :with-username-p t
                      :with-brackets-p t)
                    (telega-ins--move-to-column 40)
                    (telega-ins " ")
                    (telega-ins--date-relative (plist-get viewer :view_date)))))
              msg-id))))

      (when-let ((fwd-info (plist-get msg :forward_info)))
        (telega-ins "\n")
        (telega-ins-describe-item (telega-i18n "lng_forwarded" :user "")
          (let ((origin-sender
                 (telega--msg-origin-sender (plist-get fwd-info :origin))))
            (if (stringp origin-sender)
                (telega-ins--with-face 'telega-shadow
                  (telega-ins origin-sender))
              (telega-ins--msg-sender origin-sender
                :with-avatar-p t
                :with-username-p 'telega-username
                :with-brackets-p t)))
          (telega-ins " " (telega-i18n "telega_at") " ")
          (telega-ins--date (plist-get fwd-info :date) 'date-time))
        (when-let ((fwd-src (plist-get fwd-info :source)))
          ;; TODO: insert forward source info
          ))

      (when (and (listp telega-debug) (memq 'info telega-debug))
        (let ((print-length nil))
          (telega-ins "\n")
          (telega-ins-describe-section "DEBUG")
          (telega-ins--with-face 'bold
            (telega-ins "MsgSexp: "))
          (telega-ins-fmt "(telega-msg-get (telega-chat-get %d) %d)\n"
            chat-id msg-id)
          (telega-ins--with-face 'bold
            (telega-ins "Message: "))
          (telega-ins-fmt "%S\n" msg)))
      )))

(defun telega-ignored-messages ()
  "Display all messages that has been ignored."
  (interactive)
  (with-telega-help-win "*Telegram Ignored Messages*"
    (with-telega-buffer-modify
     (dolist (msg (ring-elements telega--ignored-messages-ring))
       (telega-button--insert 'telega-msg msg
         :inserter #'telega-ins--message-with-chat-header)
       (telega-ins "\n"))
     (goto-char (point-min)))))

(defun telega-msg-public-forwards (msg)
  "Display public forwards for the message MSG."
  (interactive (list (telega-msg-at (point))))
  (let* ((reply (telega--getMessagePublicForwards msg))
         (public-forwards (plist-get reply :forwards)))
    (when (seq-empty-p public-forwards)
      (error "telega: No forwardings to public channels for this message"))

    (with-telega-help-win "*Telegram Public Forwards*"
      (telega-ins-describe-item "Total Forwards"
        (telega-ins-fmt "%d" (plist-get reply :total_count))
        (seq-doseq (fwd public-forwards)
          (cl-ecase (telega--tl-type fwd)
            (publicForwardMessage
             (telega-ins "\n")
             (telega-button--insert 'telega-msg (plist-get fwd :message)
               :inserter #'telega-ins--message-with-chat-header))
            (publicForwardStory
             (telega-ins "\n")
             (telega-ins "TODO: forwarded story"))
            ))))))

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
        (telega-ins--with-face 'diff-removed
          (telega-ins "Orig"))
        (telega-ins " message at: ")
        (telega-ins--date (plist-get msg-old :date) 'date-time)
        (telega-ins "\n")

        (telega-ins--with-face 'diff-added
          (telega-ins "Edit"))
        (telega-ins " message at: ")
        (telega-ins--date (plist-get msg-new :edit_date) 'date-time)
        (telega-ins "\n")

        (telega-ins "-- Diff --\n")
        (telega-ins
         (telega-diff-wordwise (telega-ins--as-string
                                (telega-ins--content msg-old))
                               (telega-ins--as-string
                                (telega-ins--content msg-new))
                               'colorize))
        ))))

(defun telega-msg-add-reaction (msg &optional big-p)
  "Interactively add reaction to the message MSG.
If `\\[universal-argument]' is used, reaction with big animation will
be added."
  (interactive (list (telega-msg-for-interactive) current-prefix-arg))

  (let* ((sm-tags-p (telega-msg-match-p msg '(chat saved-messages)))
         (reaction-type
          (when sm-tags-p
            (when-let ((tag (telega-completing-read-saved-messages-tag
                             "Add Tag: " nil 'with-new-tag)))
              (plist-get tag :tag))))
         (msg-av-reactions nil))
    (unless reaction-type
      (setq msg-av-reactions (telega--getMessageAvailableReactions msg))
      (setq reaction-type
            (telega-completing-read-msg-reaction msg
              (if sm-tags-p
                  "New Tag: "
                (format "Add %sReaction: " (if big-p "BIG " "")))
              msg-av-reactions)))

    (cond
     ((null reaction-type)
      (user-error "telega: Can't add reaction to this message"))
     ((equal reaction-type '(:@type "reactionTypePaid"))
      (telega--addPendingPaidMessageReaction msg)
      (if (y-or-n-p-with-timeout "Undo paid reaction? " 5 nil)
          (telega--removePendingPaidMessageReactions msg)
        (telega--commitPendingPaidMessageReactions msg)))
     ((not (eq reaction-type 'custom))
      (telega--addMessageReaction msg reaction-type big-p 'update-recent))
     (t
      ;; Custom reaction
      (let ((top-av-reactions (plist-get msg-av-reactions :top_reactions))
            (recent-av-reactions (plist-get msg-av-reactions :recent_reactions))
            (popular-av-reactions (plist-get msg-av-reactions :popular_reactions))
            (help-window-select t)
            (reaction-type-action
             (lambda (reaction-type)
               (cl-assert (eq major-mode 'help-mode))
               (quit-window 'kill-buffer)
               (telega--addMessageReaction msg reaction-type big-p
                                           'update-recent-reactions))))
        (with-telega-help-win "*Telegram Custom Reaction*"
          (unless (seq-empty-p top-av-reactions)
            (telega-ins--with-face 'telega-describe-item-title
              (telega-ins "TOP:\n"))
            (telega-ins--line-wrap-prefix "  "
              (telega-ins--available-reaction-list
               top-av-reactions reaction-type-action)
              (telega-ins "\n\n")))

          (unless (seq-empty-p recent-av-reactions)
            (telega-ins--with-face 'telega-describe-item-title
              (telega-ins "RECENT:\n"))
            (telega-ins--line-wrap-prefix "  "
              (telega-ins--available-reaction-list
               recent-av-reactions reaction-type-action)
              (telega-ins "\n\n")))

          (unless (seq-empty-p popular-av-reactions)
            (telega-ins--with-face 'telega-describe-item-title
              (telega-ins "POPULAR:\n"))
            (telega-ins--line-wrap-prefix "  "
              (telega-ins--available-reaction-list
               popular-av-reactions reaction-type-action)
              (telega-ins "\n\n")))

          (telega-ins--custom-emoji-stickersets
           (lambda (sticker)
             (cl-assert (telega-custom-emoji-sticker-p sticker))
             (telega--addMessageReaction
              msg (list :@type "reactionTypeCustomEmoji"
                        :custom_emoji_id (telega-custom-emoji-id sticker))
              big-p 'update-recent-reactions)))))))))

(defun telega-msg-translate (msg to-language-code &optional quiet)
  "Translate message at point.
If `\\[universal-argument]' is used, select language to translate message to.
By default `telega-translate-to-language-default' is used."
  (interactive (list (telega-msg-for-interactive)
                     (if (or current-prefix-arg
                             (not telega-translate-to-language-by-default))
                         (telega-completing-read-language-code
                          "Translate to language: ")
                       telega-translate-to-language-by-default)))

  (if (equal (telega--tl-get msg :telega-translated :to_language_code)
             to-language-code)
      ;; NOTE: Cancel translation on subsequent call if called
      ;; interactively, i.e. QUIET is not specified
      (unless quiet
        (plist-put msg :telega-translated nil)
        (telega-msg-redisplay msg))

    (when-let* ((msg-text (telega-msg-content-text msg 'with-voice-note))
                (extra (telega--translateText msg-text to-language-code
                         :callback
                         (lambda (reply)
                           (let ((translated-text (telega-tl-str reply :text)))
                             (plist-put msg :telega-translated
                                        (list :to_language_code to-language-code
                                              :text translated-text))
                             (telega-msg-redisplay msg))))))
      ;; NOTE: Also translate reply_markup buttons if any
      (when-let ((reply-markup (plist-get msg :reply_markup))
                 (reply-markup-type (telega--tl-type reply-markup)))
        (when (eq reply-markup-type 'replyMarkupInlineKeyboard)
          (seq-doseq (row (plist-get reply-markup :rows))
            (seq-doseq (kbd-button row)
              (telega--translateText
                  (telega-tl-str kbd-button :text) to-language-code
                :callback
                (lambda (reply)
                  (let ((translated-text (telega-tl-str reply :text)))
                    (plist-put kbd-button :telega-translated
                               (list :to_language_code to-language-code
                                     :text translated-text))
                    (telega-msg-redisplay msg))))))))

      (plist-put msg :telega-translated
                 (list :to_language_code to-language-code
                       :loading extra))
      (telega-msg-redisplay msg)
      )))

(defun telega-msg-text-spoiler-toggle (msg)
  "Show spoiler text entity at point."
  (interactive (list (telega-msg-for-interactive)))

  (plist-put msg :telega-text-spoiler-removed
             (not (plist-get msg :telega-text-spoiler-removed)))
  (telega-msg-redisplay msg))

(defun telega-msg-blockquote-expand-toggle (msg)
  "Toggle expandable block quote at point."
  (interactive (list (telega-msg-for-interactive)))

  (when-let ((ent (telega--tl-entity-get
                   (get-text-property (point) :tl-entities)
                   'textEntityTypeExpandableBlockQuote)))
    (plist-put ent :telega-blockquote-expanded
               (not (plist-get ent :telega-blockquote-expanded)))
    (telega-msg-redisplay msg)))

(defun telega-msg-media-spoiler-toggle (msg)
  "Toggle spoiler for the media message MSG."
  (interactive (list (telega-msg-for-interactive)))

  (when (plist-get (plist-get msg :content) :has_spoiler)
    (plist-put msg :telega-media-spoiler-removed
               (not (plist-get msg :telega-media-spoiler-removed)))
    (telega-msg-redisplay msg)))

(defun telega-msg-disable-link-preview (msg)
  "Disable webpage preview for the given outgoing message."
  (interactive (list (telega-msg-for-interactive)))
  (unless (telega-msg-match-p msg '(type Text))
    (user-error "telega: can disable link preview only for text messages"))
  (unless (telega-msg-match-p msg '(message-property :can_be_edited))
    (user-error "telega: can't edit this message"))

  (telega--editMessageText
   msg (list :@type "inputMessageText"
             :text
             (telega-fmt-text-desurrogate
              (copy-sequence (telega--tl-get msg :content :text)))
             :link_preview_options
             '(:@type "linkPreviewOptions" :is_disabled t))))

(defun telega-msg--replied-message (msg)
  "Return message on which MSG depends."
  (or (plist-get msg :telega-replied-message)
      (let* ((chat-id (plist-get msg :chat_id))
             (content (plist-get msg :content))
             (replied-msg-id
              (cl-case (telega--tl-type content)
                (messagePinMessage
                 (plist-get content :message_id))
                (messageGameScore
                 (plist-get content :game_message_id))
                (PaymentSuccessful
                 (setq chat-id (plist-get content :invoice_chat_id))
                 (plist-get content :invoice_message_id))
                (t
                 (when-let ((reply-to (plist-get msg :reply_to)))
                   (when-let ((reply-chat-id (plist-get reply-to :chat_id)))
                     (when reply-chat-id
                       (setq chat-id reply-chat-id)))
                   (plist-get reply-to :message_id)))))
             (replied-msg
              (unless (or (null replied-msg-id) (zerop replied-msg-id))
                (gethash (cons chat-id replied-msg-id)
                         telega--cached-messages))))
        (when replied-msg
          (plist-put msg :telega-replied-message replied-msg))
        replied-msg)))

(defun telega-msg--replied-message-fetch (msg)
  "Fetch message on which MSG depends.
Return `loading' if replied message starts loading."
  (when (and (not (telega-msg-internal-p msg))
             (not (telega-msg--replied-message msg))
             (telega-msg-match-p msg
               '(or is-reply-to-msg
                    (type PinMessage
                          GameScore
                          PaymentSuccessful
                          ForumTopicCreated
                          ForumTopicEdited
                          ForumTopicIsClosedToggled
                          ForumTopicIsHiddenToggled))))
    (telega--getRepliedMessage msg
      (apply-partially #'telega-msg--replied-message-fetch-callback msg))
    (plist-put msg :telega-replied-message 'loading)))

(defun telega-msg--replied-message-fetch-callback (msg replied-msg)
  "Callback when the REPLIED-MSG of the MSG is fetched."
  (unless (telega--tl-error-p replied-msg)
    (telega-msg-cache replied-msg))
  (plist-put msg :telega-replied-message replied-msg)
  (telega-msg-redisplay msg)
  ;; NOTE: rootbuf also might be affected
  (telega-root-view--update :on-message-update msg))

(defun telega-msg--replied-story (msg)
  "Return a story message MSG is replying."
  (or (plist-get msg :telega-replied-story)
      (when-let* ((reply-to (plist-get msg :reply_to))
                  (chat-id (plist-get reply-to :story_poster_chat_id))
                  (story-id (plist-get reply-to :story_id))
                  (replied-story (gethash (cons chat-id story-id)
                                          telega--cached-stories)))
        (plist-put msg :telega-replied-story replied-story)
        replied-story)))

(defun telega-msg--replied-story-fetch (msg)
  "Fetch a story message MSG is replying.
Return `loading' if replied story starts loading."
  (when (and (not (telega-msg-internal-p msg))
             (not (telega-msg--replied-story msg))
             (telega-msg-match-p msg 'is-reply-to-story))
    (when-let* ((reply-to (plist-get msg :reply_to))
                (chat-id (plist-get reply-to :story_poster_chat_id))
                (story-id (plist-get reply-to :story_id)))
      (telega--getStory chat-id story-id nil
        (apply-partially #'telega-msg--replied-story-fetch-callback msg))
      (plist-put msg :telega-replied-story 'loading))))

(defun telega-msg--replied-story-fetch-callback (msg replied-story)
  "Callback when the REPLIED-STORY for the MSG has been fetched."
  (plist-put msg :telega-replied-story replied-story)
  (telega-msg-redisplay msg)
  ;; NOTE: rootbuf also might be affected
  (telega-root-view--update :on-story-update replied-story))

(defun telega-msg--message-properties-fetch (msg)
  "Fetch message properties"
  (when (and (not (telega-msg-internal-p msg))
             (not (telega-msg--replied-story msg))
             (telega-msg-match-p msg 'is-reply-to-story))
    (when-let* ((reply-to (plist-get msg :reply_to))
                (chat-id (plist-get reply-to :story_poster_chat_id))
                (story-id (plist-get reply-to :story_id)))
      (telega--getMessageProperties msg
        (lambda (msg-properties)
          (plist-put msg :telega-message-properties msg-properties)
          (telega-msg-redisplay msg)))
      (plist-put msg :telega-message-properties 'loading))))

(defun telega-msg--message-properties-callback (msg msg-properties)
  "Callback when the REPLIED-STORY for the MSG has been fetched."
  (plist-put msg :telega-message-properties msg-properties)
  (telega-msg-redisplay msg))


;;; Preview Messages
(defun telega-msg-preview--buffer-kill ()
  "Kill messages preview buffer."
  (when-let ((pbuf (get-buffer "*Telegram Messages Preview*")))
    (when-let ((win (get-buffer-window pbuf)))
      (delete-window win))
    (kill-buffer pbuf)))

(defun telega-msg-preview--buffer-create ()
  "Create buffer to preview messages."
  (with-telega-help-win "*Telegram Messages Preview*"
    ))

(defun telega-msg-preview--add (msg)
  (with-current-buffer "*Telegram Messages Preview*"
    (with-telega-buffer-modify
     (telega-save-excursion
       (telega-chatbuf-msg--pp msg 'for-preview)))))

(defun telega-msg-preview--add-multiple (messages)
  (seq-doseq (msg (plist-get messages :messages))
    (telega-msg-preview--add msg)))

(defun telega-msg-checklist-task-add (msg)
  "Add task to a checklist message MSG."
  (interactive (list (telega-msg-for-interactive)))
  (let ((checklist (telega--tl-get msg :content :list)))
    (cl-assert checklist)
    (telega--addChecklistTasks
     msg (list :@type "inputChecklistTask"
               :id (1+ (apply #'max (mapcar (telega--tl-prop :id)
                                            (plist-get checklist :tasks))))
               :text (telega-string-fmt-text
                      (telega-read-checklist-task))))
    ))

(defun telega-msg-checklist-task-toggle (msg)
  "Toggle checklist message's MSG task at point."
  (interactive (list (telega-msg-for-interactive)))
  (let ((task (get-text-property (point) :checklist-task)))
    (unless task
      (error "telega: No checklist task at point"))
    (let ((task-id (plist-get task :id))
          (task-done-p (not (telega-zerop (plist-get task :completion_date)))))
      (telega--markChecklistTasksAsDone
       msg (unless task-done-p (list task-id))
       (when task-done-p (list task-id))))
    ))

(defun telega-msg-checklist-task-edit (msg)
  "Edit checklist message's MSG task at point."
  (interactive (list (telega-msg-for-interactive)))
  (let ((edit-task (get-text-property (point) :checklist-task)))
    (unless edit-task
      (error "telega: No checklist task at point"))

    (let* ((txt (read-string "Edit task: " (telega-tl-str edit-task :text)))
           (checklist (telega--tl-get msg :content :list))
           (input-tasks
            (cl-map #'vector
                    (lambda (task)
                      (list :@type "inputChecklistTask"
                            :id (plist-get task :id)
                            :text (if (eq (plist-get task :id)
                                          (plist-get edit-task :id))
                                      (telega-string-fmt-text txt)
                                    (plist-get task :text))))
                    (plist-get checklist :tasks))))
      (telega--editMessageChecklist
       msg (list :@type "inputChecklist"
                 :title (plist-get checklist :title)
                 :tasks input-tasks
                 :others_can_add_tasks
                 (plist-get checklist :others_can_add_tasks)
                 :others_can_mark_tasks_as_done
                 (plist-get checklist ::others_can_mark_tasks_as_done)))
      )))


;;; Sponsored messages
(defvar telega-sponsored-msg-button-menu-map
  (let ((menu-map (make-sparse-keymap "Telega Sponsored Message")))
    (bindings--define-key menu-map [hide]
      '(menu-item (telega-i18n "lng_sponsored_hide_ads")
                  telega-sponsored-msg-hide
                  :help "Hide sponsored message"))
    (bindings--define-key menu-map [s1] menu-bar-separator)
    (bindings--define-key menu-map [describe]
      '(menu-item (telega-i18n "lng_info_about_label")
                  telega-describe-sponsored-message
                  :help "Describe the sponsored message"))
    (bindings--define-key menu-map [what]
      '(menu-item (telega-i18n "lng_sponsored_title")
                  telega-sponsored-messages-about
                  :help "Describe what sponsored messages are"))
    menu-map))

(defvar telega-sponsored-msg-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)

    (define-key map (kbd "i") 'telega-describe-sponsored-message)

    ;; Menu for right mouse on a message
    (define-key map [down-mouse-3] telega-sponsored-msg-button-menu-map)
    (define-key map [mouse-3] #'ignore)

    map))

(define-button-type 'telega-sponsored-msg
  :supertype 'telega
  :inserter telega-inserter-for-sponsored-msg-button

  ;; NOTE: To make input method works under message buttons,
  ;; See `quail-input-method' for details
  'read-only t
  'front-sticky t

  'keymap telega-sponsored-msg-button-map
  'action 'telega-sponsored-msg--action)

(defun telega-sponsored-msg-at-down-mouse-3 ()
  "Return sponsored message at down-mouse-3 press.
Return nil if there is no `down-mouse-3' keys in `this-command-keys'."
  (when-let* ((ev-key (assq 'down-mouse-3 (append (this-command-keys) nil)))
              (ev-start (cadr ev-key))
              (ev-point (posn-point ev-start)))
    (telega-sponsored-msg-at ev-point)))

(defun telega-sponsored-msg-at (&optional pos)
  "Return current sponsored message at POS point.
If POS is ommited, then return massage at current point.
For interactive commands acting on message at point/mouse-event
use `telega-sponsored-msg-for-interactive' instead."
  (when-let* ((button (button-at (or pos (point))))
              (value (button-get button :value)))
    (when (and button (eq (button-type button) 'telega-sponsored-msg))
      (button-get button :value))))

(defun telega-sponsored-msg-for-interactive ()
  "Return sponsored message at mouse event or at current point."
  (when-let ((msg (or (telega-sponsored-msg-at-down-mouse-3)
                      (telega-sponsored-msg-at (point)))))
    msg))

(defun telega-sponsored-msg--action (button)
  "Open sponsored message SPONSORED-MSG."
  (let ((sponsored-msg (telega-sponsored-msg-at button)))
    (telega--clickChatSponsoredMessage telega-chatbuf--chat sponsored-msg)
    (let ((sponsor (plist-get sponsored-msg :sponsor)))
      (telega-browse-url (plist-get sponsor :url)))))

(defun telega-sponsored-msg-hide (sponsored-msg)
  "Hide SPONSORED-MSG in the chatbuf footer."
  (interactive (list (telega-sponsored-msg-for-interactive)))
  (plist-put sponsored-msg :telega-hidden t)
  (telega-chatbuf--chat-update "sponsored-messages")
  ;; TODO: remove message's node
  ;(telega-msg-redisplay sponsored-msg)
  )

(defun telega-describe-sponsored-message (sponsored-msg)
  "Show info about SPONSORED-MESSAGE at point."
  (interactive (list (telega-sponsored-msg-for-interactive)))
  (with-telega-help-win "*Telegram Sponsor Info*"
    (telega-ins-describe-item "Id"
      (telega-ins-fmt "%d" (plist-get sponsored-msg :message_id)))
    (let ((sponsor (plist-get sponsored-msg :sponsor)))
      (when-let ((sponsor-info (telega-tl-str sponsor :info)))
        (telega-ins-describe-item "Sponsor Info"
          (telega-ins--line-wrap-prefix "      "
            (telega-ins sponsor-info))))
      (telega-ins-describe-item "Sponsor URL"
        (telega-ins (telega-tl-str sponsor :url))))
    (when-let ((add-info (telega-tl-str sponsored-msg :additional_info)))
      (telega-ins-describe-item "Additional Info"
        (telega-ins add-info)))

    (when (and (listp telega-debug) (memq 'info telega-debug))
      (let ((print-length nil))
        (telega-ins "\n---DEBUG---\n")
        (telega-ins-fmt "SponsoredMessage: %S\n" sponsored-msg)
        ))
    ))

(defun telega-sponsored-messages-about ()
  (interactive)
  (let ((help-window-select t))
    (with-telega-help-win "*Telegram Sponsored Mesagges*"
      (telega-ins-i18n "lng_sponsored_info_description1"))))


(provide 'telega-msg)

;; Load favorite messages
(add-hook 'telega-chats-fetched-hook #'telega-msg--favorite-messages-file-fetch)

;;; telega-msg.el ends here

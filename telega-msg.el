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

(declare-function telega-browse-url "telega-webpage" (url &optional in-web-browser))


;; Menu for right-mouse on message
(defvar telega-msg-button-menu-map
  (let ((menu-map (make-sparse-keymap "Telega Message")))
    (bindings--define-key menu-map [mark]
      '(menu-item "Mark" telega-msg-mark-toggle
                  :help "Mark the message"
                  :visible (not (telega-msg-marked-p
                                 (telega-msg-at-down-mouse-3)))))
    (bindings--define-key menu-map [unmark]
      '(menu-item "Unmark" telega-msg-mark-toggle
                  :help "Unmark the message"
                  :visible (telega-msg-marked-p (telega-msg-at-down-mouse-3))))
    (bindings--define-key menu-map [s0] menu-bar-separator)
    (bindings--define-key menu-map [add-favorite]
      '(menu-item "Add to Favorites" telega-msg-favorite-toggle
                  :help "Add message to the list of favorite messages"
                  :visible (not (telega-msg-favorite-p
                                 (telega-msg-at-down-mouse-3)))))
    (bindings--define-key menu-map [rm-favorite]
      '(menu-item "Remove from Favorites" telega-msg-favorite-toggle
                  :help "Remove message from the list of favorite messages"
                  :visible (telega-msg-favorite-p (telega-msg-at-down-mouse-3))))

    (bindings--define-key menu-map [save]
      '(menu-item (telega-i18n "lng_context_save_file") telega-msg-save
                  :help "Save message's media to a file"))
    (bindings--define-key menu-map [copy-link]
      '(menu-item (telega-i18n "lng_context_copy_link") telega-msg-copy-link
                  :help "Copy link to the message to the kill ring"))
    (bindings--define-key menu-map [copy-text]
      '(menu-item (telega-i18n "lng_context_copy_text") telega-msg-copy-text
                  :visible (let ((msg (telega-msg-at-down-mouse-3)))
                             (telega-msg-content-text msg 'with-voice-note))
                  :help "Copy message text to the kill ring"))
    (bindings--define-key menu-map [unpin]
      '(menu-item (telega-i18n "lng_context_unpin_msg") telega-msg-pin-toggle
                  :help "Unpin message"
                  :visible (let ((msg (telega-msg-at-down-mouse-3)))
                             (and (telega-chat-match-p (telega-msg-chat msg)
                                    '(my-permission :can_pin_messages))
                                  (plist-get msg :is_pinned)))))
    (bindings--define-key menu-map [pin]
      '(menu-item (telega-i18n "lng_context_pin_msg") telega-msg-pin-toggle
                  :help "Pin message"
                  :visible (let ((msg (telega-msg-at-down-mouse-3)))
                             (and (telega-chat-match-p (telega-msg-chat msg)
                                    '(my-permission :can_pin_messages))
                                  (not (plist-get msg :is_pinned))))))
    (bindings--define-key menu-map [s1] menu-bar-separator)
    (bindings--define-key menu-map [ban-sender]
      '(menu-item (propertize "Ban Sender" 'face 'error)
                  telega-msg-ban-sender
                  :help "Ban/report message sender"
                  :enable (let ((msg (telega-msg-at-down-mouse-3)))
                             (telega-chat-match-p (telega-msg-chat msg)
                               '(my-permission :can_restrict_members)))
                  ))
    (bindings--define-key menu-map [delete]
      '(menu-item (propertize (telega-i18n "lng_context_delete_msg") 'face 'error)
                  telega-msg-delete-marked-or-at-point
                  :help "Delete message"
                  :enable (let ((msg (telega-msg-at-down-mouse-3)))
                            (or (plist-get msg :can_be_deleted_only_for_self)
                                (plist-get msg :can_be_deleted_for_all_users)))
                  ))
    (bindings--define-key menu-map [forward]
      '(menu-item (telega-i18n "lng_context_forward_msg")
                  telega-msg-forward-marked-or-at-point
                  :help "Forward a message"
                  :enable (let ((msg (telega-msg-at-down-mouse-3)))
                            (plist-get msg :can_be_forwarded))
                  ))
    (bindings--define-key menu-map [forward]
      '(menu-item (telega-i18n "lng_reply_in_another_chat")
                  telega-msg-reply-in-another-chat
                  :help (telega-i18n "lng_reply_in_another_chat")
                  :enable (let ((msg (telega-msg-at-down-mouse-3)))
                            (plist-get msg :can_be_replied_in_another_chat))
                  ))
    (bindings--define-key menu-map [s2] menu-bar-separator)
    (bindings--define-key menu-map [topic]
      '(menu-item (telega-i18n "lng_replies_view_topic")
                  telega-msg-open-thread-or-topic
                  :help "Show message's topic"
                  :visible (telega-msg-match-p (telega-msg-at-down-mouse-3)
                             'is-topic)))
    (bindings--define-key menu-map [thread]
      '(menu-item (telega-i18n "lng_replies_view_thread")
                  telega-msg-open-thread-or-topic
                  :help "Show message's thread"
                  :visible (telega-msg-match-p (telega-msg-at-down-mouse-3)
                             'is-thread)))
    (bindings--define-key menu-map [translate]
      '(menu-item (telega-i18n "lng_context_translate") telega-msg-translate
                  :help "Translate message's text"
                  :visible (telega-msg-content-text
                            (telega-msg-at-down-mouse-3))))
    (bindings--define-key menu-map [edit]
      '(menu-item (telega-i18n "lng_context_edit_msg") telega-msg-edit
                  :help "Edit the message"
                  :enable (plist-get
                           (telega-msg-at-down-mouse-3) :can_be_edited)))
    (bindings--define-key menu-map [reply]
      '(menu-item (telega-i18n "lng_context_reply_msg") telega-msg-reply
                  :help "Reply to the message"))
    (bindings--define-key menu-map [describe]
      '(menu-item "Describe" telega-describe-message
                  :help "Describe the message"))
    menu-map))

(defvar telega-msg-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
;    (define-key map [remap self-insert-command] 'ignore)

    (define-key map (kbd "SPC") 'scroll-up-command)
    (define-key map (kbd "c") 'telega-msg-copy-text)
    (define-key map (kbd "d") 'telega-msg-delete-marked-or-at-point)
    (define-key map (kbd "e") 'telega-msg-edit)
    (define-key map (kbd "f") 'telega-msg-forward-marked-or-at-point)
    (define-key map (kbd "i") 'telega-describe-message)
    (define-key map (kbd "l") 'telega-msg-copy-link)
    ;; Marking, `telega-msg-forward' and `telega-msg-delete' can work
    ;; on list of marked messages
    (define-key map (kbd "m") 'telega-msg-mark-toggle)
    (define-key map (kbd "n") 'telega-button-forward)
    (define-key map (kbd "<tab>") 'telega-button-forward)
    (define-key map (kbd "p") 'telega-button-backward)
    (define-key map (kbd "<backtab>") 'telega-button-backward)
    (define-key map (kbd "r") 'telega-msg-reply)
    (define-key map (kbd "t") 'telega-msg-translate)

    (define-key map (kbd "B") 'telega-msg-ban-sender)
    (define-key map (kbd "F") 'telega-msg-forward-marked-or-at-point-to-multiple-chats)
    (define-key map (kbd "L") 'telega-msg-redisplay)
    (define-key map (kbd "P") 'telega-msg-pin-toggle)
    (define-key map (kbd "R") 'telega-msg-resend)
    (define-key map (kbd "S") 'telega-msg-save)
    (define-key map (kbd "T") 'telega-msg-open-thread-or-topic)
    (define-key map (kbd "U") 'telega-chatbuf-msg-marks-toggle)

    (define-key map (kbd "!") 'telega-msg-add-reaction)
    (define-key map (kbd "=") 'telega-msg-diff-edits)
    (define-key map (kbd "^") 'telega-msg-pin-toggle)
    (define-key map (kbd "DEL") 'telega-msg-delete-marked-or-at-point)

    (define-key map (kbd "*") 'telega-msg-favorite-toggle)

    ;; Menu for right mouse on a message
    (define-key map [down-mouse-3] telega-msg-button-menu-map)
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

(defun telega-msg-create-internal (chat fmt-text)
  "Create message for internal use.
Used to add content to chatbuf that is not a regular message.
FMT-TEXT is formatted text, can be created with `telega-fmt-text'."
  (list :@type "message"
        :id -1
        :chat_id (plist-get chat :id)
        :content (list :@type "telegaInternal"
                       :text fmt-text)))

(defun telega-msg-p (obj)
  "Return non-nil if OBJ is a message object."
  (and (listp obj) (equal "message" (plist-get obj :@type))))

(defun telega-msg-internal-p (msg)
  "Return non-nil if MSG is internal, created with `telega-msg-create-internal'."
  (eq -1 (plist-get msg :id)))

(defun telega-msg-get (chat msg-id &optional callback)
  "Get message by CHAT-ID and MSG-ID pair.
If CALLBACK is not specified, then do not perform request to
telega-server, check only in messages cache.  If CALLBACK
is specified, it should accept two argument s - MESSAGE and
optional OFFLINE-P, non-nil OFFLINE-P means no request to the
telega-server has been made."
  (declare (indent 2))
  ;; - Search in the messages cache
  ;; - [DON'T] Search in chatbuf messages, because message could be
  ;;           offloaded from the cache and will be outdated
  ;; - [DON'T] Check pinned messages in the chatbuf
  (let* ((chat-id (plist-get chat :id))
         (msg (gethash (cons chat-id msg-id) telega--cached-messages)))
    (if (or msg (null callback))
        (if callback
            (funcall callback msg 'offline-p)
          msg)

      (cl-assert callback)
      (telega--getMessage chat-id msg-id callback)
      nil)))

(defun telega-msg-at-down-mouse-3 ()
  "Return message at down-mouse-3 press.
Return nil if there is no `down-mouse-3' keys in `this-command-keys'."
  (when-let* ((ev-key (assq 'down-mouse-3 (append (this-command-keys) nil)))
              (ev-start (cadr ev-key))
              (ev-point (posn-point ev-start)))
    (telega-msg-at ev-point)))

(defun telega-msg-at (&optional pos)
  "Return current message at POS point.
If POS is ommited, then return massage at current point.
For interactive commands acting on message at point/mouse-event
use `telega-msg-for-interactive' instead."
  (let ((button (button-at (or pos (point)))))
    (when (and button (eq (button-type button) 'telega-msg))
      (button-get button :value))))

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
                      (telega--tl-get msg :interaction_info :reactions))))

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

(defun telega-msg-goto (msg &optional highlight)
  "Goto message MSG."
  (telega-chat--goto-msg
      (telega-msg-chat msg) (plist-get msg :id) highlight))

(defun telega-msg-goto-highlight (msg)
  "Goto message MSG and highlight it."
  (telega-msg-goto msg 'highlight))

(defun telega-msg-goto-reply-to-message (msg)
  "Goto message denoted by `:reply_to' field of the message MSG."
  (let* ((reply-to (plist-get msg :reply_to))
         (chat-id (plist-get reply-to :chat_id))
         (msg-id (plist-get reply-to :message_id)))
    (unless (or (telega-zerop chat-id) (telega-zerop msg-id))
      (telega-chat--goto-msg (telega-chat-get chat-id) msg-id 'highlight))))

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
  (let ((sticker (telega--tl-get msg :content :animated_emoji :sticker)))
    ;; NOTE: animated message could be a static sticker, such as in
    ;; the https://t.me/tgbetachat/1319907
    (when (and (not (telega-sticker-static-p sticker))
               telega-sticker-animated-play)
      (telega-sticker--animate sticker msg))

    ;; Try fullscreen animated emoji sticker as well
    (when clicked-p
      (telega--clickAnimatedEmojiMessage msg
        (lambda (fs-sticker)
          (when (and (not (telega--tl-error-p fs-sticker))
                     (not (telega-sticker-static-p fs-sticker))
                     telega-sticker-animated-play)
            (plist-put msg :telega-sticker-fullscreen fs-sticker)
            (telega-sticker--animate fs-sticker msg)))))
    ))

(defun telega-msg-open-story (msg)
  "Open content for the forwarded story message MSG."
  (let* ((content (plist-get msg :content))
         (chat-id (plist-get content :story_sender_chat_id))
         (story-id (plist-get content :story_id))
         (story (telega-story-get chat-id story-id)))
    (telega-story-open story msg)))

(defun telega-msg--play-video (msg file &optional done-callback)
  "Start playing video FILE for MSG."
  (declare (indent 2))
  (if (memq 'video telega-open-message-as-file)
      (progn
        (cl-assert (telega-file--downloaded-p file))
        (telega-open-file (telega--tl-get file :local :path) msg))

    (telega-video-player-run
     (telega--tl-get file :local :path) msg done-callback)))

(defun telega-msg--play-video-incrementally (msg _file)
  "Start playing video FILE while still downloading FILE."
  ;; NOTE: search `moov' atom at the beginning or ending
  ;; Considering 2k of index data per second
  (let* ((video (or (telega--tl-get msg :content :video)
                    (telega--tl-get msg :content :web_page :video)))
         (video-file (telega-file--renew video :video)))
    (if (and (telega-file--downloaded-p video-file)
             (plist-get video :telega-video-pending-open))
        (telega-msg--play-video msg video-file)

      ;; File is partially downloaded, start playing incrementally if:
      ;; 1) At least 5 seconds of the video is downloaded
      ;; 2) `moov' atom is available, we use
      ;;    `telega-ffplay-get-resolution' function to check this
      ;; 3) File downloads faster, than it takes time to play
      (let* ((fsize (telega-file--size video-file))
             (dsize (telega-file--downloaded-size video-file))
             (duration (or (plist-get video :duration) 50))
             (probe-size (plist-get video :telega-video-probe-size))
             (open-time (plist-get video :telega-video-pending-open))
             ;; Size for 10 seconds of the video
             (d5-size (/ (* 10 fsize) duration))
             ;; Downloaded duration
             (ddur (* duration (/ (float (- dsize (min dsize (or probe-size 0))))
                                  fsize))))
        (when (and ;; Check for 1)
                   probe-size (> dsize (+ probe-size d5-size))
                   ;; Check for 3)
                   open-time (> ddur (- (time-to-seconds) open-time)))
          ;; Check for 2)
          (if (not (telega-ffplay-get-resolution
                    (telega--tl-get video-file :local :path)))
              ;; NOTE: Can't play this file incrementally, so start
              ;; playing it after full download
              (plist-put video :telega-video-probe-size nil)

            ;; NOTE: By canceling downloading process we fix
            ;; filename, so file won't be updated at video player start
            ;; time.  After video player is started, we continue
            ;; downloading process, canceling it only on video player
            ;; exit
            (plist-put video :telega-video-pending-open nil)
            (telega--cancelDownloadFile video-file nil
              (lambda (_ignored)
                (let ((vfile (telega-file-get (plist-get video-file :id) 'local)))
                  (telega-msg--play-video msg vfile
                    (lambda ()
                      (telega--cancelDownloadFile vfile)))
                  ;; Continue downloading file in 0.5 seconds, giving
                  ;; time for video player command to run
                  (run-with-timer 0.5 nil #'telega-file--download vfile 32
                                  (lambda (_ignored)
                                    (telega-msg-redisplay msg))))))))))))

(defun telega-msg-open-video (msg &optional video)
  "Open content for video message MSG."
  (cl-assert (or (and msg (not video))
                 (eq video (telega--tl-get msg :content :web_page :video))))

  (let* ((video (or video (telega--tl-get msg :content :video)))
         (video-file (telega-file--renew video :video))
         (incremental-part
          (and telega-video-play-incrementally
               (plist-get video :supports_streaming)
               (not (memq 'video telega-open-message-as-file))
               ;; Store moov-size at the end to download
               (let ((moov-size (* 2 1024 (or (plist-get video :duration) 50)))
                     (vsize (telega-file--size video-file)))
               (when (> vsize moov-size)
                 (cons (- vsize moov-size) moov-size))))))
    (when (telega--tl-get msg :content :is_secret)
      (telega--openMessageContent msg))

    (cond ((telega-file--downloaded-p video-file)
           (telega-msg--play-video msg video-file))

          (incremental-part
           ;; Play video incrementally
           (plist-put video :telega-video-pending-open (time-to-seconds))
           (plist-put video :telega-video-probe-size (cdr incremental-part))
           (telega-file--download video-file 32
             (lambda (dfile)
               (telega-msg-redisplay msg)
               (telega-msg--play-video-incrementally msg dfile))
             ;; download parts: end and then from the beginning
             incremental-part nil))

          (t
           (telega-file--download video-file 32
             (lambda (file)
               (telega-msg-redisplay msg)
               (when (telega-file--downloaded-p file)
                 (telega-msg--play-video msg file))))))))

(defun telega-msg-open-audio (msg &optional audio)
  "Open content for audio message MSG."
  (cl-assert (or (not audio)
                 (eq audio (telega--tl-get msg :content :web_page :audio))))

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
      (telega-file--download audio-file 32
        (lambda (file)
          (telega-msg-redisplay msg)
          (when (telega-file--downloaded-p file)
            (if (memq 'audio telega-open-message-as-file)
                (telega-open-file (telega--tl-get file :local :path) msg)
              (plist-put msg :telega-ffplay-proc
                         (telega-ffplay-run (telega--tl-get file :local :path)
                             (concat
                              (when paused-p
                                (format "-ss %.2f " paused-p))
                              (cdr (assq 'audio telega-open-message-ffplay-args)))
                           (lambda (_proc)
                             (telega-msg-redisplay msg)))))))))))

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
                   (telega--tl-get msg :content :web_page :voice_note)))
         (note-file (progn
                      (cl-assert note)
                      (telega-file--renew note :voice)))
         (proc (plist-get msg :telega-ffplay-proc))
         (paused-p (or telega-ffplay-media-timestamp
                       (telega-ffplay-paused-p proc))))
    (if (telega-ffplay-playing-p proc)
        (telega-ffplay-pause proc)
      ;; Start playing or resume from the paused moment
      (telega-file--download note-file 32
        (lambda (file)
          (cond
           ((not (telega-file--downloaded-p file))
            ;; no-op
            )

           ((memq 'voice-note telega-open-message-as-file)
            (telega-open-file (telega--tl-get file :local :path) msg))

           (t
            ;; NOTE: Set moment we resumed for `:progress' correction
            ;; in the `telega-msg-voice-note--ffplay-callback'
            (plist-put msg :telega-ffplay-resumed-at paused-p)
            (plist-put msg :telega-ffplay-proc
                       (telega-ffplay-run (telega--tl-get file :local :path)
                           (concat
                            (when paused-p
                              (format "-ss %.2f " paused-p))
                            (unless (equal telega-vvnote-play-speed 1)
                              (format "-af atempo=%.2f "
                                      telega-vvnote-play-speed))
                            (cdr (assq 'voice-note
                                       telega-open-message-ffplay-args)))
                         (lambda (proc)
                           (telega-msg-voice-note--ffplay-callback proc msg))))
            (with-telega-chatbuf (telega-msg-chat msg)
              (telega-chatbuf--activate-vvnote-msg msg))))

          ;; NOTE: always redisplay the message to actualize
          ;; downloading progress
          (telega-msg-redisplay msg))))))

(defun telega-msg-video-note--ffplay-callback (proc frame msg)
  "Callback for video note playback."
  (let* ((proc-plist (process-plist proc))
         (note (or (telega--tl-get msg :content :video_note)
                   (telega--tl-get msg :content :web_page :video_note)))
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
                                      (if (telega-ffplay-paused-p proc)
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

(defun telega-msg-open-video-note (msg &optional video-note)
  "Open content for videoNote message MSG.
If called with `\\[universal-argument]' prefix, then open with
external player even if `telega-video-note-play-inline' is
non-nil."
  (let* ((note (or video-note (telega--tl-get msg :content :video_note)))
         (note-file (telega-file--renew note :video))
         (proc (plist-get msg :telega-ffplay-proc))
         (paused-p (or telega-ffplay-media-timestamp
                       (telega-ffplay-paused-p proc)))
         (saved-this-command this-command)
         (saved-current-prefix-arg current-prefix-arg))
    (if (telega-ffplay-playing-p proc)
        (telega-ffplay-pause proc)
      (telega-file--download note-file 32
        (lambda (file)
          (cond
           ((not (telega-file--downloaded-p file))
            ;; no-op
            )

           ((memq 'video-note telega-open-message-as-file)
            (telega-open-file (telega--tl-get file :local :path) msg))

           ((and telega-video-note-play-inline
                 ;; *NOT* called interactively
                 (or (not saved-this-command)
                     (not saved-current-prefix-arg)))
            ;; NOTE: Set moment we resumed for `:progress' correction
            ;; in the `telega-msg-video-note--ffplay-callback'
            (plist-put msg :telega-ffplay-resumed-at paused-p)
            (plist-put msg :telega-ffplay-proc
                       (telega-ffplay-to-png
                           (telega--tl-get file :local :path)
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
            (telega-ffplay-run (telega--tl-get file :local :path)
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
  (let* ((anim (or animation (telega--tl-get msg :content :animation)))
         (anim-file (telega-file--renew anim :animation))
         (proc (plist-get msg :telega-ffplay-proc))
         (saved-this-command this-command)
         (saved-current-prefix-arg current-prefix-arg))
    (if (telega-ffplay-playing-p proc)
        (telega-ffplay-stop proc)
      (telega-file--download anim-file 32
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
                           (telega--tl-get file :local :path) "-an"
                         (list #'telega-animation--ffplay-callback anim))))

           ((memq 'animation telega-open-message-as-file)
            (telega-open-file (telega--tl-get file :local :path) msg))

           (t
            (telega-ffplay-run (telega--tl-get file :local :path)
                (cdr (assq 'animation telega-open-message-ffplay-args)))))

          ;; NOTE: always redisplay the message to actualize
          ;; downloading progress
          (telega-msg-redisplay msg))))))

(defun telega-msg-open-document (msg &optional document)
  "Open content for document message MSG."
  (let* ((doc (or document (telega--tl-get msg :content :document)))
         (doc-file (telega-file--renew doc :document)))
    (telega-file--download doc-file 32
      (lambda (file)
        (telega-msg-redisplay msg)
        (when (telega-file--downloaded-p file)
          (telega-open-file (telega--tl-get file :local :path) msg))))))

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
  (cond ((plist-get web-page :animation)
         (telega-msg-open-animation msg (plist-get web-page :animation)))
        ((plist-get web-page :audio)
         (telega-msg-open-audio msg (plist-get web-page :audio)))
        ((plist-get web-page :document)
         (telega-msg-open-document msg (plist-get web-page :document)))
        ((plist-get web-page :sticker)
         (telega-msg-open-sticker msg (plist-get web-page :sticker)))
        ((plist-get web-page :video)
         (telega-msg-open-video msg (plist-get web-page :video)))
        ((plist-get web-page :video_note)
         (telega-msg-open-video-note msg (plist-get web-page :video_note)))
        ((and (string= "photo" (plist-get web-page :type))
              (plist-get web-page :photo))
         (telega-msg-open-photo msg (plist-get web-page :photo)))
        (t (when-let ((url (telega-tl-str web-page :url)))
             (telega-browse-url url)))))

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
          (telega-ins (plist-get poll :question))
          (telega-ins " (" (telega-i18n "lng_polls_votes_count"
                             :count (plist-get poll :total_voter_count))
                      ")"))
        (telega-ins "\n")
        ;; Quiz explanation goes next
        (when-let ((explanation
                    (telega-tl-str (plist-get poll :type) :explanation))
                   (label (propertize
                           (concat (telega-i18n "lng_polls_solution_title") ": ")
                           'face 'telega-shadow)))
          (telega-ins--labeled label nil
            (telega-ins explanation)
            (telega-ins "\n")))
        (telega-ins "\n")

        (let ((options (append (plist-get poll :options) nil)))
          (dotimes (popt-id (length options))
            (let ((popt (nth popt-id options)))
              (telega-ins-fmt "%s — %d%% (%s)\n"
                (upcase (telega-tl-str popt :text))
                (plist-get popt :vote_percentage)
                (telega-i18n "lng_polls_votes_count"
                  :count (plist-get popt :voter_count)))
              (when-let* ((voters-reply (telega--getPollVoters msg popt-id))
                          (voters (mapcar #'telega-msg-sender
                                          (plist-get voters-reply :senders))))
                (telega-ins--user-list voters)
                (telega-ins "\n"))
              (telega-ins "\n")
            )))
    ))))

(defun telega-msg-open-sponsored (sponsored-msg)
  "Open sponsored message SPONSORED-MSG."
  (if-let ((tdlib-link (plist-get sponsored-msg :link)))
      (telega-tme-open-tdlib-link tdlib-link)

    (when-let ((schat (telega-chat-get
                       (plist-get sponsored-msg :sponsor_chat_id) t)))
      (telega-chat--pop-to-buffer schat))))

(defun telega-describe--giveaway-info (msg ga-info)
  (with-telega-help-win "*Telegram Giveaway Info*"
    (let* ((content (plist-get msg :content))
           (ga-params (plist-get content :parameters))
           (boosted-chat (telega-chat-get
                          (plist-get ga-params :boosted_chat_id))))
      (cl-ecase (telega--tl-type ga-info)
        (premiumGiveawayInfoCompleted
         (telega-ins--with-face 'bold
           (telega-ins (telega-i18n "lng_prizes_end_title")))
         (telega-ins "\n\n")
         )

        (premiumGiveawayInfoOngoing
         (telega-ins--with-face 'bold
           (telega-ins (telega-i18n "lng_prizes_how_title")))
         (when (plist-get ga-info :is_ended)
           (telega-ins--with-face 'telega-shadow
             (telega-ins " (" (telega-i18n "lng_prizes_end_title") ")")))
         (telega-ins "\n\n")

         (telega-ins
          (telega-i18n "lng_prizes_how_text"
            :admins (telega-i18n "lng_prizes_admins"
                      :channel (telega-ins--as-string
                                (telega-ins--raw-button
                                    (telega-link-props 'sender boosted-chat
                                                       'type 'telega)
                                  (telega-ins--msg-sender boosted-chat
                                    :with-avatar-p t
                                    :with-username-p t
                                    :with-brackets-p t)))
                      :count (plist-get content :winner_count)
                      :duration (telega-i18n "lng_premium_gift_duration_months"
                                  :count (plist-get content :month_count)))))
         (telega-ins "\n\n")
         (telega-ins-i18n "lng_prizes_how_when_finish"
           :date (telega-ins--as-string
                  (telega-ins--date-iso8601
                   (plist-get ga-params :winners_selection_date)))
           :winners (if (plist-get ga-params :only_new_members)
                        (telega-i18n "lng_prizes_winners_new_of_one"
                          :count (plist-get content :winner_count)
                          :channel (telega-ins--as-string
                                    (telega-ins--raw-button
                                        (telega-link-props 'sender boosted-chat
                                                           'type 'telega)
                                      (telega-ins--msg-sender boosted-chat
                                        :with-avatar-p t
                                        :with-username-p t
                                        :with-brackets-p t)))
                          :start_date (telega-ins--as-string
                                       (telega-ins--date-iso8601
                                        (plist-get ga-info :creation_date))))
                      ;; TODO
                      ""))
         (telega-ins "\n\n")

         (let ((status (plist-get ga-info :status)))
           (cl-ecase (telega--tl-type status)
             (premiumGiveawayParticipantStatusDisallowedCountry
              )
             (premiumGiveawayParticipantStatusAdministrator
              )
             (premiumGiveawayParticipantStatusAlreadyWasMember
              )
             (premiumGiveawayParticipantStatusParticipating
              )
             (premiumGiveawayParticipantStatusEligible
              )))
         )))

    (fill-region (point-min) (point-max) 'center)
    ))

(defun telega-msg-open-premium-giveaway (msg)
  "Open show more details about premium giveaway message MSG."
  (message "telega: Fetching giveaway info...")
  (telega--getPremiumGiveawayInfo msg
    (lambda (result)
      (message "")
      (telega-describe--giveaway-info msg result))))

(defun telega-msg-emojis-only-p (msg)
  "Return non-nil if text message MSG contains only emojis."
  (when (telega-msg-match-p msg '(type Text))
    (let ((text (telega--tl-get msg :content :text :text)))
      (not (text-property-not-all
            0 (length text) 'telega-emoji-p t text)))))

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
     (when-let ((web-page (telega--tl-get msg :content :web_page)))
       (telega-msg-open-webpage msg web-page)))
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
    (messagePremiumGiveaway
     (telega-msg-open-premium-giveaway msg))

    (t (message "TODO: `open-content' for <%S>"
                (telega--tl-type (plist-get msg :content))))))

(defun telega-msg-open-thread-or-topic (msg)
  "View message MSG in the topic or thread.
MSG could be a channel post, in this case open thread in the
discussion group.
Or MSG could be in supergroup, then filter messages to the
corresponding thread or topic."
  (interactive (list (telega-msg-for-interactive)))
  (cond ((telega-msg-match-p msg 'is-topic)
         (telega-topic-goto
          (telega-topic-get (telega-msg-chat msg 'offline)
                            (plist-get msg :message_thread_id))
          (plist-get msg :id)))

        ((telega-msg-match-p msg 'post-with-comments)
         (telega-chat--goto-thread (telega-msg-chat msg 'offline)
                                   (plist-get msg :id)))

        ((telega-msg-match-p msg 'is-thread)
         (telega-chat--goto-thread (telega-msg-chat msg 'offline)
                                   (plist-get msg :message_thread_id)
                                   (plist-get msg :id)))))

(defun telega-msg-can-open-media-timestamp-p (msg)
  "Return non-nil if MSG can be opened with custom media timestamp.
Only video, audio, video-note, voice-note or a message with web page
preview, having media content, can be opened with media timestamp."
  (or (telega-msg-match-p msg '(type VideoNote VoiceNote Audio Video))
      (when-let ((web-page (telega--tl-get msg :content :web_page)))
        (or (plist-get web-page :video)
            (plist-get web-page :audio)
            (plist-get web-page :video_note)
            (plist-get web-page :voice_note)))))

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
      (telega-file--upload-internal msg-file
        (lambda (_filenotused)
          (telega-msg-redisplay msg))))))

(defun telega-msg-sender (tl-obj)
  "Convert given TL-OBJ to message sender (a chat or a user).
TL-OBJ could be a \"message\", \"sponsoredMessage\", \"chatMember\",
\"messageSender\" or \"chatMessageSender\".  Return a user or a chat."
  (let ((sender (cl-ecase (telega--tl-type tl-obj)
                  (chatMessageSender
                   (when (or (not (plist-get tl-obj :needs_premium))
                             (plist-get telega--options :is_premium))
                     (plist-get tl-obj :sender)))
                  (sponsoredMessage
                   (list :@type "messageSenderChat"
                         :chat_id (plist-get tl-obj :sponsor_chat_id)))
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

(defun telega-msg-sender-title (msg-sender &rest args)
  "Return title for the message sender MSG-SENDER.
ARGS are passed directly to `telega-ins--msg-sender'."
  (declare (indent 1))
  (telega-ins--as-string
   (apply #'telega-ins--msg-sender msg-sender args)))

(defun telega-msg-sender-title--special (msg-sender)
  "Return title for message sender MSG-SENDER to be used in special messages."
  (telega-ins--as-string
   (telega-ins--with-face 'bold
     (telega-ins--msg-sender msg-sender :with-avatar-p t))))

(defun telega-msg-sender-color (msg-sender)
  "Return color for the message sender MSG-SENDER."
  (if (telega-user-p msg-sender)
      (telega-user-color msg-sender)
    (cl-assert (telega-chat-p msg-sender))
    (telega-chat-color msg-sender)))

(defun telega-msg-sender-title-faces (msg-sender)
  "Compute faces list to use for MSG-SENDER title."
  (nconc (list (if (telega-me-p msg-sender)
                   'telega-msg-self-title
                 'telega-msg-user-title))
         ;; Maybe add some rainbow color to the message title
         (when telega-msg-rainbow-title
           (let* ((lightp (eq (frame-parameter nil 'background-mode) 'light))
                  (foreground (nth (if lightp 0 1)
                                   (telega-msg-sender-color msg-sender))))
             (when foreground
               (list (list :foreground foreground)))))))

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
    (unless (= (plist-get msg :id) (telega-chatbuf--last-message-id))
      (telega-button-forward 1 'telega-msg-at))
    
    (telega-chatbuf--chat-update "marked-messages")))

(defun telega-msg-pin-toggle (msg)
  "Toggle pin state of the message MSG.
For interactive use only."
  (interactive (list (telega-msg-for-interactive)))
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
  (telega--searchChatMessages (telega-chat-me)
      '(:@type "searchMessagesFilterDocument") 0 0
    :query "#telega_favorite_messages"
    :limit 1
    :callback
    (lambda (reply)
      (let ((messages (plist-get reply :messages)))
        (unless (seq-empty-p messages)
          (setq telega--favorite-messages-storage-message
                (seq-first messages))
          (let ((file (telega-msg--content-file
                       telega--favorite-messages-storage-message)))
            (cl-assert file)
            ;; And now load associated file asynchronously
            (telega-file--download file 32
              (lambda (tl-file)
                (setq telega--favorite-messages
                      (with-temp-buffer
                        (insert-file-contents
                         (telega--tl-get tl-file :local :path))
                        (goto-char (point-min))
                        (read (current-buffer))))
                (telega-debug "Loaded %d favorite messages"
                              (length telega--favorite-messages))
                ))))))))

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
         (webpage (plist-get content :web_page))
         (file-accessor
          (cl-some (lambda (accessor)
                     (when-let ((place (or (plist-get content (car accessor))
                                           (plist-get webpage (car accessor)))))
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
  (telega--resendMessages msg))

(defun telega-msg-save (msg)
  "Save messages's MSG media content to a file.
If MSG is an animation message, then possibly add animation to
the saved animations list."
  (interactive (list (telega-msg-for-interactive)))
  (let ((file (telega-msg--content-file msg)))
    (unless file
      (user-error "No file associated with message"))

    (cond
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
      (telega-file--download file 32
        (lambda (dfile)
          (telega-msg-redisplay msg)
          (when (telega-file--downloaded-p dfile)
            ;; TODO: This might be executed in process filter, so
            ;; pressing C-g will trigger "error in process filter: Quit"
            ;; Need to execute this outside of process filter
            (let* ((fpath (telega--tl-get dfile :local :path))
                   (fname (file-name-nondirectory fpath))
                   (new-fpath (if telega-msg-save-dir
                                  (expand-file-name fname telega-msg-save-dir)
                                (read-file-name "Save to file: " default-directory
                                                nil nil fname nil))))
              ;; NOTE: Ensure corresponding directory exists
              (let ((fdir (file-name-directory new-fpath)))
                (unless (file-exists-p fdir)
                  (if (y-or-n-p
                       (format-message
                        "Directory `%s' does not exist; create? " fdir))
                      (make-directory fdir t)
                    (error "Canceled"))))

              ;; See https://github.com/tdlib/td/issues/379
              (copy-file fpath new-fpath)
              (message (format "Wrote %s" new-fpath))))))))))

(defun telega-msg-copy-link (msg &optional for-thread-p)
  "Copy link to message to kill ring.
Use \\[yank] command to paste a link."
  (interactive (list (telega-msg-for-interactive)
                     (when telega-chatbuf--thread t)))
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
                            (and (plist-get msg :can_get_media_timestamp_links)
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

(defun telega-msg-copy-text (msg)
  "Copy a text of the message MSG."
  (interactive (list (telega-msg-for-interactive)))

  (unless (plist-get msg :can_be_saved)
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
    (kill-new msg-text)
    (message "Copied message text (%d chars)" (length msg-text))))

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

(defun telega-describe-message (msg &optional for-thread-p)
  "Show info about message at point."
  (interactive (list (telega-msg-for-interactive)
                     (when telega-chatbuf--thread t)))
  (with-telega-help-win "*Telegram Message Info*"
    (let ((chat-id (plist-get msg :chat_id))
          (msg-id (plist-get msg :id)))
      ;; We use `telega--help-win-param', to make ensure message won't
      ;; change on async requests
      (setq telega--help-win-param msg-id)

      (telega-ins "Date(ISO8601): ")
      (telega-ins--date-iso8601 (plist-get msg :date))
      (telega-ins "\n")
      (telega-ins-fmt "Chat-id: %d\n" chat-id)
      (telega-ins-fmt "Message-id: %d\n" msg-id)
      (when-let ((sender (telega-msg-sender msg)))
        (telega-ins "Sender: ")
        (telega-ins--raw-button (telega-link-props 'sender sender 'type 'telega)
          (telega-ins--msg-sender sender
            :with-avatar-p t
            :with-username-p 'telega-username
            :with-brackets-p t))
        (telega-ins "\n"))
      ;; Link to the message
      (when-let ((link (ignore-errors
                         ;; NOTE: we ignore any errors such as
                         ;;   - error=6: Public message links are available
                         ;;              only for messages in supergroups
                         ;;   - error=6: Message is scheduled
                         ;;   ...
                         (telega--getMessageLink msg
                           :for-thread-p for-thread-p))))
        (telega-ins "Link: ")
        (telega-ins--raw-button (telega-link-props 'url link 'face 'link)
          (telega-ins link))
        (telega-ins "\n"))

      (telega-ins "Internal Link: ")
      (let ((internal-link (telega-tme-internal-link-to msg)))
        (telega-ins--raw-button
            (telega-link-props 'url internal-link 'face 'link)
          (telega-ins internal-link)))
      (telega-ins "\n")

      ;; Custom emoji stickersets
      (when-let* ((custom-emojis
                   (mapcar #'telega-custom-emoji-get
                           (telega-custom-emoji--ids-for-msg msg)))
                  (sset-id-list
                   (seq-uniq (mapcar (telega--tl-prop :set_id) custom-emojis))))
        (telega-ins "Custom Emoji stickersets: ")
        (seq-doseq (sset-id (delq nil sset-id-list))
          (telega-stickerset-get sset-id nil
            (telega--gen-ins-continuation-callback 'loading
              (lambda (sset)
                (telega-ins--button (telega-stickerset-title sset)
                  :value sset
                  :action #'telega-describe-stickerset))
              msg-id))
          (telega-ins " "))
        (telega-ins "\n"))

      (when-let ((translated (plist-get msg :telega-translated)))
        (when (with-telega-chatbuf (telega-msg-chat msg)
                telega-translate-replace-content)
          (telega-ins "Original Content:\n")
          (telega-ins--column 2 nil
            (telega-ins (telega-msg-content-text msg 'with-speech)))
          (telega-ins "\n")))

      (when (plist-get msg :can_get_added_reactions)
        (telega-ins "Message Reactions: ")
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
                  (telega-ins "  ")
                  (telega-ins--msg-reaction-type (plist-get ar :type))
                  (telega-ins " ")
                  (telega-ins--msg-sender
                      (telega-msg-sender (plist-get ar :sender_id))
                   :with-avatar-p t
                   :with-username-p t
                   :with-brackets-p t)
                  (telega-ins--move-to-column 42)
                  (telega-ins " ")
                  (telega-ins--date-relative (plist-get ar :date))
                  )))
            msg-id))
        (telega-ins "\n"))

      (when (plist-get msg :can_get_viewers)
        (telega-ins "Message Viewers: ")
        ;; Asynchronously fetch message viewers
        (telega--getMessageViewers msg
          (telega--gen-ins-continuation-callback 'loading
            (lambda (viewers)
              (telega-ins-fmt "%d" (length viewers))
              (seq-doseq (viewer viewers)
                (telega-ins "\n")
                (telega-ins "  ")
                (telega-ins--msg-sender
                 (telega-user-get (plist-get viewer :user_id))
                 :with-avatar-p t
                 :with-username-p t
                 :with-brackets-p t)
                (telega-ins--move-to-column 40)
                (telega-ins " ")
                (telega-ins--date-relative (plist-get viewer :view_date))))
            msg-id))
        (telega-ins "\n"))

      (when telega-debug
        (let ((print-length nil))
          (telega-ins "\n---DEBUG---\n")
          (telega-ins-fmt "MsgSexp: (telega-msg-get (telega-chat-get %d) %d)\n"
            chat-id msg-id)
          (telega-ins-fmt "Message: %S\n" msg)))
      )))

(defun telega-ignored-messages ()
  "Display all messages that has been ignored."
  (interactive)
  (with-help-window "*Telegram Ignored Messages*"
    (set-buffer standard-output)
    (let ((inhibit-read-only t))
      (dolist (msg (ring-elements telega--ignored-messages-ring))
        (telega-button--insert 'telega-msg msg
          :inserter #'telega-ins--message-with-chat-header)
        (telega-ins "\n"))
      (goto-char (point-min)))))

(defun telega-msg-public-forwards (msg)
  "Display public forwards for the message MSG."
  (interactive (list (telega-msg-at (point))))
  (let* ((reply (telega--getMessagePublicForwards msg))
         (public-fwd-messages (append (plist-get reply :messages) nil)))
    (unless public-fwd-messages
      (error "No forwardings to public channels for this message"))

    (with-help-window "*Telegram Public Forwards*"
      (set-buffer standard-output)
      (let ((inhibit-read-only t))
        (telega-ins (propertize "Total Messages:" 'face 'bold) " "
                    (int-to-string (plist-get reply :total_count)) "\n")
        (dolist (fwd-msg public-fwd-messages)
          (telega-button--insert 'telega-msg fwd-msg
            :inserter #'telega-ins--message-with-chat-header)
          (telega-ins "\n")))
      (goto-char (point-min)))))

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
        (telega-ins--date-iso8601 (plist-get msg-old :date))
        (telega-ins "\n")

        (telega-ins--with-face 'diff-added
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

(defun telega-msg-add-reaction (msg &optional big-p)
  "Interactively add reaction to the message MSG.
If `\\[universal-argument]' is used, reaction with big animation will
be added."
  (interactive (list (telega-msg-for-interactive) current-prefix-arg))

  (let* ((custom-label "Custom Reaction")
         (chat-av-reactions
          (plist-get (telega-msg-chat msg) :available_reactions))
         (msg-av-reactions
          (telega--getMessageAvailableReactions msg))
         (reaction-choices
          (cl-case (telega--tl-type chat-av-reactions)
            (chatAvailableReactionsSome
             (mapcar (lambda (reaction-type)
                       (cl-assert (eq (telega--tl-type reaction-type)
                                      'reactionTypeEmoji))
                       (telega-tl-str reaction-type :emoji))
                     (plist-get chat-av-reactions :reactions)))
            (chatAvailableReactionsAll
             telega-emoji-reaction-list)))
         (choices (if (plist-get msg-av-reactions :allow_custom_emoji)
                      (append reaction-choices (list custom-label))
                    reaction-choices))
         (choice (funcall telega-completing-read-function
                          (format "Add %sReaction: " (if big-p "BIG " ""))
                          choices nil t)))
    (if (not (equal choice custom-label))
        (telega--addMessageReaction
         msg (list :@type "reactionTypeEmoji" :emoji choice)
         big-p 'udate-recent)

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
            (telega-ins "TOP:\n")
            (telega-ins--available-reaction-list
             top-av-reactions reaction-type-action)
            (telega-ins "\n\n"))

          (unless (seq-empty-p recent-av-reactions)
            (telega-ins "RECENT:\n")
            (telega-ins--available-reaction-list
             recent-av-reactions reaction-type-action)
            (telega-ins "\n\n"))

          (unless (seq-empty-p popular-av-reactions)
            (telega-ins "POPULAR:\n")
            (telega-ins--available-reaction-list
             popular-av-reactions reaction-type-action)
            (telega-ins "\n\n"))

          (telega-ins--custom-emoji-stickersets
           (lambda (sticker)
             (cl-assert (telega-custom-emoji-sticker-p sticker))
             (telega--addMessageReaction
              msg (list :@type "reactionTypeCustomEmoji"
                        :custom_emoji_id (telega-custom-emoji-id sticker))
              big-p 'update-recent-reactions))))))))

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

(defun telega-msg-remove-text-spoiler (msg)
  "Show spoiler text entity at point."
  (interactive (list (telega-msg-for-interactive)))

  (when-let ((ent-type (get-text-property (point) :tl-entity-type)))
    (when (eq 'textEntityTypeSpoiler (telega--tl-type ent-type))
      (plist-put ent-type :telega-show-spoiler t)
      (let ((cursor-sensor-inhibit t))
        (telega-msg-redisplay msg)))))

(defun telega-msg-remove-media-spoiler (msg)
  "Remove spoiler for the media message MSG."
  (interactive (list (telega-msg-for-interactive)))
  (let ((content (plist-get msg :content)))
    (when (and (plist-get content :has_spoiler)
               (not (plist-get content :telega-spoiler-removed)))
      (plist-put content :telega-spoiler-removed t)
      (telega-msg-redisplay msg))))

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
                  (chat-id (plist-get reply-to :story_sender_chat_id))
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
                (chat-id (plist-get reply-to :story_sender_chat_id))
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

(provide 'telega-msg)


;; Load favorite messages
(add-hook 'telega-chats-fetched-hook #'telega-msg--favorite-messages-file-fetch)

;;; telega-msg.el ends here

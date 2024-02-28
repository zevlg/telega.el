;;; telega-notifications.el --- Notifications support for telega  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Apr 18 18:18:22 2018
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

;;; ellit-org: commentary
;;
;; =telega.el= can notify you about incoming messages and calls via
;; D-Bus notifications, however notifications are disabled by default.
;;
;; Enable it with ~(telega-notifications-mode 1)~ or at =telega= load time:
;; #+begin_src emacs-lisp
;; (add-hook 'telega-load-hook 'telega-notifications-mode)
;; #+end_src

;;; Code:
(require 'cl-lib)
(require 'notifications)

(require 'telega-core)
(require 'telega-util)

(declare-function telega-chat--type "telega-chat" (chat))
(declare-function telega-chat-get "telega-chat" (chat-id &optional offline-p))
(declare-function telega-chat-title "telega-chat" (chat &optional no-badges))
(declare-function telega-chat-muted-p "telega-chat" (chat))


(defvar telega-notifications--last-id nil
  "Currently shown notification.")

(defun telega-chat-notification-scope (scope-type)
  "Return notification settings for SCOPE-TYPE.
SCOPE-TYPE is one of:
\"notificationSettingsScopePrivateChats\",
\"notificationSettingsScopeGroupChats\",
\"notificationSettingsScopeChannelChats\"."
  (let ((scope (alist-get scope-type telega--scope-notification-alist
                          nil nil #'string=)))
    (unless scope
      (setq scope (telega--getScopeNotificationSettings scope-type))
      (setf (alist-get scope-type telega--scope-notification-alist
                       nil nil #'string=)
            scope))
    scope))

(defun telega-chat-notification-setting (chat setting &optional topic)
  "For the CHAT return notification SETTING value.
SETTING could be one of: `:mute_for', `:sound', `:show_preview',
`:disable_pinned_message_notifications',
`:disable_mention_notifications'.
If TOPIC is specified, return notification setting for the given topic."
  (cl-assert chat)
  (let* ((use-default-name
          (intern (concat ":use_default_" (substring (symbol-name setting) 1))))
         (not-cfg (plist-get (or topic chat) :notification_settings))
         (use-default-p
          (plist-get not-cfg use-default-name)))
    (cond ((and topic use-default-p)
           ;; Use chat's setting
           (telega-chat-notification-setting chat setting))
          (use-default-p
           ;; Use scope's setting
           (plist-get (telega-chat-notification-scope
                       (cl-case (telega-chat--type chat)
                         (channel
                          "notificationSettingsScopeChannelChats")
                         ((basicgroup supergroup)
                          "notificationSettingsScopeGroupChats")
                         (t
                          "notificationSettingsScopePrivateChats")))
                      setting))
          (t
           (plist-get not-cfg setting)))))

(defun telega-ins--msg-notification (msg)
  "Inserter to format MSG to notify about."
  ;; Limit length of the message
  (telega-ins--with-attrs (list :max telega-notifications-msg-body-limit
                                :elide t)
    (let ((chat (telega-chat-get (plist-get msg :chat_id))))
      (unless (telega-chat-match-p chat '(type private secret bot))
        (telega-ins--msg-sender (telega-msg-sender msg))
        (telega-ins ": ")))

    (let ((telega-use-images nil))
      (telega-ins--content msg))))

(defun telega-ins--notification-scope (scope-type)
  "Insert notification scope of SCOPE-TYPE.
SCOPE-TYPE is same an in `telega-chat-notification-scope'."
  (let* ((sconf (telega-chat-notification-scope scope-type))
         (mute-for (plist-get sconf :mute_for))
         (unmuted-p (zerop mute-for))
         (sound-id (plist-get sconf :sound_id))
         (preview-p (plist-get sconf :show_preview)))
    (telega-ins--text-button (if unmuted-p
                                 (telega-symbol 'checkbox-on)
                               (telega-symbol 'checkbox-off))
      'face 'telega-link
      'action (lambda (_button)
                (telega--setScopeNotificationSettings
                    scope-type :mute_for (if unmuted-p telega-mute-for-ever 0))))
    (telega-ins " " (telega-i18n "lng_settings_desktop_notify"))
    (telega-ins "\n")

    (telega-ins--text-button (if preview-p
                                 (telega-symbol 'checkbox-on)
                               (telega-symbol 'checkbox-off))
      'face 'telega-link
      'action (lambda (_button)
                (telega--setScopeNotificationSettings scope-type
                  :show_preview (if preview-p :false t))))
    (telega-ins " " (telega-i18n "lng_settings_show_preview"))
    (telega-ins "\n")

    (let ((mute-stories-p (plist-get sconf :mute_stories)))
      (telega-ins--text-button (if mute-stories-p
                                   (telega-symbol 'checkbox-on)
                                 (telega-symbol 'checkbox-off))
        'face 'telega-link
        'action (lambda (_button)
                  (telega--setScopeNotificationSettings scope-type
                    :mute_stories (if mute-stories-p :false t))))
      (telega-ins " " "Mute Stories")
      (telega-ins "\n"))

    (let ((show-story-sender-p (plist-get sconf :show_story_sender)))
      (telega-ins--text-button (if show-story-sender-p
                                   (telega-symbol 'checkbox-on)
                                 (telega-symbol 'checkbox-off))
        'face 'telega-link
        'action (lambda (_button)
                  (telega--setScopeNotificationSettings scope-type
                    :show_story_sender (if show-story-sender-p :false t))))
      (telega-ins " " "Show Story Sender")
      (telega-ins "\n"))

    (let ((disable-pinned-p
           (plist-get sconf :disable_pinned_message_notifications)))
      (telega-ins--text-button (if disable-pinned-p
                                   (telega-symbol 'checkbox-on)
                                 (telega-symbol 'checkbox-off))
        'face 'telega-link
        'action (lambda (_button)
                  (telega--setScopeNotificationSettings scope-type
                    :disable_pinned_message_notifications
                    (if disable-pinned-p :false t))))
      (telega-ins " " "Disable Pinned Message Notification")
      (telega-ins "\n"))

    (let ((disable-mentions-p (plist-get sconf :disable_mention_notifications)))
      (telega-ins--text-button (if disable-mentions-p
                                   (telega-symbol 'checkbox-on)
                                 (telega-symbol 'checkbox-off))
        'face 'telega-link
        'action (lambda (_button)
                  (telega--setScopeNotificationSettings scope-type
                    :disable_mention_notifications
                    (if disable-mentions-p :false t))))
      (telega-ins " " "Disable Mention Notification")
      (telega-ins "\n"))

    (telega-ins (telega-i18n "lng_notification_sound") ": "
                sound-id)
    (telega-ins "\n")

    ;; Exceptions
    (when-let ((exception-chats
                (telega--getChatNotificationSettingsExceptions scope-type)))
      (telega-ins-fmt "Exceptions: %d chats" (length exception-chats))
      (telega-ins--line-wrap-prefix "  "
        (dolist (chat exception-chats)
          (telega-ins "\n")
          (telega-button--insert 'telega-chat chat
            :inserter #'telega-ins--chat
            :action #'telega-describe-chat))))
    ))

(defun telega-describe-notifications--inserter (&rest _ignored)
  "Inserter for notification settings."
  (telega-ins "telega-notifications-mode: ")
  (telega-ins--with-face 'telega-shadow
    (telega-ins (if telega-notifications-mode
                    (telega-symbol 'checkbox-on)
                  (telega-symbol 'checkbox-off))))
  (telega-ins "\n")
  (unless telega-notifications-mode
    (telega-ins--help-message
     (telega-ins "To enable notifications run \
M-x telega-notifications-mode RET")))

  (telega-ins "\n")
  (telega-ins--with-face 'bold
    (telega-ins (telega-i18n "lng_notification_private_chats") "\n"))
  (telega-ins--line-wrap-prefix "  "
    (telega-ins--notification-scope
     "notificationSettingsScopePrivateChats"))
  (telega-ins "\n")

  (telega-ins "\n")
  (telega-ins--with-face 'bold
    (telega-ins (telega-i18n "lng_notification_groups") "\n"))
  (telega-ins--line-wrap-prefix "  "
    (telega-ins--notification-scope
     "notificationSettingsScopeGroupChats"))
  (telega-ins "\n")

  (telega-ins "\n")
  (telega-ins--with-face 'bold
    (telega-ins (telega-i18n "lng_notification_channels") "\n"))
  (telega-ins--line-wrap-prefix "  "
    (telega-ins--notification-scope
     "notificationSettingsScopeChannelChats"))
  (telega-ins "\n")

  (telega-ins "\n")
  (telega-ins--box-button (telega-i18n "telega_reset_notifications")
    'action (lambda (_button)
              (when (yes-or-no-p (telega-i18n "telega_query_reset_notifications"))
                (telega--resetAllNotificationSettings))))
  (telega-ins "\n")
  (telega-ins--help-message
   (telega-ins-i18n "telega_reset_notifications_help"))
  )

(defun telega-describe-notifications (&rest _ignored)
  "Show global notifications settings."
  (interactive)
  (with-telega-help-win "*Telega Notifications*"
    (telega-describe-notifications--inserter)

    (setq telega--help-win-param nil)
    (setq telega--help-win-inserter #'telega-describe-notifications--inserter)
    ))

(defun telega-describe-notifications--maybe-redisplay ()
  "If CHAT info buffer exists and visible, then redisplay it."
  (telega-help-win--maybe-redisplay "*Telega Notifications*" nil))

(defun telega-notifications--close (id)
  "Close notification by ID."
  (when (eq telega-notifications--last-id id)
    (setq telega-notifications--last-id nil)
    (dbus-ignore-errors
      ;; See https://t.me/emacs_telega/6532
      (notifications-close-notification id))))

(defun telega-notifications--notify (notify-spec)
  "Use `notifications-notify' to popup NOTIFY-SPEC."
  (when telega-notifications--last-id
    (notifications-close-notification telega-notifications--last-id))
  (let* ((base-spec (list :app-name "emacs.telega"
                          :app-icon (telega-etc-file "telega-logo.svg")
                          ;; NOTE: with this param popups stucks sometimes
                          ;; So we use timer to manually remove the popup
                          ;; Or newly arrived notifications also
                          ;; removes current popup
                          :timeout -1
;                          :timeout (round (* 1000 telega-notifications-timeout))
                          :urgency "normal"))
         ;; DO NOT modify NOTIFY-SPEC
         (notify-args (append notify-spec base-spec)))
    (telega-debug "NOTIFY with args: %S" notify-args)
    (setq telega-notifications--last-id
          (apply 'notifications-notify notify-args))))

(defun telega-notifications--chat-msg0 (msg &optional force &rest notify-args)
  "Function called after `telega-notifications-delay' delay.
If FORCE is specified, then always popup notification.
Otherwise popup notification only if MSG have not been seen yet.
FORCE is used for testing only, should not be used in real code."
  (declare (indent 2))
  ;; Checks once more that message has not yet been read in another
  ;; telegram client
  (let* ((msg-id (plist-get msg :id))
         (chat-id (plist-get msg :chat_id))
         (chat (telega-chat-get chat-id)))
    (unless (and (not force) (telega-msg-seen-p msg chat))
      (ring-insert telega--notification-messages-ring msg)
      (setq notify-args
            (append
             notify-args
             (list :on-action `(lambda (&rest args)
                                 (x-focus-frame (telega-x-frame))
                                 (telega-chat--goto-msg
                                     (telega-chat-get ,chat-id)
                                     ,msg-id 'highlight))
                   ;; NOTE: outgoing messages bypassed notification
                   ;; conditions are scheduled messages, mark them
                   ;; with calendar symbol
                   ;; See https://github.com/tdlib/td/issues/1196
                   :title (concat (when (plist-get msg :is_outgoing)
                                    "📅 ")
                                  (if (telega-me-p chat)
                                      (telega-i18n "lng_notification_reminder")
                                    (telega-ins--as-string
                                     (telega-ins--msg-sender chat
                                       :with-username-p t))))
                   :body (if (telega-chat-notification-setting chat :show_preview)
                             (telega-ins--as-string
                              (funcall telega-inserter-for-msg-notification msg))
                           "Has new unread messages"))
             telega-notifications-msg-args))
      ;; Play sound only if CHAT setting has some sound
      (when (string-empty-p
             (or (telega-chat-notification-setting chat :sound) ""))
        (setq notify-args (telega-plist-del notify-args :sound-name)))

      (telega-notifications--notify notify-args)
      ;; Workaround stuck notifications, force closing after
      ;; `telega-notifications-timeout' timeout
      (run-with-timer telega-notifications-timeout nil
                      'telega-notifications--close
                      telega-notifications--last-id)
      )))

;;; ellit-org: notification-conditions
;; Do *NOT* pop notification if:
;;  0. Me is not member of the group chat, see
;;     https://github.com/zevlg/telega.el/issues/224
;;  1. Message is ignored by
;;     [[#client-side-messages-ignoring][client side messages ignoring]]
;;  2. Chat is muted and message does not contain unread mention or
;;     mention notification is disabled for the chat
;;  3. Message already has been read (see ~telega-msg-seen-p~)
;;  4. Message is older than 1 min (to avoid popping up messages on
;;     laptop wakeup)
;;  5. Message is currently observable in a chatbuf, i.e. chatbuf
;;     must be selected and focused.
(defun telega-notifications-msg-notify-p (msg)
  "Return non-nil if message MSG should pop-up notification."
  (let ((chat (telega-msg-chat msg)))
    (unless (or (not (telega-chat-match-p chat
                       '(or (type private secret) me-is-member)))
                (and (telega-chat-muted-p chat)
                     (or (telega-chat-notification-setting
                          chat :disable_mention_notifications)
                         (not (plist-get msg :contains_unread_mention))))
                (telega-msg-seen-p msg chat)
                (with-telega-chatbuf chat
                  (and (telega-chatbuf--msg-observable-p msg)
                       (not (telega-chatbuf--history-state-get :newer-freezed)))))
      t)))

(defun telega-notifications-chat-message (msg)
  "Function intended to be added to `telega-chat-post-message-hook'."
  (unless (or (telega-msg-match-p msg 'ignored)
              (> (- (telega-time-seconds) (plist-get msg :date)) 60))
    (when (telega-msg-match-p msg telega-notifications-msg-temex)
      (if (> telega-notifications-delay 0)
          (run-with-timer telega-notifications-delay nil
                          'telega-notifications--chat-msg0 msg)
        ;; No delay
        (telega-notifications--chat-msg0 msg)))))

(defun telega-notifications-incoming-call (call)
  "Function intended to be added to `telega-call-incoming-hook'."
  (let* ((call-id (plist-get call :id))
         (user (telega-user-get (plist-get call :user_id)))
         (notargs (list :actions (list "default" "accept")
                        :on-action `(lambda (&rest args)
                                      (x-focus-frame (telega-x-frame))
                                      (telega-voip-accept
                                       (telega-voip--by-id ,call-id)))
                        :timeout 0
                        ;; I18N: lng_call_incoming
                        :title "Incoming call"
                        :body (format "from %s" (telega-msg-sender-title user
                                                  :with-avatar-p t
                                                  :with-username-p t)))))
    (telega-notifications--notify
     (nconc notargs telega-notifications-call-args))))

;;;###autoload
(define-minor-mode telega-notifications-mode
  "Telega D-Bus notifications."
  :init-value nil :global t :group 'telega-notifications
  (if telega-notifications-mode
      (progn
        (add-hook 'telega-chat-post-message-hook 'telega-notifications-chat-message)
        (add-hook 'telega-call-incoming-hook 'telega-notifications-incoming-call))
    (remove-hook 'telega-chat-post-message-hook 'telega-notifications-chat-message)
    (remove-hook 'telega-call-incoming-hook 'telega-notifications-incoming-call)))

(defun telega-notifications-history ()
  "Show notifications history, most recent notification goes first."
  (interactive)
  (with-telega-help-win "*Telegram Notification Messages*"
    (save-excursion
      (dolist (msg (ring-elements telega--notification-messages-ring))
        (telega-button--insert 'telega-msg msg
          :inserter #'telega-ins--message-with-chat-header
          :action #'telega-msg-goto-highlight)
        (telega-ins "\n")))))

(provide 'telega-notifications)


(when (boundp 'telega-use-notifications)
  (warn "`telega-use-notifications' is deprecated in favor for `telega-notifications-mode'."))

;;; telega-notifications.el ends here

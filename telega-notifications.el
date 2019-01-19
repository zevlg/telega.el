;;; telega-notifications.el --- Notifications support for telega.

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

;;; Commentary:

;; Notification pop-ups using `notifications.el' for incoming messages
;; and calls.
;; 
;; To enable notifications use next code in your init.el:
;; 
;;   (add-hook 'telega-load-hook 'telega-notifications-mode)
;; 

;;; Code:
(require 'cl-lib)
(require 'notifications)

(require 'telega-core)
(require 'telega-util)

(defun telega-chat-notification-setting (chat setting &optional default-p)
  "For the CHAT return NOTIFICATION-SETTING value.
If DEFAULT-P is non-nil then return default setting for the CHAT."
  (let ((use-default-name
         (intern (concat ":use_default_" (substring (symbol-name setting) 1))))
        (not-cfg (plist-get chat :notification_settings)))
    (when (or default-p (plist-get not-cfg use-default-name))
      (if (memq (telega-chat--type chat 'raw) '(private secret))
          (setq not-cfg (car telega--scope-notification-settings))
        (setq not-cfg (cdr telega--scope-notification-settings))))

    (plist-get not-cfg setting)))
  
(defun telega--on-updateScopeNotificationSettings (event)
  "Handle `updateScopeNotificationSettings' EVENT."
  (let ((scope (plist-get event :scope))
        (settings (plist-get event :notification_settings)))
    (cl-ecase (telega--tl-type (plist-get event :scope))
      (notificationSettingsScopePrivateChats
       (setcar telega--scope-notification-settings settings))
      (notificationSettingsScopeGroupChats
       (setcdr telega--scope-notification-settings settings)))))

(defun telega-ins--msg-notification (msg)
  "Inserter to format MSG to notify about."
  ;; Limit length of the message
  (telega-ins--with-attrs (list :max telega-notifications-msg-body-limit
                                :elide t)
    (let ((chat (telega-chat--get (plist-get msg :chat_id))))
      (unless (memq (telega-chat--type chat 'raw) '(private secret))
        (when (telega-ins--username (plist-get msg :sender_user_id) 'name)
          (telega-ins ": "))
        (when (telega-ins--via-bot (plist-get msg :via_bot_user_id))
          (telega-ins "\n"))))

    (telega-ins--content msg)))

;;;###autoload
(defun telega-notifications-mode (&optional arg)
  "Toggle telega notifications on or off.
With positive ARG - enables notifications, otherwise disables.
If ARG is not given then treat it as 1."
  (interactive "p")
  (if (or (null arg) (> arg 0))
      (progn
        (add-hook 'telega-chat-message-hook 'telega-notifications-chat-message)
        (add-hook 'telega-incoming-call-hook 'telega-notifications-incoming-call))
    (remove-hook 'telega-chat-message-hook 'telega-notifications-chat-message)
    (remove-hook 'telega-incoming-call-hook 'telega-notifications-incoming-call)))

(defun telega-notifications--notify (notify-spec)
  (let* ((base-spec (list :app-name "emacs.telega"
                          :app-icon (telega-etc-file "telegram-logo.svg")
                          :timeout (round (* 1000 telega-notifications-timeout))
                          :urgency "normal"))
         ;; DO NOT modify NOTIFY-SPEC
         (notify-args (append notify-spec base-spec)))
    (telega-debug "NOTIFY with args: %S" notify-args)
    (apply 'notifications-notify notify-args)))

;; NOTE: standard values for :sound-name
;; http://0pointer.de/public/sound-naming-spec.html
(defun telega-notifications-chat-message (msg disable-notification)
  "Function intended to be added to `telega-chat-message-hook'."
  ;; Do NOT notify message if:
  ;;  - disable-notification is non-nil
  ;;  - Chat is muted
  ;;  - Message already has been read (see last_read_inbox_message_id)
  ;;  - Message is older then 1 min (to avoid poping up messages on
  ;;    laptop wakeup)
  ;;  - Message is currently observable in chatbuf
  ;;  - [TODO] If Emacs frame has focus and root buffer is current
  (unless (or disable-notification
              (> (- (time-to-seconds) (plist-get msg :date)) 60))
    (let* ((msg-id (plist-get msg :id))
           (chat-id (plist-get msg :chat_id))
           (chat (telega-chat--get chat-id))
           (last-read-msg-id (plist-get chat :last_read_inbox_message_id)))
      (unless (or (< msg-id last-read-msg-id)
                  (not (zerop (telega-chat-notification-setting chat :mute_for)))
                  (telega-chat-msg-observable-p chat msg-id))
        (let ((notify-args
               (nconc
                (list :actions (list "default" "show message")
                      :on-action `(lambda (&rest args)
                                    (x-focus-frame (telega-x-frame))
                                    (telega-chat--goto-msg
                                     (telega-chat--get ,chat-id)
                                     ,msg-id 'highlight))
                      :title (telega-chat--title chat 'with-username)
                      :body (if (telega-chat-notification-setting chat :show_preview)
                                (telega-ins--as-string
                                 (funcall telega-inserter-for-msg-notification msg))
                              "Has new unread messages"))
                telega-notifications-msg-args)))
          ;; Play sound only if CHAT setting has some sound
          (when (string-empty-p
                 (or (telega-chat-notification-setting chat :sound) ""))
            (setq notify-args (cl--plist-remove notify-args :sound-name)))

          (telega-notifications--notify notify-args))))))

(defun telega-notifications-incoming-call (call)
  "Function intended to be added to `telega-incoming-call-hook'."
  (let* ((call-id (plist-get call :id))
         (user (telega-user--get (plist-get call :user_id)))
         (notargs (list :actions (list "default" "accept")
                        :on-action `(lambda (&rest args)
                                      (x-focus-frame (telega-x-frame))
                                      (telega-voip-accept
                                       (telega-voip--by-id ,call-id)))
                        :timeout 0
                        ;; I18N: lng_call_incoming
                        :title "Incoming call"
                        :body (format "from %s" (telega-user--name user)))))
    (telega-notifications--notify
     (nconc notargs telega-notifications-call-args))))

(provide 'telega-notifications)

;;; telega-notifications.el ends here

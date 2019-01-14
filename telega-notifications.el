;;; telega-notifications.el --- Notifications support for telega.

;; Copyright (C) 2018 by Zajcev Evgeny.

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

;; To enable notifications use next code in your init.el:
;; 
;; (add-hook 'telega-root-mode-hook (lambda () (telega-notifications-mode 1)))
;; 

;;; Code:
(require 'cl-lib)
(require 'notifications)
(require 'telega-core)
(require 'telega-utils)

(defgroup telega-notifications nil
  "Setup for D-Bus notifications."
  :group 'telega)

(defcustom telega-notifications-delay 1.0
  "*Delay in seconds before making decision show or not the message in notification.
Taking pause before showing notification is wise, because another
telegram may be active with the chat opened, you don't want the
notification to be shown for already read message.
TODO: Not yet implemented"
  :type 'float
  :group 'telega-notifications)

(defcustom telega-notifications-timeout 2.0
  "*How long to show notification in seconds."
  :type 'float
  :group 'telega-notifications)

(defcustom telega-notifications-notify-args
  (list :sound-file (telega-etc-file "telegram-msgin.wav"))
  "*Additional arguments to `notifications-notify'.
Remove `:sound-file' to mute sounds on incoming messages."
  :type 'list
  :group 'telega-notifications)

(defcustom telega-notifications-inserter 'telega-notifications-ins-msg
  "*Inserter for the notification message."
  :type 'function
  :group 'telega-notifications)

;; See https://github.com/zevlg/telega.el/issues/32
(defcustom telega-notifications-msg-body-limit 100
  "*Limit for the message body length."
  :type 'integer
  :group 'telega-notifications)

(defvar telega--notifications nil
  "Notifications settings")

(defun telega--on-updateNotificationSettings (event)
  (let ((scope (plist-get event :scope)))
    (cl-case (intern (plist-get scope :@type))
      (notificationSettingsScopeChat
       (let ((chat (telega-chat--get (plist-get scope :chat_id))))
         (plist-put chat :notification_settings
                    (plist-get event :notification_settings))))
      (t
       (telega-debug
        "TODO scope: `telega--on-updateNotificationSettings' event=%s" event))
      )))

(defun telega-notifications-ins-msg (msg)
  "Inserter to format MSG to notify about."
  )

(defun telega-notifications--format-msg (msg)
  "Format function for the notification."
  `(telega-msg-sender-name
    telega-msg-via-bot
    telega-msg-edit-date
    "\n"
    ,@(telega-msg-inline-reply msg "")
    telega-msg-format))

;;;###autoload
(defun telega-notifications-mode (&optional arg)
  "Toggle telega notifications on or off.
With positive ARG - enables notifications, otherwise disables."
  (interactive "p")
  (if (> arg 0)
      (add-hook 'telega-chat-message-hook 'telega-notfications-chat-message)
    (remove-hook 'telega-chat-message-hook 'telega-notfications-chat-message)))

(defun telega-notfications-chat-message (msg disable-notification)
  "Function intended to be added to `telega-chat-message-hook'."
  (unless disable-notification
    (let* ((chat (telega-chat--get (plist-get msg :chat_id)))
           (not-cfg (plist-get chat :notification_settings)))
      (unless (or (not (zerop (plist-get not-cfg :mute_for)))
                  ;; Do not show notification if corresponding button
                  ;; is observable
                  (with-telega-chatbuf chat
                    (save-excursion
                      (telega-button--observable-p
                       (telega-chat-buffer--button-get (plist-get msg :id))))))
        (let ((notargs (list :app-name "emacs.telega"
                             :app-icon (telega-etc-file "telegram-logo.svg")
                             :timeout (round (* 1000 telega-notifications-timeout))
                             :urgency "normal"
                             :title (telega-chat--title chat 'with-username)
                             :body (if (plist-get not-cfg :show_preview)
                                       (telega--desurrogate-apply
                                        (telega-fmt-eval
                                         'telega-notifications--format-msg msg))
                                     "Has new unread messages"))))
          (telega-debug "NOTIFY with args: %S"
                        (nconc notargs telega-notifications-notify-args))
          (apply 'notifications-notify
                 (nconc notargs telega-notifications-notify-args)))
        ))))

(defun telega-notfications-incoming-call (call)
  "Function intended to be added to `telega-call-hook'."
  )

(provide 'telega-notifications)

;;; telega-notifications.el ends here

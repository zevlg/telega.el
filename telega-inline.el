;;; telega-inline.el --- Support for inline stuff  -*- lexical-binding:t -*-

;; Copyright (C) 2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Feb 14 04:51:54 2019
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

(defun telega--getCallbackQueryAnswer (msg payload)
  (telega-server--call
   (list :@type "getCallbackQueryAnswer"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id)
         :payload payload)))

(defun telega-inline--callback-payload (kbd-button-type)
  (cl-ecase (telega--tl-type kbd-button-type)
    (inlineKeyboardButtonTypeCallback
     (list :@type "callbackQueryPayloadData"
           :data (plist-get kbd-button-type :data)))
    ;; TODO: other types
    ))

(defun telega-inline--get-callback (msg kbd-button-type)
  "Get answer for the button callback."
  (let ((reply (telega--getCallbackQueryAnswer
                msg (telega-inline--callback-payload kbd-button-type))))
    (telega-debug "CallbackAnswer: %S" reply)

    (if (plist-get reply :show_alert)
        ;; Maybe popup message from the bot
        (with-telega-help-win "*Callback Alert*"
          (telega-ins (plist-get reply :text))
          (telega-ins-prefix "\n"
            (let ((link (plist-get reply :url)))
              (when link
                (telega-ins--raw-button (telega-link-props 'url link)
                  (telega-ins link))))))

      (message (plist-get reply :text)))))

(provide 'telega-inline)

;;; telega-inline.el ends here

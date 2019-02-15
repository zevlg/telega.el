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
(require 'telega-server)
(require 'telega-ins)

(defun telega--on-getCallbackQueryAnswer (event)
  "Handle callback reply answer."
  (let ((text (plist-get reply :text))
        (link (plist-get reply :url)))
    (if (plist-get reply :show_alert)
        ;; Maybe popup message from the bot
        (with-telega-help-win "*Callback Alert*"
          (telega-ins text)
          (when link
            (telega-ins "\n")
            (telega-ins--raw-button (telega-link-props 'url link)
              (telega-ins link))))
      (message (plist-get reply :text)))))

(defun telega--getCallbackQueryAnswer (msg payload)
  "Async send callback to bot."
  (telega-server--send
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

(defun telega-ins--inline-kbd (keyboard-button msg)
  "Insert inline KEYBOARD-BUTTON for the MSG."
  (cl-case (telega--tl-type keyboard-button)
    (inlineKeyboardButton
     (telega-ins--button (plist-get keyboard-button :text)
       'action (lambda (ignored)
                 (telega--getCallbackQueryAnswer
                  msg (telega-inline--callback-payload
                       (plist-get keyboard-button :type))))))
    (t (telega-ins-fmt "<TODO: %S>" keyboard-button))))

(provide 'telega-inline)

;;; telega-inline.el ends here

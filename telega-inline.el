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
(require 'telega-core)

(defun telega--on-callbackQueryAnswer (reply)
  "Handle callback reply answer."
  (let ((text (plist-get reply :text))
        (link (plist-get reply :url)))
    (if (plist-get reply :show_alert)
        ;; Popup message from the bot
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

(defun telega-inline--callback (kbd-button msg)
  "Generate callback function for KBD-BUTTON."
  (let ((kbd-type (plist-get kbd-button :type)))
    (cl-ecase (telega--tl-type kbd-type)
      (inlineKeyboardButtonTypeUrl
       (telega-browse-url (plist-get kbd-type :url)))

      (inlineKeyboardButtonTypeCallback
       (telega--getCallbackQueryAnswer
        msg (list :@type "callbackQueryPayloadData"
                  :data (plist-get kbd-type :data))))

      ;; TODO: other types
      )))

(defun telega-inline--help-echo (kbd-button msg)
  "Generate help-echo value for KBD-BUTTON."
  (let ((kbd-type (plist-get kbd-button :type)))
    (cl-case (telega--tl-type kbd-type)
      (inlineKeyboardButtonTypeUrl (plist-get kbd-type :url))
      )))

(provide 'telega-inline)

;;; telega-inline.el ends here

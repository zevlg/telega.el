;;; emacspeak-telega.el --- Speech-enable the Telega.el Telegram client -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Arkadiusz Świętnicki

;; Author: Arkadiusz Świętnicki <arkadiusz@swietnicki.dev>
;; Keywords: comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Speech-enable Telega

;;; Code:
(require 'cl-lib)
(cl-declaim (optimize (safety 0) (speed 3)))
(require 'subr-x)
(require 'emacspeak-preamble)
(require 'telega-notifications)

(declare-function telega-msg-chat "telega-msg" (msg &optional offline-p))

(defgroup emacspeak-telega nil
  "Telega -- the Telegram client Emacspeak integration."
  :group 'emacspeak
  :prefix "emacspeak-telega-")

(defcustom emacspeak-telega-autospeak nil
  "If non-nil, autospeak incoming messages in the current telega chat."
  :type 'boolean
  :group 'emacspeak-telega)

(defcustom emacspeak-telega-autospeak-message-limit nil
  "Maximum number of characters to speak for an incoming message.
If nil, then do not truncate autospoken Telega messages."
  :type '(choice
          (const :tag "Do not truncate" nil)
          (integer :tag "Character limit"))
  :group 'emacspeak-telega)

(make-variable-buffer-local 'emacspeak-telega-autospeak)

(defun emacspeak-telega--message-preview (msg)
  "Return MSG formatted as a speech string."
  (let* ((telega-notifications-msg-body-limit
          (or emacspeak-telega-autospeak-message-limit
              most-positive-fixnum))
         (text (string-trim
                (substring-no-properties
                 (telega-ins--as-string
                  (funcall telega-inserter-for-msg-notification msg))))))
    (if (string-empty-p text)
        "New incoming message"
      text)))

(defun emacspeak-telega--autospeak-message (msg)
  "Speak incoming MSG when autospeak is enabled in its chat buffer."
  (unless (or (plist-get msg :is_outgoing)
              (plist-get msg :ignored-p))
    (when-let* ((chat (telega-msg-chat msg 'offline)))
      (with-telega-chatbuf chat
                           (when emacspeak-telega-autospeak
                             (dtk-speak (emacspeak-telega--message-preview msg)))))))

;;;###autoload
(defun emacspeak-telega-toggle-autospeak (&optional prefix)
  "Toggle autospeaking incoming messages in the current telega chat.
Interactive PREFIX arg toggles the global default value, and then
sets the current local value to the result."
  (interactive "P")
  (unless (derived-mode-p 'telega-chat-mode)
    (user-error "Not in a telega chat buffer"))
  (cond
   (prefix
    (setq-default emacspeak-telega-autospeak
                  (not (default-value 'emacspeak-telega-autospeak)))
    (setq emacspeak-telega-autospeak
          (default-value 'emacspeak-telega-autospeak)))
   (t
    (setq emacspeak-telega-autospeak
          (not emacspeak-telega-autospeak))))
  (dtk-interp-sync)
  (emacspeak-icon (if emacspeak-telega-autospeak 'on 'off))
  (message "Turned %s telega autospeak %s."
           (if emacspeak-telega-autospeak "on" "off")
           (if prefix "" "locally")))

(add-hook 'telega-chat-post-message-hook
          #'emacspeak-telega--autospeak-message)

;;;  define emacspeak keys
(defun emacspeak-telega-setup-keys ()
  "Install Emacspeak bindings for telega chat buffers."
  (when (and (boundp 'telega-chat-mode-map)
             (keymapp telega-chat-mode-map))
    (define-key telega-chat-mode-map (kbd "C-c C-m")
                #'emacspeak-telega-toggle-autospeak)))

(emacspeak-telega-setup-keys)

(with-eval-after-load 'telega-chat
  (emacspeak-telega-setup-keys))


;; Bind faces to voices
(voice-setup-add-map '(
                       (telega-entity-type-blockquote voice-smoothen)
                       (telega-entity-type-bold voice-bolden)
                       (telega-entity-type-botcommand voice-animate-extra)
                       (telega-entity-type-cashtag voice-brighten)
                       (telega-entity-type-code voice-lighten)
                       (telega-entity-type-hashtag voice-brighten-extra)
                       (telega-entity-type-italic voice-animate)
                       (telega-entity-type-mention voice-overlay-1)
                       (telega-msg-self-title voice-annotate)
                       (telega-msg-user-title voice-bolden-extra)
                       (telega-user-online-status voice-bolden-and-animate)
                       (telega-user-non-online-status voice-smoothen)
                       ))

(provide 'emacspeak-telega)
;;; emacspeak-telega.el ends here

;;; telega-obsolete.el --- Check the use of obsolete functionality  -*- lexical-binding: t -*-

;; Copyright (C) 2020 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Jan 17 14:55:16 2020
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

;; Some telega functionality become obsolete sometimes.
;; telega warns user at load time in case he uses obsolete functionality.

;;; Code:

(require 'cl-lib)

(defvar telega-obsolete--variables nil
  "List of obsolete variables to examine at load time.")

(defun telega-obsolete--variable (obsolete-name &rest args)
  "Same as `make-obsolete-variable'."
  (setq telega-obsolete--variables
        (cl-pushnew obsolete-name telega-obsolete--variables))
  (apply 'make-obsolete-variable obsolete-name args))

(defun telega-obsolete--warning (var obsolescence-data)
  (let ((instead (car obsolescence-data))
        (version (nth 2 obsolescence-data)))
    (format-message
     "`%S' is an obsolete in telega %s%s" var version
     (cond ((stringp instead) (concat "; " (substitute-command-keys instead)))
           (instead (format-message "; use `%s' instead." instead))
           (t ".")))))


(telega-obsolete--variable 'telega-use-notifications
                           nil "0.5.0") ;don't remember correct version
(telega-obsolete--variable 'telega-chat-use-markdown-formatting
                           'telega-chat-use-markdown-version "0.5.6")
(telega-obsolete--variable 'telega-use-tracking
                           'telega-use-tracking-for "0.5.7")
(telega-obsolete--variable 'telega-avatar-factors
                           'telega-avatar-factors-alist "0.5.8")
(telega-obsolete--variable 'telega-url-shorten-patterns
                           'telega-url-shorten-regexps "0.6.7")
(telega-obsolete--variable 'telega-chat-mark-observable-messages-as-read
                           nil "0.6.12")
(telega-obsolete--variable 'telega-root-compact-view
                           nil "0.6.21")
(telega-obsolete--variable 'telega-filter-custom-push-list
                           'telega-filter-custom-folders "0.6.24")

(telega-obsolete--variable 'telega-chat-label-format
                           'telega-chat-folder-format "0.6.30")
(telega-obsolete--variable 'telega-root-view-topics-custom-labels
                           'telega-root-view-topics-folders "0.6.30")
(telega-obsolete--variable 'telega-root-view-show-other-chats
                           'telega-root-view-topics-other-chats "0.6.30")
(telega-obsolete--variable 'telega-user-photo-maxsize
                           'telega-user-photo-size "0.6.30")
(telega-obsolete--variable 'telega-photo-maxsize
                           'telega-photo-size-limits "0.6.30")
(telega-obsolete--variable 'telega-thumbnail-height
                           'telega-thumbnail-size-limits "0.6.30")
(telega-obsolete--variable 'telega-webpage-photo-maxsize
                           'telega-webpage-photo-size-limits "0.6.30")

(telega-obsolete--variable 'telega-find-file-hook
                           'telega-open-file-hook "0.6.31")
(telega-obsolete--variable 'telega-chat-me-custom-title
                           'telega-chat-title-custom-for "0.6.31")

(telega-obsolete--variable 'telega-chat-reply-prompt
                           nil "0.7.0")
(telega-obsolete--variable 'telega-chat-edit-prompt
                           nil "0.7.0")
(telega-obsolete--variable 'telega-chat-messages-ring-size
                           'telega-chat-messages-pop-ring-size
                           "0.7.0")
(telega-obsolete--variable 'telega-chat-scroll-scroll-conservatively
                           'telega-chat-scroll-conservatively
                           "0.7.3")
(telega-obsolete--variable 'telega-week-day-names
                           telega-i18n-weekday-names
                           "0.7.4")
(telega-obsolete--variable 'telega-chat-use-markdown-version
                           'telega-chat-input-markups
                           "0.7.4")
(telega-obsolete--variable 'telega-video-ffplay-args
                           'telega-open-message-ffplay-args
                           "0.7.5")
(telega-obsolete--variable 'telega-symbol-thunder
                           'telega-symbol-lightning
                           "0.7.38")
(telega-obsolete--variable 'telega-vvnote-voice-play-next
                           'telega-vvnote-play-next
                           "0.7.52")
(telega-obsolete--variable 'telega-vvnote-voice-cmd
                           'telega-vvnote-voice-record-args
                           "0.7.53")
(telega-obsolete--variable 'telega-vvnote-video-cmd
                           'telega-vvnote-video-record-args
                           "0.7.53")
(telega-obsolete--variable 'telega-voip-use-sounds
                           nil
                           "0.7.56")
(telega-obsolete--variable 'telega-symbol-horizontal-delim
                           'telega-symbol-horizontal-bar
                           "0.7.58")

(telega-obsolete--variable 'telega-voice-chat-display
                           'telega-video-chat-display
                           "0.7.90")
(telega-obsolete--variable 'telega-symbol-voice-chat-active
                           'telega-symbol-video-chat-active
                           "0.7.90")
(telega-obsolete--variable 'telega-symbol-voice-chat-passive
                           'telega-symbol-video-chat-passive
                           "0.7.90")

(telega-obsolete--variable 'telega-chat-input-anonymous-prompt
                           nil
                           "0.7.90")
(telega-obsolete--variable 'telega-chat-input-comment-prompt
                           nil
                           "0.7.90")
(telega-obsolete--variable 'telega-chat-input-prompt
                           'telega-chat-prompt-format
                           "0.7.101")
(telega-obsolete--variable 'telega-chat-prompt-show-avatar-for
                           'telega-chat-prompt-format
                           "0.7.101")

(telega-obsolete--variable 'telega-symbol-widths
                           nil
                           "0.8.1")

(telega-obsolete--variable 'telega-filter-unread-chats
                           'telega-unread-chat-temex
                           "0.8.20")

(telega-obsolete--variable 'telega-autoplay-for
                           'telega-autoplay-msg-temex
                           "0.8.20")
(telega-obsolete--variable 'telega-autoplay-outgoing
                           'telega-autoplay-msg-temex
                           "0.8.20")
(telega-obsolete--variable 'telega-autoplay-messages
                           'telega-autoplay-msg-temex
                           "0.8.20")

(telega-obsolete--variable 'telega-msg-default-reaction
                           nil
                           "0.8.42")

(telega-obsolete--variable 'telega-msg-edit-markup-spec
                           nil
                           "0.8.61")

(telega-obsolete--variable 'telega-chat-insert-message-hook
                           'telega-chatbuf-pre-msg-insert-hook
                           "0.8.72")
(telega-obsolete--variable 'telega-chat-goto-message-hook
                           'telega-msg-hover-in-hook
                           "0.8.72")

(telega-obsolete--variable 'telega-chat-button-brackets
                           'telega-brackets
                           "0.8.111")
(telega-obsolete--variable 'telega-chat-title-emoji-use-images
                           nil
                           "0.8.111")

(telega-obsolete--variable 'telega-chat-ret-always-sends-message
                           'telega-chat-send-message-on-ret
                           "0.8.140")

(telega-obsolete--variable 'telega-auto-download
                           'telega-auto-download-settings-alist
                           "0.8.150")

;; By https://github.com/zevlg/telega.el/issues/423
(telega-obsolete--variable 'telega-company-username-prefer-username
                           'telega-company-username-prefer-name
                           "0.8.152")

;; `telega-company-emoji' and `telega-company-telegram-emoji' backends
;; bypasses control to other backends if fails to complete
(telega-obsolete--variable 'telega-emoji-company-backend
                           nil
                           "0.8.170")
(telega-obsolete--variable 'telega-emoji-fuzzy-match
                           'telega-company-emoji-fuzzy-match
                           "0.8.170")

;; Check some obsolete var/fun is used
(cl-eval-when (eval load)
  (dolist (obsolete-var telega-obsolete--variables)
    (when (boundp obsolete-var)
      (display-warning
       'telega (telega-obsolete--warning
                obsolete-var (get obsolete-var 'byte-obsolete-variable)))))
  )

(provide 'telega-obsolete)

;;; telega-obsolete.el ends here

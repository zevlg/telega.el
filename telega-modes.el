;;; telega-modes.el --- Minor modes for the telega  -*- lexical-binding:t -*-

;; Copyright (C) 2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Aug 15 19:18:23 2019
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

;; - To display unread chats/chats with mentions in modeline:
;;   (telega-mode-line-mode 1)

;;
;;; Code:

(require 'telega-customize)
(require 'telega-server)
(require 'telega-filter)

(defvar tracking-buffers)
(declare-function telega "telega" (arg))

(defvar telega-mode-line-string ""
  "Used to cache formatted modeline string.")

(defcustom telega-mode-line-string-format
  '("   " (:eval (telega-mode-line-icon))
    (:eval (when telega-use-tracking
             (telega-mode-line-tracking)))
    (:eval (telega-mode-line-unread-unmuted))
    (:eval (telega-mode-line-mentions 'messages)))
  "Format in mode-line-format for `telega-mode-line-string'."
  :type 'list
  :group 'telega)

(defcustom telega-mode-line-format
  (list '(:eval (when (telega-server-live-p)
                  telega-mode-line-string)))
  "Format in mode-line-format to be used as part of `global-mode-string'."
  :type 'sexp
  :group 'telega
  :risky t)

(defvar telega-mode-line--logo-image-cache nil "Cached loaded logo image.")
(defun telega-mode-line-logo-image ()
  "Return telega logo image to be used in modeline."
  (or telega-mode-line--logo-image-cache
      (setq telega-mode-line--logo-image-cache
            (find-image
             (list (list :type 'imagemagick :file "etc/telega-logo.png"
                         :ascent 'center :mask 'heuristic
                         :height (window-mode-line-height))
                   (list :type 'svg :file "etc/telega-logo.svg"
                         :ascent 'center
                         :background (face-attribute 'mode-line :background)
                         :height (window-mode-line-height))
                   (list :type 'xpm :file "etc/telega-logo.xpm"
                         :ascent 'center))))))

(defun telega-mode-line-icon ()
  "Return telegram logo icon to be used in modeline."
  (propertize telega-symbol-telegram
              'display (telega-mode-line-logo-image)
              'local-map (eval-when-compile
                           (make-mode-line-mouse-map 'mouse-1 'telega))
              'mouse-face 'mode-line-highlight
              'help-echo "Click to show telega root buffer"))

(defmacro telega-mode-line-filter-gen (filter-spec)
  "Generate filtering command for `telega-mode-line-mode' using FILTER-SPEC."
  `(lambda ()
     (interactive)
     (telega nil)
     (telega-filters-push ,filter-spec)))

(defun telega-mode-line-tracking ()
  "Format number of tracking chats."
  (when tracking-buffers
    (concat
     " "
     (propertize (concat "[" (number-to-string (length tracking-buffers)) "]")
                 'local-map
                 (eval-when-compile
                   (make-mode-line-mouse-map
                    'mouse-1 (telega-mode-line-filter-gen '(tracking))))
                 'mouse-face 'mode-line-highlight
                 'help-echo "Click to filter tracking chats"))))

(defun telega-mode-line-unread-unmuted (&optional messages-p)
  "Format unread-unmuted chats/messages.
If MESSAGES-P is non-nil then use number of unread unmuted messages."
  (let ((uu-count (if messages-p
                      (plist-get telega--unread-message-count :unread_unmuted_count)
                    (plist-get telega--unread-chat-count :unread_unmuted_count))))
    ;; NOTE: `telega--unread-chat-count' or
    ;; `telega--unread-message-count' might not be yet updated, so
    ;; `uu-count' can be nil
    (unless (zerop (or uu-count 0))
      (concat
       " "
       (propertize (number-to-string uu-count)
                   'face 'telega-unread-unmuted-modeline
                   'local-map
                   (eval-when-compile
                     (make-mode-line-mouse-map
                      'mouse-1 (telega-mode-line-filter-gen '(unread unmuted))))
                   'mouse-face 'mode-line-highlight
                   'help-echo "Click to filter chats with unread/unmuted messages")))))

(defun telega-mode-line-mentions (&optional messages-p)
  "Format number of chats/messages with mentions.
If MESSAGES-P is non-nil then use number of messages with mentions."
  (let* ((m-chats (telega-filter-chats '(mention) telega--ordered-chats))
         (m-count (if messages-p
                      (apply '+ (mapcar (telega--tl-prop :unread_mention_count) m-chats))
                    (length m-chats))))
    (unless (zerop m-count)
      (concat
       " "
       (propertize (concat "@" (number-to-string m-count))
                   'face 'telega-mention-count
                   'local-map
                   (eval-when-compile
                     (make-mode-line-mouse-map
                      'mouse-1 (telega-mode-line-filter-gen '(mention))))
                   'mouse-face 'mode-line-highlight
                   'help-echo "Click to filter chats with mentions")))))

;;;###autoload
(define-minor-mode telega-mode-line-mode
  "Toggle display of the unread chats/mentions in the modeline."
  :init-value nil :global t :group 'telega
  (setq telega-mode-line-string "")
  (unless global-mode-string
    (setq global-mode-string '("")))

  (if telega-mode-line-mode
      (progn
        (unless (memq 'telega-mode-line-format global-mode-string)
          (setq global-mode-string
                (append global-mode-string '(telega-mode-line-format))))
        (advice-add 'telega--on-updateUnreadMessageCount
                    :after 'telega-mode-line-update)
        (advice-add 'telega--on-updateUnreadChatCount
                    :after 'telega-mode-line-update)
        (advice-add 'telega--on-updateChatUnreadMentionCount
                    :after 'telega-mode-line-update)
        (add-hook 'telega-ready-hook 'telega-mode-line-update)
        (add-hook 'telega-chats-fetched-hook 'telega-mode-line-update)
        (add-hook 'telega-kill-hook 'telega-mode-line-update)
        ;; NOTE: `tracking-buffer-added-hook', and
        ;; `tracking-buffer-removed-hook' are called *before*
        ;; tracking-buffers modification, so use advices instead
        (advice-add 'tracking-add-buffer :after 'telega-mode-line-update)
        (advice-add 'tracking-remove-buffer :after 'telega-mode-line-update)
        (telega-mode-line-update))

    (setq global-mode-string
          (delq 'telega-mode-line-format global-mode-string))
    (advice-remove 'telega--on-updateUnreadMessageCount
                   'telega-mode-line-update)
    (advice-remove 'telega--on-updateUnreadChatCount
                   'telega-mode-line-update)
    (advice-remove 'telega--on-updateChatUnreadMentionCount
                   'telega-mode-line-update)
    (remove-hook 'telega-ready-hook 'telega-mode-line-update)
    (remove-hook 'telega-chats-fetched-hook 'telega-mode-line-update)
    (remove-hook 'telega-kill-hook 'telega-mode-line-update)
    (advice-remove 'tracking-add-buffer 'telega-mode-line-update)
    (advice-remove 'tracking-remove-buffer 'telega-mode-line-update)
    ))

(defun telega-mode-line-update (&rest _ignored)
  "Update value for `telega-mode-line-string'."
  (setq telega-mode-line-string
        (when (telega-server-live-p)
          (format-mode-line telega-mode-line-string-format)))
  (force-mode-line-update))


;; Animation autoplay mode
(defcustom telega-autoplay-messages '(messageAnimation)
  "Message types to automatically play when received."
  :type 'list
  :group 'telega)

(defun telega-autoplay-on-msg (msg)
  "Automatically play contents of the message MSG.
Play in muted mode."
  (when (and (not (plist-get msg :is_outgoing))
             (memq (telega--tl-type (plist-get msg :content))
                   telega-autoplay-messages))
    (telega-msg-open-content msg)))

;;;###autoload
(define-minor-mode telega-autoplay-mode
  "Automatically play animation messages."
  :init-value nil :global t :group 'telega
  (if telega-autoplay-mode
      (add-hook 'telega-chat-post-message-hook 'telega-autoplay-on-msg)
    (remove-hook 'telega-chat-post-message-hook 'telega-autoplay-on-msg)))

(provide 'telega-modes)

;;; telega-modes.el ends here

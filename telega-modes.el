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

;; See https://zevlg.github.io/telega.el/#minor-modes

;;; Code:

(require 'telega-customize)
(require 'telega-server)
(require 'telega-filter)
(require 'telega-util)

(defvar tracking-buffers)

(defgroup telega-modes nil
  "Customization for telega minor modes."
  :prefix "telega-"
  :group 'telega)

;;; ellit-org: minor-modes
;; ** telega-mode-line-mode
;;
;; Global minor mode to display =telega= status in modeline.
;;
;; Enable with ~(telega-mode-line-mode 1)~ or at =telega= load time:
;; #+begin_src emacs-lisp
;; (add-hook 'telega-load-hook 'telega-mode-line-mode)
;; #+end_src
;;
;; Customizable options:
;;
;; - {{{user-option(telega-mode-line-string-format, 2)}}}
(defcustom telega-mode-line-string-format
  '("   " (:eval (telega-mode-line-icon))
    (:eval (telega-mode-line-online-status))
    (:eval (when telega-use-tracking-for
             (telega-mode-line-tracking)))
    (:eval (telega-mode-line-unread-unmuted))
    (:eval (telega-mode-line-mentions 'messages)))
  "Format in mode-line-format for `telega-mode-line-string'."
  :type 'list
  :group 'telega-modes)

(defvar telega-mode-line-string ""
  "Used to cache formatted modeline string.")

(defcustom telega-mode-line-format
  (list '(:eval (when (telega-server-live-p)
                  telega-mode-line-string)))
  "Format in mode-line-format to be used as part of `global-mode-string'."
  :type 'sexp
  :group 'telega-modes
  :risky t)

(defvar telega-mode-line--logo-image-cache nil "Cached loaded logo image.")
(defun telega-mode-line-logo-image ()
  "Return telega logo image to be used in modeline."
  (let* ((box-line-width-raw
          (plist-get (face-attribute 'mode-line :box) :line-width))
         (box-line-width
          (if (consp box-line-width-raw)
              (car box-line-width-raw)
            (or box-line-width-raw 0)))
         (mode-line-height
          (+ (telega-chars-xheight 1 'mode-line)
             ;; NOTE: height adjustment only needed if box-line-width
             ;; is negative. See https://t.me/emacs_telega/26677
             (if (< box-line-width 0)
                 (* 2 box-line-width)
               0))))
    (if (eq mode-line-height
            (plist-get (cdr telega-mode-line--logo-image-cache) :height))
        telega-mode-line--logo-image-cache
      (setq telega-mode-line--logo-image-cache
            (find-image
             (list (list :type 'svg :file "etc/telega-logo.svg"
                         :ascent 'center :mask 'heuristic
                         :height mode-line-height)
                   (list :type (when (fboundp 'imagemagick-types) 'imagemagick)
                         :file "etc/telega-logo.png"
                         :ascent 'center :mask 'heuristic
                         :height mode-line-height)
                   (list :type 'xpm :file "etc/telega-logo.xpm"
                         :ascent 'center
                         :height mode-line-height)))))))

(defun telega-mode-line-icon ()
  "Return telegram logo icon to be used in modeline."
  (propertize telega-symbol-telegram
              'display (telega-mode-line-logo-image)
              'local-map (eval-when-compile
                           (make-mode-line-mouse-map 'mouse-1 'telega))
              'mouse-face 'mode-line-highlight
              'help-echo "Click to show telega root buffer"))

(defun telega-mode-line-online-status ()
  "Return online status symbol."
  (if (telega-user-online-p (telega-user-me))
      telega-symbol-online-status
    (propertize telega-symbol-online-status 'face 'shadow)))

(defun telega-mode-line--online-status-update (event)
  (when (eq (plist-get event :user_id) telega--me-id)
    (telega-mode-line-update)))

(defmacro telega-mode-line-filter-gen (filter-spec)
  "Generate filtering command for `telega-mode-line-mode' using FILTER-SPEC."
  `(lambda ()
     (interactive)
     (telega nil)
     (telega-view-default)
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
  (let* ((m-chats (telega-filter-chats telega--ordered-chats '(mention)))
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

(defun telega-mode-line-update (&rest _ignored)
  "Update value for `telega-mode-line-string'."
  (setq telega-mode-line-string
        (when (telega-server-live-p)
          (format-mode-line telega-mode-line-string-format)))
  (force-mode-line-update 'all))

;;;###autoload
(define-minor-mode telega-mode-line-mode
  "Toggle display of the unread chats/mentions in the modeline."
  :init-value nil :global t :group 'telega-modes
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
        (advice-add 'telega--on-updateUserStatus
                    :after 'telega-mode-line--online-status-update)
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
    (advice-remove 'telega--on-updateUserStatus
                   'telega-mode-line--online-status-update)
    (remove-hook 'telega-ready-hook 'telega-mode-line-update)
    (remove-hook 'telega-chats-fetched-hook 'telega-mode-line-update)
    (remove-hook 'telega-kill-hook 'telega-mode-line-update)
    (advice-remove 'tracking-add-buffer 'telega-mode-line-update)
    (advice-remove 'tracking-remove-buffer 'telega-mode-line-update)
    ))

;;; ellit-org: minor-modes
;; ** telega-appindicator-mode
;;
;; Global minor mode to display =telega= status in system tray.  This
;; mode requires appindicator support in the =telega-server=.  To add
;; appindicator support to =telega-server=, please install
;; =libappindicator3-dev= system package and rebuild =telega-server=
;; with {{{kbd(M-x telega-server-build RET}}}.
;;
;; Screenshot of system tray with enabled =telega= appindicator:
;; [[https://zevlg.github.io/telega/screen-appindicator.png]]
;;
;; Enable with ~(telega-appindicator-mode 1)~ or at =telega= load time:
;; #+begin_src emacs-lisp
;; (add-hook 'telega-load-hook 'telega-appindicator-mode)
;; #+end_src
;;
;; Customizable options:
;; - {{{user-option(telega-appindicator-use-label, 2)}}}
;; - {{{user-option(telega-appindicator-icon-colors, 2)}}}
;; - {{{user-option(telega-appindicator-show-account-name, 2)}}}
;; - {{{user-option(telega-appindicator-show-mentions, 2)}}}
;; - {{{user-option(telega-appindicator-labels, 2)}}}
(declare-function telega "telega" (&optional arg))
(declare-function telega-account-current "telega")
(declare-function telega-kill "telega" (force))

(defcustom telega-appindicator-use-label nil
  "Non-nil to add text labels to the icon.
Otherwise use just icon to show info.
labels are not supported by XEMBED based system trays, such as
`exwm-systemtray' or `polybar'."
  :package-version '(telega . "0.7.20")
  :type 'boolean
  :group 'telega-modes)

(defcustom telega-appindicator-icon-colors
  '((offline "white" "black" nil)
    (online "#7739aa" "white" "#00ff00")
    (connecting "gray" "white" "white"))
  "Colors to use for offline/online appindicator icon.
Alist with `offline', `online' or `connecting' as key, and value in form
(CIRCLE-COLOR TRIANGLE-COLOR ONLINE-CIRCLE-COLOR)."
  :package-version '(telega . "0.7.34")
  :type 'list
  :group 'telega-modes)

(defcustom telega-appindicator-show-account-name t
  "*Non-nil to show current account name in appindicator label.
Applied only if `telega-appindicator-use-label' is non-nil."
  :package-version '(telega . "0.7.2")
  :type 'boolean
  :group 'telega-modes)

(defcustom telega-appindicator-show-mentions t
  "*Non-nil to show number of mentions in appindicator label.
Applied only if `telega-appindicator-use-label' is non-nil."
  :package-version '(telega . "0.7.2")
  :type 'boolean
  :group 'telega-modes)

(defcustom telega-appindicator-labels
  '("❶" "❷" "❸" "❹" "❺" "❻" "❼" "❽" "❾" "❿"
    "⓫" "⓬" "⓭" "⓮" "⓯" "⓰" "⓱" "⓲" "⓳" "⓴")
  "List of number labels to use for the number of unread unmuted chats.
Use this labels instead of plain number.
Set to nil to use plain number."
  :package-version '(telega . "0.7.2")
  :type 'list
  :group 'telega-modes)

(defvar telega-appindicator--cached-icons nil
  "Cached icons for offline/online statuses.")

;;;###autoload
(define-minor-mode telega-appindicator-mode
  "Toggle display of the unread chats/mentions in the system tray."
  :init-value nil :global t :group 'telega-modes

  (if telega-appindicator-mode
      (progn
        (advice-add 'telega--on-updateUnreadChatCount
                    :after 'telega-appindicator-update)
        (advice-add 'telega--on-updateChatUnreadMentionCount
                    :after 'telega-appindicator-update)
        (add-hook 'telega-ready-hook 'telega-appindicator-init)
        (add-hook 'telega-chats-fetched-hook 'telega-appindicator-update)
        (add-hook 'telega-online-status-hook 'telega-appindicator-update)
        (add-hook 'telega-connection-state-hook 'telega-appindicator-update)
        (when (telega-server-live-p)
          (telega-appindicator-init)))

    (remove-hook 'telega-connection-state-hook 'telega-appindicator-update)
    (remove-hook 'telega-online-status-hook 'telega-appindicator-update)
    (remove-hook 'telega-chats-fetched-hook 'telega-appindicator-update)
    (remove-hook 'telega-ready-hook 'telega-appindicator-init)
    (advice-remove 'telega--on-updateChatUnreadMentionCount
                   'telega-appindicator-update)
    (advice-remove 'telega--on-updateUnreadChatCount
                   'telega-appindicator-update)
    ;; Deactivate appindicator
    (when (telega-server-live-p)
      (telega-server--send "status passive" "appindicator"))
    ))

(defun telega-appindicator--gen-svg-icon (&optional label)
  "Generate svg icon to be used in appindicator.
Return filename of the generated icon."
  (let* ((state (cond ((eq telega--conn-state 'Connecting) 'connecting)
                      ((funcall telega-online-status-function) 'online)
                      (t 'offline)))
         (cached-label (concat (symbol-name state) label))
         (cached-entry (assoc cached-label telega-appindicator--cached-icons)))
    ;; NOTE: Check cached icon is still accessible
    (when (and cached-entry (not (file-exists-p (cdr cached-entry))))
      (setq telega-appindicator--cached-icons
            (remove cached-entry telega-appindicator--cached-icons)
            cached-entry nil))

    (or (cdr cached-entry)
        (let* ((w 48) (h 48) (logo-w 36)
               (svg (telega-svg-create w h))
               (colors (cdr (assq state telega-appindicator-icon-colors))))
          (svg-circle svg (/ h 2) (/ h 2) (/ h 2)
                      :fill-color (nth 0 colors))
          (when (nth 2 colors)
            (svg-circle svg (/ h 6) (/ h 6) (/ h 6)
                        :fill-color (nth 2 colors)))
          (telega-svg-telega-logo svg logo-w
            :fill-color (nth 1 colors)
            :transform (format "translate(%f, %f)"
                               (/ (- h logo-w) 3) (- h logo-w)))
          ;; Label
          (when (and label (not (string-empty-p label)))
            (let ((fsz 36))
              ;; XXX: white background below the label
              ;; TODO: find a better way in the future
              (svg-circle svg (- w (/ fsz 2)) (- h (/ fsz 2)) (1- (/ fsz 2))
                          :fill-color "white")
              (svg-text svg label
                        :font-size fsz
                        :font-weight "bold"
                        :fill "#ff0000"
                        :font-family "monospace"
                        :stroke-width 1
                        :stroke-color "white"
                        ;; XXX insane X/Y calculation
                        :x (- w fsz)
                        :y (- h (/ fsz 8)))))

          (let ((image (telega-svg-image svg))
                (svg-icon-file (telega-temp-name "appindicator-icon" ".svg")))
            (write-region (plist-get (cdr image) :data)
                          nil svg-icon-file nil 'quiet)
            ;; Cache it
            (setq telega-appindicator--cached-icons
                  (cons (cons cached-label svg-icon-file)
                        telega-appindicator--cached-icons))
            svg-icon-file)))))

(defun telega-appindicator-init ()
  "Initialize appindicator."
  (when telega-appindicator-mode
    (telega-server--send
     (concat "setup " (telega-appindicator--gen-svg-icon))
     "appindicator")
    (telega-appindicator-update)))

(defun telega-appindicator-update (&rest _ignored)
  "Update appindicator label."
  (when telega-appindicator-mode
    (let* ((account
            (when (and telega-appindicator-use-label
                       telega-appindicator-show-account-name)
              (car (telega-account-current))))
           (uu-chats-num
            (or (plist-get telega--unread-chat-count :unread_unmuted_count)
                0))
           (uu-chats-str
            (unless (zerop uu-chats-num)
              (or (nth (1- uu-chats-num) telega-appindicator-labels)
                  (number-to-string uu-chats-num))))
           (mentions-num
            (or (when (and telega-appindicator-use-label
                           telega-appindicator-show-mentions)
                  (length (telega-filter-chats
                           telega--ordered-chats '(mention))))
                0))
           (mentions-str
            (unless (zerop mentions-num)
              (format "@%d" mentions-num)))
           (label-strings
            (remove nil (list account
                              (when (and account
                                         (or uu-chats-str mentions-str))
                                "-")
                              uu-chats-str
                              mentions-str)))
           (new-label (mapconcat #'identity label-strings " "))
           (icon-filename
            (telega-appindicator--gen-svg-icon
             (unless telega-appindicator-use-label
               new-label))))
      (telega-server--send (concat "icon " icon-filename) "appindicator")
      (when telega-appindicator-use-label
        (telega-server--send (concat "label " new-label) "appindicator")))))

(defun telega-appindicator--on-event (event)
  "Function called when event from appindicator is received."
  (cond ((string= event "open")
         ;; NOTE: Raise Emacs frame and open rootbuf
         (x-focus-frame nil)
         (telega)
         ;; If there is single important chat, then switch to it
         (let ((ichats (telega-filter-chats
                        telega--ordered-chats
                        '(or mention (and unread unmuted)))))
           (when (= 1 (length ichats))
             (telega-switch-important-chat (car ichats)))))

        ((string= event "quit")
         (x-focus-frame nil)
         (telega-kill nil))

        (t
         (message "telega-server: Unknown appindicator-event: %s" event))))


;;; ellit-org: minor-modes
;; ** telega-autoplay-mode
;;
;; Global minor mode to automatically open content for incoming
;; messages.  Message automatically opens if its type is in the
;; ~telega-autoplay-messages~ list and message content is fully
;; observable.
;;
;; Enable with ~(telega-autoplay-mode 1)~ or at =telega= load time:
;; #+begin_src emacs-lisp
;; (add-hook 'telega-load-hook 'telega-autoplay-mode)
;; #+end_src
;;
;; Customizable options:
;; - {{{user-option(telega-autoplay-for, 2)}}}
;; - {{{user-option(telega-autoplay-outgoing, 2)}}}
;; - {{{user-option(telega-autoplay-messages, 2)}}}
(defcustom telega-autoplay-for 'all
  "Chat Filter for chats where to automatically open content."
  :type 'list
  :group 'telega-modes)

(defcustom telega-autoplay-outgoing t
  "Non-nil to play outgoing messages as well."
  :type 'boolean
  :group 'telega-modes)

(defcustom telega-autoplay-messages
  (nconc (when telega-animation-play-inline
           '(messageAnimation))
         (when telega-sticker-animated-play
           '(messageSticker)))
  "Message types to automatically play when received."
  :type 'list
  :group 'telega-modes)

(defun telega-autoplay-on-msg (msg)
  "Automatically play contents of the message MSG.
Play in muted mode."
  (when (and (or (not (plist-get msg :is_outgoing))
                 (and telega-autoplay-outgoing
                      ;; i.e. sent successfully
                      (not (plist-get msg :sending_state))))
             (memq (telega--tl-type (plist-get msg :content))
                   telega-autoplay-messages)
             (telega-chat-match-p (telega-msg-chat msg) telega-autoplay-for)
             (telega-msg-observable-p msg))
    ;; NOTE: special case for sticker messages
    (cl-case (telega--tl-type (plist-get msg :content))
      (messageSticker
       (let ((sticker (telega--tl-get msg :content :sticker)))
         (when (plist-get sticker :is_animated)
           (telega-sticker--animate sticker))))

      (t
       (telega-msg-open-content msg)))))

;;;###autoload
(define-minor-mode telega-autoplay-mode
  "Automatically play animation messages."
  :init-value nil :global t :group 'telega-modes
  (if telega-autoplay-mode
      (add-hook 'telega-chat-post-message-hook 'telega-autoplay-on-msg)
    (remove-hook 'telega-chat-post-message-hook 'telega-autoplay-on-msg)))


;;; ellit-org: minor-modes
;; ** telega-squash-message-mode
;;
;; Minor mode for chatbuf to squash messages into single one while
;; nobody saw this.
;;
;; Squashing mean adding contents of the new message to the previous
;; message by editing contents of the previous message.
;;
;; New message in chat is squashed into your previous message only if
;; all the conditions are met:
;;
;; 1. Last message in chat is sent by you
;; 2. Nobody seen your last message
;; 3. Last and new message are both text messages
;; 4. Last message can be edited
;; 5. Last and new messages are *not* replying to any message
;; 6. Last message has no associated web-page
;; 7. New message has no ~messageSendOptions~ to avoid squashing
;;    scheduled messages or similar
;;
;; Can be enabled globally in all chats matching
;; ~telega-squash-message-mode-for~ (see below) chat filter with
;; ~(global-telega-squash-message-mode 1)~ or by adding:
;;
;; #+begin_src emacs-lisp
;; (add-hook 'telega-load-hook 'global-telega-squash-message-mode)
;; #+end_src
;;
;; Customizable options:
;;
;; - {{{user-option(telega-squash-message-mode-for, 2)}}}
(defcustom telega-squash-message-mode-for
  '(not (or saved-messages (type channel)))
  "*Chat filter for `global-telega-squash-message-mode'.
Global squash message mode enables message squashing only in
chats matching this chat filter."
  :type 'list
  :group 'telega-modes)

;;;###autoload
(define-minor-mode telega-squash-message-mode
  "Toggle message squashing minor mode."
  :init-value nil
  :lighter " ◁Squash"
  :group 'telega-modes
  (if telega-squash-message-mode
      (advice-add 'telega--sendMessage
                  :around 'telega-squash-message--send-message)
    (advice-remove 'telega--sendMessage
                   'telega-squash-message--send-message)))

(defun telega-squash-message-mode--maybe (&optional arg)
  (when (telega-chat-match-p telega-chatbuf--chat telega-squash-message-mode-for)
    (telega-squash-message-mode arg)))

;;;###autoload
(define-minor-mode global-telega-squash-message-mode
  "Global mode to squashing messages."
  :init-value nil :global t :group 'telega-modes
  (if global-telega-squash-message-mode
      (progn
        (add-hook 'telega-chat-mode-hook 'telega-squash-message-mode--maybe)
        (dolist (buf (telega-chat-buffers))
          (with-current-buffer buf
            (telega-squash-message-mode--maybe 1))))

    (remove-hook 'telega-chat-mode-hook 'telega-squash-message-mode--maybe)
    (dolist (buf (telega-chat-buffers))
      (with-current-buffer buf
        (telega-squash-message-mode -1)))))

(defun telega-squash-message--squash (chat imc reply-to-msg options)
  "Return non-nil if message has been squashed."
  (with-telega-chatbuf chat
    (when (and telega-squash-message-mode
               ;; Check 3., 5. and 7.0 for new message
               (not reply-to-msg)
               (not options)
               (eq (telega--tl-type imc) 'inputMessageText))
      (let ((last-msg (plist-get chat :last_message))
            (last-read-id (plist-get chat :last_read_outbox_message_id)))
        (when (and last-msg
                   ;; Checking for 1. 2. 3. 4. and 5.
                   (telega-msg-by-me-p last-msg)
                   (< last-read-id (plist-get last-msg :id))
                   (plist-get last-msg :can_be_edited)
                   (telega-msg-type-p 'messageText last-msg)
                   (zerop (plist-get last-msg :reply_to_message_id))
                   ;; Check for 6.
                   (not (telega--tl-get last-msg :content :web_page))
                   )

          ;; Squashing IMC with `last-msg' by modifying IMC inplace
          (plist-put imc :text (telega-fmt-text-desurrogate
                                (telega-fmt-text-concat
                                 (telega--tl-get last-msg :content :text)
                                 (telega-string-fmt-text "\n")
                                 (plist-get imc :text))))
          (telega--editMessageText last-msg imc)
          t)))))

(defun telega-squash-message--send-message (send-msg-fun chat imc &optional reply-to-msg options &rest args)
  "Advice for `telega--sendMessage' used to squash messages."
  (unless (telega-squash-message--squash chat imc reply-to-msg options)
    (apply send-msg-fun chat imc reply-to-msg options args)))


;;; ellit-org: minor-modes
;; ** telega-image-mode
;;
;; Major mode to view images in chatbuf.  Same as ~image-mode~,
;; however has special bindings:
;;
;; - {{{where-is(telega-image-next,telega-image-mode-map)}}} ::
;;   {{{fundoc(telega-image-next)}}}
;;
;; - {{{where-is(telega-image-prev,telega-image-mode-map)}}} ::
;;   {{{fundoc(telega-image-prev)}}}
;;
;; To view high resolution image in chatbuf with ~telega-image-mode~
;; press {{{kbd(RET)}}} on the message with photo.
(require 'image-mode)

(declare-function telega-chat-title "telega-chat" (chat &optional with-username))
(declare-function telega-chat-brackets "telega-chat" (chat))
(declare-function telega-chatbuf--next-msg "telega-chat" (msg predicate &optional backward))
(declare-function telega-chatbuf--goto-msg "telega-chat" (msg-id &optional highlight))

(defvar telega-image--message nil
  "Message corresponding to image currently viewed.")
(make-variable-buffer-local 'telega-image--message)

(defvar telega-image-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map image-mode-map)
    (define-key map "n" 'telega-image-next)
    (define-key map "p" 'telega-image-prev)
    (define-key map "q" 'telega-image-quit)
    map))

(define-derived-mode telega-image-mode image-mode nil
  "Major mode to view images from chat buffer."
  (setq mode-name
        (concat "◁Image" (when image-type (format "[%s]" image-type))))
  )

(defun telega-image-mode-p (buffer-name &rest _unused)
  "Return non-nil if buffer named BUFFER-NAME has `telega-image-mode' major-mode.
Could be used as condition function in `display-buffer-alist'."
  (with-current-buffer buffer-name
    (eq major-mode 'telega-image-mode)))

(defun telega-image-view-file (tl-file &optional for-msg)
  "View image in telegram TL-FILE from message FOR-MSG."
  (cl-assert (telega-file--downloaded-p tl-file))
  ;; NOTE: Use `pop-to-buffer' so `display-buffer-alist' is considered
  ;; when showing the image
  (pop-to-buffer-same-window
   (with-current-buffer (find-file-noselect
                         (telega--tl-get tl-file :local :path) nil t)
     (telega-image-mode)
     (setq telega-image--message for-msg)
     (current-buffer))))

(defun telega-image-next (&optional backward)
  "Show next image in chat."
  (interactive "P")
  (unless telega-image--message
    (user-error "No telega message associated with the image"))

  (if-let ((next-image-msg (telega-chatbuf--next-msg
                            telega-image--message
                            (apply-partially #'telega-msg-type-p 'messagePhoto)
                            backward)))
      ;; Download highres photo
      (let* ((photo (telega--tl-get next-image-msg :content :photo))
             (hr (telega-photo--highres photo))
             (hr-file (telega-file--renew hr :photo))
             (oldbuffer (current-buffer)))
        ;; Goto corresponding message in the chatbuf
        (with-telega-chatbuf (telega-msg-chat next-image-msg)
          (when (telega-chatbuf--goto-msg (plist-get next-image-msg :id))
            (setq telega-chatbuf--refresh-point t)))

        (telega-file--download hr-file 32
          (lambda (tl-file)
            (if (not (telega-file--downloaded-p tl-file))
                ;; Show downloading progress in modeline
                (let ((progress (telega-file--downloading-progress tl-file)))
                  (message "Downloading.. %d%%" (* progress 100)))

              ;; TL-FILE Downloaded
              (telega-image-view-file tl-file next-image-msg)
              (ignore-errors
                (kill-buffer oldbuffer))))))

    ;; `next-image-msg' is nil (not found)
    ;; TODO: Probably need to fetch older/newer messages from the history
    (unless next-image-msg
      (let* ((chat (telega-msg-chat telega-image--message))
             (brackets (telega-chat-brackets chat)))
        (user-error "No %s image in %s%s%s"
                    (if backward "previous" "next")
                    (or (car brackets) "[")
                    (telega-chat-title chat)
                    (or (cadr brackets) "]"))))
    ))

(defun telega-image-prev ()
  "Show previous image in chat."
  (interactive)
  (telega-image-next 'previous))

(defun telega-image-quit ()
  "Kill image buffer and its window."
  (interactive)
  (quit-window 'kill))


;;; ellit-org: minor-modes
;; ** telega-edit-file-mode
;;
;; {{{fundoc1(telega-edit-file-mode)}}}
;; In this mode {{{kbd(C-x C-s)}}} will save file to Telegram cloud.
;; To enable ~telega-edit-file-mode~ for files opened from message
;; with {{{kbd(RET)}}}, use:
;;
;; #+BEGIN_SRC emacs-lisp
;;   (add-hook 'telega-open-file-hook 'telega-edit-file-mode)
;; #+END_SRC
(declare-function telega-chat-title-with-brackets "telega-chat" (chat &optional with-username-delim))
(declare-function telega-chatbuf--gen-input-file "telega-chat" (filename &optional file-type preview-p upload-callback))

(defvar telega-edit-file-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap save-buffer] 'telega-edit-file-save-buffer)

    (define-key map [menu-bar goto-message]
      '("Goto Telegram Message" . telega-edit-file-goto-message))
    (define-key map [menu-bar turn-off]
      '("Turn off minor mode" . telega-edit-file-mode))
    map))

;;;###autoload
(define-minor-mode telega-edit-file-mode
  "Minor mode to edit files from Telegram messages.
Can be enabled only for content from editable messages."
  :lighter " ◁Edit"
  :map 'telega-edit-file-mode-map

  (if telega-edit-file-mode
    (let* ((msg telega--help-win-param)
           (chat (and msg (telega-msg-chat msg))))
      (if (not (plist-get msg :can_be_edited))
          ;; No message or message can't be edited
          (telega-edit-file-mode -1)

        (setq mode-line-buffer-identification
              (list (propertized-buffer-identification "%b")
                    (propertize
                     (concat "◁" (telega-chat-title-with-brackets chat))
                     'face 'mode-line-buffer-id
                     'help-echo "mouse-1: To goto telegram message"
                     'mouse-face 'mode-line-highlight
                     'local-map
                     (eval-when-compile
                       (make-mode-line-mouse-map
                        'mouse-1 #'telega-edit-file-goto-message)))))))

    (setq mode-line-buffer-identification
          (propertized-buffer-identification "%12b"))))

(defun telega-edit-file-goto-message ()
  "Goto corresponding message."
  (interactive)
  (let ((msg telega--help-win-param))
    (unless msg
      (user-error "No Telegram message associated with the buffer"))
    (telega-msg-goto-highlight msg)))

(defun telega-edit-file--upload-callback (ufile)
  "Callback for the file uploading progress.
UFILE specifies Telegram file being uploading."
  (cond ((telega-file--uploaded-p ufile)
         (message "Uploaded %s" (telega--tl-get ufile :local :path)))

        ((telega-file--uploading-p ufile)
         (message
          "%s"
          (telega-ins--as-string
           (telega-ins "Uploading [")
           (let ((progress (telega-file--uploading-progress ufile)))
             (telega-ins-progress-bar
              progress 1.0 30 telega-symbol-upload-progress)
             (telega-ins-fmt "]%3d%%" (round (* progress 100)))))))
        ))

(defun telega-edit-file-save-buffer ()
  "Save buffer associated with message."
  (interactive)

  (let ((save-silently t))
    (save-buffer))

  (when-let ((msg telega--help-win-param))
    (unless (plist-get msg :can_be_edited)
      (user-error "Telega: message can't be edited"))

    (telega--editMessageMedia
     msg
     (list :@type "inputMessageDocument"
           :document (let ((telega-chat-upload-attaches-ahead t))
                       (telega-chatbuf--gen-input-file
                        (buffer-file-name) 'Document nil
                        #'telega-edit-file--upload-callback))))))


;;; ellit-org: minor-modes
;; ** telega-highlight-text-mode
;;
;; =jit-lock= powered minor mode to highlight given regexp.
;;
;; Similar to =hi-lock=, however supports =jit-lock= for highlighting
;; dynamic content.
(defface telega-highlight-text-face
  '((t :inherit match))
  "Face used to highlight text in `telega-highlight-text-mode'."
  :group 'telega-faces)

(defvar telega-highlight-text-regexp nil
  "Regexp to highlight.")
(make-variable-buffer-local 'telega-highlight-text-regexp)

(defun telega-highlight-text-region (beg end &optional _loudly)
  "Highlight `telega-highlight-text-regexp' in the region from BEG to END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward telega-highlight-text-regexp end t)
      (let ((overlay (make-overlay (match-beginning 0)
                                   (match-end 0))))
        (overlay-put overlay 'telega-highlight-overlay t)
        (overlay-put overlay 'face 'telega-highlight-text-face))
      (goto-char (match-end 0)))))

(define-minor-mode telega-highlight-text-mode
  "Minor mode to highlight text."
  :lighter " ◁Highlight"
  (if telega-highlight-text-mode
      (progn
        (cl-assert (and (stringp telega-highlight-text-regexp)
                        (not (string-empty-p telega-highlight-text-regexp))))
        (telega-highlight-text-region (point-min) (point-max))
        (jit-lock-register #'telega-highlight-text-region))

    (jit-lock-unregister #'telega-highlight-text-region)
    (remove-overlays nil nil 'telega-highlight-overlay t)))

(defun telega-highlight-text (text-regexp)
  "Highlight TEXT-REGEXP in the dynamic buffer."
  (setq telega-highlight-text-regexp text-regexp)
  (telega-highlight-text-mode 1))


;;; ellit-org: minor-modes
;; ** telega-patrons-mode
;;
;; Emphasize =telega=
;; [[https://opencollective.com/telega#section-contributors][patrons]]
;; by drawing special elements (telega cat ears) above the patron's
;; avatar, like: [[https://zevlg.github.io/telega/telega-patron-ava.png]]
;;
;; In addition:
;; - Display "Telega Patron Since: <date>" note in the patron's Chat/User description.
;; - All [[https://zevlg.github.io/telega.el/#telega-storiesel--display-emacs-stories-in-the-dashboard][Emacs Stories]] from =telega= patrons are automatically considered "Featured".
;;
;; If you are already =telega= patron and not in the
;; ~telega-patrons-alist~ list, please [[https://t.me/zevlg][write
;; me]].
;;
;; ~telega-patrons-mode~ is enabled by default.
(defconst telega-patrons-alist
  '((82439953 :source opencollective :since_date 1609459200)
    (781215372 :source opencollective :since_date 1609459200)
    (275409096 :source opencollective :since_date 1610236800)
    (356787489 :source opencollective :since_date 1611532800)
    (1648334150 :source opencollective :since_date 1611792000)
    (205887307 :source opencollective :since_date 1612224000)
    (110622853 :source opencollective :since_date 1612310400)
    (388827905 :source private :since_date 1614804711)
    (835801 :source opencollective :since_date 1614897092)
    (1358845605 :source opencollective :since_date 1617444333)
    )
  "Alist of telega patrons.")

(defun telega-msg-sender-patron-p (sender)
  "Return non-nil if SENDER is a telega patron.
Return patron info, or nil if SENDER is not a telega patron."
  (cdr (assq (plist-get sender :id) telega-patrons-alist)))

(defun telega-patrons--avatar-emphasize (origfun sender file &optional cheight
                                                 addon-fun)
  "Emphasize SENDER's avatar if it is in the `telega-patrons-alist'."
  (funcall origfun sender file cheight
           (if-let ((patron (telega-msg-sender-patron-p sender)))
               (lambda (svg circle)
                 (when addon-fun
                   (funcall addon-fun svg circle))
                 (let* ((svg-w (dom-attr svg 'width))
                        (cx (nth 0 circle))
                        (cy (nth 1 circle))
                        (cr (nth 2 circle))
                        (ear-w (* 0.4 cr))
                        (x-off (- cx cr))
                        (y-off (- cy cr))
                        (color
                         (nth (if (eq (frame-parameter nil 'background-mode)
                                      'light)
                                  0 1)
                              (telega-msg-sender-color sender)))
                        (svg-color
                         (when color
                           (telega-color-name-as-hex-2digits color))))
                   (telega-svg-telega-logo
                    svg ear-w
                    :transform (format "scale(-1, 1) translate(-%f, %f)"
                                       (+ ear-w x-off) y-off)
                    :fill svg-color)
                   (telega-svg-telega-logo
                    svg ear-w
                    :transform (format "translate(%f, %f)"
                                       (- svg-w ear-w x-off) y-off)
                    :fill svg-color)))
             addon-fun)))

(defun telega-patrons--avatar-title-text (origfun sender)
  "Emphasize text variant avatar for patronns."
  (if-let ((patron (telega-msg-sender-patron-p sender)))
      (concat "⸨"
              (propertize (substring (telega-msg-sender-title sender) 0 1)
                          'face (telega-msg-sender-title-faces sender))
              "⸩")
    (funcall origfun sender)))

(define-minor-mode telega-patrons-mode
  "Global mode to emphasize telega patrons."
  :init-value nil :global t :group 'telega-modes
  (if telega-patrons-mode
      (progn
        (advice-add 'telega-avatar--create-image
                    :around 'telega-patrons--avatar-emphasize)
        (advice-add 'telega-avatar--title-text
                    :around 'telega-patrons--avatar-title-text)
        )
    (advice-remove 'telega-avatar--title-text
                   'telega-patrons--avatar-title-text)
    (advice-remove 'telega-avatar--create-image
                   'telega-patrons--avatar-emphasize)
    ))

(provide 'telega-modes)

;;; telega-modes.el ends here

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
(declare-function telega-account-current "telega")
(declare-function telega "telega" (&optional arg))
(declare-function telega-kill "telega" (force))

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
    (:eval (car (telega-account-current)))
    (:eval (telega-mode-line-online-status))
    (:eval (when telega-use-tracking-for
             (telega-mode-line-tracking)))
    (:eval (telega-mode-line-unread-unmuted))
    (:eval (telega-mode-line-mentions 'messages)))
  "Format in mode-line-format for `telega-mode-line-string'."
  :type 'sexp
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
          (plist-get (face-attribute telega--default-face :box) :line-width))
         (box-line-width
          (if (consp box-line-width-raw)
              (car box-line-width-raw)
            (or box-line-width-raw 0)))
         (mode-line-height
          (+ (telega-chars-xheight 1)
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
                         :scale 1 :ascent 'center :mask 'heuristic
                         :height mode-line-height)
                   (list :type (when (fboundp 'imagemagick-types) 'imagemagick)
                         :file "etc/telega-logo.png"
                         :scale 1 :ascent 'center :mask 'heuristic
                         :height mode-line-height)
                   (list :type 'xpm :file "etc/telega-logo.xpm"
                         :scale 1 :ascent 'center
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
  ;; NOTE: At start time user info might not be available
  (when-let ((me-user (telega-user-me 'locally)))
    (if (telega-user-online-p me-user)
        telega-symbol-online-status
      (propertize telega-symbol-online-status 'face 'telega-shadow))))

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
  (when telega-mode-line-mode
    (setq telega-mode-line-string
          (when (telega-server-live-p)
            (telega-format-mode-line telega-mode-line-string-format)))
    (force-mode-line-update 'all)))

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
;; =libappindicator3-dev= or =libayatana-appindicator3-dev=
;; system package and rebuild =telega-server=
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
  :type '(alist :key-type symbol :value-type (repeat string))
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
  :type '(repeat string)
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
                (svg-icon-file
                 (expand-file-name
                  (concat "appindicator-icon-" cached-label ".svg")
                  telega-temp-dir)))
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
         (let ((ichats (telega-filter-chats telega--ordered-chats 'important)))
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
;; messages.  Message automatically opens if it matches against
;; ~telega-autoplay-msg-temex~ [[#telega-match-expressions][Message
;; Temex]] and message's content is fully observable.
;;
;; Enable with ~(telega-autoplay-mode 1)~ or at =telega= load time:
;; #+begin_src emacs-lisp
;; (add-hook 'telega-load-hook 'telega-autoplay-mode)
;; #+end_src
;;
;; Customizable options:
;; - {{{user-option(telega-autoplay-msg-temex, 2)}}}
;; - {{{user-option(telega-autoplay-custom-emojis, 2)}}}
(defcustom telega-autoplay-msg-temex
  '(type Animation Sticker AnimatedEmoji)
  "Message Temex for messages to automatically play content for."
  :type 'telega-msg-temex
  :options '((and (not outgoing)
                  (or (type Animation Sticker AnimatedEmoji)
                      (web-page :animation)
                      (web-page :sticker))))
  :group 'telega-modes)

(defcustom telega-autoplay-custom-emojis 10
  "Non-nil to automatically play this number of custom emojis in the message."
  :type '(choice (const :tag "Autoplay Disabled" nil)
                 integer)
  :group 'telega-modes)

(defun telega-autoplay-custom-emojis (msg &optional force)
  "Animate custom emojis for the message MSG."
  (when telega-autoplay-custom-emojis
    (when (or force (telega-msg-observable-p msg))
      (let* ((custom-emoji-ids (telega-custom-emoji--ids-for-msg msg))
             (custom-emoji-stickers
              (seq-take (seq-filter
                         (lambda (sticker)
                           (and sticker
                                (not (telega-sticker-static-p sticker))))
                         (mapcar #'telega-custom-emoji-get custom-emoji-ids))
                        telega-autoplay-custom-emojis)))
        (dolist (custom-emoji custom-emoji-stickers)
          (when (and (not (telega-sticker-static-p custom-emoji))
                     telega-sticker-animated-play)
            (telega-sticker--animate custom-emoji))))
      )))

(defun telega-autoplay-on-msg (msg)
  "Automatically play contents of the message MSG.
Play in muted mode."
  (when (telega-msg-observable-p msg)
    ;; Animate custom emojis first
    (telega-autoplay-custom-emojis msg 'observable)

    ;; Then animate message's content itself
    (when (telega-msg-match-p msg telega-autoplay-msg-temex)
      (let* ((content (plist-get msg :content))
             (content-type (telega--tl-type content))
             (web-page (plist-get content :web_page)))
        (cond ((or (eq 'messageAnimation content-type)
                   (plist-get web-page :animation))
               ;; NOTE: special case for animations, animate only those
               ;; which can be animated inline, see
               ;; `telega-animation-play-inline'
               (let ((animation (or (plist-get content :animation)
                                    (plist-get web-page :animation))))
                 (when (telega-animation-play-inline-p animation)
                   (telega-msg-open-animation msg animation))))

              ((or (eq 'messageSticker content-type)
                   (plist-get web-page :sticker))
               ;; NOTE: special case for sticker messages, play animated
               ;; sticker only if `telega-sticker-animated-play' is set
               (let ((sticker (or (plist-get content :sticker)
                                  (plist-get web-page :sticker))))
                 (when (and (not (telega-sticker-static-p sticker))
                            telega-sticker-animated-play)
                   (telega-sticker--animate sticker))))

              (t
               (telega-msg-open-content msg)))))))

(defun telega-autoplay-on-msg--hover-out (msg)
  "Handle hover leaving for the message MSG.
Cancel downloading of the corresporting file."
  (when (telega-msg-match-p msg telega-autoplay-msg-temex)
    (when-let ((file (telega-msg--content-file msg)))
      (telega--cancelDownloadFile file))))

;;;###autoload
(define-minor-mode telega-autoplay-mode
  "Automatically play animation messages."
  :init-value nil :global t :group 'telega-modes
  (if telega-autoplay-mode
      (progn
        (add-hook 'telega-chat-post-message-hook #'telega-autoplay-on-msg)
        (add-hook 'telega-msg-hover-in-hook #'telega-autoplay-on-msg)
        (add-hook 'telega-msg-hover-out-hook #'telega-autoplay-on-msg--hover-out))

    (remove-hook 'telega-msg-hover-out-hook #'telega-autoplay-on-msg--hover-out)
    (remove-hook 'telega-msg-hover-in-hook #'telega-autoplay-on-msg)
    (remove-hook 'telega-chat-post-message-hook #'telega-autoplay-on-msg)))


;;; ellit-org: minor-modes
;; ** telega-squash-message-mode
;;
;; Minor mode for chatbuf to squash messages into single one while
;; nobody saw this.
;;
;; Squashing mean adding contents of the new message to the previous
;; message by editing contents of the previous message.
;;
;; New message in a chat is squashed into your previous message only
;; if all next conditions are met:
;;
;; 1. Last message in chat is sent by you
;; 2. Nobody seen your last message
;; 3. Last and new message are both text messages
;; 4. Last message can be edited
;; 5. Last and new messages are *not* replying to any message
;; 6. Last message has no associated web-page
;; 7. New message has no ~messageSendOptions~ to avoid squashing
;;    scheduled messages or similar
;; 8. New message is sent within ~telega-squash-message-within-seconds~
;;    seconds from last message

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
;; - {{{user-option(telega-squash-message-within-seconds, 2)}}}
(defcustom telega-squash-message-mode-for
  '(not (or saved-messages (type channel)))
  "*Chat Temex for `global-telega-squash-message-mode'.
Global squash message mode enables message squashing only in
chats matching this chat temex."
  :type 'telega-chat-temex
  :group 'telega-modes)

(defcustom telega-squash-message-within-seconds 60
  "Maximum number of seconds between last and new message to apply squashing.
If new message is sent later then this number of seconds, then
squashing is not applied."
  :type 'integer
  :group 'telega-modes)

(defvar telega-squash-message-mode-lighter
  (concat " " (telega-symbol 'mode) "Squash")
  "Lighter for the `telega-squash-message-mode'.")

;;;###autoload
(define-minor-mode telega-squash-message-mode
  "Toggle message squashing minor mode."
  :init-value nil
  :lighter telega-squash-message-mode-lighter
  :group 'telega-modes
  (if telega-squash-message-mode
      (advice-add 'telega--sendMessage
                  :around 'telega-squash-message--send-message)
    (advice-remove 'telega--sendMessage
                   'telega-squash-message--send-message)))

(defun telega-squash-message-mode--maybe (&optional arg)
  (when (telega-chatbuf-match-p telega-squash-message-mode-for)
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
                   (telega-msg-match-p last-msg
                     '(and (prop :can_be_edited)
                           (not is-reply-to-msg)
                           (not is-reply-to-story)
                           (type Text)))
                   ;; Check for 6.
                   (not (telega--tl-get last-msg :content :web_page))
                   ;; Check for 8.
                   (< (- (telega-time-seconds)
                         (if (zerop (plist-get last-msg :edit_date))
                             (plist-get last-msg :date)
                           (plist-get last-msg :edit_date)))
                      telega-squash-message-within-seconds)
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

(declare-function telega-chat-title "telega-chat" (chat &optional no-badges))
(declare-function telega-chatbuf--next-msg "telega-chat" (msg msg-temex &optional backward))
(declare-function telega-chatbuf--goto-msg "telega-chat" (msg-id &optional highlight))

(defcustom telega-image-mode-mode-line-format
  '(" → "
    (:eval (telega-image-mode-mode-line-chat-title))
    " "
    (:eval (telega-image-mode-mode-line-chat-position)))
  "Format for the modeline."
  :type 'sexp
  :group 'telega-modes)

(defvar telega-image--message nil
  "Message corresponding to image currently viewed.")
(make-variable-buffer-local 'telega-image--message)

(defvar telega-image--position (cons nil nil)
  "Position of the image message in the chat's history
To be displayed in the modeline.")
(make-variable-buffer-local 'telega-image--position)

(defun telega-image-mode-mode-line-chat-title ()
  (substring-no-properties
   (telega-chatbuf--name (telega-msg-chat telega-image--message))))

(defun telega-image-mode-mode-line-chat-position ()
  (when (and (car telega-image--position)
             (cdr telega-image--position))
    (format "[%d/%d]"
            (car telega-image--position)
            (cdr telega-image--position))))

(defun telega-image-mode--update-modeline ()
  (setq mode-line-buffer-identification
        (list (propertized-buffer-identification "%b")
              (telega-format-mode-line telega-image-mode-mode-line-format)))
  (force-mode-line-update))

(defun telega-image-mode--chat-position-fetch ()
  "Asynchronously fetch message's position in the chat's history."
  (let ((buf (current-buffer)))
    (telega--getChatMessageCount (telega-msg-chat telega-image--message)
        '(:@type "searchMessagesFilterPhoto") nil
      (lambda (count)
        (with-current-buffer buf
          (setcdr telega-image--position count)
          (when (car telega-image--position)
            (telega-image-mode--update-modeline)))))

    (telega--getChatMessagePosition telega-image--message
        '(:@type "searchMessagesFilterPhoto") nil
      (lambda (count)
        (with-current-buffer buf
          (setcar telega-image--position count)
          (when (cdr telega-image--position)
            (telega-image-mode--update-modeline)))))
    ))

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
        (concat (telega-symbol 'mode) "Image"
                (when image-type (format "[%s]" image-type))))
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
     (telega-image-mode--update-modeline)

     ;; NOTE: fetch chat position only for "Photo" messages, to avoid
     ;; 400 errors
     (when (and for-msg (telega-msg-match-p for-msg '(type Photo)))
       (telega-image-mode--chat-position-fetch))

     (current-buffer))))

(defun telega-image-view-msg (image-msg)
  "View image associated with the IMAGE-MSG message."
  ;; Download highres photo
  (let* ((photo (telega--tl-get image-msg :content :photo))
         (hr (telega-photo--highres photo))
         (hr-file (telega-file--renew hr :photo))
         (oldbuffer (current-buffer)))
    ;; Jump to corresponding message in the chatbuf on switch-in
    (with-telega-chatbuf (telega-msg-chat image-msg)
      (telega-chatbuf--history-state-set :goto-msg image-msg))

    (telega-file--download hr-file 32
      (lambda (tl-file)
        (if (not (telega-file--downloaded-p tl-file))
            ;; Show downloading progress in modeline
            (let ((progress (telega-file--downloading-progress tl-file)))
              (message "Downloading.. %d%%" (* progress 100)))

          ;; TL-FILE Downloaded
          (telega-image-view-file tl-file image-msg)
          (ignore-errors
            (kill-buffer oldbuffer)))))))

(defun telega-image-next (&optional backward)
  "Show next image in chat."
  (interactive "P")
  (unless telega-image--message
    (user-error "No telega message associated with the image"))

  (if-let ((next-image-msg (telega-chatbuf--next-msg telega-image--message
                             '(type Photo) backward)))
      (telega-image-view-msg next-image-msg)

    ;; `next-image-msg' is nil (not found)
    ;; Need to fetch older/newer messages from the chat's history
    (message "telega: %s" (telega-i18n "lng_profile_loading"))
    (let ((chat (telega-msg-chat telega-image--message))
          (img-msg telega-image--message)
          (img-buffer (current-buffer)))
      (telega--searchChatMessages chat
          '(:@type "searchMessagesFilterPhoto")
          (plist-get telega-image--message :id)
          (if backward 0 -2)
        :limit 3
        :callback
        (lambda (reply)
          (let ((found-messages (append (plist-get reply :messages) nil)))
            (if (or (telega--tl-error-p reply)
                    (null found-messages)
                    (telega-msg-id= img-msg (car found-messages)))
                (user-error "No %s image in %s"
                            (if backward "previous" "next")
                            (telega-ins--as-string
                             (telega-ins--msg-sender chat
                               :with-avatar-p t
                               :with-username-p t
                               :with-brackets-p t)))
              ;; Found a message
              (message "")
              (with-current-buffer img-buffer
                (telega-image-view-msg (car found-messages)))
              )))
        ))))

(defun telega-image-prev ()
  "Show previous image in chat."
  (interactive)
  (telega-image-next 'previous))

(defun telega-image-quit ()
  "Kill image buffer and its window."
  (interactive)
  (when telega-image--message
    (with-telega-chatbuf (telega-msg-chat telega-image--message)
      (telega-chatbuf--history-state-delete :goto-msg)))
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
;;
;; While editing file press
;; {{{where-is(telega-edit-file-goto-message,telega-edit-file-mode-map)}}}
;; to go back to the file's message.
;;
;; To switch between files opened from =telega= use
;; {{{where-is(telega-edit-file-switch-buffer,telega-prefix-map)}}}
;; binding from the [[#telega-prefix-map][Telega prefix map]].
;;
;; To distinguish files opened from =telega= with ordinary files
;; suffix is added to the buffer name.  You can modify this suffix
;; using user option:
;;
;; - {{{user-option(telega-edit-file-buffer-name-function, 2)}}}
(declare-function telega-chatbuf--gen-input-file "telega-chat" (filename &optional file-type preview-p upload-callback))

(defcustom telega-edit-file-buffer-name-function 'telega-edit-file-buffer-name
  "Function to return buffer name when `telega-edit-file-mode' is enabled.
Function is called without arguments and should return a buffer name string.
Inside a function you can use `telega-edit-file-message<f>' to
get message associated with the file."
  :package-version '(telega . "0.7.59")
  :type 'function
  :group 'telega-modes)

(defvar telega-edit-file-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap save-buffer] 'telega-edit-file-save-buffer)
    (define-key map (kbd "M-g t") 'telega-edit-file-goto-message)

    (define-key map [menu-bar goto-message]
      '("Goto Telegram Message" . telega-edit-file-goto-message))
    (define-key map [menu-bar turn-off]
      '("Turn off minor mode" . telega-edit-file-mode))
    map))

(defun telega-edit-file-message ()
  "Return message for the currently edited file with `telega-edit-file-mode'."
  (cl-assert telega-edit-file-mode)
  telega--help-win-param)

(defun telega-edit-file-buffer-name ()
  "Return buffer name for a file edited with `telega-edit-file-mode'."
  (concat (buffer-name) (telega-symbol 'mode)
          (telega-chatbuf--name
           (telega-msg-chat (telega-edit-file-message)))))

(defvar telega-edit-file-mode-lighter
  (concat " " (telega-symbol 'mode) "Edit")
  "Lighter for the `telega-edit-file-mode'.")

;;;###autoload
(define-minor-mode telega-edit-file-mode
  "Minor mode to edit files from Telegram messages.
Can be enabled only for content from editable messages."
  :lighter telega-edit-file-mode-lighter
  :map 'telega-edit-file-mode-map

  (if telega-edit-file-mode
      (let ((msg telega--help-win-param))
        (if (not (plist-get msg :can_be_edited))
            (progn
              ;; No message or message can't be edited
              (telega-edit-file-mode -1)
              (read-only-mode 1)
              (message (concat "telega: File opened in read-only-mode "
                               "since message is read only")))

          ;; Apply buffer name modification to distinguish files
          ;; opened from telega with ordinary files.  Apply
          ;; `telega-edit-file-buffer-name-function' only once, by
          ;; marking buffer name with special text property
          (when telega-edit-file-buffer-name-function
            (let ((old-bufname (buffer-name))
                  (new-bufname (funcall telega-edit-file-buffer-name-function)))
              (unless (get-text-property 0 :telega-buffer-name old-bufname)
                (rename-buffer
                 (propertize new-bufname :telega-buffer-name t)))))
          (setq mode-line-buffer-identification
                (list (propertize
                       "%b"
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
         (message "Uploaded %s (%s%s)" (telega--tl-get ufile :local :path)
                  (file-size-human-readable (telega-file--size ufile))
                  (if (< (telega-file--size ufile) 1024)
                      " bytes"
                    "")))

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

  ;; NOTE: sometimes buffer might be marked as modified, but its
  ;; content is unchanged, causing Emacs to query user that file is
  ;; has changed since saved.  We workaround this by checking hash of
  ;; buffer and file contents.
  (when (and (buffer-modified-p)
             (file-exists-p buffer-file-name)
             (equal (buffer-hash)
                    (let ((cbuf-filename buffer-file-name))
                      (with-temp-buffer
                        (insert-file-contents cbuf-filename)
                        (buffer-hash)))))
    (set-buffer-modified-p nil))
  (let ((save-silently t))
    (save-buffer))

  (when-let ((msg telega--help-win-param))
    (unless (plist-get msg :can_be_edited)
      (user-error "Telega: message can't be edited"))

    ;; NOTE: `editMessageMedia' always replaces caption, so retain
    ;; existing caption in the call to `editMessageMedia'
    (telega--editMessageMedia
     msg
     (list :@type "inputMessageDocument"
           :caption (when-let ((cap (telega--tl-get msg :content :caption)))
                      (telega-fmt-text-desurrogate (copy-sequence cap)))
           :document (let ((telega-chat-upload-attaches-ahead t))
                       (telega-chatbuf--gen-input-file
                        (buffer-file-name) 'Document nil
                        #'telega-edit-file--upload-callback))))))

(defun telega-edit-file-switch-buffer (buffer)
  "Interactively switch to BUFFER having `telega-edit-file-mode'."
  (interactive
   (let ((edit-file-buffers (internal-complete-buffer
                             ""
                             (lambda (name-and-buf)
                               (buffer-local-value 'telega-edit-file-mode
                                                   (cdr name-and-buf)))
                             t)))
     (unless edit-file-buffers
       (user-error "No files opened from telega"))
     (list (funcall telega-completing-read-function
                    "Telega Edit File: " edit-file-buffers
                    nil t nil 'buffer-name-history))))
  (switch-to-buffer buffer))


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

(defvar telega-highlight-text-mode-lighter
  (concat " " (telega-symbol 'mode) "Highlight")
  "Lighter for the `telega-highlight-text-mode'.")

(define-minor-mode telega-highlight-text-mode
  "Minor mode to highlight text."
  :lighter telega-highlight-text-mode-lighter
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
  '((82439953   :source opencollective :since_date 1609459200)
    (781215372  :source opencollective :since_date 1609459200)
    (275409096  :source opencollective :since_date 1610236800)
    (356787489  :source opencollective :since_date 1611532800)
    (1648334150 :source opencollective :since_date 1611792000)
    (205887307  :source opencollective :since_date 1612224000)
    (110622853  :source opencollective :since_date 1612310400)
    (388827905  :source private        :since_date 1614804711)
    (835801     :source opencollective :since_date 1614897092)
    (1358845605 :source opencollective :since_date 1617444333)
    (59196540   :source opencollective :since_date 1624869937)
    (676179719  :source opencollective :since_date 1634203432)
    (52573016   :source opencollective :since_date 1643882221)
    (5974516348 :source private        :since_date 1695584567)
    (86646581   :source private        :since_date 1700213789)
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
                 (let* ((svg-w (telega-svg-width svg))
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
      (concat "⸨" (substring (telega-msg-sender-title sender) 0 1) "⸩")
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


;;; ellit-org: minor-modes
;; ** telega-my-location-mode
;;
;; ~telega-my-location~ is used by =telega= to calculate distance to
;; me for location messages.  Also, ~telega-my-location~ is used to
;; search chats nearby me.  So, having it set to correct value is
;; essential.  There is
;; [[#telega-live-locationel--manage-live-location-in-telega-using-geoel][contrib/telega-live-location.el]]
;; which uses =geo.el= to actualize ~telega-my-location~, however it
;; is not always possible to use it.
;;
;; When ~telega-my-location-mode~ is enabled, your
;; ~telega-my-location~ gets automatic update when you send location
;; message into "Saved Messages" using mobile Telegram client.
;;
;; Enable with ~(telega-my-location-mode 1)~ or at =telega= load time:
;; #+begin_src emacs-lisp
;; (add-hook 'telega-load-hook 'telega-my-location-mode)
;; #+end_src
(defun telega-my-location--on-new-message (msg)
  "Set `telega-my-location' if MSG is a location sent to \"Saved Messages\"."
  (when (eq telega--me-id (plist-get msg :chat_id))
    (let ((content (plist-get msg :content)))
      (when (equal (plist-get content :@type)
                   "messageLocation")
        (setq telega-my-location (plist-get content :location))
        (message "telega: telega-my-location → %s"
                 (telega-ins--as-string
                  (telega-ins--location telega-my-location)))

        ;; Update all my live location messages
        (telega--getActiveLiveLocationMessages
         (lambda (messages)
           (dolist (msg messages)
             (telega--editMessageLiveLocation msg telega-my-location))))

        ;; Also update all active live locations in the rootbuf
        (telega-active-locations--check)
        ))))

(define-minor-mode telega-my-location-mode
  "Global mode to set `telega-my-location' using \"Saved Messages\".
When this mode is enabled, you can set `telega-my-location' by sending
your actual location to \"Saved Messages\" using mobile Telegram client."
  :init-value nil :global t :group 'telega-modes
  (if telega-my-location-mode
      (add-hook 'telega-chat-pre-message-hook
                'telega-my-location--on-new-message)
    (remove-hook 'telega-chat-pre-message-hook
                 'telega-my-location--on-new-message)))


;;; ellit-org: minor-modes
;; ** telega-active-locations-mode
;;
;; Minor mode to display currently active live locations in the root
;; buffer.
;;
;; ~telega-active-locations-mode~ is enabled by default.
;;
;; - {{{user-option(telega-active-locations-show-avatars, 2)}}}
;; - {{{user-option(telega-active-locations-show-titles, 2)}}}
;;

;; TODO: Add support to display chat's active locations in the chatbuf
;; footer

(declare-function telega-root-aux-append "telega-root" (inserter))
(declare-function telega-root-aux-remove "telega-root" (inserter))
(declare-function telega-root-aux-redisplay "telega-root" (&optional inserter))

(defvar telega-active-location--messages nil
  "List of recently active live location messages.")

(defcustom telega-active-locations-show-avatars telega-root-show-avatars
  "*Non-nil to show user avatars in chat buffer."
  :type 'boolean
  :group 'telega-chat)

(defcustom telega-active-locations-show-titles (not telega-active-locations-show-avatars)
  "Non-nil to show sender/chat titles along the side with avatars."
  :type 'boolean
  :group 'telega-chat)

(define-minor-mode telega-active-locations-mode
  "Global mode to display currently active live locations in the root buffer."
  :init-value nil :global t :group 'telega-modes
  (if telega-active-locations-mode
      (progn
        (setq telega-active-location--messages nil)

        (advice-add 'telega--on-updateMessageContent
                    :after #'telega-active-locations--msg-updated)
        (advice-add 'telega--on-updateMessageEdited
                    :after #'telega-active-locations--msg-edited)
        (advice-add 'telega--on-updateDeleteMessages
                    :after #'telega-active-locations--msg-deleted)
        (add-hook 'telega-chat-post-message-hook
                  #'telega-active-locations--msg-new)
        (add-hook 'telega-chatbuf-pre-msg-insert-hook
                  #'telega-active-locations--msg-new)
        (add-hook 'telega-chats-fetched-hook
                  #'telega-active-locations--fetch)
        (add-hook 'telega-connection-state-hook
                  #'telega-active-locations--check)

        (telega-root-aux-append #'telega-ins--active-locations)
        (when (telega-server-live-p)
          (telega-active-locations--fetch))
        )

    (telega-root-aux-remove #'telega-ins--active-locations)
    (remove-hook 'telega-connection-state-hook
                 #'telega-active-locations--check)
    (remove-hook 'telega-chat-post-message-hook
                 #'telega-active-locations--msg-new)
    (remove-hook 'telega-chatbuf-pre-msg-insert-hook
                 #'telega-active-locations--msg-new)
    (remove-hook 'telega-chats-fetched-hook
                 #'telega-active-locations--fetch)
    (advice-remove 'telega--on-updateMessageContent
                   #'telega-active-locations--msg-updated)
    (advice-remove 'telega--on-updateMessageEdited
                   #'telega-active-locations--msg-edited)
    (advice-remove 'telega--on-updateDeleteMessages
                   #'telega-active-locations--msg-deleted    )
    ))

(defun telega-active-location--msg-find (msg-id chat-id)
  "Find active location by MSG-ID and CHAT-ID."
  (cl-find-if (lambda (msg)
                (and (eq msg-id (plist-get msg :id))
                     (eq chat-id (plist-get msg :chat_id))))
              telega-active-location--messages))

(defun telega-active-locations--msg-new (new-msg)
  "Check new message NEW-MSG is a live location message."
  (when (telega-msg-match-p new-msg '(type Location))
    (telega-active-locations--check (list new-msg))))

(defun telega-active-locations--msg-updated (event)
  "Check active live location message has been updated.
EVENT must be \"updateMessageContent\"."
  (when-let ((loc-msg (telega-active-location--msg-find
                       (plist-get event :message_id)
                       (plist-get event :chat_id))))
    (plist-put loc-msg :content (plist-get event :new_content))
    (telega-active-locations--check (list loc-msg))))

(defun telega-active-locations--msg-edited (event)
  "Check active live location message has been updated.
EVENT must be \"updateMessageEdited\"."
  (when-let ((loc-msg (telega-active-location--msg-find
                       (plist-get event :message_id)
                       (plist-get event :chat_id))))
    (plist-put loc-msg :edit_date (plist-get event :edit_date))
    (telega-active-locations--check (list loc-msg))))

(defun telega-active-locations--msg-deleted (event)
  "Check active live location message has been deleted.
EVENT must be \"updateDeleteMessages\"."
  (when-let* ((permanent-p (plist-get event :is_permanent))
              (chat-id (plist-get event :chat_id))
              (has-live-loc-in-chat-p
               (cl-find chat-id telega-active-location--messages
                        :key (telega--tl-prop :chat_id))))
    (seq-doseq (msg-id (plist-get event :message_ids))
      (when-let* ((msg (telega-active-location--msg-find msg-id chat-id))
                  (content (plist-get msg :content)))
        ;; NOTE: Mark MSG as non live, so
        ;; `telega-active-locations--check' will remove it
        (plist-put content :live_period 0)
        (plist-put content :expires_in 0)
        (telega-active-locations--check (list msg))))))

(defun telega-ins--active-location-msg (msg)
  "Inserter for active location message MSG in root aux."
  (let* ((user (telega-msg-sender msg))
         (chat (telega-msg-chat msg))
         (brackets (telega-msg-sender-brackets chat)))
    (telega-ins (car brackets))
    (when telega-active-locations-show-avatars
      (telega-ins--image
       (telega-msg-sender-avatar-image-one-line user)))
    (when telega-active-locations-show-titles
      (telega-ins--msg-sender user))
    (when (or (telega-me-p user)
              (not (telega-chat-private-p chat)))
      (telega-ins (telega-symbol 'right-arrow))
      (when telega-active-locations-show-avatars
        (telega-ins--image
         (telega-msg-sender-avatar-image-one-line chat)))
      (when telega-active-locations-show-titles
        (telega-ins--msg-sender chat
          :with-username-p t
          :with-brackets-p t)))
    (telega-ins--with-face 'telega-shadow
      (telega-ins " Live"))
    (cl-destructuring-bind (live-for updated-ago)
        (telega-msg-location-live-for msg)
      (telega-ins-fmt " for %s"
        (telega-duration-human-readable live-for 1 'long))
      (telega-ins-fmt " (%s ago)"
        (telega-duration-human-readable updated-ago 1 'long)))

    (when (and (not (telega-me-p user)) telega-my-location)
      (telega-ins " " (telega-symbol 'distance))
      (telega-ins
       (telega-distance-human-readable
        (telega-location-distance
         (telega--tl-get msg :content :location)
         telega-my-location))))
    (telega-ins (cadr brackets))))

(defun telega-ins--active-locations ()
  "Inserter for currently active live locations."
  ;; Reset active locations on telega restarts
  (unless (telega-server-live-p)
    (setq telega-active-location--messages nil))

  (when telega-active-location--messages
    (telega-ins (telega-symbol 'location) "Locations: ")

    (dolist (loc-msg telega-active-location--messages)
      (telega-ins "\n")
      (telega-ins "    ")
      (telega-ins--with-face (when (telega-me-p (telega-msg-sender loc-msg))
                               'bold)
      (telega-button--insert 'telega-msg loc-msg
        :inserter #'telega-ins--active-location-msg
        :action #'telega-msg-goto-highlight))
      (when (telega-me-p (telega-msg-sender loc-msg))
        (telega-ins " ")
        (telega-ins--button (telega-i18n "telega_stop_live_location" :count 1)
          'action (lambda (_button)
                    (telega--editMessageLiveLocation loc-msg nil)))))
    t))

(defun telega-active-locations--check (&optional messages)
  "Check messages being new or updated live location MESSAGES.
If MESSAGES is ommited, then check/update currently active location
messages."
  (let (live-locs-updated-p)
    (dolist (loc-msg (or messages
                         (copy-sequence telega-active-location--messages)))
      (cl-assert (telega-msg-match-p loc-msg '(type Location)))
      (let* ((loc-live-for (telega-msg-location-live-for loc-msg))
             (still-live-p (and
                            ;; NOTE: for outgoing messages examine
                            ;; only successfully sent messages,
                            ;; because after message is successfully
                            ;; sent it can change its ID
                            (not (plist-get loc-msg :sending_state))
                            loc-live-for (> (car loc-live-for) 0)))
             (active-loc (telega-active-location--msg-find
                          (plist-get loc-msg :id)
                          (plist-get loc-msg :chat_id))))
        (if still-live-p
            ;; Ensure loc-msg is in the list.  Update existing loc
            ;; message inplace without changing order in the
            ;; `telega-active-location--messages' list
            (if active-loc
                (setcdr active-loc (cdr loc-msg))
              (setq telega-active-location--messages
                    (cons loc-msg telega-active-location--messages)))

          (setq telega-active-location--messages
                (delq active-loc telega-active-location--messages)))

        (setq live-locs-updated-p
              (or live-locs-updated-p still-live-p active-loc))))

    (when live-locs-updated-p
      (telega-root-aux-redisplay #'telega-ins--active-locations))))

(defun telega-active-locations--fetch ()
  "Fetch my live location messages."
  (telega--getActiveLiveLocationMessages #'telega-active-locations--check)

  ;; For all chats having chatbuffer, search for recent live location
  ;; messages
  (dolist (chat (mapcar #'car telega--chat-buffers-alist))
    (telega--searchChatRecentLocationMessages chat
      #'telega-active-locations--check)))


;;; ellit-org: minor-modes
;; ** telega-active-video-chats-mode
;;
;; Minor mode to display currently active video chats in the root
;; buffer.  Also display number of active video chats in the modeline
;; if [[#telega-mode-line-mode][telega-mode-line-mode]] is enabled.
;;
;; ~telega-active-video-chats-mode~ is enabled by default.
;;
;; Customizable options:
;; - {{{user-option(telega-active-video-chats-temex, 2)}}}

(defcustom telega-active-video-chats-temex
  '(and is-known (has-video-chat non-empty))
  "Chat Temex to match chat with active video chat."
  :type 'telega-chat-temex
  :group 'telega-modes)

(defcustom telega-active-video-chats-mode-line-format
  '(:eval (telega-mode-line-active-video-chats))
  "Modeline format for active video chats for `telega-mode-line-mode'.
Set to nil to disable active video chats in the modeline."
  :type 'sexp
  :group 'telega-modes)

(defvar telega-active-video-chats--chats nil
  "List of chats where video chat is active.")

(defun telega-mode-line-active-video-chats ()
  "Format number of active video chats for the modeline."
  (when telega-active-video-chats--chats
    (concat " " telega-symbol-video-chat-active
            (propertize (int-to-string (length telega-active-video-chats--chats))
                        'face 'bold))))

(defun telega-ins--active-video-chat (chat)
  (let* ((video-chat (plist-get chat :video_chat))
         (has-participants-p (plist-get video-chat :has_participants))
         (group-call (telega-group-call-get
                         (plist-get video-chat :group_call_id))))
    (cond ((and has-participants-p
                group-call
                (not (zerop (plist-get group-call :participant_count))))
           (telega-ins (telega-symbol 'video-chat-active))
           (telega-ins (telega-tl-str group-call :title))
           (telega-ins " ")
           (seq-doseq (recent-speaker (plist-get group-call :recent_speakers))
             (telega-ins--image
              (telega-group-call--participant-image recent-speaker)))
           (telega-ins-fmt "/%d" (plist-get group-call :participant_count)))
          ((null has-participants-p)
           (telega-ins (telega-symbol 'video-chat-passive))))
    (telega-ins " ")
    (telega-ins (telega-symbol 'right-arrow))
    (telega-ins " ")
    (telega-ins--msg-sender chat
      :with-avatar-p t
      :with-username-p t
      :with-brackets-p t)
    ;; Button to hide this video chat
    (telega-ins " ")
    (telega-ins--button (propertize "✕" 'face 'bold)
      'action (lambda (_button)
                (setq telega-active-video-chats--chats
                      (delq chat telega-active-video-chats--chats))
                (telega-mode-line-update)
                (telega-root-aux-redisplay #'telega-ins--active-video-chats)))
    t))

(defun telega-ins--active-video-chats ()
  "Inserter for active video chats."
  (unless (telega-server-live-p)
    (setq telega-active-video-chats--chats nil))

  (when telega-active-video-chats--chats
    (telega-ins (telega-i18n "lng_call_box_groupcalls_subtitle") ": ")
    (seq-doseq (chat telega-active-video-chats--chats)
      (telega-ins "\n")
      (telega-ins "    ")
      (telega-ins--active-video-chat chat))
    t))

(defun telega-active-video-chats--on-update-chat (event)
  "Video chat info has been updated for some chat."
  (let ((chat (telega-chat-get (plist-get event :chat_id)))
        (need-redisplay-p nil))
    (if (telega-chat-match-p chat telega-active-video-chats-temex)
        (progn
          (cl-pushnew chat telega-active-video-chats--chats)
          (setq need-redisplay-p t))

      (setq need-redisplay-p (memq chat telega-active-video-chats--chats))
      (setq telega-active-video-chats--chats
            (delq chat telega-active-video-chats--chats)))

    (when need-redisplay-p
      (telega-mode-line-update)
      (telega-root-aux-redisplay #'telega-ins--active-video-chats))))

(defun telega-active-video-chats--on-update-call (_event)
  "Group call of an active video chat has been updated."
  (telega-root-aux-redisplay #'telega-ins--active-video-chats)
  )

(define-minor-mode telega-active-video-chats-mode
  "Global mode to display active video chats in the root buffer."
  :init-value nil :global t :group 'telega-modes
  (if telega-active-video-chats-mode
      (progn
        (setq telega-active-video-chats--chats
              (telega-filter-chats telega--ordered-chats
                telega-active-video-chats-temex))

        (unless (memq telega-active-video-chats-mode-line-format
                      telega-mode-line-string-format)
          (setq telega-mode-line-string-format
                (append telega-mode-line-string-format
                        (list telega-active-video-chats-mode-line-format)))
          (telega-mode-line-update))

        (advice-add 'telega--on-updateChatVideoChat
                    :after #'telega-active-video-chats--on-update-chat)
        (advice-add 'telega--on-updateGroupCall
                    :after #'telega-active-video-chats--on-update-call)
        (telega-root-aux-append #'telega-ins--active-video-chats))

    ;; Disabled
    (setq telega-mode-line-string-format
          (delq telega-active-video-chats-mode-line-format
                telega-mode-line-string-format))
    (telega-mode-line-update)

    (telega-root-aux-remove #'telega-ins--active-video-chats)
    (advice-remove 'telega--on-updateGroupCall
                   #'telega-active-video-chats--on-update-call)
    (advice-remove 'telega--on-updateChatVideoChat
                   #'telega-active-video-chats--on-update-chat)
    ))


;;; ellit-org: minor-modes
;; ** telega-recognize-voice-message-mode
;;
;; Minor mode to automatically recognize speech in a voice/video
;; messages.  Available only for the Telegram Premium users.
;;
;; Enable with ~(telega-recognize-voice-message-mode 1)~ or at
;; =telega= load time:
;; #+begin_src emacs-lisp
;; (add-hook 'telega-load-hook 'telega-recognize-voice-message-mode)
;; #+end_src
;;
;; Customizable options:
;; - {{{user-option(telega-recognize-voice-message-temex, 2)}}}

(defcustom telega-recognize-voice-message-temex nil
  "Message Temex for messages to automatically recognize speech.
Speech recognition is only applied to voice messages matching this
Message Temex.  Applied only if non-nil.
For example, to recognize speech in a voice messages only in private
chats, use `(chat (type private))' Message Temex."
  :type 'telega-msg-temex
  :group 'telega-modes)

(defun telega-recognize-voice--on-msg-hover-in (msg)
  "Automatically recognize text for the hovered in voice message MSG."
  (when (and (telega-msg-match-p msg '(type VoiceNote VideoNote))
             (not (telega--tl-get msg :content :voice_note
                                  :speech_recognition_result))
             (or (not telega-recognize-voice-message-temex)
                 (telega-msg-match-p msg telega-recognize-voice-message-temex))
             (telega-user-match-p (telega-user-me) 'is-premium))
    (telega--recognizeSpeech msg)))

(defun telega-recognize-voice--on-msg-new (msg)
  "Automatically recognize text for new voice message MSG.
Recognize only if message is observable."
  (when (telega-msg-observable-p msg)
    (telega-recognize-voice--on-msg-hover-in msg)))

;;;###autoload
(define-minor-mode telega-recognize-voice-message-mode
  "Global mode to automatically recognize speech in the voice messages."
  :init-value nil :global t :group 'telega-modes
  (if telega-recognize-voice-message-mode
      (progn
        (add-hook 'telega-msg-hover-in-hook
                  #'telega-recognize-voice--on-msg-hover-in)
        (add-hook 'telega-chat-post-message-hook
                  #'telega-recognize-voice--on-msg-new))

    (remove-hook 'telega-chat-post-message-hook
                 #'telega-recognize-voice--on-msg-new)
    (remove-hook 'telega-msg-hover-in-hook
                 #'telega-recognize-voice--on-msg-hover-in)))


;;; ellit-org: minor-modes
;; ** telega-auto-translate-mode
;;
;; Minor chatbuf mode to automatically translate messages in the chat
;; to your native language.  Also translates chatbuf input from your
;; native language to the chat's language.
;;
;; Customizable options:
;; - {{{user-option(telega-auto-translate-probe-language-codes, 2)}}}
;; - {{{user-option(telega-translate-to-language-by-default, 2)}}}
(defcustom telega-auto-translate-probe-language-codes nil
  "List of language codes to probe.
Chat description is used to probe chat's language."
  :type '(repeat (string :tag "Language Code"))
  :group 'telega-modes)

(defun telega-auto-translate--chatbuf-translate-visible-messages ()
  "Translate all visible messages in the chatbuf."
  (cl-assert telega-translate-to-language-by-default)
  (when-let ((window (get-buffer-window)))
    (let* ((curr-msg (telega-msg-at (window-start window)))
           (end-msg (telega-msg-at (window-end window)))
           messages)
      (while (not (eq curr-msg end-msg))
        (setq messages (cons curr-msg messages))
        (setq curr-msg (telega-chatbuf--next-msg curr-msg
                         '(not (call telega-msg-internal-p)))))

      ;; Translate 5 last messages
      (dolist (msg (seq-take messages 5))
        (telega-auto-translate--msg msg))
      )))

(defun telega-auto-translate--chatbuf-detect-language (language-codes)
  "In the chatbuf detect and set its language.
Chat description is used to detect the language."
  (if-let* ((chat telega-chatbuf--chat)
            (lang-code (car language-codes))
            (chat-info (telega-chat--info telega-chatbuf--chat 'offline))
            (full-info (telega--full-info chat-info 'offline))
            (desc (or (telega-tl-str full-info :bio)
                      (telega-tl-str full-info :description))))
    (telega--translateText desc lang-code
      :callback (lambda (reply)
                  (let ((translated-text (telega-tl-str reply :text)))
                    (with-telega-chatbuf chat
                      (if (equal translated-text desc)
                          (progn
                            ;; NOTE: if text is the same, it means we
                            ;; detected language
                            (setq telega-chatbuf-language-code lang-code)
                            (telega-chatbuf--prompt-update)
                            (telega-auto-translate--chatbuf-translate-visible-messages))
                        ;; Otherwise continue detecing language
                        (telega-auto-translate--chatbuf-detect-language
                         (cdr language-codes)))))))

    ;; Set chat language by hand
    (setq telega-chatbuf-language-code
          (telega-completing-read-language-code
           "Translate chat messages from: "))
    (telega-chatbuf--prompt-update)
    (telega-auto-translate--chatbuf-translate-visible-messages)
    ))

(defvar telega-auto-translate--pending-translations nil
  "List of pending requests for translation.")
(make-variable-buffer-local 'telega-auto-translate--pending-translations)

(defun telega-auto-translate--chatbuf-del-pending (extra &optional cancel-p)
  (setq telega-auto-translate--pending-translations
        (delq extra telega-auto-translate--pending-translations))
  (when cancel-p
    (telega-server--callback-put extra 'ignore))
  )

(defun telega-auto-translate--chatbuf-ins-translation (text)
  "Generate translation callback."
  (telega--translateText text telega-chatbuf-language-code
    :callback
    (telega--gen-ins-continuation-callback
        (propertize "Translating..." 'face 'telega-shadow 'cursor-intangible t)
      (lambda (reply)
        (telega-auto-translate--chatbuf-del-pending (plist-get reply :@extra))
        (telega-ins--with-props '(telega-translated t)
          (telega-ins (if (telega--tl-error-p reply)
                          text
                        (telega-tl-str reply :text))))))))

(defun telega-auto-translate--chatbuf-input-translate ()
  "Translate and replace chatbuf's input.
Return list of pending translations.
Or nil if translation is not needed."
  (when (and telega-auto-translate-mode
             telega-chatbuf-language-code)
    (let ((attaches (telega--split-by-text-prop
                     (telega-chatbuf-input-string) 'telega-attach)))
      (telega-chatbuf--input-delete)
      (dolist (text attaches)
        (if (or (get-text-property 0 'telega-attach text)
                (get-text-property 0 'telega-translated text))
            (insert text)

          ;; Plain text, that needs to be translated
          (setq telega-auto-translate--pending-translations
                (cons
                 (telega-auto-translate--chatbuf-ins-translation text)
                 telega-auto-translate--pending-translations))))
      telega-auto-translate--pending-translations)))

(defun telega-auto-translate--chatbuf-input-send (origfun &rest args)
  "Send chatbuf input translating text."
  (when telega-auto-translate--pending-translations
    (user-error "telega: still translating.. wait or cancel with `M-x telega-auto-translate-input-translate-cancel RET'"))
  (unless (and telega-auto-translate-mode
               (telega-auto-translate--chatbuf-input-translate))
    (apply origfun args))
  )

(defun telega-auto-translate-input-translate-cancel ()
  "Cancel translating chatbuf's input."
  (interactive)
  (let ((pending-extras telega-auto-translate--pending-translations))
    (seq-doseq (extra pending-extras)
      (telega-auto-translate--chatbuf-del-pending extra 'cancel))
    (message "telega: canceled %d translations" (length pending-extras))))

(defun telega-auto-translate--msg (msg)
  (telega-msg-translate msg telega-translate-to-language-by-default 'quiet))

(defun telega-auto-translate--on-msg-insert (msg)
  "Translate observable message MSG after it has been inserted into chatbuf."
  (when (telega-msg-observable-p msg)
    (telega-auto-translate--msg msg)))

(defun telega-auto-translate--on-msg-update (msg)
  "Message's content has been updated, rerun translation."
  (plist-put msg :telega-translated nil)
  (telega-auto-translate--on-msg-insert msg))

(defun telega-auto-translate--chatbuf-prompt-translation ()
  "Addon to chatbuf prompt in case `telega-auto-translate-mode' is enabled."
  (when (and telega-auto-translate-mode
             telega-chatbuf-language-code
             telega-translate-to-language-by-default
             (not (equal telega-chatbuf-language-code
                         telega-translate-to-language-by-default)))
    (telega-ins--as-string
     (telega-ins--with-face 'telega-shadow
       (telega-ins "["
                   telega-translate-to-language-by-default
                   (telega-symbol 'right-arrow)
                   telega-chatbuf-language-code
                   "]")))))

(defvar telega-auto-translate-mode-lighter
  (concat " " (telega-symbol 'mode) "Translate")
  "Lighter for the `telega-auto-translate-mode'.")

(define-minor-mode telega-auto-translate-mode
  "Minor mode to automatically translate chat messages."
  :lighter telega-auto-translate-mode-lighter
  (if telega-auto-translate-mode
      (progn
        (unless telega-translate-to-language-by-default
          (setq telega-translate-to-language-by-default
                (telega-completing-read-language-code
                 "Translate chat messages/input to: "))
          (telega-help-message 'translate-to-language
              "Consider customizing `telega-translate-to-language-by-default'"))

        (advice-add 'telega-chatbuf-input-send
                    :around 'telega-auto-translate--chatbuf-input-send)
        (add-hook 'telega-msg-hover-in-hook
                  #'telega-auto-translate--msg nil 'local)
        (add-hook 'telega-chatbuf-post-msg-insert-hook
                  #'telega-auto-translate--on-msg-insert nil 'local)
        (add-hook 'telega-chatbuf-pre-msg-update-hook
                  #'telega-auto-translate--on-msg-update nil 'local)

        ;; Try to detect chat's language code
        (if (and (not telega-chatbuf-language-code)
                 (telega-chatbuf-match-p 'can-send-or-post))
            (telega-auto-translate--chatbuf-detect-language
             telega-auto-translate-probe-language-codes)
          (telega-chatbuf--prompt-update)
          (telega-auto-translate--chatbuf-translate-visible-messages))
        )

    (remove-hook 'telega-chatbuf-pre-msg-update-hook
                 #'telega-auto-translate--on-msg-update 'local)
    (remove-hook 'telega-chatbuf-post-msg-insert-hook
                 #'telega-auto-translate--on-msg-insert 'local)
    (remove-hook 'telega-msg-hover-in-hook
                 #'telega-auto-translate--msg 'local)
    (advice-remove 'telega-chatbuf-input-send
                   'telega-auto-translate--chatbuf-input-send)
    (telega-chatbuf--prompt-update)
    ))


;;; ellit-org: minor-modes
;;
;; ** telega-auto-download-mode (WIP)
;;
;; Global minor mode to automatically download media files.
;; Auto-download settings depends on network type currently used by
;; =telega=.
;;
;; You can modify auto-download settings by changing network type with
;; {{{kbd(M-x telega-set-network-type RET)}}} command.
;;
;; Enable auto-download with ~(telega-auto-download-mode 1)~ or at
;; =telega= load time:
;; #+begin_src emacs-lisp
;; (add-hook 'telega-load-hook 'telega-auto-download-mode)
;; #+end_src
;;
;; Customizable options:
;; - {{{user-option(telega-auto-download-settings-alist, 2)}}}
(defconst telega-auto-download--disabled-preset
  '(:@type "autoDownloadSettings" :is_auto_download_enabled :false)
  "Auto download settings for the `:disabled' preset.")

(defvar telega-auto-download--tdlib-presets nil
  "Auto-download presets fetched with `getAutoDownloadSettingsPresets'.")

(defvar telega-auto-download-settings nil
  "Current auto download settings.
TDLib's autoDownloadSettings structure.")

(defcustom telega-auto-download-settings-alist
  '((roaming-mobile . :disabled)
    (mobile . :low)
    (wi-fi . :medium)
    (other . :high))
  "Alist with auto-download settings for different network types.
car of the item is the network type symbol from
`telega-tdlib-network-type-alist', cdr is auto-download setting.
Auto-download setting could be one of `:disabled', `:low', `:medium'
or `:high' keyword denoting auto download preset or a plist with
TDLib's autoDownloadSettings structure."
  :type 'alist
  :group 'telega-modes)

(defun telega-auto-download--apply-settings (settings &optional tl-network-type)
  "Apply auto-downloading SETTINGS for the TL-NETWORK-TYPE."
  (let ((tl-settings
         (cond ((memq settings '(:low :medium :high))
                (plist-get telega-auto-download--tdlib-presets settings))
               ((memq settings '(:disabled nil))
                telega-auto-download--disabled-preset)
               ((listp settings)
                settings))))
    (if (not tl-settings)
        (warn "telega: unknown auto download settings %S for %S network type"
              settings tl-network-type)

      (telega--setAutoDownloadSettings tl-settings tl-network-type))))

(defun telega-auto-download--start ()
  "Start auto downloading mode."
  (if telega-auto-download--tdlib-presets
      (when telega-auto-download-mode
        (dolist (auto-download-settings telega-auto-download-settings-alist)
          (telega-auto-download--apply-settings
           (cdr auto-download-settings)
           (alist-get (car auto-download-settings)
                      telega-tdlib-network-type-alist))))

    (telega--getAutoDownloadSettingsPresets
     (lambda (reply)
       (setq telega-auto-download--tdlib-presets reply)
       (telega-auto-download--start)))))

;;;###autoload
(define-minor-mode telega-auto-download-mode
  "Global mode to automatically download media files."
  :init-value nil :global t :group 'telega-modes
  (if telega-auto-download-mode
      (progn
        (add-hook 'telega-ready-hook
                  #'telega-auto-download--start)
        (when (telega-server-live-p)
          (telega-auto-download--start))
        )

    (when (telega-server-live-p)
      (telega-auto-download--apply-settings
       telega-auto-download--disabled-preset))
    (remove-hook 'telega-ready-hook
                 #'telega-auto-download--start)
    ))


;;; ellit-org: minor-modes
;; ** TODO telega-play-media-sequence-mode
;;
;; Play media content in subsequent manner.
(defconst telega-play-media-sequence--type-definitions
  '((audio (type Audio) (:@type "searchMessagesFilterAudio"))
    (video (type Video) (:@type "searchMessagesFilterVideo"))
    (voice-note (type VoiceNote) (:@type "searchMessagesFilterVoiceNote"))
    (video-note (type VideoNote) (:@type "searchMessagesFilterVideoNote"))
    (voice-and-video-note (type VoiceNote VideoNote)
                          (:@type "searchMessagesFilterVoiceAndVideoNote"))
    (animation (type Animation) (:@type "searchMessagesFilterAnimation")))
  "Definitions for the media sequences.")

(defcustom telega-play-media-sequence-types
  '(audio video voice-and-video-note)
  "List of media sequence types to play sequentially."
  :type '(repeat (choice (const :tag "Audio" audio)
                         (const :tag "Video" video)
                         (const :tag "Voice Note" voice-note)
                         (const :tag "Video Note" video-note)
                         (const :tag "Voice or Video Note" voice-and-video-note)
                         (const :tag "Animation" animation)))
  :group 'telega-modes)

(defcustom telega-play-media-sequence-once-types
  '(voice-and-video-note)
  "List of media sequences for one shot sequential play."
  :type '(repeat (choice (const :tag "Audio" audio)
                         (const :tag "Video" video)
                         (const :tag "Voice Note" voice-note)
                         (const :tag "Video Note" video-note)
                         (const :tag "Voice or Video Note" voice-and-video-note)
                         (const :tag "Animation" animation)))
  :group 'telega-modes)

(defun telega-play-media-sequence-msg-match-p (msg sequence-type)
  "Return non-nil if a message MSG matches SEQUENCE-TYPE."
  (let ((msg-temex (nth 1 (assq sequence-type
                                telega-play-media-sequence--type-definitions))))
    (telega-msg-match-p msg msg-temex)))

(defvar telega-play-media-sequence--once-p nil)
(make-variable-buffer-local 'telega-play-media-sequence--once-p)

(defvar telega-play-media-sequence--current-type nil)
(make-variable-buffer-local 'telega-play-media-sequence--current-type)

(defvar telega-play-media-sequence-mode-lighter
  (concat " " (telega-symbol 'mode) "Media Sequence")
  "Lighter for the `telega-play-media-sequence-mode'.")

(define-minor-mode telega-play-media-sequence-mode
  "Minor mode to play media messages sequentially."
  :lighter telega-play-media-sequence-mode-lighter
  )

(defun telega-play-media-sequence-once (_sequence-type)
  "Play media SEQUENCE-TYPE once."
  (if telega-play-media-sequence-mode
      ;; todo
  ))

;; Advice for
(defun telega-play-media-sequence-mode--player-sentinel (proc)
  (when-let* ((proc-plist (process-plist proc))
              (msg (plist-get proc-plist :message)))
    (with-telega-chatbuf (telega-msg-chat msg)
      (when telega-play-media-sequence-mode
        ;; todo
        ))))

;; Advice for the `telega-msg--
(defun telega-play-media-sequence--player-run (origfun filename &optional msg
                                                       done-callback)
  (let ((_proc (apply origfun filename msg done-callback)))
    ;; TODO
    t
    ))

;; Advice for the `telega-msg-voice-note--ffplay-callback'

;; Advice for the `telega-msg-video-note--ffplay-callback'

(provide 'telega-modes)

;;; telega-modes.el ends here

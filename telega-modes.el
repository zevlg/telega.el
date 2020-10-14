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

(defvar tracking-buffers)
(defvar telega-chatbuf--refresh-point)
(declare-function telega "telega" (arg))
(declare-function telega-chatbuf--goto-msg "telega-chat" (msg-id &optional highlight))

(defgroup telega-modes nil
  "Customization for telega minor modes."
  :prefix "telega-"
  :group 'telega)

;;; ellit-org: minor-modes
;; ** telega-mode-line-mode
;;
;; Global minor mode to display =telega= status in modeline.
;;
;; Enable with ~(telega-mode-line-mode 1)~, or at =telega= load time:
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
  (or telega-mode-line--logo-image-cache
      (setq telega-mode-line--logo-image-cache
            (find-image
             (list (list :type (when (fboundp 'imagemagick-types) 'imagemagick)
                         :file "etc/telega-logo.png"
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


;;; Animation autoplay mode
(defcustom telega-autoplay-messages '(messageAnimation)
  "Message types to automatically play when received."
  :type 'list
  :group 'telega-modes)

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
;; 7. New message has no `messageSendOptions' to avoid squashing
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
                   (eq (telega--tl-type (plist-get last-msg :content))
                       'messageText)
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
          (telega--editMessageText telega-chatbuf--chat last-msg imc)
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
;; To view highres image in chatbuf with ~telega-image-mode~ press
;; {{{kbd(RET)}}} on the message with photo.
(require 'image-mode)

(declare-function telega-chat-title "telega-chat" (chat &optional with-username))
(declare-function telega-chat-brackets "telega-chat" (chat))
(declare-function telega-chatbuf--next-msg "telega-chat" (msg predicate &optional backward))

(defvar telega-image--message nil
  "Message corresponding to image currently viewed.")
(make-variable-buffer-local 'telega-image--message)

(defvar telega-image-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map image-mode-map)
    (define-key map "n" 'telega-image-next)
    (define-key map "p" 'telega-image-prev)
    map))

(define-derived-mode telega-image-mode image-mode nil
  "Major mode to view images from chat buffer."
  (setq mode-name
        (concat "◁Image" (when image-type (format "[%s]" image-type))))
  )

(defun telega-image-view-file (tl-file &optional for-msg)
  "View image in telegram TL-FILE from message FOR-MSG."
  (cl-assert (telega-file--downloaded-p tl-file))
  (find-file-literally (telega--tl-get tl-file :local :path))
  (telega-image-mode)
  (setq telega-image--message for-msg))

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
            (if-let ((chat-win (get-buffer-window (current-buffer))))
                (set-window-point chat-win (point))
              (setq telega-chatbuf--refresh-point (point)))))

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
  "Minor mode to edit files from Telegram messages."
  :lighter " ◁Edit"
  :map 'telega-edit-file-mode-map

  (if telega-edit-file-mode
    (let* ((msg telega--help-win-param)
           (chat (and msg (telega-msg-chat msg))))
      (unless msg
        (telega-edit-file-mode -1)
        (user-error "Telega: No message associated with the file"))
      (unless (plist-get msg :can_be_edited)
        (telega-edit-file-mode -1)
        (user-error "Telega: message can't be edited"))

      (setq mode-line-buffer-identification
            (list (propertized-buffer-identification "%b")
                  (propertize
                   (concat "◁" (telega-chat-title-with-brackets chat))
                   'face 'mode-line-buffer-id
                   'help-echo "mouse-1: To goto telegram message"
                   'mouse-face 'mode-line-highlight
                   'local-map (eval-when-compile
                                (make-mode-line-mouse-map
                                 'mouse-1 #'telega-edit-file-goto-message))))))

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
             (telega-ins-progress-bar progress 1.0 30 ?\+ ?\s)
             (telega-ins-fmt "] %d%%" (round (* progress 100)))))))
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
     (telega-msg-chat msg 'offline) msg
     (list :@type "inputMessageDocument"
           :document (let ((telega-chat-upload-attaches-ahead t))
                       (telega-chatbuf--gen-input-file
                        (buffer-file-name) 'Document nil
                        #'telega-edit-file--upload-callback))))))

(provide 'telega-modes)

;;; telega-modes.el ends here

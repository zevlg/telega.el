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

;; See https://github.com/zevlg/telega.el/blob/master/doc/telega-manual.org#minor-modes

;;; Code:

(require 'telega-customize)
(require 'telega-server)
(require 'telega-filter)

(defvar tracking-buffers)
(declare-function telega "telega" (arg))

(defgroup telega-modes nil
  "Customization for telega minor modes."
  :prefix "telega-"
  :group 'telega)

;; * Minor Modes
;;
;; =telega= ships with various minor modes you might consider to use.

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

;; - User Option: ~telega-mode-line-string-format~
;;
;;   {{{vardoc1(telega-mode-line-string-format)}}}
(defcustom telega-mode-line-string-format
  '("   " (:eval (telega-mode-line-icon))
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
  (force-mode-line-update))

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


;; ** telega-url-shorten-mode
;;
;; Minor mode for chatbuf to show shorter version for some URLs.  For
;; example, with ~telega-url-shorten-mode~ enabled in chatbuf, urls
;; like:
;;
;; #+begin_example
;; https://github.com/zevlg/telega.el/issues/105
;; https://gitlab.com/jessieh/mood-line/issues/6
;; https://www.youtube.com/watch?v=0m2jR6_eMkU
;; https://ru.wikipedia.org/wiki/Душ
;; #+end_example
;;
;; Will look like:
;; [[https://zevlg.github.io/telega/telega-url-shorten.png]]
;;
;; Can be enabled globally in all chats matching
;; ~telega-url-shorten-mode-for~ (see below) chat filter with
;; ~(global-telega-url-shorten-mode 1)~ or by adding:
;;
;; #+begin_src emacs-lisp
;; (add-hook 'telega-load-hook 'global-telega-url-shorten-mode)
;; #+end_src
;;
;; Also consider installing =font-awesome= to display icons for
;; shorten URLs even in tty.

;; Customizable options:
;;
;; - User Option: ~telega-url-shorten-patterns~
;;
;;   {{{vardoc1(telega-url-shorten-patterns)}}}
(defcustom telega-url-shorten-patterns
  (list
   '("https?://github.com/\\(.+\\)/issues/\\([0-9]+\\)" "\\1#\\2"
     :symbol "" :svg-icon "fa-brands/github-octocat.svg" :scale 0.72)
   '("https?://gitlab.com/\\(.+\\)/issues/\\([0-9]+\\)" "\\1#\\2"
     :symbol "" :svg-icon "fa-brands/gitlab-rgb.svg" :scale 0.75)
   '("https?://www.youtube.com/watch.*[?&]v=\\([^&]+\\).+" "YouTube#\\1"
     :symbol "▶" :svg-icon "fa-brands/youtube-rgb.svg" :scale 0.6)
   '("https?://youtu.be/\\(.+\\).+" "YouTube#\\1"
     :symbol "▶" :svg-icon "fa-brands/youtube-rgb.svg" :scale 0.6)
   '("https?://\\([^.]+.\\)?wikipedia.org/wiki/\\(.+\\)" "wiki#\\2"
     :symbol "" :svg-icon "fa-brands/wikipedia.svg" :scale 0.85)
   '("https?://\\(www\\.\\)?instagram.com/\\(.+\\)" "Instagram#\\2"
     :symbol "" :svg-icon "fa-brands/instagram-rgb.svg" :scale 0.85)
   )
  "List of patterns for URL shortening."
  :type 'list
  :group 'telega-modes)

;; - User Option: ~telega-url-shorten-mode-for~, default={{{eval(telega-url-shorten-mode-for)}}}
;;
;;   {{{vardoc(telega-url-shorten-mode-for)}}}
(defcustom telega-url-shorten-mode-for 'all
  "*Chat filter for `global-telega-url-shorten-mode'.
`global-telega-url-shorten-mode' enables urls shortening only for
chats matching this chat filter."
  :type 'list
  :group 'telega-modes)

(defun telega-url-shorten--gen-icon (pattern)
  "Generate icon for the PATTERN."
  (let* ((pattern-trail (cddr pattern))
         (sym (plist-get pattern-trail :symbol))
         (icon-spec (memq :svg-icon pattern-trail))
         (icon-name (cadr icon-spec))
         (icon-props (cddr icon-spec)))
    (when sym
      (if telega-use-images
          (propertize sym 'display
                      `(image :type svg :file ,(telega-etc-file icon-name)
                              :ascent center :height ,(telega-chars-xheight 1)
                              ,@icon-props
                              :scale 1.0))
        sym))))

(defun telega-url-shorten--e-t-p (old-e-t-p entity text)
  (let* ((result (funcall old-e-t-p entity text))
         (result-td (when (eq 'telega-display (car result))
                      (cadr result))))
    (when (eq 'textEntityTypeUrl (telega--tl-type (plist-get entity :type)))
      (let ((patterns telega-url-shorten-patterns)
            tus-pat)
        (while (setq tus-pat (car patterns))
          (when (string-match (car tus-pat) result-td)
            ;; Done
            (setq result
                  (nconc (list 'telega-display
                               (concat (telega-url-shorten--gen-icon tus-pat)
                                       (replace-match
                                        (cadr tus-pat) t nil result-td)))
                         (cddr result))
                  patterns nil))
          (setq patterns (cdr patterns)))))
    result))

;;;###autoload
(define-minor-mode telega-url-shorten-mode
  "Toggle URLs shortening mode."
  :init-value nil :group 'telega-modes
  (if telega-url-shorten-mode
      (advice-add 'telega--entity-to-properties
                  :around 'telega-url-shorten--e-t-p)
    (advice-remove 'telega--entity-to-properties
                   'telega-url-shorten--e-t-p)))

(defun telega-url-shorten-mode--maybe (&optional arg)
  (when (telega-chat-match-p telega-chatbuf--chat telega-url-shorten-mode-for)
    (telega-url-shorten-mode arg)))

;;;###autoload
(define-minor-mode global-telega-url-shorten-mode
  "Global mode to shorten the URLs."
  :init-value nil :global t :group 'telega-modes
  (if global-telega-url-shorten-mode
      (progn
        (add-hook 'telega-chat-mode-hook 'telega-url-shorten-mode--maybe)
        (dolist (buf telega--chat-buffers)
          (with-current-buffer buf
            (telega-url-shorten-mode--maybe 1))))

    (remove-hook 'telega-chat-mode-hook 'telega-url-shorten-mode--maybe)
    (dolist (buf telega--chat-buffers)
      (with-current-buffer buf
        (telega-url-shorten-mode -1)))))


;; ** telega-squash-message-mode
;;
;; Minor mode for chatbuf to squash messages into single one while
;; nobody see this.
;;
;; Squashing mean adding contents of the new message to the previous
;; message by editing contents of the previous message.
;;
;; New message in chat is squahed into your previous message only if
;; all the conditions are met:
;;
;; 1. Last message in chat is sent by you
;; 2. Nobody seen your last message
;; 3. Last and new message are both text messages
;; 4. Last and new messages are *not* replying to any message
;;
;; Can be enabled globally in all chats matching
;; ~telega-squash-message-mode-for~ (see below) chat filter with
;; ~(global-telega-squash-message-mode 1)~ or by adding:
;;
;; #+begin_src emacs-lisp
;; (add-hook 'telega-load-hook 'global-telega-squash-message-mode)
;; #+end_src

;; Customizable options:
;;
;; - User Option: ~telega-squash-message-mode-for~
;;
;;   {{{vardoc(telega-squash-message-mode-for)}}}
;;
;;   By default messages are not squashed in "Saved Messages" and in
;;   your channels.
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
        (dolist (buf telega--chat-buffers)
          (with-current-buffer buf
            (telega-squash-message-mode--maybe 1))))

    (remove-hook 'telega-chat-mode-hook 'telega-squash-message-mode--maybe)
    (dolist (buf telega--chat-buffers)
      (with-current-buffer buf
        (telega-squash-message-mode -1)))))

(defun telega-squash-message--concat-text (fmt-text1 fmt-text2 &optional sep)
  "Concat two formatted texts FMT-TEXT1 and FMT-TEXT2 into one."
  (let* ((txt1 (concat (plist-get fmt-text1 :text) (or sep "")))
         (ent2-off (length txt1)))
    (list :@type "formattedTex"
          :text (concat txt1 (plist-get fmt-text2 :text))
          :entities (seq-concatenate
                     'vector (plist-get fmt-text1 :entities)
                     (mapcar (lambda (ent)
                               (list :@type "textEntity"
                                     :offset (+ ent2-off (plist-get ent :offset))
                                     :length (plist-get ent :length)
                                     :type (plist-get ent :type)))
                             (plist-get fmt-text2 :entities))))))

(defsubst telega-squash-message--squash (chat imc reply-to-msg)
  "Return non-nil if message has been squashed."
  ;; Squash only of all conditions are ment:
  ;;  1. Last message in chat is sent by you
  ;;  2. Nobody seen your last message
  ;;  3. Last and new message are both text messages
  ;;  4. Last and new messages are *not* replying to some message
  (with-telega-chatbuf chat
    (when (and telega-squash-message-mode
               ;; Check 3. and 4. for new message
               (not reply-to-msg)
               (eq (telega--tl-type imc) 'inputMessageText))
      (let ((last-msg (plist-get chat :last_message))
            (last-read-id (plist-get chat :last_read_outbox_message_id)))
        (when (and last-msg
                   ;; Checking for 1. 2. 3. and 4.
                   (telega-msg-by-me-p last-msg)
                   (< last-read-id (plist-get last-msg :id))
                   (eq (telega--tl-type (plist-get last-msg :content))
                       'messageText)
                   (zerop (plist-get last-msg :reply_to_message_id)))

          ;; Squashing IMC with `last-msg' by modifying IMC
          (plist-put imc :text (telega-squash-message--concat-text
                                (telega--tl-get last-msg :content :text)
                                (plist-get imc :text)
                                "\n"))
          (telega--editMessageText telega-chatbuf--chat last-msg imc)
          t)))))

(defun telega-squash-message--send-message (send-msg-fun chat imc &optional reply-to-msg)
  "Advice for `telega--sendMessage' used to squash messages."
  (unless (telega-squash-message--squash chat imc reply-to-msg)
    (funcall send-msg-fun chat imc reply-to-msg)))


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

(declare-function telega-msg-type-p "telega-msg" (msg-type msg))
(declare-function telega-msg-chat "telega-msg" (msg &optional offline-p))

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

(provide 'telega-modes)

;;; telega-modes.el ends here

;;; telega-company.el --- Completions with company for telega  -*- lexical-binding:t -*-

;; Copyright (C) 2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Feb  8 01:37:44 2019
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

;; company-mode backends to be used in telega:
;;  `telega-company-emoji' to complete emojis
;;  `telega-company-username' to complete usernames

;;; Code:
(require 'telega-core)
(require 'telega-tdlib)
(require 'telega-util)
(require 'telega-user)

(defvar company-backend)
(defvar company-minimum-prefix-length)
(defvar company-tooltip-minimum)
(declare-function company--row "company" (&optional pos))
(declare-function company--pseudo-tooltip-height "company")
(declare-function company-begin-backend "company" (backend &optional callback))
(declare-function company-grab "company" (regexp &optional expression limit))
(declare-function company-grab-line "company" (regexp &optional expression))
(declare-function company-call-backend "company" (&rest args))

(declare-function telega-chat--info "telega-chat" (chat))
(declare-function telega-chat--type "telega-chat" (chat &optional no-interpret))
(declare-function telega-chatbuf-attach-inline-bot-query "telega-chat" (&optional no-empty-search))
(declare-function telega--full-info "telega-info" (tlobj))

(defun telega-company-grab-single-char (char)
  "Grab string starting with single CHAR.
Matches only if CHAR does not apper in the middle of the word."
  (let ((p (point)))
    (save-excursion
      (when (looking-at "\\>")
        (skip-syntax-backward "w"))
      (when (= (char-before) char)
        (let ((char-str (char-to-string char)))
          (skip-chars-backward char-str)
          (unless (looking-at "\\>")
            (cons (buffer-substring p (point))
                  company-minimum-prefix-length)))))))


;;; Emoji completion
(defun telega-company-grab-emoji ()
  (let ((cg (company-grab ":[^: _]+" nil
                          (- (point) telega-emoji-max-length))))
    (when cg (cons cg company-minimum-prefix-length))))

(defun telega-company-emoji-annotation (emoji)
  "Generate annotation for the EMOJI."
  ;; NOTE: if `telega-emoji-use-images' is used, use "EE" as
  ;; corresponding string for better formatting.
  ;; Flag, Fitzpatrick's emojis and emojis with ZWJ char has `1'
  ;; width, though occupies 2 or more chars
  (concat "  "
          (if telega-emoji-use-images
              (propertize "EE" 'display (telega-emoji-create-svg emoji))
            emoji)))

;;;###autoload
(defun telega-company-emoji (command &optional arg &rest ignored)
  "Backend for `company' to complete emojis."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'telega-company-emoji))
    (init (telega-emoji-init))
    (require-match 'never)
    (sorted t)
    ;; Always match if having `:'
    (prefix (telega-company-grab-emoji))
    ;; No caching for fuzzy matching, otherwise it won't work
    (no-cache telega-emoji-fuzzy-match)
    (candidates
     (cl-remove-if-not
      (lambda (en)
        (or (string-prefix-p arg en)
            (and telega-emoji-fuzzy-match
                 (string-match-p
                  (regexp-quote (concat "-" (substring arg 1))) en))))
      telega-emoji-candidates))
    (annotation
     (telega-company-emoji-annotation
      (cdr (assoc arg telega-emoji-alist))))
    (post-completion
     (delete-region (- (point) (length arg)) (point))
     (let ((emoji (cdr (assoc arg telega-emoji-alist))))
       (insert emoji)))
    ))

(defun telega-company-telegram-emoji-gen-candidates (text)
  "Generate callback to asynchronously fetch emoji candidates for TEXT."
  ;; Replace `-' with spaces before the search, so one could use `:i-love-you'
  (cons :async
        (lambda (callback)
          (telega--searchEmojis
           (replace-regexp-in-string
            (regexp-quote "-") " " (substring text 1))
           nil nil
           (lambda (emojis)
             (funcall callback
                      (mapcar (lambda (emoji)
                                (propertize text 'emoji emoji))
                              emojis)))))))

;;;###autoload
(defun telega-company-telegram-emoji (command &optional arg &rest ignored)
  "Backend for `company' to complete emojis using `searchEmojis' TDLib method."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'telega-company-telegram-emoji))
    (require-match 'never)
    (sorted t)
    ;; Always match if having `:'
    (prefix (telega-company-grab-emoji))
    (candidates (telega-company-telegram-emoji-gen-candidates arg))
    (annotation
     (telega-company-emoji-annotation (get-text-property 0 'emoji arg)))
    (post-completion
     (let ((emoji (get-text-property 0 'emoji arg)))
       (delete-region (- (point) (length arg)) (point))
       (insert emoji)))
    ))


;;; Username completion for chat buffer
(defun telega-company-grab-username ()
  "Grab string starting with `@'."
  (telega-company-grab-single-char ?\@))

;;;###autoload
(defun telega-company-username (command &optional arg &rest ignored)
  "Backend for `company' to complete usernames."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'telega-company-username))
    (init (unless (eq major-mode 'telega-chat-mode)
            (error "`telega-company-username' can be used only in chat buffer")))
    (sorted t)
    (prefix (telega-company-grab-username))
    (require-match 'never)
    (candidates
     (cl-assert (> (length arg) 0))
     (let ((members
            (telega--searchChatMembers
             telega-chatbuf--chat (substring arg 1)
             ;; NOTE: not using "chatMembersFilterMention"
             ;; see https://github.com/tdlib/td/issues/1393
             ;; (list :@type "chatMembersFilterMention"
             ;;       :message_thread_id (or (plist-get telega-chatbuf--thread-msg
             ;;                                         :message_thread_id)
             ;;                              0))
             )))
       (or (nconc (mapcar (lambda (member)
                            (propertize
                             (or (telega-msg-sender-username member 'with-@)
                                 (telega-msg-sender-title member))
                             'telega-member member))
                          members)
                  (cl-remove-if-not (lambda (botname)
                                      (string-prefix-p arg botname 'ignore-case))
                                    (cl-union telega--recent-inline-bots
                                              telega-known-inline-bots
                                              :test #'string=)))

           ;; NOTE: In case there is no candidates, and `arg' starts
           ;; some username from Main chat list, then complete it
           (cl-remove-if-not
            (lambda (username)
              (and username (string-prefix-p arg username 'ignore-case)))
            (mapcar (lambda (chat)
                      (when (telega-chat-match-p chat
                              telega-company-username-complete-nonmember-for)
                        (telega-msg-sender-username chat 'with-@)))
                    telega--ordered-chats))
           )))
    (annotation
     ;; Use non-nil `company-tooltip-align-annotations' to align
     (when-let ((member (or (get-text-property 0 'telega-member arg)
                            (telega-user--by-username arg))))
       (telega-ins--as-string
        (telega-ins "  ")
        (telega-ins (telega-msg-sender-title member))
        (when telega-company-username-show-avatars
          (insert-image
           (telega-msg-sender-avatar-image-one-line member))))))
    (post-completion
     (when-let ((member (get-text-property 0 'telega-member arg)))
       (unless (or (telega-msg-sender-username member)
                   (telega-chat-p member))
         (delete-region (- (point) (length arg)) (point))
         (telega-ins (telega-string-as-markup
                      (format "[%s](tg://user?id=%d)"
                              (telega-msg-sender-title member)
                              (plist-get member :id))
                      "markdown1" #'telega-markup-markdown1-fmt))))

     (insert " ")
     (let ((chatbuf-input (telega-chatbuf-input-string)))
       (when (or (member chatbuf-input telega-known-inline-bots)
                 (member chatbuf-input telega--recent-inline-bots))
         (telega-chatbuf-attach-inline-bot-query 'no-search))))
    ))


;;; Hashtags completion for chatbuffer
(defun telega-company-grab-hashtag ()
  "Grab string starting with `#'."
  (telega-company-grab-single-char ?\#))

;;;###autoload
(defun telega-company-hashtag (command &optional arg &rest ignored)
  "Backend for `company' to complete recent hashtags."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'telega-company-hashtag))
    (init (unless (eq major-mode 'telega-chat-mode)
            (error "`telega-company-hashtag' can be used only in chat buffer")))
    (sorted t)
    (prefix (telega-company-grab-hashtag))
    (require-match 'never)
    (candidates
     (cl-assert (> (length arg) 0))
     (mapcar (lambda (ht) (concat "#" ht))
             (telega--searchHashtags (substring arg 1))))
    (post-completion
     (insert " "))
    ))


;;; Bot commands completion
(defun telega-company-grab-botcmd ()
  "Return non-nil if chatbuf input starts bot command."
  (let ((cg (company-grab-line "/[^ ]*")))
    (when (and cg (= telega-chatbuf--input-marker (match-beginning 0)))
      (cons cg company-minimum-prefix-length))))

(defun telega-company--bot-commands-list (bot-commands &optional suffix)
  (mapcar (lambda (bot-cmd)
            (propertize (concat "/" (telega-tl-str bot-cmd :command) suffix)
                        'telega-annotation
                        (telega-ins--as-string
                         (telega-ins--with-attrs
                             (list :max (/ telega-chat-fill-column 2) :elide t)
                           (telega-ins (telega-tl-str bot-cmd :description))))))
          bot-commands))

(defun telega-company--bot-commands ()
  (cl-assert telega-chatbuf--chat)
  (let* ((info (telega-chat--info telega-chatbuf--chat))
         (full-info (telega--full-info info)))
    (if (eq 'bot (telega-chat--type telega-chatbuf--chat))
        (telega-company--bot-commands-list (plist-get full-info :commands))
      (apply #'nconc
             (mapcar (lambda (bot-commands)
                       (telega-company--bot-commands-list
                        (plist-get bot-commands :commands)
                        (let ((bot-user (telega-user-get
                                         (plist-get bot-commands :bot_user_id))))
                          (concat "@" (telega-tl-str bot-user :username)))))
                     (plist-get full-info :bot_commands))))))

;;;###autoload
(defun telega-company-botcmd (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'telega-company-botcmd))
    (require-match 'never)
    (sorted t)
    ;; Always match if having `/'
    (prefix (telega-company-grab-botcmd))
    (candidates
     (all-completions arg (telega-company--bot-commands)))
    (annotation
     (get-text-property 0 'telega-annotation arg))
    ))


;; Functionality to show company tooltip always below the point
(defvar telega-company--chatbuf-row nil
  "Current row in the chatbuf before showing company tooltip.
Used when `telega-company-tooltip-always-below' is non-nil.")
(make-variable-buffer-local 'telega-company--chatbuf-row)

(defun telega-company--chatbuf-move-row (orig-show-func row &rest args)
  "Reserve space below the point so company tooltip will be shown below.
Only if `telega-company-tooltip-always-below' is non-nil."
  (let (saved-chatbuf--row)
    (when (and telega-company-tooltip-always-below
               telega-chatbuf--chat)
      ;; NOTE: If ROW is moved, then save original row to the
      ;; `telega-company--chatbuf-row' to restore it later when
      ;; tooltip hides
      (let ((height (company--pseudo-tooltip-height)))
        (when (< height 0)
          (setq saved-chatbuf--row row)
          (recenter (- (1+ company-tooltip-minimum)))
          (setq row (1+ (company--row))))))

    (apply orig-show-func row args)

    ;; NOTE: Set `telega-company--chatbuf-row' *after* calling to orig
    ;; func, because it calls `company-pseudo-tooltip-hide'
    (setq telega-company--chatbuf-row saved-chatbuf--row)))

(defun telega-company--restore-row ()
  "Restore original point row before additional space reservation.
Only if `telega-company-tooltip-always-below' is non-nil."
  (when (and telega-company-tooltip-always-below
             telega-company--chatbuf-row)
    (cl-assert  telega-chatbuf--chat)
    (let ((restore-row telega-company--chatbuf-row))
      (setq telega-company--chatbuf-row nil)
      (recenter restore-row))))


;; Utility functions
(defun telega-company--grab-backend (what)
  "Return prefix or a backend for input at point.
WHAT is one of `prefix', `backend' or `prefix-and-backend'"
  (let* ((prefix nil)
         (backend (cl-find-if (lambda (b)
                                (let ((company-backend b))
                                  (setq prefix (company-call-backend 'prefix))))
                              (list 'telega-company-username
                                    telega-emoji-company-backend
                                    'telega-company-hashtag
                                    'telega-company-botcmd))))
    (when prefix
      (cl-ecase what
        (prefix prefix)
        (backend backend)
        (prefix-and-backend (cons prefix backend))))))

(provide 'telega-company)


(advice-add 'company-pseudo-tooltip-show
            :around #'telega-company--chatbuf-move-row)
(advice-add 'company-pseudo-tooltip-hide
            :after 'telega-company--restore-row)

;;; telega-company.el ends here

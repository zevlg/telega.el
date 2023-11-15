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
(require 'telega-emoji)

(defvar company-backend)
(defvar company-minimum-prefix-length)
(defvar company-tooltip-minimum)
(declare-function company--row "company" (&optional pos))
(declare-function company--pseudo-tooltip-height "company")
(declare-function company-begin-backend "company" (backend &optional callback))
(declare-function company-grab "company" (regexp &optional expression limit))
(declare-function company-grab-line "company" (regexp &optional expression))
(declare-function company-call-backend "company" (&rest args))
(declare-function company-other-backend "company" (&optional backend))

(declare-function telega-chat--info "telega-chat" (chat))
(declare-function telega-chatbuf--message-thread-id "telega-chat")
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


;;; ellit-org: company-backends
;;
;; - telega-company-emoji :: Complete emojis via ~:<emoji>:~
;;   syntax. Completion is done using predefined set of emojis.
;;
;;   Customizable Options:
;;   - {{{user-option(telega-company-emoji-fuzzy-match, 4)}}}

(defun telega-company-grab-emoji ()
  (let ((cg (company-grab "\\(?:^\\|[[:space:]]\\)\\(:[^: _]+\\)" 1
                          (- (point) telega-emoji-candidate-max-length))))
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
(defun telega-company-emoji (command &optional arg &rest _ignored)
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
    (no-cache telega-company-emoji-fuzzy-match)
    (candidates
     (or (cl-remove-if-not
          (lambda (en)
            (or (string-prefix-p arg en)
                (and telega-company-emoji-fuzzy-match
                     (string-match-p
                      (regexp-quote (concat "-" (substring arg 1))) en))))
          telega-emoji-candidates)

         ;; NOTE: Pass control to other emoji completion backend if no
         ;; candidates
         (company-other-backend)))
    (annotation
     (telega-company-emoji-annotation
      (cdr (assoc arg telega-emoji-alist))))
    (post-completion
     (delete-region (- (point) (length arg)) (point))
     (let ((emoji (cdr (assoc arg telega-emoji-alist))))
       (insert emoji)))
    ))

;;; ellit-org: company-backends
;;
;; - telega-company-telegram-emoji :: Same as ~telega-company-emoji~,
;;   but uses Telegram cloud for the emojis completion.
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
(defun telega-company-telegram-emoji (command &optional arg &rest _ignored)
  "Backend for `company' to complete emojis using `searchEmojis' TDLib method."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'telega-company-telegram-emoji))
    (require-match 'never)
    (sorted t)
    ;; Always match if having `:'
    (prefix (telega-company-grab-emoji))
    (candidates
     (or (telega-company-telegram-emoji-gen-candidates arg)

         ;; NOTE: Pass control to other emoji completion backend if no
         ;; candidates
         (company-other-backend)))
    (annotation
     (telega-company-emoji-annotation (get-text-property 0 'emoji arg)))
    (post-completion
     (let ((emoji (get-text-property 0 'emoji arg)))
       (delete-region (- (point) (length arg)) (point))
       (insert emoji)))
    ))


;;; ellit-org: company-backends
;;
;; - telega-company-username :: Complete user mentions via ~@<username>~
;;   syntax. Here is the screenshot, showing use of this backend:
;;   [[file:https://zevlg.github.io/telega/completing-usernames.jpg]]
;;
;; Customizable options:
;; - {{{user-option(telega-company-username-prefer-name, 2)}}}
;; - {{{user-option(telega-company-username-show-avatars, 2)}}}
;; - {{{user-option(telega-company-username-markup, 2)}}}
(defun telega-company-grab-username ()
  "Grab string starting with `@'."
  (telega-company-grab-single-char ?\@))

;;;###autoload
(defun telega-company-username (command &optional arg &rest _ignored)
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
             ;; NOTE: "chatMembersFilterMention" might have some
             ;; issues (see https://github.com/tdlib/td/issues/1393).
             ;; However, using "chatMembersFilterMention" is essential
             ;; because of Topics feature.
             (list :@type "chatMembersFilterMention"
                   :message_thread_id (telega-chatbuf--message-thread-id))
             )))
       (or (nconc (mapcar (lambda (member)
                            (propertize
                             (or (telega-msg-sender-username member 'with-@)
                                 (telega-msg-sender-title member))
                             'telega-member member
                             'telega-input arg))
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
        (telega-ins--msg-sender member
          :with-avatar-p telega-company-username-show-avatars))))
    (post-completion
     (when-let ((input (get-text-property 0 'telega-input arg))
                (member (get-text-property 0 'telega-member arg)))
       ;; Name you get after completion is controlled by
       ;; `telega-company-username-prefer-name' user option
       (when (telega-user-p member)
         (delete-region (- (point) (length arg)) (point))
         (when-let* ((fmt-names telega-company-username-prefer-name)
                     (name (let ((tmp-name nil))
                             (while (and fmt-names (not tmp-name))
                               (setq tmp-name (telega-user-title
                                               member (car fmt-names) 'raw)
                                     fmt-names (cdr fmt-names)))
                             tmp-name)))
           (telega-ins
            (cond ((string-prefix-p "@" name)
                   name)

                  ((member telega-company-username-markup
                           '("markdown1" "markdown2"))
                   (telega-string-as-markup
                       (format "[%s](tg://user?id=%d)"
                               name (plist-get member :id))
                       telega-company-username-markup
                       (cdr (assoc telega-company-username-markup
                                   telega-chat-markup-functions))))
                  (t
                   (propertize
                    name
                    :tl-entity-type (list :@type "textEntityTypeMentionName"
                                          :user_id (plist-get member :id))
                    'face 'telega-entity-type-mention
                    ;; NOTE: Make it editable from the end, and not
                    ;; editable from the beginning, see
                    ;; https://t.me/emacs_telega/38257
                    'rear-nonsticky nil
                    'front-sticky nil
                    )))))))
     (insert " ")
     (let ((chatbuf-input (telega-chatbuf-input-string)))
       (when (or (member chatbuf-input telega-known-inline-bots)
                 (member chatbuf-input telega--recent-inline-bots))
         (telega-chatbuf-attach-inline-bot-query 'no-search))))
    ))


;;; ellit-org: company-backends
;;
;; - telega-company-hashtag :: Complete common hashtags via
;;   ~#<hashtag>~ syntax.
(defun telega-company-grab-hashtag ()
  "Grab string starting with `#'."
  (telega-company-grab-single-char ?\#))

;;;###autoload
(defun telega-company-hashtag (command &optional arg &rest _ignored)
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


;;; ellit-org: company-backends
;;
;; - telega-company-botcmd :: Complete bot commands via ~/<botcmd>~
;;   syntax.  This backend does not complete if ~/<botcmd>~ syntax is
;;   used in the middle of the chatbuf input, only if ~/<botcmd>~
;;   starts chatbuf input.
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
         (telega-full-info-offline-p nil)
         (full-info (telega--full-info info)))
    (if (telega-chatbuf-match-p '(type bot))
        (telega-company--bot-commands-list
         (telega--tl-get full-info :bot_info :commands))
      (apply #'nconc
             (mapcar (lambda (bot-commands)
                       (telega-company--bot-commands-list
                        (plist-get bot-commands :commands)
                        (let ((bot-user (telega-user-get
                                         (plist-get bot-commands :bot_user_id))))
                          (telega-msg-sender-username bot-user 'with-@))))
                     (plist-get full-info :bot_commands))))))

;;;###autoload
(defun telega-company-botcmd (command &optional arg &rest _ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'telega-company-botcmd))
    (require-match 'never)
    (sorted t)
    ;; Complete only if chatbuf has corresponding bot
    (prefix
     (when (telega-chatbuf-match-p '(type bot))
       (telega-company-grab-botcmd)))
    (candidates
     (all-completions arg (telega-company--bot-commands)))
    (annotation
     (get-text-property 0 'telega-annotation arg))
    ))


;;; ellit-org: company-backends
;;
;; - telega-company-markdown-precode :: Complete language name for
;;   code blocks via ~```~ syntax.
(defun telega-company-grab-markdown-precode ()
  "Return non-nil if chatbuf input starts source block."
  (when-let ((cg (company-grab "```\\([^`\t\n ]*\\)" 1)))
    (cons cg company-minimum-prefix-length)))

(defun telega-company--language-names ()
  "Return list of all language names.
Sort modes by usage of current Emacs session."
  (let* ((all-buffers (buffer-list))
         (modes
          (seq-uniq (seq-filter #'symbolp (mapcar #'cdr auto-mode-alist))))
         (indexed-modes
          (mapcar (lambda (mode)
                    (cons mode (seq-count
                                (lambda (buffer)
                                  (eq (buffer-local-value 'major-mode buffer)
                                      mode))
                                all-buffers)))
                  modes))
         (sorted-modes
          (mapcar #'car (cl-sort indexed-modes #'> :key #'cdr))))
    (delq nil (mapcar (lambda (mode)
                        (let ((mode-name (symbol-name mode)))
                          (when (string-suffix-p "-mode" mode-name)
                            (substring mode-name 0 -5))))
                      sorted-modes))))

;;;###autoload
(defun telega-company-markdown-precode (command &optional arg &rest _ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'telega-company-markdown-precode))
    (require-match 'never)
    ;; Always match if line starts with "```"
    (prefix (telega-company-grab-markdown-precode))
    (sorted t)
    (candidates
     (all-completions arg (telega-company--language-names)))
    (post-completion
     (if (save-excursion (re-search-forward "^```" nil 'noerror))
         (forward-char)
       (insert "\n")
       (save-excursion (insert "\n```"))))
    ))


;; Utility functions
(defun telega-company--grab-backend (what)
  "Return prefix or a backend for input at point.
WHAT is one of `prefix', `backend' or `prefix-and-backend'"
  (let* ((prefix nil)
         (backend (cl-find-if (lambda (b)
                                (let ((company-backend b))
                                  (setq prefix (company-call-backend 'prefix))))
                              telega-company-backends)))
    (when prefix
      (cl-ecase what
        (prefix prefix)
        (backend backend)
        (prefix-and-backend (cons prefix backend))))))

;; Functionality to show company tooltip always below the point
(defun telega-company--chatbuf-move-row (orig-show-func row &rest args)
  "Reserve space below the point so company tooltip will be shown below.
Only if `telega-company-tooltip-always-below' is non-nil."
  (when (and telega-company-tooltip-always-below
             telega-chatbuf--chat)
    (let ((height (company--pseudo-tooltip-height)))
      (when (< height 0)
        (recenter (- (1+ company-tooltip-minimum)))
        (setq row (1+ (company--row))))))

  (apply orig-show-func row args))

(provide 'telega-company)


(advice-add 'company-pseudo-tooltip-show
            :around #'telega-company--chatbuf-move-row)

;;; telega-company.el ends here

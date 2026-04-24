;;; telega-company.el --- Completions with company for telega  -*- lexical-binding:t -*-

;; Copyright (C) 2019-2026 by Zajcev Evgeny.

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

;;; ellit-org:
;; ** /telega-company.el/ -- Powerful completions with company
;; 
;; There are two operating modes: active and passive. In active mode,
;; =company-mode= is enabled, detects input context automatically and
;; pops up completion candidates if completion at point is available.
;; With passive mode you need to press
;; {{{where-is(telega-chatbuf-complete-or-next-link,telega-chat-mode-map)}}}
;; explicitly to perform completion at point.  Operating mode is
;; customized with:
;; - {{{user-option(telega-company-active, 2)}}}
;;
;; Enable company completions with:
;;;; #+begin_src elisp
;; (add-hook 'telega-chat-mode-hook 'telega-completions-setup-company)
;; #+end_src
;;
;; Consider also using =company-posframe= Emacs package (in MELPA), so
;; chatbuf's contents remain untouched when completion menu pops above
;; the chatbuf prompt.
;; 
;; If you don't like =company-posframe=, consider option:
;; - {{{user-option(telega-company-tooltip-always-below, 2)}}}
;;
;; User options you might want to customize:
;; - {{{user-option(telega-company-active, 2)}}}
;; - {{{user-option(telega-company-backends, 2)}}}
;; - {{{user-option(telega-company-tooltip-always-below, 2)}}}
;; 
;; Company backends provided by =telega-company=:

;;; Code:
(require 'telega)
(require 'company)

(defgroup telega-company nil
  "Customization for telega company completion."
  :group 'telega)

(defcustom telega-company-active t
  "*Non-nil to use `telega-company' in active mode."
  :package-version '(telega . "0.8.632")
  :type 'boolean
  :group 'telega-company)
  
(defcustom telega-company-backends '(telega-company-emoji
                                     telega-company-telegram-emoji
                                     telega-company-username
                                     telega-company-hashtag
                                     telega-company-markdown-precode
                                     telega-company-quick-reply
                                     telega-company-botcmd
                                     )
  "Company backends to use in chat buffers.
Set to nil to disable company completions in chat buffers."
  :package-version '(telega . "0.8.170")
  :type '(repeat function)
  :group 'telega-company)

(defcustom telega-company-tooltip-always-below t
  "Non-nil to show company tooltip always below the point.
Done by recentering point in the chatbuf."
  :package-version '(telega . "0.7.47")
  :type 'boolean
  :group 'telega-company)


(defun telega-company-grab-single-char (char)
  "Grab string starting with single CHAR.
Matches only if CHAR does not apper in the middle of the word."
  (when-let* ((bounds (telega-capf--bounds-for-char char)))
    (cons (buffer-substring (car bounds) (cdr bounds))
          company-minimum-prefix-length)))


;;; ellit-org:
;;
;; - telega-company-emoji :: Complete emojis via ~:<emoji>:~
;;   syntax. Completion is done using predefined set of emojis.
;;
;;   Customizable Options:
;;   - {{{user-option(telega-completions-emoji-fuzzy-match, 4)}}}

(defun telega-company-grab-emoji ()
  (let ((cg (company-grab "\\(?:^\\|[[:space:]]\\)\\(:[^: _]+\\)" 1
                          (- (point) telega-emoji-candidate-max-length))))
    (when cg (cons cg company-minimum-prefix-length))))

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
    (no-cache telega-completions-emoji-fuzzy-match)
    (candidates
     (or (telega-completions--emoji-candidates arg)
         ;; NOTE: Pass control to other emoji completion backend if no
         ;; candidates
         (company-other-backend)))
    (annotation
     (telega-completions--emoji-annotation
      (cdr (assoc arg telega-emoji-alist))))
    (post-completion
     (telega-completions--emoji-post-completion
      arg (cdr (assoc arg telega-emoji-alist))))
    ))

;;; ellit-org:
;;
;; - telega-company-telegram-emoji :: Same as ~telega-company-emoji~,
;;   but uses Telegram cloud for the emojis completion.
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
     (or (cons :async
               (apply-partially
                (telega-completions--telegram-emoji-candidates) arg))

         ;; NOTE: Pass control to other emoji completion backend if no
         ;; candidates
         (company-other-backend)))
    (annotation
     (when-let* ((emoji (get-text-property 0 'telega-emoji arg)))
       (telega-completions--emoji-annotation emoji)))
    (post-completion
     (when-let* ((emoji (get-text-property 0 'telega-emoji arg)))
       (telega-completions--emoji-post-completion arg emoji)))
    ))


;;; ellit-org:
;;
;; - telega-company-username :: Complete user mentions via ~@<username>~
;;   syntax. Here is the screenshot, showing use of this backend:
;;   [[file:https://zevlg.github.io/telega/completing-usernames.jpg]]
;;
;;   Use ~@@~ prefix to complete chat admins only.
;;
;; Customizable options:
;; - {{{user-option(telega-completions-username-prefer-name, 2)}}}
;; - {{{user-option(telega-completions-username-show-avatars, 2)}}}
;; - {{{user-option(telega-completions-username-markup, 2)}}}
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
     (telega-completions--username-search telega-chatbuf--chat arg))
    (annotation
     ;; Use non-nil `company-tooltip-align-annotations' to align
     (telega-completions--username-annotation arg))
    (post-completion
     (telega-completions--username-post-completion arg))
    ))


;;; ellit-org:
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
     (telega-completions--hashtag-search arg))
    (post-completion
     (insert " "))
    ))


;;; ellit-org:
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

;;;###autoload
(defun telega-company-botcmd (command &optional arg &rest _ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'telega-company-botcmd))
    (require-match 'never)
    (sorted t)
    ;; Complete only if chatbuf has corresponding bot
    (prefix
     (when-let* ((prefix (telega-company-grab-botcmd))
                 ((telega-completions--bot-commands telega-chatbuf--chat)))
       prefix))
    (candidates
     (all-completions arg
                      (telega-completions--bot-commands telega-chatbuf--chat)))
    (annotation
     (telega-completions--annotation arg))
    ))


;;; ellit-org:
;;
;; - telega-company-quick-reply :: Complete quick replies via
;;   ~/<shortcut>~ syntax in private chats.
(defun telega-company-grab-quick-reply ()
  "Return non-nil if chatbuf input starts a quick reply shortcut."
  (telega-company-grab-single-char ?/))

;;;###autoload
(defun telega-company-quick-reply (command &optional arg &rest _ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'telega-company-quick-reply))
    ;; Complete only if chatbuf is a private non-bot chat
    (prefix
     (when (telega-chatbuf-match-p '(type private))
       (telega-company-grab-quick-reply)))
    (candidates
     (all-completions arg (telega-completions--quick-replies)))
    (annotation
     (telega-completions--quick-reply-annotation arg))
    (post-completion
     ;; TODO: prepare messages and attach them into chatbuf
     (user-error "TODO: post-completion for Quick Replies"))
    ))


;;; ellit-org:
;;
;; - telega-company-markdown-precode :: Complete language name for
;;   code blocks via ~```~ syntax.
(defun telega-company-grab-markdown-precode ()
  "Return non-nil if chatbuf input starts source block."
  (when-let ((cg (company-grab "```\\([^`\t\n ]*\\)" 1)))
    (cons cg company-minimum-prefix-length)))

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
     (all-completions arg (telega-completions--language-names)))
    (post-completion
     (telega-completions--markdown-precode-post-completion))
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


(defun telega-company-chat-input-complete ()
  "Completion function for chatbuf's input."
  (when company-mode
    (when-let ((backend (telega-company--grab-backend 'backend)))
      (company-begin-backend backend)
      (company-complete)
      t)))

;;;###autoload
(defun telega-completions-setup-company (&optional append-p)
  "Setup company for completions in the chatbuf.
If APPEND-P is non-nil, then append telega backends to existing,
otherwise fully replace them.
Intended for use in `telega-chat-mode-hook'."
  (setq-local telega-chat-input-complete-functions
              (append telega-chat-input-complete-functions
                      (list #'telega-company-chat-input-complete)))
  (setq-local company-backends
              (append telega-company-backends
                      (when append-p
                        company-backends)))
  (when telega-company-active
    (company-mode 1))
  )

(provide 'telega-company)


(advice-add 'company-pseudo-tooltip-show
            :around #'telega-company--chatbuf-move-row)

;;; telega-company.el ends here

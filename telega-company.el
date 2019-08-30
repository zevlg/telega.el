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
(require 'telega-util)
(require 'telega-user)

(defvar company-minimum-prefix-length)
(declare-function company-begin-backend "company" (backend &optional callback))
(declare-function company-grab "company" (regexp &optional expression limit))
(declare-function company-grab-line "company" (regexp &optional expression))

(declare-function telega-chat--info "telega-chat" (chat))
(declare-function telega-chat--type "telega-chat" (chat &optional no-interpret))
(declare-function telega--full-info "telega-info" (tlobj))

(defun telega-company-grab-single-char (char)
  "Grab string starting with single CHAR."
  (if (looking-at "\\>")
      (let ((p (point)))
        (save-excursion
          (skip-syntax-backward "w")
          (when (= (char-before) char)
            (cons (buffer-substring p (1- (point)))
                  ;; Always match if CHAR is prefix
                  company-minimum-prefix-length))))

    (when (= (char-before) char)
      (cons (char-to-string char) company-minimum-prefix-length))))


;;; Emoji completion
(defun telega-company-grab-emoji ()
  (let ((cg (company-grab ":[^: _]+" nil
                          (- (point) telega-emoji-max-length))))
    (when cg (cons cg company-minimum-prefix-length))))

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
    (candidates
     (let ((fuzzy-regexp (regexp-quote (concat "-" (substring arg 1)))))
       (cl-remove-if-not (lambda (en)
                           (or (string-prefix-p arg en)
                               (string-match-p fuzzy-regexp en)))
                         telega-emoji-candidates)))
    (annotation
     (concat "  " (cdr (assoc arg telega-emoji-alist))))
    (post-completion
     (delete-region (- (point) (length arg)) (point))
     (insert (cdr (assoc arg telega-emoji-alist))))
    ))


;;; Username completion for chat buffer
;;;###autoload
(defun telega-company-username (command &optional arg &rest ignored)
  "Backend for `company' to complete usernames."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'telega-company-username))
    (init (unless (eq major-mode 'telega-chat-mode)
            (error "`telega-company-username' can be used only in chat buffer")))
    (sorted t)
    (prefix (telega-company-grab-single-char ?\@))
    (require-match 'never)
    (candidates
     (cl-assert (> (length arg) 0))
     (let ((members (telega--searchChatMembers
                     telega-chatbuf--chat (substring arg 1))))
       (delq nil
             (mapcar (lambda (member)
                       (let ((username (plist-get member :username)))
                         (unless (string-empty-p username)
                           (propertize (concat "@" username) 'telega-member member))))
                     members))))
    (annotation
     ;; Use non-nil `company-tooltip-align-annotations' to align
     (let ((member (get-text-property 0 'telega-member arg)))
       (when member
         (concat "  " (telega-user--name member 'name)))))
    (post-completion
     (insert " "))
    ))


;;; Hashtags completion for chatbuffer
;;;###autoload
(defun telega-company-hashtag (command &optional arg &rest ignored)
  "Backend for `company' to complete recent hashtags."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'telega-company-hashtag))
    (init (unless (eq major-mode 'telega-chat-mode)
            (error "`telega-company-hashtag' can be used only in chat buffer")))
    (sorted t)
    (prefix (telega-company-grab-single-char ?\#))
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
  (let ((cg (company-grab-line "/[^ ]*")))
    (when cg (cons cg company-minimum-prefix-length))))

(defun telega-company--bot-commands-alist ()
  (cl-assert telega-chatbuf--chat)
  (when (eq (telega-chat--type telega-chatbuf--chat) 'bot)
    (let* ((info (telega-chat--info telega-chatbuf--chat))
           (full-info (telega--full-info info))
           (bot-info (plist-get full-info :bot_info)))
      (mapcar (lambda (bot-cmd)
                (cons (concat "/" (plist-get bot-cmd :command))
                      (plist-get bot-cmd :description)))
              (plist-get bot-info :commands)))))

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
     (let ((cmd-alist (telega-company--bot-commands-alist)))
       (all-completions arg cmd-alist)))
    (annotation
     (let ((cmd-alist (telega-company--bot-commands-alist)))
       (concat "  " (cdr (assoc arg cmd-alist)))))
    ))

(provide 'telega-company)

;;; telega-company.el ends here

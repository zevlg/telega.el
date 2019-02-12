;;; telega-company.el --- Completions with company for telega

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
(eval-when (load)
  (require 'company))                      ; company-grab


;;; Emoji completions
(defvar telega-emoji-alist nil)
(defvar telega-emoji-candidates nil)
(defvar telega-emoji-max-length 0)

(defun telega-emoji-init ()
  "Initialize emojis."
  (unless telega-emoji-alist
    (setq telega-emoji-alist
          (nconc (with-temp-buffer
                   (insert-file-contents (telega-etc-file "emojis.alist"))
                   (goto-char (point-min))
                   (read (current-buffer)))
                 telega-emoji-custom-alist))
    (setq telega-emoji-candidates (mapcar 'car telega-emoji-alist))
    (setq telega-emoji-max-length
          (apply 'max (mapcar 'length telega-emoji-candidates)))))

;;;###autoload
(defun telega-company-emoji (command &optional arg &rest ignored)
  "Backend for `company' to complete emojis."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'telega-company-emoji))
    (init (telega-emoji-init))
    (sorted t)
    ;; Always match if having `:'
    (prefix (let ((cg (company-grab ":[^: _]+" nil
                                    (- (point) telega-emoji-max-length))))
              (when cg (cons cg company-minimum-prefix-length))))
    (candidates
     (let ((fuzzy-regexp (regexp-quote (concat "-" (substring arg 1)))))
       (cl-remove-if-not (lambda (en)
                           (or (string-prefix-p arg en)
                               (string-match-p fuzzy-regexp en)))
                         telega-emoji-candidates)))
    (annotation
     (concat (make-string (- telega-emoji-max-length (length arg)) ?\s)
             (cdr (assoc arg telega-emoji-alist))))
    (post-completion
     (delete-region (- (point) (length arg)) (point))
     (insert (cdr (assoc arg telega-emoji-alist))))
    ))


;;; Username completion for chat buffer
(defun telega-company-grab-username ()
  "Grab string starting with `@'."
  (if (looking-at "\\>")
      (let ((p (point)))
        (save-excursion
          (skip-syntax-backward "w")
          (when (= (char-before) ?\@)
            (cons (buffer-substring p (1- (point)))
                  ;; Always match if `@' prefix
                  company-minimum-prefix-length))))

    (when (= (char-before) ?\@)
      (cons "" company-minimum-prefix-length))))

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
    (candidates
     (let ((members (telega--searchChatMembers telega-chatbuf--chat arg)))
       (delq nil
             (mapcar (lambda (member)
                       (let ((username (plist-get member :username)))
                         (unless (string-empty-p username)
                           (concat "@" username))))
                     members))))
    (post-completion
     (insert " "))
    ))


;;; Stickers
(defun telega-company-grab-sticker ()
  "If chat buffer has single emoji, then grab it."
  )

(defun telega-company-sticker (command &optional arg &rest ignored)
  "Backend for `company' to complete stickers."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'telega-company-sticker))
    (init (if (eq major-mode 'telega-chat-mode)
              (telega-emoji-init)
            (error "`telega-company-sticker' can be used only in chat buffer")))
    (sorted t)
    (prefix (telega-company-grab-sticker))
    (candidates
     (let ((members (telega--searchChatMembers telega-chatbuf--chat arg)))
       (delq nil
             (mapcar (lambda (member)
                       (let ((username (plist-get member :username)))
                         (unless (string-empty-p username)
                           (concat "@" username))))
                     members))))
    (post-completion
     (insert " "))
    ))

(provide 'telega-company)

;;; telega-company.el ends here

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

(require 'format-spec)

(require 'telega-server)
(require 'telega-filter)

(defvar tracking-buffers)
(declare-function telega "telega" (arg))

(defvar telega-mode-line-string "")
(put 'telega-mode-line-string 'risky-local-variable t)

;;;###autoload
(define-minor-mode telega-mode-line-mode
  "Toggle display of the unread chats/mentions in the modeline."
  :init-value nil :global t :group 'telega
  (setq telega-mode-line-string "")
  (unless global-mode-string
    (setq global-mode-string '("")))

  (if telega-mode-line-mode
      (progn
        (unless (memq 'telega-mode-line-string global-mode-string)
          (setq global-mode-string
                (append global-mode-string '(telega-mode-line-string))))
        (advice-add 'telega--on-updateUnreadMessageCount
                    :after 'telega-mode-line-update)
        (advice-add 'telega--on-updateUnreadChatCount
                    :after 'telega-mode-line-update)
        (advice-add 'telega--on-updateChatUnreadMentionCount
                    :after 'telega-mode-line-update)
        (add-hook 'telega-ready-hook 'telega-mode-line-update)
        (add-hook 'telega-chats-fetched-hook 'telega-mode-line-update)
        (add-hook 'telega-kill-hook 'telega-mode-line-update)
        (add-hook 'tracking-buffer-added-hook 'telega-mode-line-update)
        (add-hook 'tracking-buffer-removed-hook 'telega-mode-line-update)
        (telega-mode-line-update))

    (setq global-mode-string
          (delq 'telega-mode-line-string global-mode-string))
    (advice-remove 'telega--on-updateUnreadMessageCount
                   'telega-mode-line-update)
    (advice-remove 'telega--on-updateUnreadChatCount
                   'telega-mode-line-update)
    (advice-remove 'telega--on-updateChatUnreadMentionCount
                   'telega-mode-line-update)
    (remove-hook 'telega-ready-hook 'telega-mode-line-update)
    (remove-hook 'telega-chats-fetched-hook 'telega-mode-line-update)
    (remove-hook 'telega-kill-hook 'telega-mode-line-update)
    (remove-hook 'tracking-buffer-added-hook 'telega-mode-line-update)
    (remove-hook 'tracking-buffer-removed-hook 'telega-mode-line-update)
    ))

(defcustom telega-mode-line-format-spec " %I %t%c%M"
  "Format for telega modeline
%I - Telegram logo.
%c - Number of unread unmuted chats.
%C - Number of unread messages in unmuted chats.
%m - Number of chats with unread mentions.
%M - Number of unread messages with mentions.
%u - Number of all unread chats.
%U - Number of all unread messages.
%t - Number of chats in tracking-buffers.

For %c and %C `telega-unread-unmuted-modeline' face is used.
For %m and %M `telega-mention-count' face is used.
For %u and %U `shadow' face is used.

If value has non-zero value, then suffix according to
`telega-mode-line-non-zero-suffixes' is inserted."
  :type 'string
  :group 'telega
  :set (lambda (option value)
         (set-default option value)
         (when telega-mode-line-mode
           (telega-mode-line-update))))

(defcustom telega-mode-line-prefixes
  '((?t . "[") (?c . "") (?C . "") (?m . "@") (?M . "@") (?u . "") (?U . ""))
  "Prefixes for non-zero values in `telega-mode-line-format-spec'.
Prefixes inherits corresponding face for %c, %C, %m and %M formats."
  :type 'alist
  :group 'telega)

(defcustom telega-mode-line-suffixes
  '((?t . "] ") (?c . " ") (?C . " ") (?m . " ") (?M . " ") (?u . " ") (?U . " "))
  "Suffixes for non-zero values in `telega-mode-line-format-spec'.
Suffixes does not inherit any faces used in formatting."
  :type 'alist
  :group 'telega)

(defmacro telega-mode-line-filter-gen (&rest filter-spec)
  "Generate filtering command for telega-mode-line-mode."
  `(lambda ()
     (interactive)
     (telega nil)
     (telega-filters-push '(,@filter-spec))))

(defun telega-mode-line-format-spec (c val &optional face cmd help)
  "Create format spec for the C and value VAL."
  (cons
   c
   (if (> (or val 0) 0)
       (concat
        (apply 'propertize
               (format "%s%d" (alist-get c telega-mode-line-prefixes) val)
               (nconc (when face (list 'face face))
                      (when cmd
                        (list 'keymap (make-mode-line-mouse-map 'mouse-1 cmd)
                              'mouse-face 'mode-line-highlight))
                      (when help
                        (list 'help-echo help))))
        (alist-get c telega-mode-line-suffixes))
     "")))

(defun telega-mode-line-update (&rest _ignored)
  "Update value for `telega-mode-line-string'."
  (setq telega-mode-line-string "")

  (when (process-live-p (telega-server--proc))
    ;; NOTE: lazy formatting, do not eval certain form unless it is
    ;; specified in `telega-mode-line-format-spec'
    (let* ((t-val (when (string-match-p (regexp-quote "%t") telega-mode-line-format-spec)
                    (length tracking-buffers)))
           (c-val (when (string-match-p (regexp-quote "%c") telega-mode-line-format-spec)
                    (plist-get telega--unread-chat-count :unread_unmuted_count)))
           (C-val (when (string-match-p (regexp-quote "%C") telega-mode-line-format-spec)
                    (plist-get telega--unread-message-count :unread_unmuted_count)))
           (u-val (when (string-match-p (regexp-quote "%u") telega-mode-line-format-spec)
                    (plist-get telega--unread-chat-count :unread_count)))
           (U-val (when (string-match-p (regexp-quote "%U") telega-mode-line-format-spec)
                    (plist-get telega--unread-message-count :unread_count)))
           (m-chats (when (string-match-p "%[mM]" telega-mode-line-format-spec)
                      (telega-filter-chats '(mention) telega--ordered-chats)))
           (m-val (length m-chats))
           (M-val (apply '+ (mapcar (telega--tl-prop :unread_mention_count) m-chats))))
      (setq telega-mode-line-string
            (format-spec
             telega-mode-line-format-spec
             (list (cons
                    ?I (propertize
                        telega-symbol-telegram
                        'display (telega-logo-image)
                        'keymap (make-mode-line-mouse-map 'mouse-1 'telega)
                        'help-echo "Click to show telega root buffer"))
                   (telega-mode-line-format-spec
                    ?t t-val nil
                    (telega-mode-line-filter-gen tracking)
                    "Click to filter tracking chats")
                   (telega-mode-line-format-spec
                    ?c c-val 'telega-unread-unmuted-modeline
                    (telega-mode-line-filter-gen unread notify)
                    "Click to filter chats with unread/unmuted messages")
                   (telega-mode-line-format-spec
                    ?C C-val 'telega-unread-unmuted-modeline
                    (telega-mode-line-filter-gen unread notify)
                    "Click to filter chats with unread/unmuted messages")
                   (telega-mode-line-format-spec
                    ?m m-val 'telega-mention-count
                    (telega-mode-line-filter-gen mention)
                    "Click to filter chats with mentions")
                   (telega-mode-line-format-spec
                    ?M M-val 'telega-mention-count
                    (telega-mode-line-filter-gen mention)
                    "Click to filter chats with mentions")
                   (telega-mode-line-format-spec
                    ?u u-val 'shadow
                    (telega-mode-line-filter-gen unread)
                    "Click to filter chats with unread messages")
                   (telega-mode-line-format-spec
                    ?U U-val 'shadow
                    (telega-mode-line-filter-gen unread)
                    "Click to filter chats with unread messages"))))))
  (force-mode-line-update))

(provide 'telega-modes)

;;; telega-modes.el ends here

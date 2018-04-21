;;; telega-root.el --- Root buffer for telega.

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sat Apr 14 15:00:27 2018
;; Keywords: 

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defcustom telega-root-buffer-name "*Telega Root*"
  "*Buffer name for telega root buffer."
  :type 'string
  :group 'telega-root)

(defcustom telega-root-recent-chats 10
  "*Number of recent chats to show."
  :type 'integer
  :group 'telega-root)

(defcustom telega-root-mode-line-format "Telega: (%c/%n/%m)"
  "Format for the modeline in telega-root mode.
%n The number of new messages.
%a The number of all messages.
%c The number of chats with new messages.
%m The number of mentions.
%u The number of users seen recently."
  :type 'string
  :group 'telega-root)

(defcustom telega-root-chat-brackets
  '((private "[" "]")
    (secret "{" "}")
    (bot "[[" "]]")
    (basicgroup "(" ")")
    (supergroup "((" "))")
    (channel "<" ">"))
  "Brackets used for different kinds of chats.
%( and %) from `telega-root-chat-format' will use these brackets."
  :group 'telega-root)

(defcustom telega-root-chat-format
  "%40{%A %(%t  %n%r%)%}%p%-20s\n    %M"
  "Format for the chat widget.
%a - Chat avatar
%( - Open bracket for this kind of chat
%) - Close bracket for this kind of chat
%t - Chat title
%n - Short name (username)
%s - Activity status (last seen if any)
%l - Link on http://t.me
%m - Number of members/subscribers
%u - Number of unread messages
%r - Number of unread mentions, with @ char included
%p - Pin character if chat is pinned
%M - Formatted last message"
  :type 'string
  :group 'telega-root)

(defcustom telega-root-chat-formats
  `((true . ,telega-root-chat-format))
  "Format for chats based on predicates.
Predicate is called with one argument - CHAT."
  :type '(repeat (cons (function :tag "Predicate")
                       (string :tag "Format string")))
  :group 'telega-root)
  
(defcustom telega-root-last-msg-format "%t%v %m"
  "Format for the last message in chat.
%t - Timestamp
%v - upload/seen status. V or W
%m - Message body"
  :type 'string
  :group 'telega-root)

(defvar telega-root-chat-keymap (make-sparse-keymap)
  "Keymap when point is under chat widget.")

(defvar telega-root-mode-hook nil
  "Hook run when telega root buffer is created.")

(defvar telega-root-mode-map (make-sparse-keymap)
  "The key map for telega root buffer.")

(define-derived-mode telega-root-mode nil "Telega-Root"
  "The mode for telega root buffer.

\\{telega-root-mode-map}"
  (telega-root--create)
  (add-hook 'kill-buffer-hook 'telega-root-killed nil t))

(defun telega-root-killed ()
  "Run when telega root buffer is killed.

Terminate telega-server and kill all chat buffers."
  )

;; widgets used in root buffer
(defvar telega-root--markers nil)
(defvar telega-root--widget-state nil)
(defvar telega-root--widget-search nil)
(defvar telega-root--widget-chats nil)
(defvar telega-root--widget-groups nil)
(defvar telega-root--widget-users nil)
(defvar telega-root--widget-bots nil)
(defvar telega-root--widget-channels nil)

(defsubst telega-root--state (state)
  "Change current telega-server state to STATE."
  (widget-value-set telega-root--widget-state state))

(define-widget 'telega-chat 'default
  "Widget represeting telega chat."
  :format "%v"
  :action 'telega-root--chat-open
  :keymap telega-root-chat-keymap
  :value-create 'telega-root--widget-value-create
  :value-delete 'telega-root--widget-value-delete)

(defun telega-root--widget-value-create (widget)
  (let ((chat (widget-get widget :value))
        (b (point)))
    (insert (telega-root--chat-format chat))
    (unless (overlayp (widget-get widget :overlay))
      (let ((wov (make-overlay b (point))))
        (overlay-put wov 'telega-chat chat)
        (widget-put widget :overlay wov)))))

(defun telega-root--widget-value-delete (widget)
  (let ((ov (widget-get widget :overlay)))
    (when (overlayp ov)
      (delete-overlay ov))))

(defun telega-root--widget-chat (widget)
  "Return telega chat associated with WIDGET."
  (overlay-get (widget-get widget :overlay) 'telega-chat))

(defun telega-root--widget-start (widget)
  "Return the start of WIDGET."
  (overlay-start (widget-get widget :overlay)))

(defun telega-root--widget-hide (widget)
  (overlay-put (widget-get widget :overlay) 'invisible t))

(defun telega-root--widget-show (widget)
  (overlay-put (widget-get widget :overlay) 'invisible nil))

(defun telega-root--create-section (name marker-name)
  (widget-insert "\n" name "\n" (make-string (length name) ?\-) "\n")
  (setq telega-root--markers
        (plist-put telega-root--markers marker-name (point-marker))))
  
(defun telega-root--create ()
  "Redisplay telega root buffer."
  (with-current-buffer telega-root-buffer-name
    (erase-buffer)
    (setq telega-root--widget-state
          (widget-create 'string :format "State: %v" :value "Unknown"))
    (telega-root--widget-hide
     (setq telega-root--widget-search
           (widget-create 'string :format "Search: %v")))

    (telega-root--create-section "Chats" 'chats)
    (telega-root--create-section "Groups" 'groups)
    (telega-root--create-section "Secrets" 'secrets)
    (telega-root--create-section "Users" 'users)
    (telega-root--create-section "Bots" 'bots)
    (telega-root--create-section "Channels" 'channels)
    ))

(defun telega-root--goto-order-point ()
  (telega-root--goto-start 'chats))

(defun telega-root--chat-new (chat)
  (telega-debug "TODO: `telega-root--chat-new'")
  (with-current-buffer telega-root-buffer-name
    (goto-char (plist-get telega-root--markers 'chats))
    (widget-create 'telega-chat :value chat)
    (widget-insert "\n"))
  )

(defun telega-root--chat-reorder (chat)
  (telega-debug "TODO: `telega-root--chat-reorder'")
  )

;;; Navigation
(defun telega-root--goto-prop (prop-name prop-change-func error)
  (let ((pnt (point))
        (orig-val (get-text-property (point) prop-name))
        (nval nil))
    (while (and pnt (or (null nval) (equal orig-val nval)))
      (setq pnt (funcall prop-change-func pnt prop-name)
            nval (get-text-property (or pnt (point)) prop-name)))
    (unless pnt
      (signal error nil))
    (goto-char pnt)))

(defun telega-root--goto-chat (direction)
  (telega-root--goto-prop
   'telega-chat
   (if (eq direction :next)
       #'next-single-property-change
     #'previous-single-property-change)
   (if (eq direction :next)
       'end-of-buffer
     'beginning-of-buffer)))

(defun telega-root-next-chat (&optional n)
  "Jump to next N chat."
  (interactive "p")
  (dotimes (_ n)
    (telega-root--goto-chat :next)))

(defun telega-root-prev-chat (&optional n)
  "Jump to previous N chat."
  (interactive "p")
  (dotimes (_ n)
    (telega-root--goto-chat :prev)))

(provide 'telega-root)

;;; telega-root.el ends here

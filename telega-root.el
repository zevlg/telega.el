;;; telega-root.el --- Root buffer for telega

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
(require 'wid-edit)
(require 'telega-core)
(require 'telega-customize)
(require 'telega-filter)

(defgroup telega-root nil
  "Customization for telega-root-mode"
  :prefix "telega-root-"
  :group 'telega)

(defcustom telega-root-buffer-name "*Telega Root*"
  "*Buffer name for telega root buffer."
  :type 'string
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
  :type 'list
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

(defvar telega-root-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "/") telega-filter-map)
    map)
  "The key map for telega root buffer.")

(define-derived-mode telega-root-mode nil "Telega-Root"
  "The mode for telega root buffer.

\\{telega-root-mode-map}"
  (setq telega-root--widget-state
        (widget-create 'default
                       :format "Status: %v\n"
                       :value-create 'widget-item-value-create
                       :value "Unknown"))
  (setq telega-root--widget-search
        (widget-create 'editable-field
                       :size 40
                       :format "Search: %v\n"
                       :action 'telega-search))
  (setq telega-root--widget-filters
        (widget-create 'telega-filter-list
                       :format "Filters: %v\n"
                       :value (car telega--filters)))
  (setq telega-root--widget-customs
        (apply #'widget-create
               'group :format "Customs:%v\n"
               (mapcar (lambda (custom)
                         `(telega-filter-custom :value ,custom))
                       telega-filter-custom-alist)))

  (widget-insert "\n")
  (setq telega-root--chats-marker (point-marker))
  (widget-setup)

  (telega-filters-reset)
  (add-hook 'kill-buffer-hook 'telega-root-killed nil t))

(defun telega-root-killed ()
  "Run when telega root buffer is killed.
Terminate telega-server and kill all chat buffers."
  (telega-server-kill))

;; widgets used in root buffer
(defvar telega-root--chats-marker nil
  "Marker where chats list starts.")
(defvar telega-root--widget-state nil
  "Widget for connection state.")
(defvar telega-root--widget-search nil
  "Widget for search field.")
(defvar telega-root--widget-filters nil
  "Widget to display active filters.")
(defvar telega-root--widget-customs nil
  "Widget with custom filters to apply.")

(defsubst telega-root--state (state)
  "Change current telega-server state to STATE."
  (with-current-buffer telega-root-buffer-name
    (widget-value-set telega-root--widget-state state)))

(defun telega-root--widget-hide (widget)
  (widget-apply widget :deactivate)
  (let ((inactive-overlay (widget-get widget :inactive)))
    (overlay-put inactive-overlay 'invisible t)))

(defun telega-root--widget-show (widget)
  (widget-apply widget :activate))

(defun telega-root--widget-customs-find (cus-name)
  "Find custom button by CUS-NAME."
  (cl-find cus-name (widget-get telega-root--widget-customs :children)
           :test (lambda (name child)
                   (string= name (car (widget-get child :value))))))

(defun telega-root--redisplay ()
  "Redisplay root buffer."
  (let ((in-chats-p (> (point) telega-root--chats-marker))
        (button (get-char-property (point) 'button)))
  (telega-root-saving-point
  ;; NOTE: If point is on any button, then try to keep point position
  ;; on that button after updating widgets
  (let ((butt (get-char-property (point) 'button)))

    (widget-value-set telega-root--widget-filters (car telega--filters))
    (widget-value-set telega-root--widget-customs telega-filter-custom-alist)

    ;; Try to keep point on same button
    (when (eq (car butt) 'telega-filter-custom)
      (let ((new-butt (telega-root--widget-customs-find
                       (car (widget-get butt :value)))))
        (when new-butt
          (goto-char (+ 2 (widget-get new-butt :from)))))))
  (telega-root--chats-redisplay))

(defmacro telega-root-saving-point
  ;; NOTE: If point is on any button, then try to keep point position
  ;; on that button after updating widgets
  (let ((butt (get-char-property (point) 'button)))


(defun telega-root--chat-new (chat)
  (telega-debug "TODO: `telega-root--chat-new'")
  (with-current-buffer telega-root-buffer-name
    (goto-char telega-root--chats-marker)
    ;; (let ((widget (widget-create 'telega-chat :value chat)))
    ;;   (unless (telega-filter--test chat (cons 'and telega--filters))
    ;;     (telega-root--widget-hide widget))
    ;;   (telega-root--chat-reorder chat widget))
    ))

(defun telega-root--chat-update (chat)
  "Something changed in CHAT, button needs to be updated."
  (telega-debug "TODO: `telega-root--chat-update'")
  )

(defun telega-root--chat-reorder (chat &optional widget)
  (telega-debug "TODO: `telega-root--chat-reorder'")
  )

(defun telega-root--chats-redisplay ()
  "Redisplay chats according to active filters."
  (message "TODO: `telega-root--chats-redisplay'")
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

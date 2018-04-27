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

(defvar telega-root-mode-hook nil
  "Hook run when telega root buffer is created.")

(defvar telega-root-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'telega-button-forward)
    (define-key map "p" 'telega-button-backward)
    (define-key map [?\t] 'telega-button-forward)
    (define-key map "\e\t" 'telega-button-backward)
    (define-key map [backtab] 'telega-button-backward)

    (set-keymap-parent map button-buffer-map)
    (define-key map (kbd "/") telega-filter-map)

    (define-key map (kbd "? w") 'telega-desribe-connected-websites)
    (define-key map (kbd "C-/") 'telega-filter-undo)
    (define-key map (kbd "C-_") 'telega-filter-undo)
    (define-key map (kbd "C-x C-/") 'telega-filter-redo)
    (define-key map (kbd "C-x C-_") 'telega-filter-redo)
    (define-key map (kbd "q") 'telega-kill)
    map)
  "The key map for telega root buffer.")

(define-derived-mode telega-root-mode nil "Telega-Root"
  "The mode for telega root buffer.
Keymap:
\\{telega-root-mode-map}"
  :group 'telega-root
;  (telega-root--setup-modeline)
  (telega-root--redisplay)
  (setq buffer-read-only t)
  (goto-char telega-root--chats-marker) ;default start point
  (add-hook 'kill-buffer-hook 'telega-root--killed nil t))

(defun telega-root--killed ()
  "Run when telega root buffer is killed.
Terminate telega-server and kill all chat buffers."
  (when telega-status--timer
    (cancel-timer telega-status--timer))
  (telega-chats--kill-em-all)
  (telega-server-kill))

(defsubst telega-root--buffer ()
  "Return telega root buffer."
  (get-buffer telega-root-buffer-name))

(defmacro with-telega-root-buffer (&rest body)
  "Execute BODY setting current buffer to root buffer.
Inhibits read-only flag."
  `(when (buffer-live-p (telega-root--buffer))
     (with-current-buffer telega-root-buffer-name
       (save-excursion
         (let ((inhibit-read-only t))
           ,@body)))))
(put 'with-telega-root-buffer 'lisp-indent-function 0)


;;; Connection Status
(defcustom telega-status-animate-interval 0.5
  "Status animation interval."
  :type 'number
  :group 'telega-root)

(defvar telega-status--timer nil
  "Timer used to animate status string.")

(define-button-type 'telega-status
  :format '("Status: " telega--status)
  'face nil)

(defun telega-status--animate ()
  "Animate dots at the end of the current connection status."
  (when (string-match "\\.+$" telega--status)
    (telega-status--set
     (concat (substring telega--status nil (match-beginning 0))
             (make-string (1+ (% (- (match-end 0) (match-beginning 0)) 3)) ?.))
     'raw)))

(defun telega-status--start-timer ()
  "Start telega status animation timer."
  (when telega-status--timer
    (cancel-timer telega-status--timer))
  (setq telega-status--timer
        (run-with-timer telega-status-animate-interval
                        telega-status-animate-interval
                        #'telega-status--animate)))

(defun telega-status--set (new-status &optional raw)
  "Set new status for the telegram connection.
If RAW is given then do not modify status for animation."
  (setq telega--status new-status)
  (unless raw
    (if (string-match "ing" telega--status)
        (progn
          (setq telega--status (concat telega--status "."))
          (telega-status--start-timer))
      (when telega-status--timer
        (cancel-timer telega-status--timer))))

  (with-telega-root-buffer
    (goto-char (point-min))
    (let ((button (telega-button-find 'telega-status)))
      (button-put button :value telega--status)
      (telega-button--redisplay button))))


;; root runtime variables
(defvar telega-root--chats-marker nil
  "Marker where chats list starts.")

(define-button-type 'telega-active-filters
  :format '("---" (prin1-to-string :min 70
                                   :align center :align-char ?-
                                   :max 70
                                   :elide t :elide-trail 30)
            "---")
  'face nil)

(defun telega-root--redisplay ()
  "Redisplay the root buffer."
  (let* ((cb (button-at (point)))   ; try to keep point on this button
         (cb-type (button-type cb))
         (cb-value (button-get cb :value)))
    (with-telega-root-buffer
      (erase-buffer)
      (telega-button-insert
        'telega-status :value telega--status
        'inactive :do-not-select-it-by-forward-backward-commands)
      (insert "\n\n")

      ;; Custom filters
      (dolist (custom telega-filters-custom)
        (let* ((help-echo (format "Filter (custom \"%s\") expands to: %s"
                                  (car custom) (cdr custom)))
               (button (telega-button-insert 'telega-filter
                         :value custom 'help-echo help-echo)))
          (telega-filter-button--set-inactivity-props button)
          (if (> (current-column) telega-filters-fill-column)
              (insert "\n")
            (insert "   "))))

      (unless (= (preceding-char) ?\n) (insert "\n"))
      (insert "\n")
      (telega-button-insert
        'telega-active-filters :value (car telega--filters)
        'inactive :do-not-select-it-by-forward-backward-commands)
      (insert "\n")
      (setq telega-root--chats-marker (point-marker))

      (dolist (chat telega--ordered-chats)
        (telega-root--chat-update chat 'filters-are-ok)))

    ;; Goto previously saved button
    (goto-char (1- (or (telega-button-find cb-type cb-value)
                       telega-root--chats-marker)))
    (telega-button-forward 1)))


(defun telega-root--chat-update (chat &optional inhibit-filters-redisplay)
  "Something changed in CHAT, button needs to be updated.
If there is no button for the CHAT, new button is created.
If INHIBIT-FILTERS-REDISPLAY specified then do not redisplay filters buttons."
  (telega-debug "IN: `telega-root--chat-update': %s"
                (telega-chat--title chat))
  (with-telega-root-buffer
    (goto-char (point-min))
    (let ((button (telega-button-find 'telega-chat chat)))
      (if button
          (progn
            (button-put button :value chat)
            (telega-button--redisplay button))

        ;; New chat
        (goto-char telega-root--chats-marker)
        (setq button (telega-button-insert 'telega-chat :value chat)))

      (button-put button 'invisible (not (telega-chat--visible-p chat)))

      ;; Update `telega--filtered-chats' according to chat update
      (setq telega--filtered-chats
            (delq chat telega--filtered-chats))
      (when (telega-filter--test chat (telega--filters-prepare))
        (setq telega--filtered-chats
              (push chat telega--filtered-chats)))

      ;; NOTE: Update might affect custom filters, refresh them too
      (unless inhibit-filters-redisplay
        (telega-root--filters-redisplay)))))

(defun telega-root--chat-reorder (chat)
  (telega-debug "TODO: `telega-root--chat-reorder'")

  ;; TODO: find chat before which this CHAT is placed and insert it
  ;; befor that chat's start point
  (with-telega-root-buffer
   (goto-char telega-root--chats-marker)
   (let* ((button (telega-button-find 'telega-chat chat))
          (chat-after (cadr (memq chat telega--ordered-chats)))
          (button-after (or (and chat-after (telega-button-find 'telega-chat chat-after))
                            (point-max))))
     (assert button nil "button no found for chat: %s" (telega-chat--title chat))
     (telega-button-move button button-after)))
  )

(defun telega-root--filters-redisplay ()
  "Redisplay custom filters buttons."
  (with-telega-root-buffer
    (goto-char (point-min))
    (telega-button-foreach 'telega-filter (button)
      (telega-button--redisplay button)
      (telega-filter-button--set-inactivity-props button))))

(defun telega-root--chats-redisplay ()
  "Redisplay chats according to active filters."
  (message "TODO: `telega-root--chats-redisplay'")
  )

(provide 'telega-root)

;;; telega-root.el ends here

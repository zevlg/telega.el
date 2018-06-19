;;; telega-root.el --- Root buffer for telega

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sat Apr 14 15:00:27 2018
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

(defvar telega-root-mode-hook nil
  "Hook run when telega root buffer is created.")

(defvar telega-root-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'telega-button-forward)
    (define-key map "p" 'telega-button-backward)
    (define-key map [?\t] 'telega-button-forward)
    (define-key map "\e\t" 'telega-button-backward)
    (define-key map [backtab] 'telega-button-backward)

    (define-key map (kbd "/") telega-filter-map)

    (define-key map (kbd "? w") 'telega-describe-connected-websites)
    (define-key map (kbd "? s") 'telega-describe-active-sessions)
    (define-key map (kbd "? t") 'telega-describe-terms-of-service)

    (define-key map (kbd "C-/") 'telega-filter-undo)
    (define-key map (kbd "C-_") 'telega-filter-undo)
    (define-key map (kbd "C-x C-/") 'telega-filter-redo)
    (define-key map (kbd "C-x C-_") 'telega-filter-redo)

    (define-key map (kbd "q") 'telega-kill)
    (define-key map (kbd "c") 'telega-chat-with)
    map)
  "The key map for telega root buffer.")

(define-derived-mode telega-root-mode nil "Telega-Root"
  "The mode for telega root buffer.
Keymap:
\\{telega-root-mode-map}"
  :group 'telega-root
  (setq mode-line-buffer-identification
        (telega-root--modeline-buffer-identification))

  ;; Reseting filters will trigger `telega-root--redisplay'
  (telega-filters-reset)
  (setq buffer-read-only t)
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
       (let ((inhibit-read-only t))
         ,@body))))
(put 'with-telega-root-buffer 'lisp-indent-function 0)


;;; Connection Status
(defcustom telega-status-animate-interval 0.5
  "Status animation interval."
  :type 'number
  :group 'telega-root)

(defvar telega-status--timer nil
  "Timer used to animate status string.")

(define-button-type 'telega-status
  :supertype 'telega
  :format '("Status: " identity))

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
  (telega-debug "Status: %s --> %s" telega--status new-status)

  (setq telega--status new-status)
  (unless raw
    (if (string-match "ing" telega--status)
        (progn
          (setq telega--status (concat telega--status "."))
          (telega-status--start-timer))
      (when telega-status--timer
        (cancel-timer telega-status--timer))))

  (with-telega-root-buffer
    (save-excursion
      (goto-char (point-min))
      (let ((button (telega-button-find 'telega-status)))
        (assert button nil "Telega status button is gone")
        (button-put button :value telega--status)
        (telega-button--redisplay button)))))

(defun telega-root--redisplay ()
  "Redisplay the root buffer."
  (let* ((cb (button-at (point)))   ; try to keep point on this button
         (cb-type (and cb (button-type cb)))
         (cb-value (and cb (button-get cb :value))))
    (with-telega-root-buffer
      (erase-buffer)
      (telega-button-insert
        'telega-status :value telega--status
        'inactive :do-not-select-it-by-forward-backward-commands)
      (insert "\n\n")

      ;; Custom filters
      (dolist (custom telega-filters-custom)
        (telega-filter-button--set-inactivity-props
         (telega-button-insert 'telega-filter :value custom))
        (if (> (current-column) telega-filters-fill-column)
            (insert "\n")
          (insert "   ")))

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
    (goto-char (point-min))
    (goto-char (1- (or (and cb-type (telega-button-find cb-type cb-value))
                       (point-max))))
    (condition-case nil
        (telega-button-forward 1)
      (error (forward-char 1)))))


(defun telega-root--chat-button (chat)
  "Return button corresponding to CHAT."
  (with-telega-root-buffer
    (goto-char (point-min))
    (telega-button-find 'telega-chat chat)))

(defun telega-root--chat-update (chat &optional inhibit-filters-redisplay)
  "Something changed in CHAT, button needs to be updated.
If there is no button for the CHAT, new button is created.
If INHIBIT-FILTERS-REDISPLAY specified then do not redisplay filters buttons."
  (telega-debug "IN: `telega-root--chat-update': %s" (telega-chat--title chat))

  ;; Update `telega--filtered-chats' according to chat update It
  ;; might affect visibility, chat button formatting itself and
  ;; custom filters
  (setq telega--filtered-chats
        (delq chat telega--filtered-chats))
  (when (telega-filter-chats nil (list chat))
    (setq telega--filtered-chats
          (push chat telega--filtered-chats)))

  (with-telega-root-buffer
    (save-excursion
      (let ((button (telega-root--chat-button chat))
            (visible-p (memq chat telega--filtered-chats)))
        (if button
            (progn
              (button-put button :value chat)
              (when visible-p
                (telega-button--redisplay button)))

          ;; New chat
          (goto-char (point-max))
          (setq button (telega-button-insert 'telega-chat :value chat)))

        (button-put button 'invisible (not visible-p))

        ;; NOTE: Update might affect custom filters, refresh them too
        (unless inhibit-filters-redisplay
          (telega-root--filters-redisplay))))))

(defun telega-root--chat-reorder (chat)
  "Move CHAT to correct place according to its order."
  (with-telega-root-buffer
    (save-excursion
      (goto-char (point-min))
      (let* ((button (telega-root--chat-button chat))
             (chat-after (cadr (memq chat telega--ordered-chats)))
             (button-after (or (telega-root--chat-button chat-after)
                               (point-max))))
        (assert button nil "button no found for chat: %s" (telega-chat--title chat))
        (telega-button-move button button-after)))))

(defun telega-root--user-update (user)
  "Something changed in USER, private chat might need to be updated."
  (let ((chat (telega-chat--get (plist-get user :id) 'offline)))
    (when chat
      (telega-root--chat-update chat))))

(defun telega-root--filters-redisplay ()
  "Redisplay custom filters buttons."
  (with-telega-root-buffer
    (save-excursion
      (goto-char (point-min))
      (telega-button-foreach 'telega-filter (button)
        (telega-filter-button--set-inactivity-props
         (telega-button--redisplay button))))))

(defun telega-root--chats-redisplay ()
  "Redisplay chats according to active filters."
  (message "TODO: `telega-root--chats-redisplay'")
  )

(defun telega-root--modeline-buffer-identification ()
  "Return `mode-line-buffer-identification' for the root buffer."
  (let ((title "%12b")
        (unread_unmuted
         (unless (zerop telega--unread-unmuted-count)
           (propertize (format " %d" telega--unread-unmuted-count)
                       'face 'telega-unread-unmuted-modeline
                       'local-map
                       '(keymap
                         (mode-line
                          keymap (mouse-1 . telega-filter-unread-unmuted)))
                       'mouse-face 'mode-line-highlight
                       'help-echo
                       "Click to filter chats with unread/unmuted messages"))))
    (when (display-graphic-p)
      (let ((logo-img (or telega--logo-image-cache
                          (setq telega--logo-image-cache
                                (find-image
                                 '((:type xpm :file "etc/telegram-logo.xpm"
                                          :ascent center)))))))
        (setq title (concat "  " title))
        (add-text-properties 0 1 (list 'display logo-img) title)))

    (list title unread_unmuted)))

(defun telega--on-updateUnreadMessageCount (event)
  "Number of unread messages has changed."
  (setq telega--unread-count (plist-get event :unread_count)
        telega--unread-unmuted-count (plist-get event :unread_unmuted_count))

  (with-telega-root-buffer
    (setq mode-line-buffer-identification
          (telega-root--modeline-buffer-identification))
    (force-mode-line-update)))

(provide 'telega-root)

;;; telega-root.el ends here

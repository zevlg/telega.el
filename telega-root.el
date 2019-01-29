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
(require 'ewoc)
(require 'telega-core)
(require 'telega-server)
(require 'telega-filter)
(require 'telega-voip)
(require 'telega-ins)
(require 'telega-util)
(require 'telega-customize)

(declare-function tracking-mode "tracking" (&optional arg))

(defvar telega-root--ewoc nil)
(defvar telega-root-search--ewoc nil
  "Ewoc for global chats searched.")

(defvar telega-status--timer nil
  "Timer used to animate status string.")

(defvar telega-root-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'telega-button-forward)
    (define-key map "p" 'telega-button-backward)
    (define-key map [?\t] 'telega-button-forward)
    (define-key map "\e\t" 'telega-button-backward)
    (define-key map [backtab] 'telega-button-backward)

    (define-key map (kbd "/") telega-filter-map)
    (define-key map (kbd "C-/") 'telega-filter-undo)
    (define-key map (kbd "C-_") 'telega-filter-undo)
    (define-key map (kbd "C-x C-/") 'telega-filter-redo)
    (define-key map (kbd "C-x C-_") 'telega-filter-redo)

    (define-key map (kbd "? w") 'telega-describe-connected-websites)
    (define-key map (kbd "? s") 'telega-describe-active-sessions)
    (define-key map (kbd "? n") 'telega-describe-network)
    (define-key map (kbd "? p") 'telega-describe-privacy-settings)

    (define-key map (kbd "j") 'telega-chat-join-by-link)
    ;; commands to all currently filtered chats
    (define-key map (kbd "D") 'telega-chats-filtered-delete)
    (define-key map (kbd "R") 'telega-chats-filtered-toggle-read)

    ;; Calls bindings
    (define-key map (kbd "c a") 'telega-voip-accept)
    (define-key map (kbd "c d") 'telega-voip-discard)
    (define-key map (kbd "c b") 'telega-voip-buffer-show)
    (define-key map (kbd "c l") 'telega-voip-list-calls)

    (define-key map (kbd "q") 'telega-kill)
    (define-key map (kbd "m") 'telega-chat-with)
    map)
  "The key map for telega root buffer.")



(define-derived-mode telega-root-mode nil "Telega-Root"
  "The mode for telega root buffer.
Keymap:
\\{telega-root-mode-map}"
  :group 'telega-root
  (setq mode-line-buffer-identification
        (telega-root--modeline-buffer-identification))

  (telega--filters-reset telega-filter-default)

  (setq buffer-read-only nil)
  (erase-buffer)

  ;; Status goes first
  (telega-button--insert
   'telega-status (cons telega--status telega--status-aux))

  ;; delim
  (insert "\n")
  (unless telega-root-compact-view
    (insert "\n"))

  ;; Custom filters
  (telega-filters--create)

  ;; Put invisible delimiter, so `telega-root-search--ewoc' can be
  ;; totally empty and its marker won't move by inserts made by
  ;; `telega-root--ewoc'
  (goto-char (point-max))
  (insert " ")
  (insert (propertize "\n" 'invisible t))
  ;; NOTE: we are using ewoc with `nosep' so newline is not inserted
  ;; for non-visible chat buttons
  (setq telega-root-search--ewoc
        (ewoc-create 'telega-chat-search--pp "" "" t))

  (goto-char (point-max))
  (insert "\n")
  (setq telega-root--ewoc
        (ewoc-create 'telega-chat-visible--pp nil nil t))
  (dolist (chat telega--ordered-chats)
    (ewoc-enter-last telega-root--ewoc chat))

  (setq buffer-read-only t)
  (add-hook 'kill-buffer-hook 'telega-root--killed nil t)

  (when telega-use-tracking
    (tracking-mode 1)))

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
  (declare (indent 0))
  `(when (buffer-live-p (telega-root--buffer))
     (with-current-buffer telega-root-buffer-name
       (let ((inhibit-read-only t))
         ,@body))))


;;; Auth/Connection Status
(define-button-type 'telega-status
  :supertype 'telega
  :inserter 'telega-ins--status
  'inactive t)

(defun telega-ins--status (status)
  "Default inserter for the `telega-status' button.
STATUS is cons with connection status as car and aux status as cdr."
  (let ((conn-status (car status))
        (aux-status (cdr status)))
    (telega-ins "Status: " conn-status)
    (unless (string-empty-p aux-status)
      (if (< (current-column) 28)
          (move-to-column 30 t)
        (insert "  "))
      (telega-ins aux-status))))

(defmacro telega-status--animate-dots (status)
  "Animate status dots at the end of the STATUS string.
Return `nil' if there is nothing to animate and new string otherwise."
  `(when (string-match "\\.+$" ,status)
     (concat (substring ,status nil (match-beginning 0))
             (make-string
              (1+ (% (- (match-end 0) (match-beginning 0)) 3)) ?.))))

(defun telega-status--animate ()
  "Animate dots at the end of the current connection or/and aux status."
  (let ((conn-status (telega-status--animate-dots telega--status))
        (aux-status (telega-status--animate-dots telega--status-aux)))
    (when (or conn-status aux-status)
      (telega-status--set conn-status aux-status 'raw))))

(defun telega-status--start-timer ()
  "Start telega status animation timer."
  (when telega-status--timer
    (cancel-timer telega-status--timer))
  (setq telega-status--timer
        (run-with-timer telega-status-animate-interval
                        telega-status-animate-interval
                        #'telega-status--animate)))

(defun telega-status--set (conn-status &optional aux-status raw)
  "Set new status for the telegram connection to CONN-STATUS.
aux status is set to AUX-STATUS.  Both statuses can be `nil' to
unchange their current value.
If RAW is given then do not modify statuses for animation."
  (let ((old-status (cons telega--status telega--status-aux)))
    (when conn-status
      (setq telega--status conn-status))
    (when aux-status
      (setq telega--status-aux aux-status))

    (unless raw
      (telega-debug "Status: %s --> %s"
                    old-status (cons telega--status telega--status-aux))

      (cond ((string-match "ing" telega--status)
             (setq telega--status (concat telega--status "."))
             (telega-status--start-timer))
            ((string-match "\\.+$" telega--status-aux)
             (telega-status--start-timer))
            (telega-status--timer
             (cancel-timer telega-status--timer))))

  (with-telega-root-buffer
    (telega-save-cursor
      (let ((button (button-at (point-min))))
        (cl-assert (eq (button-type button) 'telega-status)
                   nil "Telega status button is gone")
        (telega-button--update-value
         button (cons telega--status telega--status-aux)))))
  ))


(defun telega-root--redisplay ()
  "Redisplay root's buffer contents."
  (telega-filters--redisplay)
  (with-telega-root-buffer
    (telega-save-cursor
      (let ((global-search-chats
             (telega-filter-chats
              nil (remq 'empty telega--search-public-chats))))

        ;; If there any globally searched public chats matching
        ;; current filters, fill the `telega-root-search--ewoc' with
        ;; them and install the footer as separator
        (telega-ewoc--set-header telega-root-search--ewoc "")
        (telega-ewoc--set-footer telega-root-search--ewoc "")
        (telega-ewoc--clean telega-root-search--ewoc)
        (when global-search-chats
          (dolist (gschat global-search-chats)
            (ewoc-enter-last telega-root-search--ewoc gschat))
          (telega-ewoc--set-header telega-root-search--ewoc
                                   "\n.---------[GLOBAL SEARCH\n")
          (telega-ewoc--set-footer telega-root-search--ewoc
                                   "`---------")))

      (ewoc-refresh telega-root--ewoc))))

(defun telega-root--chat-update (chat &optional for-reorder)
  "Something changed in CHAT, button needs to be updated.
If FOR-REORDER is non-nil, then CHAT's node is ok, just update filters."
  (telega-debug "IN: `telega-root--chat-update': %s" (telega-chat-title chat))

  ;; Update `telega--filtered-chats' according to chat update. It
  ;; might affect visibility, chat button formatting itself and custom
  ;; filters
  (setq telega--filtered-chats
        (delq chat telega--filtered-chats))
  (when (telega-filter-chats nil (list chat))
    (setq telega--filtered-chats
          (push chat telega--filtered-chats)))

  (unless for-reorder
    (with-telega-root-buffer
      (telega-save-cursor
        (let ((enode (telega-ewoc--find-node-by-data telega-root--ewoc chat)))
          (cl-assert enode nil "Ewoc node not found for chat:%s"
                     (telega-chat-title chat))

          (setf (ewoc--node-data enode) chat)
          (ewoc-invalidate telega-root--ewoc enode)))))

  ;; NOTE: Update might affect custom filters, refresh them too
  (telega-filters--redisplay))

(defun telega-root--chat-reorder (chat &optional new-chat-p)
  "Move CHAT to correct place according to its order.
NEW-CHAT-P is used for optimization, to omit ewoc's node search."
  (with-telega-root-buffer
    (let* ((node (unless new-chat-p
                   (telega-ewoc--find-node-by-data telega-root--ewoc chat)))
           (chat-after (cadr (memq chat telega--ordered-chats)))
           (node-after (telega-ewoc--find-node-by-data
                        telega-root--ewoc chat-after)))
      (when node
        (ewoc-delete telega-root--ewoc node))
      (if node-after
          (ewoc-enter-before telega-root--ewoc node-after chat)
        (ewoc-enter-last telega-root--ewoc chat)))))

(defun telega-root--chat-new (chat)
  "New CHAT has been created. Display it in root's ewoc."
  (telega-root--chat-reorder chat 'new-chat)

  ;; In case of initial chats load, redisplay custom filters
  ;; on every 50 chats loaded
  (when (and telega-filters--inhibit-redisplay
             (zerop (% (length telega--ordered-chats) 50)))
    (let ((telega-filters--inhibit-redisplay nil))
      (telega-filters--redisplay))))

(defun telega-root--modeline-buffer-identification ()
  "Return `mode-line-buffer-identification' for the root buffer."
  (let* ((title "%12b")
         (uu-chats-count
          (or (plist-get telega--unread-chat-count :unread_unmuted_count) 0))
         (unread-unmuted
          (unless (zerop uu-chats-count)
            (propertize (format " %d" uu-chats-count)
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

    (list title unread-unmuted)))

(defun telega--on-updateUnreadMessageCount (event)
  "Number of unread messages has changed."
  (setq telega--unread-message-count (cddr event))

  (with-telega-root-buffer
    (setq mode-line-buffer-identification
          (telega-root--modeline-buffer-identification))
    (force-mode-line-update)))

(defun telega--on-updateUnreadChatCount (event)
  "Number of unread/unmuted chats has been changed."
  (setq telega--unread-chat-count (cddr event))

  (with-telega-root-buffer
    (setq mode-line-buffer-identification
          (telega-root--modeline-buffer-identification))
    (force-mode-line-update)))

(provide 'telega-root)

;;; telega-root.el ends here

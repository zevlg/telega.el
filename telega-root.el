;;; telega-root.el --- Root buffer for telega  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2019 by Zajcev Evgeny.

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

;;; ellit-org: commentary
;; 
;; rootbuf is the heart of the =telega=.  Switch to rootbuf with
;; {{{kbd(M-x telega RET)}}} or use
;; {{{where-is(telega,telega-prefix-map)}}} binding from the
;; [[#telega-prefix-map][Telega prefix map]].
;; 
;; *TODO*: describe parts of the rootbuf

;;; Code:
(require 'ewoc)
(require 'telega-core)
(require 'telega-util)
(require 'telega-server)
(require 'telega-filter)
(require 'telega-sort)
(require 'telega-info)
(require 'telega-voip)
(require 'telega-ins)
(require 'telega-chat)
(require 'telega-customize)

(declare-function tracking-mode "tracking" (&optional arg))

(declare-function telega-chat--update "telega-tdlib-events" (chat &rest events))
(declare-function telega-chats-dirty--update "telega-tdlib-events")
(declare-function telega-chat--mark-dirty "telega-tdlib-events" (chat &rest events))

(declare-function telega-account-current "telega")
(declare-function telega-account-switch "telega" (account))


(defvar telega-root--view nil
  "Current root view spec.
First element is symbol denoting the view.
Second arg is root view header to show.
Rest elements are ewoc specs.")
(defvar telega-root--view-filter nil
  "Additional Chat Filter applied implicitely for the root view.")
(defvar telega-root-view--header-marker nil
  "Marker used for root view header.")
(defvar telega-root-view--ewocs-marker nil
  "Ewocs in `telega-root-view--ewocs-alist' starts here.")
(defvar telega-root-view--ewocs-alist nil
  "Named ewocs alist in rootbuf.")

(defvar telega-status--timer nil
  "Timer used to animate status string.")
(defvar telega-loading--timer nil
  "Timer used to animate Loading.. for root view ewocs.")
(defvar telega-online--timer nil
  "Timer used to change online status.")
(defvar telega-idle--timer nil
  "Runs when Emacs gets idle.")

(defvar telega-root-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'ignore)
    (define-key map "n" 'telega-button-forward)
    (define-key map "p" 'telega-button-backward)
    (define-key map [?\t] 'telega-button-forward)
    (define-key map "\e\t" 'telega-button-backward)
    (define-key map [backtab] 'telega-button-backward)

    (define-key map (kbd "\\") telega-sort-map)

    (define-key map (kbd "/") telega-filter-map)
    (define-key map (kbd "C-/") 'telega-filter-undo)
    (define-key map (kbd "C-_") 'telega-filter-undo)
    (define-key map (kbd "C-x C-/") 'telega-filter-redo)
    (define-key map (kbd "C-x C-_") 'telega-filter-redo)

    ;; Getting help
    (define-key map (kbd "? w") 'telega-describe-connected-websites)
    (define-key map (kbd "? s") 'telega-describe-active-sessions)
    (define-key map (kbd "? n") 'telega-describe-network)
    (define-key map (kbd "? y") 'telega-describe-notifications)
    (define-key map (kbd "? N") 'telega-describe-notifications)
    (define-key map (kbd "? p") 'telega-describe-privacy-settings)

    (define-key map (kbd "J") 'telega-chat-join-by-link)
    (define-key map (kbd "N") 'telega-chat-create)
    ;; Commands to all currently filtered chats

    ;; NOTE: Deleting all chats is very-very-very dangerous, so
    ;; disabled, use M-x telega-chats-filtered-delete RET if you know
    ;; what you are doing
    ;; (define-key map (kbd "D") 'telega-chats-filtered-delete)
    (define-key map (kbd "K") 'telega-chats-filtered-kill-chatbuf)
    (define-key map (kbd "R") 'telega-chats-filtered-toggle-read)

    ;;; ellit-org: rootbuf-folder-bindings
    ;; - {{{where-is(telega-folder-create,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-folder-create, 2)}}}
    (define-key map (kbd "F +") 'telega-folder-create)
    ;;; ellit-org: rootbuf-folder-bindings
    ;; - {{{where-is(telega-folder-delete,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-folder-delete, 2)}}}
    (define-key map (kbd "F -") 'telega-folder-delete)
    ;;; ellit-org: rootbuf-folder-bindings
    ;; - {{{where-is(telega-folders-reorder,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-folders-reorder, 2)}}}
    (define-key map (kbd "F =") 'telega-folders-reorder)
    ;;; ellit-org: rootbuf-folder-bindings
    ;; - {{{where-is(telega-folder-rename,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-folder-rename, 2)}}}
    (define-key map (kbd "F R") 'telega-folder-rename)
    ;;; ellit-org: rootbuf-folder-bindings
    ;; - {{{where-is(telega-chat-add-to-folder,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-chat-add-to-folder, 2)}}}
    (define-key map (kbd "F a") 'telega-chat-add-to-folder)
    ;;; ellit-org: rootbuf-folder-bindings
    ;; - {{{where-is(telega-chat-remove-from-folder,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-chat-remove-from-folder, 2)}}}
    (define-key map (kbd "F d") 'telega-chat-remove-from-folder)

    ;; Calls bindings
    (define-key map (kbd "c a") 'telega-voip-accept)
    (define-key map (kbd "c d") 'telega-voip-discard)
    (define-key map (kbd "c b") 'telega-voip-buffer-show)
    (define-key map (kbd "c l") 'telega-voip-list-calls)

    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "Q") 'telega-kill)

    ;;; ellit-org: rootbuf-fastnav-bindings
    ;; - {{{where-is(telega-root-next-unread,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-root-next-unread, 2)}}}
    (define-key map (kbd "M-g u") 'telega-root-next-unread)

    ;;; ellit-org: rootbuf-fastnav-bindings
    ;; - {{{where-is(telega-root-next-important,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-root-next-important, 2)}}}
    ;;
    ;;   Important message is a message matching "Important" custom
    ;;   [[#chat-filters][chat filter]].  If there is no "Important"
    ;;   custom chat filter, then ~(or mention (and unread unmuted))~
    ;;   chat filter is used.
    (define-key map (kbd "M-g i") 'telega-root-next-important)

    ;;; ellit-org: rootbuf-fastnav-bindings
    ;; - {{{where-is(telega-root-next-mention,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-root-next-mention, 2)}}}
    (define-key map (kbd "M-g m") 'telega-root-next-mention)
    (define-key map (kbd "M-g @") 'telega-root-next-mention)

    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-search,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-search, 2)}}}
    (define-key map (kbd "s") 'telega-view-search)
    (define-key map (kbd "v s") 'telega-view-search)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-nearby,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-nearby, 2)}}}
    (define-key map (kbd "v n") 'telega-view-nearby)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-reset,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-reset, 2)}}}
    (define-key map (kbd "v v") 'telega-view-reset)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-compact,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-compact, 2)}}}
    (define-key map (kbd "v 0") 'telega-view-compact)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-one-line,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-one-line, 2)}}}
    (define-key map (kbd "v 1") 'telega-view-one-line)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-two-lines,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-two-lines, 2)}}}
    (define-key map (kbd "v 2") 'telega-view-two-lines)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-topics,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-topics, 2)}}}
    ;; 
    ;;   Customizable options:
    ;;   - {{{user-option(telega-root-view-topics, 4)}}}
    ;;   - {{{user-option(telega-root-view-topics-folders, 4)}}}
    ;;   - {{{user-option(telega-root-view-topics-other-chats, 4)}}}
    (define-key map (kbd "v t") 'telega-view-topics)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-top,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-top, 2)}}}
    ;; 
    ;;   Customizable options:
    ;;   - {{{user-option(telega-root-view-top-categories, 4)}}}
    (define-key map (kbd "v T") 'telega-view-top)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-settings,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-settings, 2)}}}
    (define-key map (kbd "v S") 'telega-view-settings)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-contacts,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-contacts, 2)}}}
    (define-key map (kbd "v c") 'telega-view-contacts)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-calls,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-calls, 2)}}}
    (define-key map (kbd "v C") 'telega-view-calls)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-last-messages,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-last-messages, 2)}}}
    (define-key map (kbd "v l") 'telega-view-last-messages)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-folders,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-folders, 2)}}}
    (define-key map (kbd "v f") 'telega-view-folders)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-pinned-messages,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-pinned-messages, 2)}}}
    (define-key map (kbd "v ^") 'telega-view-pinned-messages)
    (define-key map (kbd "v p") 'telega-view-pinned-messages)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-deleted-chats,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-deleted-chats, 2)}}}
    (define-key map (kbd "v d") 'telega-view-deleted-chats)

    map)
  "The key map for telega root buffer.")

(define-derived-mode telega-root-mode nil "◁Root"
  "The mode for telega root buffer.

Chat bindings (cursor on chat):
\\{telega-chat-button-map}
Global root bindings:
\\{telega-root-mode-map}"
  :group 'telega-root
  (telega-runtime-setup)

  (setq-local nobreak-char-display nil)
  ;; NOTE: make `telega-root-keep-cursor' working as expected
  (setq-local switch-to-buffer-preserve-window-point nil)

  (setq buffer-read-only nil)
  (erase-buffer)

  ;; Status goes first
  (telega-button--insert
   'telega-status (cons telega--status telega--status-aux))

  ;; delim
  (insert "\n")

  ;; Custom filters
  (telega-filters--reset telega-filter-default)
  (setq telega-tdlib--chat-list (telega-filter-active-tdlib-chat-list))
  (telega-filters--create)

  (save-excursion
    ;; Meta Ewoc for root view ewocs
    (goto-char (point-max))
    (insert "\n")
    (setq telega-root-view--header-marker (point-marker))
    (setq telega-root-view--ewocs-marker (point-marker)))

  ;; Apply default view of the rootbuf
  (setq telega-root--view nil)
  (telega-view-reset)

  (setq buffer-read-only t)
  (add-hook 'kill-buffer-hook 'telega-root--killed nil t)

  (cursor-sensor-mode 1)
  (when telega-use-tracking-for
    (tracking-mode 1)))


(defun telega-root--killed ()
  "Run when telega root buffer is killed.
Terminate telega-server and kill all chat buffers."
  (when telega-status--timer
    (cancel-timer telega-status--timer))
  (when telega-loading--timer
    (cancel-timer telega-loading--timer))
  (when telega-online--timer
    (cancel-timer telega-online--timer))
  (telega-chats--kill-em-all)
  (telega-server-kill)

  (telega-runtime-teardown))

(defun telega-root--buffer ()
  "Return telega root buffer."
  (get-buffer telega-root-buffer-name))


;; Utility functions for root view ewocs
(defmacro with-telega-root-view-ewoc (name view-ewoc-sym &rest body)
  "Execute BODY binding VIEW-EWOC-SYM to root view ewoc named by NAME."
  (declare (indent 2))
  `(when-let ((,view-ewoc-sym
               (cdr (assoc ,name telega-root-view--ewocs-alist))))
     (with-telega-root-buffer
       ,@body)))

(defun telega-root-view--ewoc-header (header)
  "Format HEADER for root view ewoc."
  (when header
    (telega-ins--as-string
     (telega-ins--with-attrs
         (list :min telega-root-fill-column
               :max telega-root-fill-column
               :align 'left
               :face 'telega-root-heading)
       (telega-ins header))
     (telega-ins "\n"))))

(defun telega-root-view--ewoc-create (ewoc-spec)
  "Pretty printer for ewoc, specified by EWOC-SPEC.
EWOC-SPEC is plist with keyword elements:
`:name', `:pretty-printer', `:header', `:footer', `:items'
`:on-chat-update', `:on-user-update', `:on-message-update',
`:loading'."
  (cl-assert (stringp (plist-get ewoc-spec :name)))
  (let ((ewoc (ewoc-create (telega-ewoc--gen-pp
                            (plist-get ewoc-spec :pretty-printer))
                           (telega-root-view--ewoc-header
                            (plist-get ewoc-spec :header))
                           (or (plist-get ewoc-spec :footer)
                               (when (plist-get ewoc-spec :loading)
                                 "Loading..\n"))
                           'no-sep)))
    (setq telega-root-view--ewocs-alist
          (append telega-root-view--ewocs-alist
                  (list (cons (plist-get ewoc-spec :name) ewoc))))
    (dolist (item (plist-get ewoc-spec :items))
      (ewoc-enter-last ewoc item))))

(defun telega-root-view--ewoc-spec (ewoc-name)
  "Return ewoc spec for ewoc with name EWOC-NAME."
  (cl-find ewoc-name (nthcdr 2 telega-root--view)
           :key (telega--tl-prop :name) :test #'equal))

(defun telega-root-view--ewoc-sorter (ewoc-name &optional default)
  "Return sorter for ewoc named EWOC-NAME."
  (or (plist-get (telega-root-view--ewoc-spec ewoc-name) :sorter)
      default
      #'ignore))

(defun telega-root-view--ewoc-loading-start (ewoc-name loading)
  "Start loading in the root view ewoc named EWOC-NAME.
LOADING is extra value from corresponding TDLib request."
  (declare (indent 1))
  (let ((ewoc-spec (telega-root-view--ewoc-spec ewoc-name)))
    (cl-assert (not (plist-get ewoc-spec :loading)))
    (plist-put ewoc-spec :loading loading)
    (with-telega-root-view-ewoc (plist-get ewoc-spec :name) ewoc
      (telega-save-cursor
        (telega-ewoc--set-footer ewoc "Loading..\n")))
    (telega-loading--timer-start)))

(defun telega-root-view--ewoc-loading-done (ewoc-name &optional items)
  "Loading is done in root view's ewoc named by EWOC-NAME.
ITEMS is a list of loaded items to be added into ewoc."
  (when-let* ((ewoc-spec (telega-root-view--ewoc-spec ewoc-name))
              (loading (plist-get ewoc-spec :loading)))
    (plist-put ewoc-spec :loading nil)
    (with-telega-root-view-ewoc ewoc-name ewoc
      (telega-save-cursor
        (telega-ewoc--set-footer ewoc "")

        (dolist (item (sort items (telega-root-view--ewoc-sorter ewoc-name)))
          (ewoc-enter-last ewoc item)))

      (run-hooks 'telega-root-update-hook))))

(defun telega-root--keep-cursor-at-chat (chat)
  "Keep cursor position at CHAT.
Keep cursor position only if CHAT is visible."
  (when (telega-filter-chats (list chat)) ;visible-p
    (with-telega-root-view-ewoc "root" root-ewoc
      (when-let ((node (telega-ewoc--find-by-data root-ewoc chat)))
        (goto-char (ewoc-location node))
        (dolist (win (get-buffer-window-list))
          (set-window-point win (point)))
        (run-hooks 'telega-root-update-hook)))))


;;; Pretty Printers for root view ewocs
(defun telega-root--chat-pp (chat &optional custom-inserter custom-action)
  "Pretty printer for any CHAT button."
  (telega-button--insert 'telega-chat chat
    :inserter (or custom-inserter
                  telega-inserter-for-chat-button)
    :action (or custom-action #'telega-chat--pop-to-buffer))
  (unless (= (char-before) ?\n)
    (insert "\n")))

(defun telega-root--chat-known-pp (chat &optional custom-inserter custom-action)
  "Pretty printer for known CHAT button."
  ;; Insert only visible chat buttons
  ;; See https://github.com/zevlg/telega.el/issues/3
  (let ((visible-p (telega-filter-chats (list chat))))
    (when visible-p
      (telega-root--chat-pp chat custom-inserter custom-action))))

(defun telega-root--global-chat-pp (chat &optional custom-inserter)
  "Display CHAT found in global public chats search."
  (let* ((telega-chat-button-width (+ telega-chat-button-width
                                      (/ telega-chat-button-width 2)))
         (telega-filters--inhibit-list '(chat-list folder main archive)))
    (telega-root--chat-known-pp chat custom-inserter)))

(defun telega-root--nearby-chat-known-pp (chat &optional custom-inserter)
  "Pretty printers for known CHAT, that is in nearby list."
  (let ((visible-p (telega-chat-nearby-find (plist-get chat :id))))
    (when visible-p
      (telega-root--chat-known-pp
       chat (or custom-inserter #'telega-ins--chat-nearby-2lines)))))

(defun telega-root--nearby-global-chat-pp (chat &optional custom-inserter)
  "Pretty printer for some, maybe unknown, nearby CHAT."
  (telega-root--global-chat-pp
   chat (or custom-inserter #'telega-ins--chat-nearby-2lines)))

(defun telega-root--contact-pp (contact-user &optional custom-inserter)
  "Pretty printer for CONTACT-USER button shown in root buffer.
CONTACT is some user you have exchanged contacts with."
  ;; NOTE: If CONTACT-USER has corresponding chat, then show contact
  ;; only if it matches active chat filter
  ;; If CONTACT-USER has no chat, then always show it
  (let* ((user-chat
          (telega-chat-get (plist-get contact-user :id) 'offline))
         (visible-p (or (not user-chat)
                        (let ((telega-filters--inhibit-list
                               '(chat-list folder main archive)))
                          (telega-filter-chats (list user-chat))))))
    (when visible-p
      (telega-button--insert 'telega-user contact-user
        :inserter (or custom-inserter
                      telega-inserter-for-root-contact-button)
        :action #'telega-user-chat-with)
      (telega-ins "\n"))))

(defun telega-root--nearby-contact-pp (contact-user &optional custom-inserter)
  "Pretty printer for CONTACT-USER nearby."
  (let* ((user-chat
          (telega-chat-get (plist-get contact-user :id) 'offline))
         (visible-p (when user-chat
                      (telega-chat-nearby-find (plist-get user-chat :id)))))
    (when visible-p
      (telega-root--contact-pp contact-user custom-inserter))))

(defun telega-root--chat-goto-last-message (chat)
  "Goto last message in the CHAT."
  (let ((last-msg (plist-get chat :last_message)))
    (unless last-msg
      (user-error "No last message in chat: %s" (telega-chat-title chat)))
    (telega-msg-goto-highlight last-msg)))

(defun telega-root--chat-last-message-pp (chat)
  "Pretty printer for CHAT's last message."
  (let ((visible-p (plist-get chat :last_message)))
    (when visible-p
      (let ((telega-chat-button-width (+ telega-chat-button-width
                                         (/ telega-chat-button-width 2))))
        (telega-root--chat-known-pp
         chat
         #'telega-ins--chat-last-message
         #'telega-root--chat-goto-last-message)))))

(defun telega-root--message-pp (msg &optional custom-inserter)
  "Pretty printer for MSG button shown in root buffer."
  (declare (indent 1))
  (let ((visible-p (telega-filter-chats (list (telega-msg-chat msg)))))
    (when visible-p
      (telega-button--insert 'telega-msg msg
        :inserter (or custom-inserter #'telega-ins--root-msg)
        :action #'telega-msg-goto-highlight)
      (telega-ins "\n")
      )))

(defun telega-root--message-call-pp (msg)
  "Pretty printer for call MSG button shown in root buffer."
  (telega-root--message-pp msg #'telega-ins--root-msg-call))


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
    (telega-ins "Status")
    (when-let (account (telega-account-current))
      (telega-ins " (")
      (telega-ins--button (car account)
        'face 'bold
        'action (lambda (_ignored)
                  (call-interactively #'telega-account-switch))
        'help "Switch to another account")
      (telega-ins ")"))
    (telega-ins ": " conn-status)
    (unless (string-empty-p aux-status)
      (if (< (current-column) 28)
          (telega-ins (make-string (- 30 (current-column)) ?\s))
        (telega-ins "  "))
      (telega-ins aux-status))))

(defun telega-status--animate ()
  "Animate dots at the end of the current connection or/and aux status."
  (let ((conn-status (telega--animate-dots telega--status))
        (aux-status (telega--animate-dots telega--status-aux)))
    (when (or conn-status aux-status)
      (telega-status--set conn-status aux-status 'raw))))

(defun telega-status--timer-start ()
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
             (telega-status--timer-start))
            ((string-match "\\.+$" telega--status-aux)
             (telega-status--timer-start))
            (telega-status--timer
             (cancel-timer telega-status--timer))))

  (with-telega-root-buffer
    (setq mode-line-process (concat ":" telega--status))
    (telega-save-cursor
      (let ((button (button-at (point-min))))
        (cl-assert (and button (eq (button-type button) 'telega-status))
                   nil "Telega status button is gone")
        (telega-button--update-value
         button (cons telega--status telega--status-aux)))))
  ))

(defun telega-root--on-chat-update0 (ewoc-name ewoc chat &optional chat-node)
  "Update CHAT in EWOC named EWOC-NAME.
CHAT could be new to the ewoc, in this case create new node.
CHAT-NODE is EWOC's node for the CHAT."
  (unless chat-node
    (setq chat-node (telega-ewoc--find-by-data ewoc chat)))

  (if (and chat-node (not (plist-get chat :telega-need-reorder-p)))
      (telega-ewoc--move-node ewoc chat-node chat-node
                              telega-root-keep-cursor)

    ;; Reorder needed or new chat created
    (let* ((cmp-func (telega-root-view--ewoc-sorter ewoc-name #'telega-chat>))
           (before-node (telega-ewoc--find-if ewoc
                          (lambda (echat)
                            (unless (eq chat echat)
                              (funcall cmp-func chat echat))))))
      (if (not chat-node)
          ;; New chat created
          (telega-save-cursor
            (if before-node
                (ewoc-enter-before ewoc before-node chat)
              (ewoc-enter-last ewoc chat)))

        ;; Reorder
        (telega-ewoc--move-node ewoc chat-node before-node
                                telega-root-keep-cursor))
      )))

(defun telega-root--any-on-chat-update (ewoc-name ewoc chat _events)
  "Update CHAT in EWOC.
If corresponding chat node does not exists in EWOC, then create new one."
  (telega-root--on-chat-update0 ewoc-name ewoc chat))

(defun telega-root--existing-on-chat-update (ewoc-name ewoc chat _events)
  "Update CHAT in EWOC, only if corresponding chat node exists."
  (when-let ((chat-node (telega-ewoc--find-by-data ewoc chat)))
    (telega-root--on-chat-update0 ewoc-name ewoc chat chat-node)))

(defun telega-root--contact-on-user-update (ewoc-name ewoc user)
  "Update USER in EWOC."
  ;; User might change online status
  (when-let ((user-node (telega-ewoc--find-by-data ewoc user)))
    (let* ((user-cmp-func (telega-root-view--ewoc-sorter
                           ewoc-name #'telega-user-cmp-by-status))
           (before-node (telega-ewoc--find-if ewoc
                          (lambda (euser)
                            (unless (eq user euser)
                              (funcall user-cmp-func user euser))))))
      (cl-assert (not (eq user-node before-node)))
      (telega-ewoc--move-node ewoc user-node before-node telega-root-keep-cursor)
      )))

(defun telega-root--contact-on-chat-update (ewoc-name ewoc chat _events)
  (when-let ((user (telega-chat-user chat)))
    (telega-root--contact-on-user-update ewoc-name ewoc user)))


;;; Fast navigation
(defun telega-root-next-match-p (chat-filter &optional n wrap)
  "Goto N's chat matching CHAT-FILTER."
  (goto-char
   (save-excursion
     (or (telega-button-forward
          (or n 1)
          (lambda (button)
            (when-let ((chat (telega-chat-at button)))
              (telega-chat-match-p chat chat-filter)))
          'no-error)
         (when wrap
           ;; Wrap from the beginning
           (goto-char (point-min))
           (telega-button-forward
               (or n 1)
             (lambda (button)
               (when-let ((chat (telega-chat-at button)))
                 (telega-chat-match-p chat chat-filter)))
             'no-error))
         (user-error "No more chats matching: %S" chat-filter)))))

(defun telega-root-next-unread (n)
  "Move point to the next chat with unread message."
  (interactive "p")
  (telega-root-next-match-p 'unread n 'wrap))

(defun telega-root-next-important (n)
  "Move point to the next chat with important messages."
  (interactive "p")
  (let ((important-filter (or (cdr (assoc "Important" telega-filters-custom))
                              '(or mention (and unread unmuted)))))
    (telega-root-next-match-p important-filter n 'wrap)))

(defun telega-root-next-mention (n)
  "Move point to the next chat with mention."
  (interactive "p")
  (telega-root-next-match-p 'mention n 'wrap))


;;; Searching contacts, global public chats and messages
(defun telega-root--loading-animate ()
  "Animate loading dots for the footers of search ewocs."
  (let ((need-animation-p nil))
    (dolist (ewoc (mapcar #'cdr telega-root-view--ewocs-alist))
      (let ((new-footer (telega--animate-dots (cdr (ewoc-get-hf ewoc)))))
        (when new-footer
          (with-telega-root-buffer
            (telega-save-cursor
              (setq need-animation-p t)
              (telega-ewoc--set-footer ewoc (concat new-footer "\n")))))))

    (unless need-animation-p
      (cancel-timer telega-loading--timer)
      (setq telega-loading--timer nil))))

(defun telega-loading--timer-start ()
  "Ensure `telega-loading--timer' is started."
  (unless telega-loading--timer
    (setq telega-loading--timer
          (run-with-timer telega-status-animate-interval
                          telega-status-animate-interval
                          #'telega-root--loading-animate))))

(defun telega-root--messages-search (&optional last-msg)
  "Search for messages."
  (let* ((ewoc-spec (telega-root-view--ewoc-spec "messages"))
         (query (plist-get ewoc-spec :search-query)))
    (cl-assert query)
    (telega-root-view--ewoc-loading-start "messages"
      (telega--searchMessages query last-msg telega-tdlib--chat-list
                              #'telega-root--messages-add))))

(defun telega-root--call-messages-search (&optional last-msg)
  "Search for call messages."
  (let* ((ewoc-spec (telega-root-view--ewoc-spec "messages"))
         (only-missed-p (plist-get ewoc-spec :only-missed-p)))
    (telega-root-view--ewoc-loading-start "messages"
      (telega--searchCallMessages last-msg nil only-missed-p
        #'telega-root--call-messages-add))))

(defun telega-root--messages-add0 (messages search-func)
  "Add MESSAGES to the \"messages\" ewoc."
  (telega-root-view--ewoc-loading-done "messages" messages)

  ;; If none of the messages is visible (according to active
  ;; filters) and last-msg is available, then fetch more messages
  ;; automatically.
  ;; Otherwise, when at least one message is display, show
  ;; "Load More" button
  (when-let ((last-msg (car (last messages))))
    (with-telega-root-view-ewoc "messages" ewoc
      (if (telega-ewoc--empty-p ewoc)
          ;; no nodes visible, fetch next automatically
          (funcall search-func last-msg)

        (telega-save-cursor
          (telega-ewoc--set-footer
           ewoc (telega-ins--as-string
                 (telega-ins--button "Load More"
                   :value last-msg
                   :action search-func)))))
      )))

(defun telega-root--messages-add (messages)
  (telega-root--messages-add0 messages #'telega-root--messages-search))

(defun telega-root--call-messages-add (messages)
  (telega-root--messages-add0 messages #'telega-root--call-messages-search))


;;; Emacs runtime environment for telega
(defun telega--check-buffer-switch ()
  "Check if chat buffer is switched.
And run `telega-chatbuf--switch-out' or `telega-chatbuf--switch-in'."
  (let ((cbuf (current-buffer)))
    (unless (eq cbuf telega--last-buffer)
      (condition-case err
          ;; NOTE: trigger switch out only if buffer loses visibility
          ;; so help windows, such as bot inlines can be shown,
          ;; without sending drafts
          (when (and (buffer-live-p telega--last-buffer)
                     (not (get-buffer-window telega--last-buffer)))
            (with-current-buffer telega--last-buffer
              (when telega-chatbuf--chat
                (telega-chatbuf--switch-out))))
        (error
         (message "telega: error in `telega-chatbuf--switch-out': %S" err)))

      (setq telega--last-buffer cbuf)

      (when telega--help-win-dirty-p
        (telega-help-win--maybe-redisplay cbuf telega--help-win-param))

      (condition-case err
          ;; NOTE: trigger switch in only if buffer gets visibility
          (when telega-chatbuf--chat
            (telega-chatbuf--switch-in)

            ;; See docstring for `telega-root-keep-cursor'
            (when (eq telega-root-keep-cursor 'track)
              (telega-root--keep-cursor-at-chat telega-chatbuf--chat))
            )
        (error
         (message "telega: error in `telega-chatbuf--switch-in': %S" err))))))

(defun telega-online-status-timer-function ()
  "Timer function for online status change."
  (setq telega-online--timer nil)
  ;; NOTE:
  ;;  - telega server might unexpectedly die
  ;;  - telega might not be in authorized state, so setOption will
  ;;    result in error "Unauthorized"
  (when (and (telega-server-live-p)
             (equal telega--auth-state "Ready"))
    (let* ((online-p (funcall telega-online-status-function))
           ;; Me user might be not available yet, at start time
           (me-user (telega-user-me 'locally))
           (curr-online-p (when me-user (telega-user-online-p me-user))))
      (unless (eq online-p curr-online-p)
        (telega--setOption :online (if online-p t :false))))))

(defun telega-check-focus-change ()
  "Function called when some emacs frame changes focus."
  ;; Make a decision about online status in `status-interval' seconds
  (let ((status-interval (if (funcall telega-online-status-function)
                             telega-online-status-interval
                           telega-offline-status-interval)))
    (if telega-online--timer
        (timer-set-time telega-online--timer (time-add nil status-interval))
      (setq telega-online--timer
            (run-with-timer
             status-interval nil 'telega-online-status-timer-function))))

  ;; Support for Emacs without 'after-focus-change-function
  (unless (boundp 'after-focus-change-function)
    (when (eq major-mode 'telega-chat-mode)
      (telega-chatbuf--check-focus-change)))
  )

(defun telega-handle-focus-change (&optional in-p)
  "Handle frame focus change.
If IN-P is non-nil then it is `focus-in', otherwise `focus-out'."
  (let ((frame (selected-frame)))
    (when (frame-live-p frame)
      (setf (frame-parameter frame 'x-has-focus) in-p)
      (telega-check-focus-change))))

(defalias 'telega-handle-focus-out 'telega-handle-focus-change)

(defun telega-handle-focus-in ()
  (telega-handle-focus-change t))

(defun telega-handle-emacs-idle ()
  "Timer function for `telega-idle--timer'."
  ;; For `telega-buffer-p' as `telega-online-status-function'
  (unless telega-online--timer
    (telega-check-focus-change))
  )

(defun telega-runtime-setup ()
  "Setup Emacs environment for telega runtime."
  ;; Adjust `telega-location-size' in case it exceeds 1024x1024
  (let ((cheight (car telega-location-size))
        (cwidth (cdr telega-location-size)))
    (while (> (telega-chars-xheight cheight) 1024)
      (cl-decf cheight))
    (while (> (telega-chars-xwidth cwidth) 1024)
      (cl-decf cwidth))
    (setq telega-location-size (cons cheight cwidth)))

  (add-hook 'post-command-hook 'telega--check-buffer-switch)
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function
                    'telega-check-focus-change)

    (with-no-warnings
      (add-hook 'focus-in-hook 'telega-handle-focus-in)
      (add-hook 'focus-out-hook 'telega-handle-focus-out)))

  (setq telega-idle--timer
        (run-with-idle-timer telega-idle-delay
                             :repeat #'telega-handle-emacs-idle))
  )

(defun telega-runtime-teardown ()
  "Teardown telega runtime Emacs environment."
  (remove-hook 'post-command-hook 'telega--check-buffer-switch)

  (if (boundp 'after-focus-change-function)
      (remove-function after-focus-change-function
                       'telega-check-focus-change)

    (with-no-warnings
      (remove-hook 'focus-in-hook 'telega-handle-focus-in)
      (remove-hook 'focus-out-hook 'telega-handle-focus-out)))

  (cancel-timer telega-idle--timer)
  )

;;; RootView
(defun telega-root-view--set-header (header)
  "Set HEADER for the root view."
  (save-excursion
    (delete-region telega-root-view--header-marker
                   telega-root-view--ewocs-marker)
    (telega-ins header)
    (setq telega-root-view--ewocs-marker (point-marker))))

(defun telega-root-view--update (on-update-prop &rest args)
  "Update root view ewocs using ON-UPDATE-PROP ewoc-spec property and ARGS."
  (with-telega-root-buffer
    (dolist (ewoc-spec (nthcdr 2 telega-root--view))
      (let ((ewoc-name (plist-get ewoc-spec :name)))
        (with-telega-root-view-ewoc ewoc-name ewoc
          (when-let ((on-update-func (plist-get ewoc-spec on-update-prop)))
            (apply on-update-func ewoc-name ewoc args)))))

    (run-hooks 'telega-root-update-hook)))

(defun telega-root-view--redisplay ()
  "Resort items in root view ewocs according to active sort criteria."
  (with-telega-root-buffer
    (telega-save-cursor
      (dolist (ewoc-spec (nthcdr 2 telega-root--view))
        (let ((ewoc-name (plist-get ewoc-spec :name)))
          (with-telega-root-view-ewoc ewoc-name ewoc
            (let ((items (ewoc-collect ewoc #'identity)))
              (telega-ewoc--clean ewoc)
              (dolist (item (sort items (telega-root-view--ewoc-sorter ewoc-name)))
                (ewoc-enter-last ewoc item))
              )))))

    (run-hooks 'telega-root-update-hook)))

(defun telega-root-view--apply (view-spec &optional view-filter)
  "Enable root view defined by VIEW-SPEC.
VIEW-SPEC is list, where first element is function name.
Second element is string to display in root ewoc header.
Third element is inserter function for the chats.
VIEW-FILTER is additional chat filter for this root view."
  (with-telega-root-buffer
    ;; Cancel all ewoc loading activities
    (dolist (ewoc-spec (nthcdr 2 telega-root--view))
      (when-let ((loading (plist-get ewoc-spec :loading)))
        (telega-server--callback-put loading 'ignore)))

    ;; Recover ewocs
    (setq telega-root-view--ewocs-alist nil)
    ;; Always move cursor to the start of the ewocs
    (goto-char telega-root-view--header-marker)
    (delete-region (point) (point-max))

    ;; Apply root view filter before applying VIEW-SPEC, because
    ;; filter might affect the view
    (unless (equal view-filter telega-root--view-filter)
      (setq telega-root--view-filter view-filter)
      (telega-filters--update)
      (telega-filters--redisplay))

    ;; Activate VIEW-SPEC by creating ewocs specified in view-spec
    (setq telega-root--view view-spec)
    (save-excursion
      (when-let ((view-name (nth 1 view-spec)))
        (telega-ins--with-attrs
            (list :elide t
                  :elide-trail (/ telega-root-fill-column 3)
                  :min telega-root-fill-column
                  :max telega-root-fill-column
                  :align 'left
                  :face 'telega-root-heading)
          (telega-ins (if (listp view-name) (car view-name) "View") ": ")
          (telega-ins--with-face 'bold
            (telega-ins (if (listp view-name) (cadr view-name) view-name)))
          (telega-ins " ")
          (telega-ins--button "Reset"
            :action #'telega-view-reset)
          (telega-ins " "))
        (telega-ins "\n"))

      (setq telega-root-view--ewocs-marker (point-marker))
      (let ((ewoc-specs (nthcdr 2 view-spec))
            (need-loading-timer-p nil))
        (while ewoc-specs
          (telega-root-view--ewoc-create (car ewoc-specs))
          (goto-char (point-max))
          (when (plist-get (car ewoc-specs) :loading)
            (setq need-loading-timer-p t))
          (when (setq ewoc-specs (cdr ewoc-specs))
            (telega-ins telega-root-view-ewocs-delim)))

        (when need-loading-timer-p
          (telega-loading--timer-start))))
    ))

(defun telega-view--root-ewoc-spec (&optional custom-inserter)
  "Return view spec for the default root view."
  (list :name "root"
        :pretty-printer (if custom-inserter
                            (lambda (chat)
                              (telega-root--chat-known-pp chat custom-inserter))
                          #'telega-root--chat-known-pp)
        :sorter #'telega-chat>
        :items telega--ordered-chats
        :on-chat-update #'telega-root--any-on-chat-update))

(defun telega-view-default (&optional func view-name custom-inserter)
  "Default root view."
  (interactive)
  (telega-root-view--apply
   (list (or func 'telega-view-default)
         view-name (telega-view--root-ewoc-spec custom-inserter))))

(defun telega-view-reset (&rest _ignored_args)
  "Reset rootview to the default value."
  (interactive)
  (let ((reset-view-func (or telega-root-default-view-function
                             #'telega-view-default)))
    (unless (eq telega-root-default-view-function (car telega-root--view))
      (call-interactively reset-view-func))))

(defun telega-view-compact ()
  "Compact view for the rootbuf."
  (interactive)
  (telega-view-default
   'telega-view-compact "Compact" #'telega-ins--chat))

(defun telega-view-one-line ()
  "View chat list as one line."
  (interactive)
  (telega-view-default
   'telega-view-one-line "One Line" #'telega-ins--chat-full))

(defun telega-view-two-lines ()
  "View chat list as 2 lines."
  (interactive)
  (telega-view-default
   'telega-view-two-lines "Two Lines" #'telega-ins--chat-full-2lines))

(defun telega-root--on-message-update (_ewoc-name _ewoc _msg)
  "Handle message update."
  ;; TODO
  )

(defun telega-root--messages-sorter (msg1 msg2)
  "Sorter for searched messages."
  (if (or telega--sort-criteria telega--sort-inverted)
      ;; Active sort criteria is used
      (let ((telega-sort--inhibit-order t))
        (telega-chat> (telega-msg-chat msg1) (telega-msg-chat msg2)))

    ;; Sort by message date
    (> (if (zerop (plist-get msg1 :edit_date))
           (plist-get msg1 :date)
         (plist-get msg1 :edit_date))
       (if (zerop (plist-get msg2 :edit_date))
           (plist-get msg2 :date)
         (plist-get msg2 :edit_date)))))

(defun telega-view-search (query)
  "View QUERY search results."
  (interactive
   (list (read-string "Search Query: " nil 'telega-search-history)))

  (telega-root-view--apply
   (list 'telega-view-search
         (concat "Search"
                 (unless (string-empty-p query)
                   (format " \"%s\"" query)))
         (list :name "root"
               :pretty-printer #'telega-root--chat-known-pp
               :search-query query
               :sorter #'telega-chat>
               :loading (telega--searchChats query nil
                          (apply-partially
                           #'telega-root-view--ewoc-loading-done "root"))
               :on-chat-update #'telega-root--existing-on-chat-update)
         (list :name "contacts"
               :pretty-printer #'telega-root--contact-pp
               :header "CONTACTS"
               :search-query query
               :sorter #'telega-user-cmp-by-status
               :loading (telega--searchContacts query nil
                          (apply-partially
                           #'telega-root-view--ewoc-loading-done "contacts"))
               :on-chat-update #'telega-root--contact-on-chat-update
               :on-user-update #'telega-root--contact-on-user-update)
         (list :name "global"
               :pretty-printer #'telega-root--global-chat-pp
               :header "GLOBAL CHATS"
               :search-query query
               :sorter #'telega-chat>
               :loading (telega--searchPublicChats query
                          (apply-partially
                           #'telega-root-view--ewoc-loading-done "global"))
               :on-chat-update #'telega-root--existing-on-chat-update)

         (list :name "messages"
               :pretty-printer #'telega-root--message-pp
               :header "MESSAGES"
               :search-query query
               :sorter #'telega-root--messages-sorter
               :on-message-update #'telega-root--on-message-update)
         ))

  (telega-root--messages-search)
  )

(defun telega-view-contacts (query)
  "View contacts searched by QUERY.
If QUERY is empty string, then show all contacts."
  (interactive
   (list (read-string "Search Contacts [RET for all]: ")))

  (telega-root-view--apply
   (list 'telega-view-contacts
         (concat "Contacts"
                 (unless (string-empty-p query)
                   (format " \"%s\"" query)))
         (list :name "contacts"
               :pretty-printer #'telega-root--contact-pp
               :search-query query
               :sorter #'telega-user-cmp-by-status
               :loading (telega--searchContacts query nil
                          (apply-partially
                           #'telega-root-view--ewoc-loading-done "contacts"))
               :on-chat-update #'telega-root--contact-on-chat-update
               :on-user-update #'telega-root--contact-on-user-update))
   ))

(defun telega-root--nearby-on-chat-update (ewoc-name ewoc chat _events)
  "Update nearby CHAT in EWOC, chat dirtiness is cased by EVENTS."
  (when (telega-chat-nearby-find (plist-get chat :id))
    (telega-root--on-chat-update0 ewoc-name ewoc chat)))

(defun telega-root--nearby-sorter (chat1 chat2)
  "Sorter for nearby chats CHAT1 and CHAT2."
  (let ((telega--sort-criteria
         (append telega--sort-criteria '(nearby-distance))))
    (telega-chat> chat1 chat2)))

(defun telega-root--nearby-chats-add (chats)
  "Mark all nearby chats as dirty and update them."
  (telega-root-view--ewoc-loading-done "global")
  (dolist (chat chats)
    (telega-chat--mark-dirty chat))

  (telega-chats-dirty--update)
  (telega-filters--redisplay))

(defun telega-view-nearby ()
  "View contacts and chats nearby `telega-my-location'."
  (interactive)
  (unless telega-my-location
    (user-error "`telega-my-location' is unset, can't search nearby chats"))

  (telega-root-view--apply
   (list 'telega-view-nearby
         (concat "Nearby " (telega-location-to-string telega-my-location))
         (list :name "root"
               :pretty-printer #'telega-root--nearby-chat-known-pp
               :sorter #'telega-chat>
               :items telega--ordered-chats
               :on-chat-update #'telega-root--any-on-chat-update)
         (list :name "contacts"
               :pretty-printer #'telega-root--nearby-contact-pp
               :header "CONTACTS NEARBY"
               :sorter #'telega-user-cmp-by-status
               :loading (telega--getContacts
                         (apply-partially
                          #'telega-root-view--ewoc-loading-done "contacts"))
               :on-chat-update #'telega-root--contact-on-chat-update
               :on-user-update #'telega-root--contact-on-user-update)
         (list :name "global"
               :pretty-printer #'telega-root--nearby-global-chat-pp
               :header "CHATS NEARBY"
               :sorter #'telega-root--nearby-sorter
               :loading (telega--searchChatsNearby telega-my-location
                          #'telega-root--nearby-chats-add)
               :on-chat-update #'telega-root--nearby-on-chat-update)
         )))

(defun telega-view-last-messages ()
  "View last messages in the chats."
  (interactive)
  (telega-root-view--apply
   (list 'telega-view-last-messages
         "Last Messages"
         (list :name "root"
               :pretty-printer #'telega-root--chat-last-message-pp
               :items telega--ordered-chats
               :sorter #'telega-chat>
               :on-chat-update #'telega-root--any-on-chat-update))))

(defun telega-view-calls (arg)
  "View calls.
If `\\[universal-argument]' is given, then view missed calls only."
  (interactive "P")
  (telega-root-view--apply
   (list 'telega-view-calls
         (if arg "Missed Calls" "All Calls")
         (list :name "messages"
               :pretty-printer #'telega-root--message-call-pp
               :sorter #'telega-root--messages-sorter
               :only-missed-p arg)))

  (telega-root--call-messages-search))

(defun telega-root--topics-on-chat-update (ewoc-name ewoc chat events)
  "Handler for chat updates in \"topics\" root view."
  (let ((topic-filter (plist-get (telega-root-view--ewoc-spec ewoc-name)
                                 :topic-filter)))
    (if (telega-chat-match-p chat topic-filter)
        (telega-root--any-on-chat-update ewoc-name ewoc chat events)

      ;; Possible need a removal from EWOC
      (when-let ((chat-node (telega-ewoc--find-by-data ewoc chat)))
        (ewoc-delete ewoc chat-node)))
    ))

(defun telega-view-topics--ewoc-spec (topic-spec &optional no-upcase-p)
  "Return ewoc spec for topic ewoc labeled with LABEL."
  (list :name (car topic-spec)
        :topic-filter (cdr topic-spec)
        :header (funcall (if no-upcase-p #'identity #'upcase) (car topic-spec))
        :pretty-printer #'telega-root--chat-known-pp
        :sorter #'telega-chat>
        :items (telega-filter-chats telega--ordered-chats (cdr topic-spec))
        :on-chat-update #'telega-root--topics-on-chat-update))

(defun telega-view-topics ()
  "Group chats by `telega-root-view-topics'."
  (interactive)
  (let* ((folder-names (telega-folder-names))
         (ewoc-specs-for-folders
          (when telega-root-view-topics-folders
            (mapcar (lambda (folder-name)
                      (telega-view-topics--ewoc-spec
                       (cons (telega-folder-format "%i%f" folder-name)
                             (list 'folder folder-name))
                       'no-upcase))
                    folder-names))))
    (telega-root-view--apply
     `(telega-view-topics
       "Topics"
       ,@(when (eq telega-root-view-topics-folders 'prepend)
           ewoc-specs-for-folders)
       ,@(mapcar #'telega-view-topics--ewoc-spec telega-root-view-topics)
       ,@(when (eq telega-root-view-topics-folders 'append)
           ewoc-specs-for-folders)
       ,(when telega-root-view-topics-other-chats
          (let ((other-filter
                 `(not (any ,@(mapcar 'cdr telega-root-view-topics)
                            ,@(when telega-root-view-topics-folders
                                (list (cons 'folder folder-names)))))))
            (list :name "topics-other-chats"
                  :topic-filter other-filter
                  :header "OTHER CHATS"
                  :pretty-printer #'telega-root--chat-known-pp
                  :sorter #'telega-chat>
                  :items (telega-filter-chats telega--ordered-chats other-filter)
                  :on-chat-update #'telega-root--topics-on-chat-update)))))))

(defun telega-view-top--sorter (chat1 chat2)
  "Sorter for top chats."
  (let ((telega-sort--inhibit-order t))
    (telega-chat> chat1 chat2)))

(defun telega-view-top--ewoc-spec (category-spec)
  "Return ewoc spec for top ewoc using CATEGORY-SPEC."
  (list :name (car category-spec)
        :top-category (car category-spec)
        :header (upcase (car category-spec))
        :pretty-printer #'telega-root--chat-known-pp
        :sorter #'telega-view-top--sorter
        :loading (telega--getTopChats
                     (car category-spec) (cdr category-spec)
                   (apply-partially
                    #'telega-root-view--ewoc-loading-done (car category-spec)))
        :on-chat-update #'telega-root--existing-on-chat-update))

(defun telega-view-top ()
  "View top chats in all categories."
  (interactive)
  (telega-root-view--apply
   (nconc (list 'telega-view-top "Top Chats")
          (mapcar #'telega-view-top--ewoc-spec
                  telega-root-view-top-categories)))
  )

(defun telega-view-settings--me-pp (me-id)
  "Pretty printer for me in settings root view."
  (let* ((me-user (telega-user--get me-id))
         (photo (plist-get me-user :profile_photo))
         (avatar (telega-media--image
                  (cons me-user (lambda (user file)
                                  (telega-avatar--create-image user file 3)))
                  (cons photo :small)
                  nil :telega-avatar-3)))
    (telega-ins--image-slices avatar nil
      (lambda (slice-num)
        (telega-ins " ")
        (cond ((= slice-num 0)
               (telega-ins--with-face 'bold
                 (telega-ins (telega-user--name me-user 'name)))
               (telega-ins " ")
               (telega-ins--button "Change"
                 'action (lambda (_button)
                           (let* ((names (split-string (read-from-minibuffer "Your Name: ") " "))
                                  (first-name (car names))
                                  (last-name (mapconcat #'identity (cdr names) " ")))
                             (telega--setName first-name last-name)))))
              ((= slice-num 1)
               (telega-ins "+" (telega-tl-str me-user :phone_number)))
              ((= slice-num 2)
               (if-let ((username (telega-tl-str me-user :username)))
                   (progn
                     (telega-ins "@" username)
                     (telega-ins " ")
                     (telega-ins--button "Change"
                       'action (lambda (_button)
                                 (telega--setUsername
                                  (read-string
                                   "Set username [empty to delete]: ")))))
                 (telega-ins--button "Set Username"
                   'action (lambda (_button)
                             (telega--setUsername
                              (read-string
                               "Set username [empty to delete]: ")))))))
       ))

    (telega-ins "\n")
    (telega-ins "Profile Photos: ")
    (telega-ins--button "Set Profile Photo"
      'action (lambda (_ignored)
                (let ((photo (read-file-name "Profile Photo: " nil nil t)))
                  (telega--setProfilePhoto photo))))
    (telega-ins "\n")
    (telega-ins--user-profile-photos me-user
      (lambda ()
        (with-telega-root-view-ewoc "me" ewoc
          (ewoc-refresh ewoc))))

    (telega-ins--labeled (concat (telega-i18n "profile_bio") " ") nil
      (if-let ((bio (telega-tl-str (telega--full-info me-user) :bio)))
          (progn
            (telega-ins bio)
            (telega-ins " ")
            (telega-ins--button "Change Bio"
              'action (lambda (_button)
                        (telega--setBio
                         (read-string
                          "Change bio [empty to delete]: " bio)))))
        (telega-ins--button "Set Bio"
          'action (lambda (_button)
                    (telega--setBio
                     (read-string
                      "Set bio [empty to delete]: "))))))
    (telega-ins "\n")
    (telega-ins--labeled "  " nil
      (telega-ins--with-face 'shadow
        (telega-ins-i18n "settings_about_bio")))
    (telega-ins "\n")
    (telega-ins--account-ttl
     (lambda ()
       (with-telega-root-view-ewoc "me" ewoc
         (ewoc-refresh ewoc))))
    (telega-ins "\n")
  ))

(defun telega-root--me-on-user-update (_ewoc-name ewoc user)
  "Update USER in EWOC."
  (when (telega-me-p user)
    (telega-save-cursor
      (ewoc-refresh ewoc))))

(defun telega-view-settings--option-pp (option-spec)
  "Pretty printer for option in \"settings\" root view."
  (let* ((opt-title (nth 0 option-spec))
         (opt-val (telega--getOption (nth 1 option-spec))))
    (telega-ins opt-title ": ")
    (cl-assert (memq (telega--tl-type opt-val)
                     '(optionValueBoolean optionValueEmpty)))

    (telega-ins--button (if (plist-get opt-val :value)
                            telega-symbol-heavy-checkmark
                          "  ")
      'action (lambda (_ignored)
                (telega--setOption (nth 1 option-spec)
                  (not (plist-get opt-val :value))
                  'sync)
                (with-telega-root-view-ewoc "options" opts-ewoc
                  (telega-save-cursor
                    (ewoc-refresh opts-ewoc)))))
    (telega-ins "\n")
    ))

(defun telega-view-settings--link-pp (link)
  "Pretty printer for link in \"settings\" root view.
LINK is cons, where car is the link description, and cdr is the url."
  (telega-ins (car link) ": ")
  (telega-ins--raw-button (telega-link-props 'url (cdr link))
    (telega-ins (cdr link)))
  (telega-ins "\n"))

(defun telega-view-settings ()
  "View top chats in all categories."
  (interactive)
  (telega-root-view--apply
   (list 'telega-view-settings (telega-i18n "menu_settings")
         (list :name "me"
               :pretty-printer #'telega-view-settings--me-pp
               :items (list telega--me-id)
               :on-user-update #'telega-root--me-on-user-update)
         (list :name "options"
               :pretty-printer #'telega-view-settings--option-pp
               :header "OPTIONS"
               :items (list (list (telega-i18n "telega_option_sensitive_content")
                                  :ignore_sensitive_content_restrictions)
                            (list (telega-i18n "telega_option_prefer_ipv6")
                                  :prefer_ipv6)
                            ;; :disable_contact_registered_notifications
                            ;; :disable_top_chats
                            ;; :use_quick_ack
                            ;; :use_storage_optimizer
                            ))
         (list :name "links"
               :pretty-printer #'telega-view-settings--link-pp
               :header "LINKS"
               :items (list (cons (telega-i18n "telega_settings_telega_manual")
                                  "https://zevlg.github.io/telega.el/")
                            (cons (telega-i18n "settings_faq")
                                  "https://telegram.org/faq")
                            (cons (telega-i18n "settings_ask_question")
                                  "https://t.me/emacs_telega")))
         ))
  )

(defun telega-view-folders--gen-pp (folder-name)
  "Generate pretty printer for chats in folder with FOLDER-NAME."
  (let ((tdlib-chat-list (telega-filter--folder-tdlib-chat-list folder-name)))
    (lambda (chat)
      (when (telega-chat-match-p chat (list 'folder folder-name))
        (let ((telega-tdlib--chat-list tdlib-chat-list)
              (telega-filters--inhibit-list '(chat-list folder main archive)))
          (telega-root--chat-known-pp chat))))))

(defun telega-view-folders--gen-sorter (folder-name)
  (let ((tdlib-chat-list (telega-filter--folder-tdlib-chat-list folder-name)))
    (lambda (chat1 chat2)
      (let ((telega-tdlib--chat-list tdlib-chat-list))
        (telega-chat> chat1 chat2)))))

(defun telega-view-folders--ewoc-spec (folder-spec)
  (let* ((folder-name (telega-tl-str folder-spec :title))
         (tdlib-chat-list (telega-filter--folder-tdlib-chat-list folder-name)))
    (cl-assert tdlib-chat-list)
    (list :name folder-name
          :pretty-printer (telega-view-folders--gen-pp folder-name)
          :header (telega-folder-format "%i%f" folder-name)
          :sorter (telega-view-folders--gen-sorter folder-name)
          :loading (telega--getChats nil tdlib-chat-list
                     (apply-partially
                      #'telega-root-view--ewoc-loading-done folder-name))
          :on-chat-update #'telega-root--any-on-chat-update)))

(defun telega-view-folders ()
  "View Telegram folders."
  (interactive)
  (telega-root-view--apply
   (nconc (list 'telega-view-folders "Folders")
          (mapcar #'telega-view-folders--ewoc-spec
                  telega-tdlib--chat-filters))))

(defun telega-root--chat-goto-pinned-message (chat)
  "Goto pinned message in the CHAT."
  (let ((pin-msg-id (plist-get chat :pinned_message_id)))
    (unless (zerop pin-msg-id)
      (telega-chat--goto-msg chat pin-msg-id 'highlight))))

(defun telega-root--chat-pinned-message-pp (chat)
  "Pretty printer for CHAT's pinned message."
  (let ((visible-p (not (zerop (plist-get chat :pinned_message_id)))))
    (when visible-p
      (let ((telega-chat-button-width (+ telega-chat-button-width
                                         (/ telega-chat-button-width 2))))
        (telega-root--chat-known-pp
         chat
         #'telega-ins--chat-pinned-message
         #'telega-root--chat-goto-pinned-message)))))

(defun telega-view-pinned-messages ()
  "View pinned messages of the chats."
  (interactive)
  (telega-root-view--apply
   (list 'telega-view-pinned-messages
         (telega-i18n "telega_view_pinned_messages")
         (list :name "root"
               :pretty-printer #'telega-root--chat-pinned-message-pp
               :items telega--ordered-chats
               :sorter #'telega-chat>
               :on-chat-update #'telega-root--any-on-chat-update))
   'has-pinned-message))

(defun telega-root--deleted-chat-pp (chat)
  "Pretty printer for deleted chat."
  (when (and (memq chat telega-deleted-chats)
             (equal (telega-chat-order chat) "0"))
    (telega-root--global-chat-pp chat)))

(defun telega-view-deleted-chats ()
  "View recently deleted chats."
  (interactive)
  (telega-root-view--apply
   (list 'telega-view-deleted-chats
         (telega-i18n "telega_view_deleted_chats")
         (list :name "root"
               :pretty-printer #'telega-root--deleted-chat-pp
               :items telega-deleted-chats
               :sorter (lambda (chat1 chat2)
                         (> (length (memq chat1 telega-deleted-chats))
                            (length (memq chat2 telega-deleted-chats))))
               :on-chat-update #'telega-root--any-on-chat-update))
   ))

(provide 'telega-root)

;;; telega-root.el ends here

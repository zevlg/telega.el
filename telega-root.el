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

;;; Commentary:

;; * Root Buffer
;; 
;; rootbuf is the heart of =telega=.
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
(require 'telega-customize)

(declare-function tracking-mode "tracking" (&optional arg))

(declare-function telega-chats--kill-em-all "telega-chat")
(declare-function telega-chat-title "telega-chat" (chat &optional with-username))
(declare-function telega-chat-get "telega-chat" (chat-id &optional offline-p))
(declare-function telega-chat-at "telega-chat" (&optional pos))
(declare-function telega-chat--info "telega-chat" (chat))
(declare-function telega-chatbuf--switch-in "telega-chat")
(declare-function telega-chatbuf--switch-out "telega-chat")
(declare-function telega-chatbuf--check-focus-change "telega-chat")


(defvar telega-root--ewoc nil)
(defvar telega-contacts--ewoc nil
  "Ewoc for contacts list.")
(defvar telega-search--ewoc nil
  "Ewoc for global chats searched.")
(defvar telega-messages--ewoc nil
  "Ewoc for searched messages.")

(defvar telega-status--timer nil
  "Timer used to animate status string.")
(defvar telega-search--timer nil
  "Timer used to animate Loading.. status of global/messages search.")
(defvar telega-online--timer nil
  "Timer used to change online status.")

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
    (define-key map (kbd "R") 'telega-chats-filtered-toggle-read)

    ;; Calls bindings
    (define-key map (kbd "c a") 'telega-voip-accept)
    (define-key map (kbd "c d") 'telega-voip-discard)
    (define-key map (kbd "c b") 'telega-voip-buffer-show)
    (define-key map (kbd "c l") 'telega-voip-list-calls)

    (define-key map (kbd "s") 'telega-search)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "Q") 'telega-kill)

    (define-key map (kbd "m") 'telega-chat-with)

    ;; ** Fast navigation
    ;; 
    ;; {{{kbd(M-g)}}} prefix in rootbuf is used to jump across chat buttons.
    ;; 
    ;; - Key: {{{where-is(telega-root-next-unread,telega-root-mode-map)}}}
    ;; 
    ;;   {{{fundoc(telega-root-next-unread)}}}
    (define-key map (kbd "M-g u") 'telega-root-next-unread)

    ;; - Key: {{{where-is(telega-root-next-important,telega-root-mode-map)}}}
    ;; 
    ;;   {{{fundoc(telega-root-next-important)}}}
    ;; 
    ;;   Important message is the messages matching "Important" custom
    ;;   [[#chat-filters][chat filter]].  If there is no "Important"
    ;;   custom chat filter, then ~(or mention (and unread unmuted))~
    ;;   chat filter is used.
    (define-key map (kbd "M-g i") 'telega-root-next-important)

    ;; - Key: {{{where-is(telega-root-next-mention,telega-root-mode-map)}}}
    ;; 
    ;;   {{{fundoc(telega-root-next-mention)}}}
    (define-key map (kbd "M-g m") 'telega-root-next-mention)
    (define-key map (kbd "M-g @") 'telega-root-next-mention)
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
  (telega-filters--reset telega-filter-default)

  ;; NOTE: make `telega-root-keep-cursor' working as expected
  (setq-local switch-to-buffer-preserve-window-point nil)

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

  (save-excursion
    ;; NOTE: we are using ewoc with `nosep' so newline is not inserted
    ;; for non-visible chat buttons
    (goto-char (point-max))
    (insert "\n")
    (setq telega-root--ewoc
          (ewoc-create (if telega-debug
                           'telega-chat-known--pp
                         (telega-ewoc--gen-pp 'telega-chat-known--pp))
                       nil nil t))
    (dolist (chat telega--ordered-chats)
      (ewoc-enter-last telega-root--ewoc chat))

    ;; Contacts
    (goto-char (point-max))
    (insert "\n")
    (setq telega-contacts--ewoc
          (ewoc-create (if telega-debug
                           'telega-contact-root--pp
                         (telega-ewoc--gen-pp 'telega-contact-root--pp))
                       "" "" t))

    ;; Global search
    (goto-char (point-max))
    (insert "\n")
    (setq telega-search--ewoc
          (ewoc-create (if telega-debug
                           'telega-chat-global--pp
                         (telega-ewoc--gen-pp 'telega-chat-global--pp))
                       "" "" t))

    ;; Messages
    (goto-char (point-max))
    (insert "\n")
    (setq telega-messages--ewoc
          (ewoc-create (if telega-debug
                           'telega-msg-root--pp
                         (telega-ewoc--gen-pp 'telega-msg-root--pp))
                       "" "" t))
    )

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
  (when telega-search--timer
    (cancel-timer telega-search--timer))
  (when telega-online--timer
    (cancel-timer telega-online--timer))
  (telega-chats--kill-em-all)
  (telega-server-kill)

  (telega-runtime-teardown))

(defun telega-root--buffer ()
  "Return telega root buffer."
  (get-buffer telega-root-buffer-name))

(defun telega-root--keep-cursor-at (chat)
  "Keep cursor position at CHAT."
  (with-telega-root-buffer
    (when-let ((node (telega-ewoc--find-by-data telega-root--ewoc chat)))
      (goto-char (ewoc-location node))

      ;; NOTE: if rootbuf window is shown, also update window's point
      (dolist (win (get-buffer-window-list))
        (set-window-point win (ewoc-location node)))))

  ;; NOTE: Treat point move as chat update to allow user do things as
  ;; if chat's order was updated
  (run-hook-with-args 'telega-chat-update-hook chat))


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

(defun telega-root--redisplay ()
  "Redisplay root's buffer contents."
  (telega-filters--redisplay)

  (with-telega-root-buffer
    (telega-save-cursor
      (if telega-search-query
          ;; Setup headers of all the ewocs
          (let ((heading-attrs (list :min telega-root-fill-column
                                     :max telega-root-fill-column
                                     :align 'left
                                     :face 'telega-root-heading)))
            (telega-ewoc--set-header
             telega-root--ewoc
             (telega-ins--as-string
              (telega-ins--with-attrs
                  (nconc (list :elide t
                               :elide-trail (/ telega-root-fill-column 3))
                         heading-attrs)
                (telega-ins "Search: ")
                (telega-ins--with-face 'bold
                  (telega-ins telega-search-query))
                (telega-ins " ")
                (telega-ins--button "Cancel"
                  :action 'telega-search-cancel)
                (telega-ins " "))
              (telega-ins "\n")))

            (telega-ewoc--set-header
             telega-contacts--ewoc
             (telega-ins--as-string
              (telega-ins--with-attrs heading-attrs
                (telega-ins "CONTACTS"))
              (telega-ins "\n")))

            (telega-ewoc--set-header
             telega-search--ewoc
             (telega-ins--as-string
              (telega-ins--with-attrs heading-attrs
                ;; I18N: lng_search_global_results
                (telega-ins "GLOBAL SEARCH"))
              (telega-ins "\n")))

            (telega-ewoc--set-header
             telega-messages--ewoc
             (telega-ins--as-string
              (telega-ins--with-attrs heading-attrs
                (telega-ins "MESSAGES"))
              (telega-ins "\n"))))

        ;; No active search
        (ewoc-set-hf telega-search--ewoc "" "")
        (ewoc-set-hf telega-contacts--ewoc "" "")
        (ewoc-set-hf telega-messages--ewoc "" "")
        (telega-ewoc--set-header telega-root--ewoc ""))

      (ewoc-refresh telega-root--ewoc)
      (ewoc-refresh telega-contacts--ewoc)
      (ewoc-refresh telega-search--ewoc)
      (ewoc-refresh telega-messages--ewoc))))

(defun telega-root--chat-update (chat &optional for-reorder)
  "Something changed in CHAT, button needs to be updated.
If FOR-REORDER is non-nil, then CHAT's node is ok, just update filters."
  (telega-debug "IN: `telega-root--chat-update': %s" (telega-chat-title chat))

  (unless for-reorder
    (with-telega-root-buffer
      (telega-save-cursor
        (let ((enode (telega-ewoc--find-by-data
                      telega-root--ewoc chat)))
          (cl-assert enode nil "Ewoc node not found for chat:%s"
                     (telega-chat-title chat))
          (with-telega-deferred-events
            (ewoc-invalidate telega-root--ewoc enode))))))

  ;; Possible update chat in global search
  (let ((gnode (telega-ewoc--find-by-data
                telega-search--ewoc chat)))
    (when gnode
      (ewoc-invalidate telega-search--ewoc gnode)))

  ;; Update chats in searched messages
  (ewoc-map (lambda (msg)
              (eq chat (telega-msg-chat msg)))
            telega-messages--ewoc)

  (telega-filters--chat-update chat)
  (run-hook-with-args 'telega-chat-update-hook chat))

(defun telega-root--chat-reorder (chat &optional new-chat-p)
  "Move CHAT to correct place according to its order.
NEW-CHAT-P is used for optimization, to omit ewoc's node search."
  (with-telega-root-buffer
    (let* ((node (unless new-chat-p
                   (telega-ewoc--find-by-data telega-root--ewoc chat)))
           (chat-button (button-at (point)))
           (point-off (and telega-root-keep-cursor
                           chat-button
                           ;; If chat is deleted, postpone
                           ;; `telega-root-keep-cursor' behaviour
                           ;; Ignore custom order
                           (not (string= "0" (plist-get chat :order)))
                           (eq (button-get chat-button :value) chat)
                           (- (point) (button-start chat-button))))
           (chat-after (cadr (memq chat telega--ordered-chats)))
           (node-after (telega-ewoc--find-by-data
                        telega-root--ewoc chat-after)))
      (when node
        (ewoc-delete telega-root--ewoc node))
      (setq node
            (with-telega-deferred-events
              (if node-after
                  (ewoc-enter-before telega-root--ewoc node-after chat)
                (ewoc-enter-last telega-root--ewoc chat))))
      (cl-assert node)
      (when point-off
        (goto-char (ewoc-location node))
        (forward-char point-off)
        ;; NOTE: If rootbuf window is visible, also update window's positions
        (dolist (win (get-buffer-window-list))
          (set-window-point win (point)))
        ))))

(defun telega-root--chat-new (chat)
  "New CHAT has been created. Display it in root's ewoc."
  (telega-root--chat-reorder chat 'new-chat)

  ;; In case of initial chats load, redisplay custom filters
  ;; on every 50 chats loaded
  (when (and telega-filters--inhibit-redisplay
             (zerop (% (length telega--ordered-chats) 50)))
    (let ((telega-filters--inhibit-redisplay nil))
      (telega-filters--redisplay))))

(defun telega--on-updateUnreadMessageCount (event)
  "Number of unread messages has changed."
  (let ((chat-list (plist-get event :chat_list)))
    (when (or (null chat-list)
              (equal chat-list (list :@type "chatListMain")))
      (setq telega--unread-message-count (cddr event)))))

(defun telega--on-updateUnreadChatCount (event)
  "Number of unread/unmuted chats has been changed."
  (let ((chat-list (plist-get event :chat_list)))
    (when (or (null chat-list)
              (equal chat-list (list :@type "chatListMain")))
      (setq telega--unread-chat-count (cddr event)))))


;;; Fast navigation
(defun telega-root-next-match-p (chat-filter &optional n)
  "Goto N's chat matching CHAT-FILTER."
  (goto-char
   (save-excursion
     (or (telega-button-forward
          (or n 1)
          (lambda (button)
            (when-let ((chat (telega-chat-at button)))
              (telega-chat-match-p chat chat-filter)))
          'no-error)
         (user-error "No more chats matching: %S" chat-filter)))))

(defun telega-root-next-unread (n)
  "Move point to the next chat with unread message."
  (interactive "p")
  (telega-root-next-match-p 'unread n))

(defun telega-root-next-important (n)
  "Move point to the next chat with important messages."
  (interactive "p")
  (let ((important-filter (or (cdr (assoc "Important" telega-filters-custom))
                              '(or mention (and unread unmuted)))))
    (telega-root-next-match-p important-filter n)))

(defun telega-root-next-mention (n)
  "Move point to the next chat with mention."
  (interactive "p")
  (telega-root-next-match-p 'mention n))


;;; Searching global public chats and messages
(defun telega-search--animate ()
  "Animate loading dots for the footers of search ewocs."
  (let ((new-sf (telega--animate-dots
                 (cdr (ewoc-get-hf telega-search--ewoc))))
        (new-mf (telega--animate-dots
                 (cdr (ewoc-get-hf telega-messages--ewoc)))))
    (with-telega-root-buffer
      (telega-save-cursor
        (when new-sf
          (telega-ewoc--set-footer telega-search--ewoc (concat new-sf "\n")))
        (when new-mf
          (telega-ewoc--set-footer telega-messages--ewoc (concat new-mf "\n")))))

    (unless (or new-sf new-mf)
      (cancel-timer telega-search--timer)
      (setq telega-search--timer nil))))

(defun telega-search--timer-start ()
  (when telega-search--timer
    (cancel-timer telega-search--timer))
  (setq telega-search--timer
        (run-with-timer telega-status-animate-interval
                        telega-status-animate-interval
                        #'telega-search--animate)))

(defun telega-root--messages-chats ()
  "Return chats of the searched messages."
  (mapcar 'telega-msg-chat (ewoc-collect telega-messages--ewoc 'identity)))

(defun telega-root--messages-search (&optional last-msg)
  "Search the messages with `telega-search-query'.
If LAST-MSG is specified, then continue searching."
  (cl-assert (not telega--search-messages-loading))
  (setq telega--search-messages-loading
        (telega--searchMessages telega-search-query last-msg
                                (telega-filter-active-chat-list-name)
                                'telega-root--messages-add))

  (with-telega-root-buffer
    (telega-save-cursor
      (telega-ewoc--set-footer telega-messages--ewoc "Loading..\n")
      (telega-search--timer-start))))

(defun telega-root--messages-add (messages)
  "Add MESSAGES to the `telega-messages--ewoc'."
  (setq telega--search-messages-loading nil)
  (with-telega-root-buffer
    (telega-save-cursor
      (telega-ewoc--set-footer telega-messages--ewoc "")
      (dolist (msg messages)
        (ewoc-enter-last telega-messages--ewoc msg))

      (telega-filters-apply 'no-root-redisplay)

      ;; If none of the messages is visible (according to active
      ;; filters) and last-msg is available, then fetch more messages
      ;; automatically.
      ;; Otherwise, when at least one message is display, show
      ;; "Load More" button
      (let ((last-msg (car (last messages))))
        (when last-msg
          (if (telega-ewoc--empty-p telega-messages--ewoc)
              ;; no nodes visible, fetch next automatically
              (telega-root--messages-search last-msg)

            (telega-ewoc--set-footer
             telega-messages--ewoc
             (telega-ins--as-string
              (telega-ins--button "Load More"
                :value last-msg
                :action 'telega-root--messages-search))))
          )))))

(defun telega-root--global-chats ()
  "Return globally found chats."
  (ewoc-collect telega-search--ewoc 'identity))

(defun telega-root--global-search ()
  "Globally search for public chats with `telega-search-query'"
  (cl-assert (not telega--search-global-loading))
  ;; telega--searchPublicChats may return nil, meaning no search is
  ;; done.  For example if query is less then 5 chars
  (when (setq telega--search-global-loading
              (telega--searchPublicChats
               telega-search-query 'telega-root--global-add))
    (with-telega-root-buffer
      (telega-save-cursor
        (telega-ewoc--set-footer telega-search--ewoc "Loading.\n")
        (telega-search--timer-start)))))

(defun telega-root--global-add (chats)
  "Add CHATS to `telega-search--ewoc'."
  (setq telega--search-global-loading nil)
  (with-telega-root-buffer
    (telega-save-cursor
      (telega-ewoc--set-footer telega-search--ewoc "")
      (dolist (chat chats)
        (ewoc-enter-last telega-search--ewoc chat))

      (telega-filters-apply 'no-root-redisplay))))

(defun telega-search-async--cancel ()
  "Cancel async searches."
  (with-telega-root-buffer
    (telega-save-cursor
      (telega-ewoc--clean telega-search--ewoc)
      (telega-ewoc--clean telega-contacts--ewoc)
      (telega-ewoc--clean telega-messages--ewoc)))

  (when telega--search-global-loading
    (telega-server--callback-put telega--search-global-loading 'ignore)
    (setq  telega--search-global-loading nil))
  (when telega--search-messages-loading
    (telega-server--callback-put telega--search-messages-loading 'ignore)
    (setq telega--search-messages-loading nil)))

(defun telega-search-cancel (&rest _ignoredargs)
  "Cancel currently active search results."
  (interactive)
  (telega-search-async--cancel)
  (setq telega-search-query nil)
  (setq telega--search-chats nil)
  (setq telega--search-contacts nil)

  (telega-filters-apply))

(defun telega-search (cancel-p)
  "Search for the QUERY in chats, global public chats and messages.
If used with PREFIX-ARG, then cancel current search."
  (interactive "P")
  (if cancel-p
      (telega-search-cancel)

    (let ((query (read-string "Global search: " nil 'telega-search-history)))
      ;; Always move cursor to the search title
      (goto-char (telega-ewoc--location telega-root--ewoc))

      ;; Asynchronously load results from global search for chats and
      ;; messages
      (telega-search-async--cancel)

      (setq telega-search-query query)
      (setq telega--search-chats
            (telega--searchChats query))

      (setq telega--search-contacts
            (sort (telega--searchContacts query) 'telega-user-cmp-by-status))
      ;; NOTE: filter out contacts, that are already in `telega--search-chats'
      ;; (setq telega--search-contacts
      ;;       (cl-remove-if (lambda (contact)
      ;;                       (telega-filter-chats
      ;;                        telega--search-chats
      ;;                        (list 'ids (plist-get contact :id))))
      ;;                     (telega--searchContacts query)))
      (dolist (contact telega--search-contacts)
        (ewoc-enter-last telega-contacts--ewoc contact))

      (telega-root--global-search)
      (telega-root--messages-search)

      (telega-filters-apply))))


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
      (condition-case err
          ;; NOTE: trigger switch in only if buffer gets visibility
          (when telega-chatbuf--chat
            (telega-chatbuf--switch-in))
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
    (let ((online-p (telega-focus-state))
          (curr-online-p (telega-user-online-p (telega-user-me))))
      (unless (eq online-p curr-online-p)
        (telega--setOption :online (if online-p t :false))))))

(defun telega-check-focus-change ()
  "Function called when some emacs frame changes focus."
  ;; Make a decision about online status in `status-interval' seconds
  (let ((status-interval (if (telega-focus-state)
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
      (add-hook 'focus-out-hook 'telega-handle-focus-out))))

(defun telega-runtime-teardown ()
  "Teardown telega runtime Emacs environment."
  (remove-hook 'post-command-hook 'telega--check-buffer-switch)

  (if (boundp 'after-focus-change-function)
      (remove-function after-focus-change-function
                       'telega-check-focus-change)

    (with-no-warnings
      (remove-hook 'focus-in-hook 'telega-handle-focus-in)
      (remove-hook 'focus-out-hook 'telega-handle-focus-out))))

(provide 'telega-root)

;;; telega-root.el ends here

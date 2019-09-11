;;; telega-filter.el --- Chats filtering in root buffer  -*- lexical-binding:t -*-

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sun Apr 22 17:36:38 2018
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
(require 'telega-core)
(require 'telega-ins)
(require 'telega-customize)

(defvar tracking-buffers)

(declare-function telega-chat-muted-p "telega-chat"  (chat))
(declare-function telega-chat--type "telega-chat" (chat &optional no-interpret))
(declare-function telega-chat-title "telega-chat" (chat &optional with-username))
(declare-function telega-chat--info "telega-chat" (chat))
(declare-function telega-chat--user "telega-chat" (user))
(declare-function telega--getCreatedPublicChats "telega-chat")
(declare-function telega-chats-top "telega-chat" (category))

(declare-function telega-root--buffer "telega-root")
(declare-function telega-root--global-chats "telega-root")
(declare-function telega-root--redisplay "telega-root")


(defvar telega-filters--ewoc nil "ewoc for custom filters.")

(defvar telega-filters--inhibit-redisplay nil
  "Non-nil to do nothing on `telega-filters--redisplay'.
Used for optimization, when initially fetching chats, to speed things up.
Also used when TDLib updates connection state after waking up.")
(defvar telega-filters--inhibit-list nil
  "List of filters to inhibit.
Bind it to temporary disable some filters.")

(defvar telega-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ":") 'telega-filters-edit)
    (define-key map (kbd "e") 'telega-filters-edit)
    (define-key map (kbd "n") 'telega-filter-by-name)
    (define-key map (kbd "t") 'telega-filter-by-type)
    (define-key map (kbd "c") 'telega-filter-by-contact)
    (define-key map (kbd "f") 'telega-filter-by-custom)
    (define-key map (kbd "u") 'telega-filter-by-unread)
    (define-key map (kbd "m") 'telega-filter-by-mention)
    (define-key map (kbd "p") 'telega-filter-by-pin)
    (define-key map (kbd "y") 'telega-filter-by-unmuted)
    (define-key map (kbd "v") 'telega-filter-by-verified)
    (define-key map (kbd "o") 'telega-filter-by-opened)
    (define-key map (kbd "r") 'telega-filter-by-restriction)
    (define-key map (kbd "s") 'telega-filter-by-user-status)
    (define-key map (kbd "l") 'telega-filter-by-label)
    (define-key map (kbd "T") 'telega-filter-by-top)
    (define-key map (kbd "SPC") 'telega-filter-by-tracking)
    (define-key map (kbd "!") 'telega-filters-negate)
    (define-key map (kbd "/") 'telega-filters-reset)
    (define-key map (kbd "d") 'telega-filters-pop-last)
    (define-key map (kbd "DEL") 'telega-filters-pop-last)
    map)
  "Keymap for filtering commands.")

(define-button-type 'telega-filter
  :supertype 'telega
  :inserter telega-inserter-for-filter-button
  :help-echo (lambda (custom)
               (format "Filter (custom \"%s\") expands to: %s"
                       (car custom) (cdr custom)))
  'action #'telega-filter-button--action)

(defun telega-filter-button--action (button)
  "Action to take when custom filter button is pressed.
If prefix ARG is specified then set custom filter as active,
otherwise add to existing active filters."
  (let* ((custom (button-get button :value))
         (fspec (if telega-filter-custom-expand
                    (cdr custom)
                  (list 'custom (car custom)))))
    (if current-prefix-arg
        (telega-filters-push (list fspec))
      (telega-filter-add fspec))))

(defmacro telega-filter-active ()
  "Return active filter."
  `(car telega--filters))

(defun telega-filter-active-p (filter)
  "Return non-nil if FILTER is active filter."
  (equal filter (telega-filter-active)))

(defun telega-filter-default-p (&optional filter)
  "Return non-nil if FILTER is the `telega-filter-default'.
If FILTER is nil, then active filter is used."
  (equal (or filter (telega-filter-active)) (list telega-filter-default)))


;; ewoc stuff
(defun telega-filter--pp (custom)
  "Pretty printer for CUSTOM filter button."
  (when (> (+ (current-column) telega-filter-button-width)
           telega-root-fill-column)
    (insert "\n"))
  (telega-button--insert 'telega-filter custom)
  (insert "  "))

(defun telega-filters--footer ()
  "Generate string used as root header."
  (let ((filters-width (- telega-root-fill-column 8)))
    (telega-ins--as-string
     (telega-ins "\n")
     (unless telega-root-compact-view
       (telega-ins "\n"))
     (telega-ins "----")
     (telega-ins--with-attrs (list :min filters-width
                                   :align 'center
                                   :align-symbol "-"
                                   :max filters-width
                                   :elide t
                                   :elide-trail (/ filters-width 2))
       (let* ((active-filter (telega-filter-active))
              (af-str (prin1-to-string active-filter)))
         (unless (telega-filter-default-p active-filter)
           (setq af-str (propertize af-str 'face 'telega-filter-active)))
         (telega-ins af-str)))
     (telega-ins "----"))))

(defun telega-filters--create ()
  "Create ewoc for custom filters."
  (setq telega-filters--inhibit-redisplay nil)

  ;; Footer of the `telega-filters--ewoc' is active filter at the
  ;; moment
  (setq telega-filters--ewoc
        (ewoc-create #'telega-filter--pp nil (telega-filters--footer) t))
  (dolist (custom telega-filters-custom)
    (ewoc-enter-last telega-filters--ewoc custom)))

(defun telega-filters--redisplay ()
  "Redisplay custom filters buttons."
  (unless telega-filters--inhibit-redisplay
    (with-telega-root-buffer
      (telega-save-cursor
        (telega-ewoc--set-footer
         telega-filters--ewoc (telega-filters--footer))
        (ewoc-refresh telega-filters--ewoc)))))


;;; Filtering routines
(defun telega-filters-apply (&optional no-root-redisplay)
  "Apply current filers.
If NO-ROOT-REDISPLAY is specified, then redisplay only custom
filters buttons.
Used on search results updates."
  (if telega-search-query
      (setq telega--filtered-chats
            (nconc (telega-filter-chats nil telega--search-chats)
;                   (telega-filter-chats nil (telega-root--messages-chats))
                   (let ((telega-filters--inhibit-list '(has-order)))
                     (telega-filter-chats nil (telega-root--global-chats)))))

    (setq telega--filtered-chats
          (telega-filter-chats nil telega--ordered-chats)))

  (if no-root-redisplay
      (telega-filters--redisplay)
    (telega-root--redisplay)))

(defun telega-filters--chat-update (chat)
  "CHAT has been updated, it might affect custom filters."
  (if telega-search-query
      (telega-filters-apply 'no-root-redisplay)

    ;; Fast version of what is done in `telega-filters-apply'
    (setq telega--filtered-chats
          (delq chat telega--filtered-chats))
    (when (telega-filter-chats nil (list chat))
      (setq telega--filtered-chats
            (push chat telega--filtered-chats)))
    (telega-filters--redisplay)))

(defun telega-filters--reset (&optional default)
  "Reset all filters.
Set active filter to DEFAULT."
  (setq telega--filters (when default
                          (if (listp default)
                              (list default)
                            (list (list default))))
        telega--undo-filters nil))

(defun telega-filters--prepare ()
  "Prepare `telega--filters' for the application."
  (let ((active-filters (telega-filter-active)))
    (cond ((null active-filters) 'all)
          ((= (length active-filters) 1) (car active-filters))
          ((eq 'all (car active-filters))
           (if (= 1 (length (cdr active-filters)))
               (cadr active-filters)
             active-filters))
          (t (cons 'all active-filters)))))

(defun telega-filters-push (flist)
  "Set active filters list to FLIST."
  (unless (telega-filter-active-p flist)
    (setq telega--undo-filters nil)
    (setq telega--filters (push flist telega--filters)))
  (telega-filters-apply))

(defun telega-filter-add (fspec)
  "Add filter specified by FSPEC.
This filter can be undone with `telega-filter-undo'.
Do not add FSPEC if it is already in the list."
  (unless (member fspec (telega-filter-active))
    (telega-filters-push
     (append (telega-filter-active) (list fspec)))))

(defun telega-filter-chats (filter-spec chats-list)
  "Filter CHATS-LIST matching filter specification FILTER-SPEC.
If FILTER-SPEC is nil, then currently active filters are used."
  (let ((fspec (or filter-spec (telega-filters--prepare))))
    (cl-remove-if-not
     (lambda (chat)
       ;; Filter out chats we are not member of
       ;; See https://github.com/zevlg/telega.el/issues/10
       (and (telega-filter--test chat fspec)
            (telega-filter--test chat 'has-order)))
     chats-list)))

(defun telega-filters-reset ()
  "Reset all active filters to default."
  (interactive)
  (telega-filters--reset)
  (telega-filters-push (list telega-filter-default)))

(defun telega-filter-undo (&optional arg)
  "Undo last ARG filters."
  (interactive "p")
  (unless (cdr telega--filters)
    (error "Nothing to undo"))
  (dotimes (_ arg)
    (when (cdr telega--filters)
      (push (car telega--filters) telega--undo-filters)
      (setq telega--filters (cdr telega--filters))))
  (telega-filters-apply)
  (message "Undo last filter!"))

(defun telega-filter-redo (&optional arg)
  "Redo last ARG filters."
  (interactive "p")
  (unless telega--undo-filters
    (error "Nothing to redo"))
  (dotimes (_ arg)
    (when telega--undo-filters
      (push (pop telega--undo-filters) telega--filters)))
  (telega-filters-apply)
  (message "Redo last filter!"))

(defun telega-filters-edit (flist)
  "Edit and reapply filters list."
  (interactive
   (let* ((print-level nil)
          (active-filter (telega-filter-active))
          (af-as-string (if active-filter
                            (prin1-to-string active-filter)
                          ""))
          (new-flist (read-from-minibuffer
                      "Filters: " af-as-string read-expression-map t)))
     (list new-flist)))
  (telega-filters-push flist))

(defun telega-filters-pop-last (n)
  "Pop last N filters."
  (interactive "p")
  (telega-filters-push (butlast (telega-filter-active) n)))


;;; Filters definitions
(defmacro define-telega-filter (name args &rest body)
  "Define new filter for telega chats.
ARGS specifies arguments to operation, first must always be chat."
  (let ((fsym (intern (format "telega--filter-%S" name))))
    `(defun ,fsym ,args
       ,@body)))

(defun telega-filter--get (fname)
  (if (memq fname telega-filters--inhibit-list)
    (lambda (&rest _ignored) t)

    (let ((fsym (intern (format "telega--filter-%S" fname))))
      (unless (fboundp fsym)
        (error (concat "Filter function `%S' for filter \"%s\" is undefined.\n"
                       "Use `define-telega-filter' to define new filters.")
               fsym fname))
      (symbol-function fsym))))

(defun telega-filter--test (chat fspec)
  "Return non-nil if CHAT matches filters specified by FSPEC."
  (cond ((null fspec) nil)
        ((symbolp fspec)
         (funcall (telega-filter--get fspec) chat))
        ((consp fspec)
         (apply (telega-filter--get (car fspec)) chat (cdr fspec)))
        (t (error "Invalid filter spec: %s" fspec))))

(define-telega-filter any (chat &rest flist)
  "Return non-nil if CHAT matches any of filter in FLIST."
  (cl-find chat flist :test #'telega-filter--test))
(defalias 'telega--filter-or 'telega--filter-any)

(define-telega-filter all (chat &rest flist)
  "Return non-nil if CHAT matches all filters in FLIST.
If FLIST is empty then return t."
  (not (cl-find chat flist :test-not #'telega-filter--test)))
(defalias 'telega--filter-and 'telega--filter-all)

(define-telega-filter not (chat fspec)
  "Negage filter FSPEC."
  (not (telega-filter--test chat fspec)))

(defun telega-filters-negate ()
  "Negate active filters."
  (interactive)
  (telega-filters-push (list `(not ,(telega-filters--prepare)))))

(define-telega-filter type (chat &rest ctypes)
  "Matches CHAT by its type."
  (memq (telega-chat--type chat) ctypes))

(defun telega-filter-by-type (ctype)
  "Filter chats by its type."
  (interactive
   (list (funcall telega-completing-read-function
          "Chat type: "
          (mapcar #'symbol-name telega-chat-types)
          nil t)))
  (telega-filter-add `(type ,(intern ctype))))

(define-telega-filter name (chat regexp)
  "Matches CHAT if its title matches REGEXP."
  (or (string-match regexp (telega-chat-title chat))
      (let ((info (telega-chat--info chat)))
        (or (string-match regexp (or (telega-tl-str info :first_name) ""))
            (string-match regexp (or (telega-tl-str info :last_name) ""))
            (string-match regexp (or (telega-tl-str info :username) ""))))))

(defun telega-filter-by-name (regexp)
  "Filter by REGEXP matching chat's title.
Use `telega-filter-by-name' for fuzzy searching."
  (interactive (list (read-regexp "Chat name regexp: ")))
  (telega-filter-add `(name ,regexp)))

(define-telega-filter custom (chat name)
  "Matches CHAT if custom filter with NAME matches."
  (let ((fspec (cdr (assoc name telega-filters-custom))))
    (unless fspec
      (error "No such custom filter \"%s\"" name))
    (telega-filter--test chat fspec)))

(defun telega-filter-by-custom (name)
  "Filter by custom filter."
  (interactive (list (let ((completion-ignore-case t))
                       (funcall telega-completing-read-function
                        "Custom filter: "
                        (mapcar #'car telega-filters-custom)
                        nil t))))
  (telega-filter-add `(custom ,name)))

(define-telega-filter pin (chat)
  "Matches if CHAT is pinned."
  (plist-get chat :is_pinned))

(defun telega-filter-by-pin ()
  "Filter only pinned chats."
  (interactive)
  (telega-filter-add 'pin))

(define-telega-filter unread (chat &optional n)
  "Matches CHAT with at least N unread messages.
By default N is 1.
Also matches chats marked as unread."
  (or (>= (plist-get chat :unread_count) (or n 1))
      (plist-get chat :is_marked_as_unread)))

(defun telega-filter-by-unread (n)
  "Filter chats with at least N unread messages."
  (interactive "p")
  (if (= n 1)
      (telega-filter-add 'unread)
    (telega-filter-add `(unread ,n))))

(define-telega-filter mention (chat &optional n)
  "Matches CHAT with at least N unread mentions."
  (>= (plist-get chat :unread_mention_count) (or n 1)))

(defun telega-filter-by-mention (n)
  "Filter chats with at least N unread mentions."
  (interactive "p")
  (telega-filter-add `(mention ,n)))

(define-telega-filter unmuted (chat)
  "Matches CHAT with enabled notifications."
  (not (telega-chat-muted-p chat)))

(defun telega-filter-by-unmuted ()
  "Filter chats with enabled notifications."
  (interactive)
  (telega-filter-add 'unmuted))

(define-telega-filter user-status (chat &rest valid-statuses)
  "Matches private CHATs with user status in VALID-STATUSES."
  (and (eq (telega-chat--type chat) 'private)
       (member (telega-user--seen (telega-chat--user chat)) valid-statuses)))

(defun telega-filter-by-user-status (status)
  "Filter private chats by its user STATUS."
  (interactive (let ((completion-ignore-case t))
                 (list (funcall telega-completing-read-function
                        "Member status: "
                        '("Recently" "Online" "Offline"
                          "LastWeek" "LastMonth" "Empty")
                        nil t))))
  (telega-filter-add `(user-status ,status)))

(define-telega-filter verified (chat)
  "Matches only verified chats."
  (plist-get (telega-chat--info chat) :is_verified))

(defun telega-filter-by-verified ()
  "Filter verified chats."
  (interactive)
  (telega-filter-add 'verified))

(define-telega-filter ids (chat &rest ids)
  "Matches only chats which :id is in IDS."
  (memq (plist-get chat :id) ids))

(defun telega-filter-by-created-by-me ()
  "Filter public chats created by me."
  (interactive)
  (telega-filter-add
   (cons 'ids (mapcar (telega--tl-prop :id)
                      (telega--getCreatedPublicChats)))))

(define-telega-filter me-is-member (chat)
  "Filter chats where me is valid member."
  (not (and (memq (telega-chat--type chat 'raw) '(basicgroup supergroup))
            (memq (telega--tl-type (plist-get (telega-chat--info chat) :status))
                  '(chatMemberStatusLeft chatMemberStatusBanned)))))

(define-telega-filter has-last-message (chat)
  "Filter chats which has last message."
  (plist-get chat :last_message))

(define-telega-filter has-order (chat)
  "Filter chats which non-0 order."
  (not (string= "0" (plist-get chat :order))))

(define-telega-filter opened (chat)
  "Filter chats that are opened, i.e. has corresponding chat buffer."
  (with-telega-chatbuf chat
    t))

(defun telega-filter-by-opened ()
  "Filter chats that are opened."
  (interactive)
  (telega-filter-add 'opened))

(define-telega-filter restriction (chat &rest suffixes)
  "Filter restricted chats.
Suffixes is a list of suffixes to filter on.
Suffix can be one of:
  -all      - All platforms
  -ios      - For iOS devices
  -android  - For Android devices
  -wp       - Windows?

If suffixes not specified, then match any restriction reason."
  (let ((reason (telega-tl-str (telega-chat--info chat) :restriction_reason)))
    (unless (string-empty-p reason)
      (or (not suffixes)
          (cl-find reason suffixes
                   :test (lambda (string regexp)
                           (string-match-p regexp string)))))))

(defun telega-filter-by-restriction ()
  "Filter chats by restriction reason.
To specify suffixes use `/ e' command and edit filter string directly."
  (interactive)
  (telega-filter-add 'restriction))

(define-telega-filter contact (chat relationship)
  "Filter private chats that has RELATIONSHIP contact.
RELATIONSHIP is one of `in' or `out'."
  (and (eq (telega-chat--type chat) 'private)
       (eq 'linkStateIsContact
           (telega--tl-type
            (plist-get (telega-chat--user chat)
                       (cl-ecase relationship
                         (in :incoming_link)
                         (out :outgoing_link)))))))

(defun telega-filter-by-contact (&optional incoming-p)
  "Filter chats with users that are in contacts.
By default filter contacts by outgoing link relationship.
Specify INCOMING-P to filter by incoming link relationship."
  (interactive "P")
  (telega-filter-add (list 'contact (if incoming-p 'in 'out))))

(define-telega-filter top (chat)
  "Filter if CHAT is in top usage."
  (let ((category (cl-case (telega-chat--type chat)
                    (private 'Users)
                    (bot 'Bots)
                    ((basicgroup supergroup) 'Groups)
                    (channel 'Channel))))
    (memq chat (telega-chats-top category))))

(defun telega-filter-by-top ()
  "Filter top used chats by CATEGORY."
  (interactive)
  (telega-filter-add 'top))

(define-telega-filter saved-messages (chat)
  "Matches CHAT if it is SavedMessages chat."
  (telega-me-p chat))

(defun telega-filter-by-label (label)
  "Filter chats by custom chat LABEL.
See `telega-chat-custom-label'."
  (interactive
   (list (funcall telega-completing-read-function
                  "Custom label: "
                  (let ((labels (mapcar (lambda (chat)
                                          (telega-chat-uaprop chat :label))
                                        telega--ordered-chats)))
                    (mapcar 'substring-no-properties
                            (seq-uniq (cl-remove-if-not 'stringp labels))))
                  nil t)))
  (telega-filter-add (list 'label label)))

(define-telega-filter label (chat label)
  "Matches CHAT if it is custom label is LABEL."
  (equal (telega-chat-uaprop chat :label) label))

(defun telega-filter-by-tracking ()
  "Filter chats currently tracking."
  (interactive)
  (telega-filter-add (list 'tracking)))

(define-telega-filter tracking (chat)
  "Matches CHAT if it is in tracking buffers list."
  (with-telega-chatbuf chat
    (member (buffer-name) tracking-buffers)))

(provide 'telega-filter)

;;; telega-filter.el ends here

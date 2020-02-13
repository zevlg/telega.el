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

;; * Chat Filters
;; 
;; Chat Filters are used to match chats, same as regexps are used to
;; match strings.  Chat Filters uses S-exp notation similar to ~rx~
;; package for regexps.
;;
;; Primitive Chat Filter is a specifier to match some property of the
;; chat.  Each primitive Chat Filter has name (elisp symbol) and
;; corresponding function named ~telega--filter-<FILTER-NAME>~.
;; You can specify primitive Chat Filter in either way:
;;   1. ~<FILTER-NAME>~
;;   2. ~( <FILTER-NAME> <ARG1> [<ARG2> ...] )~
;;
;; Primitive Chat Filters are combined using ~and~, ~or~ and ~not~
;; filters, forming final Chat Filter.  So Chat Filter is a logical
;; combination of other Chat Filters, down to primitive Chat Filters.
;;
;; Chat Filter examples:
;;   - all ::
;;     Matches all chats
;; 
;;   - (or saved-messages (type channel bot)) ::
;;     Matches bots/channels chats or "Saved Messages" chat
;;
;;   - (and unmuted (unread 10) (mention 1)) ::
;;     Matches unmuted chats with at least 10 unread messages and at
;;     least one message with unread mention

;;; Code:
(require 'telega-core)
(require 'telega-ins)
(require 'telega-customize)

(defvar tracking-buffers)

(declare-function telega-chat-muted-p "telega-chat"  (chat))
(declare-function telega-chat--type "telega-chat" (chat &optional no-interpret))
(declare-function telega-chat-title "telega-chat" (chat &optional with-username))
(declare-function telega-chat-label "telega-chat" (chat))
(declare-function telega-chat--info "telega-chat" (chat))
(declare-function telega-chat--user "telega-chat" (chat))
(declare-function telega-chat-user "telega-chat" (chat &optional include-bots-p))
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
    (define-key map (kbd "/") 'telega-filters-reset)
    (define-key map (kbd ":") 'telega-filters-edit)
    (define-key map (kbd "!") 'telega-filters-negate)
    (define-key map (kbd "SPC") 'telega-filter-by-tracking)
    ;; `a' mnemominc is to "add" some filter
    (define-key map (kbd "a") 'telega-filter-by-filter)
    (define-key map (kbd "b") 'telega-filter-by-has-chatbuf)
    (define-key map (kbd "c") 'telega-filter-by-contact)
    (define-key map (kbd "C") 'telega-filter-by-custom)
    (define-key map (kbd "d") 'telega-filters-pop-last)
    (define-key map (kbd "DEL") 'telega-filters-pop-last)
    (define-key map (kbd "e") 'telega-filters-edit)
    (define-key map (kbd "f") 'telega-filter-by-filter)
    (define-key map (kbd "i") 'telega-filter-by-important)
    (define-key map (kbd "n") 'telega-filter-by-search)
    (define-key map (kbd "l") 'telega-filter-by-label)
    (define-key map (kbd "m") 'telega-filter-by-mention)
    (define-key map (kbd "o") 'telega-filter-by-online-status)
    (define-key map (kbd "p") 'telega-filter-by-pin)
    (define-key map (kbd "r") 'telega-filter-by-restriction)
    (define-key map (kbd "s") 'telega-filter-by-search)
    (define-key map (kbd "t") 'telega-filter-by-type)
    (define-key map (kbd "T") 'telega-filter-by-top)
    (define-key map (kbd "u") 'telega-filter-by-unread)
    (define-key map (kbd "v") 'telega-filter-by-verified)
    (define-key map (kbd "y") 'telega-filter-by-unmuted)
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
    (if (or current-prefix-arg
            (member (car custom) telega-filter-custom-push-list))
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

(defun telega-filter-active-chat-list-name ()
  "Return name of the chat list according to active filter.
Return one of \"Main\" or \"Archive\"."
  (let ((ff (car (telega-filter-active))))
    (cond ((or (eq ff 'archive)
               (equal ff '(chat-list "Archive")))
           "Archive")
          ((or (eq ff 'main)
               (equal ff '(chat-list "Main")))
           "Main")
          (t "Main"))))

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
     (telega-ins "-/--")
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
     (telega-ins "----")

     (when telega--sort-criteria
       (telega-ins "\n")
       (telega-ins "-\\--")
       (telega-ins--with-attrs (list :min filters-width
                                     :align 'center
                                     :align-symbol "-"
                                     :max filters-width
                                     :elide t
                                     :elide-trail (/ filters-width 2))
         (telega-ins--with-face 'bold
           (when telega--sort-inverted
             (telega-ins "(inverted "))
           (telega-ins-fmt "%S" telega--sort-criteria)
           (when telega--sort-inverted
             (telega-ins ")"))))
       (telega-ins "----"))
     )))

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
            (nconc (telega-filter-chats telega--search-chats)
;                   (telega-filter-chats (telega-root--messages-chats))
                   (let ((telega-filters--inhibit-list '(has-order chat-list main archive)))
                     (telega-filter-chats (telega-root--global-chats)))))

    (setq telega--filtered-chats
          (telega-filter-chats telega--ordered-chats)))

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
    (when (telega-filter-chats (list chat))
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

(defun telega-filter-chats (chat-list &optional chat-filter)
  "Match chats in CHAT-LIST against CHAT-FILTER.
Return list of chats that matches CHAT-FILTER.
Return only chats with non-0 order.
If CHAT-FILTER is ommited, then active filters from
`telega--filters' is used as CHAT-FILTER."
  (unless chat-filter
    (setq chat-filter (telega-filters--prepare)))

  (cl-remove-if-not
   (lambda (chat)
     ;; Filter out chats we are not member of
     ;; See https://github.com/zevlg/telega.el/issues/10
     (and (telega-chat-match-p chat chat-filter)
          (telega-chat-match-p chat 'has-order)))
   chat-list))

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

;; ** List of chat filters
(defmacro define-telega-filter (name args &rest body)
  "Define new filter for telega chats.
ARGS specifies arguments to operation, first must always be chat."
  (let ((fsym (intern (format "telega--filter-%S" name))))
    `(defun ,fsym ,args
       ,@body)))

(defun telega-filter--get (filter-name)
  "Return function corresponding to primitive Chat Filter named FILTER-NAME."
  (if (memq filter-name telega-filters--inhibit-list)
      (lambda (&rest _ignored) t)

    (let ((fsym (intern (format "telega--filter-%S" filter-name))))
      (unless (fboundp fsym)
        (error (concat "Filter function `%S' for filter \"%s\" is undefined.\n"
                       "Use `define-telega-filter' to define new filters.")
               fsym filter-name))
      (symbol-function fsym))))

(defun telega-chat-match-p (chat chat-filter)
  "Return non-nil if CHAT-FILTER matches CHAT."
  (cond ((null chat-filter) nil)
        ((symbolp chat-filter)
         (funcall (telega-filter--get chat-filter) chat))
        ((consp chat-filter)
         (apply (telega-filter--get (car chat-filter)) chat (cdr chat-filter)))
        (t (error "Invalid Chat Filter: %S" chat-filter))))

(defun telega-filter-by-filter (filter-name)
  "Filter by some filter."
  ;; Query user for filter with corresponding interactive function, or
  ;; filter without argument
  (interactive
   (list
    (let* ((filter-funs
            (cl-remove-if
             (lambda (funsym)
               (> (length (help-function-arglist funsym)) 1))
             (apropos-internal "^telega--filter-[a-z-]+" 'functionp)))
           (filter-names
            (mapcar (lambda (funame)
                      (substring funame (length "telega--filter-")))
                    (mapcar 'symbol-name filter-funs)))
           (i-filter-names
            (mapcar (lambda (funsym)
                      (substring (symbol-name funsym)
                                 (length "telega-filter-by-")))
                    (apropos-internal "^telega-filter-by-[a-z-]+" 'functionp))))
      (funcall telega-completing-read-function
               "Chat Filter: "
               (seq-uniq (nconc filter-names i-filter-names)) nil t))))

  (let ((i-filter-fun-sym (intern (concat "telega-filter-by-" filter-name))))
    (if (fboundp i-filter-fun-sym)
        (call-interactively i-filter-fun-sym)

      (telega-filter-add (intern filter-name)))))

;; - (any ~FILTER-LIST~...) ::
;;   {{{fundoc(telega--filter-any)}}}
(define-telega-filter any (chat &rest filter-list)
  "Matches if any filter in FILTER-LIST matches."
  (cl-find chat filter-list :test #'telega-chat-match-p))
;; - (or ~FILTER-LIST~...) ::
;;   Same as ~any~
(defalias 'telega--filter-or 'telega--filter-any)

;; - (all ~FILTER-LIST~...) ::
;;   {{{fundoc(telega--filter-all)}}}
(define-telega-filter all (chat &rest filter-list)
  "Matches if all filters in FILTER-LIST matches.
Also matches if FILTER-LIST is empty."
  (not (cl-find chat filter-list :test-not #'telega-chat-match-p)))

;; - (and ~FILTER-LIST~...) ::
;;   Same as ~all~
(defalias 'telega--filter-and 'telega--filter-all)

;; - (not ~FILTER~) ::
;;   {{{fundoc(telega--filter-not)}}}
;; 
;;   + Key (rootbuf): {{{where-is(telega-filters-negate,telega-root-mode-map)}}}
;;     {{{fundoc(telega-filters-negate)}}}
(define-telega-filter not (chat filter)
  "Matches if FILTER not maches."
  (not (telega-chat-match-p chat filter)))

(defun telega-filters-negate ()
  "Negate active filters."
  (interactive)
  (telega-filters-push (list `(not ,(telega-filters--prepare)))))

;; - (type ~CHAT-TYPE-LIST~) ::
;;   {{{fundoc(telega--filter-type)}}}
;; 
;;   See [[#chat-types][Chat types]]
;; 
;;   + Key (rootbuf): {{{where-is(telega-filter-by-type,telega-root-mode-map)}}}
;;     {{{fundoc(telega-filter-by-type)}}}
(define-telega-filter type (chat &rest chat-type-list)
  "Matches if chat type is one of CHAT-TYPE-LIST."
  (memq (telega-chat--type chat) chat-type-list))

(defun telega-filter-by-type (ctype)
  "Filter chats by its type."
  (interactive
   (list (funcall telega-completing-read-function
          "Chat type: "
          (mapcar #'symbol-name telega-chat-types)
          nil t)))
  (telega-filter-add `(type ,(intern ctype))))

;; - (name ~REGEXP~) ::
;;   {{{fundoc(telega--filter-name)}}}
(define-telega-filter name (chat regexp)
  "Matches if chat's title matches REGEXP."
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

;; - (search ~QUERY~) ::
;;   {{{fundoc(telega--filter-search)}}}
(define-telega-filter search (chat _query)
  "Matches if chat maches search QUERY."
  (memq chat telega--search-chats))

(defun telega-filter-by-search (query)
  "Filter chats by QUERY."
  (interactive (list (read-string "Chat search query: ")))
  (setq telega--search-chats (telega--searchChats query))
  (telega-filter-add `(search ,query)))

;; - (custom ~NAME~) ::
;;   {{{fundoc(telega--filter-custom)}}}
(define-telega-filter custom (chat name)
  "Matches if custom filter with NAME matches."
  (let ((chat-filter (cdr (assoc name telega-filters-custom))))
    (unless chat-filter
      (error "No such custom chat filter \"%s\"" name))
    (telega-chat-match-p chat chat-filter)))

(defun telega-filter-by-custom (name)
  "Filter by custom filter."
  (interactive (list (let ((completion-ignore-case t))
                       (funcall telega-completing-read-function
                        "Custom filter: "
                        (mapcar #'car telega-filters-custom)
                        nil t))))
  (telega-filter-add `(custom ,name)))

;; - pin ::
;;   {{{fundoc(telega--filter-pin)}}}
(define-telega-filter pin (chat)
  "Matches if chat is pinned."
  (plist-get chat :is_pinned))

(defun telega-filter-by-pin ()
  "Filter only pinned chats."
  (interactive)
  (telega-filter-add 'pin))

;; - has-pinned-message ::
;;   {{{fundoc(telega--filter-has-pinned-message)}}}
(define-telega-filter has-pinned-message (chat)
  "Matches if chat has pinned message."
  (not (zerop (plist-get chat :pinned_message_id))))

(defun telega-filter-by-pinned-message ()
  "Filter only chats with pinned message."
  (interactive)
  (telega-filter-add 'has-pinned-message))

;; - (unread [ ~N~ ]) ::
;;   {{{fundoc(telega--filter-unread)}}}
(define-telega-filter unread (chat &optional n)
  "Matches if chat has least N unread messages.
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

;; - (mention [ ~N~ ]) ::
;;   {{{fundoc(telega--filter-mention)}}}
(define-telega-filter mention (chat &optional n)
  "Matches if chat has least N unread mentions.
By default N is 1."
  (>= (plist-get chat :unread_mention_count) (or n 1)))

(defun telega-filter-by-mention (n)
  "Filter chats with at least N unread mentions."
  (interactive "p")
  (telega-filter-add `(mention ,n)))

;; - unmuted ::
;;   {{{fundoc(telega--filter-unmuted)}}}
(define-telega-filter unmuted (chat)
  "Matches if chat has enabled notifications."
  (not (telega-chat-muted-p chat)))

(defun telega-filter-by-unmuted ()
  "Filter chats with enabled notifications."
  (interactive)
  (telega-filter-add 'unmuted))

(defun telega-filter-by-important ()
  "Filter important chats.
Important chat is the chat with unread messages and enabled notifications."
  (interactive)
  (telega-filter-add '(and unread unmuted)))

;; - (online-status ~STATUS-LIST~...) ::
;;   {{{fundoc(telega--filter-online-status)}}}
;; 
;;   Each element in ~STATUS-LIST~ is one of: "Online", "Offline",
;;   "Recently", "LastWeek", "LastMonth" or "Empty"
(define-telega-filter online-status (chat &rest status-list)
  "Matches private chat where user status is one of STATUS-LIST."
  (when-let ((user (telega-chat-user chat)))
    (member (telega-user--seen user) status-list)))

(defun telega-filter-by-online-status (status)
  "Filter private chats by its user online STATUS."
  (interactive (let ((completion-ignore-case t))
                 (list (funcall telega-completing-read-function
                        "Member status: "
                        '("Recently" "Online" "Offline"
                          "LastWeek" "LastMonth" "Empty")
                        nil t))))
  (telega-filter-add `(online-status ,status)))

;; - verified ::
;;   {{{fundoc(telega--filter-verified)}}}
(define-telega-filter verified (chat)
  "Matches if chat is verified."
  (plist-get (telega-chat--info chat) :is_verified))

(defun telega-filter-by-verified ()
  "Filter verified chats."
  (interactive)
  (telega-filter-add 'verified))

;; - (ids ~ID-LIST~...) ::
;;   {{{fundoc(telega--filter-ids)}}}
(define-telega-filter ids (chat &rest id-list)
  "Matches if chat's id is one of in ID-LIST."
  (memq (plist-get chat :id) id-list))

(defun telega-filter-by-created-by-me ()
  "Filter public chats created by me."
  (interactive)
  (telega-filter-add
   (cons 'ids (mapcar (telega--tl-prop :id)
                      (telega--getCreatedPublicChats)))))

;; - me-is-member ::
;;   {{{fundoc(telega--filter-me-is-member)}}}
(define-telega-filter me-is-member (chat)
  "Matches if me is member of the chat."
  (not (and (memq (telega-chat--type chat 'raw) '(basicgroup supergroup))
            (memq (telega--tl-type (plist-get (telega-chat--info chat) :status))
                  '(chatMemberStatusLeft chatMemberStatusBanned)))))

;; - has-last-message ::
;;   {{{fundoc(telega--filter-has-last-message)}}}
(define-telega-filter has-last-message (chat)
  "Matches if chat has last message."
  (plist-get chat :last_message))

;; - has-order ::
;;   {{{fundoc(telega--filter-has-order)}}}
;; 
;;   Only chats with non-0 order are listed in rootbuf.  I.e. this
;;   filter is implicitly applied along with active chat filter.
(define-telega-filter has-order (chat)
  "Matches if chat has non-0 order."
  (not (string= "0" (plist-get chat :order))))

;; - has-avatar ::
;;   {{{fundoc(telega--filter-has-avatar)}}}
(define-telega-filter has-avatar (chat)
  "Matches if chat has chat photo."
  (plist-get chat :photo))

;; - has-chatbuf ::
;;   {{{fundoc(telega--filter-has-chatbuf)}}}
(define-telega-filter has-chatbuf (chat)
  "Matches if chat has corresponding chatbuf."
  (with-telega-chatbuf chat
    t))

(defun telega-filter-by-has-chatbuf ()
  "Filter chats that has corresponding chat buffer."
  (interactive)
  (telega-filter-add 'has-chatbuf))

;; - (permission ~PERM~) ::
;;   {{{fundoc(telega--filter-permission)}}}
(define-telega-filter permission (chat perm)
  "Matches if chat has PERM set in chat permissions.
PERM could be one of:
`:can_send_messages', `:can_send_media_messages', `:can_send_polls',
`:can_send_other_messages', `:can_add_web_page_previews',
`:can_change_info', `:can_invite_users', `:can_pin_messages'"
  (plist-get (plist-get chat :permissions) perm))

(defun telega-filter-by-permission (perm)
  "Filter chats by allowed permission PERM."
  (interactive
   (let ((str-perm (funcall telega-completing-read-function
                            "Chat permission: "
                            '("can_send_messages"
                              "can_send_media_messages"
                              "can_send_polls"
                              "can_send_other_messages"
                              "can_add_web_page_previews"
                              "can_change_info"
                              "can_invite_users"
                              "can_pin_messages")
                            nil t)))
     (list (intern (concat ":" str-perm)))))
  (telega-filter-add (list 'permission perm)))

;; - (restriction ~SUFFIX-LIST~...) ::
;;   {{{fundoc1(telega--filter-restriction)}}}
;; 
;;   Each element in ~SUFFIX-LIST~ is one of:
;;   + "-all" - Restricted on all platforms
;;   + "-ios" - Restricted for iOS devices
;;   + "-android" - Restricted for Android devices
;;   + "-wp" - Restricted on Windows
;; 
;;   If ~SUFFIX-LIST~ is ommited, then match any reason.
(define-telega-filter restriction (chat &rest suffix-list)
  "Matches restricted chats.
SUFFIX-LIST is a list of suffixes to filter on.
Suffix can be one of:
  -all      - All platforms
  -ios      - For iOS devices
  -android  - For Android devices
  -wp       - Windows?

If SUFFIX-LIST is not specified, then match any restriction reason."
  (when-let ((reason (telega-tl-str
                      (telega-chat--info chat) :restriction_reason)))
    (or (not suffix-list)
        (cl-find reason suffix-list
                 :test (lambda (string regexp)
                         (string-match-p regexp string))))))

(defun telega-filter-by-restriction ()
  "Filter chats by restriction reason.
To specify suffixes use `/ e' command and edit filter string directly."
  (interactive)
  (telega-filter-add 'restriction))

;; - (contact [ ~MUTUAL-P~ ]) ::
;;   {{{fundoc(telega--filter-contact)}}}
(define-telega-filter contact (chat &optional mutual-p)
  "Matches private chats if corresponding user is a contact.
If MUTUAL-P is non-nil, then mach only if contact is mutual."
  (and (eq (telega-chat--type chat) 'private)
       (plist-get (telega-chat--user chat)
                  (if mutual-p
                      :is_mutual_contact
                    :is_contact))))

(defun telega-filter-by-contact (&optional mutual-p)
  "Filter chats with users that are in contacts.
Specify MUTUAL-P to filter only mutual contacts."
  (interactive "P")
  (telega-filter-add (if mutual-p
                         (list 'contact 'mutual)
                       'contact)))

;; - top ::
;;   {{{fundoc(telega--filter-top)}}}
(define-telega-filter top (chat)
  "Matches if chat is in top usage."
  (let ((category (cl-case (telega-chat--type chat)
                    (private 'Users)
                    (bot 'Bots)
                    ((basicgroup supergroup) 'Groups)
                    (channel 'Channels))))
    (memq chat (telega-chats-top category))))

(defun telega-filter-by-top ()
  "Filter top used chats by CATEGORY."
  (interactive)
  (telega-filter-add 'top))

;; - saved-messages ::
;;   {{{fundoc(telega--filter-saved-messages)}}}
(define-telega-filter saved-messages (chat)
  "Matches only SavedMessages chat."
  (telega-me-p chat))

(defun telega-filter-by-label (label)
  "Filter chats by custom chat LABEL.
See `telega-chat-set-custom-label'."
  (interactive (list (funcall telega-completing-read-function
                              "Custom label: "
                              (telega-custom-labels 'no-props)
                              nil t)))
  (telega-filter-add (list 'label label)))

;; - (label ~LABEL~) ::
;;   {{{fundoc1(telega--filter-label)}}}
(define-telega-filter label (chat label)
  "Matches chat with custom LABEL."
  (equal (telega-chat-label chat) label))

;; - tracking ::
;;   {{{fundoc1(telega--filter-tracking)}}}
(define-telega-filter tracking (chat)
  "Matches if chat is in tracking buffers list."
  (with-telega-chatbuf chat
    (member (buffer-name) tracking-buffers)))

(defun telega-filter-by-tracking ()
  "Matches if chat is in tracking buffers list."
  (interactive)
  (telega-filter-add (list 'tracking)))

;; - last-message-by-me ::
;;   {{{fundoc1(telega--filter-last-message-by-me)}}}
(define-telega-filter last-message-by-me (chat)
  "Matches if chat's last message sent by me."
  (when-let ((last-msg (plist-get chat :last_message)))
    (eq (plist-get last-msg :sender_user_id) telega--me-id)))

;; - (chat-list ~LIST-NAME~) ::
;;   {{{fundoc(telega--filter-chat-list)}}}
(define-telega-filter chat-list (chat list-name)
  "Matches if chat is in chat list named LIST-NAME.
Only \"Main\" and \"Archive\" names are supported."
  (equal (plist-get (plist-get chat :chat_list) :@type)
         (concat "chatList" (capitalize list-name))))

;; - main ::
;;   {{{fundoc(telega--filter-main)}}}
(define-telega-filter main (chat)
  "Matches if chat from \"Main\" chat list."
  (telega-chat-match-p chat '(chat-list "Main")))

;; - archive ::
;;   {{{fundoc(telega--filter-archive)}}}
(define-telega-filter archive (chat)
  "Matchis if chat is archived, i.e. in \"Archive\" chat list."
  (telega-chat-match-p chat '(chat-list "Archive")))

;; - has-scheduled-messages ::
;;   {{{fundoc(telega--filter-has-scheduled-messages)}}}
(define-telega-filter has-scheduled-messages (chat)
  "Matches if chat has scheduled messages."
  (plist-get chat :has_scheduled_messages))

(define-telega-filter has-action-bar (chat)
  "Matches CHAT with active action bar."
  (plist-get chat :action_bar))

(provide 'telega-filter)

;;; telega-filter.el ends here

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

;;; ellit-org: commentary
;;
;; Chat Filters are used to match chats, same as regexps are used to
;; match strings.  Chat Filters uses S-exp notation similar to ~rx~
;; package for regexps.  Consider Chat Filters as extremely powerful
;; "Folders" functionality in official client.
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
;;   - ~all~ ::
;;     Matches all chats
;;
;;   - ~(or saved-messages (type channel bot))~ ::
;;     Matches bots/channels chats or "Saved Messages" chat
;;
;;   - ~(and unmuted (unread 10) (mention 1))~ ::
;;     Matches unmuted chats with at least 10 unread messages and at
;;     least one message with unread mention
;;
;; Matching is done using ~telega-chat-match-p~ function.

;;; Code:
(require 'telega-core)
(require 'telega-ins)
(require 'telega-customize)

(defvar tracking-buffers nil)
(defvar telega-root--view-filter)

(declare-function telega-chat-muted-p "telega-chat"  (chat))
(declare-function telega-chat--type "telega-chat" (chat &optional no-interpret))
(declare-function telega-chat-channel-p "telega-chat" (chat))
(declare-function telega-chat-title "telega-chat" (chat &optional with-username))
(declare-function telega-chat--info "telega-chat" (chat))
(declare-function telega-chat--user "telega-chat" (chat))
(declare-function telega-chat-user "telega-chat" (chat &optional include-bots-p))
(declare-function telega-chats-top "telega-chat" (category))

(declare-function telega-root--buffer "telega-root")
(declare-function telega-root-view--redisplay "telega-root")


(defvar telega-filters--ewoc nil "ewoc for custom filters.")
(defvar telega-filters--dirty nil
  "Non-nil if filter's ewoc is dirty and need to be redisplayed.
Could be a list of custom filters marked dirty.
If t, then all custom filters are dirty.")

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
    (define-key map (kbd "f") 'telega-filter-by-folder)
    (define-key map (kbd "i") 'telega-filter-by-important)
    (define-key map (kbd "n") 'telega-filter-by-nearby)
    (define-key map (kbd "l") 'telega-filter-by-label)
    (define-key map (kbd "m") 'telega-filter-by-mention)
    (define-key map (kbd "o") 'telega-filter-by-online-status)
    (define-key map (kbd "^") 'telega-filter-by-pin)
    (define-key map (kbd "P") 'telega-filter-by-pin)
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
                       (nth 0 custom) (nth 1 custom)))
  'action #'telega-filter-button--action)

(defun telega-ins--filter (custom-spec)
  "Inserter for the custom filter button specified by CUSTOM-SPEC.
See `telega-filter--ewoc-spec' for CUSTOM-SPEC description."
  (let* ((name (nth 0 custom-spec))
         (chats (nthcdr 2 custom-spec))
         (active-p (not (null chats)))
         (nchats (length chats))
         (unread (apply #'+ (mapcar (telega--tl-prop :unread_count) chats)))
         (mentions (apply #'+ (mapcar
                               (telega--tl-prop :unread_mention_count) chats)))
         (umwidth 7)
         (title-width (- telega-filter-button-width umwidth)))
    (telega-ins--with-props (list 'inactive (not active-p)
                                  'action (if active-p
                                              #'telega-filter-button--action
                                            'ignore))
      (telega-ins--with-face (if active-p
                                 'telega-filter-button-active
                               'telega-filter-button-inactive)
        (telega-ins "[")
        (telega-ins--with-attrs (list :min title-width
                                      :max title-width
                                      :elide t
                                      :align 'left)
          (telega-ins (number-to-string nchats) ":")
          (if (telega-filter--custom-active-p custom-spec)
              (telega-ins--with-face 'bold
                (telega-ins name))
            (telega-ins name)))
        (telega-ins--with-attrs (list :min umwidth
                                      :max umwidth
                                      :elide t
                                      :align 'right)
          (unless (zerop unread)
            (telega-ins-fmt "%d" unread))
          (unless (zerop mentions)
            (telega-ins-fmt "@%d" mentions)))
        (telega-ins "]")))))

(defun telega-filter-button--action (button)
  "Action to take when custom filter button is pressed.
If prefix ARG is specified then set custom filter as active,
otherwise toggle custom filter in active chat filters."
  (let* ((custom (button-get button :value))
         (fspec (if (or telega-filter-custom-expand
                        (telega-filter--custom-folder-p custom))
                    (nth 1 custom)
                  (list 'custom (nth 0 custom)))))
    (if (or current-prefix-arg
            (telega-filter--custom-folder-p custom))
        (telega-filters-push (list fspec))

      (if-let ((active-cf (telega-filter--custom-active-p custom)))
          (telega-filters-push (delete active-cf (telega-filter-active)))
        (telega-filter-add fspec)))))

(defun telega-filter-active (&optional with-root-view-filter)
  "Return active filter.
If WITH-ROOT-VIEW-FILTER is non-nil, then add root view filter."
  (let ((filter (car telega--filters)))
    (when (and with-root-view-filter telega-root--view-filter)
      (setq filter (list 'and filter telega-root--view-filter)))
    filter))

(defun telega-filter-active-prepare (&optional with-root-view-filter)
  "Prepare `telega--filters' for the application.
WITH-ROOT-VIEW-FILTER is passed directly to `telega-filter-active'.
Return chat filter prepared for the application."
  (let ((active-filters (telega-filter-active with-root-view-filter)))
    (cond ((null active-filters) 'all)
          ((= (length active-filters) 1) (car active-filters))
          ((eq 'all (car active-filters))
           (if (= 1 (length (cdr active-filters)))
               (cadr active-filters)
             active-filters))
          (t (cons 'all active-filters)))))

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

(defun telega-filter--folder-tdlib-chat-list (folder-name)
  "Return tdlib chat list for folder with FOLDER-NAME.
Return nil if folder with FOLDER-NAME is not known by TDLib."
  (or (cdr (assoc folder-name telega-filter-custom-folders))
      (progn
        ;; Strip off folder symbol, in case FOLDER-NAME from custom
        ;; filter spec is used
        (when-let ((real-folder-name
                    (get-text-property 0 'telega-folder folder-name)))
          (setq folder-name real-folder-name))
        (when-let ((filter-info (cl-find folder-name telega-tdlib--chat-filters
                                         :key (lambda (fi)
                                                (telega-tl-str fi :title))
                                         :test #'equal)))
          (list :@type "chatListFilter"
                :chat_filter_id (plist-get filter-info :id))))))

(defun telega-filter--custom-active-p (custom)
  "Return non-nil if CUSTOM filter is in active chat filter.
Actually return active chat filter corresponding to CUSTOM filter."
  (car (or (member (nth 1 custom) (telega-filter-active))
           (member (list 'custom (car custom)) (telega-filter-active)))))

(defun telega-filter--custom-folder-p (custom)
  "Return non-nil if CUSTOM filter is telegram folder."
  (or (get-text-property 0 'telega-folder (car custom))
      (assoc (car custom) telega-filter-custom-folders)))

(defun telega-filter--custom-folder-spec (tdlib-chat-filter)
  "Return custom filter spec for the TDLIB-CHAT-FILTER folder."
  (let ((fn (telega-tl-str tdlib-chat-filter :title)))
    (cons (telega-folder-format "%i%f" fn tdlib-chat-filter)
          (list 'folder (substring-no-properties fn)))))

(defun telega-filter--custom-chats (custom)
  "Return chats matching CUSTOM filter."
  (telega-filter-chats
   (if (telega-filter--custom-folder-p custom)
       telega--ordered-chats
     telega--filtered-chats)
   (cdr custom)))

(defun telega-filters--refresh ()
  "Refresh `telega-filters--ewoc' contents.
Used when `updateChatFilters' is received."
  (telega-ewoc--clean telega-filters--ewoc)

  (dolist (custom (append telega-filters-custom
                          (when telega-filter-custom-show-folders
                            (mapcar #'telega-filter--custom-folder-spec
                                    telega-tdlib--chat-filters))))
    (ewoc-enter-last telega-filters--ewoc
                     (cons (car custom)
                           (cons (cdr custom)
                                 (telega-filter--custom-chats custom)))))
  )

(defun telega-filters--create ()
  "Create ewoc for custom filters."
  ;; Footer of the `telega-filters--ewoc' is active filter at the
  ;; moment
  (setq telega-filters--ewoc
        (ewoc-create #'telega-filter--pp nil (telega-filters--footer) t))
  (telega-filters--refresh))

(defun telega-filters--mark-dirty (custom-spec)
  "Mark CUSTOM filter button as dirty."
  (when (listp telega-filters--dirty)
    (setq telega-filters--dirty
          (cl-pushnew (car custom-spec) telega-filters--dirty :test #'equal))))

(defun telega-filters--redisplay-footer ()
  "Redisplay custom filters footer.
Used when active sort criteria changes."
  (with-telega-root-buffer
    (save-excursion
      (telega-ewoc--set-footer
       telega-filters--ewoc (telega-filters--footer)))))

(defun telega-filters--redisplay ()
  "Redisplay custom filters buttons."
  (when telega-filters--dirty
    (with-telega-root-buffer
      (telega-save-cursor
        ;; Redisplay footer only on full redisplay
        (when (eq telega-filters--dirty t)
          (telega-ewoc--set-footer telega-filters--ewoc
                                   (telega-filters--footer)))

        ;; Refresh only dirty nodes
        (telega-ewoc-map-refresh telega-filters--ewoc
          (lambda (custom)
            (or (eq telega-filters--dirty t)
                (member (car custom) telega-filters--dirty)))))

      (run-hooks 'telega-root-update-hook))

    (setq telega-filters--dirty nil)))


;;; Filtering routines
(defun telega-filter-active-tdlib-chat-list ()
  "Return tdlib chat list etracting it from active chat filter."
  (let ((cl-fspec nil))
    (dolist (fspec (car telega--filters))
      (when (or (memq fspec '(main archive))
                (and (consp fspec)
                     (memq (car fspec) '(chat-list folder))))
        (setq cl-fspec fspec)))
    (unless cl-fspec
      (setq cl-fspec 'main))

    (cond ((eq cl-fspec 'main) '(:@type "chatListMain"))
          ((eq cl-fspec 'archive) '(:@type "chatListArchive"))
          (t (telega-filter--folder-tdlib-chat-list (cadr cl-fspec))))))

(defun telega-filters--update ()
  "Update `telega--filtered-chats'."
  ;; If `telega-tdlib--chat-list' changes, then chats resort is
  ;; required, because chat order may be different in different tdlib
  ;; chat lists
  (let ((new-tdlib-chat-list (telega-filter-active-tdlib-chat-list)))
    (unless (equal telega-tdlib--chat-list new-tdlib-chat-list)
      (setq telega-tdlib--chat-list new-tdlib-chat-list)
      (setq telega--ordered-chats
            (sort telega--ordered-chats #'telega-chat>))))

  (setq telega--filtered-chats
        (telega-filter-chats telega--ordered-chats))

  (dolist (ewoc-spec (ewoc-collect telega-filters--ewoc #'identity))
    (setcdr (cdr ewoc-spec)
            (telega-filter--custom-chats
             (cons (car ewoc-spec) (cadr ewoc-spec)))))

  (setq telega-filters--dirty t))

(defun telega-filters--chat-update (chat)
  "CHAT has been updated, it might affect custom filters."
  ;; Fast version of what is done in `telega-filters--update'
  (let ((chat-matches-p nil))
    (setq telega--filtered-chats
          (delq chat telega--filtered-chats))
    (when (telega-filter-chats (list chat))
      (setq chat-matches-p t)
      (setq telega--filtered-chats
            (push chat telega--filtered-chats)))

    ;; Remove/Add chat in ewoc specs for custom filters
    (dolist (ewoc-spec (ewoc-collect telega-filters--ewoc #'identity))
      (let* ((match-p (and (or (telega-filter--custom-folder-p ewoc-spec)
                               chat-matches-p)
                           (telega-filter-chats (list chat) (nth 1 ewoc-spec))))
             (chats (nthcdr 2 ewoc-spec))
             (has-chat-p (memq chat chats)))
        (when (or has-chat-p match-p)
          (telega-filters--mark-dirty ewoc-spec))
        (if match-p
            (unless has-chat-p
              (setcdr (cdr ewoc-spec) (cons chat chats)))
          (when has-chat-p
            (setcdr (cdr ewoc-spec) (delq chat chats))))))
    ))

(defun telega-filters--reset (&optional default)
  "Reset all filters.
Set active filter to DEFAULT."
  (setq telega--filters (when default
                          (if (listp default)
                              (list default)
                            (list (list default))))
        telega--undo-filters nil))

(defun telega-filters-push (flist)
  "Set active filters list to FLIST."
  (unless (telega-filter-active-p flist)
    (setq telega--undo-filters nil)
    (setq telega--filters (push flist telega--filters)))
  (telega-filters--update)
  (telega-filters--redisplay)
  (telega-root-view--redisplay))

(defun telega-filter-add (fspec)
  "Add filter specified by FSPEC.
This filter can be undone with `telega-filter-undo'.
Do not add FSPEC if it is already in the list."
  (unless (member fspec (telega-filter-active))
    ;; TODO: can do it faster, just by filtering
    ;; `telega--filtered-chats' with new filter spec
    (telega-filters-push
     (append (telega-filter-active) (list fspec)))))

(defun telega-filter-chats (chat-list &optional chat-filter)
  "Match chats in CHAT-LIST against CHAT-FILTER.
Return list of chats that matches CHAT-FILTER.
Return only chats with non-0 order.
If CHAT-FILTER is ommited, then active filter from
`telega--filters' is used as CHAT-FILTER."
  (unless chat-filter
    (setq chat-filter (telega-filter-active-prepare 'with-root-view)))

  (cl-remove-if-not
   (lambda (chat)
     (telega-chat-match-p chat chat-filter))
   chat-list))

(defun telega-filters-reset ()
  "Reset active filter to the `telega-filter-default'."
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
  (telega-filters--update)
  (telega-filters--redisplay)
  (telega-root-view--redisplay)
  (message "Undo last filter!"))

(defun telega-filter-redo (&optional arg)
  "Redo last ARG filters."
  (interactive "p")
  (unless telega--undo-filters
    (error "Nothing to redo"))
  (dotimes (_ arg)
    (when telega--undo-filters
      (push (pop telega--undo-filters) telega--filters)))
  (telega-filters--update)
  (telega-filters--redisplay)
  (telega-root-view--redisplay)
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
         ;; NOTE: support for lambda function as chat-filter
         (if (functionp chat-filter)
             (funcall chat-filter chat)
           (apply (telega-filter--get (car chat-filter))
                  chat (cdr chat-filter))))
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

;;; ellit-org: chat-filters
;; - (any ~FILTER-LIST~...) ::
;;   {{{fundoc(telega--filter-any, 2)}}}
(define-telega-filter any (chat &rest filter-list)
  "Matches if any filter in FILTER-LIST matches."
  (cl-find chat filter-list :test #'telega-chat-match-p))
;;; ellit-org: chat-filters
;; - (or ~FILTER-LIST~...) ::
;;   Same as ~any~
(defalias 'telega--filter-or 'telega--filter-any)

;;; ellit-org: chat-filters
;; - (all ~FILTER-LIST~...) ::
;;   {{{fundoc(telega--filter-all, 2)}}}
(define-telega-filter all (chat &rest filter-list)
  "Matches if all filters in FILTER-LIST matches.
Also matches if FILTER-LIST is empty."
  (not (cl-find chat filter-list :test-not #'telega-chat-match-p)))

;;; ellit-org: chat-filters
;; - (and ~FILTER-LIST~...) ::
;;   Same as ~all~
(defalias 'telega--filter-and 'telega--filter-all)

;;; ellit-org: chat-filters
;; - (not ~FILTER~) ::
;;   {{{fundoc(telega--filter-not, 2)}}}
(define-telega-filter not (chat filter)
  "Matches if FILTER not maches."
  (not (telega-chat-match-p chat filter)))

(defun telega-filters-negate (arg)
  "Negate last filter.
If `\\[universal-argument]' is specified, then negate whole active filter."
  (interactive "P")
  (telega-filters-push
   (if arg
       (list `(not ,(telega-filter-active-prepare)))
     (append (butlast (telega-filter-active))
             (list `(not ,(car (last (telega-filter-active)))))))))

;;; ellit-org: chat-filters
;; - (type ~CHAT-TYPE-LIST~), {{{where-is(telega-filter-by-type,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--filter-type, 2)}}}
;;
;;   Every chat has a type.  Type is one of:
;;   - private :: Private chat with telegram user
;;   - secret :: Secret chat with telegram user
;;   - bot :: Chat with telegram bot
;;   - basicgroup :: Small chat group, could be upgraded to supergroup
;;   - supergroup :: Chat group with all the chat possibilities
;;   - channel :: Supergroup with unlimited members, where only admins can post messages
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

;;; ellit-org: chat-filters
;; - (name ~REGEXP~) ::
;;   {{{fundoc(telega--filter-name, 2)}}}
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

;;; ellit-org: chat-filters
;; - (search ~QUERY~), {{{where-is(telega-filter-by-search,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--filter-search, 2)}}}
(define-telega-filter search (chat _query)
  "Matches if chat maches search QUERY."
  (memq chat telega--search-chats))

(defun telega-filter-by-search (query)
  "Filter chats by QUERY."
  (interactive (list (read-string "Chat search query: ")))
  (setq telega--search-chats (telega--searchChats query))
  (telega-filter-add `(search ,query)))

;;; ellit-org: chat-filters
;; - nearby, {{{where-is(telega-filter-by-nearby,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--filter-nearby, 2)}}}
(define-telega-filter nearby (chat)
  "Matches if chat is nearby `telega-my-location'."
  (telega-chat-nearby-find (plist-get chat :id)))

(defun telega-filter-by-nearby ()
  "Filter chats nearby `telega-my-location'."
  (interactive)
  (unless telega-my-location
    (user-error "`telega-my-location' is unset, can't search nearby chats"))
  (telega--searchChatsNearby telega-my-location)
  (telega-filter-add 'nearby))

;;; ellit-org: chat-filters
;; - (custom ~NAME~), {{{where-is(telega-filter-by-custom,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--filter-custom, 2)}}}
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

;;; ellit-org: chat-filters
;; - pin, {{{where-is(telega-filter-by-pin,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--filter-pin, 2)}}}
(define-telega-filter pin (chat)
  "Matches if chat is pinned."
  (plist-get chat :is_pinned))

(defun telega-filter-by-pin ()
  "Filter only pinned chats."
  (interactive)
  (telega-filter-add 'pin))

;;; ellit-org: chat-filters
;; - (has-username [ ~USERNAME~ ]) ::
;;   {{{fundoc(telega--filter-has-username, 2)}}}
(define-telega-filter has-username (chat &optional username)
  "Matches if chat has username associated with the chat."
  (when-let ((chat-username (telega-chat-username chat)))
    (or (not username)
        (equal username chat-username))))

;;; ellit-org: chat-filters
;; - has-pinned-message ::
;;   {{{fundoc(telega--filter-has-pinned-message, 2)}}}
(define-telega-filter has-pinned-message (chat)
  "Matches if chat has pinned message."
  (not (zerop (plist-get chat :pinned_message_id))))

(defun telega-filter-by-pinned-message ()
  "Filter only chats with pinned message."
  (interactive)
  (telega-filter-add 'has-pinned-message))

;;; ellit-org: chat-filters
;; - (unread [ ~N~ ]), {{{where-is(telega-filter-by-unread,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--filter-unread, 2)}}}
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

;;; ellit-org: chat-filters
;; - (mention [ ~N~ ]), {{{where-is(telega-filter-by-mention,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--filter-mention, 2)}}}
(define-telega-filter mention (chat &optional n)
  "Matches if chat has least N unread mentions.
By default N is 1."
  (>= (plist-get chat :unread_mention_count) (or n 1)))

(defun telega-filter-by-mention (n)
  "Filter chats with at least N unread mentions."
  (interactive "p")
  (telega-filter-add `(mention ,n)))

;;; ellit-org: chat-filters
;; - unmuted, {{{where-is(telega-filter-by-unmuted,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--filter-unmuted, 2)}}}
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

;;; ellit-org: chat-filters
;; - (online-status ~STATUS-LIST~...), {{{where-is(telega-filter-by-online-status,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--filter-online-status, 2)}}}
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
                        '("Online" "Recently" "LastWeek" "LastMonth"
                          "Offline" "Empty")
                        nil t))))
  (telega-filter-add `(online-status ,status)))

;;; ellit-org: chat-filters
;; - verified, {{{where-is(telega-filter-by-verified,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--filter-verified, 2)}}}
(define-telega-filter verified (chat)
  "Matches if chat is verified."
  (plist-get (telega-chat--info chat) :is_verified))

(defun telega-filter-by-verified ()
  "Filter verified chats."
  (interactive)
  (telega-filter-add 'verified))

;;; ellit-org: chat-filters
;; - (ids ~ID-LIST~...) ::
;;   {{{fundoc(telega--filter-ids, 2)}}}
(define-telega-filter ids (chat &rest id-list)
  "Matches if chat's id is one of in ID-LIST."
  (memq (plist-get chat :id) id-list))

(defun telega-filter-by-created-by-me ()
  "Filter public chats created by me."
  (interactive)
  (telega-filter-add
   (cons 'ids (mapcar (telega--tl-prop :id)
                      (telega--getCreatedPublicChats)))))

;;; ellit-org: chat-filters
;; - me-is-owner ::
;;   {{{fundoc(telega--filter-me-is-owner, 2)}}}
(define-telega-filter me-is-owner (chat)
  "Matches if me is the owner of the chat.
Only basicgroup, supergroup and channel can be owned."
  (when (memq (telega-chat--type chat 'raw) '(basicgroup supergroup))
    (let ((chat-info (telega-chat--info chat)))
      (eq (telega--tl-type (plist-get chat-info :status))
          'chatMemberStatusCreator))))

;;; ellit-org: chat-filters
;; - me-is-member ::
;;   {{{fundoc(telega--filter-me-is-member, 2)}}}
(define-telega-filter me-is-member (chat)
  "Matches if me is member of the chat."
  (not (and (memq (telega-chat--type chat 'raw) '(basicgroup supergroup))
            (memq (telega--tl-type (plist-get (telega-chat--info chat) :status))
                  '(chatMemberStatusLeft chatMemberStatusBanned)))))

;;; ellit-org: chat-filters
;; - has-last-message ::
;;   {{{fundoc(telega--filter-has-last-message, 2)}}}
(define-telega-filter has-last-message (chat)
  "Matches if chat has last message."
  (plist-get chat :last_message))

;;; ellit-org: chat-filters
;; - has-avatar ::
;;   {{{fundoc(telega--filter-has-avatar, 2)}}}
(define-telega-filter has-avatar (chat)
  "Matches if chat has chat photo."
  (plist-get chat :photo))

;;; ellit-org: chat-filters
;; - has-animated-avatar ::
;;   {{{fundoc(telega--filter-has-animated-avatar, 2)}}}
(define-telega-filter has-animated-avatar (chat)
  "Matches if CHAT has animated chat photo."
  (telega--tl-get chat :photo :has_animation))

;;; ellit-org: chat-filters
;; - has-chatbuf, {{{where-is(telega-filter-by-has-chatbuf,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--filter-has-chatbuf, 2)}}}
(define-telega-filter has-chatbuf (chat)
  "Matches if chat has corresponding chatbuf."
  (with-telega-chatbuf chat
    t))

(defun telega-filter-by-has-chatbuf ()
  "Filter chats that has corresponding chat buffer."
  (interactive)
  (telega-filter-add 'has-chatbuf))

;;; ellit-org: chat-filters
;; - (permission ~PERM~) ::
;;   {{{fundoc(telega--filter-permission, 2)}}}
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

;;; ellit-org: chat-filters
;; - (restriction ~SUFFIX-LIST~...), {{{where-is(telega-filter-by-restriction,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--filter-restriction, 2)}}}
;;
;;   Chat restriction reason reported only if chat must be restricted
;;   by current client.  See
;;   [[https://github.com/tdlib/td/issues/1203][TDLib#1203]]
(define-telega-filter restriction (chat &rest suffix-list)
  "Matches restricted chats.
SUFFIX-LIST is a list of suffixes to filter on.
Suffix can be one of:
- \"-all\"      - All platforms
- \"-ios\"      - For iOS devices
- \"-android\"  - For Android devices
- \"-wp\"       - Windows?

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

;;; ellit-org: chat-filters
;; - (contact [ ~MUTUAL-P~ ]), {{{where-is(telega-filter-by-contact,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--filter-contact, 2)}}}
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

;;; ellit-org: chat-filters
;; - top, {{{where-is(telega-filter-by-top,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--filter-top, 2)}}}
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

;;; ellit-org: chat-filters
;; - saved-messages ::
;;   {{{fundoc(telega--filter-saved-messages, 2)}}}
(define-telega-filter saved-messages (chat)
  "Matches only SavedMessages chat."
  (telega-me-p chat))

;;; ellit-org: chat-filters
;; - tracking, {{{where-is(telega-filter-by-tracking,telega-root-mode-map)}}} ::
;;   {{{fundoc1(telega--filter-tracking, 2)}}}
(define-telega-filter tracking (chat)
  "Matches if chat is in tracking buffers list."
  (with-telega-chatbuf chat
    (member (buffer-name) tracking-buffers)))

(defun telega-filter-by-tracking ()
  "Matches if chat is in tracking buffers list."
  (interactive)
  (telega-filter-add (list 'tracking)))

;;; ellit-org: chat-filters
;; - last-message-by-me ::
;;   {{{fundoc1(telega--filter-last-message-by-me, 2)}}}
(define-telega-filter last-message-by-me (chat)
  "Matches if chat's last message sent by me."
  (when-let ((last-msg (plist-get chat :last_message)))
    (eq (plist-get last-msg :sender_user_id) telega--me-id)))

;;; ellit-org: chat-filters
;; - (chat-list ~LIST-NAME~), {{{where-is(telega-filter-by-folder,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--filter-chat-list, 2)}}}
(define-telega-filter chat-list (chat list-name)
  "Matches if chat is in chat list named LIST-NAME.
LIST-NAME is `main' or `archive' symbol, or string naming tdlib
chat filter aka Telegram Folder."
  (when-let ((pos (cl-find list-name (plist-get chat :positions)
                           :key #'telega-chat-position--list-name
                           :test #'equal)))
    ;; NOTE: positions with zero order should be removed in
    ;; `telega--on-updateChatPosition'
    (cl-assert (not (equal "0" (plist-get pos :order))))
    t))

;;; ellit-org: chat-filters
;; - (folder ~FOLDER-NAMES~...), {{{where-is(telega-filter-by-folder,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--filter-folder, 2)}}}
(define-telega-filter folder (chat &rest folder-names)
  "Matches if chat belongs to some Chat Folder of FOLDER-NAMES."
  (seq-intersection (telega-chat-folders chat) folder-names))

(defun telega-filter-by-folder (folder)
  "Match chats by Telegram FOLDER."
  (interactive (list (telega-completing-read-folder "Telegram Folder: ")))
  (telega-filters-push
   (list (list 'folder (substring-no-properties folder)))))

;;; ellit-org: chat-filters
;; - main ::
;;   {{{fundoc(telega--filter-main, 2)}}}
(define-telega-filter main (chat)
  "Matches if chat from \"Main\" chat list."
  (telega-chat-match-p chat '(chat-list main)))

;;; ellit-org: chat-filters
;; - archive ::
;;   {{{fundoc(telega--filter-archive, 2)}}}
(define-telega-filter archive (chat)
  "Matchis if chat is archived, i.e. in \"Archive\" chat list."
  (telega-chat-match-p chat '(chat-list archive)))

;;; ellit-org: chat-filters
;; - has-scheduled-messages ::
;;   {{{fundoc(telega--filter-has-scheduled-messages, 2)}}}
(define-telega-filter has-scheduled-messages (chat)
  "Matches if chat has scheduled messages."
  (plist-get chat :has_scheduled_messages))

;;; ellit-org: chat-filters
;; - has-action-bar ::
;;   {{{fundoc(telega--filter-has-action-bar, 2)}}}
(define-telega-filter has-action-bar (chat)
  "Matches CHAT with active action bar."
  (plist-get chat :action_bar))

;;; ellit-org: chat-filters
;; - has-reply-markup ::
;;   {{{fundoc(telega--filter-has-reply-markup, 2)}}}
(define-telega-filter has-reply-markup (chat)
  "Matches CHAT with reply markup message."
  (not (eq 0 (plist-get chat :reply_markup_message_id))))

;;; ellit-org: chat-filters
;; - can-get-statistics ::
;;   {{{fundoc(telega--filter-can-get-statistics, 2)}}}
;; 
;;   Available since TDLib 1.6.9
(define-telega-filter can-get-statistics (chat)
  "Matches if statistics available for CHAT."
  (when (eq (telega-chat--type chat 'raw) 'supergroup)
    (let ((full-info (telega--full-info (telega-chat--info chat))))
      (plist-get full-info :can_get_statistics))))

;;; ellit-org: chat-filters
;; - has-linked-chat ::
;;   {{{fundoc(telega--filter-has-linked-chat, 2)}}}
(define-telega-filter has-linked-chat (chat)
  "Matches if CHAT is supergroup and has linked chat."
  (plist-get (telega-chat--info chat) :has_linked_chat))

;;; ellit-org: chat-filters
;; - has-discussion-group ::
;;   {{{fundoc(telega--filter-has-discussion-group, 2)}}}
(define-telega-filter has-discussion-group (chat)
  "Matches if CHAT is a channel with a linked discussion group."
  (telega-chat-match-p chat '(and (type channel) has-linked-chat)))

;;; ellit-org: chat-filters
;; - has-location ::
;;   {{{fundoc(telega--filter-has-location, 2)}}}
(define-telega-filter has-location (chat)
  "Matches if CHAT is supergroup and has linked chat."
  (plist-get (telega-chat--info chat) :has_location))

;;; ellit-org: chat-filters
;; - inactive-supergroups ::
;;   {{{fundoc(telega--filter-inactive-supergroups, 2)}}}
(define-telega-filter inactive-supergroups (chat)
  "Matches if CHAT is inactive supergroup."
  (memq chat telega--search-chats))

(defun telega-filter-by-inactive-supergroups ()
  "Filter inactive supergroups.
Can be used when user reaches limit on the number of joined
supergroups and channels and receives CHANNELS_TOO_MUCH error."
  (interactive)
  (setq telega--search-chats (telega--getInactiveSupergroupChats))
  (telega-filter-add 'inactive-supergroups))

;;; ellit-org: chat-filters
;; - default-disable-notification ::
;;   {{{fundoc(telega--filter-default-disable-notification, 2)}}}
(define-telega-filter default-disable-notification (chat)
  "Matches if CHAT has non-nil default disable notification setting."
  (plist-get chat :default_disable_notification))

(provide 'telega-filter)

;;; telega-filter.el ends here

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
(declare-function telega-chat-bot-p "telega-chat" (chat))
(declare-function telega-chat-title "telega-chat" (chat &optional with-username))
(declare-function telega-chat--info "telega-chat" (chat &optional local-p))
(declare-function telega-chat-user "telega-chat" (chat &optional include-bots-p))
(declare-function telega-chats-top "telega-chat" (category))
(declare-function telega-chat-member-my-status "telega-chat" (chat))
(declare-function telega-chat-member-my-permissions "telega-chat" (chat))

(declare-function telega-root--buffer "telega-root")
(declare-function telega-root-view--redisplay "telega-root")

(declare-function telega--full-info "telega-info" (tlobj &optional offline-p _callback))


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
  (let* ((filter (nth 1 custom-spec))
         ;; NOTE: Main and Archive are emphasized with
         ;; `telega-symbol-chat-list'
         (name (concat (when (memq filter '(main archive))
                         (telega-symbol 'chat-list))
                       (nth 0 custom-spec)))
         (chats (nthcdr 2 custom-spec))
         ;; NOTE: Folders always active
         (active-p (or (telega-filter--folder-p filter) (not (null chats))))
         (nchats (length chats))
         (unread (apply #'+ (mapcar (telega--tl-prop :unread_count) chats)))
         (mentions (apply #'+ (mapcar
                               (telega--tl-prop :unread_mention_count) chats)))
         (umstring
          (telega-ins--as-string
           (telega-ins--with-attrs (list :max 7
                                         :align-symbol "\u00a0"
                                         :elide t
                                         :align 'right)
             (unless (zerop unread)
               (telega-ins (telega-number-human-readable unread)))
             (unless (zerop mentions)
               (telega-ins-fmt "@%d" mentions)))))
         (filter-button-width
          (telega-canonicalize-number telega-filter-button-width
                                      telega-root-fill-column))
         (title-width
          (- filter-button-width (string-width umstring)
             ;; `filter-button-width' is width for the button
             ;; including brackes
             2
             )))
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
                                      ;; non-break space
                                      :align-symbol "\u00a0"
                                      :elide t
                                      :align 'left)
          (telega-ins (number-to-string nchats) ":")
          (if (telega-filter--custom-active-p custom-spec)
              (telega-ins--with-face 'bold
                (telega-ins name))
            (telega-ins name)))
        (telega-ins "\u00a0")
        (telega-ins umstring)
        (telega-ins "]")))))

(defun telega-filter-button--action (button)
  "Action to take when custom filter button is pressed.
If prefix ARG is specified then set custom filter as active,
otherwise toggle custom filter in the active chat filters."
  (let* ((custom (button-get button :value))
         (fspec (if (or telega-filter-custom-expand
                        (telega-filter--folder-p (nth 1 custom)))
                    (nth 1 custom)
                  (list 'custom (nth 0 custom)))))
    (if current-prefix-arg
        (telega-filters-push (list fspec))

      ;; NOTE: if CUSTOM is a folder filter, then replace current
      ;; folder in active chat filter with new one
      (if (and (telega-filter--folder-p (car (telega-filter-active)))
               (telega-filter--folder-p fspec))
          (telega-filters-push (cons fspec (cdr (telega-filter-active))))

        ;; Otherwise toggle it in the active chat filter
        (if (member fspec (telega-filter-active))
            (telega-filters-push (delete fspec (telega-filter-active)))
          (telega-filter-add fspec))))))

(defun telega-filter-active (&optional with-root-view-filter)
  "Return active filter.
If WITH-ROOT-VIEW-FILTER is non-nil, then add root view filter."
  (let ((filter (car telega--filters)))
    (when (and with-root-view-filter telega-root--view-filter)
      (setq filter (append filter (list telega-root--view-filter))))
    filter))

;; Special logic for Main and Archive custom filters
;; 
;; We peredent that Main or Archive starts active chat filter to match
;; chats agains them, so numbers in custom buttons are displaydd
;; correctly.
;; 
;; For Main we apply this only if Archive is currently active
;; For Archive we apply this if any Folder filter is currently active
;; 
;; See https://t.me/emacs_telega/22303
;; 
;; This logic affects `telega-filter--custom-chats' to calculate full
;; list of chats matching custom filter and
;; `telega-filters--chat-update' to check if updated chat stops/starts
;; matching custom filter
(defun telega-filter-active-special-p (for-custom-filter)
  "Return non-nil if active chat filter is special FOR-CUSTOM-FILTER."
  (let ((active-filter (telega-filter-active)))
    (or (and (eq 'main for-custom-filter)
             (eq 'archive (car active-filter)))
        (and (eq 'archive for-custom-filter)
             (telega-filter--folder-p (car active-filter))))))

(defun telega-filter-active-prepare (&optional with-root-view-filter
                                               for-custom-filter)
  "Prepare `telega--filters' for the application.
WITH-ROOT-VIEW-FILTER is passed directly to `telega-filter-active'.
Return chat filter prepared for the application.

FOR-CUSTOM-FILTER could be specified ty apply special logic."
  (let ((active-filter (telega-filter-active with-root-view-filter)))
    (when (and for-custom-filter
               (telega-filter-active-special-p for-custom-filter))
      (setq active-filter
            (cons for-custom-filter (cdr active-filter))))

    (cond ((null active-filter) 'all)
          ((= (length active-filter) 1) (car active-filter))
          ((eq 'all (car active-filter))
           (if (= 1 (length (cdr active-filter)))
               (cadr active-filter)
             active-filter))
          (t (cons 'all active-filter)))))

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
  (let* ((filter-button-width
          (telega-canonicalize-number telega-filter-button-width
                                      telega-root-fill-column))
         (ccolumn (current-column))
         (custom-filter (nth 1 custom))
         (folder-p (and (listp custom-filter)
                        (eq 'folder (car custom-filter)))))
    ;; NOTE: 3 - two spaces and a newline
    (cond ((and (not (memq (if folder-p 'folders 'custom)
                           telega-filter-custom-one-liners))
                (> (+ 3 ccolumn filter-button-width) telega-root-fill-column))
           (insert "\n"))
          ;; NOTE: start Folders from newline for any non-nil
          ;; `telega-filter-custom-one-liners' setting
          ((and telega-filter-custom-one-liners
                folder-p
                (equal (nth 1 custom-filter)
                       (telega-tl-str (car telega-tdlib--chat-filters) :title)))
           (insert "\n"))
          ((zerop ccolumn)
           ;; no-op
           )
          (t
           (insert "  "))))
  (telega-button--insert 'telega-filter custom))

(defun telega-filters--footer ()
  "Generate string used as root header."
  (let ((filters-width (- telega-root-fill-column 8)))
    (telega-ins--as-string
     (telega-ins "\n")
     (telega-ins (telega-symbol 'horizontal-bar)
                 "/"
                 (telega-symbol 'horizontal-bar)
                 (telega-symbol 'horizontal-bar))
     (telega-ins--with-attrs (list :min filters-width
                                   :align 'center
                                   :align-symbol 'horizontal-bar
                                   :max filters-width
                                   :elide t
                                   :elide-trail (/ filters-width 2))
       (let* ((active-filter (telega-filter-active))
              (af-str (prin1-to-string active-filter)))
         (unless (telega-filter-default-p active-filter)
           (setq af-str (propertize af-str 'face 'telega-filter-active)))
         (telega-ins af-str)))
     (telega-ins (telega-symbol 'horizontal-bar)
                 (telega-symbol 'horizontal-bar)
                 (telega-symbol 'horizontal-bar)
                 (telega-symbol 'horizontal-bar))

     (when (or telega--sort-criteria telega--sort-inverted)
       (telega-ins "\n")
       (telega-ins (telega-symbol 'horizontal-bar)
                   "\\"
                   (telega-symbol 'horizontal-bar)
                   (telega-symbol 'horizontal-bar))
       (telega-ins--with-attrs (list :min filters-width
                                     :align 'center
                                     :align-symbol 'horizontal-bar
                                     :max filters-width
                                     :elide t
                                     :elide-trail (/ filters-width 2))
         (telega-ins--with-face 'bold
           (when telega--sort-inverted
             (telega-ins "(inverted"))
           (when telega--sort-criteria
             (when telega--sort-inverted
               (telega-ins " "))
             (telega-ins-fmt "%S" telega--sort-criteria))
           (when telega--sort-inverted
             (telega-ins ")"))))
       (telega-ins (telega-symbol 'horizontal-bar)
                   (telega-symbol 'horizontal-bar)
                   (telega-symbol 'horizontal-bar)
                   (telega-symbol 'horizontal-bar)))
     )))

(defun telega-filter--custom-active-p (custom)
  "Return non-nil if CUSTOM filter is active chat filter.
Actually return active chat filter corresponding to CUSTOM filter."
  (car (or (member (nth 1 custom) (telega-filter-active))
           (member (list 'custom (car custom)) (telega-filter-active)))))

(defun telega-filter--custom-folder-spec (tdlib-chat-filter)
  "Return custom filter spec for the TDLIB-CHAT-FILTER folder."
  (let ((fn (telega-tl-str tdlib-chat-filter :title)))
    (cons (telega-folder-format "%i%f" fn tdlib-chat-filter)
          (list 'folder (substring-no-properties fn)))))

(defun telega-filter--custom-chats (custom)
  "Return chats matching CUSTOM chat filter."
  (if (telega-filter-active-special-p (cdr custom))
      (telega-filter-chats
       telega--ordered-chats (telega-filter-active-prepare t (cdr custom)))
    (telega-filter-chats telega--filtered-chats (cdr custom))))

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
(defun telega-filter--folder-p (fspec)
  "Return non-nil if chat filter FSPEC is folder filter."
  (or (memq fspec '(main archive))
      (and (consp fspec)
           (memq (car fspec) '(chat-list folder)))))

(defun telega-filter-active-tdlib-chat-list ()
  "Return TDLib chat list extracting it from active chat filter.
Normally first element in active chat filter is a folder, if 
list, if no, then `main' is returned."
  (let* ((active-filter (telega-filter-active))
         (cl-fspec (if (telega-filter--folder-p (car active-filter))
                       (car active-filter)
                     'main)))
    (cond ((eq cl-fspec 'main) '(:@type "chatListMain"))
          ((eq cl-fspec 'archive) '(:@type "chatListArchive"))
          (t (telega-folder--tdlib-chat-list (cadr cl-fspec))))))

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
    (when (telega-chat-match-p chat (telega-filter-active-prepare t))
      (setq chat-matches-p t)
      (setq telega--filtered-chats
            (push chat telega--filtered-chats)))

    ;; Remove/Add chat in ewoc specs for custom filters
    (dolist (ewoc-spec (ewoc-collect telega-filters--ewoc #'identity))
      (let* ((filter (nth 1 ewoc-spec))
             (match-p (if (telega-filter-active-special-p filter)
                          (telega-chat-match-p chat
                            (telega-filter-active-prepare t filter))
                        ;; No special treatment
                        (and chat-matches-p
                             (telega-chat-match-p chat filter))))
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
  (setq telega--filters (when default (list (list default)))
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
  (declare (doc-string 3) (indent 2))
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
  "Return non-nil if CHAT-FILTER matches CHAT.
If CHAT-FILTER is not specified, active chat filter is used."
  (declare (indent 1))
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

(defun telega-chat-match-active-p (chat)
  "Return non-nil if CHAT matches active chat filter."
  (telega-chat-match-p chat (telega-filter-active-prepare 'with-root-view)))

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

(defun telega-filter-by-filter (filter-name)
  "Interactively select a Chat filter to add to active filter."
  ;; Query user for filter with corresponding interactive function, or
  ;; filter without argument
  (interactive
   (list
    (let* ((filter-funs
            (cl-remove-if
             (lambda (funsym)
               (let ((funargs (help-function-arglist funsym)))
                 (and (> (length funargs) 1)
                      (not (eq '&optional (nth 1 funargs))))))
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
;;   - ~private~ Private chat with a Telegram user
;;   - ~secret~ Secret chat with a Telegram user
;;   - ~bot~ Chat with a Telegram bot
;;   - ~basicgroup~ Small chat group, could be upgraded to supergroup
;;   - ~supergroup~ Chat group with all the chat possibilities
;;   - ~channel~ Supergroup with unlimited members, where only admins can post messages
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
      (let ((info (telega-chat--info chat 'locally)))
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
;;   UNAVAILABLE since TDLib 1.6.10, chat has no fast way (property)
;;   to get know that chat has a pinned message.  See
;;   https://github.com/tdlib/td/issues/1275

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
Important chat is a chat with unread messages and enabled notifications."
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
  (plist-get (telega-chat--info chat 'locally) :is_verified))

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

(defun telega-filter-by-my-public-chats (chat-type)
  "Filter public chats created by me.
CHAT-TYPE is one of `has-username' or `location-based'."
  (interactive
   (list (intern (completing-read "Public Chat Type: "
                                  '("has-username" "location-based") nil t))))
  (telega-filter-add
   (cons 'ids (mapcar (telega--tl-prop :id)
                      (telega--getCreatedPublicChats chat-type)))))

;;; ellit-org: chat-filters
;; - (me-is-owner [ ~OR-ADMIN~ ]) ::
;;   {{{fundoc(telega--filter-me-is-owner, 2)}}}
(define-telega-filter me-is-owner (chat &optional or-admin)
  "Matches if me is owner of the chat.
Only basicgroup, supergroup and channel can be owned.
If optional OR-ADMIN is specified, then match also if me is
administrator in the chat."
  (when-let ((status (telega-chat-member-my-status chat)))
    (memq (telega--tl-type status)
          (list 'chatMemberStatusCreator
                (when or-admin 'chatMemberStatusAdministrator)))))

;;; ellit-org: chat-filters
;; - me-is-member ::
;;   {{{fundoc(telega--filter-me-is-member, 2)}}}
(define-telega-filter me-is-member (chat)
  "Matches if me is member of the chat.
Matches only basicgroup, supergroup or a channel."
  (when-let ((status (telega-chat-member-my-status chat)))
    (cl-ecase (telega--tl-type status)
      ((chatMemberStatusAdministrator chatMemberStatusMember)
       t)
      ((chatMemberStatusCreator chatMemberStatusRestricted)
       (plist-get status :is_member))
      ((chatMemberStatusLeft chatMemberStatusBanned)
       nil))))

;;; ellit-org: chat-filters
;; - me-is-anonymous ::
;;   {{{fundoc(telega--filter-me-is-anonymous, 2)}}}
(define-telega-filter me-is-anonymous (chat)
  "Matches if me is anonymous in the chat."
  (plist-get (telega-chat-member-my-status chat) :is_anonymous))

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
PERM could be one of listed in `telega-chat--chat-permisions'."
  (plist-get (plist-get chat :permissions) perm))

(defun telega-filter-by-permission (perm)
  "Filter chats by allowed permission PERM."
  (interactive (list (telega-completing-read-permission
                      "Chat permission: " telega-chat--chat-permisions)))
  (telega-filter-add (list 'permission perm)))

;;; ellit-org: chat-filters
;; - (my-permission ~PERM~) ::
;;   {{{fundoc(telega--filter-my-permission, 2)}}}
(define-telega-filter my-permission (chat perm)
  "Matches if me has PERM permission in the chat.
PERM could be one of in `telega-chat--chat-permisions' list or in
`telega-chat--admin-permissions' list."
  (plist-get (telega-chat-member-my-permissions chat) perm))

(defun telega-filter-by-my-permission (perm)
  "Filter chats by allowed permission PERM for me."
  (interactive (list (telega-completing-read-permission
                      "Chat permission: "
                      (append telega-chat--chat-permisions
                              telega-chat--admin-permissions))))
  (telega-filter-add (list 'my-permission perm)))
   
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
  (when-let ((reason (telega-tl-str (telega-chat--info chat 'locally)
                                    :restriction_reason)))
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
  (when-let ((chat-user (telega-chat-user chat)))
    (plist-get chat-user (if mutual-p
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
  "Matches only \"Saved Messages\" chat."
  (telega-me-p chat))

;;; ellit-org: chat-filters
;; - replies-messages ::
;;   {{{fundoc(telega--filter-replies-messages, 2)}}}
(define-telega-filter replies-messages (chat)
  "Matches only \"Replies\" chat."
  (telega-replies-p chat))

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
    (telega-msg-by-me-p last-msg)))

;;; ellit-org: chat-filters
;; - (chat-list ~LIST-NAME~), {{{where-is(telega-filter-by-folder,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--filter-chat-list, 2)}}}
(define-telega-filter chat-list (chat list-name)
  "Matches if chat is in chat list named LIST-NAME.
LIST-NAME is `main' or `archive' symbol, or string naming Chat Folder."
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
  "Matches if chat is archived, i.e. in \"Archive\" chat list."
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
  "Matches if chat has active action bar."
  (plist-get chat :action_bar))

;;; ellit-org: chat-filters
;; - has-reply-markup ::
;;   {{{fundoc(telega--filter-has-reply-markup, 2)}}}
(define-telega-filter has-reply-markup (chat)
  "Matches if chat has reply markup message."
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

;;; ellit-org: chat-filters
;; - temporary-muted ::
;;   {{{fundoc(telega--filter-temporary-muted, 2)}}}
(define-telega-filter temporary-muted (chat)
  "Matches if CHAT is temporary muted."
  (let ((muted-for (telega-chat-notification-setting chat :mute_for)))
    (and (> muted-for 0)
         (< muted-for telega-mute-for-ever))))

;;; ellit-org: chat-filters
;; - fake-or-scam ::
;;   {{{fundoc(telega--filter-fake-or-scam, 2)}}}
(define-telega-filter fake-or-scam (chat)
  "Matches if chat is fake or scam user or group."
  (let ((info (telega-chat--info chat)))
    (or (plist-get info :is_scam)
        (plist-get info :is_fake))))

;;; ellit-org: chat-filters
;; - (has-video-chat [ ~NON-EMPTY~ ]) ::
;;   {{{fundoc(telega--filter-has-video-chat, 2)}}}
(define-telega-filter has-video-chat (chat &optional non-empty)
  "Matches if chat contains a live video chat.
If non-nil NON-EMPTY is specified, then match only if video chat is
not empty."
  (when-let* ((video-chat (plist-get chat :video_chat))
              (group-call-id (plist-get video-chat :group_call_id)))
    (and (not (zerop group-call-id))
         (or (null non-empty)
             (plist-get video-chat :has_participants)))))

(defun telega-filter-by-has-video-chat (including-empty-p)
  "Filter chats with started video chat.
If INCLUDING-EMPTY-P is non-nil, then keep also empty video chats."
  (interactive (list (y-or-n-p "Include empty/scheduled video chats? ")))
  (telega-filter-add (if including-empty-p
                         'has-video-chat
                       '(has-video-chat with-users))))

;;; ellit-org: chat-filters
;; - has-favorite-messages ::
;;   {{{fundoc(telega--filter-has-favorite-messages, 2)}}}
(define-telega-filter has-favorite-messages (chat)
  "Matches if chat has favorite messages."
  (telega-chat-uaprop chat :telega-favorite-ids))

;;; ellit-org: chat-filters
;; - is-public ::
;;   {{{fundoc(telega--filter-has-message-ttl, 2)}}}
(define-telega-filter has-message-ttl (chat)
  "Matches if chat has `:message_ttl'."
  (when-let ((msg-ttl (plist-get chat :message_ttl)))
    (> msg-ttl 0)))

;;; ellit-org: chat-filters
;; - is-broadcast-group ::
;;   {{{fundoc(telega--filter-is-broadcast-group, 2)}}}
(define-telega-filter is-broadcast-group (chat)
  "Matches if chat is a broadcast group."
  (plist-get (telega-chat--info chat) :is_broadcast_group))

;;; ellit-org: chat-filters
;; - has-groups-in-common ::
;;   {{{fundoc(telega--filter-has-groups-in-common, 2)}}}
(define-telega-filter has-groups-in-common (chat)
  "Matches if corresponding user has groups in common with me."
  (when-let ((user (telega-chat-user chat)))
    (not (zerop (plist-get (telega--full-info user) :group_in_common_count)))))

;;; ellit-org: chat-filters
;; - is-telega-patron ::
;;   {{{fundoc(telega--filter-is-telega-patron, 2)}}}
(define-telega-filter is-telega-patron (chat)
  "Matches if corresponding user is a telega patron."
  (telega-msg-sender-patron-p chat))

;;; ellit-org: chat-filters
;; - has-sponsored-message ::
;;   {{{fundoc(telega--filter-has-sponsored-message, 2)}}}
(define-telega-filter has-sponsored-message (chat)
  "Matches if chat has sponsored message.
BE AWARE: This filter will do blocking request for every chat."
  (when (telega-chat-match-p chat '(type channel))
    (telega--getChatSponsoredMessage chat)))

;;; ellit-org: chat-filters
;; - has-protected-content ::
;;   {{{fundoc(telega--filter-has-protected-content, 2)}}}
(define-telega-filter has-protected-content (chat)
  "Matches if chat has protected content."
  (plist-get chat :has_protected_content))

;;; ellit-org: chat-filters
;; - has-private-forwards ::
;;   {{{fundoc(telega--filter-has-private-forwards, 2)}}}
(define-telega-filter has-private-forwards (chat)
  "Matches if user can't be linked in forwarded messages."
  (when-let ((user (telega-chat-user chat)))
    (plist-get (telega--full-info user) :has_private_forwards)))

;;; ellit-org: chat-filters
;; - has-default-sender ::
;;   {{{fundoc(telega--filter-has-default-sender, 2)}}}
(define-telega-filter has-default-sender (chat)
  "Matches if chat allows choosing a message sender."
  (plist-get chat :message_sender_id))

(define-telega-filter can-send-or-post (chat)
  "Me can send or post messages to the CHAT."
  (let ((my-perms (telega-chat-member-my-permissions chat)))
    (or (plist-get my-perms :can_send_messages)
        (plist-get my-perms :can_post_messages))))

;;; ellit-org: chat-filters
;; - is-inline-bot ::
;;   {{{fundoc(telega--filter-is-inline-bot, 2)}}}
(define-telega-filter is-inline-bot (chat)
  "Matches if corresponding bot accepts inline requests."
  (when (telega-chat-bot-p chat)
    (when-let ((user (telega-chat-user chat 'include-bots)))
      (telega--tl-get user :type :is_inline))))

(provide 'telega-filter)

;;; telega-filter.el ends here

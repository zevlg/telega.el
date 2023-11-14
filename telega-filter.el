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
;;   - ~(return t)~ ::
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
(declare-function telega-chat-channel-p "telega-chat" (chat))
(declare-function telega-chat-bot-p "telega-chat" (chat))
(declare-function telega-chat-title "telega-chat" (chat &optional no-badges))
(declare-function telega-chat--info "telega-chat" (chat &optional local-p))
(declare-function telega-chats-top "telega-chat" (category))
(declare-function telega-chat-member-my-status "telega-chat" (chat))
(declare-function telega-chat-member-my-permissions "telega-chat" (chat))
(declare-function telega-chatbuf--last-read-inbox-msg-id "telega-chat")

(declare-function telega-root--buffer "telega-root") ; `with-telega-root-buffer'
(declare-function telega-root-view--redisplay "telega-root")


(defvar telega-filters--ewoc nil "ewoc for custom filters.")
(defvar telega-filters--dirty nil
  "Non-nil if filter's ewoc is dirty and need to be redisplayed.
Could be a list of custom filters marked dirty.
If t, then all custom filters are dirty.")

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

(defvar telega-filter-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "r") 'telega-filter-read-all)
    map)
  "The key map for filter buttons.")

(define-button-type 'telega-filter
  :supertype 'telega
  :inserter telega-inserter-for-filter-button
  :help-echo (lambda (custom)
               (format "Filter (custom \"%s\") expands to: %s"
                       (nth 0 custom) (nth 1 custom)))
  'action #'telega-filter-button--action
  'keymap telega-filter-button-map)

(defun telega-ins--filter (custom-spec)
  "Inserter for the custom filter button specified by CUSTOM-SPEC.
See `telega-filter--ewoc-spec' for CUSTOM-SPEC description."
  (let* ((filter (nth 1 custom-spec))
         ;; NOTE: Main and Archive are emphasized with
         ;; `telega-symbol-chat-list'
         (name (concat (when (memq filter '(main archive))
                         (telega-symbol 'chat-list))
                       (telega-filter--custom-name custom-spec)))
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
                                         :align-symbol telega-symbol-nbsp
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
                                      :align-symbol telega-symbol-nbsp
                                      :elide t
                                      :align 'left)
          (telega-ins (number-to-string nchats) ":")
          (if (telega-filter--custom-active-p custom-spec)
              (telega-ins--with-face 'bold
                (telega-ins name))
            (telega-ins name)))
        (telega-ins telega-symbol-nbsp)
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
                  (list 'custom (telega-filter--custom-name custom)))))
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

(defun telega-filter-read-all (custom-filter)
  "Read all messages in all chats from active filter."
  (interactive (let ((button (button-at (point))))
                 (when (and button (eq (button-type button) 'telega-filter))
                   (list (button-get button :value)))))

  (when (telega-filter--folder-p (nth 1 custom-filter))
    (let* ((folder-chats (telega-filter-chats
                             telega--ordered-chats (nth 1 custom-filter)))
           (unread-chats (telega-filter-chats folder-chats 'unread))
           (nchats (length unread-chats)))
      (when (and (> nchats 0)
                 (y-or-n-p (format "telega: Read all %d chats from «%s»? "
                                   nchats (nth 0 custom-filter))))
        (telega--readChatList
         (telega-folder--tdlib-chat-list (nth 0 custom-filter)))))))

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
                       (telega-tl-str (car telega-tdlib--chat-folders) :title)))
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

(defun telega-filter--custom-name (custom)
  "Return name for the CUSTOM chat filter.
Possibly apply i18n."
  (let ((custom-name (car custom)))
    (or (when (string-prefix-p "lng_" custom-name)
          (telega-i18n-noerror custom-name))
        custom-name)))

(defun telega-filter--custom-active-p (custom)
  "Return non-nil if CUSTOM filter is active chat filter.
Actually return active chat filter corresponding to CUSTOM filter."
  (let ((active-filter (telega-filter-active)))
    (car (or (member (nth 1 custom) active-filter)
             (member (list 'custom (telega-filter--custom-name custom))
                     active-filter)))))

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
Used when `updateChatFolders' is received."
  (telega-ewoc--clean telega-filters--ewoc)

  (dolist (custom (append telega-filters-custom
                          (when telega-filter-custom-show-folders
                            (mapcar #'telega-filter--custom-folder-spec
                                    telega-tdlib--chat-folders))))
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

(defun telega-chat-match-active-p (chat)
  "Return non-nil if CHAT matches active chat filter."
  (telega-chat-match-p chat (telega-filter-active-prepare 'with-root-view)))

(defun telega-filter-chats (chat-list &optional chat-temex)
  "Match chats in CHAT-LIST against CHAT-TEMEX.
Return list of chats matching CHAT-TEMEX.
Return only chats with non-0 order.
If CHAT-TEMEX is ommited, then active chat filter from
`telega--filters' is used as CHAT-TEMEX."
  (declare (indent 1))
  (unless chat-temex
    (setq chat-temex (telega-filter-active-prepare 'with-root-view)))

  (cl-remove-if-not
   (telega-match-gen-predicate 'chat chat-temex)
   chat-list))

(defun telega-filter-by-filter (filter-name)
  "Interactively select a Chat filter to add to active filter."
  ;; Query user for filter with corresponding interactive function, or
  ;; filter without argument
  (interactive
   (list
    (let* ((fprefix "telega-match--primitive-chat-")
           (filter-funs
            (cl-remove-if
             (lambda (funsym)
               (let ((funargs (help-function-arglist funsym)))
                 (and (> (length funargs) 1)
                      (not (eq '&optional (nth 1 funargs))))))
             (apropos-internal (concat "^" fprefix "[a-z-]+") 'functionp)))
           (filter-names
            (mapcar (lambda (funame)
                      (substring funame (length fprefix)))
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

(defun telega-filters-negate (arg)
  "Negate last filter.
If `\\[universal-argument]' is specified, then negate whole active filter."
  (interactive "P")
  (telega-filters-push
   (if arg
       (list `(not ,(telega-filter-active-prepare)))
     (append (butlast (telega-filter-active))
             (list `(not ,(car (last (telega-filter-active)))))))))

(defun telega-filter-by-type (ctype)
  "Filter chats by CHAT-TYPE.
CHAT-TYPE is a symbol, one of `telega-chat-types'."
  (interactive
   (list (intern (funcall telega-completing-read-function
                          "Chat type: "
                          (mapcar #'symbol-name telega-chat-types)
                          nil t))))
  (telega-filter-add (list 'type ctype)))

(defun telega-filter-by-name (regexp)
  "Filter by REGEXP matching chat's title.
Use `telega-filter-by-name' for fuzzy searching."
  (interactive (list (read-regexp "Chat name regexp: ")))
  (telega-filter-add (list 'name regexp)))

(defun telega-filter-by-search (query)
  "Filter chats by QUERY."
  (interactive (list (read-string "Chat search query: ")))
  (setq telega--search-chats (telega--searchChats query))
  (telega-filter-add (list 'search query)))

(defun telega-filter-by-nearby ()
  "Filter chats nearby `telega-my-location'."
  (interactive)
  (unless telega-my-location
    (user-error "`telega-my-location' is unset, can't search nearby chats"))
  (telega--searchChatsNearby telega-my-location)
  (telega-filter-add 'nearby))

(defun telega-filter-by-custom (name)
  "Filter by custom chat filter."
  (interactive (list (let ((completion-ignore-case t))
                       (funcall telega-completing-read-function
                        "Custom filter: "
                        (mapcar #'telega-filter--custom-name
                                telega-filters-custom)
                        nil t))))
  (telega-filter-add (list 'custom name)))

(defun telega-filter-by-pin ()
  "Filter only pinned chats."
  (interactive)
  (telega-filter-add '(prop :is_pinned)))

(defun telega-filter-by-unread (n)
  "Filter chats with at least N unread messages."
  (interactive "p")
  (if (= n 1)
      (telega-filter-add 'unread)
    (telega-filter-add (list 'unread n))))

(defun telega-filter-by-mention (n)
  "Filter chats with at least N unread mentions."
  (interactive "p")
  (telega-filter-add (list 'mention n)))

(defun telega-filter-by-unmuted ()
  "Filter chats with enabled notifications."
  (interactive)
  (telega-filter-add 'unmuted))

(defun telega-filter-by-important ()
  "Filter important chats."
  (interactive)
  (telega-filter-add 'important))

(defun telega-filter-by-online-status (status)
  "Filter private chats by its user online STATUS."
  (interactive (let ((completion-ignore-case t))
                 (list (funcall telega-completing-read-function
                        "User status: "
                        '("Online" "Recently" "LastWeek" "LastMonth"
                          "Offline" "Empty")
                        nil t))))
  (telega-filter-add `(user (status ,status))))

(defun telega-filter-by-verified ()
  "Filter verified chats."
  (interactive)
  (telega-filter-add 'verified))

(defun telega-filter-by-my-public-chats (chat-type)
  "Filter public chats created by me.
CHAT-TYPE is one of `has-username' or `location-based'."
  (interactive
   (list (intern (completing-read "Public Chat Type: "
                                  '("has-username" "location-based") nil t))))
  (telega-filter-add
   (cons 'ids (mapcar (telega--tl-prop :id)
                      (telega--getCreatedPublicChats chat-type)))))

(defun telega-filter-by-has-chatbuf ()
  "Filter chats that has corresponding chat buffer."
  (interactive)
  (telega-filter-add 'has-chatbuf))

(defun telega-filter-by-permission (perm)
  "Filter chats by allowed permission PERM."
  (interactive (list (telega-completing-read-permission
                      "Chat permission: " telega-chat--chat-permissions)))
  (telega-filter-add (list 'permission perm)))

(defun telega-filter-by-my-permission (perm)
  "Filter chats by allowed permission PERM for me."
  (interactive (list (telega-completing-read-permission
                      "Chat permission: "
                      (append telega-chat--chat-permissions
                              telega-chat--admin-permissions))))
  (telega-filter-add (list 'my-permission perm)))

(defun telega-filter-by-restriction ()
  "Filter chats by restriction reason.
To specify suffixes use `/ e' command and edit filter string directly."
  (interactive)
  (telega-filter-add 'restriction))

(defun telega-filter-by-contact (&optional mutual-p)
  "Filter chats with users that are in contacts.
Specify MUTUAL-P to filter only mutual contacts."
  (interactive "P")
  (telega-filter-add (list 'user (if mutual-p
                                     '(contact mutual)
                                   'contact))))

(defun telega-filter-by-top ()
  "Filter top used chats by CATEGORY."
  (interactive)
  (telega-filter-add 'top))

(defun telega-filter-by-tracking ()
  "Matches if chat is in tracking buffers list."
  (interactive)
  (telega-filter-add 'tracking))

(defun telega-filter-by-folder (folder)
  "Match chats by Telegram FOLDER."
  (interactive (list (telega-completing-read-folder "Telegram Folder: ")))
  (telega-filters-push `((folder ,(substring-no-properties folder)))))

(defun telega-filter-by-inactive-supergroups ()
  "Filter inactive supergroups.
Can be used when user reaches limit on the number of joined
supergroups and channels and receives CHANNELS_TOO_MUCH error."
  (interactive)
  (setq telega--search-chats (telega--getInactiveSupergroupChats))
  (telega-filter-add 'inactive-supergroups))

(defun telega-filter-by-can-send-stories ()
  "Filter chats you can post stories to."
  (interactive)
  (setq telega--search-chats (telega--getChatsToSendStories))
  (telega-filter-add 'can-send-stories))

(defun telega-filter-by-has-video-chat (including-empty-p)
  "Filter chats with started video chat.
If INCLUDING-EMPTY-P is non-nil, then keep also empty video chats."
  (interactive (list (y-or-n-p "Include empty/scheduled video chats? ")))
  (telega-filter-add (if including-empty-p
                         'has-video-chat
                       '(has-video-chat with-users))))

(defun telega-filter-by-has-groups-in-common ()
  "Filter chats with users me has groups in common."
  (interactive)
  (telega-filter-add '(user groups-in-common)))

(defun telega-filter-by-is-telega-patron ()
  "Filter chats with telega patrons."
  (interactive)
  (telega-filter-add '(user is-telega-patron)))

(defun telega-filter-by-unread-reactions (n)
  "Filter chats having at least N unread reactions."
  (interactive "p")
  (telega-filter-add (list 'unread-reactions n)))

(defun telega-filter-by-last-message-from-me ()
  "Filter chats where last message's sender is me."
  (interactive)
  (telega-filter-add '(last-message (sender me))))

(defun telega-filter-by-is-forum ()
  "Filter chats which are forums."
  (interactive)
  (telega-filter-add 'is-forum))

(provide 'telega-filter)

;;; telega-filter.el ends here

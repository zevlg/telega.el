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
;; *TODO*: describe parts of the rootbuf: status, custom-filters,
;; *folders, active chat filter, active chat sorter
;;
;; rootbuf lists chats filtered by active chat filter.  Press
;; {{{where-is(telega-describe-chat,telega-chat-button-map)}}} to get
;; detailed description of the chat at point.

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

(declare-function telega-chat--update "telega-tdlib-events" (chat &rest dirtiness))
(declare-function telega-chats-dirty--update "telega-tdlib-events")
(declare-function telega-chat--mark-dirty "telega-tdlib-events" (chat &optional event))

(declare-function telega-account-current "telega")
(declare-function telega-account-switch "telega" (account))

(defvar telega-temex-remap-list)


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
(defvar telega-root-aux--ewoc nil
  "Auxiliary data shown in the rootbuf.
Use `telega-root-aux-inserters' to customize it.")
(defvar telega-status--timer nil
  "Timer used to animate status string.")
(defvar telega-loading--timer nil
  "Timer used to animate Loading.. for root view ewocs.")
(defvar telega-online--timer nil
  "Timer used to change online status.")
(defvar telega-idle--timer nil
  "Runs when Emacs gets idle.")

(defvar telega-root-view-map
  (let ((map (make-sparse-keymap)))
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-search,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-search, 2)}}}
    (define-key map (kbd "s") 'telega-view-search)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-nearby,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-nearby, 2)}}}
    (define-key map (kbd "n") 'telega-view-nearby)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-reset,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-reset, 2)}}}
    (define-key map (kbd "v") 'telega-view-reset)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-compact,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-compact, 2)}}}
    (define-key map (kbd "0") 'telega-view-compact)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-one-line,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-one-line, 2)}}}
    (define-key map (kbd "1") 'telega-view-one-line)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-two-lines,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-two-lines, 2)}}}
    (define-key map (kbd "2") 'telega-view-two-lines)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-topics,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-topics, 2)}}}
    ;;
    ;;   Customizable options:
    ;;   - {{{user-option(telega-root-view-topics, 4)}}}
    ;;   - {{{user-option(telega-root-view-topics-folders, 4)}}}
    ;;   - {{{user-option(telega-root-view-topics-other-chats, 4)}}}
    (define-key map (kbd "t") 'telega-view-topics)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-files,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-files, 2)}}}
    ;;
    ;;   If you use this view frequently, consider setting
    ;;   ~telega-chat-upload-attaches-ahead~ to nil, to avoid file
    ;;   duplications for "uploading" kind. See
    ;;   https://github.com/tdlib/td/issues/1348#issuecomment-752654650
    ;;   for details
    ;;
    ;;   Press {{{kbd(d)}}} under downloaded filename to delete the
    ;;   file.  Only files cached by TDLib in the ~telega-cache-dir~
    ;;   can be deleted.
    ;;
    ;;   Customizable options:
    ;;   - {{{user-option(telega-root-view-files-exclude-subdirs, 4)}}}
    ;;   - {{{user-option(telega-chat-upload-attaches-ahead, 4)}}}
    (define-key map (kbd "F") 'telega-view-files)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-top,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-top, 2)}}}
    ;;
    ;;   Customizable options:
    ;;   - {{{user-option(telega-root-view-top-categories, 4)}}}
    (define-key map (kbd "T") 'telega-view-top)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-settings,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-settings, 2)}}}
    (define-key map (kbd "S") 'telega-view-settings)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-contacts,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-contacts, 2)}}}
    (define-key map (kbd "c") 'telega-view-contacts)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-calls,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-calls, 2)}}}
    (define-key map (kbd "C") 'telega-view-calls)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-last-messages,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-last-messages, 2)}}}
    (define-key map (kbd "l") 'telega-view-last-messages)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-folders,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-folders, 2)}}}
    (define-key map (kbd "f") 'telega-view-folders)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-deleted-chats,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-deleted-chats, 2)}}}
    (define-key map (kbd "d") 'telega-view-deleted-chats)
    ;;; ellit-org: rootbuf-view-bindings
    ;; - {{{where-is(telega-view-favorite-messages,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-favorite-messages, 2)}}}
    (define-key map (kbd "*") 'telega-view-favorite-messages)
    map)
  "Keymap for Root View commands.")

(defvar telega-folder-map
  (let ((map (make-sparse-keymap)))
    ;;; ellit-org: rootbuf-folder-bindings
    ;; - {{{where-is(telega-folder-create,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-folder-create, 2)}}}
    (define-key map (kbd "+") 'telega-folder-create)
    ;;; ellit-org: rootbuf-folder-bindings
    ;; - {{{where-is(telega-folder-delete,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-folder-delete, 2)}}}
    (define-key map (kbd "-") 'telega-folder-delete)
    ;;; ellit-org: rootbuf-folder-bindings
    ;; - {{{where-is(telega-folders-reorder,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-folders-reorder, 2)}}}
    (define-key map (kbd "=") 'telega-folders-reorder)
    ;;; ellit-org: rootbuf-folder-bindings
    ;; - {{{where-is(telega-folder-rename,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-folder-rename, 2)}}}
    (define-key map (kbd "R") 'telega-folder-rename)
    ;;; ellit-org: rootbuf-folder-bindings
    ;; - {{{where-is(telega-folder-set-icon,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-folder-set-icon, 2)}}}
    (define-key map (kbd "I") 'telega-folder-set-icon)

    ;;; ellit-org: rootbuf-folder-bindings
    ;; - {{{where-is(telega-chat-add-to-folder,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-chat-add-to-folder, 2)}}}
    (define-key map (kbd "a") 'telega-chat-add-to-folder)
    ;;; ellit-org: rootbuf-folder-bindings
    ;; - {{{where-is(telega-chat-remove-from-folder,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-chat-remove-from-folder, 2)}}}
    (define-key map (kbd "d") 'telega-chat-remove-from-folder)
    map)
  "Keymap for Folder commands in the rootbuf.")

(defvar telega-describe-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "w") 'telega-describe-connected-websites)
    (define-key map (kbd "s") 'telega-describe-active-sessions)
    (define-key map (kbd "n") 'telega-describe-network)
    (define-key map (kbd "y") 'telega-describe-notifications)
    (define-key map (kbd "N") 'telega-describe-notifications)
    (define-key map (kbd "p") 'telega-describe-privacy-settings)
    map)
  "Keymap for help/describe commands.")

(defvar telega-voip-map
  (let ((map (make-sparse-keymap)))
    ;;; ellit-org: rootbuf-voip-bindings
    ;; - {{{where-is(telega-chat-call,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-chat-call,2)}}}
    (define-key map (kbd "c") 'telega-chat-call)
    ;;; ellit-org: rootbuf-voip-bindings
    ;; - {{{where-is(telega-voip-accept,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-voip-accept,2)}}}
    (define-key map (kbd "a") 'telega-voip-accept)
    ;;; ellit-org: rootbuf-voip-bindings
    ;; - {{{where-is(telega-voip-discard,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-voip-discard,2)}}}
    (define-key map (kbd "d") 'telega-voip-discard)
    ;;; ellit-org: rootbuf-voip-bindings
    ;; - {{{where-is(telega-voip-buffer-show,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-voip-buffer-show,2)}}}
    (define-key map (kbd "b") 'telega-voip-buffer-show)
    ;;; ellit-org: rootbuf-voip-bindings
    ;; - {{{where-is(telega-view-calls,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-view-calls,2)}}}
    (define-key map (kbd "l") 'telega-view-calls)
    map)
  "Keymap for VoIP commands.")

(defvar telega-root-fastnav-map
  (let ((map (make-sparse-keymap)))
    ;;; ellit-org: rootbuf-fastnav-bindings
    ;; - {{{where-is(telega-root-next-unread,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-root-next-unread, 2)}}}
    (define-key map (kbd "u") 'telega-root-next-unread)

    ;;; ellit-org: rootbuf-fastnav-bindings
    ;; - {{{where-is(telega-root-next-important,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-root-next-important, 2)}}}
    ;;
    ;;   Chat is important if matches ~telega-important-chat-temex~
    ;;   [[#telega-match-expressions][temex]].
    (define-key map (kbd "i") 'telega-root-next-important)

    ;;; ellit-org: rootbuf-fastnav-bindings
    ;; - {{{where-is(telega-root-next-mention,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-root-next-mention, 2)}}}
    (define-key map (kbd "m") 'telega-root-next-mention)
    (define-key map (kbd "@") 'telega-root-next-mention)

    ;;; ellit-org: rootbuf-fastnav-bindings
    ;; - {{{where-is(telega-root-next-reaction,telega-root-mode-map)}}} ::
    ;;   {{{fundoc(telega-root-next-reaction, 2)}}}
    (define-key map (kbd "!") 'telega-root-next-reaction)
    map)
  "Keymap for fast navigation commands in the rootbuf.")

(defvar telega-root-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'telega-button-forward)
    (define-key map "p" 'telega-button-backward)
    (define-key map [?\t] 'telega-button-forward)
    (define-key map "\e\t" 'telega-button-backward)
    (define-key map [backtab] 'telega-button-backward)

    (define-key map (kbd "\\") telega-sort-map)

    ;; Chat Filter commands
    (define-key map (kbd "/") telega-filter-map)
    (define-key map (kbd "C-/") 'telega-filter-undo)
    (define-key map (kbd "C-_") 'telega-filter-undo)
    (define-key map (kbd "C-x C-/") 'telega-filter-redo)
    (define-key map (kbd "C-x C-_") 'telega-filter-redo)

    (define-key map (kbd "J") 'telega-chat-join-by-link)
    (define-key map (kbd "N") 'telega-chat-create)
    ;; Commands to all currently filtered chats

    ;; NOTE: Deleting all chats is very-very-very dangerous, so
    ;; disabled, use M-x telega-chats-filtered-delete RET if you know
    ;; what you are doing
    ;; (define-key map (kbd "D") 'telega-chats-filtered-delete)
    (define-key map (kbd "K") 'telega-chats-filtered-kill-chatbuf)
    (define-key map (kbd "R") 'telega-chats-filtered-toggle-read)

    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "Q") 'telega-kill)

    ;; Additional prefix keymaps
    (define-key map (kbd "?") telega-describe-map)
    (define-key map (kbd "F") telega-folder-map)
    (define-key map (kbd "c") telega-voip-map)
    (define-key map (kbd "M-g") telega-root-fastnav-map)
    (define-key map (kbd "v") telega-root-view-map)

    (define-key map (kbd "s") 'telega-view-search)
    map)
  "The key map for telega root buffer.")

;;; ellit-org: minor-modes
;; ** telega-root-auto-fill-mode
;;
;; Global minor mode to automatically adjust ~telega-root-fill-column~
;; to the width of the window displaying rootbuf.
;;
;; ~telega-root-auto-fill-mode~ is enabled by default.
(define-minor-mode telega-root-auto-fill-mode
  "Toggle rootbuf autofill mode."
  :init-value nil
  :global t :group 'telega-modes
  (if telega-root-auto-fill-mode
      (with-telega-root-buffer
        (add-hook 'window-size-change-functions
                  #'telega-root-buffer-auto-fill nil 'local)
        (add-hook 'text-scale-mode-hook
                  #'telega-root-buffer-auto-fill nil 'local)
        (when-let ((rootbuf-win (get-buffer-window)))
          (telega-root-buffer-auto-fill rootbuf-win)))

    (with-telega-root-buffer
      (remove-hook 'text-scale-mode-hook
                   #'telega-root-buffer-auto-fill 'local)
      (remove-hook 'window-size-change-functions
                   #'telega-root-buffer-auto-fill 'local))))

(define-derived-mode telega-root-mode nil
  '((:eval (telega-symbol 'mode))
    "Root"
    (telega-root-auto-fill-mode
     "[autofill]" (:eval (format "[%d]" telega-root-fill-column))))
  "The mode for telega root buffer.

Chat bindings (cursor on chat):
\\{telega-chat-button-map}
Global root bindings:
\\{telega-root-mode-map}"
  :group 'telega-root

  (unless (eq (current-buffer) (telega-root--buffer))
    (error "telega: Can't enable `telega-root-mode' in random buffer"))

  (setq-local nobreak-char-display nil)
  ;; NOTE: make `telega-root-keep-cursor' working as expected
  (setq-local switch-to-buffer-preserve-window-point nil)

  (setq buffer-read-only nil)
  (erase-buffer)

  ;; Ewoc for rootbuf auxiliary inserters
  (setq telega-root-aux--ewoc
        (ewoc-create (telega-ewoc--gen-pp #'telega-root-aux--pp) nil nil t))
  ;; Status inserter always goes first
  (ewoc-enter-last telega-root-aux--ewoc #'telega-ins--status)
  ;; Then goes additional inserters
  (dolist (aux-inserter telega-root-aux-inserters)
    (ewoc-enter-last telega-root-aux--ewoc aux-inserter))
  (goto-char (point-max))

  ;; Delim between ewocs
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

  (setq truncate-lines t)
  (setq buffer-read-only t)
  (add-hook 'kill-buffer-hook 'telega-root--killed nil t)

  (cursor-sensor-mode 1)
  (when telega-use-tracking-for
    (unless (fboundp 'tracking-mode)
      (user-error "Please install `tracking' package \
to make use of `telega-use-tracking-for'"))
    (tracking-mode 1))

  ;; NOTE: If rootbuf autofill mode was enabled before telega start,
  ;; then reapply it, because it uses buffer local hook
  (when telega-root-auto-fill-mode
    (telega-root-auto-fill-mode 1))

  (telega-runtime-setup))

(defun telega-root-aux--pp (inserter)
  "Pretty printer for rootbuf aux INSERTER from `telega-root-aux-inserters'."
  ;; NOTE: special case for status inserter, no need in leading newline
  (telega-ins-prefix (unless (eq inserter #'telega-ins--status) "\n")
    (funcall inserter)))

(defun telega-root-aux-redisplay (&optional item)
  "Redisplay auxiliary ITEM.
ITEM is a corresponding inserter from the `telega-root-aux-inserters'.
If ITEM is not given, then redisplay whole aux ewoc."
  (with-telega-root-buffer
    (telega-save-cursor
      (if item
          (when-let ((aux-node (telega-ewoc--find-by-data
                                telega-root-aux--ewoc item)))
            (ewoc-invalidate telega-root-aux--ewoc aux-node))

        ;; NOTE: `telega-root-aux-inserters' might have changed, so
        ;; recreate all items
        (telega-ewoc--clean telega-root-aux--ewoc)
        (dolist (aux-inserter telega-root-aux-inserters)
          (ewoc-enter-last telega-root-aux--ewoc aux-inserter))))))

(defun telega-root-aux-append (inserter)
  "Append INSERTER to root aux inserters."
  (unless (memq inserter telega-root-aux-inserters)
    (setq telega-root-aux-inserters
          (append telega-root-aux-inserters (list inserter)))
    (with-telega-root-buffer
      (telega-save-excursion
        (ewoc-enter-last telega-root-aux--ewoc inserter)))))

(defun telega-root-aux-remove (inserter)
  "Remove root aux INSERTER."
  (setq telega-root-aux-inserters
        (delq inserter telega-root-aux-inserters))
  (with-telega-root-buffer
    (telega-save-excursion
      (when-let ((node (telega-ewoc--find-by-data
                        telega-root-aux--ewoc inserter)))
        (ewoc-delete telega-root-aux--ewoc node)))))


(defun telega-root--killed ()
  "Run when telega root buffer is killed.
Terminate telega-server and kill all chat and supplementary buffers."
  (when telega-status--timer
    (cancel-timer telega-status--timer))
  (when telega-loading--timer
    (cancel-timer telega-loading--timer))
  (when telega-online--timer
    (cancel-timer telega-online--timer))

  (telega-runtime-teardown)

  ;; NOTE: Kill all telega buffers, except for root buffer to avoid
  ;; infinite kill buffer loop, because `telega-root--killed' is
  ;; called when root buffer is killed
  (dolist (tbuf (cl-remove-if-not #'telega-buffer-p (buffer-list)))
    (unless (eq tbuf (telega-root--buffer))
      (kill-buffer tbuf)))
  (telega-server-kill))

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
`:on-notifications-update', `:on-file-update',
`:on-group-call-update', `:loading'."
  (cl-assert (stringp (plist-get ewoc-spec :name)))
  (let ((ewoc (ewoc-create (telega-ewoc--gen-pp
                            (plist-get ewoc-spec :pretty-printer))
                           (telega-root-view--ewoc-header
                            (plist-get ewoc-spec :header))
                           (or (plist-get ewoc-spec :footer)
                               (when (plist-get ewoc-spec :loading)
                                 (concat (telega-i18n "lng_profile_loading")
                                         "\n")))
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

(defun telega-root-view--ewoc-loading-start (ewoc-name loading &optional footer)
  "Start loading in the root view ewoc named EWOC-NAME.
LOADING is extra value from corresponding TDLib request.
FOOTER is string to use as footer, by default \"Loading...\" is used."
  (declare (indent 1))
  (let ((ewoc-spec (telega-root-view--ewoc-spec ewoc-name)))
    (cl-assert (not (plist-get ewoc-spec :loading)))
    (plist-put ewoc-spec :loading loading)
    (with-telega-root-view-ewoc (plist-get ewoc-spec :name) ewoc
      (telega-save-cursor
        (telega-ewoc--set-footer
         ewoc (concat (or footer (telega-i18n "lng_profile_loading")) "\n"))))
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

        (seq-doseq (item (sort items (telega-root-view--ewoc-sorter ewoc-name)))
          (ewoc-enter-last ewoc item)))

      (run-hooks 'telega-root-update-hook))))

(defun telega-root--keep-cursor-at-chat (chat)
  "Keep cursor position at CHAT.
Keep cursor position only if CHAT is visible."
  (when (telega-chat-match-active-p chat) ;visible-p
    (with-telega-root-view-ewoc "root" root-ewoc
      (when-let ((node (telega-ewoc--find-by-data root-ewoc chat)))
        (goto-char (ewoc-location node))

        ;; NOTE: if chatbuf is opened with topic, then try to find
        ;; corresponding topic button next to the chat button
        (when-let* ((topic (with-telega-chatbuf chat
                             (telega-chatbuf--thread-topic)))
                    (topics-ewoc (button-get (button-at (point)) :topics-ewoc))
                    (topic-node (telega-ewoc--find-by-data topics-ewoc topic)))
          (goto-char (ewoc-location topic-node)))

        (unless (get-buffer-window)
          (telega-buffer--hack-win-point))
        (dolist (win (get-buffer-window-list))
          (set-window-point win (point)))
        (run-hooks 'telega-root-update-hook)))))

(defun telega-root-buffer-auto-fill (&optional win)
  "Automatically resize root buffer formatting to WIN's width."
  (interactive (list (get-buffer-window)))
  (unless (eq (window-buffer win) (telega-root--buffer))
    (user-error (concat "telega: `telega-root-buffer-auto-fill' "
                        "can be called only in Root Buffer.")))

  ;; NOTE: refill only if WIN is selected, making
  ;; `(line-number-display-width)' to work correctly
  (when (or (null win) (eq (selected-window) win))
    ;; NOTE: `window-width' does not regard use of both
    ;; `text-scale-increase' or `text-scale-decrease'.  So we manually
    ;; calculate window width in characters
    ;;
    ;; Also, take into account width occupied by
    ;; `display-line-numbers-mode', see
    ;; https://github.com/zevlg/telega.el/issues/325
    (let ((new-fill-column (1- (/ (- (window-width win 'pixels)
                                     (line-number-display-width 'pixels))
                                  (telega-chars-xwidth 1)))))
      (when (and new-fill-column
                 (> new-fill-column 15)   ;XXX ignore too narrow window
                 (not (eq new-fill-column telega-root-fill-column)))
        (let ((progress (make-progress-reporter
                         (format "telega: rootbuf auto fill %d -> %d ..."
                                 telega-root-fill-column new-fill-column))))
          (with-telega-root-buffer
            (setq telega-root-fill-column new-fill-column)
            ;; Fully redisplay filters
            (let ((telega-filters--dirty t))
              (telega-filters--redisplay))
            ;; Redisplay Root View header and all its ewocs
            (telega-save-cursor
              (goto-char telega-root-view--header-marker)
              (telega-root-view--ins-header telega-root--view)
              (delete-region (point) telega-root-view--ewocs-marker)

              (dolist (ewoc-spec (nthcdr 2 telega-root--view))
                (with-telega-root-view-ewoc (plist-get ewoc-spec :name) ewoc
                  (when-let ((ewoc-hdr (telega-root-view--ewoc-header
                                        (plist-get ewoc-spec :header))))
                    (telega-ewoc--set-header ewoc ewoc-hdr))
                  (ewoc-refresh ewoc))))
            (run-hooks 'telega-root-update-hook))

          (progress-reporter-done progress))))))


;;; Pretty Printers for root view ewocs
(defun telega-chat-button-toggle-view (chat)
  "Toogle view for CHAT button."
  (interactive (list (telega-chat-at (point))))
  (if (telega-chat-match-p chat 'is-forum)
      (progn
        (plist-put chat :telega-topics-visible
                   (not (plist-get chat :telega-topics-visible)))
        (telega-root-view--update :on-chat-update chat))

    ;; TODO: toogle preview for the last message
    ))

(defun telega-chat-button-action (chat)
  "Action to take when CHAT button is pushed in the rootbuf.
If `\\[universal-argument]' is specified, then open chat in a preview mode."
  ;; NOTE: load chat history only after toggling preview mode, it
  ;; might affect how initial history is loaded, ref
  ;; `telega-chat-preview-mode-from-last-message'
  (with-current-buffer (telega-chat--pop-to-buffer chat :no-history-load)
    ;; Possibly need to toggle preview mode depending on
    ;; universal-argument
    (cond ((and current-prefix-arg (not telega-chat-preview-mode))
           (telega-chat-preview-mode 1))

          ((and (not current-prefix-arg) telega-chat-preview-mode)
           (telega-chat-preview-mode 0)))

    ;; NOTE: load history only if there is no messages yet, i.e. new
    ;; chatbuf has been created
    (unless (or telega-chatbuf--history-loading
                (telega-chatbuf--last-msg))
      (telega-chatbuf--load-initial-history))))

(defun telega-topic-button-action (topic)
  "Action to take when TOPIC button is pushed in the rootbuf.
If `\\[universal-argument]' is specified, then open topic in a preview mode."
  (telega-topic-goto topic (plist-get topic :last_read_outbox_message_id)))

(defun telega-root--chat-topic-pp (topic &optional custom-topic-inserter)
  "Pretty printer for TOPIC button."
  (telega-button--insert 'telega-topic topic
    :inserter (or custom-topic-inserter
                  telega-inserter-for-topic-button)
    :action #'telega-topic-button-action)
  (telega-ins "\n"))

(defun telega-root--chat-pp (chat &optional custom-inserter custom-action)
  "Pretty printer for any CHAT button."
  (let ((chat-button (telega-button--insert 'telega-chat chat
                       :inserter (or custom-inserter
                                     telega-inserter-for-chat-button)
                       :action (or custom-action #'telega-chat-button-action))))
    (telega-ins "\n")

    (when-let ((topics-ewoc (when (plist-get chat :telega-topics-visible)
                              (ewoc-create #'telega-root--chat-topic-pp
                                           nil nil 'no-sep))))
      (button-put chat-button :topics-ewoc topics-ewoc)
      (seq-doseq (topic (telega-chat-topics chat))
        (ewoc-enter-last topics-ewoc topic))
      ;; NOTE: `ewoc-enter-last' inserts under `save-excursion', but
      ;; we need point to move forward.  So, move point to the end of
      ;; the topic ewocs making topics ewoc part of the chat node
      (goto-char (ewoc-location (ewoc--footer topics-ewoc))))
    ))

(defun telega-root--chat-known-pp (chat &optional custom-inserter custom-action)
  "Pretty printer for known CHAT button."
  ;; Insert only visible chat buttons
  ;; See https://github.com/zevlg/telega.el/issues/3
  (let ((visible-p (telega-chat-match-active-p chat)))
    (when visible-p
      (telega-root--chat-pp chat custom-inserter custom-action))))

(defun telega-root--global-chat-button-action (chat)
  "Action to take when global CHAT is opened.
Adds CHAT to recently found chats list."
  (unless (telega-chat-match-p chat 'is-known)
    (telega--addRecentlyFoundChat chat))
  (telega-chat-button-action chat))

(defun telega-root--global-chat-pp (chat &optional custom-inserter)
  "Display CHAT found in global public chats search."
  (let ((telega-chat-button-width
         (round (* (telega-canonicalize-number telega-chat-button-width
                                               telega-root-fill-column)
                   1.5)))
        (telega-chat-button-format-plist
         (list :with-title-faces-p nil
               :with-username-p 'telega-username
               :with-members-trail-p t
               :with-status-icons-trail-p t))
        (telega-temex-remap-list '((chat chat-list . (return t)))))
    (telega-root--chat-known-pp chat custom-inserter
                                #'telega-root--global-chat-button-action)))

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
                        (let ((telega-temex-remap-list
                               '((chat chat-list . (return t)))))
                          (telega-chat-match-active-p user-chat)))))
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
      (let ((telega-chat-button-width
             (round (* (telega-canonicalize-number telega-chat-button-width
                                                   telega-root-fill-column)
                       (/ 3.0 2)))))
        (telega-root--chat-known-pp
         chat
         #'telega-ins--chat-last-message
         #'telega-root--chat-goto-last-message)))))

(defun telega-root--message-pp (msg &optional custom-inserter)
  "Pretty printer for MSG button shown in root buffer."
  (declare (indent 1))
  (let ((visible-p (telega-chat-match-active-p (telega-msg-chat msg))))
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
(defun telega-ins--status ()
  "Inserter for the telega status.
Status values are hold in the `telega--status' and
`telega--status-aux' variables."
  (telega-ins "Status")
  (when-let (account (telega-account-current))
    (let ((user-me (telega-user-me 'offline)))
      (telega-ins " (")
      (when user-me
        (telega-ins--image
         (telega-msg-sender-avatar-image-one-line user-me)))
      (telega-ins--button (car account)
        'face 'bold
        'action (lambda (_ignored)
                  (call-interactively #'telega-account-switch))
        'help "Switch to another account")
      (when user-me
        (telega-ins--user-emoji-status user-me))
      (telega-ins ")")))
  (unless (eq 'other telega-network-type)
    (telega-ins-fmt " [%S]" telega-network-type))
  (telega-ins ": " telega--status)
  (unless (string-empty-p telega--status-aux)
    (when (< (current-column) 30)
      (telega-ins (make-string (- 30 (current-column)) ?\s)))
    (telega-ins "  ")
    (telega-ins telega--status-aux))
  t)

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
             (cancel-timer telega-status--timer)
             (setq telega-status--timer nil))))

    (with-telega-root-buffer
      (setq mode-line-process (concat ":" telega--status))
      (telega-root-aux-redisplay #'telega-ins--status))))

(defun telega-root--on-chat-update0 (ewoc-name ewoc chat &optional chat-node)
  "Update CHAT in EWOC named EWOC-NAME.
CHAT could be new to the ewoc, in this case create new node.
CHAT-NODE is EWOC's node for the CHAT."
  (unless chat-node
    (setq chat-node (telega-ewoc--find-by-data ewoc chat)))

  (if (and chat-node (not (telega-chat-order-dirty-p chat)))
      (telega-ewoc--move-node ewoc chat-node chat-node
                              telega-root-keep-cursor)

    ;; Reorder needed or new chat created
    (let* ((cmp-func (telega-root-view--ewoc-sorter ewoc-name #'telega-chat>))
           (before-node (telega-ewoc--find-if ewoc
                          (lambda (echat)
                            (unless (or (eq chat echat)
                                        (telega-chat-order-dirty-p echat))
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

(defun telega-root--any-on-chat-update (ewoc-name ewoc chat)
  "Update CHAT in EWOC.
If corresponding chat node does not exists in EWOC, then create new one."
  (telega-root--on-chat-update0 ewoc-name ewoc chat))

(defun telega-root--existing-on-chat-update (ewoc-name ewoc chat)
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

(defun telega-root--contact-on-chat-update (ewoc-name ewoc chat)
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
           ;; Wrap from the beginning, or backwards from the end of buffer
           ;; if n is negative
           (goto-char (if (> n 0) (point-min) (point-max)))
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
  "Move point to the next important chat."
  (interactive "p")
  (telega-root-next-match-p 'important n 'wrap))

(defun telega-root-next-mention (n)
  "Move point to the next chat with mention."
  (interactive "p")
  (telega-root-next-match-p 'mention n 'wrap))

(defun telega-root-next-reaction (n)
  "Move point to the next chat with unread reaction."
  (interactive "p")
  (telega-root-next-match-p 'unread-reactions n 'wrap))


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

(defun telega-root--found-messages-add (ewoc-name search-func found-messages)
  "Add FOUND-MESSAGES to the EWOC-NAME ewoc."
  (telega-root-view--ewoc-loading-done
   ewoc-name (plist-get found-messages :messages))

  ;; NOTE: if `next_offset' is non-empty, then more messages are
  ;; available
  (when-let ((next-offset (telega-tl-str found-messages :next_offset)))
    (with-telega-root-view-ewoc ewoc-name ewoc
      (telega-save-cursor
        (telega-ewoc--set-footer ewoc
          (telega-ins--as-string
           (telega-ins--button "Load More"
             :value next-offset
             :action search-func)))))))

(defun telega-root--messages-search (&optional offset)
  "Search for messages in all chats."
  (let* ((ewoc-spec (telega-root-view--ewoc-spec "messages"))
         (query (plist-get ewoc-spec :search-query)))
    (cl-assert query)
    (telega-root-view--ewoc-loading-start "messages"
      (telega--searchMessages query
        :chat-list telega-tdlib--chat-list
        :offset offset
        :callback (apply-partially
                   #'telega-root--found-messages-add
                   "messages"
                   #'telega-root--messages-search)))))

(defun telega-root--outgoing-doc-messages-search (&optional _offset)
  (let* ((ewoc-spec (telega-root-view--ewoc-spec "outgoing-doc-messages"))
         (query (plist-get ewoc-spec :search-query)))
    (cl-assert query)
    (telega-root-view--ewoc-loading-start "outgoing-doc-messages"
      (telega--searchOutgoingDocumentMessages query
        :callback (apply-partially
                   #'telega-root--found-messages-add
                   "outgoing-doc-messages"
                   #'telega-root--outgoing-doc-messages-search)))))

(defun telega-root--call-messages-search (&optional offset)
  "Search for call messages."
  (let* ((ewoc-spec (telega-root-view--ewoc-spec "messages"))
         (only-missed-p (plist-get ewoc-spec :only-missed-p)))
    (telega-root-view--ewoc-loading-start "messages"
      (telega--searchCallMessages
        :offset offset
        :only-missed-p only-missed-p
        :callback (apply-partially
                   #'telega-root--found-messages-add
                   "messages"
                   #'telega-root--call-messages-search)))))


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
    (let* ((online-p (funcall telega-online-status-function))
           ;; Me user might be not available yet, at start time
           (me-user (telega-user-me 'locally))
           (curr-online-p (when me-user (telega-user-online-p me-user))))
      (unless (eq online-p curr-online-p)
        (telega--setOption :online (if online-p t :false))

        (run-hook-with-args 'telega-online-status-hook online-p)))))

(defun telega-check-focus-change ()
  "Function called when some emacs frame changes focus."
  ;; Make a decision about online status in `status-interval' seconds
  (let ((status-interval (if (funcall telega-online-status-function)
                             telega-online-status-interval
                           telega-offline-status-interval)))
    (when telega-online--timer
      (cancel-timer telega-online--timer))
    (setq telega-online--timer
          (run-with-timer status-interval nil
                          #'telega-online-status-timer-function)))

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

  ;; TODO: Run some heavy tasks, such as downloading stickers,
  ;; full-filling user info, etc
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
(defun telega-root-view--ins-header (view-spec)
  "Insert root view header at the point."
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
    (telega-ins "\n")))

(defun telega-root-view--update (on-update-prop &rest args)
  "Update root view ewocs using ON-UPDATE-PROP ewoc-spec property and ARGS."
  (let ((rootbuf-updated-p nil))
    (dolist (ewoc-spec (nthcdr 2 telega-root--view))
      (when-let ((on-update-func (plist-get ewoc-spec on-update-prop))
                 (ewoc-name (plist-get ewoc-spec :name)))
        (setq rootbuf-updated-p t)
        (with-telega-root-view-ewoc ewoc-name ewoc
          (apply on-update-func ewoc-name ewoc args))))

    (when rootbuf-updated-p
      (with-telega-root-buffer
        (run-hooks 'telega-root-update-hook)))))

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
      (unless (eq telega-root-default-view-function (car view-spec))
        (telega-root-view--ins-header view-spec))
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

(defun telega-root--on-update-last-message (ewoc-name ewoc msg)
  "Message has been updated, this might affect rootview."
  ;; NOTE: If last message is updated, update message's chat in a
  ;; given ewoc
  (when (telega-msg-match-p msg 'is-last)
    (when-let* ((ewoc-spec (telega-root-view--ewoc-spec ewoc-name))
                (on-chat-update-func (plist-get ewoc-spec :on-chat-update)))
      (funcall on-chat-update-func ewoc-name ewoc (telega-msg-chat msg)))))

(defun telega-view--root-ewoc-spec (&optional custom-inserter)
  "Return view spec for the default root view."
  (list :name "root"
        :pretty-printer (if custom-inserter
                            (lambda (chat)
                              (telega-root--chat-known-pp chat custom-inserter))
                          #'telega-root--chat-known-pp)
        :sorter #'telega-chat>
        :items telega--ordered-chats
        :on-chat-update #'telega-root--any-on-chat-update
        :on-message-update
        (unless (eq custom-inserter #'telega-ins--chat-compact)
          #'telega-root--on-update-last-message)
        ))

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
   'telega-view-compact "Compact" #'telega-ins--chat-compact))

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
               :header (upcase (telega-i18n "lng_contacts_header"))
               :search-query query
               :sorter #'telega-user-cmp-by-status
               :loading (telega--searchContacts query nil
                          (apply-partially
                           #'telega-root-view--ewoc-loading-done "contacts"))
               :on-chat-update #'telega-root--contact-on-chat-update
               :on-user-update #'telega-root--contact-on-user-update)
         (list :name "recent"
               :pretty-printer #'telega-root--global-chat-pp
               :header "RECENT"
               :search-query query
               :sorter #'telega-chat>
               :loading (telega--searchRecentlyFoundChats query nil
                          (apply-partially
                           #'telega-root-view--ewoc-loading-done "recent"))
               :on-chat-update #'telega-root--existing-on-chat-update)
         (list :name "global"
               :pretty-printer #'telega-root--global-chat-pp
               :header "GLOBAL CHATS"
               :search-query query
               :sorter #'telega-chat>
               :loading (telega--searchPublicChats query
                          (apply-partially
                           #'telega-root-view--ewoc-loading-done "global"))
               :on-chat-update #'telega-root--existing-on-chat-update)

         (list :name "outgoing-doc-messages"
               :pretty-printer #'telega-root--message-pp
               :header "OUTGOING DOCUMENTS"
               :search-query query
               :sorter #'telega-root--messages-sorter
               :on-message-update #'telega-root--on-message-update)

         (list :name "messages"
               :pretty-printer #'telega-root--message-pp
               :header (upcase (telega-i18n "lng_settings_messages"))
               :search-query query
               :sorter #'telega-root--messages-sorter
               :on-message-update #'telega-root--on-message-update)
         ))

  ;; NOTE: If query looks like a phone number, also search contact by
  ;; phone number
  (when (string-match-p "+?[0-9]+" query)
    (telega--searchUserByPhoneNumber query
      (lambda (user)
        (unless (telega--tl-error-p user)
          (with-telega-root-view-ewoc "contacts" ewoc
            (ewoc-enter-first ewoc user))))))

  (telega-root--messages-search)
  (telega-root--outgoing-doc-messages-search)
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

(defun telega-root--nearby-on-chat-update (ewoc-name ewoc chat)
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

(defun telega-root--blocked-chat-pp (chat)
  "Pretty printer for blocket CHAT."
  (when (telega-chat-match-p chat 'is-blocked)
    (let ((telega-chat-button-width
           (round (* (telega-canonicalize-number telega-chat-button-width
                                                 telega-root-fill-column)
                     1.5)))
          (telega-temex-remap-list '((chat chat-list . (return t)))))
      (telega-root--chat-known-pp chat))))

(defun telega-view-blocked ()
  "View blocked message senders."
  (interactive)
  (telega-root-view--apply
   (list 'telega-view-blocked
         "Blocked Chats"
         (list :name "blocked"
               :sorter #'telega-chat>
               :pretty-printer #'telega-root--blocked-chat-pp
               :items (telega-filter-chats telega--ordered-chats 'is-blocked)
               :on-chat-update #'telega-root--any-on-chat-update))))

(defun telega-root--topics-on-chat-update (ewoc-name ewoc chat)
  "Handler for chat updates in \"topics\" root view."
  (let ((topic-filter (plist-get (telega-root-view--ewoc-spec ewoc-name)
                                 :topic-filter)))
    (if (telega-chat-match-p chat topic-filter)
        (telega-root--any-on-chat-update ewoc-name ewoc chat)

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
                 `(not (or ,@(mapcar 'cdr telega-root-view-topics)
                           ,@(when telega-root-view-topics-folders
                               (mapcar (apply-partially #'list 'folder)
                                       folder-names))))))
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
  (let* ((me-user (telega-user-get me-id))
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
                 (telega-ins (telega-user-title me-user 'full-name)))
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
               (if-let* ((usernames (plist-get me-user :usernames))
                         (username (plist-get usernames :editable_username)))
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

    ;; Emoji status
    (when (telega-user-match-p me-user 'is-premium)
      (telega-ins "Emoji Status: ")
      (telega-ins--user-emoji-status me-user)
      (telega-ins " ")
      (telega-ins--button "Set Emoji Status"
        'action (lambda (_button)
                  (let ((duration (if current-prefix-arg
                                      (telega-completing-read-emoji-status-duration "Set emoji status for: ")
                                    0)))
                    (telega-sticker-choose-emoji-status
                     (lambda (sticker)
                       (telega--setEmojiStatus
                        (telega-custom-emoji-id sticker) duration))))
                  ;; TODO: set emoji status
                  ))
      (telega-ins "\n"))

    (telega-ins "Profile Photos: ")
    (telega-ins--button "Set Profile Photo"
      'action (lambda (_ignored)
                (let ((photo (read-file-name "Profile Photo: " nil nil t)))
                  (telega--setProfilePhoto photo))))
    (telega-ins "\n")
    (telega-ins--user-profile-photos me-user nil
      (lambda ()
        (with-telega-root-view-ewoc "me" ewoc
          (ewoc-refresh ewoc))))
    (telega-ins "\n")

    (telega-ins--labeled (concat (telega-i18n "lng_profile_bio") " ") nil
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
    (telega-ins--help-message
     (telega-ins-i18n "lng_settings_about_bio"))
    (telega-ins--account-ttl)))

(defun telega-root--me-on-user-update (_ewoc-name ewoc user)
  "Update USER in EWOC."
  (when (telega-me-p user)
    (telega-save-cursor
      (ewoc-refresh ewoc))))

(defun telega-view-settings--option-pp (option-spec)
  "Pretty printer for option in \"settings\" root view."
  (let* ((opt-name (nth 0 option-spec))
         (opt-val (if (plist-member telega--options opt-name)
                      (plist-get telega--options opt-name)
                    (plist-get (telega--getOption opt-name) :value)))
         (opt-title (nth 1 option-spec))
         (opt-about (nth 2 option-spec)))
    ;; Only boolean values are supported
    (cl-assert (memq opt-val '(t nil)))

    (telega-ins--button (if opt-val
                            telega-symbol-heavy-checkmark
                          telega-symbol-blank-button)
      'action (lambda (_ignored)
                (telega--setOption opt-name (not opt-val) 'sync)
                (with-telega-root-view-ewoc "options" opts-ewoc
                  (telega-save-cursor
                    (ewoc-refresh opts-ewoc)))))
    (telega-ins " " opt-title)
    (telega-ins "\n")
    (when opt-about
      (telega-ins--help-message
       (telega-ins opt-about)))
    ))

(defun telega-view-settings--link-pp (link)
  "Pretty printer for link in \"settings\" root view.
LINK is cons, where car is the link description, and cdr is the url."
  (telega-ins (car link) ": ")
  (telega-ins--raw-button (telega-link-props 'url (cdr link) 'face 'telega-link)
    (telega-ins (cdr link)))
  (telega-ins "\n"))

(defun telega-view-settings--notifications-pp (_ignored)
  (telega-describe-notifications--inserter))

(defun telega-view-settings--notifications-update (_name ewoc)
  (let* ((root-win (get-buffer-window))
         (w-start (when root-win (window-start root-win))))
    (telega-save-cursor
      (ewoc-refresh ewoc))
    (when w-start
      (set-window-start root-win w-start 'noforce)
      (set-window-point root-win (point)))))

(defun telega-view-settings ()
  "View and edit your Telegram settings."
  (interactive)
  (telega-root-view--apply
   (list 'telega-view-settings (telega-i18n "lng_menu_settings")
         (list :name "me"
               :pretty-printer #'telega-view-settings--me-pp
               :items (list telega--me-id))
         (list :name "options"
               :pretty-printer #'telega-view-settings--option-pp
               :header "OPTIONS"
               :items
               (append
                (when (plist-get telega--options
                                 :can_ignore_sensitive_content_restrictions)
                  `((:ignore_sensitive_content_restrictions
                     ,(telega-i18n "lng_settings_sensitive_disable_filtering")
                     ,(telega-i18n "lng_settings_sensitive_about"))))
                `((:prefer_ipv6
                   ,(telega-i18n "lng_connection_try_ipv6"))
                  (:disable_time_adjustment_protection
                   "Disable Time Adjustment Protection"
                   "Enabling this significantly reduces disk usage")
                  (:ignore_default_disable_notification
                   "Ignore Default Disable Notification"
                   "Enabling this will ignore per-chat \
Default Disable Notification setting"))
                ;; :disable_contact_registered_notifications
                ;; :disable_top_chats
                ;; :use_quick_ack
                ;; :use_storage_optimizer
                (when (plist-get
                       telega--options
                       :can_archive_and_mute_new_chats_from_unknown_users)
                  `((:archive_and_mute_new_chats_from_unknown_users
                     ,(telega-i18n "lng_settings_auto_archive")
                     ,(telega-i18n "lng_settings_auto_archive_about"))))))
         (list :name "notifications"
               :pretty-printer #'telega-view-settings--notifications-pp
               :header "NOTIFICATIONS"
               :items (list 'dummy-ignored)
               :on-notifications-update
               #'telega-view-settings--notifications-update)
         (list :name "links"
               :pretty-printer #'telega-view-settings--link-pp
               :header "LINKS"
               :items (list (cons (telega-i18n "telega_settings_telega_manual")
                                  "https://zevlg.github.io/telega.el/")
                            (cons (telega-i18n "lng_settings_faq")
                                  "https://telegram.org/faq")
                            (cons (telega-i18n "lng_settings_ask_question")
                                  "https://t.me/emacs_telega")
                            (cons (telega-i18n "telega_settings_donate")
                                  "https://opencollective.com/telega")))
         ))

  ;; NOTE: install `:on-user-update' after displaying "me" ewoc, so
  ;; any "updateUserFullInfo" inside pretty-printer won't corrupt the
  ;; ewoc.  "updateUserFullInfo" could be triggered on the
  ;; `telega--full-info' call
  (plist-put (telega-root-view--ewoc-spec "me")
             :on-user-update #'telega-root--me-on-user-update)
  )

;; "Folders" root view
(defun telega-view-folders--gen-pp (folder-name)
  "Generate pretty printer for chats in folder with FOLDER-NAME."
  (let ((tdlib-chat-list (telega-folder--tdlib-chat-list folder-name)))
    (lambda (chat)
      (when (telega-chat-match-p chat (list 'folder folder-name))
        (let ((telega-tdlib--chat-list tdlib-chat-list)
              (telega-temex-remap-list '((chat chat-list . (return t)))))
          (telega-root--chat-known-pp chat))))))

(defun telega-view-folders--gen-sorter (folder-name)
  (let ((tdlib-chat-list (telega-folder--tdlib-chat-list folder-name)))
    (lambda (chat1 chat2)
      (let ((telega-tdlib--chat-list tdlib-chat-list))
        (telega-chat> chat1 chat2)))))

(defun telega-root--folders-on-chat-update (ewoc-name ewoc chat)
  "Handler for chat updates in \"folders\" root view."
  (let ((folder-name (plist-get (telega-root-view--ewoc-spec ewoc-name)
                                :name)))
    (if (telega-chat-match-p chat (list 'folder folder-name))
      (telega-root--any-on-chat-update ewoc-name ewoc chat))))

(defun telega-view-folders--ewoc-spec (folder-spec)
  (let ((folder-name (telega-tl-str folder-spec :title)))
    (list :name folder-name
          :pretty-printer (telega-view-folders--gen-pp folder-name)
          :header (telega-folder-format "%i%f" folder-name)
          :sorter (telega-view-folders--gen-sorter folder-name)
          :items (telega-filter-chats telega--ordered-chats
                   (list 'folder folder-name))
          :on-chat-update #'telega-root--folders-on-chat-update)))

(defun telega-view-folders ()
  "View Telegram folders."
  (interactive)
  (telega-root-view--apply
   (nconc (list 'telega-view-folders "Folders")
          (mapcar #'telega-view-folders--ewoc-spec
                  telega-tdlib--chat-folders))))

;; "Recently Deleted Chats" root view
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

;; "Files" root view
(defun telega-view-files--ins-file (file predicate)
  "Inserter for FILE in Files root view."
  (let* ((uploading-p (memq predicate '(telega-file--uploading-p
                                        telega-file--uploaded-p
                                        telega-file--partially-uploaded-p)))
         (downloaded-p (eq predicate #'telega-file--downloaded-p))
         (local-path (telega--tl-get file :local :path))
         (part-width (- (/ telega-root-fill-column 3) 3))
         (col1-width (if downloaded-p
                         (+ part-width part-width 15)
                       part-width)))
    (cl-assert (not (string-empty-p local-path)))
    (telega-ins--with-attrs (list :max col1-width :min col1-width
                                  :align 'left :elide t
                                  :elide-trail col1-width)
      (telega-button--insert 'telega local-path
        'face (if (telega-file--downloaded-p file)
                  'telega-link
                'default)
        ;; `d' to delete the file at point, you can delete only files
        ;; cached by TDLib, TDLib won't delete files outside its cache dir
        'keymap (when (telega-file--downloaded-p file)
                  (let ((map (make-sparse-keymap)))
                    (set-keymap-parent map button-map)
                    (define-key map (kbd "d") (lambda ()
                                                (interactive)
                                                (telega--deleteFile file)))
                    map))
        :inserter (lambda (file-path)
                    (telega-ins
                     (if downloaded-p
                         (telega-short-filename file-path)
                       (file-name-nondirectory file-path))))
        :action #'telega-open-file))

    (unless downloaded-p
      (telega-ins " ")
      (telega-ins "[")
      (let ((progress (if uploading-p
                          (telega-file--uploading-progress file)
                        (telega-file--downloading-progress file))))
        (telega-ins-progress-bar
         progress 1.0 part-width
         (if uploading-p
             telega-symbol-upload-progress
           telega-symbol-download-progress))
        (telega-ins-fmt "]%3d%%" (round (* progress 100))))
      (telega-ins "  ")
      (telega-ins--with-attrs (list :min 6 :align 'right)
        (telega-ins (file-size-human-readable
                     (if uploading-p
                         (telega--tl-get file :remote :uploaded_size)
                       (telega--tl-get file :local :downloaded_size))))))

    (telega-ins " | ")
    (telega-ins--with-attrs (list :min 6 :align 'right)
      (telega-ins (file-size-human-readable (telega-file--size file))))

    (cond ((eq 'telega-file--downloading-p predicate)
           (telega-ins "  ")
           (telega-ins--button "Cancel"
             :value file
             :action #'telega--cancelDownloadFile))
          ((eq 'telega-file--partially-downloaded-p predicate)
           (telega-ins "  ")
           (telega-ins--button "Resume"
             :value file
             :action #'telega-file--download)))
    (when telega-debug
      (telega-ins-fmt "  ID:%5d" (plist-get file :id)))
    ))

(defun telega-view-files--gen-pp (predicate)
  "Generate pretty printer for files matching PREDICATE.
PREDICATE is one of `telega-file--downloading-p',
`telega-file--uploading-p', `telega-file--downloaded-p',
`telega-file--uploaded-p', `telega-file--partially-downloaded-p',
`telega-file--partially-uploaded-p'."
  (lambda (file-id)
    (let* ((file (telega-file-get file-id))
           (file-path (telega--tl-get file :local :path)))
      (when (and (funcall predicate file)
                 ;; NOTE: Ignore files without local file path
                 (not (string-empty-p file-path))
                 ;; NOTE: Check all excluded subdirs
                 (not (seq-intersection
                       (cdr (assq predicate
                                  telega-root-view-files-exclude-subdirs))
                       (split-string file-path "/"))))
        (telega-view-files--ins-file file predicate)
        (telega-ins "\n")))))

(defconst telega-view-files--predicates
  '(("downloading" . telega-file--downloading-p)
    ("uploading"   . telega-file--uploading-p)
    ("partially-downloaded" . telega-file--partially-downloaded-p)
    ("partially-uploaded"   . telega-file--partially-uploaded-p)
    ("downloaded"  . telega-file--downloaded-p)
    ;; NOTE: Do not list "uploaded", because we have
    ;; no information who uploaded the file
    ;; TODO: we might have a list of files started
    ;; uploading with `telega--preliminaryUploadFile' to list
    ;; them as "uploaded-by-me".
    ;("uploaded"    . telega-file--uploaded-p)
    ))

(defun telega-root--on-file-update (ewoc-name ewoc file)
  "FILE has been updated."
  (let ((file-node (telega-ewoc--find-by-data ewoc (plist-get file :id)))
        (predicate (cdr (assoc ewoc-name telega-view-files--predicates))))
    (if file-node
        (if (funcall predicate file)
            (ewoc-invalidate ewoc file-node)
          (ewoc-delete ewoc file-node))

      (when (funcall predicate file)
        (ewoc-enter-first ewoc (plist-get file :id))))))

(defun telega-view-files--ewoc-spec (kind)
  "Generate ewoc spec for files tracking of KIND.
KIND is one of \"downloading\", \"uploading\", \"downloaded\",
\"uploaded\", \"partially-downloaded\", \"partially-uploaded\"."
  ;; NOTE: most recent files goes first, `:telega-file-recency' is set
  ;; in `telega-file--ensure'
  (let* ((predicate (cdr (assoc kind telega-view-files--predicates)))
         (kind-files (cl-sort (cl-remove-if-not
                               predicate (hash-table-values telega--files))
                              #'> :key (telega--tl-prop :telega-file-recency))))
    (list :name kind
          :header (upcase kind)
          :pretty-printer (telega-view-files--gen-pp predicate)
          ;; NOTE: store file's id as data, to match by `:id', because
          ;; file changes on file updates
          :items (mapcar (telega--tl-prop :id) kind-files)
          :on-file-update #'telega-root--on-file-update)))

(defun telega-view-files (kinds)
  "View status of files known to telega.
File can be in one of the state kinds: \"downloading\", \"uploading\",
\"partially-downloaded\", \"partially-uploaded\", \"downloaded\".
If `\\[universal-argument] is specified, then query user about file
state kinds to show. By default all kinds are shown."
  (interactive
   (let ((all-kinds (mapcar #'car telega-view-files--predicates)))
     (list (if current-prefix-arg
               (telega-gen-completing-read-list
                "File State Kind" all-kinds
                #'identity telega-completing-read-function)
             all-kinds))))

  (telega-root-view--apply
   (nconc (list 'telega-view-loading-files
                (telega-i18n "telega_view_files"))
          (mapcar #'telega-view-files--ewoc-spec kinds))))


;; Favorite messages
(defun telega-ins--favorite-message (msg)
  "Inserter for favorite MSG in \"Favorite Messages\" root view."
  ;; TODO: implement 2-lines message inserter
  (telega-ins "  ")
  (telega-ins--chat-msg-one-line (telega-msg-chat msg) msg)
  (let ((fav (telega-msg-favorite-p msg)))
    (cl-assert fav)
    (telega-ins-prefix "\n  "
      (telega-ins--with-face 'telega-shadow
        (telega-ins (plist-get fav :comment))))))

(defun telega-root--favorite-message-pp (msg)
  "Pretty printer for favorite message MSG."
  (when (telega-msg-favorite-p msg)
    (telega-root--message-pp msg #'telega-ins--favorite-message)))

(defun telega-view-favorite-msg--ewoc-spec (chat)
  "Create ewoc for favorite messages in the CHAT."
  (let ((ewoc-name (symbol-name (gensym "fav-ewoc"))))
    (list :name ewoc-name
          :header (telega-ins--as-string
                   (let ((telega-chat-button-format-plist
                          (list :with-folder-format telega-chat-folder-format
                                :with-title-faces-p t
                                :with-username-p 'telega-username
                                :with-unread-trail-p t
                                :with-status-icons-trail-p t)))
                     (telega-ins--chat chat)))
          :pretty-printer #'telega-root--favorite-message-pp
          :loading (telega--getMessages (plist-get chat :id)
                       (mapcar (telega--tl-prop :id)
                               (seq-filter (lambda (fav)
                                             (eq (plist-get chat :id)
                                                 (plist-get fav :chat_id)))
                                           telega--favorite-messages))
                     (apply-partially #'telega-root-view--ewoc-loading-done
                                      ewoc-name))
          :sorter #'telega-root--messages-sorter
          :on-message-update #'telega-root--on-message-update)))

(defun telega-view-favorite-messages ()
  "View favorite messages in all the chats."
  (interactive)
  (telega-root-view--apply
   (nconc (list 'telega-view-favorite-messages
                (concat (telega-symbol 'favorite) "Favorite Messages"))
          (mapcar #'telega-view-favorite-msg--ewoc-spec
                  (telega-filter-chats
                   telega--ordered-chats 'has-favorite-messages)))))


;; Voice Chats, inspired by https://t.me/designers/177
(defun telega-root--passive-video-chat-pp (_chat)
  ;; TODO
  )

(defun telega-root--scheduled-video-chat-pp (_chat)
  ;; TODO
  )

(defun telega-root--active-video-chat-pp (_chat)
  ;; TODO
  )

(defun telega-root--on-group-call-update (_ewoc-name _ewoc _group-call)
  ;; TODO
  )

(defun telega-view-video-chats ()
  "View active/passive/scheduled video chats."
  (interactive)
  (telega-root-view--apply
   (list 'telega-view-video-chats "Video Chats"
         (list :name "Active"
               :pretty-printer #'telega-root--active-video-chat-pp
               :items nil               ; todo
               :on-group-call-update #'telega-root--on-group-call-update)
         (list :name "Scheduled"
               :pretty-printer #'telega-root--scheduled-video-chat-pp
               :items nil               ; todo
               :on-group-call-update #'telega-root--on-group-call-update)
         (list :name "Passive"
               :pretty-printer #'telega-root--passive-video-chat-pp
               :items nil               ; todo
               :on-group-call-update #'telega-root--on-group-call-update)
         )))


;; Logging in via QR code
(defun telega-qr-code--show (link)
  "Hide QR code scanning dialog.
If LINK is nil, then link is loading."
  (cl-assert telega-use-images)
  (with-telega-root-view-ewoc "root" ewoc
    (telega-save-cursor
      (telega-ewoc--set-footer ewoc
        (telega-ins--as-string
         (if link
             (telega-ins--image
              (telega-qr-code--create-image link (telega-chars-xheight 10)))
           (telega-ins "QR code loading.."))
         (telega-ins "\n")
         (telega-ins--with-face 'bold
           (telega-ins (telega-i18n "lng_intro_qr_title") "\n"))
         (telega-ins "1. " (telega-i18n "lng_intro_qr_step1") "\n")
         (telega-ins "2. " (telega-i18n "lng_intro_qr_step2") "\n")
         (telega-ins "3. " (telega-i18n "lng_intro_qr_step3") "\n")
         (telega-ins "\n")
         (telega-ins--button (telega-i18n "lng_intro_qr_skip")
           'action (lambda (_button)
                     ;; Skip QR auth and fallback to phone number as
                     ;; auth method, see
                     ;; https://github.com/tdlib/td/issues/1645
                     (setq telega--relogin-with-phone-number t)
                     (telega-logout))))))

    (run-hooks 'telega-root-update-hook)))

(defun telega-qr-code--hide ()
  "Hide QR code scanning dialog."
  (with-telega-root-view-ewoc "root" ewoc
    (telega-save-cursor
      (telega-ewoc--set-footer ewoc ""))

    (run-hooks 'telega-root-update-hook)))

(provide 'telega-root)

;;; telega-root.el ends here

;;; telega-filter.el --- Chats filtering in root buffer

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sun Apr 22 17:36:38 2018
;; Keywords: 

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'telega-core)
(require 'telega-customize)

(defgroup telega-filter nil
  "Customize chats filtration."
  :prefix "telega-filter-"
  :group 'telega)

(defcustom telega-filter-default 'all
  "*Default chats filter to apply.
For example: `(or pin (custom \"Groups&Users\"))'"
  :type 'list
  :group 'telega-filter)

(defcustom telega-filter-custom-alist
  '(("All" . (all))
    ("Groups&Users" . (type private secret basicgroup supergroup))
    ("Secrets" . (type secret))
    ("Groups" . (type basicgroup supergroup))
    ("Bots" . (type bot))
    ("Channels" . (type channel)))
  "*Custom filters for chats."
  :type 'alist
  :group 'telega-filter)

(defface telega-filter-active-face '((t (:bold t)))
  "*Face to use for active custom filters."
  :group 'telega-filter)

(defvar telega-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ":") 'telega-filters-edit)
    (define-key map (kbd "n") 'telega-filter-by-name)
    (define-key map (kbd "t") 'telega-filter-by-type)
    (define-key map (kbd "c") 'telega-filter-by-custom)
    (define-key map (kbd "u") 'telega-filter-by-unread)
    (define-key map (kbd "m") 'telega-filter-by-mention)
    (define-key map (kbd "!") 'telega-filters-negate)
    (define-key map (kbd "/") 'telega-filters-reset)
    map)
  "Keymap for filtering commands.")

(define-widget 'telega-filter-list 'editable-field
  "Widget to show active chat filters."
  :format "Filters: %v"
  :size 60
  :value-to-internal (lambda (_wid val)
                       (if val
                           (substring (prin1-to-string val) 1 -1)
                         ""))
  :value-to-external (lambda (_wid val)
                       (unless (string-empty-p val)
                         (car (read-from-string (concat "(" val ")")))))
  :action (lambda (wid &optional _event)
            (telega-filters-edit (widget-value wid))))

(define-widget 'telega-filter-custom 'push-button
  "Widget to represet custom chat filter.
Format for the title is: [N:NAME  UNREAD@MENTIONS], where:
 N - Number of chats matched by both, custom filter and active filters.
 NAME - Name of the custom filter
 UNREAD - Number of unread messages in N chats
 MENTIONS - Number of mentions in N chats

Button highlights with `telega-filter-active-face' if all chats
matching by this custom filter also matches by active filters.

If you press on button, then custom filter activates."
  :button-prefix "["
  :button-suffix "]"
  :format " %[%v%]"
  :match 'ignore
  :button-face-get 'telega-filter--widget-face-get
  :value-create 'telega-filter--widget-value-create
  :action 'telega-filter--widget-action)

(defun telega-filter--widget-face-get (widget)
  "Return active face, if WIDGET's filter is active."
  (let ((chats (telega-filter-chats (cdr (widget-get widget :value)))))
    (when (and chats (cl-subsetp chats telega--filtered-chats))
      'telega-filter-active-face)))
  ;; (when (member (list 'custom (car (widget-get widget :value)))
  ;;               (car telega--filters))
  ;;   'telega-filter-active-face))

(defun telega-filter--widget-value-create (widget)
  "Create value for custom filter WIDGET.
It composes string in form
\"<number-of-matching-chats>:<name> [<unread>[@<mentions>]]\""
  (let* ((cus-filter (widget-get widget :value))
         (cus-chats (telega-filter-chats (cdr cus-filter)))
         (chats (and cus-chats (telega-filter-chats nil cus-chats)))
         (unread (apply #'+ (mapcar (telega--tl-prop :unread_count) chats)))
         (mentions (apply #'+ (mapcar (telega--tl-prop :unread_mention_count) chats)))
         (v (format "%d:%s" (length chats) (car cus-filter))))
    (unless (zerop unread)
      (setq v (concat v " " (number-to-string unread)))
      (unless (zerop mentions)
        (setq v (concat v "@" (number-to-string mentions)))))
    (insert v)))

(defun telega-filter--widget-action (widget &optional event)
  (message "TODO: `telega-filter--widget-activate'")
  (telega--filters-reset)
  (telega-filter-add 'custom (car (widget-get widget :value))))

;;; Filters
(defun telega--filters-apply ()
  "Apply current filers."
  (setq telega--filtered-chats (telega-filter-chats))

  (telega-root--redisplay))

(defun telega--filters-reset ()
  "Reset all filters."
  (setq telega--filters nil
        telega--undo-filters nil))

(defun telega--filters-push (flist)
  "Set active filters list to FLIST."
  (setq telega--filters (push flist telega--filters))
  (telega--filters-apply))

(defun telega-filter-add (fname &rest fargs)
  "Add filter specified by FSPEC.
This filter can be undone with `telega-filter-undo'."
  (telega--filters-push
   (append (car telega--filters) (list (cons fname fargs)))))

(defun telega-filter-chats (&optional fspec chats)
  "Filter chats matching filter specified by FSPEC.
If FSPEC is not specified then `telega--filters' is used.
If CHATS is not specified then `telega--ordered-chats' is used.
Use `(and chats (telega-filter-chats fspec chats))' to ensure CHATS is non-nil."
  (cl-remove-if-not
   (lambda (chat)
     (telega-filter--test chat (or fspec (cons 'all (car telega--filters)))))
   (or chats telega--ordered-chats)))

;;;###autoload
(defun telega-filters-reset ()
  "Reset all active filters to default."
  (interactive)
  (telega--filters-reset)
  (telega--filters-push (list telega-filter-default)))

;;;###autoload
(defun telega-filter-undo (&optional arg)
  "Undo last ARG filters."
  (interactive "p")
  (unless telega--filters
    (error "Nothing to undo"))
  (dotimes (_ arg)
    (when telega--filters
      (push (car telega--filters) telega--undo-filters)
      (setq telega--filters (cdr telega--filters))))
  (telega--filters-apply)
  (message "Undo!"))

;;;###autoload
(defun telega-filter-redo (&optional arg)
  "Redo last ARG filters."
  (interactive "p")
  (unless telega--undo-filters
    (error "Nothing to redo"))
  (dotimes (_ arg)
    (when telega--undo-filters
      (push (pop telega--undo-filters) telega--filters)))
  (telega--filters-apply)
  (message "Redo!"))

;;;###autoload
(defun telega-filters-edit (flist)
  "Edit and reapply filters list."
  (interactive
   (let* ((print-level nil)
          (flist-as-string (if telega--filters
                               (prin1-to-string telega--filters)
                             ""))
          (new-flist (read-from-minibuffer
                      "Filters: " flist-as-string read-expression-map t)))
     (list new-flist)))
  (telega--filters-push flist))


;;; Filters definitions
(defmacro define-telega-filter (name args &rest body)
  "Define new filter for telega chats.
ARGS specifies arguments to operation, first must always be chat."
  (let ((fsym (intern (format "telega--filter-%S" name))))
    `(defun ,fsym ,args
       ,@body)))

(defun telega-filter--get (fname)
  (let ((fsym (intern (format "telega--filter-%S" fname))))
    (unless (fboundp fsym)
      (error (concat "Filter function `%S' for filter \"%s\" is undefined.\n"
                     "Use `define-telega-filter' to define new filters.")
             fsym fname))
    (symbol-function fsym)))

(defun telega-filter--test (chat fspec)
  "Return non-nil if CHAT matches filters specified by FSPEC."
  (cond ((symbolp fspec)
         (funcall (telega-filter--get fspec) chat))
        ((consp fspec)
         (apply (telega-filter--get (car fspec)) chat (cdr fspec)))
        (t (error "Invalid filter spec: %s" fspec))))

(define-telega-filter any (chat &rest flist)
  "Return non-nil if CHAT matches any of filter in FLIST."
  (cl-find chat flist :test #'telega-filter--test))

(define-telega-filter all (chat &rest flist)
  "Return non-nil if CHAT matches all filters in FLIST.
If FLIST is empty then return t."
  (not (cl-find chat flist :test-not #'telega-filter--test)))

(define-telega-filter not (chat fspec)
  "Negage filter FSPEC."
  (not (telega-filter--test chat fspec)))

;;;###autoload
(defun telega-filters-negate ()
  "Negate active filters."
  (interactive)
  (telega--filters-push `(not (all ,@(car telega--filters)))))

(define-telega-filter type (chat &rest ctypes)
  "Matches CHAT by its type."
  (memq (telega-chat--type chat) ctypes))

;;;###autoload
(defun telega-filter-by-type (ctype)
  "Filter chats by its type."
  (interactive
   (list (completing-read
          "Chat type: "
          (mapcar #'list (mapcar #'symbol-name telega-chat-types))
          nil t)))
  (telega-filter-add 'type (intern ctype)))

(define-telega-filter name (chat regexp)
  "Matches CHAT if its title matches REGEXP."
  (string-match regexp (telega-chat--title chat)))

;;;###autoload
(defun telega-filter-by-name (regexp)
  "Filter by REGEXP matching chat's title."
  (interactive (list (read-regexp "Chat name regexp: ")))
  (telega-filter-add 'name regexp))

(define-telega-filter custom (chat name)
  "Matches CHAT if custom filte with NAME matches."
  (let ((fspec (cdr (assoc name telega-filter-custom-alist))))
    (unless fspec
      (error "No such custom filter \"%s\"" name))
    (telega-filter--test chat fspec)))

;;;###autoload
(defun telega-filter-by-custom (name)
  "Filter by custom filter."
  (interactive (list (completing-read
                      "Custom filter: "
                      (mapcar #'list (mapcar #'car telega-filter-custom-alist))
                      nil t)))
  (telega-filter-add 'custom name))

(define-telega-filter pin (chat)
  "Matches if CHAT is pinned."
  (telega--tl-bool chat :is_pinned))

;;;###autoload
(defun telega-filter-by-pin ()
  "Filter only pinned chats."
  (telega-filter-add 'pin))

(define-telega-filter unread (chat &optional n)
  "Matches CHAT with at least N unread messages.
By default N is 1."
  (>= (plist-get chat :unread_count) (or n 1)))

;;;###autoload
(defun telega-filter-by-unread (n)
  "Filter chats with at least N unread messages."
  (interactive "p")
  (telega-filter-add 'unread n))

(define-telega-filter mention (chat &optional n)
  "Matches CHAT with at least N unread mentions."
  (>= (plist-get chat :unread_mention_count) (or n 1)))

;;;###autoload
(defun telega-filter-by-mention (n)
  "Filter chats with at least N unread mentions."
  (interactive "p")
  (telega-filter-add 'mention n))

(provide 'telega-filter)

;;; telega-filter.el ends here

;;; telega-transient.el --- Transient commands support for telega.el  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2021 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Mon Feb  8 17:50:19 2021
;; Package-Requires: ((transient "0.3.0"))
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

;;; ellit-org:
;; ** /telega-transient.el/ -- Transient (magit-like style) commands for telega  :new:
;;
;; Use transient (magit-like style) commands in the telega.  Enable it
;; with:
;;
;; #+begin_src emacs-lisp
;; (require 'telega-transient)
;; (telega-transient-mode 1)
;; #+end_src
;;
;; Control keymaps for which to use transient with:
;; - {{{user-option(telega-transient-keymaps, 2)}}}

;;; Code:
(require 'telega)
(require 'transient)

(defgroup telega-transient nil
  "Customisation for telega-transient."
  :prefix "telega-transient-"
  :group 'telega)

(defcustom telega-transient-keymaps
  '(telega-prefix-map
    telega-sort-map
    telega-filter-map
    telega-describe-map
    telega-folder-map
    telega-voip-map
    telega-root-fastnav-map
    telega-root-view-map
    telega-chatbuf-fastnav-map)
  "List of keymaps names to apply transient for."
  :type 'list
  :group 'telega-transient)

;; TODO: add support for columns, i.e. vector of vectors instead of
;; single vector, see `magit-diff' as example
(defmacro telega-transient-define-prefix-by-keymap (name label keymap)
  (declare (indent 2))
  `(transient-define-prefix ,name
     ,(format "Transient command for `%S' keymap." keymap)
     [,label
      ,@(mapcar (lambda (kf)
                  (list (key-description (vector (car kf)))
                        (car (split-string (documentation (cdr kf)) "\n"))
                        (cdr kf)))
                (cl-remove-if-not #'commandp (cdr (symbol-value keymap))
                                  :key #'cdr))
      ]))

(telega-transient-define-prefix-by-keymap telega-transient-telega
    "Telega commands:" telega-prefix-map)
(telega-transient-define-prefix-by-keymap telega-transient-chat-sort
    "Chat Sorter to apply:" telega-sort-map)
(telega-transient-define-prefix-by-keymap telega-transient-chat-filter
    "Chat Filter to apply:" telega-filter-map)
(telega-transient-define-prefix-by-keymap telega-transient-describe
    "Describe commands:" telega-describe-map)
(telega-transient-define-prefix-by-keymap telega-transient-chat-folder
    "Chat Folder commands:" telega-folder-map)
(telega-transient-define-prefix-by-keymap telega-transient-voip
    "VoIP commands:" telega-voip-map)
(telega-transient-define-prefix-by-keymap telega-transient-root-fastnav
    "Root buffer fast navigation commands:" telega-root-fastnav-map)
(telega-transient-define-prefix-by-keymap telega-transient-root-view
    "Root View commands:" telega-root-view-map)
(telega-transient-define-prefix-by-keymap telega-transient-chatbuf-fastnav
    "Chatbuf fast navigation commands:" telega-chatbuf-fastnav-map)

;;;###autoload
(define-minor-mode telega-transient-mode
  "Global mode to enable transient commands in the telega"
  :init-value nil :global t :group 'telega-modes
  (if telega-transient-mode
      (progn
        (when (memq 'telega-sort-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "\\")
            'telega-transient-chat-sort))
        (when (memq 'telega-filter-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "/")
            'telega-transient-chat-filter))
        (when (memq 'telega-describe-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "?")
            'telega-transient-describe))
        (when (memq 'telega-folder-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "F")
            'telega-transient-chat-folder))
        (when (memq 'telega-voip-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "c")
            'telega-transient-voip))
        (when (memq 'telega-root-fastnav-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "M-g")
            'telega-transient-root-fastnav))
        (when (memq 'telega-root-view-map telega-transient-keymaps)
          (define-key telega-root-mode-map (kbd "v")
            'telega-transient-root-view))

        (when (memq 'telega-chatbuf-fastnav-map telega-transient-keymaps)
          (define-key telega-chat-mode-map (kbd "M-g")
            'telega-transient-chatbuf-fastnav))
        )

    (define-key telega-root-mode-map (kbd "\\") telega-sort-map)
    (define-key telega-root-mode-map (kbd "/") telega-filter-map)
    (define-key telega-root-mode-map (kbd "?") telega-describe-map)
    (define-key telega-root-mode-map (kbd "F") telega-folder-map)
    (define-key telega-root-mode-map (kbd "c") telega-voip-map)
    (define-key telega-root-mode-map (kbd "M-g") telega-root-fastnav-map)
    (define-key telega-root-mode-map (kbd "v") telega-root-view-map)

    (define-key telega-chat-mode-map (kbd "M-g") telega-chatbuf-fastnav-map)
    ))

(provide 'telega-transient)

;;; telega-transient.el ends here

;;; telega-sort.el --- Chat sorting in rootbuf  -*- lexical-binding:t -*-

;; Copyright (C) 2020 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sat Jan 25 09:42:22 2020
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

;; * Sorting Chats
;;
;; It is possible to sort chats in rootbuf out of Telega built-in
;; order.  Sorting chats is done by some criteria.  Built-in criterias
;; are in ~telega-sort-criteria-alist~.  Do not insert criterias
;; directly into ~telega-sort-criteria-alist~, use
;; ~define-telega-sorter~ instead.
;; 
;; - {{{kbd(\)}}} ::
;;   rootbuf prefix map for sorting commands
;;
;; - {{{where-is(telega-sort-reset,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega-sort-reset)}}}
;;
;;   It is possible to add multiple criteria using ~telega-sort-reset~
;;   with prefix argument {{{kbd(C-u)}}}.
;;   
;; - {{{where-is(telega-sort-by-sorter,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega-sort-by-sorter)}}}
;;
;;   Use this command to reset active sorter.

;;; Code:

(defvar telega-root--ewoc)
(declare-function telega-root--redisplay "telega-root")

(declare-function telega-chat--reorder "telega-chat" (chat order))
(declare-function telega-chat--info "telega-chat" (chat))
(declare-function telega-chat-title "telega-chat" (chat &optional with-username))

(defvar telega-sort-criteria-alist nil)
(defvar telega-sort--inhibit-order nil
  "Bind to non-nil to inhibit chat order when sorting.")

(defvar telega-sort-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "\\") 'telega-sort-reset)
    ;; `a' mnemominc is to "add" some sorter
    (define-key map (kbd "a") 'telega-sort-by-sorter)
    (define-key map (kbd "s") 'telega-sort-by-sorter)
    (define-key map (kbd "u") 'telega-sort-by-unread-count)
    (define-key map (kbd "t") 'telega-sort-by-title)
    (define-key map (kbd "j") 'telega-sort-by-join-date)
    (define-key map (kbd "o") 'telega-sort-by-online-members)
    (define-key map (kbd "m") 'telega-sort-by-member-count)
    (define-key map (kbd "!") 'telega-sort-invert)

    (define-key map (kbd "d") 'telega-sort-pop-last)
    (define-key map (kbd "DEL") 'telega-sort-pop-last)
    map)
  "Keymap for sorting commands.")

(defmacro define-telega-sorter (name order-events args &rest body)
  (let ((fsym (intern (format "telega--sort-%S" name)))
        (cmd (intern (format "telega-sort-by-%S" name))))
    `(progn
       (put (quote ,name) :telega-order-events (quote ,order-events))
       (setq telega-sort-criteria-alist
             (push (cons (quote ,name) (quote ,fsym))
                   telega-sort-criteria-alist))
       (defun ,fsym ,args
              ,@body)
       (defun ,cmd ()
         ,(format "Sort chats by `%S' criteria." name)
         (interactive)
         (telega-sort-set-active-criteria (list (quote ,name)))))))

(defsubst telega-sort--canonicalize-criteria (criteria)
  "Return CRITERIA in canonical (i.e. list) form."
  (if (listp criteria)
      criteria
    (list criteria)))

(defun telega-chats-compare (criteria chat1 chat2)
  "Return non-nil if CHAT1 is greater than CHAT2 according to CRITERIA.
CRITERIA could be a lit of sort criterias."
  (unless (listp criteria)
    (setq criteria (list criteria)))

  (if (null criteria)
      (unless telega-sort--inhibit-order
        (string> (telega-chat-order chat1) (telega-chat-order chat2)))

    (let* ((cmpfun (alist-get (car criteria) telega-sort-criteria-alist))
           (c1-val (funcall cmpfun chat1))
           (c2-val (funcall cmpfun chat2)))
      (cond ((equal c1-val c2-val)
             ;; Traverse the criteria list
             (telega-chats-compare (cdr criteria) chat1 chat2))
            ((null c1-val) nil)
            ((null c2-val) t)
            ((and (stringp c1-val) (stringp c2-val))
             ;; Make "A" > "Z", so alphabetical order goes out-of-box
             (string< c1-val c2-val))
            (t (> c1-val c2-val))))))

(defun telega-sort-chats (criteria chats)
  "Sort CHATS by criteria."
  (sort chats (apply-partially 'telega-chats-compare criteria)))

(defun telega-sort-by-sorter (criteria &optional arg)
  "Interactively add CRITERIA to active sorter.
If prefix ARG is used, then add sort criteria, instead of
overwritting currently active one."
  (interactive
   (let ((cname (funcall telega-completing-read-function
                         "Sort criteria: "
                         (mapcar 'symbol-name
                                 (mapcar 'car telega-sort-criteria-alist)))))
     (list (intern cname) current-prefix-arg)))

  (telega-sort-set-active-criteria
   (append (when arg telega--sort-criteria) (list criteria))))

(defun telega-sort-reset ()
  "Reset active sorter."
  (interactive)
  (telega-sort-set-active-criteria nil))

(defun telega-sort-invert ()
  "Invert current active sorter."
  (interactive)
  (telega-sort-set-active-criteria
   telega--sort-criteria (not telega--sort-inverted)))

(defun telega-sort-set-active-criteria (criteria &optional inverted)
  "Set CRITERIA as active sort criteria."
  (cl-assert (listp criteria))
  (when (or (not (equal telega--sort-criteria criteria))
            (not (eq telega--sort-inverted inverted)))
    (setq telega--sort-criteria criteria)
    (setq telega--sort-inverted inverted)

    (setq telega--ordered-chats
          (sort telega--ordered-chats 'telega-chat>))

    (telega-save-cursor
      (telega-ewoc--clean telega-root--ewoc)
      (dolist (chat telega--ordered-chats)
        (ewoc-enter-last telega-root--ewoc chat)))
    (telega-filters--redisplay)
  ))

(defconst telega-sort--order-events
  '("updateChatOrder" "updateChatIsPinned" "updateChatLastMessage"
    "updateChatIsSponsored" "updateChatDraftMessage")
  "List of events with `:order' property.")

(defun telega-sort-maybe-reorder (chat event)
  "React on CHAT's related telegram EVENT.
Some events might require chat reordering.
Return non-nil if CHAT has been reordered."
  (let ((event-type (plist-get event :@type)))
    (cond ((member event-type telega-sort--order-events)
           (telega-chat--reorder chat (plist-get event :order))
           t)
          ((and telega--sort-criteria
                (cl-some (lambda (criteria-sym)
                           (member event-type
                                   (get criteria-sym :telega-order-events)))
                         telega--sort-criteria))
           (telega-chat--reorder chat nil)
           t))))


;; ** Sorting criteria
(define-telega-sorter order () (chat)
  (telega-chat-order chat))

;; - ~unread-count~, {{{where-is(telega-sort-by-unread-count,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--sort-unread-count)}}}
(define-telega-sorter unread-count ("updateChatReadInbox") (chat)
  (plist-get chat :unread_count))

;; - ~title~, {{{where-is(telega-sort-by-title,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--sort-title)}}} (thanks to https://t.me/Kurvivor)
(define-telega-sorter title ("updateChatTitle") (chat)
  "Sort chats by number of online members."
  (telega-chat-title chat))

;; - ~member-count~, {{{where-is(telega-sort-by-member-count,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--sort-member-count)}}}
(define-telega-sorter member-count ("updateBasicGroup" "updateSupergroup") (chat)
  "Sort chats by number of members in the chat."
  (plist-get (telega-chat--info chat) :member_count))
 
;; - ~online-members~, {{{where-is(telega-sort-by-online-members,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--sort-online-members)}}}
(define-telega-sorter online-members ("updateChatOnlineMemberCount") (chat)
  "Sort chats by number of online members."
  (plist-get chat :x-online-count))

;; - ~join-date~, {{{where-is(telega-sort-by-join-date,telega-root-mode-map)}}} ::
;;   {{{fundoc(telega--sort-join-date)}}}
(define-telega-sorter join-date () (chat)
  "Sort chats by join date.  Last joined chats goes first."
  (plist-get (telega-chat--info chat) :date))

;; - TODO Date of last message sent by ~telega-user-me~
;; - TODO Date of last mention (thanks to https://t.me/lainposter)

(provide 'telega-sort)

;;; telega-sort.el ends here

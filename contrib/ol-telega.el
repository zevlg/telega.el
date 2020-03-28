;;; ol-telega.el --- Links to telega chats and messages -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sat Mar 28 11:15:21 2020
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
(require 'telega-tme)
(require 'telega-util)

(require 'ol)

(defun org-telega-follow-link (link)
  "Follow a telegram LINK to chat or message."
  (telega-tme-open-tg (concat "tg:telega:" link)))

(defun org-telega-store-link ()
  "Store a link to a telegram chat or message."
  (when-let ((link (telega-tme-internal-link-to
                    (or (telega-msg-at (point))
                        (telega-chat-at (point))))))
    ;; NOTE: strip leading "tg:"
    (let ((org-link (substring link 3)))
      (org-link-store-props :type "telega" :link org-link)
      org-link)))

(defun org-telega-complete-link ()
  "Complting link to the chat."
  (let ((chat (telega-completing-read-chat "Chat: ")))
    ;; NOTE: strip leading "tg:"
    (substring (telega-tme-internal-link-to  chat) 3)))

(org-link-set-parameters "telega"
                         :follow 'org-telega-follow-link
                         :store 'org-telega-store-link
                         :complete 'org-telega-complete-link)

(provide 'ol-telega)

;;; ol-telega.el ends here

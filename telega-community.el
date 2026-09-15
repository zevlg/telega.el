;;; telega-community.el --- Telegram communities  -*- lexical-binding: t -*-

;; Copyright (C) 2026 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Jul 24 11:56:22 2026
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

(defun telega-community-get (cid)
  "Get community by community id CID."
  (alist-get cid telega--communities-alist))

(defun telega-community-title--special (community)
  "Format a COMMUNITY title for use in the special message."
  (telega-ins--as-string
   (telega-ins--raw-button
       (list 'action (lambda (_button)
                       (telega-describe-community community)))
     (telega-ins--with-face 'bold
       (when-let* ((photo-info (plist-get community :photo)))
         (telega-ins--image
          (telega-media--imageNEW photo-info
              #'telega-chat-photo-info--create-image
            :cheight 1))
         (telega-ins " "))
       (telega-ins (telega-tl-str community :name))))))

(defun telega-community-title-for-completion (community)
  (telega-community-title--special community))

(defvar telega-community-read-history nil)
(defun telega-completing-read-community (prompt)
  "Read a community."
  (when-let* ((choices
               (mapcar (lambda (community)
                         (cons (telega-community-title-for-completion community)
                               community))
                       (mapcar #'cdr telega--communities-alist)))
              (choice (telega-completing-read
                       prompt choices nil t nil
                       'telega-community-read-history)))
    (cdr (assoc choice choices))))

(defun telega-describe-community (community)
  "Describes a COMMUNITY."
  (interactive (list (telega-completing-read-community
                      (concat (telega-i18n "lng_community_title") ": "))))
  ;; TODO:
  (message "TODO: describe a community")
  )

(provide 'telega-community)

;;; telega-community.el ends here

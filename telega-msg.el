;;; telega-msg.el --- Messages for telega

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri May  4 03:49:22 2018
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

(defvar telega-msg-button-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "r") 'telega-msg-reply)
    (define-key map (kbd "f") 'telega-msg-forward)
    (define-key map (kbd "e") 'telega-msg-edit)
    (define-key map (kbd "DEL") 'telega-msg-delete)
    map))

(define-button-type 'telega-msg
  :supertype 'telega
  :format #'telega-msg-button--formatter
  'keymap telega-msg-button-keymap
  'action #'telega-msg-button--action)

(defun telega-msg-button--formatter (msg)
  
  )

(defun telega-msg-propertize (text text-entities)
  "Propertize message TEXT according to ENTITIES."
  (dolist (entity text-entities)
    (add-text-properties
     (plist-get entity :offset)
     (+ (plist-get entity :offset) (plist-get entity :length))
     (telega-text-entity-to-properties entity)
     text))
  text)

(defun telega-msg-format (msg)
  (plist-get (plist-get (plist-get msg :content) :text) :text)
  )

(provide 'telega-msg)

;;; telega-msg.el ends here

;;; telega-i18n.el --- I18N for the telega  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Dec 11 02:03:42 2018
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
(require 'telega-customize)

(defvar telega-i18n--en-strings nil "English language strings.")
(defvar telega-i18n--local-strings nil "Local language strings.")
(defvar telega-i18n--strings nil)

(defun telega--getLanguagePackStrings (lang-pack-id &optional keys)
  (let ((reply (telega-server--call
                (list :@type "getLanguagePackStrings"
                      :language_pack_id lang-pack-id
                      :keys (or keys [])))))
    (mapcar (lambda (str)
              (cons (telega-tl-str str :key)
                    (telega-tl-str str :value)))
            (plist-get reply :strings))))

(defun telega-i18n-init ()
  "Initialize I18N subsystem."
  (setq telega-i18n--en-strings
        (telega--getLanguagePackStrings "en"))
  ;; TODO: initialize local language
  )

(defun telega-i18n (key)
  "Get I18N string for the KEY."
  (let ((val (cdr (assoc key telega-i18n--local-strings))))
    (when (or (not val)
              (eq (telega--tl-type val) 'languagePackStringValueDeleted))
      ;; fallback to english strings
      (setq val (cdr (assoc key telega-i18n--en-strings))))
    (cl-assert val nil (format "KEY=%s not found in strings" key))
    (cl-ecase (telega--tl-type val)
      (languagePackStringValueOrdinary
       (telega-tl-str val :value))
      (languagePackStringValuePluralized
       val)
      (languagePackStringValueDeleted
       (cl-assert nil nil (format "KEY=%s is deleted from strings" key))))))

(defun telega-i18n-plural (key &rest args)
  (let ((val (telega-i18n key)))
    (message "TODO: telega-i18n-plural")
    ))
    
(provide 'telega-i18n)

;;; telega-i18n.el ends here

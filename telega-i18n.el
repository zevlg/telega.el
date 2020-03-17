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

;; See https://translations.telegram.org

;;; Code:
(require 'telega-core)
(require 'telega-tdlib)
(require 'telega-customize)

(defcustom telega-i18n-plural-rule-functions
  (list (cons "en" 'telega-i18n-plural-rule-en)
        (cons "ru" 'telega-i18n-plural-rule-ru))
  "Alist of plural rules functions."
  :type 'alist
  :group 'telega)

(defconst telega-i18n--en-strings nil
  "English language strings.
Loaded from \"etc/langs/en.plist\" in `telega-i18n-init'.")
(defvar telega-i18n--strings nil
  "Language strings for `telega-language'.")
(defvar telega-i18n--plural-func nil)

(defun telega-i18n-init ()
  "Initialize I18N subsystem."
  (setq telega-i18n--en-strings
        (with-temp-buffer
          (insert-file-contents (telega-etc-file "langs/en.plist"))
          (goto-char (point-min))
          (read (current-buffer))))

  (if (equal telega-language "en")
      (setq telega-i18n--strings telega-i18n--en-strings
            telega-i18n--plural-func #'telega-i18n-plural-rule-en)

    ;; Asynchronously load `telega-language' strings
    (telega--getLanguagePackStrings telega-language nil
      (lambda (pack-strings)
        (setq telega-i18n--strings pack-strings)))

    ;; Asynchronously setup `telega-i18n--plural-func'
    (telega--getLanguagePackInfo telega-language
      (lambda (pack-info)
        (let ((plural-code (plist-get pack-info :plural_code)))
          (setq telega-i18n--plural-func
                (cdr (assoc plural-code telega-i18n-plural-rule-functions))))))
    ))

;; See https://www.unicode.org/cldr/charts/latest/supplemental/language_plural_rules.html
(defun telega-i18n-plural-rule (n)
  "Apply plural rule corresponding N value.
Return one of: `:zero_value', `:one_value', `:two_value',
`:few_value', `:many_value' or `:other_value'."
  (or (and telega-i18n--plural-func
           (funcall telega-i18n--plural-func n))
      :other_value))

(defun telega-i18n-plural-rule-en (n)
  "Plural rules for English language."
  (cond ((and (= (% n 10) 1)
              (not (= (% n 100) 11)))
         :one_value)
        ((and (= (% n 10) 2)
              (not (= (% n 100) 12)))
         :two_value)
        ((and (= (% n 10) 3)
              (not (= (% n 100) 13)))
         :few_value)))

(defun telega-i18n-plural-rule-ru (n)
  "Plural rules for Russian language."
  (cond ((and (= (% n 10) 1)
              (not (= (% n 100) 11)))
         :one_value)
        ((and (memq (% n 10) '(2 3 4))
              (not (memq (% n 100) '(12 13 14))))
         :few_value)
        ((or (= (% n 10) 0)
             (memq (% n 10) '(5 6 7 8 9))
             (memq (% n 100) '(11 12 13 14)))
         :many_value)))

(defun telega-i18n (key &rest args)
  "Get I18N string for the KEY."
  (declare (indent 1))
  (let* ((str (or (cdr (assoc (concat "lng_" key) telega-i18n--strings))
                  (cdr (assoc key telega-i18n--strings))
                  (cdr (assoc (concat "lng_" key) telega-i18n--en-strings))
                  (cdr (assoc key telega-i18n--en-strings))))
         (val (or (telega-tl-str str :value)
                  (let ((count (plist-get args :count)))
                    (unless count
                      (if str
                          (error "\"%s\" is plural, `:count' is required" key)
                        (error "\"%s\" is unknown for i18n" key)))
                    (or (and (eq count 0)
                             (telega-tl-str str :zero_value))
                        (telega-tl-str str (telega-i18n-plural-rule count))))
                  (telega-tl-str str :other_value))))

    ;; NOTE: **text** means bold
    (setq val (replace-regexp-in-string
               "\\*\\*\\([^*]+\\)\\*\\*"
               (lambda (match)
                 (propertize (match-string 1 match) 'face 'bold))
               val nil 'literal))

    (while args
      (setq val (replace-regexp-in-string
                 (regexp-quote
                  (concat "{" (substring (symbol-name (car args)) 1) "}"))
                 (format "%s" (cadr args))
                 val nil 'literal))
      (setq args (cddr args)))
    val))

(provide 'telega-i18n)

;;; telega-i18n.el ends here

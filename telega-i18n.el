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
(require 'telega-tdlib)

(defvar telega-i18n-month-names
  '((full "January" "February" "March" "April" "May" "June" "July"
           "August" "September" "October" "November" "December")
    (short "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
           "Aug" "Sep" "Oct" "Nov" "Dec"))
  "Month names in full and short forms.")

(defvar telega-i18n-weekday-names
  '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
  "Day names starting from sunday.")

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

(defun telega-i18n--etc-langs-strings (lang)
  "Read language strings from etc/langs/LANG.plist file."
  (with-temp-buffer
    (insert-file-contents
     (telega-etc-file (concat "langs/" lang ".plist")))
    (goto-char (point-min))
    (read (current-buffer))))

(defun telega-i18n--apply-strings ()
  "Apply i18n strings to telega configuration."
  (setq telega-i18n-weekday-names
        (mapcar (lambda (daynum)
                  (telega-i18n (format "lng_weekday%d" daynum)))
                '(7 1 2 3 4 5 6)))
  (setcdr (assq 'full telega-i18n-month-names)
          (mapcar (lambda (daynum)
                    (telega-i18n (format "lng_month%d" daynum)))
                  '(1 2 3 4 5 6 7 8 9 10 11 12)))
  (setcdr (assq 'short telega-i18n-month-names)
          (mapcar (lambda (daynum)
                    (telega-i18n (format "lng_month%d_small" daynum)))
                  '(1 2 3 4 5 6 7 8 9 10 11 12)))
  )

(defun telega-i18n-init ()
  "Initialize I18N subsystem."
  (setq telega-i18n--en-strings (telega-i18n--etc-langs-strings "en"))

  (if (equal telega-language "en")
      (progn
        (setq telega-i18n--strings telega-i18n--en-strings
              telega-i18n--plural-func #'telega-i18n-plural-rule-en)
        (telega-i18n--apply-strings))

    ;; Asynchronously setup plural code function and local strings (if
    ;; any in etc/langs dir)
    (setq telega-i18n--strings
          (ignore-errors (telega-i18n--etc-langs-strings telega-language)))
    (telega--getLanguagePackInfo telega-language
      (lambda (pack-info)
        (setq telega-i18n--plural-func
              (cdr (assoc (plist-get pack-info :plural_code)
                          telega-i18n-plural-rule-functions)))))
    ;; Asynchronously load `telega-language' strings
    (telega--getLanguagePackStrings telega-language nil
      (lambda (pack-strings)
        ;; Merge in received PACK-STRINGS
        (dolist (pack-string pack-strings)
          (setf (alist-get (car pack-string) telega-i18n--strings
                           nil nil #'string=)
                (cdr pack-string)))
        (telega-i18n--apply-strings)
        ;; NOTE: custom filters might use i18n strings, so update
        ;; custom filters as well
        (let ((telega-filters--dirty t))
          (telega-filters--redisplay))))
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
  (let* ((str (or (cdr (assoc key telega-i18n--strings))
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

    (while args
      (setq val (replace-regexp-in-string
                 (regexp-quote
                  (concat "{" (substring (symbol-name (car args)) 1) "}"))
                 (format "%s" (cadr args))
                 val nil 'literal))
      (setq args (cddr args)))

    ;; NOTE: **text** means bold
    (setq val (replace-regexp-in-string
               "\\*\\*\\([^*]+\\)\\*\\*"
               (lambda (match)
                 (propertize (match-string 1 match) 'face 'bold))
               val nil 'literal))
    val))

(defun telega-i18n-noerror (key)
  "Same as `telega-i18n', but do not trigger an error if KEY is not found.
Return KEY if KEY is unknown to i18n."
  (or (ignore-errors (telega-i18n key))
      key))

(provide 'telega-i18n)

;;; telega-i18n.el ends here

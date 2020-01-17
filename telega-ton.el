;;; telega-ton.el --- TON in telega    -*- lexical-binding:t -*-

;; Copyright (C) 2020 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Jan 16 01:50:25 2020
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

(defmacro telega-ton--send (sexp)
  `(telega-server--send ,sexp "ton"))

(defmacro telega-ton--call (sexp &optional callback)
  `(telega-server--call ,sexp ,callback "ton"))

(defun telega-ton--gen-entropy (&optional len)
  "Generate random string suitable for entropy."
  (with-temp-buffer
    (dotimes (_unused (or len 30))
      (insert (char-to-string (+ 97 (random 23)))))
    (buffer-string)))

(defun telega-ton-init ()
  "Initialize TON."
  (let ((ton-lite-client-conf
         (with-temp-buffer
           (insert-file-contents
            (telega-etc-file "ton-lite-client-test1.config.json"))
           (buffer-string))))
    (telega-ton--send
     (list :@type "init"
           :options (list :@type "options"
                          :config ton-lite-client-conf
                          :keystore_directory telega-ton-keystore-dir
                          :use_callbacks_for_network :false)))))

(provide 'telega-ton)

;;; telega-ton.el ends here

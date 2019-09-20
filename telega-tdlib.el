;;; telega-tdlib.el --- TDLib API interface  -*- lexical-binding:t -*-

;; Copyright (C) 2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Sep 17 15:01:21 2019
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

;; Emacs lisp interface to TDLib API

;;; Code:
(require 'telega-server)


(defmacro with-telega-server-reply (reply post-form call-sexp &optional callback)
  "Do sync or async call to telega-server, processing REPLY by POST-FORM.
CALL-SEXP and CALLBACK are passed directly to `telega-server--call'."
  (declare (indent 2))
  (let ((reply-sym (gensym "reply"))
        (reply-var (car reply)))
    `(let ((,reply-var (telega-server--call
                        ,call-sexp
                        (when ,callback
                          (lambda (,reply-sym)
                            (let ((,reply-var ,reply-sym))
                              (funcall ,callback ,post-form)))))))
       (if ,callback
           ,reply-var
         ,post-form))))

(defun telega--searchEmojis (text &optional exact-match-p callback)
  "Search for emojis by TEXT keywords.
Non-nil EXACT-MATCH-P to return only emojis that exactly matches TEXT."
  (with-telega-server-reply (reply)
      (mapcar (lambda (emoji)
                (telega--desurrogate-apply emoji 'no-props))
              (plist-get reply :emojis))

    (list :@type "searchEmojis"
          :text text
          :exact_match (or exact-match-p :false))
    callback))

(defun telega--setChatDescription (chat descr)
  "Set CHAT's description to DESCR."
  (telega-server--send
   (list :@type "setChatDescription"
         :chat_id (plist-get chat :id)
         :description (or descr ""))))

(provide 'telega-tdlib)

;;; telega-tdlib.el ends here

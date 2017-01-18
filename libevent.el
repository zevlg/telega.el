;;; libevent.el --- FFI to libevent

;; Copyright (C) 2017 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Jan 19 00:09:45 2017
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

;; FFI bindings to libevent

;;; Code:
(require 'ffi)

(define-ffi-library libevent "libevent")

(defconst event-base-t :pointer)

(define-ffi-function event:base_new "event_base_new" :pointer
  nil libevent)

(define-ffi-function event:base_loop "event_base_loop" :int
  (event-base-t :int) libevent)

(defun event--base-loop (base &rest flags)
  (let ((iflags 0))
    (while flags
      (incf iflags (case ((car flags)) (:once 1) (:nonblock 2) (t 0)))
      (setq flags (cdr flags)))

    (event:base_loop base iflags)))

(provide 'libevent)

;;; libevent.el ends here

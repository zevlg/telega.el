;;; libtgl.el --- FFI to libtgl

;; Copyright (C) 2016 by Zajcev Evgeny

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Nov 30 18:18:23 2016
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
(require 'ffi)

(defvar tgl-state-t :pointer)
(defvar tgl-closure-t :pointer)

(define-ffi-library libtgl "libtgl")

(define-ffi-function tgl:state_alloc "tgl_state_alloc" :pointer
  nil libtgl)

(define-ffi-function tgl:set_rsa_key "tgl_set_rsa_key" :void
  (tgl-state-t :pointer) libtgl)

(defun tgl--set-key (tls key)
  (with-ffi-string (keycstr key)
    (tgl:set_rsa_key tls keycstr)))

(define-ffi-function tgl:set_download_directory "tgl_set_download_directory" :void
  (tgl-state-t :pointer) libtgl)
(defun tgl--set-download-dir (tls dir)
  (with-ffi-string (dircstr dir)
    (tgl:set_download_directory tls dircstr)))

(define-ffi-struct tgl-update-callback-t
  (new_msg :type tgl-closure-t)
  (marked_read :type tgl-closure-t)
  (logprintf :type tgl-closure-t)
  (get_values :type tgl-closure-t)
  (callback :type tgl-closure-t)
  (logged_in :type tgl-closure-t)
  (started :type tgl-closure-t)
  (type_notification :type tgl-closure-t)
  (type_in_chat_notification :type tgl-closure-t)
  (type_in_secret_chat_notification :type tgl-closure-t)
  (status_notification :type tgl-closure-t)
  (user_registered :type tgl-closure-t)
  (user_activated :type tgl-closure-t)
  (new_authorization :type tgl-closure-t)
  (chat_update :type tgl-closure-t)
  (channel_update :type tgl-closure-t)
  (user_update :type tgl-closure-t)
  (secret_chat_update :type tgl-closure-t)
  (msg_receive :type tgl-closure-t)
  (our_id :type tgl-closure-t)
  (notification :type tgl-closure-t)
  (user_status_update :type tgl-closure-t)
  (create_print_name :type tgl-closure-t)
  (on_failed_login :type tgl-closure-t)
  )

(define-ffi-function tgl:set_callback "tgl_set_callback" :void
  (tgl-state-t tgl-update-callback-t) libtgl)

(define-ffi-function tgl:init "tgl_init" :int
  (tgl-state-t) libtgl)

(define-ffi-function tgl:incr_verbosity "tgl_incr_verbosity" :void
  (tgl-state-t) libtgl)

(define-ffi-function tgl:set_verbosity "tgl_set_verbosity" :void
  (tgl-state-t :int) libtgl)

(define-ffi-function tgl:enable_pfs "tgl_enable_pfs" :void
  (tgl-state-t) libtgl)

(define-ffi-function tgl:set_test_mode "tgl_set_test_mode" :void
  (tgl-state-t) libtgl)

(provide 'libtgl)

;;; libtgl.el ends here

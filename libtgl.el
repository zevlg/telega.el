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

(define-ffi-library libtgl "libtgl")

(defconst tgl--mtproto-default_key
  (ffi--dlsym "_mtproto_default_key" (libtgl)))
(defconst tgl--mtproto-default-key-len
  (ffi--mem-ref (ffi--dlsym "_mtproto_default_key_len" (libtgl)) :uint))
(defconst tgl--mtproto-default-e
  (ffi--mem-ref (ffi--dlsym "_mtproto_default_e" (libtgl)) :longlong))

(defconst tgl--conn-methods
  (ffi--dlsym "tgl_conn_methods" (libtgl)))
(defconst tgl--timer-methods
  (ffi--dlsym "tgl_libevent_timers" (libtgl)))

(defconst tgl-closure-t :pointer)
(defconst tgl-peer-t :pointer)

(define-ffi-struct tgl-peer-id
  (peer-type :type :int)
  (peer-id :type :int) 
  (access-hash :type :longlong))

(define-ffi-struct tgl-state
  (our-id :type tgl-peer-id)
  (encr-root :type :int)
  (encr-prime :type :pointer)           ;unsigned char*
  (encr-prime-bn :type :pointer)        ;TGLC_bn*
  (encr-param-version :type :int)
  (pts :type :int)
  (qts :type :int)
  (date :type :int)
  (seq :type :int)
  (binlog-enabled :type :int)
  (test-mode :type :int)
  (verbosity :type :int)
  (unread-messages :type :int)
  (active-queries :type :int)
  ;; other fields
  )
(defconst tgl-state-t :pointer)         ;tgl-state*

(define-ffi-struct tgl-file-location
  (dc :type :int)
  (volume :type :longlong)
  (local-id :type :int)
  (secret :type :longlong))

(define-ffi-struct tgl-user-status
  (online :type :int)
  (when :type :int)
  (ev :type :pointer))                  ;tgl_timer*

(define-ffi-struct tgl-user-part
  (first-name :type :pointer)           ;char*
  (last-name :type :pointer)            ;char*
  (phone :type :pointer)                ;char*
  (access-hash :type :longlong)
  (status :type tgl-user-status)
  ;; other fields
  )

(define-ffi-union tgl-peer-union
  (user :type tgl-user-part)
  ;; other parts
  )

(define-ffi-struct tgl-peer
  ;; Common fields
  ;;  tgl-peer-t is actually union
  (id :type tgl-peer-id)
  (flags :type :int)
  (last :type :pointer)                 ; tgl_message*
  (print-name :type :pointer)           ; char*
  (username :type :pointer)             ; char*
  (structure-version :type :int)
  (photo-big :type tgl-file-location)
  (photo-small :type tgl-file-location)
  (last-read-in :type :int)
  (last-read-out :type :int)
  (photo-id :type :longlong)
  (photo :type :pointer)                ; tgl_photo*
  (extra :type :pointer)

  (parts :type tgl-peer-union)
  )

(defalias 'tgl--peer-name 'tgl-peer-print-name)

(defmacro tgl-peer-type (ptype)
  `(cl-case (,ptype)
     (:user 1)
     (:chat 2)
     (:geo-chat 3)
     (:encr-chat 4)
     (:channel 5)
     (:temp-id 100)
     (:random-id 101)
     (t 0)))
 
(define-ffi-function tgl:state_alloc "tgl_state_alloc" :pointer
  nil libtgl)

(define-ffi-function tgl:disable_link_preview "tgl_disable_link_preview" :void
  (tgl-state-t) libtgl)

(define-ffi-function tgl:register_app_id "tgl_register_app_id" :void
  (tgl-state-t :int :pointer) libtgl)

(defun tgl--register-app-id (tls app-id app-hash)
  (with-ffi-string (chash app-hash)
    (tgl:register_app_id tls app-id chash)))

(define-ffi-function tgl:set_app_version "tgl_set_app_version" :void
  (tgl-state-t :pointer) libtgl)

(defun tgl--set-app-version (tls app-version)
  (with-ffi-string (cver app-version)
    (tgl:set_app_version tls cver)))

(define-ffi-function tgl:set_rsa_key "tgl_set_rsa_key" :void
  (tgl-state-t :pointer) libtgl)

(defun tgl--set-key (tls key)
  (with-ffi-string (keycstr key)
    (tgl:set_rsa_key tls keycstr)))

(define-ffi-function tgl:set_rsa_key_direct "tgl_set_rsa_key_direct" :void
  (tgl-state-t :ulong :int :pointer) libtgl)

(define-ffi-function tgl:set_download_directory "tgl_set_download_directory" :void
  (tgl-state-t :pointer) libtgl)
(defun tgl--set-download-dir (tls dir)
  (with-ffi-string (dircstr dir)
    (tgl:set_download_directory tls dircstr)))

(define-ffi-function tgl:set_ev_base "tgl_set_ev_base" :void
  (tgl-state-t :pointer) libtgl)

(define-ffi-function tgl:set_net_methods "tgl_set_net_methods" :void
  (tgl-state-t :pointer) libtgl)

(define-ffi-function tgl:set_timer_methods "tgl_set_timer_methods" :void
  (tgl-state-t :pointer) libtgl)

(define-ffi-struct tgl-update-callback
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
(defconst tgl-update-callback-t :pointer) ;tgl-update-callback*

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

(define-ffi-function tgl:set_binlog_mode "tgl_set_binlog_mode" :void
  (tgl-state-t :int) libtgl)

(define-ffi-function tgl:set_binlog_path "tgl_set_binlog_path" :void
  (tgl-state-t :pointer) libtgl)

(defun tgl--set-binlog-path (tls path)
  (with-ffi-string (cpath path)
    (tgl:set_binlog_path tls cpath)))

(define-ffi-function tgl:login "tgl_login" :void
  (tgl-state-t) libtgl)


(defconst tgl-timer-cb-t :pointer)

; struct tgl_timer *(*alloc) (struct tgl_state *TLS, void (*cb)(struct tgl_state *TLS, void *arg), void *arg);
(defconst tgl-timer-alloc-cif
  (ffi--prep-cif :pointer (vector tgl-state-t tgl-timer-cb-t :pointer)))
; void (*insert) (struct tgl_timer *t, double timeout);
(defconst tgl-timer-insert-cif
  (ffi--prep-cif :void (vector tgl-state-t :pointer :double)))
; void (*remove) (struct tgl_timer *t);
(defconst tgl-timer-remove-cif
  (ffi--prep-cif :void (vector tgl-state-t :pointer)))
; void (*free) (struct tgl_timer *t);
(defconst tgl-timer-free-cif
  (ffi--prep-cif :void (vector tgl-state-t :pointer)))

(define-ffi-struct tgl-timer-methods
  (alloc :type :pointer)
  (insert :type :pointer)
  (remove :type :pointer)
  (free :type :pointer)
  )


; int (*write_out) (struct connection *c, const void *data, int len);
(defconst tgl-net-write-out-cif
  (ffi--prep-cif :int (vector :pointer :pointer :int)))
; int (*read_in) (struct connection *c, void *data, int len);
(defconst tgl-net-read-in-cif
  (ffi--prep-cif :int (vector :pointer :pointer :int)))
; int (*read_in_lookup) (struct connection *c, void *data, int len);
(defconst tgl-net-read-in-lookup-cif
  (ffi--prep-cif :int (vector :pointer :pointer :int)))
; void (*flush_out) (struct connection *c);
(defconst tgl-net-flush-out-cif
  (ffi--prep-cif :void (vector :pointer)))
; void (*incr_out_packet_num) (struct connection *c);
(defconst tgl-net-incr-out-packet-cif
  (ffi--prep-cif :void (vector :pointer)))
; void (*free) (struct connection *c);
(defconst tgl-net-free-cif
  (ffi--prep-cif :void (vector :pointer)))
; struct tgl_dc *(*get_dc) (struct connection *c);
(defconst tgl-net-get-dc-cif
  (ffi--prep-cif :pointer (vector :pointer)))
; struct tgl_session *(*get_session) (struct connection *c);
(defconst tgl-net-get-session-cif
  (ffi--prep-cif :pointer (vector :pointer)))
; struct connection *(*create_connection) (struct tgl_state *TLS, const char *host, int port, struct tgl_session *session, struct tgl_dc *dc, struct mtproto_methods *methods);
(defconst tgl-net-create-connection-cif
  (ffi--prep-cif :pointer (vector tgl-state-t :pointer :int :pointer :pointer :pointer)))

(define-ffi-struct tgl-net-methods
  (write-out :type :pointer)
  (read-in :type :pointer)
  (read-in-lookup :type :pointer)
  (flush-out :type :pointer)
  (incr-out-packet-num :type :pointer)
  (free :type :pointer)
  (get-dc :type :pointer)
  (get-session :type :pointer)
  (create-connection :type :pointer))

;; Binlog
(define-ffi-function tgl:bl_do_dc_option "bl_do_dc_option" :void
  (tgl-state-t :int :int :pointer :int :pointer :int :int) libtgl)

(defun tgl--bl-do-dc-option (tls num name servname)
  (with-ffi-strings ((cname name) (cserv servname))
    (tgl:bl_do_dc_option
     tls 0 num cname (length name) cserv (length servname) 443)))

(define-ffi-function tgl:bl_do_set_working_dc "bl_do_set_working_dc" :void
  (tgl-state-t :int) libtgl)

(define-ffi-function tgl:bl_do_dc_signed "bl_do_dc_signed" :void
  (tgl-state-t :int) libtgl)

(define-ffi-function tgl:bl_do_set_auth_key "bl_do_set_auth_key" :void
  (tgl-state-t :int :pointer) libtgl)

(define-ffi-function tgl:bl_do_set_our_id "bl_do_set_our_id" :void
  (tgl-state-t tgl-peer-id) libtgl)

(defun tgl--bl-set-our-id (tls our-id)
  (with-ffi-temporary (cpid tgl-peer-id)
    (setf (tgl-peer-id-peer-type cpid) (tgl-peer-type :user)
          (tgl-peer-id-peer-id cpid) our-id
          (tgl-peer-id-access-hash cpid) 0)
    (tgl:bl_do_set_our_id tls cpid)))


(define-ffi-function tgl:peer_get "tgl_peer_get" tgl-peer-t
  (tgl-state-t tgl-peer-id) libtgl)

(define-ffi-function tgl:peer_get_by_name "tgl_peer_get_by_name" tgl-peer-t
  (tgl-state-t :pointer) libtgl)

(defun tgl--peer-by-name (tls pname)
  (with-ffi-string (cname pname)
    (tgl:peer_get_by_name tls cname)))

(provide 'libtgl)

;;; libtgl.el ends here

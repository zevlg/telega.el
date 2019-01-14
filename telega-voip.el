;;; telega-voip.el --- Support for VOIP calls.

;; Copyright (C) 2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Jan 10 17:33:13 2019
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
(defgroup telega-voip nil
  "VOIP settings."
  :group 'telega)

(defcustom telega-voip-allow-p2p nil
  "*Non-nil to allow P2P connections for calls."
  :type 'boolean
  :group 'telega-voip)

(defcustom telega-voip-auto-accept t
  "*Non-nil to automatically accept incoming calls."
  :type 'boolean
  :group 'telega-voip)

(defvar telega-voip-buffer nil
  "Buffer currently used for the active call.")
(defvar telega-voip-call nil
  "Currently active call.")

(defvar telega-voip-protocol
  (list :@type "callProtocol"
        :udp_p2p t :udp_reflector t
        :min_layer 65 :max_layer 65))

(defun telega--on-updateCall (event)
  "Called when some call data has been updated."
  (let* ((call (plist-get event :call))
         (state (plist-get call :state)))
    (cl-case (telega--tl-type state)
      (callStateReady
       (telega-server--send (plist-get state :config) "voip-server-config")
       (let ((call-setup
              (list :is_outgoing (or (plist-get call :is_outgoing) :false)
                    :encryption_key (plist-get state :encryption_key)
                    :allow_p2p (or telega-voip-allow-p2p :false)
                    :max_layer (telega--tl-get state :protocol :max_layer)
                    :endpoints (plist-get state :connections))))
         (telega-server--send call-setup "voip-start")))
      ((callStateDiscarded callStateError)
       (telega-server--send nil "voip-stop"))
      ))
  )

(defun telega--createCall (user)
  "Create outgoing call to the USER."
  (telega-server--call
   (list :@type "createCall"
         :user_id (plist-get user :id)
         :protocol telega-voip-protocol)))

(defun telega--acceptCall (call-id)
  "Accept incomming call, defined by CALL-ID."
  (telega-server--call
   (list :@type "acceptCall"
         :call_id call-id
         :protocol telega-voip-protocol)))

(defun telega--discardCall (call-id disconnected-p duration connection-id)
  "Discard call defined by CALL-ID."
  (telega-server--send
   (list :@type "discardCall"
         :call_id call-id
         :is_disconnected (or disconnected-p :false)
         :duration duration
         :connection_id connection-id)))

(provide 'telega-voip)

;;; telega-voip.el ends here

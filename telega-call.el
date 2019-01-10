;;; telega-call.el --- Support for VOIP calls.

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

(defun telega--on-updateCall (event)
  "Called when some call data has been updated."
  )

(defun telega--createCall (user)
  "Create outgoing call to the USER."
  )

(defun telega--acceptCall (call-id)
  "Accept incomming call, defined by CALL-ID."
  )

(defun telega--discardCall (call-id disconnected-p duration connection-id)
  "Discard call defined by CALL-ID."
  (telega-server--send
   (list :@type "discardCall"
         :call_id call-id
         :is_disconnected (or disconnected-p :false)
         :duration duration
         :connection_id connection-id)))


(provide 'telega-call)

;;; telega-call.el ends here

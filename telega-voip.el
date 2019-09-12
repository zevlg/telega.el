;;; telega-voip.el --- Support for VOIP calls  -*- lexical-binding:t -*-

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

;; VOIP calls support for the telega

;;; Code:
(require 'cl-lib)
(require 'telega-core)
(require 'telega-customize)
(require 'telega-ffplay)

(declare-function telega-root--buffer "telega-root")
(declare-function telega-root--chat-update "telega-root" (chat))
(declare-function telega-status--set "telega-root"
                  (conn-status &optional aux-status raw))
(declare-function telega-chat-get "telega-chat")


(defconst telega-voip-protocol
  (list :@type "callProtocol"
        :udp_p2p t :udp_reflector t
        :min_layer 65 :max_layer 65))

(defsubst telega-voip--by-id (call-id)
  "Return call by CALL-ID."
  (alist-get call-id telega-voip--alist))

(defsubst telega-voip--by-user-id (user-id)
  "Return call to user defined by USER-ID."
  (cdr (cl-find user-id telega-voip--alist
                :test '= :key (lambda (el)
                                (plist-get (cdr el) :user_id)))))

(defun telega-voip--call-emojis (call)
  "Return emojis string for the CALL."
  (telega--desurrogate-apply
   ;; Use `mapconcat' instead of direct `concat', because :emojis is a
   ;; vector
   (mapconcat 'identity (telega--tl-get call :state :emojis) "")))

(defun telega-voip--incoming-call ()
  "Return first incoming call that can be accepted."
  (cl-find-if (lambda (call)
                (and (not (plist-get call :is_outgoing))
                     (eq (telega--tl-type (plist-get call :state))
                         'callStatePending)))
              (mapcar 'cdr telega-voip--alist)))

(defun telega-voip--aux-status (call)
  "Update `telega--status-aux' according to active CALL."
  (telega-status--set
   nil (telega-ins--as-string
        (when call
          (let ((user-id (plist-get call :user_id))
                (state (plist-get call :state)))
            ;; server/user receive status
            (cond ((plist-get state :is_received)
                   (telega-ins telega-symbol-heavy-checkmark))
                  ((plist-get state :is_created)
                   (telega-ins telega-symbol-checkmark)))
            (telega-ins telega-symbol-phone)
            (if (plist-get call :is_outgoing)
                (telega-ins "→")
              (telega-ins "←"))

            (apply 'insert-text-button
                   (telega-user--name (telega-user--get user-id) 'name)
                   (telega-link-props 'user user-id))
            (telega-ins-fmt " %s"
              (substring (plist-get state :@type) 9))
            (cl-case (telega--tl-type state)
              (callStatePending
               ;; dot for animation
               (telega-ins "."))
              (callStateReady
               ;; key emojis
               (telega-ins " " (telega-voip--call-emojis call)))))))))

(defun telega--on-updateCall (event)
  "Called when some call data has been updated."
  (let* ((call (plist-get event :call))
         (state (plist-get call :state))
         (call-id (plist-get call :id))
         (old-call (telega-voip--by-id call-id)))
    (setf (alist-get call-id telega-voip--alist) call)

    ;; Update active call value
    (when (eq call-id (plist-get telega-voip--active-call :id))
      (setq telega-voip--active-call call))

    (cl-case (telega--tl-type state)
      (callStatePending
       (unless old-call
         (if (plist-get call :is_outgoing)
             (run-hook-with-args 'telega-call-outgoing-hook call)
           (run-hook-with-args 'telega-call-incoming-hook call)))

       ;; * If no active calls and CALL is outgoing, then make it
       ;;   active
       ;; * If there is active call and `telega-voip-busy-if-active' is
       ;;   non-nil then discard all other incoming calls
       (if (plist-get call :is_outgoing)
           (unless telega-voip--active-call
             (setq telega-voip--active-call call))

         (when (and telega-voip-busy-if-active
                    telega-voip--active-call
                    (not (eq call telega-voip--active-call)))
           (telega--discardCall call-id)))

       (when (and telega-voip-help-echo
                  (not telega-voip--active-call)
                  (eq call (telega-voip--incoming-call)))
         (let ((prefix (when (eq (telega-root--buffer) (window-buffer))
                         "\\<telega-root-mode-map>")))
           (message "telega: Press `%s' to answer, `%s' to decline"
                    (substitute-command-keys
                     (concat prefix "\\[telega-voip-accept]"))
                    (substitute-command-keys
                     (concat prefix "\\[telega-voip-discard]"))))))

      (callStateReady
       (unless (eq call telega-voip--active-call)
         (error "Another call became Ready, while having active call"))

       (run-hook-with-args 'telega-call-ready-hook call)

       (let ((start
              (list :@command "start"
                    :server_config (plist-get state :config)
                    :is_outgoing (or (plist-get call :is_outgoing) :false)
                    :encryption_key (plist-get state :encryption_key)
                    :allow_p2p (or telega-voip-allow-p2p :false)
                    :max_layer (telega--tl-get state :protocol :max_layer)
                    :endpoints (plist-get state :connections))))
         (when telega-voip-logfile
           (telega-server--send
            (list :@command "config"
                  :log-file-path telega-voip-logfile) "voip"))

         (telega-server--send start "voip"))

       (when telega-voip-help-echo
         (message "telega: Press `%s' to hang up"
                  (substitute-command-keys
                   (concat (when (eq (telega-root--buffer) (window-buffer))
                             "\\<telega-root-mode-map>")
                           "\\[telega-voip-discard]")))))

      (callStateError
       (let ((err (plist-get state :error))
             (user (telega-user--get (plist-get call :user_id))))
         (message "Error[%d] calling %s: %s" (plist-get err :code)
                  (telega-user--name user) (plist-get err :message))))

      (callStateDiscarded
       (let ((discad (plist-get state :reason))
             (user (telega-user--get (plist-get call :user_id))))
         (message "Call %s discaded: %s" (telega-user--name user)
                  (substring (plist-get discad :@type) 17))))
      )

    ;; Delete call from the list, if call is ended
    (when (memq (telega--tl-type state) '(callStateError callStateDiscarded))
      (unwind-protect
          (run-hook-with-args 'telega-call-end-hook call)
        (when (eq telega-voip--active-call call)
          (telega-server--send (list :@command "stop") "voip")
          (setq telega-voip--active-call nil))
        (setq telega-voip--alist (assq-delete-all call-id telega-voip--alist))))

    ;; Update corresponding chat button
    (when-let ((chat (telega-chat-get (plist-get call :user_id) 'offline)))
      (telega-root--chat-update chat))

    ;; Update aux status
    (telega-voip--aux-status
     (or telega-voip--active-call (telega-voip--incoming-call)))
    ))

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

(defun telega--discardCall (call-id &optional disconnected-p duration connection-id)
  "Discard call defined by CALL-ID."
  (telega-server--send
   (nconc (list :@type "discardCall"
                :call_id call-id)
          (when disconnected-p
            (list :is_disconnected (or disconnected-p :false)))
          (when duration
            (list :duration duration))
          (when connection-id
            :connection_id connection-id))))


(defun telega-voip-discard (call)
  "Discard the CALL.
If called interactively then discard active call."
  (interactive (list (or telega-voip--active-call
                         (telega-voip--incoming-call)
                         (error "No active or incoming call to discard"))))
  (when (eq (plist-get call :id)
            (plist-get telega-voip--active-call :id))
    (telega-server--send (list :@command "stop") "voip"))

  (telega--discardCall (plist-get call :id)))

(defun telega-voip-active-call-p (call)
  "Return non-nil if CALL currently active.
Compare calls by `:id'."
  (eq (plist-get call :id) (plist-get telega-voip--active-call :id)))

(defun telega-voip-activate-call (call)
  "Activate the CALL, i.e. make CALL currently active.
Discard currently active call, if any."
  (when telega-voip--active-call
    (telega-voip-discard telega-voip--active-call))

  (setq telega-voip--active-call call)
  (telega-voip--aux-status telega-voip--active-call))

(defun telega-voip-call (user &optional force)
  "Call the USER.
Discard active call if any."
  (when (or force
            (not telega-voip--active-call)
            (y-or-n-p (format "Active call will be discarded, call %s? "
                              (telega-user--name user 'name))))
    (when telega-voip--active-call
      (telega-voip-discard telega-voip--active-call)
      (setq telega-voip--active-call nil))

    (telega--createCall user)))

(defun telega-voip-accept (call)
  "Accept last incoming CALL.
Discard active call if any."
  (interactive (list (telega-voip--incoming-call)))

  (unless call
    (error "No incoming call to accept"))

  ;; TODO: might be situation when we twice accept the call, it will
  ;; lead to call discard.  Check that CALL is not already active
  (telega--acceptCall (plist-get call :id))
  (telega-voip-activate-call call)

  ;; TODO: show call buffer for the CALL
  )

(defun telega-voip-buffer-show (_call)
  "Show callbuf for the CALL."
  (interactive (list telega-voip--active-call))
  (message "TODO: `telega-voip-buffer-show'"))

(defun telega-voip-list-calls (only-missed)
  "List recent calls.
If prefix arg is given then list only missed calls."
  (interactive "P")
  (let* ((ret (telega-server--call
               (list :@type "searchCallMessages"
                     :from_message_id 0
                     :limit 100
                     ;; NOTE: `:only_missed t' gives some problems :(
                     :only_missed (if (null only-missed) :false t))))
         (messages (mapcar #'identity (plist-get ret :messages))))
    (with-telega-help-win "*Telega Recent Calls*"
      (telega-ins (if only-missed "Missed" "All") " Calls\n")
      (telega-ins (make-string (- (point-max) 2) ?-) "\n")

      (dolist (call-msg messages)
        (telega-ins--with-attrs (list :align 'left
                                      :min 60
                                      :max 60
                                      :elide t)
          (telega-ins--username (plist-get call-msg :sender_user_id) 'name)
          (telega-ins ": ")
          (telega-ins--content-one-line call-msg))
        (telega-ins--with-attrs (list :align 'right :min 10)
          (telega-ins--date (plist-get call-msg :date)))
        (telega-ins "\n")))
    ))


(defun telega-voip-sounds--play-incoming (_call)
  "Incomming CALL pending."
  (unless telega-voip--active-call
    (telega-ffplay-run (telega-etc-file "sounds/call_incoming.mp3") nil
                       "-nodisp" "-loop" "0")
    ))

(defun telega-voip-sounds--play-outgoing (_call)
  "Outgoing CALL initiated."
  (telega-ffplay-run (telega-etc-file "sounds/call_outgoing.mp3") nil
                     "-nodisp" "-loop" "0"))

(defun telega-voip-sounds--play-connect (_call)
  "Call ready to be used."
  (telega-ffplay-run (telega-etc-file "sounds/call_connect.mp3") nil
                     "-nodisp"))

(defun telega-voip-sounds--play-end (call)
  "CALL finished."
  (when (or (not telega-voip--active-call)
            (telega-voip-active-call-p call))
    (let* ((state (plist-get call :state))
           (reason (when (eq (telega--tl-type state) 'callStateDiscarded)
                     (telega--tl-type (plist-get state :reason))))
           (snd (if (and (plist-get call :is_outgoing)
                         (memq reason '(callDiscardReasonDeclined
                                        callDiscardReasonMissed)))
                    "sounds/call_busy.mp3"
                  "sounds/call_end.mp3")))
      (telega-ffplay-run (telega-etc-file snd) nil "-nodisp"))
    ))

(defun telega-voip-sounds-mode (&optional arg)
  "Toggle soundsToggle telega notifications on or off.
With positive ARG - enables notifications, otherwise disables.
If ARG is not given then treat it as 1."
  (interactive "p")
  (if (or (null arg) (> arg 0))
      (progn
        (add-hook 'telega-call-incoming-hook 'telega-voip-sounds--play-incoming)
        (add-hook 'telega-call-outgoing-hook 'telega-voip-sounds--play-outgoing)
        (add-hook 'telega-call-ready-hook 'telega-voip-sounds--play-connect)
        (add-hook 'telega-call-end-hook 'telega-voip-sounds--play-end))
    (remove-hook 'telega-call-incoming-hook 'telega-voip-sounds--play-incoming)
    (remove-hook 'telega-call-outgoing-hook 'telega-voip-sounds--play-outgoing)
    (remove-hook 'telega-call-ready-hook 'telega-voip-sounds--play-connect)
    (remove-hook 'telega-call-end-hook 'telega-voip-sounds--play-end)))

(provide 'telega-voip)

;; On load
(when telega-voip-use-sounds
  (telega-voip-sounds-mode 1))

;;; telega-voip.el ends here

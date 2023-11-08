;;; telega-live-location.el --- Live location for telega, based on geo.el  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (c) 2020 Would (oldosfan).

;; telega-live-location.el is part of geo.el.

;; geo.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; geo.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with geo.el. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; ellit-org:
;; ** /telega-live-location.el/ -- Manage live location in Telega using geo.el
;;
;; Enable this mode with {{{kbd(M-x global-telega-live-location-mode RET)}}}
;;
;; This mode installs new ~live-geo-location~ chat attach type, use it
;; with {{{kbd(C-c C-a live-geo-location RET)}}} in the chatbuf.
;; 
;; This mode requires the =geo.el= library, available at
;; https://git.sr.ht/~oldosfan/geo-xdg.el
;; 
;; Take into account that using ~geo-simulate~ backend to fake geo
;; location data is Telegram API ToS violation.  See 1.4 in
;; https://core.telegram.org/api/terms

;;; Code:
(require 'cl-lib)
(require 'telega)

(require 'geo)

(defun telega-live-location--geo-loc (geo-loc)
  "Covert geo location GEO-LOC into telega location plist."
  (list :latitude (geo-location-lat geo-loc)
        :longitude (geo-location-lon geo-loc)))

(defun telega-live-location--on-geo-location-changed (geo-loc)
  "Hook to be called when the location is changed.
LOC should be the new location."
  (setq telega-my-location (telega-live-location--geo-loc geo-loc))

  (when (telega-server-live-p)
    ;; Share my location to Telegram in case `:is_location_visible'
    ;; option is used
    (when (plist-get telega--options :is_location_visible)
      (telega--setLocation telega-my-location))

    ;; Asynchronously update all live location messages
    (telega--getActiveLiveLocationMessages
     (lambda (messages)
       (let ((geo-heading (or (geo-last-heading) 0))
             (loc (nconc (list :@type "location") telega-my-location)))
         (dolist (msg messages)
           (telega--editMessageLiveLocation
            msg loc
            ;; NOTE: Convert half-circle azimuth (used by geo.el) to
            ;; full-circle azimuth (used by TDLib)
            :heading (round (if (< geo-heading 0)
                                (+ 360 geo-heading)
                              geo-heading)))))))))

(defun telega-live-location--read-location-advice (prompt &rest args)
  "Advice for `telega-read-live-location'.
Return live location from geo module, otherwise fallback to
`telega-read-live-location'."
  (if (geo-last-location)
      (telega-live-location--geo-loc (geo-last-location))

    (warn "Last location from geo module is unavailable")
    (apply #'telega-read-live-location prompt args)))

(defun telega-live-location-attach-live-geo-location ()
  "Attach live location from geo module to chatbuf."
  (interactive)
  (let ((current-prefix-arg '(4)))      ; for live location attach
    (call-interactively 'telega-chatbuf-attach-location)))

(define-minor-mode global-telega-live-location-mode
  "Global mode to manage live locations with `geo.el' package."
  :init-value nil :global t :group 'telega-modes
  (if global-telega-live-location-mode
      (progn
        (add-hook 'geo-data-changed-hook
                  #'telega-live-location--on-geo-location-changed)
        (advice-add 'telega-read-live-location :override
                    #'telega-live-location--read-location-advice)
        ;; For `C-c C-a live-geo-location RET'
        (unless (assoc "live-geo-location" telega-chat-attach-commands)
          (add-to-list 'telega-chat-attach-commands
                       '("live-geo-location"
                         (eval (geo-last-location))
                         #'telega-live-location-attach-live-geo-location)
                       'append))

        ;; Update location on start
        (when-let ((geo-loc (geo-last-location)))
          (telega-live-location--on-geo-location-changed geo-loc))
        )

    (remove-hook 'geo-data-changed-hook
                 #'telega-live-location--on-geo-location-changed)
    (advice-remove 'telega-read-live-location
                   #'telega-live-location--read-location-advice)
    (when-let ((cc (assoc "live-geo-location" telega-chat-attach-commands)))
      (setq telega-chat-attach-commands (remove cc telega-chat-attach-commands)))
    ))

(provide 'telega-live-location)

;;; telega-live-location.el ends here

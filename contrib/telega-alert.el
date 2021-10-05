;;; telega-alert.el --- Notifications with alert.el  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2020 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Apr 23 12:48:24 2020
;; Package-Requires: ((alert "1.2"))
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

;;; ellit-org:
;; ** /telega-alert.el/ -- Notifications using =alert.el=
;; 
;; To enable notifications using =alert.el= use:
;; #+begin_src emacs-lisp
;; (telega-alert-mode 1)
;; #+end_src
;; 
;; Alerts for =telega.el= are fired with ~:mode 'telega-chat-mode~
;; value.  You might use this to customize alert rules with
;; ~alert-add-rule~.

;;; Code:
(require 'telega)
(require 'alert)

(defun telega-alert--notify (notify-spec)
  (alert (plist-get notify-spec :body)
         :title (plist-get notify-spec :title)
         :icon (telega-etc-file "telegram-logo.svg")
         :mode 'telega-chat-mode))

;;;###autoload
(define-minor-mode telega-alert-mode
  "Notify using alert module."
  :init-value nil :global t :group 'telega-notifications
  (if telega-alert-mode
      (progn
        (telega-notifications-mode 1)
        (advice-add 'telega-notifications--notify :override
                    #'telega-alert--notify)
        (advice-add 'telega-notifications--close :override
                    #'ignore))

    (telega-notifications-mode -1)
    (advice-remove 'telega-notifications--notify
                   #'telega-alert--notify)
    (advice-remove 'telega-notifications--close
                   #'ignore)))

(provide 'telega-alert)

;;; telega-alert.el ends here

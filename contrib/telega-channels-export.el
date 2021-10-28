;;; telega-channels-export.el --- Export all subscribed channels to OPML -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2021 by 4da.

;; Author: 4da <4da@sandycat.info>
;; Created: Sat Oct 21 15:00 2021
;; Package-Requires: ((esxml "0.3.7"))
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
;;; ellit-org:
;; ** /telega-channels-export.el/ -- Export all telegam channels to OPML
;;
;; Usage:
;;
;; Login to telega via M-x telega, after that call M-x
;; telega-channels-export and choose file name to channels export to.
;; You can customize telega-export-channels-template variable with
;; template of rsshub or rssbrdige (or whatever that supports
;; telegram) url.

;;; Code:
(require 'telega)
(require 'esxml)

(defsubst telega-chat-description (chat)
  "Return CHAT's description. "
  (telega-tl-str (telega--full-info (telega-chat--info chat)) :description :formattedText))

(defcustom telega-export-channels-template
  "https://sebsauvage.net/rss-bridge/?action=display&bridge=Telegram&username=%s&format=Atom"
  "URL template where signle %s denotes channel username"
  :type 'string
  :group 'telega
  )

(defun telega-channels-export--outline (username title description)
  (let ((http-url (format "https://t.me/%s" username))
        (rss-url (format telega-export-channels-template username))
        (plain-title (if title (substring-no-properties title) ""))
        (plain-description (if description (substring-no-properties description) "")))

    `(outline ((text . ,plain-title)
               (description . ,plain-description)
               (htmlUrl . ,http-url)
               (type . "Atom")
               (xmlUrl . ,rss-url)
               ))))

(defun telega-channels-export (filename)
  "Export all telegam channels to OPML"

  (interactive "FSelect output file: ")

  (let* ((filter '(and (type channel) has-username))

        (outlines
         (mapcar (lambda (chat)
                   (telega-channels-export--outline
                    (telega-chat-username chat)
                    (telega-chat-title chat)
                    (telega-tl-str (telega--full-info (telega-chat--info chat))
                                   :description :formattedText)
                    ))
                 (telega-filter-chats
                  telega--ordered-chats filter)))

        (toplevel-outline
         `(outline ((text . "Telegram"))
                   ,@outlines)))

    (with-temp-file filename
      (insert
       (esxml-to-xml `(opml ((version . "1.0"))
                            (head () (title () "Exported channels"))
                            (body ()
                                  (outline ((text . "Telegram"))
                                           ,@outlines
                                           )
                                  )
                            ))))

  ;; TODO find out why this fails
  ;; (sgml-pretty-print 0 (buffer-size))
  )

  (message (format "Channels saved to %s" filename))
)

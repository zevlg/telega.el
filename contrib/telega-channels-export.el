;;; telega-channels-export.el --- Export all subscribed channels to OPML -*- lexical-binding: t; -*-

;; Copyright (C) 2021 by 4da.

;; Author: 4da <4da@sandycat.info>
;; Created: Sat Oct 21 15:00 2021
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
;; Allows user to get a list of all telegram channels she is subscribed to which have usernames.
;;
;; Usage:
;;
;; Login to telega via M-x telega, after that call M-x
;; telega-export-channels and choose file name to channels export to.
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

(defun make-rss-url (username)
  (format telega-export-channels-template username))

(defun make-channel-outline (username title description)
  (setq http-url (format "https://t.me/%s" username))
  (setq rss-url (make-rss-url username))
  (setq plain-title (format "%s" title))
  (setq plain-description (format "%s" description))
  (set-text-properties 0 (length title) nil plain-title)
  (set-text-properties 0 (length description) nil plain-description)

  `(outline ((text . ,plain-title)
             (description . ,plain-description)
             (htmlUrl . ,http-url)
             (type . "Atom")
             (xmlUrl . ,rss-url)
             )))

(defun telega-export-channels ()
  (interactive)

  (setq filename (read-file-name "Enter file name:"))
  (setq filter '(and (type channel) has-username))

  (setq outlines
        (mapcar (lambda (chat)
                  (make-channel-outline
                   (telega-chat-username chat)
                   (telega-chat-title chat)
                   (telega-chat-description chat)
                   ))
                (telega-filter-chats
                 telega--ordered-chats filter)))

  (setq toplevel-outline
        `(outline ((text . "Telegram"))
                  ,@outlines))

  (with-temp-file filename
    (insert
     (esxml-to-xml `(opml ((version . "1.0"))
                          (head () (title () "Exported channels"))
                          (body ()
                                (outline ((text . "Telegram"))
                                         ,@outlines
                                         )
                                )
                          )))

    ;; TODO find out why this fails
    ;; (sgml-pretty-print 0 (buffer-size))
    )

  (message (format "Channels saved to %s" filename))
)

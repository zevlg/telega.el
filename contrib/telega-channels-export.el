;;; telega-channels-export.el --- Export all subscribed channels to OPML -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2021 by 4da.

;; Author: 4da <4da@sandycat.info>
;; Created: Sat Oct 21 15:00 2021
;; Package-Requires: ((esxml "0.3.7"))
;; Keywords: news

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
;; ** /telega-channels-export.el/ -- Export Telegam channels to OPML  :new:
;;
;; Use {{{kbd(M-x telega-channels-export RET)}}} to export public
;; channels shown in the rootbuf (i.e. matching Active Chat Filter)
;; into XML file of [[https://en.wikipedia.org/wiki/OPML][OPML]]
;; format.  Making it possible to import this file into any RSS reader
;; that supports OPML.
;;
;; Customizable options:
;; - {{{user-option(telega-channels-export-template, 2)}}}

;;; Code:
(require 'telega)
(require 'xml)                          ; `xml-escape-string'
(require 'esxml)

(defcustom telega-channels-export-template
  "https://sebsauvage.net/rss-bridge/?action=display&bridge=Telegram&username=%s&format=Atom"
  "URL template where single %s is substituteed with channel username.
Use rsshub or rssbrdige (or whatever that supports Telegram) url."
  :type 'string
  :group 'telega)

(defun telega-channels-export--outline (chat)
  "Return OPML outline for the CHAT.
CHAT must be a public channel or supergroup."
  (let* ((username (telega-chat-username chat))
         (http-url (concat (or (plist-get telega--options :t_me_url)
                               "https://t.me/")
                           username))
         (rss-url (format telega-channels-export-template username))
         (title (substring-no-properties (or (telega-chat-title chat) "")))
         (description (substring-no-properties
                       (or (telega-tl-str (telega--full-info
                                           (telega-chat--info chat))
                                          :description)
                           ""))))
    `(outline ((text . ,(xml-escape-string title))
               (description . ,(xml-escape-string description))
               (htmlUrl . ,(xml-escape-string http-url))
               (type . "Atom")
               (xmlUrl . ,(xml-escape-string rss-url))))))

;;;###autoload
(defun telega-channels-export (filename)
  "Export all telegam channels to OPML"
  (interactive "FExport channels into XML file: ")

  (let* ((filter '(and (type channel) has-username))
         (chats (telega-filter-chats telega--filtered-chats filter))
         (outlines (mapcar #'telega-channels-export--outline chats)))
    (with-temp-file filename
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
      (insert
       (pp-esxml-to-xml
        `(opml ((version . "1.0"))
               (head () (title () "Exported channels"))
               (body ()
                     (outline ((text . "Telegram"))
                              ,@outlines))))))

    (message "%d channels exported into %s" (length chats) filename)))

(provide 'telega-channels-export)

;;; telega-channels-export.el ends here

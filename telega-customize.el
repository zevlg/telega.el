;;; telega-customize.el --- Customization for telega

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Mon Apr 23 18:11:45 2018
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
(defgroup telega nil
  "Telegram client."
  :prefix "telega-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/zevlg/telega.el"))

(defcustom telega-directory (expand-file-name "~/.telega")
  "Directory for telega runtime files."
  :type 'string
  :group 'telega)

(defcustom telega-cache-dir (expand-file-name "cache" telega-directory)
  "*Directory for telegram downloads."
  :type 'string
  :group 'telega)

(defcustom telega-language "en"
  "*IETF language tag of the user's language."
  :type 'string
  :group 'telega)

(defcustom telega-options-plist nil
  "*Plist of options to set.
Only writable options can be set.  See: https://core.telegram.org/tdlib/options
NOT IMPLEMENTED"
  :type 'plist
  :group 'telega)

(defcustom telega-debug nil
  "*Non-nil to enable telega debugging buffer."
  :type 'boolean
  :group 'telega)

(defcustom telega-use-test-dc nil
  "*Non-nil to use telegram's test environment instead of production."
  :type 'bool
  :group 'telega)

(defcustom telega-use-file-database t
  "Non-nil to save downloaded/uploaded files among restarts."
  :type 'bool
  :group 'telega)

(defcustom telega-use-chat-info-database t
  "Cache chats informations among restarts.
Implies `telega-use-file-database' set to non-nil."
  :type 'bool
  :group 'telega)

(defcustom telega-use-message-database t
  "Cache chats and messages among restarts.
Implies `telega-use-chat-info-database' set to non-nil."
  :type 'bool
  :group 'telega)

(defcustom telega-socks5-proxy nil
  "*Plist specifying socks5 proxy to use.
`(:server <SERVER> :port <PORT> :username <NAME> :password <PASSWORD>)"
  :type 'plist
  :group 'telega)

(defcustom telega-eliding-string "..."
  "*String used for eliding long string in formats.
Nice looking middle dots can be done by setting
`telega-eliding-string' to `(make-string 3 #x00b7)'
or set it to \"\\u2026\" or \"\\u22ef\" to use unicode char for
ellipsis."
  :type 'string
  :group 'telega)

(defcustom telega-pin-string "p"
  "*String used as pin.
You could use \"\\U0001F4CC\" if you have utf-8 locale."
  :type 'string
  :group 'telega)

(defcustom telega-week-start-day 1
  "*The day of the week on which a week in the calendar begins.
0 means Sunday, 1 means Monday (default), and so on."
  :type 'integer
  :group 'telega)

(defcustom telega-week-day-names
  '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
  "*Week day names to use when printing times."
  :type 'list
  :group 'telega)

(defcustom telega-chat-input-prompt ">>>"
  "*Prompt for the chat buffers."
  :type 'string
  :group 'telega)

(defcustom telega-chat-reply-prompt "`->"
  "*Prompt to use when replying to message."
  :type 'string
  :group 'telega)

(defcustom telega-chat-edit-prompt "`->"
  "*Prompt to use when replying to message."
  :type 'string
  :group 'telega)

(defcustom telega-chat-input-ring-size 50
  "*Size of the chat input history."
  :type 'integer
  :group 'telega-chat)

(defcustom telega-chat-fill-column fill-column
  "*Column to fill messages to."
  :type 'integer
  :group 'telega-chat)

(defcustom telega-chat-msg-pending "*"
  "Mark to use for pending messages."
  :type 'string
  :group 'telega-chat)

(defcustom telega-chat-msg-succeeded "v"
  "Mark messages that are successfully sent to server."
  :type 'string
  :group 'telega-chat)

(defcustom telega-chat-msg-failed (propertize "!" 'face 'error)
  "Mark messages that have sending state failed."
  :type 'string
  :group 'telega-chat)

(defcustom telega-chat-initial-history-messages 50
  "Number of messages to fetch, when entering new chat."
  :type 'integer
  :group 'telega-chat)

(defcustom telega-chat-insert-date-breaks t
  "*Non-nil to insert breaks inbetween messages of different dates.
TODO"
  :type 'boolean
  :group 'telega-chat)

(defcustom telega-completing-read-function 'ido-completing-read
  "Completing read function to use."
  :type 'function
  :group 'telega)


;;; Faces
(defface telega-chat-prompt
  '((t (:inherit default :weight bold)))
  "Face for chat input prompt"
  :group 'telega)

(defface telega-unread-unmuted-modeline
  '((t :inherit error))
  "Face used to display number of unread/unmuted messages in modeline."
  :group 'telega)

(defface telega-muted-count
  `((t :inherit shadow))
  "Face to display count of messages in muted chats."
  :group 'telega)

(defface telega-unmuted-count
  '((t :inherit default :foreground "blue"))
  "Face to display count messages in unmuted chats."
  :group 'telega)

(defface telega-mention-count
  '((t :weight bold :foreground "blue"))
  "Face to display count of the mentions."
  :group 'telega)

(defface telega-entity-type-mention
  '((t :inherit default :foreground "blue"))
  "Face to display @mentions."
  :group 'telega-chat)

(defface telega-entity-type-hashtag
  '((t :inherit default :foreground "blue"))
  "Face to display #hashtags."
  :group 'telega-chat)

(defface telega-entity-type-cashtag
  '((t :inherit default :foreground "blue"))
  "Face to display #hashtags."
  :group 'telega-chat)

(defface telega-entity-type-botcommand
  '((t :inherit default :foreground "blue"))
  "Face to display /command if there is bot in chat."
  :group 'telega-chat)

(defface telega-entity-type-bold
  '((t :inherit bold))
  "Face to display bold text."
  :group 'telega-chat)

(defface telega-entity-type-italic
  '((t :inherit italic))
  "Face to display italic text."
  :group 'telega-chat)

(defface telega-entity-type-code
  '((t :family "Monospace Serif"))
  "Face to display code."
  :group 'telega-chat)

(defface telega-entity-type-pre
  '((t :family "Monospace Serif"))
  "Face to display text ala <pre> HTML tag."
  :group 'telega-chat)

(defface telega-chat-user-title
  '((t :inherit 'widget-field))
  "Face to display user title in chat buffers."
  :group 'telega-chat)

(defface telega-chat-self-title
  '((t :inherit 'widget-field :bold t))
  "Face to display title of myself in chat buffers."
  :group 'telega-chat)


(defgroup telega-hooks nil
  "Hooks called by telega."
  :group 'telega)

(defcustom telega-ready-hook nil
  "Hook called when telega is ready to process queries."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-closed-hook nil
  "Hook called when telega exited."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-chats-fetched-hook nil
  "Hook called when all chats list has been received."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-user-update-hook '(telega-root--user-update)
  "Hook called with single argument USER, when USER's info is updated."
  :type 'hook
  :group 'telega-hooks)

(provide 'telega-customize)

;;; telega-customize.el ends here

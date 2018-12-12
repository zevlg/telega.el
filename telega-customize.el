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

(defcustom telega-use-short-filenames t
  "*Non-nil to cut /home/user/.telega/cache from filenames."
  :type 'boolean
  :group 'telega)

(defcustom telega-chat--display-buffer-action display-buffer--same-window-action
  "Action value when poping to chatbuffer.
See docstring for `display-buffer' for the values."
  :type 'cons
  :group 'telega)


(defgroup telega-server nil
  "Customisation for telega-server."
  :prefix "telega-server-"
  :group 'telega)

(defcustom telega-server-command "telega-server"
  "Command to run as telega server."
  :type 'string
  :group 'telega-server)

(defcustom telega-server-logfile
  (expand-file-name "telega-server.log" telega-directory)
  "*Write server logs to this file."
  :type 'string
  :group 'telega-server)

(defcustom telega-server-verbosity 5
  "*Verbosity level for server process."
  :type 'number
  :group 'telega-server)

(defcustom telega-server-call-timeout 0.5
  "*Timeout for `telega-server--call'."
  :type 'number
  :group 'telega-server)


(defgroup telega-root nil
  "Customization for telega-root-mode"
  :prefix "telega-root-"
  :group 'telega)

(defcustom telega-root-buffer-name "*Telega Root*"
  "*Buffer name for telega root buffer."
  :type 'string
  :group 'telega-root)

(defcustom telega-root-fill-column fill-column
  "*Maximum width to use in root buffer to display active filters and chats."
  :type 'integer
  :group 'telega-root)

(defcustom telega-chat-button-width 28
  "*Width for the chat buttons in root buffer.."
  :type 'integer
  :group 'telega-root)

(defcustom telega-chat-button-brackets
  '((all "[" "]"))
  "Brackets to use for chat button."
  :type 'list
  :group 'telega-root)

(defcustom telega-chat-me-custom-title (propertize "Saved Messages" 'face 'bold)
  "*Custom title for the chat with myself.
Set it to `nil' to use your user name instead of default \"Saved Messages\"."
  :type 'string
  :group 'telega-root)

(defcustom telega-status-animate-interval 0.5
  "Dots animation interval for telega status shown in root buffer."
  :type 'number
  :group 'telega-root)


(defgroup telega-filter nil
  "Customize chats filtration."
  :prefix "telega-filter-"
  :group 'telega)

(defcustom telega-filter-default 'all
  "*Default chats filter to apply.
For example:
  `(any pin unread)'  - to show pinned or chats with unread messages.
"
  :type 'list
  :group 'telega-filter)

(defcustom telega-filters-custom
  '(("All" . all)
    ("Secrets" . (type secret))
    ("Private" . (type private))
    ("Channels" . (type channel))
    ("Groups" . (type basicgroup supergroup))
    ("Bots" . (type bot))
    ("Notify" . notify))
  "*Alist of custom filters for chats.
In form (NAME . FILTER-SPEC)."
  :type 'alist
  :group 'telega-filter)

(defcustom telega-filter-custom-expand t
  "*Non-nil to expand custom filter when adding to active filters."
  :type 'boolean
  :group 'telega-filter)

(defcustom telega-filter-button-width 28
  "*Width of the custom filter buttons."
  :type 'integer
  :group 'telega-filter)


(defgroup telega-inserter nil
  "Group to customize inserters used by telega for formatting."
  :group 'telega
  :prefix "telega-inserter-")

(defcustom telega-inserter-for-filter-button 'telega-ins--filter
  "Inserter for the custom filter buttons."
  :type 'function
  :group 'telega-inserters)

(defcustom telega-inserter-for-chat-button 'telega-ins--chat-full
  "Inserter for the chat button in root buffer."
  :type 'function
  :group 'telega-inserters)

(defcustom telega-inserter-for-msg-button 'telega-ins--msg
  "Inserter for message button in chat buffer."
  :type 'function
  :group 'telega-inserter)


(defgroup telega-chat nil
  "Customization for chat buffer."
  :group 'telega)

(defcustom telega-chat-input-prompt ">>> "
  "*Prompt for the chat buffers."
  :type 'string
  :group 'telega-chat)

(defcustom telega-chat-reply-prompt telega-chat-input-prompt
  "*Prompt to use when replying to message."
  :type 'string
  :group 'telega-chat)

(defcustom telega-chat-edit-prompt telega-chat-input-prompt
  "*Prompt to use when replying to message."
  :type 'string
  :group 'telega)

(defcustom telega-chat-input-ring-size 50
  "*Size of the chat input history."
  :type 'integer
  :group 'telega-chat)

(defcustom telega-chat-fill-column fill-column
  "*Column to fill chat messages to."
  :type 'integer
  :group 'telega-chat)

(defcustom telega-chat-history-limit 50
  "Number of messages to fetch on history requests."
  :type 'integer
  :group 'telega-chat)

(defcustom telega-chat-insert-date-breaks t
  "*Non-nil to insert breaks inbetween messages of different days.
NOT YET IMPLEMENTED"
  :type 'boolean
  :group 'telega-chat)

(defcustom telega-msg-show-sender-status nil
  "*Non-nil to show message sender status.
Such as admin, creator, etc
DO NOT USE.  TODO: sender statuses need to be cached."
  :type 'boolean
  :group 'telega-msg)

(defcustom telega-msg-photo-types '("w" "d" "y" "c" "x" "b" "m" "s" "a")
  "*Size types to use for full size photos."
  :type 'list
  :group 'telega-msg)

(defcustom telega-msg-photo-slices 'auto
  "*Split photo into horisontal slices for better scrolling experience.
Integer or `auto'."
  :type 'integer
  :group 'telega-msg)

(defcustom telega-photo-maxsize '(432 . 960)
  "*Limit displayed image size to this (WIDTH . HEIGHT) value."
  :type '(cons integer integer)
  :group 'telega)

(defcustom telega-photo-fill-char ?o
  "Char to use for filling image placement."
  :type 'char
  :group 'telega)
  
;; (defcustom telega-msg-photo-props
;;   '(:scale 1.0 :max-height 432 :max-width 960 :ascent 100)
;;   "*Properties to apply when image is created for the photos."
;;   :type 'plist
;;   :group 'telega-msg)

(defcustom telega-auto-download
  '((photos all)
    (videos opened)
    (files opened)
    (voice-messages opened)
    (video-messages opened)
    (web-page all))
  "*Alist in form (KIND . FILTER-SPEC).
To denote for which chats to automatically download media content.
KIND is one of `photos', `videos', `files', `voice-messages',
`video-messages' and `web-page'.
Used by `telega-msg-autodownload-media'."
  :type 'boolean
  :group 'telega)

(defcustom telega-completing-read-function 'ido-completing-read
  "Completing read function to use."
  :type 'function
  :group 'telega)

;; special symbols
(defgroup telega-symbol nil
  "Group to customize special symbols used by telega."
  :group 'telega)

(defcustom telega-symbol-eliding "..."
  "*String used for eliding long string in formats.
Nice looking middle dots can be done by setting
`telega-symbol-eliding' to `(make-string 3 #x00b7)'
or set it to \"\\u2026\" or \"\\u22ef\" to use unicode char for
ellipsis."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-eye "👁"        ;\U0001F441
  "String to use as eye symbol."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-pin "📌"       ;\U0001F4CC
  "*String to use as pin symbol."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-lock "🔒"      ;\U0001F512
  "*String to use as lock symbol."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-document "📄"  ;\U0001F4C4
  "*String to use as document symbol."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-photo "📷"     ;\U0001F4F7
  "*String to use as photo symbol."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-msg-pending "⌛" ;\u231B
  "Symbol to use for pending outgoing messages."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-msg-succeed "✓" ;\u2713
  "Symbol to use for successfully sent messages."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-msg-viewed "✔" ;\u2714
  "Symbol to use for seen outgoing messages."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-msg-failed "⛔" ;\u26D4
  "Mark messages that have sending state failed."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-vertical-bar "| "
  "Symbol used to form vertical lines."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-underline-bar "_"
  "Symbol used to draw underline bar.
\"\uFF3F\" is also good candidate for underline bar."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-draft (propertize "Draft" 'face 'error)
  "Symbol used for draft formatting."
  :type 'string
  :group 'telega-symbol)


;;; Faces
(defgroup telega-faces nil
  "Group to customize faces used by telega."
  :group 'telega)

(defface telega-filter-button-active '((t :inherit default))
  "*Face to use for active custom filters."
  :group 'telega-faces)

(defface telega-filter-button-inactive '((t :inherit shadow))
  "*Face to use for inactive custom filters."
  :group 'telega-faces)

(defface telega-chat-prompt
  '((t (:inherit default :weight bold)))
  "Face for chat input prompt"
  :group 'telega-faces)

(defface telega-unread-unmuted-modeline
  '((t :inherit error))
  "Face used to display number of unread/unmuted messages in modeline."
  :group 'telega-faces)

(defface telega-muted-count
  `((t :inherit shadow))
  "Face to display count of messages in muted chats."
  :group 'telega-faces)

(defface telega-unmuted-count
  '((t :inherit default :foreground "blue"))
  "Face to display count messages in unmuted chats."
  :group 'telega-faces)

(defface telega-mention-count
  '((t :weight bold :foreground "blue"))
  "Face to display count of the mentions."
  :group 'telega-faces)

(defface telega-entity-type-mention
  '((t :inherit default :foreground "blue"))
  "Face to display @mentions."
  :group 'telega-faces)

(defface telega-link
  '((t :inherit link))
  "Face to display various links."
  :group 'telega-faces)

(defface telega-entity-type-hashtag
  '((t :inherit telega-link))
  "Face to display #hashtags."
  :group 'telega-faces)

(defface telega-entity-type-cashtag
  '((t :inherit default :foreground "blue"))
  "Face to display $USD cashtags"
  :group 'telega-faces)

(defface telega-entity-type-botcommand
  '((t :inherit default :foreground "blue"))
  "Face to display /command if there is bot in chat."
  :group 'telega-faces)

(defface telega-entity-type-bold
  '((t :inherit bold))
  "Face to display bold text."
  :group 'telega-faces)

(defface telega-entity-type-italic
  '((t :inherit italic))
  "Face to display italic text."
  :group 'telega-faces)

(defface telega-entity-type-code
  '((t :family "Monospace Serif"))
  "Face to display code.
You can customize its `:height' to fit width of the default face.
Use `(set-face-attribute 'telega-entity-type-code nil :height 0.83333333)'"
  :group 'telega-faces)

(defface telega-entity-type-pre
  '((t :family "Monospace Serif"))
  "Face to display text ala <pre> HTML tag.
You can customize its `:height' to fit width of the default face."
  :group 'telega-faces)

(defface telega-entity-type-texturl
  '((t :inherit button))
  "Face to display urls."
  :group 'telega-faces)

(defface telega-chat-user-title
  '((t :inherit 'widget-field))
  "Face to display user title in chat buffers."
  :group 'telega-faces)

(defface telega-chat-inline-reply
  '((t :inherit 'widget-field))
  "Face to highlight replies to messages."
  :group 'telega-faces)

(defface telega-chat-self-title
  '((t :inherit 'telega-chat-user-title :bold t))
  "Face to display title of myself in chat buffers."
  :group 'telega-faces)

(defface telega-msg-status
  '((t :height 0.8))
  "Face used to display `telega-symbol-msg-XXX' symbols in message."
  :group 'telega-faces)

(defface telega-webpage-sitename
  '((t :inherit shadow))
  "Face to display webpage's site_name."
  :group 'telega-faces)

(defface telega-webpage-title
  '((t :inherit bold))
  "Face to display webpage's title."
  :group 'telega-faces)


(defgroup telega-hooks nil
  "Group to customize hooks used by telega."
  :group 'telega)

(defcustom telega-root-mode-hook nil
  "Hook run when telega root buffer is created."
  :type 'hook
  :group 'telega-hooks)

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

(defcustom telega-user-update-hook
  '(telega-media--autodownload-on-user telega-root--user-update)
  "Hook called with single argument USER, when USER's info is updated."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-chat-before-oldest-msg-hook nil
  "Hook called before oldest chat message is inserted.
Called with single argument - MSG.
Called only if corresponding chat is opened.
Could be used for example to insert date breaks."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-chat-before-youngest-msg-hook nil
  "Hook called before youngest chat message is inserted.
Called with single argument - MSG.
Called only if corresponding chat is opened.
Could be used for example to insert date breaks."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-chat-pre-message-hook '(telega-media--autodownload-on-msg)
  "Hook called uppon new message arrival, before inserting into chatbuffer.
Called with two arguments - message and disable-notification.
Always called, even if corresponding chat is closed at the moment."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-chat-message-hook nil
  "Hook called when new message has been inserted into chatbuffer.
Called with two arguments - message and disable-notification.
Always called, even if corresponding chat is closed at the moment."
  :type 'hook
  :group 'telega-hooks)

(provide 'telega-customize)

;;; telega-customize.el ends here

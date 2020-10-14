;;; telega-customize.el --- Customization for telega  -*- lexical-binding:t -*-

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

(defcustom telega-database-dir telega-directory
  "*Directory for the TDLib's persistent database."
  :type 'string
  :group 'telega)

(defcustom telega-cache-dir (expand-file-name "cache" telega-directory)
  "*Directory for telegram downloads."
  :type 'string
  :group 'telega)

(defcustom telega-temp-dir (expand-file-name "temp" telega-directory)
  "*Directory for temporary files used by telega."
  :type 'string
  :group 'telega)

(defcustom telega-accounts nil
  "*List of the accounts to be used by telega.
Each element is a list in form:
(ACCOUNT-NAME CUSTOM-VAR1 VAL1 CUSTOM-VAR2 VAL2 ...).
At least `telega-database-dir' should be customized for each account."
  :type 'list
  :group 'telega)

(defcustom telega-language "en"
  "*IETF language tag of the user's language."
  :type 'string
  :group 'telega)

(defcustom telega-options-plist
  (list :online t :localization_target "tdesktop")
  "*Plist of options to set.
To use custom language pack (from \"tdesktop\" localization target),
add `:language_pack_id' option.
Only writable options can be set.  See: https://core.telegram.org/tdlib/options"
  :type 'plist
  :group 'telega)

(defcustom telega-debug nil
  "*Non-nil to enable telega debugging buffer."
  :type 'boolean
  :group 'telega)

(defcustom telega-use-test-dc nil
  "*Non-nil to use telegram's test environment instead of production."
  :type 'boolean
  :group 'telega)

(defcustom telega-use-file-database t
  "Non-nil to save downloaded/uploaded files among restarts."
  :type 'boolean
  :group 'telega)

(defcustom telega-use-chat-info-database t
  "Cache chats informations among restarts.
Implies `telega-use-file-database' set to non-nil."
  :type 'boolean
  :group 'telega)

(defcustom telega-use-message-database t
  "Cache chats and messages among restarts.
Implies `telega-use-chat-info-database' set to non-nil."
  :type 'boolean
  :group 'telega)

(defcustom telega-proxies nil
  "*List of proxies.
Format is:
  (:server \"<SERVER>\" :port <PORT> :enable <BOOL> :type <PROXY-TYPE>)

where PROXY-TYPE is one of:
  (:@type \"proxyTypeSocks5\" :username <USER> :password <PASSWORD>)
  (:@type \"proxyTypeHttp\" :username <USER> :password <PASSWORD>
         :http_only <BOOL>)
  (:@type \"proxyTypeMtproto\" :secret <SECRET-STRING>)

<BOOL> is either t or `:false', nil is not valid value."
  :type 'list
  :group 'telega)

(defcustom telega-idle-delay 0.5
  "*Delay before taking actions when Emacs gets idle."
  :type 'number
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

(defcustom telega-help-messages t
  "*Non-nil to show sometime UI related messages."
  :type 'boolean
  :group 'telega)

(defcustom telega-use-short-filenames t
  "*Non-nil to cut /home/user/.telega/cache from filenames."
  :type 'boolean
  :group 'telega)

(defcustom telega-use-tracking-for nil
  "*Specifies Chat Filter for chats to be tracked with tracking.el.
Make sure you have tracking.el loaded if this option is used.
Only chats with corresponding opened chatbuf are tracked."
  :package-version '(telega . "0.5.7")
  :type 'list
  :options '((not (or saved-messages (type channel bot)))
             (or unmuted mention))
  :group 'telega)

(defcustom telega-use-images (or (and (fboundp 'image-transforms-p)
                                      (funcall 'image-transforms-p))
                                 (fboundp 'imagemagick-types))
  "Non-nil to show images."
  :type 'boolean
  :group 'telega
  :set (lambda (option value)
         (if (or (fboundp 'imagemagick-types)
                 (and (fboundp 'image-transforms-p)
                      (funcall 'image-transforms-p)))
             (set-default option value)
           (set-default option nil)
           (when value
             (warn "No ImageMagick support, so images can't be displayed in telega")))))

;; See https://t.me/emacs_telega/12459
(defcustom telega-button-endings #'telega-button--endings-func
  "*Characters to use as beginning/ending of the button.
Set to (\"[\" . \"]\") in nox-emacs setup.
Could be a function of one argument - LABEL, should return cons
cell of endings for the button with LABEL."
  :type '(choice function cons)
  :group 'telega)

(defcustom telega-rainbow-lightness '(35 . 65)
  "Lightness value for colors.
car value is for light scheme, cdr value is for dark scheme."
  :package-version '(telega . "0.6.12")
  :type 'cons
  :group 'telega)

(defcustom telega-rainbow-saturation '(50 . 50)
  "Saturation value for colors.
car value is for light scheme, cdr value is for dark scheme."
  :package-version '(telega . "0.6.12")
  :type 'cons
  :group 'telega)

(defcustom telega-rainbow-color-function #'telega-color-rainbow-identifier
  "Function used to assign color to the users/chats.
Should accept two arguments - IDENTIFIER and BACKGROUND-MODE.
Should return color or nil."
  :package-version '(telega . "0.6.12")
  :type 'function
  :group 'telega)

(defcustom telega-rainbow-color-custom-for
  (list '(saved-messages . nil))
  "List of custom colors for chats.
Each element is cons cell, where car is Chat Filter, and cdr is color."
  :package-version '(telega . "0.6.12")
  :type 'list
  :group 'telega)

;;; ellit-org: inline-bot-options
;; - {{{user-option(telega-known-inline-bots,2)}}}
(defcustom telega-known-inline-bots '("@gif" "@youtube" "@pic")
  "List of known bots for everyday use."
  :type 'list
  :group 'telega)

;;; ellit-org: inline-bot-options
;; - {{{user-option(telega-inline-query-window-select,2)}}}
(defcustom telega-inline-query-window-select t
  "*Non-nil to select window with inline query results."
  :type 'boolean
  :group 'telega)

(defcustom telega-inline-login-url-action 'query-all
  "Action to take on login url keyboard buttons."
  :package-version '(telega . "0.6.30")
  :type '(choice (const :tag "Ask user what to do" 'query-all)
                 (const :tag "Ask user to open url only" 'query-open)
                 (const :tag "Open url without query, but query user for login and write access" 'query-login-and-write-access)
                 (const :tag "Open url without query, but query user for login, without querying for write access" 'query-login-only)
                 (const :tag "Do not login, just open the url" nil))
  :group 'telega)

(defcustom telega-chat--display-buffer-action
  '((display-buffer-reuse-window display-buffer-same-window))
  "Action value when poping to chatbuffer.
See docstring for `display-buffer' for the values."
  :type 'cons
  :group 'telega)

(defcustom telega-emoji-company-backend 'telega-company-emoji
  "Company backend to use for emoji completions."
  :type 'symbol
  :group 'telega
  :options '(telega-company-telegram-emoji))

(defcustom telega-emoji-fuzzy-match t
  "*Non-nil to use fuzzy prefix matching.
For example without fuzzy matches, prefix `:jo' will match only
`:joy:', `:joy-cat:' and `:joystick:'.  With fuzzy matching
enabled it will match also `:flag-jo:' and `:black-jocker:'."
  :type 'boolean
  :group 'telega)

(defcustom telega-emoji-custom-alist nil
  "*Alist of custom emojis to add along with `etc/emojis.alist'."
  :type 'alist
  :group 'telega)

(defcustom telega-emoji-font-family
  (let ((ffl (font-family-list)))
    (or (car (member "Emoji One" ffl))
        (car (member "Noto Color Emoji" ffl))))
  "*Font to use for emoji image generation using `telega-emoji-create-svg'."
  :type 'string
  :group 'telega)

(defcustom telega-emoji-use-images (when telega-emoji-font-family t)
  "*Non-nil to use images for emojis.
Value is ignored if `telega-use-images' is nil."
  :type 'boolean
  :group 'telega)

(defcustom telega-emoji-large-height 2
  "*Vertical size in characters for emoji only messages.
Used only if `telega-emoji-use-images' is non-nil."
  :type 'integer
  :group 'telega)

(defcustom telega-sticker-size '(4 . 24)
  "*Size for the sticker.
car is height in chars to use.
cdr is maximum width in chars to use."
  :type 'cons
  :group 'telega)

(defcustom telega-sticker-favorite-background "cornflower blue"
  "*Background color for the favorite stickers."
  :type 'string
  :group 'telega)

(defcustom telega-sticker-set-download nil
  "*Non-nil to automatically download known sticker sets."
  :type 'boolean
  :group 'telega)

(defcustom telega-sticker-set-show-cover telega-use-images
  "*Non-nil to show sticker set cover when completing stickerset."
  :type 'boolean
  :group 'telega)

(defcustom telega-sticker-set-show-emoji nil
  "*Non-nil to show emoji along with sticker in sticker set help win."
  :type 'boolean
  :group 'telega)

(defcustom telega-animation-height 5
  "*Height of animations in char heights."
  :type 'integer
  :group 'telega)

(defcustom telega-animation-play-inline t
  "*Non-nil to play animation inside telega."
  :type 'boolean
  :group 'telega)

(defcustom telega-animation-download-saved nil
  "*Non-nil to automatically download saved animations."
  :type 'boolean
  :group 'telega)

(defcustom telega-avatar-factors-alist
  '((1 . (0.8 . 0.1))
    (2 . (0.8 . 0.1)))
  "*Alist of size coefficients used in avatar creation.
Each element is in form:
  (CHEIGHT CIRCLE-FACTOR . MARGIN-FACTOR)
See `telega-avatar--create-img' for more info."
  :package-version '(telega . "0.5.8")
  :type 'alist
  :group 'telega)

(defcustom telega-vvnote-waves-height-factor 0.75
  "*Factor for waves svg height.
There is a restriction to its value:
`(* (telega-chars-xheight 1) telega-vvnote-waves-height-factor)' must be > 8."
  :type 'float
  :group 'telega)

(defcustom telega-video-note-height 6
  "*Height in chars for video notes."
  :type 'integer
  :group 'telega)

(defcustom telega-video-note-play-inline t
  "*Non-nil to play video notes inside chatbuffer."
  :type 'boolean
  :group 'telega)

(defcustom telega-video-play-inline nil
  "*Non-nil to play video files inside telega."
  :type 'boolean
  :group 'telega)

(defcustom telega-video-ffplay-args nil
  "*Additional arguments to ffplay to play video messages.
To play in fullscreen, set `telega-video-ffplay-args' to '(\"-fs\")."
  :type 'list
  :group 'telega)

(defcustom telega-open-file-function #'find-file
  "Function to use to open files associated with messages.
Called with single argument - filename to open.
Could be used to open files in external programs.
Set it to `org-open-file' to use Org mode to open files."
  :package-version '(telega . "0.6.31")
  :type 'function
  :options '(org-open-file)
  :group 'telega)

;; Locations
(defcustom telega-location-url-format
  "http://maps.google.com/?q=%N,%E&ll=%N,%E&z=15"
  "*URL format used to open location messages.
%N substituted with lattitude.
%E substituted with longitude."
  :type 'string
  :group 'telega)

(defcustom telega-my-location nil
  "Set to non-nil to use this as location of me.
Plist in form (:latitude <LAT> :longitude <LONG>)
To publically expose this location set `:is_location_visible' to
non-nil in `telega-options-plist'."
  :type 'plist
  :group 'telega)

(defcustom telega-location-size (cons 10 40)
  "*Size for location image in char height/width.
In pixels height and width should be in range [16..1024]."
  :type 'cons
  :group 'telega)

(defcustom telega-location-zoom 15
  "*Zoom for location image.
In range [13..20]"
  :type 'integer
  :group 'telega)

(defcustom telega-location-scale 1
  "*Scale for location image.
In range [1..3]"
  :type 'number
  :group 'telega)

(defcustom telega-poll-result-color "#000066"
  "Color used to draw poll results in poll messages."
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

(defcustom telega-server-libs-prefix "/usr/local"
  "*Prefix where tdlib and tgvoip libraries are installed.
TELEGA-SERVER-LIBS-PREFIX/include is used for headers files.
TELEGA-SERVER-LIBS-PREFIX/lib is used for library files."
  :type 'string
  :group 'telega-server)

(defcustom telega-server-logfile
  (expand-file-name "telega-server.log" telega-directory)
  "*Write server logs to this file.
Set it to nil to disable telega-server logging."
  :type 'string
  :group 'telega-server)

(defcustom telega-server-verbosity (if telega-debug 5 3)
  "*Verbosity level for server process.
Verbosity levels are from 0 (disabled) to 5 (debug)."
  :type 'number
  :group 'telega-server)

(defcustom telega-server-call-timeout 1.0
  "*Timeout for `telega-server--call'."
  :type 'number
  :group 'telega-server)


(defgroup telega-root nil
  "Customization for telega-root-mode"
  :prefix "telega-root-"
  :group 'telega)

(defcustom telega-root-default-view-function 'telega-view-default
  "*Default view for the rootbuf."
  :package-version '(telega . "0.6.23")
  :type 'function
  :group 'telega-root)

(defcustom telega-root-view-ewocs-delim
  (propertize "\n" 'display '((height 0.25)))
  "Delimiter for the root view ewocs."
  :package-version '(telega . "0.6.23")
  :type 'string
  :group 'telega-root)

(defcustom telega-root-view-topics
  '(("Important" . (or mention (and unread unmuted))))
  "Alist of topics for \"topics\" root view.
Car is name of the topic, cdr is chat filter to match chats."
  :package-version '(telega . "0.6.23")
  :type 'alist
  :group 'telega-root)

;;; ellit-org: folders-options
;; - {{{user-option(telega-root-view-topics-folders, 2)}}}
(defcustom telega-root-view-topics-folders 'append
  "*Non-nil to add Chat Folders to the list of topics.
Could be one of `prepend', `append' or nil."
  :package-version '(telega . "0.6.30")
  :type '(choice (const :tag "Prepend folders to the topics" prepend)
                 (const :tag "Append folders to the topics" append)
                 (const :tag "Do not use folders in the topics" nil))
  :group 'telega-root)

(defcustom telega-root-view-topics-other-chats t
  "*Non-nil to show other chats in the \"topics\" root view."
  :package-version '(telega . "0.6.30")
  :type 'boolean
  :group 'telega-root)

(defcustom telega-root-view-top-categories
  '(("Users" . 10)
    ("Groups" . 10)
    ("Channels" . 10)
    ("Bots" . 10)
    ("InlineBots" . 10)
    ("Calls" . 10)
    ("ForwardChats" . 10))
  "List of top categories with limits."
  :package-version '(telega . "0.6.23")
  :type 'alist
  :group 'telega-root)

(defcustom telega-root-keep-cursor 'track
  "*Non-nil to keep cursor at current chat, even if chat's order changes.
Set to `track', to move cursor to corresponding chat button, when
chat buffers are switched, useful in side-by-side window setup
for rootbuf and chatbuf.

Consider setting `switch-to-buffer-preserve-window-point' to nil,
to make `telega-root-keep-cursor' always work as expected."
  :type '(choice (const :tag "Track chat buffers" track)
                 (const :tag "Stick to chat at point" t))
  :group 'telega-root)

(defcustom telega-root-show-avatars telega-use-images
  "*Non-nil to show chat avatars in root buffer."
  :type 'boolean
  :group 'telega-root)

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
  '(((type private)    "{" "}")
    ((type basicgroup) "(" ")")
    ((type supergroup) "[" "]")
    ((type channel)    "<" ">")
    (all               "[" "]"))
  "Brackets to use for chat button.
Each element is in form:
  (<CHAT-FILTER> <OPEN-BRACKET> <CLOSE-BRACKET>)"
  :type 'list
  :group 'telega-root)

(defcustom telega-chat-me-custom-title (lambda (title)
                                         (propertize title 'face 'bold))
  "*Custom title for the chat with myself.
Could be a function accepting title and returning new title.
If nil, then \"saved_messages\" name from `telega-i18n' is used."
  :type '(or string function)
  :group 'telega-root)

(defcustom telega-chat-title-emoji-use-images telega-emoji-use-images
  "*Non-nil to use images for emojis in chat's title.
Otherwise use simple chars."
  :type 'boolean
  :group 'telega-chat)

;;; ellit-org: folders-options
;; - {{{user-option(telega-folder-icons-alist, 2)}}}
(defcustom telega-folder-icons-alist
  (list (cons "Favorite" "★")
        (cons "Love" "♥")
        (cons "Travel" "🛫")
        (cons "Cat" "🐱")
        (cons "Sport" "🏅")
        (cons "Mask" "😷"))
  "Alist of symbols to be used as folder icons instead of `telega-symbol-folder'."
  :type 'alist
  :group 'telega)

;;; ellit-org: folders-options
;; - {{{user-option(telega-chat-folder-format, 2)}}}
(defcustom telega-chat-folder-format (propertize "%F | " 'face 'bold)
  "*Non-nil to prefix chat's title with chat folder.
%i - Replaced with folder's icon from `telega-folder-icon-names' or
     `telega-symbol-folder'.
%f - Replaced with folder's title.
%F - Replaced with folder's icon from `telega-folder-icon-names'
     if icon is unique, or equivalent to %i%f."
  :package-version '(telega . "0.6.30")
  :type 'string
  :group 'telega-root)

;;; ellit-org: folders-options
;; - {{{user-option(telega-chat-folders-exclude, 2)}}}
(defcustom telega-chat-folders-exclude (list "Unread" "Personal")
  "Exclude these folders when determining chat's folder.
When determining which chat folder to use in
`telega-chat-folders-format', these folders are excluded, if
single folder is left, then it is used in the formatting."
  :package-version '(telega . "0.6.30")
  :type 'list
  :group 'telega-root)

(defcustom telega-status-animate-interval 0.5
  "Dots animation interval for telega status shown in root buffer."
  :type 'number
  :group 'telega-root)

(defcustom telega-online-status-function (if (or (fboundp 'frame-focus-state)
                                                 (display-graphic-p))
                                             'telega-focus-state
                                           'telega-buffer-p)
  "Function used to determine if user is online.
Function should return non-nil if user is online, and nil if offline.
See https://github.com/zevlg/telega.el/issues/171"
  :package-version '(telega . "0.6.14")
  :options '(telega-buffer-p)
  :type 'function
  :group 'telega)

(defcustom telega-offline-status-interval 3
  "Interval in seconds before going offline when emacs looses focus."
  :type 'number
  :group 'telega)

(defcustom telega-online-status-interval 0.1
  "Interval in seconds before going online when emacs gets focus."
  :type 'number
  :group 'telega)


(defgroup telega-filter nil
  "Customize chats filtration."
  :prefix "telega-filter-"
  :group 'telega)

(defcustom telega-filter-default 'main
  "*Default chat filter to apply."
  :type 'list
  :options '(all (or saved-messages pin unread))
  :group 'telega-filter)

(defcustom telega-filters-custom
  '(("📑Main" . main)
    ("Groups" . (type basicgroup supergroup))
    ("Channels" . (type channel))
    ("Online" . (and (not saved-messages) (online-status "Online")))
    ("Important" . (or mention (and unread unmuted)))
    ("📑Archive" . archive))
  "*Alist of custom filters in form (NAME . CHAT-FILTER).
This filters are displayed as filter buttons at the top of rootbuf.
TODO: If NAME starts with \"lng_\" then `telega-i18n' is used."
  :package-version '(telega . "0.6.23")
  :type 'alist
  :group 'telega-filter)

(defcustom telega-filter-custom-expand t
  "*Non-nil to expand custom filter when adding to active filters."
  :type 'boolean
  :group 'telega-filter)

(defcustom telega-filter-custom-folders
  (list (cons "📑Main" (list :@type "chatListMain"))
        (cons "📑Archive" (list :@type "chatListArchive")))
  "*Alist of custom folders.
Where car is custom filter name, and cdr is tdlib chat list for this filter."
  :package-version '(telega . "0.6.24")
  :type 'alist
  :group 'telega-filter)

;;; ellit-org: folders-options
;; - {{{user-option(telega-filter-custom-show-folders, 2)}}}
(defcustom telega-filter-custom-show-folders t
  "Non-nil to show telegram folders along with custom filters."
  :package-version '(telega . "0.6.24")
  :type 'boolean
  :group 'telega-filter)

(defcustom telega-filter-button-width 20
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
  :options '(telega-ins--chat-full-2lines)
  :group 'telega-inserters)

(defcustom telega-inserter-for-nearby-chat-button 'telega-ins--chat-nearby-2lines
  "Inserter for nearby chat button in rootbuf."
  :package-version '(telega . "0.6.23")
  :type 'function
  :group 'telega-inserters)

(defcustom telega-inserter-for-msg-button 'telega-ins--message
  "Inserter for message button in chat buffer.
Accepts at least two arguments - MSG and NO-HEADER-P.
See `telega-ins--message' for NO-HEADER argument."
  :type 'function
  :group 'telega-inserter)

(defcustom telega-inserter-for-msg-notification 'telega-ins--msg-notification
  "*Inserter used to form body for notification bubble."
  :type 'function
  :group 'telega-inserter)

(defcustom telega-inserter-for-root-contact-button 'telega-ins--root-contact-2lines
  "*Inserter for buttons in CONTACTS ewoc in rootbuf."
  :package-version '(telega . "0.6.23")
  :type 'function
  :options '(telega-ins--root-contact)
  :group 'telega-inserter)


(defgroup telega-webpage nil
  "Customization for instant view webpage rendering."
  :group 'telega)

(defcustom telega-webpage-fill-column fill-column
  "*Fill column to use for webpage rendering."
  :type 'integer
  :group 'telega-webpage)

(defcustom telega-webpage-history-max 100
  "*Maximum number of viewed webpages to remember in history."
  :type 'number
  :group 'telega-webpage)

(defcustom telega-webpage-header-line-format
  '(" " (:eval (concat telega-webpage--sitename
                       (and telega-webpage--sitename ": ")
                       telega-webpage--url
                       "  "
                       (format "History: %d/%d"
                               (1+ telega-webpage-history--index)
                               (length telega-webpage-history))
                       )))
  "Header line format for instant webpage."
  :type 'list
  :group 'telega-webpage)

(defcustom telega-webpage-photo-size-limits
  (list (/ telega-webpage-fill-column 2) 10
        telega-webpage-fill-column 20)
  "Same as `telega-photo-size-limits' but for photos in Instant View."
  :package-version '(telega . "0.6.30")
  :type '(list integer integer integer integer)
  :group 'telega-webpage)


(defgroup telega-user nil
  "Customization for users."
  :group 'telega)

(defcustom telega-user-show-avatars telega-use-images
  "Non-nil to show avatars for the users."
  :type 'boolean
  :group 'telega-user)

(defcustom telega-user-show-relationship t
  "*Non-nil to show user relationship with me.
Used when showing chat members list."
  :type 'boolean
  :group 'telega-user)

(defcustom telega-user-photo-size 10
  "*Show profile photos of this number of characters in width and height."
  :package-version '(telega . "0.6.30")
  :type 'integer
  :group 'telega-user)


(defgroup telega-chat nil
  "Customization for chat buffer."
  :group 'telega)

(defcustom telega-chat-show-avatars telega-use-images
  "*Non-nil to show user avatars in chat buffer."
  :type 'boolean
  :group 'telega-chat)

(defcustom telega-chat-input-prompt ">>> "
  "*Chatbuf input prompt."
  :type 'string
  :group 'telega-chat)

(defcustom telega-chat-reply-prompt telega-chat-input-prompt
  "*Chatbuf input prompt when replying to a message."
  :type 'string
  :group 'telega-chat)

(defcustom telega-chat-edit-prompt telega-chat-input-prompt
  "*Chatbuf input prompt when editing a message."
  :type 'string
  :group 'telega)

(defcustom telega-chat-prompt-show-avatar-for
  (when telega-use-images
    '(and has-avatar
          (permission :can_send_messages)))
  "*Show chat avatar nearby prompt input for chats matching this Chat Filter."
  :type 'list
  :group 'telega-chat)

(defcustom telega-chat-scroll-scroll-conservatively 101
  "Value for `scroll-conservatively' in chatbuf."
  :package-version '(telega . "0.6.12")
  :type 'int
  :group 'telega-chat)

(defcustom telega-chat-messages-ring-size 50
  "*Size of the chat massages ring."
  :package-version '(telega . "0.6.22")
  :type 'integer
  :group 'telega-chat)
  
(defcustom telega-chat-input-ring-size 50
  "*Size of the chat input history."
  :type 'integer
  :group 'telega-chat)

(defcustom telega-chat-fill-column fill-column
  "*Column to fill chat messages to."
  :type 'integer
  :group 'telega-chat)

(defcustom telega-chat-history-limit 20
  "Number of messages to fetch on history requests."
  :type 'integer
  :group 'telega-chat)

(defcustom telega-chat-insert-date-breaks t
  "*Non-nil to insert breaks inbetween messages of different days.
NOT YET IMPLEMENTED"
  :type 'boolean
  :group 'telega-chat)

(defcustom telega-chat-use-markdown-version nil
  "*Non-nil to use markdown formatting for outgoing messages.
Used as default value for MARKDOWN-VERSION in
`telega-chatbuf-input-send' command.

Supported versions are: 0, 1 and 2
Supported Markup:
  1) *bold text*
  2) _italic text_
  2.1) __underline text__    (only for v2)
  2.2) ~strike through text~ (only for v2)
  3) `inlined code`
  4) ```<language-name-not-displayed>
     first line of multiline preformatted code
     second line
     last line```
  5) [link text](http://actual.url)
  6) [username](tg://user?id=<USER-ID>)
"
  :package-version '(telega . "0.5.6")
  :type '(choice (const :tag "Disable markdown" nil)
                 (const :tag "Markdown v0" 0)
                 (const :tag "Markdown v1" 1)
                 (const :tag "Markdown v2" 2))
  :group 'telega-chat)

(declare-function telega-chat-private-p "telega-chat" (chat &optional include-bots-p))

(defcustom telega-chat-attach-commands
  '(("photo" nil telega-chatbuf-attach-photo)
    ("self-destruct-photo" (lambda ()
                             (telega-chat-private-p telega-chatbuf--chat))
     telega-chatbuf-attach-ttl-photo)
    ("video" nil telega-chatbuf-attach-video)
    ("self-destruct-video" (lambda ()
                             (telega-chat-private-p telega-chatbuf--chat))
     telega-chatbuf-attach-ttl-video)
    ("note-video" nil telega-chatbuf-attach-note-video)
    ("audio" nil telega-chatbuf-attach-audio)
    ("note-voice" nil telega-chatbuf-attach-note-voice)
    ("file" nil telega-chatbuf-attach-file)
    ("gif" nil telega-chatbuf-attach-gif)
    ("location" nil telega-chatbuf-attach-location)
    ("poll" (lambda ()
              (not (telega-chat-private-p telega-chatbuf--chat)))
     telega-chatbuf-attach-poll)
    ("contact" nil telega-chatbuf-attach-contact)
    ("member" nil telega-chatbuf-attach-member)
    ("sticker" nil telega-chatbuf-attach-sticker)
    ("animation" nil telega-chatbuf-attach-animation)
    ("dice" nil telega-chatbuf-attach-dice)
    ("screenshot" (lambda () telega-screenshot-function)
     telega-chatbuf-attach-screenshot)
    ("clipboard" (lambda ()
                   ;; Avoid "Selection owner couldn't convert" error
                   (ignore-errors
                     (gui-get-selection 'CLIPBOARD 'image/png)))
     telega-chatbuf-attach-clipboard)
    ("markup" nil
     telega-chatbuf-attach-markup)

    ;; Special attachment types affecting how message is sent
    ("scheduled" nil telega-chatbuf-attach-scheduled)
    ("disable-notification"
     (lambda ()
       (not (plist-get telega-chatbuf--chat :default_disable_notification)))
     telega-chatbuf-attach-toggle-disable-notification)
    ("enable-notification"
     (lambda ()
       (plist-get telega-chatbuf--chat :default_disable_notification))
     telega-chatbuf-attach-toggle-disable-notification)
    ("disable-webpage-preview" nil
     telega-chatbuf-attach-disable-webpage-preview)
    )
  "*List of the attachments available for `C-c C-a' in chatbuf.
Each element is a list of three elements:
  \\(ATTACH-NAME AVAILABILITY-FUNC COMMAND-FUNC\\)

AVAILABILITY-FUNC nil, if COMMAND-FUNC is always available,
otherwise function returning non-nil if COMMAND is available.
COMMAND-FUNC - Command function to execute."
  :type 'list
  :group 'telega-chat)

(defcustom telega-chat-upload-attaches-ahead t
  "*Non-nil to upload attachments ahead, before message actually sent.
Having this non-nil \"speedups\" uploading, its like files uploads instantly."
  :type 'boolean
  :group 'telega-chat)

(defcustom telega-chat-markup-functions
  '(("markdown1" . telega-markup-markdown1-fmt)
    ("markdown2" . telega-markup-markdown2-fmt)
    ("html" . telega-markup-html-fmt)
    ("org" . telega-markup-org-fmt))
  "List of markups to use on `C-c C-a markup RET'."
  :type 'list
  :group 'telega-chat)

;; See https://t.me/emacs_telega/11981
(defcustom telega-chat-group-messages-timespan 120
  "*Maximum timespan between two messages in order to group them.
If difference between message's dates is greater than this
timespan, then do not group messages."
  :type 'integer
  :group 'telega-chat)

(defcustom telega-chat-group-messages-for
  '(not (or saved-messages (type channel bot)))
  "*Chat Filter for chats where to group messages by sender."
  :type 'list
  :group 'telega-chat)

(defcustom telega-chat-show-deleted-messages-for nil
  "*Chat Filter for chats where to show deleted messages in chatbuf."
  :type 'list
  :options '((not saved-messages))
  :group 'telega-chat)

(defcustom telega-chat-mode-line-format
  '((:eval (telega-chatbuf-mode-line-discuss))
    (:eval (telega-chatbuf-mode-line-unread))
    (:eval (telega-chatbuf-mode-line-marked))
    (:eval (telega-chatbuf-mode-line-members 'use-icons))
    (:eval (telega-chatbuf-mode-line-pinned-msg 20))
    (:eval (telega-chatbuf-mode-line-messages-filter)))
  "Additional mode line format for chat buffer identification.
See `mode-line-buffer-identification'."
  :package-version '(telega . "0.6.29")
  :type 'sexp
  :group 'telega-chat)

(defcustom telega-chat-completing-sort-criteria
  '(chatbuf-visibility chatbuf-recency)
  "Criteria to sort chats in `telega-completing-read-chat'."
  :package-version '(telega . "0.6.30")
  :type 'symbol
  :group 'telega-chat)

(defcustom telega-chat-switch-buffer-sort-criteria 'chatbuf-recency
  "Criteria to sort open chats when switching with `telega-switch-buffer'."
  :type 'symbol
  :group 'telega-chat)

(defcustom telega-chat-delete-moves-to-archive t
  "*Non-nil to move chat to Archive on deletion.
NOT YET USED."
  :type 'boolean
  :group 'telega-chat)

(defcustom telega-chat-input-complete-function nil
  "*Custom function to be called before any other on TAB in chatbuf input.
Should return non-nil if completion occured."
  :package-version '(telega . "0.6.30")
  :type 'function
  :options '(counsel-company)
  :group 'telega-chat)
  

;; VoIP
(defgroup telega-voip nil
  "VOIP settings."
  :group 'telega)

(defcustom telega-voip-logfile
  (expand-file-name "telega-voip.log" telega-directory)
  "*Write VoIP logs to this file.
Set it to nil to disable VoIP logging."
  :type 'string
  :group 'telega-voip)

(defcustom telega-voip-allow-p2p nil
  "*Non-nil to allow P2P connections for calls."
  :type 'boolean
  :group 'telega-voip)

(defcustom telega-voip-busy-if-active t
  "*Reply with busy status to any incoming calls if have active call."
  :type 'boolean
  :group 'telega-voip)

(defcustom telega-voip-help-echo t
  "*Non-nil to show help messages in echo area on call activation."
  :type 'boolean
  :group 'telega-voip)

(defcustom telega-voip-use-sounds nil
  "*Non-nil to play sounds (using ffplay) for call status changes."
  :type 'boolean
  :group 'telega-voip)


;; Notifications
(defgroup telega-notifications nil
  "Setup for D-Bus notifications."
  :group 'telega)

(defcustom telega-notifications-delay 0.5
  "*Delay in seconds for notifications.
This delay is taken before making decision show or not the
message in notification.
Taking pause before showing notification is wise, because another
telegram may be active with the chat opened, you don't want the
notification to be shown for already read message.
Set it to 0, for no delay notifications."
  :type 'float
  :group 'telega-notifications)

(defcustom telega-notifications-timeout 4.0
  "*How long to show notification in seconds."
  :type 'float
  :group 'telega-notifications)

;; NOTE: standard values for :sound-name
;; http://0pointer.de/public/sound-naming-spec.html
(defcustom telega-notifications-msg-args
  (list :sound-name "message-new-instant")
  "*Additional arguments to `notifications-notify' on chat messages."
  :type 'list
  :group 'telega-notifications)

(defcustom telega-notifications-call-args
  (list :sound-name "phone-incoming-call")
  "*Additional arguments to `notifications-notify' on incoming calls."
  :type 'list
  :group 'telega-notifications)

;; See https://github.com/zevlg/telega.el/issues/32
(defcustom telega-notifications-msg-body-limit 100
  "*Limit for the message body length.
Used by `telega-ins--msg-notification'."
  :type 'integer
  :group 'telega-notifications)

(defcustom telega-notifications-defaults nil
  "Cons cell with notifications settings.
car for private/secret chats
cdr for any groups including channels
For example:
  (cons
    (list :mute_for 0 :show_preview t)
    (list :mute_for 599695961 :show_preview t))"
  :type 'cons
  :group 'telega-notifications)


(defgroup telega-msg nil
  "Customization for telega messages formatting."
  :prefix "telega-msg-"
  :group 'telega)

(defcustom telega-msg-rainbow-title t
  "*Non-nil to display user names in chatbuf with their assigned color."
  :type 'boolean
  :group 'telega-msg)

(defcustom telega-msg-heading-whole-line nil
  "*Non-nil to spread `telega-msg-heading' face to full line width.
Also applies to `telega-msg-inline-reply' face."
  :type 'boolean
  :group 'telega-msg)

(defcustom telega-msg-use-images-in-one-line telega-use-images
  "Non-nil to show images in one-line message version."
  :package-version '(telega . "0.6.30")
  :type 'boolean
  :group 'telega-msg)

(defcustom telega-photo-show-details t
  "*Non-nil to show photo details for photo messages."
  :type 'boolean
  :group 'telega-msg)

(defcustom telega-photo-size-limits '(10 6 55 12)
  "*Limits image size for the photos.
Limits to (MIN-WIDTH MIN-HEIGHT MAX-WIDTH MAX-HEIGHT) characters."
  :package-version '(telega . "0.6.30")
  :type '(list integer integer integer integer)
  :group 'telega)

(defcustom telega-thumbnail-size-limits telega-photo-size-limits
  "*Same as `telega-photo-size-limits', but for thumbnails.
Used for such messages as audio/document/etc."
  :package-version '(telega . "0.6.30")
  :type '(list integer integer integer integer)
  :group 'telega)

(defcustom telega-auto-download
  '((photo all)
    (video opened)
    (file opened)
    (voice-message opened)
    (video-message opened)
    (web-page opened)
    (instant-view opened))
  "*Alist in form (KIND . FILTER-SPEC).
To denote for which chats to automatically download media content.
KIND is one of `photo', `video', `file', `voice-message',
`video-message', `web-page', `instant-view'.
NOT USED"
  :type 'boolean
  :group 'telega)

(defcustom telega-ignored-messages-visible nil
  "*Non-nil to make ignored messages visible as <IGNORED MESSAGE>."
  :package-version '(telega . "0.6.30")
  :type 'boolean
  :group 'telega-chat)

(defcustom telega-ignored-messages-ring-size 100
  "*Maximum number of ignored messages in `telega--ignored-messages-ring'.
Message is ignored if its `:ignore' option is set to non-nil."
  :type 'number
  :group 'telega-chat)

(defcustom telega-completing-read-function 'ido-completing-read
  "Completing read function to use."
  :type 'function
  :group 'telega)

(defcustom telega-screenshot-function
  (cond ((executable-find "flameshot")
         'telega-screenshot-with-flameshot)
        ((executable-find "screencapture")
         'telega-screenshot-with-screencapture)
        ((executable-find "pngpaste")
         'telega-screenshot-with-pngpaste)
        ((executable-find "gnome-screenshot")
         'telega-screenshot-with-gnome-screenshot)
        ((executable-find "import")
         'telega-screenshot-with-import))
  "*Function to use to make screenshot.
Function should take two arguments - TOFILE and REGION-P."
  :type 'function
  :group 'telega)

;; special symbols
(defgroup telega-symbol nil
  "Group to customize special symbols used by telega."
  :group 'telega)

(defcustom telega-symbol-telegram (propertize "◀" 'face '(italic telega-blue))
  "*String used as telegram logo."
  :type 'string
  :group 'telega-symbol)

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

(defcustom telega-symbol-custom-order (cons "↓" "↑")
  "Symbols used to emphasize custom order for the chat.
car is used if custom order is less then real chat's order.
cdr is used if custom order is greater then real chat's order."
  :type 'cons
  :group 'telega-symbol)

(defcustom telega-symbol-lock "🔒"      ;\U0001F512
  "*String to use as lock symbol."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-flames "🔥"
  "*Symbol used in self-destruct photos/videos."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-attachment "📎"  ;\U0001F4CE
  "*String to use as attachment symbol.
\"📄\" is also good candidate."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-photo "📷"     ;\U0001F4F7
  "*String to use as photo symbol."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-audio "🎶"     ;\U0001F3B6
  "*String to use as audio symbol."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-video "📹"     ;\U0001F4F9
  "*String to use as video symbol."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-game "🎮"      ;\U0001F3AE
  "*String to use as video game symbol."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-pending "⌛"   ;\u231B
  "Symbol to use for pending outgoing messages."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-checkmark "✓" ;\u2713
  "Symbol for simple check mark."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-heavy-checkmark "✔" ;\u2714
  "Symbol for heavy check mark."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-failed (propertize "⛔" 'face 'error)
  "Mark messages that have sending state failed."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-vertical-bar "|\u00A0"
  "Symbol used to form vertical lines."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-underline-bar "_"
  "Symbol used to draw underline bar.
\"\uFF3F\" is also good candidate for underline bar."
  :type 'string
  :options (list "⎼"
                 (propertize " " 'face 'underline))
  :group 'telega-symbol)

(defcustom telega-symbol-underline-bar-partial "."
  "Symbol used to draw underline bar in chatbuf with partial history."
  :type 'string
  :options '("🢑" "🢓")
  :group 'telega-symbol)

(defcustom telega-symbol-draft (propertize "Draft" 'face 'error)
  "Symbol used for draft formatting."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-unread "●"
  "Symbol used for chats marked as unread.
Good candidates also are 🄌 or ⬤."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-verified (propertize "🅥" 'face 'telega-blue)
  "Symbol used to emphasize verified users/groups."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-star (propertize "★" 'face 'error)
  "Symbol used to emphasize stared chats."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-thunder "🗲"
  "Symbol used inside INSTANT VIEW buttons."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-location "🌐"
  "Symbol used for location."
  :type 'string
  :options '("🌍")
  :group 'telega-symbol)

(defcustom telega-symbol-phone "📞"
  "Symbol used as phone."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-square "■"
  "Symbol used for large squares."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-ballout-empty "☐"
  "Symbol used for empty ballout."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-ballout-check "☑"
  "Symbol used for checked ballout."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-contact "🚹"
  "Symbol used for contacts."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-play "▶"
  "Symbol used for playing."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-pause "⏸"
  "Symbol used for pause."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-invoice "🛒"
  "Symbol used in invoice messages."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-poll "📊"
  "Symbol used in poll messages."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-poll-options (list "○" "●")
  "Symbols used to display poll options with single choice.
First - for non-selected option.
Second - for selected option."
  :type 'list
  :group 'telega-symbol)

(defcustom telega-symbol-poll-multiple-options (list "☐" "☑")
  "Symbols used to display poll options with multiple answers allowed.
First - for non-selected option.
Second - for selected option."
  :type 'list
  :group 'telega-symbol)

(defcustom telega-symbol-quiz-options (list (compose-chars ?○ ?✓)
                                            "○"
                                            (compose-chars ?○ ?✗))
  "Symbols used to display quiz options.
First - for correct option.
Second - for non-selected incorrect option.
Third - for selected incorrect option."
  :type 'list
  :group 'telega-symbol)

(defcustom telega-symbol-attach-brackets (cons "⟬" "⟭")
  "Symbols used to emphasize attachment in chat buffer input."
  :type 'cons
  :group 'telega-symbol)

(defcustom telega-symbol-webpage-details (cons "▼" "▲")
  "Symbols used to display `pageBlockDetails' webpage block."
  :type 'cons
  :group 'telega-symbol)

(defcustom telega-symbol-online-status (propertize "*" 'face 'success)
  "Symbol used to display user's online status in root buffer.
If nil, then user's online status is not displayed."
  :type 'string-or-null-p
  :group 'telega-symbol)

(defcustom telega-symbol-blocked (propertize "⛒" 'face 'error)
  "Symbol used to mark blacklisted users."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-inline "⮍"
  "Symbol used to mark attachments with inline result from bot."
  :type 'string
  :group 'telega-symbol)

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'telega-mark
    (vector #b11111111) nil nil '(top periodic)))

(defcustom telega-symbol-mark (if (fboundp 'define-fringe-bitmap)
                                  (propertize "*" 'display  '(left-fringe telega-mark error))
                                (propertize "*" 'face 'error))
  "*Symbol used to denote marked messages/chats."
  :type 'string
  :group 'telega-symbol)

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'telega-mention
    (vector #b01111110
            #b01111110
            #b11000011
            #b11000011
            #b11001111
            #b10011111
            #b10110011
            #b10110011
            #b10011111
            #b11001110
            #b11000000
            #b11000001
            #b01111111
            #b01111110)))

(defcustom telega-symbol-mention-mark
  (if (fboundp 'define-fringe-bitmap)
      (propertize "\u200B" 'display
                  '(left-fringe telega-mention telega-mention-count))
    (propertize "\u200B" 'face 'telega-mention-count))
  "*Symbol used to denote massages which contains unread mention."
  :package-version '(telega . "0.6.30")
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-alarm "⏲"
  "*Symbol used for scheduled messages."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-dice-list (list "🎲" "⚀" "⚁" "⚂" "⚃" "⚄" "⚅")
  "List of dices to show for \"messageDice\"."
  :type 'list
  :group 'telega-symbol)

(defcustom telega-symbol-folder "🖿"
  "Symbol used for Telegram folders."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-linked "⭾"
  "Symbol used for linked chats button in modeline."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-keyboard "🖮"
  "Symbol used to display reply markup keyboard."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-widths
  (list
   (list 1
         telega-symbol-contact)
   (list 2
         telega-symbol-telegram
         telega-symbol-eye
         telega-symbol-unread
         telega-symbol-verified
         telega-symbol-thunder

         telega-symbol-checkmark
         telega-symbol-heavy-checkmark
         telega-symbol-ballout-empty
         telega-symbol-ballout-check

         telega-symbol-play
         (nth 0 telega-symbol-poll-options)
         (nth 1 telega-symbol-poll-options)
         telega-symbol-blocked
         telega-symbol-alarm
         telega-symbol-folder
         telega-symbol-keyboard
         ))
  "*Custom widths for some symbols, used for correct formatting.
Use `telega-symbol-set-width' to install symbol's width.
Install all symbol widths inside `telega-load-hook'."
  :type 'list
  :group 'telega-symbol)


;;; Faces
(defgroup telega-faces nil
  "Group to customize faces used by telega."
  :group 'telega)

(defface telega-link
  '((t :inherit link :underline nil))
  "Face to display various links."
  :group 'telega-faces)

(defface telega-button
  '((((class color) (min-colors 88) (background light))
     :foreground "RoyalBlue3"
     :box (:line-width -2 :color "RoyalBlue3" :style nil))
    (((class color) (min-colors 88) (background dark))
     :foreground "cyan1"
     :box (:line-width -2 :color "cyan1" :style nil))
    (t :inherit highlight))
  "Face used for telega buttons."
  :group 'telega-faces)

(defface telega-button-active
  '((((class color) (min-colors 88) (background light))
     :inherit telega-button
     :foreground "white" :background "RoyalBlue3")
    (((class color) (min-colors 88) (background dark))
     :foreground "white" :background "cyan1"
     :inherit telega-button)
    (t :inherit telega-button))
  "Face used for active (cursor inside) telega buttons."
  :group 'telega-faces)

(defface telega-box
  '((((class color) (min-colors 88) (background light))
     :inherit default
     :box (:line-width -2 :color "gray30" :style nil))
    (((class color) (min-colors 88) (background dark))
     :inherit default
     :box (:line-width -2 :color "gray70" :style nil)))
  "Face used to outline boxes."
  :group 'telega-faces)

(defface telega-blue
  '((t :foreground "#2ca5e0"))
  "*Official blue color of telegram."
  :group 'telega-faces)

(defface telega-enckey-00
  '((t :foreground "#ffffff" :background "#ffffff"))
  "Face used for encryption key"
  :group 'telega-faces)

(defface telega-enckey-01
  '((t :foreground "#d5e6f3" :background "#d5e6f3"))
  "Face used for encryption key"
  :group 'telega-faces)

(defface telega-enckey-10
  '((t :foreground "#2d5775" :background "#2d5775"))
  "Face used for encryption key"
  :group 'telega-faces)

(defface telega-enckey-11
  '((t :foreground "#2f99c9" :background "#2f99c9"))
  "Face used for encryption key"
  :group 'telega-faces)

(defface telega-filter-button-active
  '((t :inherit default))
  "*Face to use for active custom filters."
  :group 'telega-faces)

(defface telega-filter-button-inactive
  '((t :inherit shadow))
  "*Face to use for inactive custom filters."
  :group 'telega-faces)

(defface telega-filter-active
  '((t :inherit bold))
  "Face to emphasize active chat filter other then `telega-filter-default'."
  :group 'telega-faces)

(defface telega-root-heading
  '((((background light))
     :background "light gray" :foreground "dim gray")
    (((background dark))
     :background "dim gray" :foreground "light gray"))
  "Face used to display headings, such as GLOBAL SEARCH, in root buffer."
  :group 'telega-faces)

(defface telega-chat-prompt
  '((t :weight bold))
  "Face for chat input prompt"
  :group 'telega-faces)

(defface telega-chat-input-attachment
  '((t :inherit bold))
  "Face for chat input attachments."
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
  '((((class color) (background dark))
     :foreground "RoyalBlue1")
    (((class color) (background light))
     :foreground "blue"))
  "Face to display count messages in unmuted chats."
  :group 'telega-faces)

(defface telega-mention-count
  '((t :inherit telega-unmuted-count :weight bold))
  "Face to display count of the mentions."
  :group 'telega-faces)

(defface telega-username
  '((((class color) (background dark))
     :foreground "DodgerBlue")
    (((class color) (background light))
     :foreground "RoyalBlue")
    (t :bold t))
  "Face to display username for chats/users."
  :group 'telega-faces)

(defface telega-entity-type-mention
  '((t :inherit telega-username))
  "Face to display @mentions."
  :group 'telega-faces)

(defface telega-entity-type-hashtag
  '((t :inherit telega-link))
  "Face to display #hashtags."
  :group 'telega-faces)

(defface telega-entity-type-cashtag
  '((t :inherit telega-link))
  "Face to display $USD cashtags"
  :group 'telega-faces)

(defface telega-entity-type-botcommand
  '((t :inherit telega-link))
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

(defface telega-entity-type-underline
  '((t :inherit underline))
  "Face to display underline text."
  :group 'telega-faces)

(defface telega-entity-type-strikethrough
  '((t :strike-through t))
  "Face to display strikethrough text."
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

(defface telega-secret-title
  '((t :foreground "#00b12c"))
  "Face to display title of secret chat in root buffer."
  :group 'telega-faces)

(defface telega-msg-heading
  '((((class color) (background light))
     :background "gray85")
    (((class color) (background dark))
     :background "gray25")
    (t :inherit widget-field))
  "Face to display messages header."
  :group 'telega-faces)

(defface telega-msg-self-title
  '((t :bold t))
  "Face to display title of myself in chat buffers."
  :group 'telega-faces)

(defface telega-msg-user-title
  '((t nil))
  "Face to display user title in chat buffers."
  :group 'telega-faces)

(defface telega-msg-inline-reply
  '((t :inherit 'telega-msg-heading))
  "Face to highlight replies to messages."
  :group 'telega-faces)

(defface telega-msg-inline-forward
  '((t :inherit 'telega-msg-heading))
  "Face to highlight message forwarding header."
  :group 'telega-faces)

(defface telega-msg-outgoing-status
  '((t :height 0.8))
  "Face used to display message outgoing status symbol."
  :group 'telega-faces)

(defface telega-webpage-chat-link
  '((t :background "gray85"))
  "Face to display `pageBlockChatLink' web page blocks"
  :group 'telega-faces)

(defface telega-webpage-sitename
  '((t :inherit telega-link :bold t))
  "Face to display webpage's site_name."
  :group 'telega-faces)

(defface telega-webpage-title
  '((t :inherit bold))
  "Face to display webpage's title."
  :group 'telega-faces)

(defface telega-webpage-strike-through
  '((t :strike-through t))
  "Face to display strike through RichText."
  :group 'telega-faces)

(defface telega-webpage-header
  '((t :family "FreeSerif" :height 1.2))
  "Face to display header in webpage instant view."
  :group 'telega-faces)

(defface telega-webpage-subheader
  '((t :inherit telega-webpage-header :height 1.1))
  "Face to display subheader in webpage instant view."
  :group 'telega-faces)

(defface telega-webpage-fixed
  '((t :family "Monospace Serif" :height 0.85))
  "Face to display fixed text in webpage instant view."
  :group 'telega-faces)

(defface telega-webpage-preformatted
  '((t :inherit telega-webpage-fixed :background "gray85"))
  "Face to display preformatted text in webpage instant view."
  :group 'telega-faces)

(defface telega-user-online-status
  '((((class color) (background dark))
     :foreground "green")
    (((class color) (background light))
     :foreground "cornflower blue")
    (t :bold t))
  "Face to display user status if online."
  :group 'telega-faces)

(defface telega-user-non-online-status
  '((t :inherit shadow))
  "Face to display user status if non-online."
  :group 'telega-faces)

(defface telega-delim-face
  '((t :inherit shadow :height 0.5))
  "Face used to display horizontal delimiters."
  :group 'telega-faces)

(defface telega-button-highlight
  '((t :inherit highlight))
  "Face used to highlight active button."
  :group 'telega-faces)


(defgroup telega-hooks nil
  "Group to customize hooks used by telega."
  :group 'telega)

(defcustom telega-load-hook nil
  "Hooks run when telega is loaded."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-root-mode-hook nil
  "Hook run when telega root buffer is created."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-root-update-hook nil
  "Hook to run on rootbuf updateds.
This hook is run with rootbuf being current."
  :package-version '(telega . "0.6.23")
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-ready-hook nil
  "Hook called when telega is ready to process queries."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-kill-hook nil
  "Hook called when telega exited."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-connection-state-hook nil
  "Hook called when connection state changes."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-chats-fetched-hook nil
  "Hook called when all chats list has been fetched."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-chat-update-hook nil
  "Hook called with single argument CHAT, when CHAT updates."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-user-update-hook nil
  "Hook called with single argument USER, when USER's info is updated."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-chat-created-hook nil
  "Hook called when new chat has been loaded/created.
Called with one argument - chat."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-chat-insert-message-hook nil
  "Hook called before inserting message into chatbuf.
Called with single argument - MESSAGE.
This hook can be used to ignore message, see
https://github.com/zevlg/telega.el#configuring-client-side-messages-filtering."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-chat-pre-message-hook nil
  "Hook called uppon new message arrival, before inserting into chatbuffer.
Called with single argument - MESSAGE.
Always called, even if corresponding chat is closed at the moment."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-chat-post-message-hook nil
  "Hook called when new message has been inserted into chatbuffer.
Called with single argument - MESSAGE.
Always called, even if corresponding chat is closed at the moment.
Called even for messages ignored by client side filtering, to
check message is filtered by client side filtering use
`telega-msg-ignored-p'."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-call-incoming-hook nil
  "Hook called when incoming call pending.
Called with single argument - incoming call.
Use `telega-voip-active-call-p' to understand if call is currently active."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-call-outgoing-hook nil
  "Hook called when outgoing call is made.
Called with single argument - outgoing call."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-call-ready-hook nil
  "Hook called when call is ready.
Called with single argument - the call."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-call-end-hook nil
  "Hook called when call is ended.
Called with single argument - the call.
To find out call state examine the `:state' value."
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-open-file-hook nil
  "Hook called when `telega-open-file' is called.
Hook is called only if file has been opened inside Emacs and has
corresponding buffer."
  :package-version '(telega . "0.6.31")
  :type 'hook
  :group 'telega-hooks)

(provide 'telega-customize)

;;; telega-customize.el ends here

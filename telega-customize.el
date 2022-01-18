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
(require 'cus-edit)                     ; `custom-variable-type'
(require 'dired)                        ; `dired-dwim-target'
(require 'svg)                          ; `svg-embed-base-uri-image'

(defgroup telega nil
  "Telegram client."
  :prefix "telega-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/zevlg/telega.el"))

(defcustom telega-directory (expand-file-name "~/.telega")
  "Directory for telega runtime files.
Set this variable before loading telega, because other variables
depends on `telega-directory' value."
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
- (:@type \"proxyTypeSocks5\" :username <USER> :password <PASSWORD>)
- (:@type \"proxyTypeHttp\" :username <USER> :password <PASSWORD>
   :http_only <BOOL>)
- (:@type \"proxyTypeMtproto\" :secret <SECRET-STRING>)

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

(defcustom telega-old-date-format "%D.%M.%Y"
  "Format for old dates in the chat buffer.
Date considered \"old\" if older then one week from now.
Resulting string must be no longer then 8 chars. Format specifiers:
%D - Two digits for a day.
%M - Two digits for a month.
%Y - Two digits for a year relative to 2000."
  :package-version '(telega . "0.7.22")
  :type 'string
  :group 'telega)

(defcustom telega-help-messages t
  "*Non-nil to show sometime UI related messages."
  :type 'boolean
  :group 'telega)

(defcustom telega-use-short-filenames t
  "*Non-nil to cut /home/user/.telega/cache from filenames."
  :type 'boolean
  :group 'telega)

(defcustom telega-use-short-numbers t
  "Non-nil to write numbers larger then 1000 in \"X.Yk\" form."
  :package-version '(telega . "0.7.57")
  :type 'boolean
  :group 'telega)

(defcustom telega-use-tracking-for nil
  "*Specifies Chat Filter for chats to be tracked with tracking.el.
Make sure you have tracking.el loaded if this option is used.
Only chats with corresponding opened chatbuf are tracked.
Tracking notifications for telega buffers will use the
`telega-tracking` face."
  :package-version '(telega . "0.5.7")
  :type 'list
  :options '((not (or saved-messages (type channel bot)))
             (or unmuted mention))
  :group 'telega)

(defcustom telega-use-images (or (and (fboundp 'image-transforms-p)
                                      (funcall 'image-transforms-p))
                                 (fboundp 'imagemagick-types))
  "Non-nil to show images.
Explicitly set it to non-nil if using Emacs as a service and
want to create X frames to show images.
See https://zevlg.github.io/telega.el/#settings-for-emacs-as-daemon"
  :type 'boolean
  :group 'telega)

(defcustom telega-use-one-line-preview-for
  (when (fboundp 'svg-embed-base-uri-image) 'all)
  "Chat Filter for chats where to show one-line previews for photos/videos.
Used only if `telega-use-images' is non-nil.
Enable it only if you have `:base-uri' SVG functionality, otherwise
performance might suffer."
  :package-version '(telega . "0.7.26")
  :type 'list
  :group 'telega-chat)

;; See https://t.me/emacs_telega/12459
(defcustom telega-button-endings 'telega-button--endings-func
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

(defcustom telega-rainbow-color-function 'telega-color-rainbow-identifier
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

(defcustom telega-company-tooltip-always-below t
  "Non-nil to show company tooltip always below the point.
Done by recentering point in the chatbuf."
  :package-version '(telega . "0.7.47")
  :type 'boolean
  :group 'telega)

(defcustom telega-company-username-complete-nonmember-for '(type bot)
  "Non-nil Chat Filter to complete usernames for non-mebers."
  :package-version '(telega . "0.8.0")
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
  :type '(choice (const :tag "Ask user what to do" query-all)
                 (const :tag "Ask user to open url only" query-open)
                 (const :tag "Open url without query, but query user for login and write access" query-login-and-write-access)
                 (const :tag "Open url without query, but query user for login, without querying for write access" query-login-only)
                 (const :tag "Do not login, just open the url" nil))
  :group 'telega)

(defcustom telega-chat--display-buffer-action
  '((display-buffer-reuse-window display-buffer-same-window))
  "Action value when poping to chatbuffer.
See docstring for `display-buffer' for the values."
  :type 'cons
  :group 'telega)


;;; Docker support
(defgroup telega-docker nil
  "Customisation for docker support."
  :prefix "telega-docker-"
  :group 'telega)

(defcustom telega-use-docker nil
  "*Non-nil to use \"docker\" to run various tools.
Including `telega-server'."
  :package-version '(telega . "0.7.40")
  :type 'boolean
  :group 'telega-docker)

(defcustom telega-docker-security-opt "apparmor=unconfined"
  "security-opt option for the docker run command.
Set to \"apparmor=unconfined\" if you use `telega-appindicator-mode'."
  :package-version '(telega . "0.7.40")
  :type 'string
  :group 'telega-docker)

(defcustom telega-docker-volumes nil
  "List of additional volumes to attach."
  :package-version '(telega . "0.7.40")
  :type 'list
  :group 'telega-docker)

;; To use something like: docker run --security-opt apparmor=unconfined -i -u %u -v %w:%w -v /tmp/.X11-unix:/tmp/.X11-unix -v $XAUTHORITY:$XAUTHORITY -v /var/run/dbus:/var/run/dbus -e DISPLAY=$DISPLAY -e XAUTHORITY=$XAUTHORITY --net=host %i
(defcustom telega-docker-run-command nil
  "Custom docker command to use to run `telega-server' in docker.
If nil, autogenerate the command according to all telega docker settings.
%u - substituted with current used UID:GID
%w - substituted with current Telegram account database directory.
%i - substituted with infered docker image name."
  :package-version '(telega . "0.7.40")
  :type '(or nil string)
  :group 'telega-docker)


(defgroup telega-emoji nil
  "Customisation for telega emojis."
  :prefix "telega-emoji-"
  :group 'telega)

(defcustom telega-emoji-company-backend 'telega-company-emoji
  "Company backend to use for emoji completions."
  :type 'symbol
  :options '(telega-company-telegram-emoji)
  :group 'telega-emoji)

(defcustom telega-emoji-fuzzy-match t
  "*Non-nil to use fuzzy prefix matching.
For example without fuzzy matches, prefix `:jo' will match only
`:joy:', `:joy-cat:' and `:joystick:'.  With fuzzy matching
enabled it will match also `:flag-jo:' and `:black-jocker:'."
  :type 'boolean
  :group 'telega-emoji)

(defcustom telega-emoji-custom-alist nil
  "*Alist of custom emojis to add along with `etc/emojis.alist'."
  :type 'alist
  :group 'telega-emoji)

(defcustom telega-emoji-font-family
  (let ((ffl (font-family-list)))
    (or (car (member "Emoji One" ffl))
        (car (member "Twemoji" ffl))
        (car (member "Noto Color Emoji" ffl))))
  "*Font to use for emoji image generation using `telega-emoji-create-svg'."
  :type 'string
  :group 'telega-emoji)

(defcustom telega-emoji-use-images
  (and telega-use-images (image-type-available-p 'svg) telega-emoji-font-family)
  "*Non-nil to use images for emojis."
  :type 'boolean
  :group 'telega-emoji)

(defcustom telega-emoji-large-height 2
  "*Vertical size in characters for emoji only messages.
Used only if `telega-emoji-use-images' is non-nil."
  :type 'integer
  :group 'telega-emoji)


;; Stickers/Animations
(defcustom telega-sticker-size '(4 . 24)
  "*Size for the sticker.
car is height in chars to use.
cdr is maximum width in chars to use."
  :type 'cons
  :group 'telega)

(defcustom telega-sticker-favorite-background "cornflower blue"
  "*Background color for the favorite stickers.
Can be nil, in this case favorite stickers are not outlined."
  :type '(or nil string)
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

(defcustom telega-sticker-animated-play
  (or (executable-find "tgs2png") telega-use-docker)
  "Non-nil to play animated stickers inside Emacs.
Requires `tgs2png' program from https://github.com/zevlg/tgs2png"
  :package-version '(telega . "0.7.30")
  :type 'boolean
  :group 'telega)

(defcustom telega-emoji-animated-play
  (when telega-sticker-animated-play 'with-sound)
  "Non-nil to play animated emoji as animated sticker.
Can be the `with-sound' symbol , in this case play animated emoji with sound."
  :package-version '(telega . "0.7.81")
  :type 'boolean
  :group 'telega-emoji)

(defcustom telega-animation-height 5
  "*Height in chars for animations."
  :type 'integer
  :group 'telega)

(defcustom telega-animation-play-inline 10
  "*Non-nil to play animation inside telega.
If number, then play animation inline only if animation is shorter
then this number of seconds."
  :package-version '(telega . "0.7.45")
  :type '(or boolean integer)
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

(defcustom telega-avatar-text-compose-chars nil
  "Non-nil to compose sender initials with `telega-symbol-circle'.
Use this to make textual avatars look prettier."
  :type 'boolean
  :group 'telega)

(defcustom telega-vvnote-waves-height-factor 0.75
  "*Factor for waves svg height.
There is a restriction to its value:
`(* (telega-chars-xheight 1) telega-vvnote-waves-height-factor)' must be > 8."
  :type 'float
  :group 'telega)

(defcustom telega-video-note-height '(6 . 9)
  "*Height in chars for video notes.
Can be also a cons cell, where car specifies height for video note
when note is not playing, and cdr specifies height for video note when
note is currently playing."
  :package-version '(telega . 0.7.54)
  :type '(choice integer (cons integer integer))
  :group 'telega)

(defcustom telega-video-note-play-inline t
  "*Non-nil to play video notes inside chatbuffer."
  :type 'boolean
  :group 'telega)

(defcustom telega-video-play-inline nil
  "*Non-nil to play video files inside telega.
NOT USED."
  :type 'boolean
  :group 'telega)

(defcustom telega-video-play-incrementally t
  "Non-nil to start playing video while still downloading the file."
  :package-version '(telega . "0.7.40")
  :type 'boolean
  :group 'telega)

(defcustom telega-video-player-command
  (cond ((executable-find "ffplay")
         '(concat "ffplay -autoexit"
                  (when telega-ffplay-media-timestamp
                    (format " -ss %f" telega-ffplay-media-timestamp))))
        ((executable-find "mpv")
         '(concat "mpv"
                  (when telega-ffplay-media-timestamp
                    (format " --start=%f" telega-ffplay-media-timestamp))))
        ((executable-find "mplayer")
         '(concat "mplayer"
                  (when telega-ffplay-media-timestamp
                    (format " -ss %f" telega-ffplay-media-timestamp)))))
  "Command used to play video files.
Can be a sexp to evaluate to get a command."
  :package-version '(telega . "0.7.57")
  :type '(or string list)
  :options '("mpv" "ffplay -autoexit -fs")
  :group 'telega)

(defcustom telega-open-file-function #'find-file
  "Function to use to open files associated with messages.
Called with single argument - filename to open.
Could be used to open files in external programs.
Set it to `org-open-file' to use Org mode to open files."
  :package-version '(telega . "0.6.31")
  :type 'function
  :options '(org-open-file browse-url-xdg-open)
  :group 'telega)

(defcustom telega-open-message-as-file nil
  "List of message types to open as file using `telega-open-file-function'.
Supported message types are: `photo', `video', `audio',
`video-note', `voice-note', `animation'.
Document messages are always opens as file."
  :package-version '(telega . "0.7.0")
  :type '(repeat (choice (const :tag "Photo messages" photo)
                         (const :tag "Video messages" video)
                         (const :tag "Audio messages" audio)
                         (const :tag "Video Note messages" video-note)
                         (const :tag "Voice Note messages" voice-note)
                         (const :tag "Animation messages" animation)))
  :group 'telega)

(defcustom telega-open-message-ffplay-args
  '((audio      . "-nodisp")
    (voice-note . "-nodisp")
    (animation  . "-loop 0"))
  "*Additional arguments to ffplay to play various type of messages.
Each element is a cons cell, where car is one of: `video', `audio',
`video-note', `voice-note', `animation' and cdr is string with
additional ffplay arguments.

Some useful ffplay arguments to consider:
 - \"-volume 10\" to play with dimmed volume
 - \"-fs\" for `video' messages, to start at fullscreen."
  :package-version '(telega . "0.7.5")
  :type 'list
  :group 'telega)

(defcustom telega-browse-url-alist nil
  "Alist of custom url browse functions.
Each element is in form: `(PREDICATE-OR-REGEX . FUNCTION)'."
  :package-version '(telega . "0.7.8")
  :type 'list
  :group 'telega)

;; Locations
(defcustom telega-location-url-format
  "http://maps.google.com/?q=%N,%E&ll=%N,%E&z=15"
  "*URL format used to open location messages.
%N substituted with latitude.
%E substituted with longitude."
  :type 'string
  :options '("https://yandex.ru/maps/?ll=%E,%N&pt=%E,%N&z=15")
  :group 'telega)

(defcustom telega-my-location nil
  "Set to non-nil to use this as location of me.
Plist in form (:latitude <LAT> :longitude <LONG>)
To publically expose this location set `:is_location_visible' to
non-nil in `telega-options-plist'.
Used to calculate distances from other peers to me."
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
In range [1..3].  Use 1."
  :type 'number
  :group 'telega)

(defcustom telega-location-show-me t
  "*Non-nil to show me on location maps if it fits into the map thumbnail."
  :package-version '(telega . "0.7.63")
  :type 'boolean
  :group 'telega)

(defcustom telega-location-live-tracks t
  "*Non-nil to draw live location tracks."
  :type 'boolean
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
  "Command to run as telega server.
It should be absolute path or binary file searchable in `exec-path'."
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

(defcustom telega-root-view-files-exclude-subdirs
  '((telega-file--downloaded-p "thumbnails" "profile_photos"))
  "Alist specifying which subdirs to exclude when viewing files.
car of each element is predicate matching file, and rest is list of
subdirectories to ignore, i.e. if absolute file name contains any of
the subdirectory in list, then file is ignored.
Supported predicates: `telega-file--downloading-p',
`telega-file--uploading-p', `telega-file--downloaded-p',
`telega-file--uploaded-p', `telega-file--partially-downloaded-p',
`telega-file--partially-uploaded-p'"
  :package-version '(telega . "0.7.6")
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

(defcustom telega-root-aux-inserters nil
  "List of additional inserters to show below Status in the rootbuf.
Can be used to display additional global data.
Insert MUST return non-nil if something has been inserted."
  :package-version '(telega . "0.7.56")
  :type '(repeat function)
  :group 'telega-root)

(defcustom telega-chat-button-width '(0.35 15 48)
  "*Width for the chat buttons in root buffer.
If integer, then use this number of chars.
If float in range (0..1), then occupy this percents of
`telega-root-fill-column' chars, but not less then 10 chars.
If list, where first element is float, then use 1 and 2 list values as
min and max values for a width calculation using
 `telega-canonicalize-number'."
  :package-version '(telega . "0.7.41")
  :type '(or number list)
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

(defcustom telega-chat-title-custom-for
  (list (cons 'saved-messages
              (lambda (title) (propertize title 'face 'bold)))
        (cons 'replies-messages
              (lambda (title) (propertize title 'face 'bold))))
  "Alist of custom titles for chats.
Each element is a cons cell, where car is a Chat Filter and cdr
is a function accepting title string and returning string."
  :package-version '(telega . "0.6.31")
  :type 'alist
  :group 'telega-root)

(defcustom telega-chat-title-emoji-use-images telega-emoji-use-images
  "*Non-nil to use images for emojis in chat's title.
Otherwise use simple chars."
  :type 'boolean
  :group 'telega-chat)

;;; ellit-org: folders-options
;; - {{{user-option(telega-folder-icons-alist, 2)}}}
(defcustom telega-folder-icons-alist
  (list (cons "All"      "💬")
        (cons "Unread"   "✅")
        (cons "Unmuted"  "🔔")
        (cons "Bots"     "🤖️")
        (cons "Channels" "📢")
        (cons "Groups"   "👥")
        (cons "Private"  "👤")
        ;; NOTE: all folders has "Custom" icon name, to avoid icon
        ;; being displayed with folder name we exclude "Custom" from
        ;; the list
;        (cons "Custom"   "📁")
        (cons "Setup"    "📋")
        (cons "Cat"      "🐱")
        (cons "Crown"    "👑")
        (cons "Favorite" "⭐️")
        (cons "Flower"   "🌹")
        (cons "Game"     "🎮")
        (cons "Home"     "🏠")
        (cons "Love"     "❤️")
        (cons "Mask"     "🎭")          ; or 😷
        (cons "Party"    "🍸")
        (cons "Sport"    "⚽️")          ; or 🏅
        (cons "Study"    "🎓")
        (cons "Trade"    "📊")          ; or 📈
        (cons "Travel"   "🛫️")          ; or ✈️
        (cons "Work"     "💼")
        )
  "Alist of symbols to be used as folder icons instead of `telega-symbol-folder'.
See list of all available icon names in `telega-folder-icon-names'."
  :type 'alist
  :group 'telega)

;;; ellit-org: folders-options
;; - {{{user-option(telega-chat-folder-format, 2)}}}
(defcustom telega-chat-folder-format (propertize "%F | " 'face 'bold)
  "*Non-nil to prefix chat's title with chat folder.
%I - Replaced with folder's icon from `telega-folder-icon-names' or
     empty string if there is no icon.
%i - Replaced with folder's icon from `telega-folder-format' or
     `telega-symbol-folder' if there is no icon.
%f - Replaced with folder's title.
%F - Replaced with folder's icon from `telega-folder-icon-names'
     if icon is unique, or equivalent to %I%f."
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

(defcustom telega-filter-unread-chats '(and main unread)
  "*Chat Filter for `telega-switch-unread-chat' command."
  :package-version '(telega . "0.7.22")
  :type 'list
  :group 'telega-filter)

(defcustom telega-filters-custom
  '(("Main" . main)
    ("Important" . (or mention (and unread unmuted)))
    ("Online" . (and (not saved-messages) (online-status "Online")))
    ("Groups" . (type basicgroup supergroup))
    ("Channels" . (type channel))
    ("Archive" . archive))
  "*Alist of custom filters in form (NAME . CHAT-FILTER).
NAME is evaluated to get resulting string, so it could be a lisp
form.
This filters are displayed as filter buttons at the top of rootbuf."
  :package-version '(telega . "0.7.57")
  :type 'alist
  :group 'telega-filter)

(defcustom telega-filter-custom-expand t
  "*Non-nil to expand custom filter when adding to active filters."
  :type 'boolean
  :group 'telega-filter)

;;; ellit-org: folders-options
;; - {{{user-option(telega-filter-custom-show-folders, 2)}}}
(defcustom telega-filter-custom-show-folders t
  "Non-nil to show telegram folders along the side with custom filters."
  :package-version '(telega . "0.6.24")
  :type 'boolean
  :group 'telega-filter)

(defcustom telega-filter-custom-one-liners '(custom folders)
  "List of custom filter types to display in one line.
If `custom' is in the list, then display all custom filters in one line.
If `folders' is in the list, then display all folders in one line.
Otherwise fit custom filters into `telega-root-fill-column'."
  :package-version '(telega . "0.7.56")
  :type '(repeat (choice (const :tag "Custom Filters" custom)
                         (const :tag "Folders" folders)))
  :group 'telega-filter)

(defcustom telega-filter-button-width '(0.25 17 25)
  "*Width of the custom filter buttons.
If integer, then use this number of chars.
If float in range (0..1), then occupy this percents of
`telega-root-fill-column' chars, but not less then 15 chars.
If list, where first element is float, then use 1 and 2 list values as
min and max values for a width calculation using
`telega-canonicalize-number'."
  :package-version '(telega . "0.7.41")
  :type '(or number list)
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

;; WebPage preview settings
(defcustom telega-webpage-preview-size-limits '(10 5 55 10)
  "Same as `telega-photo-size-limits', but for webpage preview image.
Set to nil to disable photo in a webpage preview."
  :package-version '(telega . "0.7.20")
  :type '(choice (const :tag "Disable photo preview" nil)
                 (list integer integer integer integer))
  :group 'telega-webpage)

(defcustom telega-webpage-preview-description-limit nil
  "Maximum length in chars to display webpage preview description.
Set to 0 to disable description in a webpage preview."
  :package-version '(telega . "0.7.20")
  :type '(choice (const :tag "No limit" nil)
                 (const :tag "Disable webpage preview description" 0)
                 integer)
  :group 'telega-webpage)


(defgroup telega-user nil
  "Customization for users."
  :group 'telega)

(defcustom telega-user-show-avatars telega-use-images
  "Non-nil to show avatars for the users."
  :type 'boolean
  :group 'telega-user)

(defcustom telega-company-username-show-avatars telega-user-show-avatars
  "Non-nil to show avatars in the company annotation."
  :package-version '(telega . "0.7.44")
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

(defcustom telega-chat-ret-always-sends-message t
  "Non-nil to make `\\<telega-chat-mode-map>\\[telega-chatbuf-newline-or-input-send]' always send a message.
Otherwise
`\\<telega-chat-mode-map>\\[telega-chatbuf-newline-or-input-send]'
sends a message only if point is at the end of the chatbuf input or
inserts newline otherwise."
  :package-version '(telega . "0.7.2")
  :type 'boolean
  :group 'telega-chat)

(defcustom telega-chat-send-messages-async nil
  "Non-nil to send messages in async manner, not waiter telega-server reply.
Setting it to non-nil might introduce additional flickery in chatbuf."
  :package-version '(telega . "0.7.2")
  :type 'boolean
  :group 'telega-chat)

(defcustom telega-chat-send-disable-webpage-preview nil
  "Non-nil to disable web page previews for the outgoing messages.
There will be no way to enable web page previews untill this option is
enabled."
  :package-version '(telega . "0.7.2")
  :type 'boolean
  :group 'telega-chat)

(defcustom telega-chat-show-avatars telega-use-images
  "*Non-nil to show user avatars in chat buffer."
  :type 'boolean
  :group 'telega-chat)

(defcustom telega-chat-prompt-format
  '((:eval (telega-chatbuf-prompt-default-sender-avatar))
    (:eval (telega-chatbuf-prompt-body))
    (:eval (when (and telega-use-images
                      (telega-chatbuf-match-p
                       '(and has-avatar can-send-or-post)))
             (telega-chatbuf-prompt-chat-avatar)))
    ">>> ")
  "*Modeline compatible format for the chatbuf input prompt.
You can use `telega-chatbuf-editing-msg' or
`telega-chatbuf-replying-msg' in `:eval' section if you want different
prompt when editing/replying a message."
  :package-version '(telega . "0.7.101")
  :type 'string
  :group 'telega-chat)

(defcustom telega-chat-input-ring-size 50
  "*Size of the chat input history."
  :type 'integer
  :group 'telega-chat)

(defcustom telega-chat-input-markups '(nil "markdown1" "markdown2")
  "Markups to apply when sending input with `\\<telega-chat-mode-map>\\[telega-chatbuf-newline-or-input-send]'.
Each index in the list corresponds to the number of
`\\[universal-argument]' supplied before `RET', i.e. first element is
used for ordinary `RET', second is used for `C-u RET', and third is for
`C-u C-u RET' and so on.  Supported markups are defined in
`telega-chat-markup-functions'."
  :type 'list
  :package-version '(telega . "0.7.4")
  :group 'telega-chat)

(defcustom telega-chat-scroll-conservatively 101
  "Value for `scroll-conservatively' in chatbuf."
  :package-version '(telega . "0.7.3")
  :type 'int
  :group 'telega-chat)

(defcustom telega-chat-bidi-display-reordering nil
  "Value for `bidi-display-reordering' in chatbuf."
  :package-version '(telega . "0.7.3")
  :type 'boolean
  :group 'telega-chat)

(defcustom telega-chat-messages-pop-ring-size 50
  "*Size of the chat pop messages ring.
`telega-chatbuf-goto-pop-message' command is used to pop messages
from the ring."
  :package-version '(telega . "0.7.0")
  :type 'integer
  :group 'telega-chat)

(defcustom telega-chat-fill-column fill-column
  "*Column to fill chat messages to."
  :type 'integer
  :group 'telega-chat)

(defcustom telega-chat-history-limit 30
  "Number of messages to fetch on history requests."
  :package-version '(telega . "0.7.0")
  :type 'integer
  :group 'telega-chat)

(defcustom telega-chat-use-date-breaks-for 'all
  "Chat Filter for chats where to insert date breaks.
Date break is a special mark separating two messages received on
different days. Such as:
#+begin_example
  MSG1                              <--- msg sent on 27dec
  -------(28 December 2020)------   <--- date break
  MSG2                              <--- msg sent on 28dec
#+end_example"
  :package-version '(telega . "0.7.4")
  :type 'list
  :group 'telega-chat)

(defcustom telega-chat-attach-commands
  '(("photo" nil telega-chatbuf-attach-photo)
    ("self-destruct-photo" (lambda ()
                             (telega-chat-private-p telega-chatbuf--chat))
     telega-chatbuf-attach-ttl-photo)
    ("video" nil telega-chatbuf-attach-video)
    ("self-destruct-video" (lambda ()
                             (telega-chat-private-p telega-chatbuf--chat))
     telega-chatbuf-attach-ttl-video)
    ("video-note" nil telega-chatbuf-attach-video-note)
    ("audio" nil telega-chatbuf-attach-audio)
    ("voice-note" nil telega-chatbuf-attach-voice-note)
    ("file" nil telega-chatbuf-attach-file)
    ("gif" nil telega-chatbuf-attach-gif)
    ("location" nil telega-chatbuf-attach-location)
    ("poll" (lambda ()
              (not (telega-chat-private-p telega-chatbuf--chat)))
     telega-chatbuf-attach-poll)
    ("contact" nil telega-chatbuf-attach-contact)
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
    ("theme"
     (lambda ()
       (telega-chatbuf-match-p '(type private secret)))
     telega-chatbuf-attach-chat-theme)

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
    ("send-by"
     (lambda ()
       (telega-chatbuf-match-p 'has-default-sender))
     telega-chatbuf-attach-send-by)
    )
  "*List of the attachments available for `C-c C-a' in chatbuf.
Each element is a list of three elements:
  \\(ATTACH-NAME AVAILABILITY-FUNC COMMAND-FUNC\\)

AVAILABILITY-FUNC nil, if COMMAND-FUNC is always available,
otherwise function returning non-nil if COMMAND is available.
COMMAND-FUNC - Command function to execute."
  :type 'list
  :group 'telega-chat)

(custom-declare-variable
 'telega-dired-dwim-target
 'dired-dwim-target
 "*Value to bind `dired-dwim-target' to, in telega file pickers."
 :type (custom-variable-type 'dired-dwim-target)
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

(defcustom telega-chat-delete-move-to-archive-for nil
  "Chat Filter for chats to move to Archive on chat deletion.
NOT YET USED."
  :type 'boolean
  :group 'telega-chat)

(defcustom telega-chat-mode-line-format
  '((:eval (telega-chatbuf-mode-line-video-chat 20))
    (:eval (telega-chatbuf-mode-line-discuss))
    (:eval (telega-chatbuf-mode-line-unread))
    (:eval (telega-chatbuf-mode-line-marked))
    (:eval (telega-chatbuf-mode-line-members 'use-icons))
    (:eval (telega-chatbuf-mode-line-pinned-or-thread-msg 20))
    (:eval (telega-chatbuf-mode-line-messages-filter)))
  "Additional mode line format for chat buffer identification.
See `mode-line-buffer-identification'."
  :package-version '(telega . "0.7.35")
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

(defcustom telega-chat-input-complete-function nil
  "*Custom function to be called before any other on TAB in chatbuf input.
Should return non-nil if completion occured."
  :package-version '(telega . "0.6.30")
  :type 'function
  :options '(counsel-company)
  :group 'telega-chat)

(defcustom telega-chat-message-filters-as-media nil
  "List of message filters from `telega-chat--message-filters'.
Messages for these filters are displayed in compact way in chatbuf.
This is EXPERIMENTAL feature, use on your own risk."
  :package-version '(telega . "0.7.0")
  :type 'list
  :options '("photo" "photo-video" "gif" "video" "chat-photo")
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
  "*Reply with busy status to any incoming call if other call is active."
  :type 'boolean
  :group 'telega-voip)

(defcustom telega-voip-help-echo t
  "*Non-nil to show help messages in echo area on call activation."
  :type 'boolean
  :group 'telega-voip)

(defcustom telega-video-chat-display
  '((active footer modeline)
    (passive modeline)
    (scheduled footer modeline))
  "Alist to specify where to display video chat info.
Each element is a cons cell where car is one of `active', `passive' or
`scheduled'and cdr is list of possible values:
  - `footer' to display video chat info in the chatbuf footer
  - `modeline' to display video chat info in the modeline.
Video chat is considered `active' if it has at least one participant,
otherwise video chat is `passive'."
  :type 'list
  :package-version '(telega . "0.7.90")
  :group 'telega-voip)


;; Notifications
(defgroup telega-notifications nil
  "Setup for D-Bus notifications."
  :group 'telega)

(defcustom telega-notifications-delay 0.5
  "*Delay in seconds for notifications.
This delay is taken before making decision show or not the
message in notification. Taking pause before showing notification
is wise, because another Telegram client may be active with the
chat opened, you don't want the notification to be shown for
already read message.  Set it to 0, to not take any delay."
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
  "*Additional arguments to `notifications-notify' on chat messages.
Add `:actions' with value `(\"default\" \"show message\")' for
clickable notifications. Clickable notifications does not work well
with notify-osd, thats why actions are disabled by default."
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
  "Alist with notification settings for notification groups.
car of each element is one of: `private', `group' or `channel'.
rest is notification settings.
For example:
  '((private :mute_for 0 :show_preview t)
    (group :mute_for 599695961 :show_preview t)
    (channel :mute_for 540465803 :show_preview t))"
  :package-version '(telega . "0.7.2")
  :type 'cons
  :group 'telega-notifications)


(defgroup telega-msg nil
  "Customization for telega messages formatting."
  :prefix "telega-msg-"
  :group 'telega)

(defcustom telega-msg-ignore-predicates nil
  "List of predicates to ignore messages.
Each element is a function accepting single argument - messages and
returning non-nil if message should be ignored.
Use this for Clint Side Messages Filtering."
  :package-version '(telega . "0.7.5")
  :type '(list function)
  :group 'telega-msg)

(defcustom telega-msg-rainbow-title t
  "*Non-nil to display user names in chatbuf with their assigned color."
  :type 'boolean
  :group 'telega-msg)

(defcustom telega-msg-heading-whole-line nil
  "*Non-nil to spread `telega-msg-heading' face to full line width.
Also applies to `telega-msg-inline-reply' face."
  :type 'boolean
  :group 'telega-msg)

(defcustom telega-msg-edit-markup-spec
  '(telega--fmt-text-markdown2 . "markdown2")
  "Cons cell specifying how to format message text when editing.
car is a function to convert message's text to markup string.
cdr is a markup name from `telega-chat-markup-functions' to use as
markup attachment.  Use nil to edit message as is, without using
\"markup\" attachment type."
  :package-version '(telega . "0.7.11")
  :type 'cons
  :options '((telega--fmt-text-markdown1 . "markdown1"))
  :group 'telega-msg)

(defcustom telega-msg-hack-on-can-get-message-thread t
  "Non-nil to hack on `:can_get_message_thread' message property.
In case MSG has `:message_thread_id' and has no
`:can_get_message_thread', we can still fetch the thread, because
aparently message thread id is actually a thread starter message
id.  This could change anytime in Telegram, and this option will
no longer work."
  :package-version '(telega . "0.6.30")
  :type 'boolean
  :group 'telega-msg)

(defcustom telega-photo-show-details t
  "*Non-nil to show photo details for photo messages."
  :type 'boolean
  :group 'telega-msg)

(defcustom telega-msg-save-dir nil
  "*Directory for `telega-msg-save' without asking.
If nil, the saved path is always asked."
  :type 'string
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

(defcustom telega-inline-photo-size-limits '(10 4 20 8)
  "Same as `telega-photo-size-limits', but for photos from inline queries.
Inlined photos are displayed, when sending messages via bots, for
example @gif `TAB' will popup buffer with inlined photos. "
  :package-version '(telega . "0.7.5")
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
  "*Non-nil to make ignored messages visible as <ignored message>."
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
  :options '(ivy-completing-read helm--completing-read-default)
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
        ((executable-find "maim")
         'telega-screenshot-with-maim)
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

(defcustom telega-symbol-mode "◁"
  "*String used for telega modes."
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

(defcustom telega-symbol-eye "👁️"
  "String to use as eye symbol."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-pin "📌"
  "*String to use as pin symbol."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-custom-order (cons "🡓" "🡑")
  "Symbols used to emphasize custom order for the chat.
car is used if custom order is less then real chat's order.
cdr is used if custom order is greater then real chat's order."
  :type 'cons
  :group 'telega-symbol)

(defcustom telega-symbol-lock "🔒️"
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
  :options '("📄")
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

(defcustom telega-symbol-blank-button (propertize " " 'display '(space :width 2))
  "Symbol for blank checkmark buttons."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-failed (propertize "⛔" 'face 'error)
  "Mark messages that have sending state failed."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-horizontal-bar "-"
  "Symbol used to draw horizontal bars/delimiters.
Horizontal delimiters are used to draw chat filter/sorter bar in rootbuf."
  :package-version '(telega . "0.7.58")
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

(defcustom telega-symbol-verified "✴️"
  "Symbol used to emphasize verified users/groups."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-star (propertize "★" 'face 'error)
  "Symbol used to emphasize starred chats."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-lightning "⚡️"
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

(defcustom telega-symbol-member "👤"
  "Symbol used for chat members and non-contact users."
  :type 'string
  :options '("🧍")
  :group 'telega-symbol)

(defcustom telega-symbol-contact "👥"
  "Symbol used for contacts."
  :type 'string
  :options '("👥")
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

(defcustom telega-symbol-credit-card "💳"
  "Symbol used for inline keyboard buttons of type \"buy\"."
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

(defcustom telega-symbol-mark
  (if (fboundp 'define-fringe-bitmap)
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

(defcustom telega-symbol-alarm "⏲️"
  "*Symbol used for scheduled messages."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-dice-list (list "🎲" "⚀" "⚁" "⚂" "⚃" "⚄" "⚅")
  "List of dices to show for \"messageDice\"."
  :type 'list
  :group 'telega-symbol)

(defcustom telega-symbol-folder "📁"
  "Symbol used for Telegram folders."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-multiple-folders "🗂️"
  "Symbol to use to denote multiple folders."
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

(defcustom telega-symbol-reply "⮪"
  "Symbol used to for replies."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-forward (compose-chars ?🗩 ?🠒)
  "Symbol used to display forwarding."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-circle "◯"
  "Circle to create nice looking text avatars."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-bulp "💡"
  "Bulp symbol to be used for quiz polls with explanation."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-chat-list "📑"
  "Symbol to use to emphasize main or archive custom filter buttons."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-bell "🔔"
  "Symbol to use to draw a bell (notification)."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-download-progress '(?= . ?>)
  "Symbols to use when drawing progress bar for dowloading files.
By default `(?= . ?>)' is used resulting in =====> progress bar."
  :package-version '(telega . "0.7.5")
  :type '(or char (cons char char))
  :group 'telega)

(defcustom telega-symbol-upload-progress '(?+ . ?>)
  "Symbols to use when drawing progress bar for uploading files.
By default `(?+ . ?>)' is used resulting in +++++> progress bar."
  :package-version '(telega . "0.7.5")
  :type '(or char (cons char char))
  :group 'telega)

(defcustom telega-symbol-video-chat-active "🗣️"
  "Symbol to use for non-empty video chats."
  :package-version '(telega . "0.7.90")
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-video-chat-passive "🗧"
  "Symbol to use for empty video chats."
  :package-version '(telega . "0.7.90")
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-favorite "🔖"
  "Symbol to use for favorite messages, bookmarks."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-leave-comment "💬"
  "Symbol used to display symbol nearby \"Leave Comment\" button."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-distance "📏"
  "Symbol used to display distance."
  :type 'string
  :group 'telega-symbol)

(defcustom telega-symbol-copyright "©"
  "Symbol used to emphasize protected content."
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
         telega-symbol-multiple-folders
         telega-symbol-keyboard
         telega-symbol-reply
         telega-symbol-forward
         telega-symbol-circle
         telega-symbol-bulp
         telega-symbol-chat-list
         telega-symbol-bell
         telega-symbol-video-chat-active
         telega-symbol-video-chat-passive
         ))
  "*Custom widths for some symbols, used for correct formatting.
Use `telega-symbol-set-width' to install symbol's width.
Install all symbol widths inside `telega-load-hook'."
  :type 'list
  :group 'telega-symbol)

(defcustom telega-symbols-emojify
  `((verified ,(when (image-type-available-p 'svg)
                 '(telega-etc-file "verified.svg")))
    (vertical-bar ,(when (image-type-available-p 'svg)
                     '(telega-svg-create-vertical-bar)))
    (horizontal-bar ,(when (image-type-available-p 'svg)
                       '(telega-svg-create-horizontal-bar)))
    (underline-bar ,(when (image-type-available-p 'svg)
                      '(telega-svg-create-horizontal-bar
                        1 0.7 telega-symbol-underline-bar)))
    alarm attachment
    bell bulp
    chat-list contact
    distance
    eye
    failed favorite flames folder
    game
    invoice
    leave-comment lightning lock location
    member multiple-folders
    pause phone photo pin poll play
    reply
    video video-chat-active video-chat-passive

    "⏪" "⏩"
    )
  "List of symbols to emojify if `telega-emoji-use-images' is non-nil.
Each element is either XXX from ending of telega-symbol-XXX or list,
where car is XXX and rest is form to evaluate to get image or a
filename with an image to be used."
  :package-version '(telega . "0.7.1")
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

;; NOTE: better to use :line-width (-2 . -2), but this is only in newer Emacs
;; see https://t.me/emacs_telega/22129
(defface telega-button
  `((((class color) (min-colors 88) (background light))
     :foreground "RoyalBlue3"
     :box (:line-width ,(if (version< emacs-version "28.0") -2 (cons -2 -2))
                       :color "RoyalBlue3" :style nil))
    (((class color) (min-colors 88) (background dark))
     :foreground "cyan1"
     :box (:line-width ,(if (version< emacs-version "28.0") -2 (cons -2 -2))
                       :color "cyan1" :style nil))
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

(defface telega-tracking
  '((t :inherit telega-username))
  "Face to display tracking mode-line notifications."
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

(defface telega-entity-type-spoiler
  `((t :foreground ,(face-foreground 'default)
       :background ,(face-foreground 'default)))
  "Face to display spoilers in the text."
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
  '((t :family "FreeSerif" :height 1.3))
  "Face to display header in webpage instant view."
  :group 'telega-faces)

(defface telega-webpage-subheader
  '((t :family "FreeSerif" :height 1.15))
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

(defface telega-msg-sponsored
  '((t :inherit shadow))
  "Face to display sponsored message."
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

(defcustom telega-chat-goto-message-hook nil
  "Hook called when user goes to a message.
Called with a single argument - MESSAGE."
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

(defcustom telega-file-downloaded-hook nil
  "Hook called when some file has been downloaded.
Called with single argument - TDLib file structure.
Could be used to copy downloaded files to another place.
See https://t.me/emacs_telega/21925"
  :package-version '(telega . "0.7.1")
  :type 'hook
  :group 'telega-hooks)

(defcustom telega-online-status-hook nil
  "Hook called when my online status changes.
Called with single argument - ONLINE-P."
  :package-version '(telega . "0.7.20")
  :type 'hook
  :group 'telega-hooks)

(provide 'telega-customize)

;;; telega-customize.el ends here

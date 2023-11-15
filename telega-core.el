;;; telega-core.el --- Core functionality for telega  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Mon Apr 23 18:09:01 2018
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

;; Variables, macroses, defsubst and runtime goodies for telega

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'ring)
(require 'cursor-sensor)

(require 'telega-customize)

(declare-function telega-chat--info "telega-chat" (chat))
(declare-function telega-window-recenter "telega-util" (win &optional nlines from-point))
(declare-function telega-emoji-create-svg "telega-util" (emoji &optional c-height))
(declare-function telega-chats-compare "telega-sort" (criteria chat1 chat2))

(defvar telega--lib-directory nil
  "The directory from where this library was first loaded.")

(defun telega-etc-file (filename)
  "Return absolute path to FILENAME from etc/ directory in telega."
  (expand-file-name (concat "etc/" filename) telega--lib-directory))

(defconst telega-spoiler-translation-table
  (let ((table (make-char-table 'translation-table)))
    (set-char-table-range table t ?\█)
    (aset table ?\s ?\s)
    (aset table ?\n ?\n)
    table)
  "Translation table to hide for spoiler text.")

(defconst telega-chat-types
  '(private secret basicgroup supergroup bot channel)
  "All types of chats supported by telega.")

(defconst telega-msg-id-step 1048576    ; 2^20
  "Message id difference between two consequent messages.")

(defconst telega-mute-for-ever 500000000)

(defconst telega-mute-for-intervals
  ;;  1h    4h    1d     7d   forever
  `(3600 14400 86400 604800 ,telega-mute-for-ever))

(defconst telega--slow-mode-delays '(0 10 30 60 300 900 3600)
  "List of allowed slow mode delays.")

(defconst telega-chat--chat-media-permissions
  '((:can_send_audios . "lng_rights_chat_music")
    (:can_send_documents . "lng_rights_chat_files")
    (:can_send_photos . "lng_rights_chat_photos")
    (:can_send_videos . "lng_rights_chat_videos")
    (:can_send_video_notes . "lng_rights_chat_video_messages")
    (:can_send_voice_notes . "lng_rights_chat_voice_messages")))

(defconst telega-chat--chat-permissions
  `((:can_send_basic_messages . "lng_rights_chat_send_text")

    ;; Media, "lng_rights_chat_send_media"
    ,@telega-chat--chat-media-permissions

    (:can_send_polls . "lng_rights_chat_send_polls")
    (:can_send_other_messages . "lng_rights_chat_send_stickers")
    (:can_add_web_page_previews . "lng_rights_chat_send_links")
    (:can_change_info . "lng_rights_group_info")
    (:can_invite_users . "lng_rights_chat_add_members")
    (:can_pin_messages . "lng_rights_group_pin")
    (:can_manage_topics . "lng_rights_group_topics")))

(defconst telega-chat--admin-permissions
  '((:can_be_edited . "lng_rights_edit_admin")
    (:can_manage_chat . nil)            ;TODO
    (:can_change_info . "lng_rights_group_info")
    (:can_post_messages . "lng_rights_channel_post")
    (:can_edit_messages . "lng_rights_channel_edit")
    (:can_delete_messages . "lng_rights_group_delete")
    (:can_invite_users . "lng_rights_group_invite_link")
    (:can_restrict_members . "telega_rights_restrict_members")
    (:can_pin_messages . "lng_rights_group_pin")
    (:can_manage_topics . "lng_rights_group_topics")
    (:can_promote_members . "telega_rights_promote_members")
    (:can_manage_video_chats . "lng_rights_group_manage_calls")

    (:can_post_stories . "lng_rights_channel_post_stories")
    (:can_edit_stories . "lng_rights_channel_edit_stories")
    (:can_delete_stories . "lng_rights_channel_delete_stories")

    (:is_anonymous . "lng_rights_group_anonymous")))

(defconst telega-chat--admin-permissions-for-channels
  '(:can_post_messages :can_edit_messages :can_post_stories :can_edit_stories
                       :can_delete_stories))

(defconst telega-notification-scope-types
  '((private . "notificationSettingsScopePrivateChats")
    (group . "notificationSettingsScopeGroupChats")
    (channel . "notificationSettingsScopeChannelChats"))
  "Map of lisp name of the scope to TDLib scope type name.")

(defconst telega-currency-symbols-alist
  '(("EUR" . "€")
    ("USD" . "$")
    ("RUB" . "₽"))
  "Alist of currency symbols.")

(defvar telega--column-offset 0
  "Additional offset for the `telega-currency-column'.")

(defconst telega-symbol-nbsp "\u00a0"
  "Non-breakable space.")

(defconst telega-emoji-animated-fullscreen-list
  '("🎆" "🎉" "🎈" "👍" "💩" "❤" "👻" "👎" "🤮" "😂" "💸" "🎃" "🍆")
  "List of animated emojis with fullscreen support.")

(defvar telega-emoji-reaction-list nil
  "List of supported reactions, updated on `updateActiveEmojiReactions' event.")

(defvar telega-default-reaction-type nil
  "Default reaction for the messages.
Updated on `updateDefaultReactionType' event.")

(defconst telega-translate-languages-alist
  '(("Afrikaans" . "af") ("Albanian" . "sq") ("Amharic" . "am")
    ("Arabic" . "ar") ("Armenian" . "hy") ("Azerbaijani" . "az")
    ("Basque" . "eu") ("Belarusian" . "be") ("Bengali" . "bn")
    ("Bosnian" . "bs") ("Bulgarian" . "bg") ("Catalan" . "ca")
    ("Cebuano" . "ceb") ("Chinese" . "zh")
    ("Chinese Simplified" . "zh-CN") ("Chinese Traditional" . "zh-TW")
    ("Chichewa" . "ny")
    ("Corsican" . "co") ("Croatian" . "hr") ("Czech" . "cs")
    ("Danish" . "da") ("Dutch" . "nl") ("English" . "en")
    ("Esperanto" . "eo") ("Estonian" . "et") ("Filipino" . "tl")
    ("Finnish" . "fi") ("French" . "fr") ("Frisian" . "fy")
    ("Galician" . "gl") ("Georgian" . "ka") ("German" . "de")
    ("Greek" . "el") ("Gujarati" . "gu") ("Haitian Creole" . "ht")
    ("Hausa" . "ha") ("Hawaiian" . "haw") ("Hebrew" . "iw")
    ("Hindi" . "hi") ("Hmong" . "hmn") ("Hungarian" . "hu")
    ("Icelandic" . "is") ("Igbo" . "ig") ("Indonesian" . "id")
    ("Irish" . "ga") ("Italian" . "it") ("Japanese" . "ja")
    ("Kannada" . "kn") ("Kazakh" . "kk") ("Khmer" . "km")
    ("Korean" . "ko") ("Kurdish (Kurmanji)" . "ku") ("Kyrgyz" . "ky")
    ("Lao" . "lo") ("Latin" . "la") ("Latvian" . "lv")
    ("Lithuanian" . "lt") ("Luxembourgish" . "lb") ("Macedonian" . "mk")
    ("Malagasy" . "mg") ("Malay" . "ms") ("Malayalam" . "ml")
    ("Maltese" . "mt") ("Maori" . "mi") ("Marathi" . "mr")
    ("Mongolian" . "mn") ("Myanmar (Burmese)" . "my") ("Nepali" . "ne")
    ("Norwegian" . "no") ("Pashto" . "ps") ("Persian" . "fa")
    ("Polish" . "pl") ("Portuguese" . "pt") ("Punjabi" . "pa")
    ("Romanian" . "ro") ("Russian" . "ru") ("Samoan" . "sm")
    ("Scots Gaelic" . "gd") ("Serbian" . "sr") ("Sesotho" . "st")
    ("Shona" . "sn") ("Sindhi" . "sd") ("Sinhala" . "si")
    ("Slovak" . "sk") ("Slovenian" . "sl") ("Somali" . "so")
    ("Spanish" . "es") ("Sundanese" . "su") ("Swahili" . "sw")
    ("Swedish" . "sv") ("Tajik" . "tg") ("Tamil" . "ta")
    ("Telugu" . "te") ("Thai" . "th") ("Turkish" . "tr")
    ("Ukrainian" . "uk") ("Urdu" . "ur") ("Uzbek" . "uz")
    ("Vietnamese" . "vi") ("Welsh" . "cy") ("Xhosa" . "xh")
    ("Yiddish" . "yi") ("Yoruba" . "yo") ("Zulu" . "zu"))
  "Language codes used for translations.")

;;; Runtime variables
(defvar telega-msg-contains-unread-mention nil
  "Bind this variable when displaying message containing unread mention.")

(defvar telega--chat nil
  "Telega chat for the current buffer.
Used in some buffers to refer chat.")
(make-variable-buffer-local 'telega--chat)

(defvar telega--help-win-param nil
  "Parameter for the `telega--help-win-redisplay-func'.
Used in help buffers to store some additional data.")
(make-variable-buffer-local 'telega--help-win-param)

(defvar telega--help-win-inserter nil
  "Inserter function for the help win.
This function accepts exactly one argument - `telega--help-win-param'.")
(make-variable-buffer-local 'telega--help-win-inserter)

(defvar telega--help-win-dirty-p nil
  "Non-nil if help win need redisplay.
Used for optimisations.")
(make-variable-buffer-local 'telega--help-win-dirty-p)

(defvar telega--me-id nil "User id of myself.")
(defvar telega--replies-id nil "Id of the \"Replies\" chat.")
(defvar telega--options nil "Options updated from telega-server.")
(defvar telega--auth-state nil
  "Current Authorization state.")
(defvar telega--conn-state nil
  "Current connection state.")
(defvar telega--status "Not Started" "Status of the connection to telegram.")
(defvar telega--status-aux
  "Aux status used for long requests, such as fetching chats/searching/etc")
(defvar telega--chats nil "Hash table (id -> chat) for all chats.")
(defvar telega--chat-topics nil "Hash table (id -> topics list) for forums chats.")
(defvar telega--story-list-chat-count nil
  "Plist with number of chats having active stories.
Props are `main' and `archive'.")
(defvar telega--chat-active-stories nil
  "Hash table (chat-id -> chatActiveStories) for chat's stories.")

(defvar telega--cached-messages nil
  "Hash table ((chat-id . msg-id) -> msg) of cached messages.
Such as pinned, replies, etc.")
(defvar telega--cached-stories nil
  "Hash table ((chat-id . story-id) -> story) of cached stories.")
(defvar telega--actions nil
  "Hash table ((chat-id . msg-thread-id) -> alist-of-user-actions).")
(defun telega-chat--actions (chat &optional msg-thread-id)
  "Return actions for the CHAT and optional MSG-THREAD-ID."
  (gethash (cons (plist-get chat :id) (or msg-thread-id 0)) telega--actions))

(defvar telega--ordered-chats nil "Ordered list of all chats.")
(defvar telega--filtered-chats nil
  "Chats filtered by currently active filters.
Used to calculate numbers displayed in custom filter buttons.")
(defvar telega-deleted-chats nil
  "List of recently deleted chats.
Used for \"Recently Deleted Chats\" rootview.")
(defvar telega--blocked-user-ids-alist '((blockListMain . (0))
                                         (blockListStories . (0)))
  "Alist of blocked users.
car of the element is block list, one of `blockListMain' or
`blockListStories', cdr is a list of user ids in that block list.
Used to avoid fetching user's full-info to find out that user is blocked.
CAR of the user ids list is current offset for
`telega--getBlockedMessageSenders'.")

(defvar telega--dirty-chats nil
  "List of chats that need to be updated with `telega-chat--update'.
Dirtiness types are stored in the `:telega-dirtiness' chat's property.")
(defvar telega--filters nil "List of active filters.")
(defvar telega--undo-filters nil "List of undo entries.")
(defvar telega--sort-criteria nil "Active sorting criteria list.")
(defvar telega--sort-inverted nil "Non-nil if sorting is inverted.")
(defvar telega--sort-reorder-dirtiness nil
  "List of event types affecting chat order for Active sorting criteria.")

(defvar telega--info nil "Alist of (TYPE . INFO-TABLE).")
(defvar telega--full-info nil "Alist of (TYPE . FULL-INFO-TABLE).")

(defvar telega-full-info-offline-p t
  "Non-nil to not request telega-server in case full info is not available.
Bind to nil to ensure full info is actualized.")

(defvar telega--top-chats nil
  "Alist of (CATEGORY LAST-UPDATE-TIME ..)
CATEGORY is one of `Users', `Bots', `Groups', `Channels',
`InlineBots', `Calls'")
(defvar telega--last-buffer nil
  "Used to track buffers switching.
So we can run the code when switching from chat buffer.")
(defvar telega--stickersets nil
  "Alist of seen sticker sets.
ID -> sticker set.
Take into account that ID is the string.")
(defvar telega--stickersets-installed-ids nil
  "List of ids for installed sticker sets.
Used by `telega-stickerset-installed-p'.")
(defvar telega--stickersets-installed nil
  "List of `stickerSetInfo' for installed sticker sets.")
(defvar telega--stickersets-trending nil
  "List of trending sticker sets info.")
(defvar telega--stickersets-trending-premium nil
  "List of trending Premium sticker sets info.")
(defvar telega--stickersets-system nil
  "List of system sticker sets, such as animated dices, animated emojis.")
(defvar telega--stickersets-custom-emojis nil
  "List of custom emojis sticker sets info.")
(defvar telega--stickers-favorite nil
  "List of favorite stickers.")
(defvar telega--stickers-recent nil
  "List of recently used stickers.")
(defvar telega--stickers-recent-attached nil
  "List of recently attached stickers.")
(defvar telega--animated-emojis nil
  "List of all supported animated emojis.")
(defvar telega--animated-emojis-stickerset-id nil
  "Id for sticker set with animated emojis.")
(defvar telega--custom-emoji-stickers nil
  "Hash of custom_emoji_id -> sticker for the custom emojis.")

(defvar telega--animations-saved nil
  "List of saved animations.")
(defvar telega--chat-themes nil
  "List of chat themes.")

(defvar telega--dice-emojis nil
  "List of supported emojis for random dice messages.")
(defvar telega--suggested-actions nil
  "List of suggested actions to be taken.")

(defvar telega--group-calls nil "Hash table (id -> group-call).")

(defvar telega--favorite-messages-storage-message 'not-yet-fetched
  "Message where favorite messages are stored.
Document message with the #telega_favorite_messages hashtag.")
(defvar telega--favorite-messages nil
  "List of favorite messages.
Favorite message is a plist with at least `:chat_id', `:id' properties.
`:timestamp' and `:comment' properties are also supported.")

;; Searching
(defvar telega-search-history nil
  "List of recent search queries.")
(defvar telega--search-chats nil
  "Result of last `telega--searchChats' or `telega--searchChatsOnServer'.")
(defvar telega--nearby-chats nil
  "List of nearby chats returned from `telega--searchChatsNearby'.")

(defvar telega--unread-message-count nil
  "Plist with counts for unread/unmuted messages.
Props are `:unread_count' and `:unread_unmuted_count'")
(defvar telega--unread-chat-count nil
  "Plist with counts for unread/unmuted chats.
Props are `:unread_count', `:unread_unmuted_count', `:marked_as_unread_count'
and `:marked_as_unread_unmuted_count'")

(defvar telega--chat-buffers-alist nil
  "Alist of chats and corresponding chatbuf.")
(defun telega-chat-buffers ()
  "Return list of all chatbufs."
  (mapcar #'cdr telega--chat-buffers-alist))

(defun telega-chat-buffers-manage (&optional for-new-chatbuf)
  "Keep number of chat buffers within `telega-chat-buffers-limit'.
If FOR-NEW-CHATBUF is specified, then do not kill this chatbuf
whatever conditions are."
  ;; NOTE: never kill newly created FOR-NEW-CHATBUF chat buffer
  (let* ((chat-buffers (delq for-new-chatbuf (telega-chat-buffers)))
         (nbuffers-to-kill (- (length chat-buffers) telega-chat-buffers-limit)))
    (when (> nbuffers-to-kill 0)
      (let* ((all-buffers (buffer-list))
             (buffers (sort chat-buffers
                            (lambda (buf1 buf2)
                              (< (length (memq buf1 all-buffers))
                                 (length (memq buf2 all-buffers)))))))
        (while (and buffers (> nbuffers-to-kill 0))
          ;; NOTE: kill only invisible (not having a window) chat
          ;; buffers
          (unless (get-buffer-window (car buffers) t)
            (telega-debug "Killing least recent %S" (car buffers))
            (cl-decf nbuffers-to-kill)
            (kill-buffer (car buffers)))
          (setq buffers (cdr buffers)))
        ))))

(defvar telega--files nil
  "Files hash FILE-ID -> (list FILE UPDATE-CALBACKS..).")
(defvar telega--files-updates nil
  "Hash of FILE-ID -> (list-of (UPDATE-CB CB-ARGS))
UPDATE-CB is callback to call when file updates.
UPDATE-CB is called with FILE and CB-ARGS as arguments.
UPDATE-CB should return non-nil to be removed after its being called.")
(defvar telega--proxy-pings nil
  "Alist for the proxy pings.
In form (PROXY-ID . TIMESTAMP SECONDS)")
(defvar telega-voip--alist nil
  "Alist of all calls currently in processing.
In form (ID . CALL)")
(defvar telega-voip--active-call nil
  "Currently active call.
Active call is either outgoing call or accepted incoming call.
Only one call can be currently active.")
(defvar telega--scope-notification-alist (cons nil nil)
  "Default notification settings for chats.
alist where key is one of:
\"notificationSettingsScopePrivateChats\",
\"notificationSettingsScopeGroupChats\",
\"notificationSettingsScopeChannelChats\".")

(defvar telega-tdlib--chat-folders nil
  "List of chat folders received from TDLib.")
(defvar telega-tdlib--chat-list nil
  "Active tdlib chat list used for ordering.")
(defvar telega-tdlib--unix-time nil
  "Plist holding remote/local unix times.
Used for adjustments for timing info received from Telegram.")

;; Minibuffer stuff used by chatbuf and stickers
(defvar telega-minibuffer--choices nil
  "Bind to list of choices.
Each element in form: (NAME SSET-ID)")
(defvar telega-minibuffer--chat nil
  "Bind to chat currently active.")
(defvar telega-minibuffer--string nil
  "Bind to Saved string entered to minibuffer.")

(defvar telega--ignored-messages-ring (make-ring 0)
  "Ring of ignored messages.
Use \\[execute-extended-command] telega-ignored-messages RET to
display the list.")

(defvar telega-docker--container-id nil
  "Docker image id currently running.")

;; See https://github.com/tdlib/td/issues/1645
(defvar telega--relogin-with-phone-number nil
  "This var is used to relogin with phone number when skipping QR auth")

(defvar telega--recent-inline-bots nil
  "List of usernames for recently used inline bots.")

(defvar telega--notification-messages-ring (make-ring 1)
  "Ring of messages triggered notification.
Use \\[execute-extended-command] telega-notifications-history RET to
display the list.")

(defvar telega-topic--default-icons nil
  "Cached list of topic icons which can be used by all users.")

(defvar telega--default-face 'default
  "Bind this to alter size calculation for the images.")

(defvar telega--accent-colors-alist nil
  "Alist id -> accent-color received by `updateAccentColors' event.")
(defvar telega--accent-colors-available-ids nil
  "Accent colors received by `updateAccentColors' event.")


;;; Shared chat buffer local variables
(defvar telega-chatbuf--chat nil
  "Telega chat for the current chat buffer.")
(make-variable-buffer-local 'telega-chatbuf--chat)

(defun telega-chatbuf--chat (buffer)
  "Return chat corresponding chat BUFFER."
  (buffer-local-value 'telega-chatbuf--chat buffer))

(defvar telega-chatbuf--marked-messages nil
  "List of marked messages.")
(make-variable-buffer-local 'telega-chatbuf--marked-messages)

(defvar telega-chatbuf--marked-messages-1 nil
  "List of previously marked messages.")
(make-variable-buffer-local 'telega-chatbuf--marked-messages-1)

(defvar telega-chatbuf--inline-query nil
  "Non-nil if some inline bot has been requested.
Actual value is `:@extra` value of the call to inline bot.")
(make-variable-buffer-local 'telega-chatbuf--inline-query)

(defvar telega-chatbuf--input-marker nil)
(make-variable-buffer-local 'telega-chatbuf--input-marker)

(defvar telega-chatbuf--administrators nil
  "List of administrators in chatbuf chat.
Asynchronously loaded when chatbuf is created.")
(make-variable-buffer-local 'telega-chatbuf--administrators)

(defvar telega-chatbuf--hidden-headers
  '(:active-stories nil :pinned-stories nil :video-chat nil)
  "Plist to check whether some header is hidden by pressing [x] button.")
(make-variable-buffer-local 'telega-chatbuf--hidden-headers)

(defvar telega-chatbuf--group-call-users nil
  "List of group call participants.")
(make-variable-buffer-local 'telega-chatbuf--group-call-users)

(defvar telega-chatbuf--fetch-alist nil
  "Alist of async requests (fetches) to the telega-server.
Could be used for fetching `admins', `pinned-messages', `reply-markup', etc.")
(make-variable-buffer-local 'telega-chatbuf--fetch-alist)

(defvar telega-chatbuf--bot-start-parameter nil
  "Parameter to pass to `telega--sendBotStartMessage' when START is pressed.")
(make-variable-buffer-local 'telega-chatbuf--bot-start-parameter)

(defvar telega-chatbuf-language-code nil
  "A two-letter ISO 639-1 language code for the chat's language.")
(make-variable-buffer-local 'telega-chatbuf-language-code)

(defvar telega-chatbuf--focus-status nil
  "Last focus status, retrieved with `(telega-focus-state)'.")
(make-variable-buffer-local 'telega-chatbuf--focus-status)

(defvar telega-chatbuf--focus-debounce-timer nil
  "Timer for debouncing focus status.")
(make-variable-buffer-local 'telega-chatbuf--focus-debounce-timer)

(defvar telega-chatbuf--history-state-plist nil
  "Plist containing different state of history loading.
Could contain `:loading', `:older-loaded', `:newer-freezed' or
`:newer-loaded' elements.")
(make-variable-buffer-local 'telega-chatbuf--history-state-plist)

(defvar telega-chatbuf--thread nil
  "Thread currently active in the chatbuf.
Thread is either TL `forumTopic' or `message' starting a thread.")
(make-variable-buffer-local 'telega-chatbuf--thread)

(defun telega-chatbuf--thread-msg ()
  "Return chatbuf's thread as thread's root message."
  (when (telega-msg-p telega-chatbuf--thread)
    telega-chatbuf--thread))

(defun telega-chatbuf--thread-info ()
  (plist-get (telega-chatbuf--thread-msg) :telega-thread-info))

(defun telega-chatbuf--thread-topic ()
  "Return chatbuf's thread as topic."
  (unless (telega-chatbuf--thread-msg)
    ;; Must be topic or nil at this point
    telega-chatbuf--thread))

(defun telega-chatbuf--message-thread-id ()
  "Return message thread id for the chatbuf.
To be used in various TDLib methods as `:message_thread_id` argument."
  (or (when-let ((topic (telega-chatbuf--thread-topic)))
        (telega-topic-msg-thread-id topic))
      (when-let ((thread (telega-chatbuf--thread-msg)))
        (plist-get telega-chatbuf--thread :message_thread_id))
      0))

(defvar telega-chatbuf--aux-plist nil
  "Supplimentary plist for aux prompt.")
(make-variable-buffer-local 'telega-chatbuf--aux-plist)


(defun telega--init-vars ()
  "Initialize runtime variables.
Done when telega server is ready to receive queries."
  (setq telega--auth-state nil)
  (setq telega--conn-state nil)
  (setq telega--status "Disconnected")
  (setq telega--status-aux "")
  (setq telega--me-id -1)
  (setq telega--replies-id nil)
  (setq telega--options
        ;; default limits
        (list :message_caption_length_max 1024
              :message_text_length_max 4096))
  (setq telega--chats (make-hash-table :test #'eq))
  (setq telega--chat-topics (make-hash-table :test #'eq))
  (setq telega--chat-active-stories (make-hash-table :test #'eq))
  (setq telega--cached-messages (make-hash-table :test #'equal))
  (setq telega--cached-stories (make-hash-table :test #'equal))
  (setq telega--top-chats nil)

  (setq telega--search-chats nil)
  (setq telega--nearby-chats nil)

  (setq telega-deleted-chats nil)
  (setq telega--blocked-user-ids-alist
        (list (cons 'blockListMain (list 0))
              (cons 'blockListStories (list 0))))
  (setq telega--ordered-chats nil)
  (setq telega--filtered-chats nil)
  (setq telega--dirty-chats nil)
  (setq telega--actions (make-hash-table :test 'equal))
  (setq telega--filters nil)
  (setq telega--undo-filters nil)
  (setq telega--sort-criteria nil)
  (setq telega--sort-inverted nil)
  (setq telega--sort-reorder-dirtiness nil)
  (setq telega--info
        (list (cons 'user (make-hash-table :test 'eq))
              (cons 'secretChat (make-hash-table :test 'eq))
              (cons 'basicGroup (make-hash-table :test 'eq))
              (cons 'supergroup (make-hash-table :test 'eq))))
  (setq telega--full-info
        (list (cons 'user (make-hash-table :test 'eq))
              (cons 'basicGroup (make-hash-table :test 'eq))
              (cons 'supergroup (make-hash-table :test 'eq))))

  (setq telega--ignored-messages-ring
        (make-ring telega-ignored-messages-ring-size))
  (setq telega--unread-message-count nil)
  (setq telega--unread-chat-count nil)
  (setq telega--story-list-chat-count nil)

  (setq telega--files (make-hash-table :test 'eq))
  (setq telega--files-updates (make-hash-table :test 'eq))

  (setq telega-voip--alist nil)
  (setq telega-voip--active-call nil)

  (setq telega--proxy-pings nil)
  (setq telega--scope-notification-alist nil)

  (setq telega--stickersets nil)
  (setq telega--stickersets-installed-ids nil)
  (setq telega--stickersets-installed nil)
  (setq telega--stickersets-trending nil)
  (setq telega--stickersets-trending-premium nil)
  (setq telega--stickersets-system nil)
  (setq telega--stickersets-custom-emojis nil)
  (setq telega--stickers-favorite nil)
  (setq telega--stickers-recent nil)
  (setq telega--stickers-recent-attached nil)
  (setq telega--animated-emojis nil)
  (setq telega--animated-emojis-stickerset-id nil)
  (setq telega--custom-emoji-stickers
        (make-hash-table :size 200 :test 'equal))
  (setq telega--animations-saved nil)
  (setq telega--chat-themes nil)
  (setq telega--dice-emojis nil)

  (setq telega-tdlib--chat-folders nil)
  (setq telega-tdlib--chat-list nil)
  (setq telega-tdlib--unix-time nil)

  (setq telega--group-calls (make-hash-table :test 'eq))
  (setq telega-docker--container-id nil)
  (setq telega--recent-inline-bots nil)

  (setq telega--favorite-messages-storage-message 'not-yet-fetched)
  (setq telega--favorite-messages nil)

  (setq telega--notification-messages-ring
        (make-ring telega-notifications-history-ring-size))

  (setq telega--accent-colors-alist nil
        telega--accent-colors-available-ids nil)
  )

(defun telega-test-env (&optional quiet-p)
  "Test Emacs environment.
If QUIET-P is non-nil, then show success message in echo area.
Return non-nil if all tests are passed."
  (interactive "P")
  ;; 62bits for numbers is required
  ;; i.e. ./configure --with-wide-int
  (cl-assert (= most-positive-fixnum 2305843009213693951) nil
             "Emacs with wide ints (--with-wide-int) is required")
  (cl-assert (= (string-to-number "542353335") 542353335) nil
             (concat "Emacs with `(string-to-number \"542353335\") ==> 542353335'"
                     " is required"))

  ;; at least 25.1 emacs is required
  ;; see https://t.me/emacs_telega/1592
  (cl-assert (fboundp 'cursor-intangible-mode) nil
             "Emacs with `cursor-intangible-mode' is required")

  ;; For now stick with at least 27.1 Emacs
  (cl-assert (string-version-lessp "27.0" emacs-version) nil
             (format "At least Emacs 27.0 is required, but you have %s"
                     emacs-version))

  ;; imagemagick for images NOT required, we have now fallback in case
  ;; native image transforms available (newer Emacs)
  (cl-assert (or (image-type-available-p 'imagemagick)
                 (if (telega-x-frame)
                     (and (fboundp 'image-transforms-p)
                          (funcall 'image-transforms-p))
                   ;; For TTY-only emacs, images are not required
                   t))
             nil
             (concat "Emacs with `imagemagick' support is required."
                     " (libmagickcore, libmagickwand, --with-imagemagick)"))
  ;; SVG is no longer required if avatars are disabled (in TTY for example)
  (cl-assert (or (image-type-available-p 'svg)
                 (and (not telega-root-show-avatars)
                      (not telega-user-show-avatars)
                      (not telega-chat-show-avatars)))
             nil
             (concat "Emacs with `svg' support is needed to show avatars.  "
                     "Disable `telega-XXX-show-avatars' or recompile Emacs with svg support"))
  (unless quiet-p
    (message "Your Emacs is suitable to run telega.el"))
  t)

(defmacro telega-save-window-start (start end &rest body)
  "Execute BODY saving window start and point.
Window start is saved only if window start is inbetween START and
END."
  (declare (indent 2))
  (let ((buf-win-sym (gensym))
        (win-start (gensym "winstart"))
        (win-start-line (gensym)))
    `(let* ((,buf-win-sym (get-buffer-window))
            (,win-start (when ,buf-win-sym
                          (window-start ,buf-win-sym)))
            (,win-start-line (when (and ,buf-win-sym
                                        (>= ,win-start ,start)
                                        (<= ,win-start ,end))
                               (1+ (count-lines 1 ,win-start)))))
       (unwind-protect
           (progn ,@body)
         (when (and ,buf-win-sym ,win-start-line)
           (save-excursion
             (goto-char (point-min))
             (forward-line (1- ,win-start-line))
             (set-window-start ,buf-win-sym (point) 'noforce)))))))

(defmacro telega-save-excursion (&rest body)
  "Execute BODY saving current point as moving marker."
  (declare (indent 0))
  (let ((pnt-sym (gensym)))
    `(let* ((,pnt-sym (copy-marker (point) t)))
       (unwind-protect
           (progn ,@body)
         (goto-char ,pnt-sym)))))

(defmacro telega-save-cursor (&rest body)
  "Execute BODY saving cursor's line and column position."
  (declare (indent 0))
  (let ((line-sym (gensym "line"))
        (col-sym (gensym "col")))
    `(let ((,line-sym (+ (if (bolp) 1 0) (count-lines 1 (point))))
           (,col-sym (current-column)))
       (unwind-protect
           (progn ,@body)
         (goto-char (point-min))
         (cl-assert (> ,line-sym 0))
         (forward-line (1- ,line-sym))
         (move-to-column ,col-sym)))))

(defmacro with-telega-debug-buffer (&rest body)
  "Execute BODY only if `telega-debug' is non-nil, making debug buffer current."
  `(when telega-debug
     (with-current-buffer (get-buffer-create "*telega-debug*")
       (telega-save-excursion
         ,@body))))

(defmacro with-telega-buffer-modify (&rest body)
  "Run BODY inhibiting `buffer-read-only' variable."
  `(with-silent-modifications
     ,@body))

(defmacro with-telega-root-buffer (&rest body)
  "Execute BODY setting current buffer to root buffer.
Inhibits read-only flag."
  (declare (indent 0))
  `(when (buffer-live-p (telega-root--buffer))
     (with-current-buffer telega-root-buffer-name
       (let ((inhibit-read-only t))
         (unwind-protect
             (progn ,@body)
           (set-buffer-modified-p nil))))))

(defmacro with-telega-chatbuf (chat &rest body)
  "Execute BODY setting current buffer to chat buffer of CHAT.
Executes BODY only if chat buffer already exists.
If there is no corresponding buffer, then do nothing.
Inhibits read-only flag."
  (declare (indent 1))
  (let ((bufsym (cl-gensym "buf"))
        (chatsym (cl-gensym "chat")))
    `(let* ((,chatsym ,chat)
            (,bufsym (if (and telega-chatbuf--chat
                              (eq telega-chatbuf--chat ,chatsym))
                         (current-buffer)
                       (cdr (assq ,chatsym telega--chat-buffers-alist)))))
       (when (buffer-live-p ,bufsym)
         (with-current-buffer ,bufsym
           (with-telega-buffer-modify
            ,@body))))))

(defmacro with-telega-help-win (buffer-or-name &rest body)
  "Execute BODY in help BUFFER-OR-NAME."
  (declare (indent 1))
  `(progn
     (with-help-window ,buffer-or-name)
     (redisplay)
     (with-help-window ,buffer-or-name
       (set-buffer standard-output)
       (setq-local nobreak-char-display nil)
       (cursor-sensor-mode 1)
       ,@body)))

(defun telega-help-win--maybe-redisplay (buffer-or-name for-param)
  "Possible redisplay help win with BUFFER-OR-NAME.
If BUFFER-OR-NAME exists and visible then redisplay it."
  (when-let ((help-buf (get-buffer buffer-or-name)))
    (with-current-buffer help-buf
      (when (and (eq for-param telega--help-win-param)
                 telega--help-win-inserter)
        (if (get-buffer-window help-buf)
            ;; Buffer is visible in some HELP-WIN
            (telega-save-window-start (point-min) (point-max)
              (telega-save-cursor
                (let ((inhibit-read-only t))
                  (setq telega--help-win-dirty-p nil)
                  (erase-buffer)
                  (funcall telega--help-win-inserter
                           telega--help-win-param))))

          ;; Buffer is not visible, mark it as dirty, so it will be
          ;; redisplayed when switched in
          (setq telega--help-win-dirty-p t))))))

(defmacro telega-help-message (help-name fmt &rest fmt-args)
  "Show once help message formatted with FMT and FMT-ARGS.
Show message only if `telega-help-messages' is non-nil."
  (declare (indent 2))
  `(when (and telega-help-messages
              (not (get 'telega-help-messages ,help-name)))
     (put 'telega-help-messages ,help-name t)
     (message (concat "Telega: " ,fmt) ,@fmt-args)))

(defsubst telega-debug (fmt &rest args)
  "Insert formatted string into debug buffer.
FMT and ARGS are passed directly to `format'."
  (with-telega-debug-buffer
   (goto-char (point-max))
   (insert (apply 'format (cons (concat "%d: " fmt "\n")
                                (cons (telega-time-seconds) args))))))

(defmacro telega--tl-type (tl-obj)
  `(intern (plist-get ,tl-obj :@type)))

(defmacro telega--tl-error-p (tl-obj)
  "Return non-nil if TL-OBJ is error object."
  `(eq (telega--tl-type ,tl-obj) 'error))

(defmacro telega--tl-get (obj prop1 &rest props)
  "`plist-get' which works with multiple arguments.
For example:
`(telega--tl-get obj :prop1 :prop2)' is equivalent to
`(plist-get (plist-get obj :prop1) :prop2)`"
  (let ((ret `(plist-get ,obj ,prop1)))
    (dolist (prop props)
      (setq ret (list 'plist-get ret prop)))
    ret))

(defmacro telega--tl-prop (prop1 &rest props)
  "Generate function to get property by PROP1 and PROPS.
Uses `telega--tl-get' to obtain the property."
  (let ((tl-obj-sym (cl-gensym "tl-obj")))
    `(lambda (,tl-obj-sym)
       (telega--tl-get ,tl-obj-sym ,prop1 ,@props))))

(defmacro telega--tl-dolist (bind &rest body)
  "Execute BODY by traversing plist.
BIND is in form ((PROP VAL) PLIST)."
  (declare (indent 1))
  `(cl-loop for ,(car bind) on ,(cadr bind)
            by #'cddr
            do (progn ,@body)))

(defsubst telega-file--size (file)
  "Return FILE size."
  ;; NOTE: fsize is 0 if unknown, in this case esize is approximate
  ;; size
  (let ((fsize (plist-get file :size))
        (esize (plist-get file :expected_size)))
    (if (zerop fsize) esize fsize)))

(defsubst telega-file--downloaded-p (file)
  "Return non-nil if FILE has been downloaded."
  (and (telega--tl-get file :local :is_downloading_completed)
       (file-exists-p (telega--tl-get file :local :path))))

(defsubst telega-file--downloading-p (file)
  "Return non-nil if FILE is downloading right now."
  (telega--tl-get file :local :is_downloading_active))

(defsubst telega-file--can-download-p (file)
  "Return non-nil if FILE can be downloaded.
May return nil even when `telega-file--downloaded-p' returns non-nil."
  (telega--tl-get file :local :can_be_downloaded))

(defsubst telega-file--need-download-p (file)
  (and (telega-file--can-download-p file)
       (not (telega-file--downloaded-p file))))

(defsubst telega-file--downloaded-size (file)
  (telega--tl-get file :local :downloaded_size))

(defsubst telega-file--downloading-progress (file)
  "Return progress of FILE downloading as float from 0 to 1."
  (let ((fsize (telega-file--size file)))
    (if (zerop fsize)
        0
      (color-clamp (/ (float (telega-file--downloaded-size file)) fsize)))))

(defun telega-file--partially-downloaded-p (file)
  "Return non-nil if FILE is partially downloaded."
  (and (not (telega-file--downloading-p file))
       (< 0 (telega-file--downloading-progress file) 1)))

(defsubst telega-file--uploaded-p (file)
  "Return non-nil if FILE has been uploaded."
  (telega--tl-get file :remote :is_uploading_completed))

(defsubst telega-file--uploading-p (file)
  "Return non-nil if FILE is uploading right now."
  (telega--tl-get file :remote :is_uploading_active))

(defsubst telega-file--uploading-progress (file)
  "Return progress of FILE uploading as float from 0 to 1."
  (color-clamp (/ (float (telega--tl-get file :remote :uploaded_size))
                  (telega-file--size file))))

(defun telega-file--partially-uploaded-p (file)
  "Return non-nil if FILE is partially uploaded."
  (and (not (telega-file--uploading-p file))
       (< 0 (telega-file--uploading-progress file) 1)))

(defvar telega-inhibit-telega-display-by nil
  "Bind it to non-nil to inhibit `telega-display' property for non-emoji parts.
Can be a list of symbols if you need to inhibit particular transforms.
Use it to get/copy text ommiting text modifications from plugins, such
at `telega-url-shorten'.")

(defsubst telega--desurrogate-apply-part (part &optional keep-properties)
  "Apply PART's `telega-display'"
  (let ((part-display (get-text-property 0 'telega-display part)))
    (cond (part-display
           (if keep-properties
               ;; keep all properties except for `telega-display'
               ;; Apply `telega-emoji-p' property as well
               (let* ((part-props0 (telega-plist-del
                                    (text-properties-at 0 part) 'telega-display))
                      (emoji-p (plist-get part-props0 'telega-emoji-p))
                      (part-props1 (telega-plist-del part-props0 'telega-emoji-p))
                      ;; NOTE: we always create new cell for 'display
                      ;; property as in `image-insert', see comment
                      ;; about this in `image-insert' sources
                      (addon-props (when (and emoji-p
                                              telega-use-images
                                              telega-emoji-use-images)
                                     (list 'rear-nonsticky '(display)
                                           'display (cons 'image (cdr (telega-emoji-create-svg part-display)))))))
                 ;; Inhibit `telega-display' if created by one of the
                 ;; plugins listen in the
                 ;; `telega-inhibit-telega-display-by'
                 (when (and telega-inhibit-telega-display-by
                            (if (listp telega-inhibit-telega-display-by)
                                (memq (plist-get part-props1 'telega-display-by)
                                      telega-inhibit-telega-display-by)
                              (plist-get part-props1 'telega-display-by)))
                   (setq part-props1 (telega-plist-del
                                      part-props1 'telega-display-by))
                   (setq part-display part))

                 (apply 'propertize part-display (nconc part-props1 addon-props)))
             part-display))
          (keep-properties part)
          (t (substring-no-properties part)))))

(defsubst telega--desurrogate-apply-part-keep-properties (part)
  (telega--desurrogate-apply-part part 'keep-props))

(defsubst telega--desurrogate-apply (str &optional no-properties)
  "Apply `telega-display' properties to STR.
Resulting in new string with no surrogate pairs.
If NO-PROPERTIES is specified, then do not keep text properties."
  (mapconcat (if no-properties
                 #'telega--desurrogate-apply-part
               #'telega--desurrogate-apply-part-keep-properties)
             (telega--split-by-text-prop str 'telega-display) ""))

(defsubst telega--tl-unpack (obj)
  "Unpack TL object OBJ."
  obj)

(defsubst telega--tl-pack (obj)
  "Pack object OBJ."
  ;; Remove text props from strings, etc
  (cond ((stringp obj) (substring-no-properties obj))
        ((vectorp obj) (cl-map 'vector #'telega--tl-pack obj))
        ((listp obj) (mapcar #'telega--tl-pack obj))
        (t obj)))

(defun telega-tl-str (obj prop &optional no-properties)
  "Get property PROP from OBJ, desurrogating resulting string.
NO-PROPERTIES is passed directly to `telega--desurrogate-apply'.
Return nil for empty strings.
Also supports \"formattedText\" a value of the OBJ's PROP."
  (let* ((prop-val (plist-get obj prop))
         (text (if (and (listp prop-val)
                        (equal (plist-get prop-val :@type) "formattedText"))
                   (telega--fmt-text-faces prop-val)
                 prop-val))
         (ret (telega--desurrogate-apply text no-properties)))
    (unless (string-empty-p ret)
      ret)))

(defun telega--tl-json-object (tl-obj)
  "Extract json object from from TL-OBJ object."
  (cl-ecase (telega--tl-type tl-obj)
    (jsonValueObject
     ;; json object as alist
     (mapcar (lambda (obj)
               (cl-assert (telega--tl-type obj) 'jsonObjectMember)
               (cons (telega-tl-str obj :key)
                     (telega--tl-json-object (plist-get obj :value))))
             (plist-get tl-obj :members)))
    (jsonValueArray
     (mapcar #'telega--tl-json-object (plist-get tl-obj :values)))
    (jsonValueNumber
     (plist-get tl-obj :value))
    (jsonValueString
     (telega-tl-str tl-obj :value))
    (jsonValueBoolean
     (plist-get tl-obj :value))
    (jsonValueNull
     nil)
    ))

(defsubst telega-zerop (value)
  "Return non-nil if VALUE is nil or `zerop'."
  (or (null value) (zerop value)))

(defsubst telega-replies-p (chat)
  "Return non-nil if CHAT is Replies chat."
  (eq telega--replies-id (plist-get chat :id)))

(defsubst telega-me-p (msg-sender)
  "Return non-nil if MSG-SENDER is me.
MSG-SENDER could be a user or a chat."
  (eq telega--me-id (plist-get msg-sender :id)))

(defsubst telega-chat-p (msg-sender)
  "Return non-nil if CHAT is Telegram chat."
  (and msg-sender (eq 'chat (telega--tl-type msg-sender))))

(defsubst telega-user-p (msg-sender)
  "Return non-nil if USER is Telegram user."
  (and msg-sender (eq 'user (telega--tl-type msg-sender))))


;; Matching
(defvar telega-temex-match-prefix nil)
(declare-function telega-match-p "telega-match" (object temex))

(defun telega-chat-match-p (chat temex)
  "Return non-nil if CHAT matches TEMEX."
  (declare (indent 1))
  (let ((telega-temex-match-prefix 'chat))
    (telega-match-p chat temex)))

(defun telega-chatbuf-match-p (chat-temex)
  "Return non-nil if chatbuf matches CHAT-TEMEX."
  (cl-assert telega-chatbuf--chat)
  (telega-chat-match-p telega-chatbuf--chat chat-temex))

(defun telega-user-match-p (user temex)
  "Return non-nil if USER matches TEMEX."
  (declare (indent 1))
  (let ((telega-temex-match-prefix 'user))
    (telega-match-p user temex)))

(defun telega-msg-match-p (msg temex)
  "Return non-nil if message MSG matches TEMEX."
  (declare (indent 1))
  (let ((telega-temex-match-prefix 'msg))
    (telega-match-p msg temex)))

(defun telega-sender-match-p (sender temex)
  "Return non-nil if message SENDER matches TEMEX."
  (declare (indent 1))
  (let ((telega-temex-match-prefix 'sender))
    (telega-match-p sender temex)))

(defun telega-topic-match-p (topic temex)
  "Return non-nil if TOPIC matches TEMEX."
  (declare (indent 1))
  (let ((telega-temex-match-prefix 'topic))
    (telega-match-p topic temex)))

(defun telega-story-match-p (story temex)
  "Return non-nil if STORY matches TEMEX."
  (declare (indent 1))
  (let ((telega-temex-match-prefix 'story))
    (telega-match-p story temex)))

(defun telega-match-gen-predicate (prefix temex)
  "Return predicate function to match TDLib object against TEMEX.
PREFIX is one of: `msg', `chat', `user' or `sender'."
  (declare (indent 1))
  ;; NOTE: Backward compatibily with "msg-" as prefix
  (when (stringp prefix)
    (setq prefix (intern (substring prefix 0 -1))))

  (lambda (obj)
    (let ((telega-temex-match-prefix prefix))
      (telega-match-p obj temex))))


;;; Formatting
(defun telega-fmt-eval-fill (estr attrs)
  "Fill ESTR to `:fill-column' value from ATTRS.
Keeps newlines in ESTR.
Return list of strings."
  (let ((fill-column (- (or (plist-get attrs :fill-column) fill-column)
                        (length (plist-get attrs :fill-prefix)))))
    (apply #'nconc
           (mapcar (if (plist-get attrs :fill)
                       (lambda (str)
                         (split-string
                          (with-temp-buffer
                            (insert str)
                            (fill-region (point-min) (point-max)
                                         (plist-get attrs :fill) t)
                            (buffer-substring (point-min) (point-max)))
                          "\n"))
                     #'list)
                   (split-string estr "\n")))))

(defun telega-fmt-eval-truncate (estr attrs)
  "Apply `:max', `:elide-string' and `:elide-trail' properties from ATTRS to ESTR."
  (let* ((max (plist-get attrs :max))
         ;; NOTE: always apply elide
;         (elide (plist-get attrs :elide))
         (elide-str (or (plist-get attrs :elide-string) telega-symbol-eliding))
         (elide-trail (or (plist-get attrs :elide-trail) 0))
         (estr-trail (if (> elide-trail 0) (substring estr (- elide-trail)) ""))
         (lead-width (- max (string-width elide-str) (string-width estr-trail)))
         (estr-lead (truncate-string-to-width estr lead-width)))
    ;; Correct `estr-lead' in case of multibyte chars, because
    ;; `truncate-string-to-width' does not always do its job right
    (while (and (not (string-empty-p estr-lead))
                (< lead-width (string-width estr-lead)))
      (setq estr-lead (substring estr-lead 0 -1)))
    (concat estr-lead elide-str estr-trail)))

(defun telega-fmt-eval-align (estr attrs)
  "Apply `:min', `:align' and `:align-symbol' properties from ATTRS to ESTR.
`:align-symbol' might be a symbol, in this case `telega-symbol' is
used to get a string for alignment."
  (let* ((min (plist-get attrs :min))
         (width (- min (string-width estr)))
         (align (plist-get attrs :align))
         (align-symbol (or (plist-get attrs :align-symbol) " "))
         (left "")
         (right ""))
    ;; Grow `left' and `right' until they have required width
    (while (< (string-width left) (/ width 2))
      (setq left (concat left (if (symbolp align-symbol)
                                  (telega-symbol align-symbol)
                                align-symbol))))
    (while (< (string-width right) (- width (/ width 2)))
      (setq right (concat right (if (symbolp align-symbol)
                                    (telega-symbol align-symbol)
                                  align-symbol))))

    (cl-ecase align
      (left (concat estr left right))
      (right (concat left right estr))
      ((center centre) (concat left estr right)))))

(defun telega-fmt-eval-min-max (estr attrs)
  "Apply `:min' and `:max' properties from ATTRS to ESTR."
  (let ((max (plist-get attrs :max))
        (min (plist-get attrs :min))
        (estr-width (string-width estr)))
    (cond ((and max (> estr-width max))
           (telega-fmt-eval-truncate estr attrs))
          ((and min (< estr-width min))
           (telega-fmt-eval-align estr attrs))
          (t estr))))

(defun telega-fmt-eval-face (estr attrs)
  "Apply `:face' attribute from ATTRS to ESTR."
  (let ((face (plist-get attrs :face)))
    (when face
      (add-face-text-property 0 (length estr) face t estr))
    estr))

(defun telega-fmt-eval-attrs (estr attrs)
  "Apply all attributes ATTRS to ESTR."
  ;; Blackmagic for fast execution, but
  ;; NOTE:
  ;;  - Do not prefix first line
  ;;  - Do not prefix empty lines with blank prefix
  ;;  - If last string is empty, do not prefix it
  (let* ((fpx (plist-get attrs :fill-prefix))
         (fpx-blank-p (or (not fpx) (string-blank-p fpx)))
         (filled-estrs (telega-fmt-eval-fill estr attrs))
         (formatted-estrs (list (telega-fmt-eval-min-max
                                 (pop filled-estrs) attrs)))
         (festr-tail formatted-estrs))
    (while filled-estrs
      (let* ((estr (car filled-estrs))
             (estr-last-p (not (cdr filled-estrs)))
             (festr-elem
              (list (telega-fmt-eval-min-max
                     (concat (unless (and (string-empty-p estr)
                                          (or estr-last-p fpx-blank-p))
                               fpx)
                             estr)
                     attrs))))
        (setcdr festr-tail festr-elem)
        (setq festr-tail festr-elem)
        (setq filled-estrs (cdr filled-estrs))))
    (telega-fmt-eval-face
     (mapconcat #'identity formatted-estrs "\n")
     attrs)))

(defsubst telega-fmt-atom (atom)
  "Convert ATOM to string.
NIL yields empty string for the convenience."
  (cond ((stringp atom) atom)
        ((null atom) "")
        (t (with-output-to-string
             (princ atom)))))

(defun telega-fmt-eval-elem (elem value)
  "Format single element ELEM with VALUE."
  (let (attrs)
    (when (and (not (functionp elem)) (listp elem))
      (setq attrs (cdr elem)
            elem (car elem)))

    (telega-fmt-eval-attrs
     (cond ((functionp elem)
            (telega-fmt-atom (funcall elem value)))
           ((symbolp elem)
            (telega-fmt-atom (symbol-value elem)))
           ((listp elem)
            (telega-fmt-eval elem value))
           (t (telega-fmt-atom elem)))
     attrs)))

(defun telega-fmt-eval (fmt-spec value)
  "Evaluate simple format FMT-SPEC, applying it to VALUE."
  (when (functionp fmt-spec)
    (setq fmt-spec (funcall fmt-spec value)))

  (let ((fmt-result (if (stringp fmt-spec) fmt-spec "")))
    (while (consp fmt-spec)
      (when (car fmt-spec)
        (setq fmt-result
              (concat fmt-result
                      (telega-fmt-eval-elem (car fmt-spec) value))))
      (setq fmt-spec (cdr fmt-spec)))
    fmt-result))

(defsubst telega--time-at00 (timestamp &optional decoded-ts)
  "Return time at 00:00:001 at TIMESTAMP's day.
Optional DECODED-TS is the result of already applied `decode-time'."
  (let ((dt (or decoded-ts (decode-time timestamp))))
    (1+ (- (floor timestamp) (* 3600 (nth 2 dt)) (* 60 (nth 1 dt)) (nth 0 dt)))))

;;; Buttons for telega
(defun telega-button--ins-error (_val)
  "Default inserter for the `telega' button."
  (error "Button `:inserter' is unset"))

;; Make 'telega-button be separate (from 'button) type
(put 'telega-button 'type 'telega)
(put 'telega-button 'keymap button-map)
(put 'telega-button 'action 'telega-button--action)
(put 'telega-button 'rear-nonsticky t)
(put 'telega-button 'face nil)
(put 'telega-button :inserter 'telega-button--ins-error)
(put 'telega-button :value nil)
(put 'telega 'button-category-symbol 'telega-button)

(defun telega-button--action (button)
  "Run BUTTON's `:action' function on its `:value'.
Return t if `:action' has been called."
  (when-let ((telega-action (button-get button :action)))
    (funcall telega-action (button-get button :value))
    t))

(defun telega-button--sensor-func (_window oldpos dir)
  "Function to be used in `cursor-sensor-functions' text property.
Activates button if cursor enter, deactivates if leaves."
  (let ((inhibit-read-only t)
        (button-region (telega--region-with-cursor-sensor
                        (if (eq dir 'entered) (point) oldpos))))
    (when button-region
      (put-text-property (car button-region) (cdr button-region)
                         'face (if (eq dir 'entered)
                                   'telega-button-active
                                 'telega-button))
      (when (eq dir 'entered)
        (telega-button--help-echo (car button-region)))
      )))

;; `:help-echo' is also available for buttons
(defun telega-button--help-echo (button)
  "Show help message for BUTTON defined by `:help-echo' property."
  (when telega-help-messages
    (let ((help-echo (or (button-get button :help-echo)
                         (button-get button 'help-echo))))
      (when (functionp help-echo)
        (setq help-echo (funcall help-echo (button-get button :value))))
      (when help-echo
        (message "%s" (eval help-echo))))))

(defun telega-button--insert (button-type value &rest props)
  "Insert telega button of BUTTON-TYPE with VALUE and PROPS.
Properties specified in PROPS are retained on
`telega-button--update-value' calls."
  (declare (indent 2))
  (let ((button (apply 'make-text-button
                       (prog1 (point)
                         (funcall (or (plist-get props :inserter)
                                      (button-type-get button-type :inserter))
                                  value))
                       (point)
                       :type button-type
                       :value value
                       :telega-retain-props
                       (telega-plist-map (lambda (prop _ignored) prop) props)
                       props)))
    (button-at button)))

(defmacro telega-button--change (button new-button)
  "Change BUTTON to NEW-BUTTON."
  (declare (indent 1))
  (let ((newbutton (gensym "newbutton")))
    `(let ((inhibit-read-only t))
       (goto-char (button-start ,button))
       (let ((,newbutton ,new-button))
         (delete-region (point) (button-end ,button))
         (set-marker ,button ,newbutton)))))

(defun telega-button--update-value (button new-value &rest props)
  "Update BUTTON's value to NEW-VALUE.
Additional properties PROPS are updated in button."
  ;; NOTE: retain values for properties specified in
  ;; `telega-button--insert', new PROPS overrides retained props
  (let ((new-props nil))
    (dolist (rprop (button-get button :telega-retain-props))
      (setq new-props (plist-put new-props rprop (button-get button rprop))))
    (telega--tl-dolist ((addprop value) props)
      (setq new-props (plist-put new-props addprop value)))

    (telega-button--change button
      (apply #'telega-button--insert (button-type button)
             new-value new-props))))

(defun telega-button--observable-p (button)
  "Return non-nil if BUTTON is observable in some window.
I.e. shown in some window, see `pos-visible-in-window-p'.

Return nil if button is not visible, `full' if BUTTON is fully
visible, `top' if top is visible and bottom is not, `bottom' if bottom
is visible and top is not, `button' if only BUTTON point is visible."
  ;; NOTE: BUTTON could be a real button (with value) or simple marker
  ;; for simple marker do not check top and bottom
  (cl-assert (markerp button))
  (when-let* ((bwin (get-buffer-window (marker-buffer button)))
              (frame-focused-p
               ;; NOTE: frame must be focused, so notifications will
               ;; popup if frame is out of focus and new message
               ;; arrives in the opened chatbuf. See
               ;; https://t.me/emacs_telega/36301
               (telega-focus-state (window-frame bwin))))
    (let* ((button-p (button-get button :value))
           (top-p (when button-p
                    (pos-visible-in-window-p (button-start button) bwin)))
           (bottom-p (when button-p
                       (pos-visible-in-window-p (button-end button) bwin))))
      (cond ((and top-p bottom-p) 'full)
            (top-p 'top)
            (bottom-p 'bottom)
            ((pos-visible-in-window-p button bwin) 'button)))))

(defun telega-button--make-observable (button &optional even-if-visible-p)
  "Make BUTTON observable in window."
  (when (markerp button)
    (when-let ((bstart (button-start button))
               (bend (button-end button))
               (bwin (get-buffer-window (marker-buffer button))))
      (unless (and (not even-if-visible-p)
                   (pos-visible-in-window-p bstart bwin)
                   (pos-visible-in-window-p bend bwin))
        (let ((win-h (window-height bwin))
              (button-h (count-lines bstart bend)))
          (if (>= button-h (/ win-h 2))
              ;; NOTE: Always keep at least 2 lines visible above the
              ;; message
              (telega-window-recenter bwin 2 bstart)
            (telega-window-recenter bwin)))))))

(defun telega-button-forward (n &optional predicate no-error)
  "Move forward to N visible/active button.
If PREDICATE is specified, then forward only buttons for which
PREDICATE returns non-nil.  PREDICATE is called with single arg -
button.
If NO-ERROR, do not signal error if no further buttons could be
found.
If NO-ERROR is `interactive', then do whatever `telega-button-forward'
do for interactive calls, i.e. observe the button and show help
message (if any)."
  (declare (indent 1))
  (interactive "p")
  (let (button)
    (dotimes (_ (abs n))
      ;; NOTE: In Emacs26, there is no `no-error' argument for
      ;; `forward-button', so we use `ignore-errors' instead
      ;; See: https://t.me/emacs_telega/11931
      (while (and (setq button (if no-error
                                   (ignore-errors (forward-button (cl-signum n)))
                                 (forward-button (cl-signum n))))
                  (or (and predicate
                           (not (funcall predicate button)))
                      (button-get button 'invisible)
                      (button-get button 'inactive)))))

    ;; NOTE: Non-nil `no-error' is normally given on non-interactive
    ;; calls, so make button observable only on interactive calls
    (when (and button (or (not no-error) (eq no-error 'interactive)))
      (telega-button--make-observable button)
      (telega-button--help-echo button))
    button))

(defun telega-button-backward (n &optional predicate no-error)
  "Move backward to N visible/active button.
PREDICATE and NO-ERROR are passed to `telega-button-forward'."
  (interactive "p")
  (telega-button-forward (- n) predicate no-error))


;;; Chats part
(defmacro telega-chat-uaprop-del (chat uaprop-name)
  "Delete custom CHAT property named UAPROP-NAME."
  `(telega-chat--set-uaprops
    ,chat (telega-plist-del (plist-get ,chat :uaprops) ,uaprop-name)))

(defmacro telega-chat-uaprop (chat uaprop-name)
  "Return value for CHAT's custom property with name UAPROP-NAME."
  `(plist-get (plist-get ,chat :uaprops) ,uaprop-name))

(gv-define-setter telega-chat-uaprop (value chat uaprop-name)
  "Set CHAT's user property UAPROP-NAME to VALUE.
Return VALUE."
  (let ((valsym (gensym "value")))
    `(let ((,valsym ,value))
       (telega-chat--set-uaprops
        ,chat (plist-put (plist-get ,chat :uaprops) ,uaprop-name ,valsym))
       ,valsym)))

(defun telega-chat-position--list-name (position &optional no-props)
  "Return list name for the POSITION.
If NO-PROPS is non-nil, then remove properties from the resulting string."
  (let* ((pos-list (plist-get position :list))
         (pos-list-type (plist-get pos-list :@type)))
    (cond ((string= "chatListMain" pos-list-type)
           'main)
          ((string= "chatListFolder" pos-list-type)
           (telega-tl-str (cl-find (plist-get pos-list :chat_folder_id)
                                   telega-tdlib--chat-folders
                                   :key (telega--tl-prop :id))
                          :title no-props))
          ((string= "chatListArchive" pos-list-type)
           'archive))))

(defsubst telega-chat-position (chat)
  "Return CHAT position in current `telega-tdlib--chat-list'."
  (cl-find telega-tdlib--chat-list (plist-get chat :positions)
           :key (telega--tl-prop :list)
           :test #'equal))

(defun telega-chat-at (&optional pos)
  "Return current chat at point."
  (let ((button (button-at (or pos (point)))))
    (when (and button (eq (button-type button) 'telega-chat))
      (button-get button :value))))

(defun telega-chat-order (chat &optional ignore-custom)
  "Return CHAT's order as string.
Order from `telega-tdlib--chat-list' position is used.
If CHAT has custom order, then return its custom order."
  (or (unless ignore-custom
        (telega-chat-uaprop chat :order))
      (plist-get (telega-chat-position chat) :order)
      "0"))

(defconst telega--chat-reorder-dirtiness
  '("updateNewChat"
    "updateChatPosition"
    "updateChatDraftMessage"
    "updateChatLastMessage"

    ;; Special dirtiness, so (telega-chat--update chat 'reorder) can
    ;; be used to force chat reordering
    reorder
    )
  "List of dirtiness events when chat needs to update its order.")

(defun telega-chat-order-dirty-p (chat)
  "Return non-nil if CHAT's order is dirty, i.e. CHAT need to be reordered."
  (let ((dirtiness (plist-get chat :telega-dirtiness)))
    (or (cl-intersection telega--chat-reorder-dirtiness dirtiness
                         :test #'equal)
        (cl-intersection telega--sort-reorder-dirtiness dirtiness
                         :test #'equal))))

(defsubst telega-chat> (chat1 chat2)
  "Compare CHAT1 with CHAT2 according to `telega--sort-criteria'.
Return if CHAT1 is greater than CHAT2."
  (telega-chats-compare telega--sort-criteria
                        (if telega--sort-inverted chat2 chat1)
                        (if telega--sort-inverted chat1 chat2)))

(defsubst telega-chatbuf-has-input-p ()
  "Return non-nil if chatbuf has some input."
  (< telega-chatbuf--input-marker (point-max)))

(defsubst telega-chatbuf-input-string ()
  "Return non-nil if chatbuf has some input."
  (buffer-substring telega-chatbuf--input-marker (point-max)))

(defsubst telega-chatbuf--input-delete ()
  "Delete chatbuf's input."
  (delete-region telega-chatbuf--input-marker (point-max)))

(defsubst telega-chatbuf--input-draft-p ()
  "Return non-nil if chatbuf input is the draft.
Draft input is the input that have `:draft-input-p' property on both sides."
  (and (telega-chatbuf-has-input-p)
       (get-text-property telega-chatbuf--input-marker :draft-input-p)
       (get-text-property (1- (point-max)) :draft-input-p)))

(defmacro telega-chatbuf--history-state-get (state)
  "Check chatbuf's history state contains STATUS."
  `(plist-get telega-chatbuf--history-state-plist ,state))

(defmacro telega-chatbuf--history-state-set (state value)
  "Push STATUS into chatbuf's history state."
  `(setq telega-chatbuf--history-state-plist
         (plist-put telega-chatbuf--history-state-plist ,state ,value)))

(defmacro telega-chatbuf--history-state-delete (state)
  "Remove STATUS from chatbuf's history state."
  `(setq telega-chatbuf--history-state-plist
         (telega-plist-del telega-chatbuf--history-state-plist ,state)))

(defmacro telega-chat-nearby-find (chat-id)
  "Find nearby chat in `telega--nearby-chats' by CHAT-ID."
  `(cl-find ,chat-id telega--nearby-chats :key (telega--tl-prop :chat_id)))

(defun telega-chat-nearby--ensure (nearby-chat)
  "Ensure NEARBY-CHAT is in `telega--nearby-chats'."
  (let ((nb-chat (telega-chat-nearby-find (plist-get nearby-chat :chat_id))))
    (if nb-chat
        (plist-put nb-chat :distance (plist-get nearby-chat :distance))
      (setq nb-chat nearby-chat)
      (setq telega--nearby-chats (cons nearby-chat telega--nearby-chats)))
    nb-chat))

(defun telega-chat-nearby-distance (chat)
  "Return distance in meters to the CHAT.
Return non-nil only if CHAT is nearby."
  (plist-get (telega-chat-nearby-find (plist-get chat :id)) :distance))

;; Msg part
(defun telega-msg-id= (msg1 msg2)
  (= (plist-get msg1 :id) (plist-get msg2 :id)))

(defsubst telega-msg-cache (msg)
  "Put message MSG into messages cache `telega--cached-messages'."
  (puthash (cons (plist-get msg :chat_id) (plist-get msg :id)) msg
           telega--cached-messages))

(defun telega-msg-location-live-for (msg)
  "For live location message MSG return number of seconds it will last live.
Return nil if location message is not live.
Return list of two values - (LIVE-FOR UPDATED-AGO)."
  (let* ((content (plist-get msg :content))
         (live-period (plist-get content :live_period))
         (expires-in (plist-get content :expires_in)))
    (unless (or (zerop live-period) (zerop expires-in))
      (let ((current-ts (telega-time-seconds))
            (since (if (zerop (plist-get msg :edit_date))
                       (plist-get msg :date)
                     (plist-get msg :edit_date))))
        (list (- (+ since expires-in) current-ts)
              (- current-ts since))))))


;;; Inserters part
(defun telega-ins (&rest args)
  "Insert all strings in ARGS.
Return non-nil if something has been inserted."
  (< (prog1 (point) (apply #'insert (delq nil args))) (point)))

(defmacro telega-ins-fmt (fmt &rest args)
  "Insert string formatted by FMT and ARGS.
Return t."
  (declare (indent 1))
  `(telega-ins (format ,fmt ,@args)))

(defmacro telega-ins-i18n (key &rest args)
  (declare (indent 1))
  `(telega-ins (telega-i18n ,key ,@args)))

(defmacro telega-ins--as-string (&rest body)
  "Execute BODY inserters and return result as a string."
  ;; NOTE: Use current buffer as insert placeholder, so all buffer
  ;; local variables are kept.  If multibyte characters are disabled
  ;; in the buffer (e.g. image-mode) then create temp buffer with all
  ;; local vars having same values as in original buffer
  ;;
  ;; Note: Do not use `narrow-to-region'/`widen' because buffer can be
  ;; narrowed only once, but `telega-ins--as-string' might be called
  ;; in recursive manner
  (let ((point-sym (gensym "point"))
        (v-sym (gensym "vsym"))
        (lvars-sym (gensym "lvars")))
    `(if enable-multibyte-characters
         ;; Fast version.  ARGUABLE. this version degradates on large
         ;; buffers
         (let ((,point-sym (point)))
           (with-telega-buffer-modify
            (unwind-protect
                (progn
                  ;; Always start at the beginning of the line
                  (insert "\n")
                  ,@body
                  (buffer-substring (1+ ,point-sym) (point)))
              (delete-region ,point-sym (point)))))

       ;; Fallback to slow versing using temp buffer with local vars
       ;; kept
       (let ((,lvars-sym (buffer-local-variables)))
         (with-temp-buffer
           (dolist (,v-sym ,lvars-sym)
             (condition-case ()
                 (if (symbolp ,v-sym)
                     (makunbound (make-local-variable ,v-sym))
                   (set (make-local-variable (car ,v-sym)) (cdr ,v-sym)))
               ;; E.g. for enable-multibyte-characters.
               (setting-constant nil)))

           (set-buffer-multibyte t)
           (with-telega-buffer-modify
            ,@body)
           (buffer-string))))))

(defmacro telega-ins--one-lined (&rest body)
  "Execute BODY making insertation one-lined.
It makes one line by replacing all newlines by spaces."
  (let ((startsym (gensym "start"))
        (endsym (gensym "end"))
        (result (gensym "result")))
    `(let ((,startsym (point))
           (,result (progn ,@body))
           (,endsym (point)))
       (save-excursion
         (goto-char ,startsym)
         (while (search-forward "\n" ,endsym 'noerror)
           (replace-match " ")))
       ,result)))

(defmacro telega-ins--with-attrs (attrs &rest body)
  "Execute inserters BODY applying ATTRS after insertation.
Return t."
  (declare (indent 1))
  `(telega-ins
    (telega-fmt-eval-attrs (telega-ins--as-string ,@body) ,attrs)))

(defmacro telega-ins--with-face (face &rest body)
  "Execute BODY highlighting result with FACE."
  (declare (indent 1))
  (let ((startsym (gensym "start"))
        (facesym (gensym "face"))
        (result (gensym "result")))
    `(let ((,startsym (point))
           (,facesym ,face)
           (,result (progn ,@body)))
       (when ,facesym
         (add-face-text-property ,startsym (point) ,facesym 'append))
       ,result)))

(defmacro telega-ins--column (column fill-col &rest body)
  "Execute BODY at COLUMN filling to FILL-COL.
If COLUMN is nil or less then current column, then current column is used."
  (declare (indent 2))
  (let ((colsym (gensym "col"))
        (curcol (gensym "curcol")))
    `(let ((,colsym ,column)
           (,curcol (telega-current-column)))
       (when (or (null ,colsym) (< ,colsym ,curcol))
         (setq ,colsym ,curcol))

       (telega-ins (make-string (- ,colsym ,curcol) ?\s))
;       (move-to-column ,colsym t)
       (telega-ins--with-attrs
           (list :fill 'left
                 :fill-prefix (make-string ,colsym ?\s)
                 :fill-column ,fill-col)
         ,@body))))

(defmacro telega-ins--labeled (label fill-col &rest body)
  "Execute BODY filling it to FILL-COL, prefixing first line with LABEL."
  (declare (indent 2))
  `(progn
     (telega-ins ,label)
     (telega-ins--column nil ,fill-col
       ,@body)))

(defmacro telega-ins--help-message (&rest body)
  "Insert help message using `telega-shadow' face.
If help message has been inserted, insert newline at the end."
  `(when telega-help-messages
     (telega-ins--labeled "  " nil
       (telega-ins--with-face 'telega-shadow
         (when (progn ,@body)
           (telega-ins "\n"))))))

(defmacro telega-ins--raw-button (props &rest body)
  "Execute BODY creating text button with PROPS."
  (declare (indent 1))
  `(button-at (apply 'make-text-button (prog1 (point) ,@body) (point)
                     ,props)))

(defmacro telega-ins--with-props (props &rest body)
  "Execute inserters applying PROPS after insertation.
Return what BODY returns."
  (declare (indent 1))
  (let ((spnt-sym (gensym "pnt")))
    `(let ((,spnt-sym (point)))
       (prog1
           (progn ,@body)
         (add-text-properties ,spnt-sym (point) ,props)))))

(defun telega--region-by-text-prop (beg prop &optional limit)
  "Return region after BEG point with text property PROP set."
  (unless (get-text-property beg prop)
    (setq beg (next-single-char-property-change beg prop nil limit)))
  (let ((end (next-single-char-property-change beg prop nil limit)))
    (when (> end beg)
      (cons beg end))))

(defmacro telega-ins--line-wrap-prefix (prefix &rest body)
  "Execute BODY adding `line-prefix' and `wrap-prefix' properties.
`line-prefix' and `wrap-prefix' are contatenated on subsequent calls to
`telega-ins--line-wrap-prefix'."
  (declare (indent 1))
  (let ((lwprefix-sym (gensym "lwprefix"))
        (lwprefix-props-sym (gensym "lwprefix-props"))
        (start-sym (gensym "start"))
        (region-sym (gensym "region")))
    `(let* ((,start-sym (point))
            (,lwprefix-sym ,prefix)
            (,lwprefix-props-sym (list :telega-lwprefix ,lwprefix-sym
                                       'line-prefix ,lwprefix-sym
                                       'wrap-prefix ,lwprefix-sym))
            (telega--column-offset (+ telega--column-offset
                                      (string-width ,lwprefix-sym)))
            ,region-sym)
       (prog1
           (progn ,@body)
         (while (setq ,region-sym (telega--region-by-text-prop
                                   ,start-sym 'line-prefix (point)))
           (add-text-properties ,start-sym (car ,region-sym)
                                ,lwprefix-props-sym)
           (add-text-properties
            (car ,region-sym) (cdr ,region-sym)
            (list 'line-prefix (concat ,lwprefix-sym
                                       (get-text-property (car ,region-sym)
                                                          'line-prefix))
                  'wrap-prefix (concat ,lwprefix-sym
                                       (get-text-property (car ,region-sym)
                                                          'wrap-prefix))))
           (setq ,start-sym (cdr ,region-sym)))
         (add-text-properties ,start-sym (point) ,lwprefix-props-sym)))))

(defmacro telega-ins-prefix (prefix &rest body)
  "In case BODY inserted anything then PREFIX is also inserted before BODY."
  (declare (indent 1))
  (let ((spnt-sym (gensym "pnt")))
    `(let ((,spnt-sym (point)))
       (when (progn ,@body)
         (save-excursion
           (goto-char ,spnt-sym)
           (telega-ins ,prefix))
         t))))

(defun telega-ins--move-to-column (column)
  "Insert space aligned to COLUMN.
Uses `:align-to' display property."
  ;; NOTE: Use Pixel Specification for `:align-to' this will take
  ;; `text-scale-mode' into account.
  ;; However, if displaying in the terminal, then use ordinary columns.
  ;; See https://t.me/emacs_telega/32464
  (let ((align-to (if (display-graphic-p)
                      (list (telega-chars-xwidth column))
                    column)))
    (telega-ins--with-props `(display (space :align-to ,align-to))
      (telega-ins " "))))

(defmacro telega-ins--sequence (var-seq sep-ins &rest body-ins)
  "Insert items from sequence using BODY-INS separating them with SEP-INS.
VAR-SEQ is used directly in the `seq-doseq' form."
  (declare (indent 1))
  (let ((need-sep-sym (gensym "need-sep")))
    `(let ((,need-sep-sym nil))
       (seq-doseq ,var-seq
         (when ,need-sep-sym
           ,sep-ins)
         (when (progn ,@body-ins)
           (setq ,need-sep-sym t)))
       ,need-sep-sym)))

(provide 'telega-core)


;; Need update the value on every load, because value might change,
;; MELPA might change the directory
(setq telega--lib-directory
      (or (and load-file-name
               (file-name-directory load-file-name))
          default-directory))

;;; telega-core.el ends here

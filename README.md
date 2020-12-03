# ![logo](etc/telega-logo64.png) telega.el [![Telegram chat](https://img.shields.io/badge/chat-%40emacs__telega-%2335ADE1)](https://t.me/emacs_telega) [![TDLib](https://img.shields.io/badge/tdlib-v1.7.0-%2335ADE1)](https://github.com/tdlib/td) [![MELPA](https://melpa.org/packages/telega-badge.svg)](https://melpa.org/#/telega) [![MELPA Stable](http://stable.melpa.org/packages/telega-badge.svg)](http://stable.melpa.org/#/telega) [![GNU Guix](https://img.shields.io/badge/GNU%20Guix-0.7.1-blue)](https://guix.gnu.org/packages/emacs-telega-0.7.1-0.04e53d4/)

See [Telega Manual](https://zevlg.github.io/telega.el/) for
comprehensive documentation.

---

`telega.el` is full featured unofficial client for
[Telegram](https://telegram.org "Telegram") platform for [GNU
Emacs](https://www.gnu.org/software/emacs/ "GNU Emacs").

`telega.el` is actively developed, for this reason, some features are
not implemented, or they are present just as skeleton for future
implementation. However, the core parts are mature enough so that it
is possible to use `telega.el` for basic chat.

# Features

In some random order:

- [x] Listing chats, reordering chats according to internal Telegram
      order
- [x] Getting info about users, groups and supergroups
- [x] Joining chats by invitation link `M-x telega-chat-join-by-link RET`
- [x] Fetching chat history, sending messages, replies, edits, deleting
- [x] Resend failed messages
- [x] Creating new groups, upgrading basicgroup to supergroup
- [x] Forwarding messages
- [x] D-Bus notifications on incoming messages in chats with enabled
      notifications
- [ ] VoIP calls, including D-Bus call notification
- [x] Downloading files from the cloud
- [x] Uploading files/media (also pasting images from clipboard) to the cloud
- [x] Display chat actions, such as "@user is typing..."
- [x] Display/Update chat's draft message
- [x] Company backends for emoji (`:<emoji>` syntax), usernames,
      hashtags, bot commands completions
- [x] Secret chats
- [x] Online global searching chats/contacts/messages
- [x] Avatars, Photos, Stickers
- [x] Animated GIF, via ffplay
- [x] Special messages such as location/live location, voice/video
      messages, contacts, self-destruct photo/video, etc
- [x] Poll messages, Poll creation
- [x] Games, Game Scores 
- [x] Handling local links, such as "tg:" or "https://t.me/xxx"
- [x] ReplyMarkup (inline keyboard buttons) in the messages
- [x] InstantView for web pages
- [x] Blocking/Unblocking users, listing blocked users
- [x] Inline bots requests, via "@bot query<TAB>"
- [x] Chat Lists, "Archived Chats", see [telega#100](https://github.com/zevlg/telega.el/issues/100)
- [x] Searching for messages in chat (PARTLY)
- [x] Shared media
- [x] Multiple accounts
- [ ] Traffic control, see [telega#62](https://github.com/zevlg/telega.el/issues/62)
- [x] [TranslationsPlatform](https://translations.telegram.org) support (PARTLY)
- [x] Message scheduling, reminders [PARTLY]
- [ ] Cloud themes, see [TDLib#691](https://github.com/tdlib/td/issues/691)
- [x] Telegram Folders (since TDLib 1.6.6)
- [x] Disable/Enable message notification on receiver side

## Unique features

- [x] Expressive `ibuffer`-like chats filtering
- [x] Powerful [chats sorting](https://zevlg.github.io/telega.el/#sorting-chats)
- [x] Custom order for chats (some chats on top, some chats on bottom, etc)
- [x] Client side messages filtering
- [x] Get diff for the message edits, see [screenshot](https://zevlg.github.io/telega/screen-13.png)
- [x] [Messages squashing](https://zevlg.github.io/telega.el/#telega-squash-message-mode) minor mode.
- [x] [Highlight code blocks](https://zevlg.github.io/telega.el/#telega-mnzel--display-emacs-content-inside-telega-messages) minor mode.

And much more.

# Screenshots

Root buffer screen:

![screen15](https://zevlg.github.io/telega/screen-15.png)

Chat in [@emacs_en](https://t.me/emacs_en "emacs_en") group:

![screen11](https://zevlg.github.io/telega/screen-11.png)

Attaching funny cat sticker:

![screen12](https://zevlg.github.io/telega/screen-12.png)

# Starting with telega.el

See [Telega Manual](https://zevlg.github.io/telega.el/) for
comprehensive documentation.

# How to contribute

Join our [Telegram group](https://t.me/emacs_telega "Telegram group")
to discuss the development of `telega.el`.

Submitting [issues](https://github.com/zevlg/telega.el/issues) is
exceptionally helpful.

# License

`telega.el` is licensed under GNU GPL version 3.

# FAQ

**Q**: I have this error while installing telega

```console
Cannot open load file: No such file or directory, visual-fill-column
```

**A**: `telega.el` depends on `visual-fill-column` package, please
install it first.  This package is available from
[MELPA](https://melpa.org)

**Q**: I have this error while running telega

```elisp
(error "Invalid image type ‘svg’")
```

and/or

```elisp
(error "Invalid image type ‘imagemagick’")
```

**A**: `telega.el` requires Emacs with SVG and ImageMagick support.
SVG support in Emacs is done using `librsvg` library.  As to
imagemagick, you will need `libmagickcore-dev` and `libmagickwand-dev`
packages installed.  But unfortunately Emacs recently disabled
imagemagick support by default (see
https://lists.gnu.org/r/emacs-devel/2018-12/msg00036.html).  So you
need to compile Emacs by hand, specifying `--with-imagemagick` flag to
`./configure` script.

Telega won't depend on `imagemagick` in future, since required image
features has been added to newer Emacs, see
https://lists.gnu.org/r/emacs-devel/2019-06/msg00242.html

**Q**: Does telega have proxy support?

**A**: Yes, use `telega-proxies` custom variable, for example:

```elisp
(setq telega-proxies
      (list
       '(:server "1.2.3.4" :port 8080 :enable :false
                 :type (:@type "proxyTypeSocks5"
                               :username "rkn" :password "jopa"))
       '(:server "2.3.4.5" :port 8088 :enable t
                 :type (:@type "proxyTypeSocks5"
                               :username "rkn" :password "jopa"))
       ))
```
See `C-h v telega-proxies RET` for full range of proxy types.

**Q**: Stickers are not shown.

**A**: Make sure you have `imagemagick` support and please install `webp` package

**Q**: `telega.el` is unbearable slow.

**A**: You might be hitting into Emacs bug, described here https://lists.gnu.org/archive/html/bug-gnu-emacs/2020-01/msg00548.html

Also see https://github.com/zevlg/telega.el/issues/161

**Q**: There are no glyphs for some unicode characters.

**A**: Please install `fonts-symbola` package

**Q**: There is some formatting issues when some unicode characters are used.

**A**: Yes, partly.  If character has full width of multiple ordinary chars you can tweak `char-width-table`.  Add code like this to your init.el:

```elisp
(setq telega-symbol-unread "🄌")

(defun my-telega-load ()
  ;; 🄌 occupies two full chars, but (string-width "🄌") returns 1
  ;; so we install custom widths to `char-width-table'
  (telega-symbol-set-width telega-symbol-unread 2)

  ;; ... other code
  )

(add-hook 'telega-load-hook 'my-telega-load)
```

There is also `telega-symbol-widths` custom variable, you might want
to modify it.

**Q**: Is there erc-like chats tracking functionality?

**A**: Yes, set `telega-use-tracking-for` to non-nil.

Tracking is done only for opened chats, i.e. chats having
corresponding chat buffer.

Its value is a [Chat Filter](https://zevlg.github.io/telega.el/#chat-filters).

For example, to enable tracking for chats with enabled notifications or for chats where you have unread mention, use:

```elisp
(setq telega-use-tracking-for '(or unmuted mention))
```

**Q**: Is it possible to use telega in tty-only Emacs (aka
emacs-nox)?

**A**: Yes, set `telega-use-images` to `nil`, before start.

**Q**: Is it possible to use markup in outgoing messages?

**A**: Yes, use `C-u RET` to send message with markup.  See [Sending ordinary messages](https://zevlg.github.io/telega.el/#sending-ordinary-messages) for details

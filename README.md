[![CI test](https://github.com/zevlg/telega.el/workflows/CI/badge.svg)](https://github.com/zevlg/telega.el/actions) [![DOCKER image build](https://github.com/zevlg/telega.el/workflows/DOCKER/badge.svg)](https://github.com/zevlg/telega.el/actions) [![Telegram chat](https://img.shields.io/badge/chat-%40emacs__telega-%2335ADE1?logo=telegram)](https://t.me/emacs_telega) [![TDLib](https://img.shields.io/badge/TDLib-v1.8.29-%2335ADE1)](https://github.com/tdlib/td) [![MELPA](https://melpa.org/packages/telega-badge.svg)](https://melpa.org/#/telega) [![MELPA Stable](http://stable.melpa.org/packages/telega-badge.svg)](http://stable.melpa.org/#/telega) [![GNU Guix](https://repology.org/badge/version-for-repo/gnuguix/emacs:telega.svg?header=GNU%20Guix)](https://packages.guix.gnu.org/packages/emacs-telega/) [![Open Collective](https://img.shields.io/opencollective/backers/telega?logo=opencollective)](https://opencollective.com/telega)

[![Backers](https://opencollective.com/telega/backers.svg?avatarHeight=32&width=600)](https://opencollective.com/telega)

# ![logo](etc/telega-logo.svg) telega.el

See [Telega Manual](https://zevlg.github.io/telega.el/) for
comprehensive documentation.

**Latest telega.el release can be found in the
https://github.com/zevlg/telega.el/tree/release-0.8.0 branch, it is
compatible with the latest TDLib major release 1.8.0**

---

`telega.el` is full featured unofficial client for
[Telegram](https://telegram.org "Telegram") platform for [GNU
Emacs](https://www.gnu.org/software/emacs/ "GNU Emacs").

`telega.el` is actively developed, for this reason, some features are
not implemented, or they are present just as skeleton for future
implementation. However, the core parts are mature enough so that it
is possible to use `telega.el` for basic chat.

# Features

`telega.el` implements most of the features official Telegram clients
has.  The only major feature `telega.el` lacks is full support for
voice/video chats.

## Unique features

- [x] Expressive `ibuffer`-like chats filtering
- [x] Powerful [chats sorting](https://zevlg.github.io/telega.el/#sorting-chats)
- [x] Custom order for chats (some chats on top, some chats on bottom, etc)
- [x] [Client side messages filtering](https://zevlg.github.io/telega.el/index-master.html#client-side-messages-ignoring)
- [x] Get diff for the message edits, see [screenshot](https://zevlg.github.io/telega/screen-13.png)
- [x] [Messages squashing](https://zevlg.github.io/telega.el/#telega-squash-message-mode) minor mode.
- [x] [Emacs Stories](https://zevlg.github.io/telega.el/#telega-storiesel--display-emacs-stories-in-the-dashboard) - share your Emacs experience with other Emacs users.
- [x] [AdBlock](https://zevlg.github.io/telega.el/index-master.html#telega-adblockel--block-advertisement-messages) - block advertisement messages in Telegram channels.

And much more.

# Screenshots

Root buffer screen:

![screen31](https://zevlg.github.io/telega/telega-31.png)

Chat in [@emacs_en](https://t.me/emacs_en "emacs_en") group:

![screen11](https://zevlg.github.io/telega/screen-11.png)

Dashboard with Emacs Stories and important chats:

![dashboard](https://zevlg.github.io/telega/emacs-stories-dashboard.png)

# Starting with telega.el

See [Telega Manual](https://zevlg.github.io/telega.el/) for
comprehensive documentation.

# How to contribute

Join our [Telegram group](https://t.me/emacs_telega "Telegram group")
to discuss the development of `telega.el`.

Submitting [issues](https://github.com/zevlg/telega.el/issues) is
exceptionally helpful.

Support `telega.el` by donating to:
| ![TON](etc/donate_ton.jpg) |
|:--:|
| **TON:** UQDLQ-q6Yob4zB7KVpk2xKCPHBwfFrc5JmGCF74NPyB1ViIN |

| ![USDT TRC20](etc/donate_usdt.jpg) |
|:--:|
| **USDT TRC20:** TJ9ekHtD1dnZsCAKJoc7Yx1emNmdLwFbzP |

| ![BTC](etc/donate_btc.jpg) |
|:--:|
| **BTC:** 1PeqqU1dGEqXy52yk3XEySaHXREsrtDvCc |

Donating makes you telega patron forever and you get telega cat eyars
if you wish.

# License

`telega.el` is licensed under GNU GPL version 3.

# FAQ

**Q**: **I have this error while installing telega**

```console
Cannot open load file: No such file or directory, visual-fill-column
```

**A**: `telega.el` depends on the `visual-fill-column` package, please
install it first.  This package is available from
[MELPA](https://melpa.org)

**Q**: **I have this error while running telega**

```elisp
(error "Invalid image type ‘svg’")
```

and/or

```elisp
(error "Invalid image type ‘imagemagick’")
```

**A**: The appropriate behavior is adjusted based on what version of
Emacs you use. If you are using 26.3 or older you need to ensure that
your Emacs was configured with SVG and ImageMagick support. SVG support
is provided using the `librsvg` library, and ImageMagick is provided by
`libmagickcore` and `libmagickwand` development libraries.

If you are using Emacs 27.1+ the ImageMagick support was deprecated as
it posed a significant security issue, but `telega` now relies on the
in-built (and faster) `image-transforms` for those versions.

**Q**: **Does telega have proxy support?**

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

**Q**: **Stickers are not shown.**

**A**: If you are using Emacs 26.3 or older, ensure you it was 
configured with ImageMagick support. Next, install the `webp` package.

**Q**: **There are no glyphs for some unicode characters.**

**A**: Please either install `fonts-symbola` package, or run
`guix package -i font-gnu-{freefont,unifont}` on GNU Guix

If using `fonts-symbola`, add this to your init.el:
```elisp
(set-fontset-font t 'unicode "Symbola" nil 'append)
```

**Q**: **There is some formatting issues when some unicode characters are used.**

**A**: Yes, partly.  If character has full width of multiple ordinary chars 
you can tweak `char-width-table`.  Add code like this to your init.el:

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

**Q**: **Is there erc-like chats tracking functionality?**

**A**: Yes, set `telega-use-tracking-for` to non-nil

Tracking is done only for opened chats, i.e. chats having
corresponding chat buffer.

Its value is a [Chat Filter](https://zevlg.github.io/telega.el/#chat-filters).

For example, to enable tracking for chats with enabled notifications or for chats where you have unread mention, use:

```elisp
(setq telega-use-tracking-for '(or unmuted mention))
```

**Q**: **Is it possible to use telega in tty-only Emacs (aka
emacs-nox)?**

**A**: Yes, set `telega-use-images` to `nil` before starting `telega`

**Q**: **Is it possible to add markup to messages?**

**A**: Yes, use `C-u RET` to send a message with markup

See [Sending ordinary messages](https://zevlg.github.io/telega.el/#sending-ordinary-messages) for details

You may also find `telega-mnz.el` from the `contrib` directory to be
complimentary.

**Q**: **I've enabled `telega-notifications-mode`, but notifications
does not show**

**A**: Make sure your time is correct

Eval `(telega-time-seconds)` to get UTC time in your Emacs, 
it should be more or less the same as on https://www.unixtimestamp.com/

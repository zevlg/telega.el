# telega.el

`telega.el` is an unofficial client for
[Telegram](https://telegram.org "Telegram") platform for [GNU
Emacs](https://gnu.org/emacs "GNU Emacs").

`telega.el` is in its very alpha stages and it is actively
developed. For this reason, many features are not implemented, or they
are present just as skeleton for future implementation. However, the
core parts of this major-mode are mature enough so that it is possible
to use `telega.el` for basic chat.

# Features

- [x] Listing chats, reordering chats according to internal Telegram
      order
- [x] Expressive `ibuffer`-like chats filtering
- [x] Getting info about users, groups and supergroups
- [x] Fetching chat history, sending messages, replies
- [x] D-Bus notifications on incoming messages in chats with enabled
      notifications
- [x] Downloading files from the cloud
- [ ] Uploading files/media to the cloud
- [x] Emoji support (only in GNU Emacs with surrogate pairs support,
      see [Bug#24784](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=24784))
- [ ] Display chat actions, such as "@user is typing..."
- [ ] Emoji input via `:<emoji>:` syntax with completions
- [ ] Username completions for fast mentions
- [ ] Secret chats
- [ ] Online searching chats/messages
- [ ] Avatars, photos
- [ ] Stickers
- [ ] Animated GIF
- [ ] Special messages such as location/live location, voice/video
      messages, etc
- [ ] ReplyMarkup (inline keyboard buttons) in the messages
- [ ] InstantView for web pages 

# Screenshots

Chat buffer and combination of root buffer/chat buffer running under
iOS terminal [blink](https://github.com/blinksh/blink "blink"):

![screen0](https://zevlg.github.io/telega/screen0.png)

![screen1](https://zevlg.github.io/telega/screen1.png)

`telega.el` running under GUI version of Emacs:

![screen2](https://zevlg.github.io/telega/screen2.png)

# Installation

Being in active development, `telega.el` is not ready to be
distributed on archives such as MELPA. At the moment, the only way to
use is from this git repositary. This involves few simple steps
described below.

`telega.el` is built on top of the official library provided by
Telegram [tdlib](https://core.telegram.org/tdlib "tdlib"). Most of the
distributions do not package this libary, so it has to be build from
source.

## Dependences

   * `GNU Emacs` 
   * `GNU make`
   * `GNU gperf`
   * `CMake`

`make` is found in most of the modern machines. The other packages can
be download with the system package manager (such as `apt` for
Debian-based distributions, `dnf` for Fedora or `pacman` for
Arch-based).

### Building tdlib

[tdlib](https://core.telegram.org/tdlib "tdlib") is the backed used to
communicate with the servers. It requires a large amount of memory to
be built.

To get the source:
```console
$ git clone https://github.com/tdlib/td.git
```

Move into the folder with `cd td`

Prepare a folder for building the library:
```console
$ mkdir build && cd build && cmake ../
```

Build the sources:
```console
$ make -jN
```

with `N` number of cores that should be used for the compilation (the optimal
value is the number of physical cores on the machine).

Finally, to install the library system-wide:
```console
$ sudo make install
```

Now that the library is set-up, it is time to install `telega.el`. The
first step consists in building `telega-server`, which is a C
interface to the `tdlib`.

## Building telega-server

To get the source:

```console
$ git clone https://github.com/zevlg/telega.el
```

Moving into the folder with `cd telega.el`, it is possible to build
the `telega-server` executable and move into the `$HOME/.telega` with:

```console
$ make && make install
```

This command does not require superuser privileges.

Start with `M-x telega RET` and follow instructions

## Installing telega.el

Now it is time to install `telega.el` on GNU Emacs. 

This can be done with `use-package`:

```elisp
(use-package telega
  :load-path  "~/telega.el"
  :commands (telega)
  :defer t)
```

Or with:
```elisp
(add-to-list 'load-path "~/telega.el")
(require 'telega)
```

The code should be put in the configuration file for Emacs, which
usually is `init.el`, or `emacs.el`.

## Fire up `telega.el`

`telega.el` can now be started with `M-x telega RET`. The first time
it ask for the phone number to login to the Telegram network.

# Enabling D-Bus notifications

`telega.el` ships with support for D-Bus notifications, but they are disabled by default.  To enable notifications add next code to your `init.el`:

```elisp
(add-hook 'telega-root-mode-hook (lambda () (telega-notifications-mode 1)))
```

# How to contribute

Join our [Telegram group](https://t.me/emacs_telega "Telegram group")
to discuss the development of `telega.el`.

# License

`telega.el` is licensed under GNU GPL version 3.

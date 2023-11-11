;;; telega-adblock.el --- Advertisement blocking for telega  -*- lexical-binding: t -*-

;; Copyright (C) 2021 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Jun 16 11:10:15 2021
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

;;; ellit-org:
;; ** /telega-adblock.el/ -- Block advertisement messages
;;
;; Telegram channels often advertises another channels in annoying
;; manner.  Sometimes adverts has inappropriate content.  This mode
;; uses [[#client-side-messages-ignoring][client side messages
;; ignoring]] functionality to block such advert messages.
;;
;; To enable advertisement blocking use:
;; #+begin_src emacs-lisp
;; (telega-adblock-mode 1)
;; #+end_src
;;
;; Customizable options:
;; - {{{user-option(telega-adblock-for, 2)}}}
;; - {{{user-option(telega-adblock-chat-order-if-last-message-ignored, 2)}}}
;; - {{{user-option(telega-adblock-verbose, 2)}}}
;; - {{{user-option(telega-adblock-max-distance, 2)}}}
;; - {{{user-option(telega-adblock-forwarded-messages, 2)}}}
;; - {{{user-option(telega-adblock-block-msg-temex, 2)}}}
;; - {{{user-option(telega-adblock-allow-msg-temex, 2)}}}

;; TODO:
;; - "invisible" links, example: https://t.me/botoid/1058351
;;   codepoint: 0x200B (ZERO WIDTH SPACE)
;; 
;; - If at least one link to the url is allowed, then any text
;;   pointing to that url is allowed
;; 
;; - Block images/videos from the album of a blocked message.

;;; Code:
(require 'rx)
(require 'telega)

(defgroup telega-adblock nil
  "Customisation for telega adblock mode."
  :prefix "telega-adblock-"
  :group 'telega-modes)

(defcustom telega-adblock-for '(and (type channel)
                                    (not unmuted)
                                    (not verified))
  "Chat Temex to match chats for which to apply adblock logic."
  :type 'telega-chat-temex
  :group 'telega-adblock)

(defcustom telega-adblock-forwarded-messages t
  "Non-nil to block messages forwarded from other channels.
Block them even if a message has no links at all."
  :type 'boolean
  :group 'telega-adblock)

(defcustom telega-adblock-max-distance 4
  "Maximum string-distance for self-link.
Used for heuristics to avoid blocking non-advert messages in some channels.
Set it to less value if you see some advert messages not being blocked."
  :type 'integer
  :group 'telega-adblock)

(defcustom telega-adblock-verbose nil
  "Non-nil to show (in echo area) reason why message is ignored by adblock."
  :type 'boolean
  :group 'telega-adblock)

(defcustom telega-adblock-chat-order-if-last-message-ignored nil
  "Custom chat order for chats with last message being ignored by adblock.
Set to \"1\" to put chats with ignored last message to the bottom of
the rootbuf."
  :type '(choice (const :tag "Unchanged" nil)
                 (string :tag "Custom order"))
  :group 'telega-adblock)

(defcustom telega-adblock-block-msg-temex nil
  "Message's matching this temex will be ignored by adblock."
  :type 'telega-msg-temex
  :options '((contains "#advert"))
  :group 'telega-adblock)

(defcustom telega-adblock-allow-msg-temex
  '(or is-reply-to-msg is-reply-to-story post-with-comments)
  "Message's matching this temex will be allowed."
  :type 'telega-msg-temex
  :group 'telega-adblock)

;; TODO: allow links to known chats
(defcustom telega-adblock-allow-links-to-known-chats t
  "Non-nil to not block messages with links to known chats."
  :type 'boolean
  :group 'telega-adblock)

;; TODO: heuristics about multiple links to same url
;; to block messages like https://t.me/c/1127375190/3747

(defun telega-adblock-msg-extract-links (msg)
  "Extract all links from the message.
Return a list of cons cells, where car is the text used for link and
cdr is the URL."
  (let ((reply-markup (plist-get msg :reply_markup))
        (ret-links nil))
    ;; Extract links from the reply-markup keyboard
    (seq-doseq (row (plist-get reply-markup :rows))
      (seq-doseq (kbd-button row)
        (let ((kbd-type (plist-get kbd-button :type)))
          (when (eq 'inlineKeyboardButtonTypeUrl (telega--tl-type kbd-type))
            (setq ret-links
                  (cons (cons (telega-tl-str kbd-button :text)
                              (plist-get kbd-type :url))
                        ret-links))))))

    ;; Extract links from the message's text
    (let* ((content (plist-get msg :content))
           (msg-text (or (telega-tl-str content :text)
                         (telega-tl-str content :caption))))
      (seq-doseq (txt (telega--split-by-text-prop msg-text :telega-link))
        (when-let* ((txt-link (get-text-property 0 :telega-link txt))
                    (link-url
                     (cl-case (car txt-link)
                       ;; NOTE: Convert direct mention to the url
                       ;; see https://github.com/zevlg/telega.el/issues/309
                       (username
                        (concat "https://t.me/"
                                ;; Strip leading "@"
                                (substring (cdr txt-link) 1)))
                       (url
                        (cdr txt-link)))))
          (setq ret-links
                (cons (cons txt link-url)
                      ret-links)))))
    ret-links))

(defun telega-adblock--link-internal-p (chat link-spec)
  "Return non-nil if link points to CHAT itself."
  ;; NOTE: string-distance is case sensitive, thats why we use
  ;; lowercase everywhere
  (let ((chat-title (downcase (telega-chat-title chat)))
        (chat-username (telega-chat-username chat))
        (link-text (downcase (car link-spec)))
        (link-url (cdr link-spec)))
    (or
     ;; 1. Link text matches CHAT's title/username
     (<= (string-distance chat-title link-text)
         telega-adblock-max-distance)
     ;; 2. Link text starts chat's title
     (string-prefix-p link-text chat-title)
     ;; 2.5 Link text contains chat's title
     (string-match-p (regexp-quote chat-title) link-text)
     ;; 3. Link text resembles channel's username
     (and chat-username
          (<= (string-distance (downcase chat-username) link-text)
              telega-adblock-max-distance))
     ;; 4. Link URL is in the chat's description
     (let* ((full-info (telega--full-info (telega-chat--info chat) 'offline))
            (descr (telega-tl-str full-info :description)))
       (and descr
            (string-match-p (regexp-quote link-url) descr)))
     ;; 5. TODO: Link URL points to the Discussion Group of the channel
     )))

(defun telega-adblock--link-other-channel-p (chat link-spec)
  "Return non-nil if link points to another channel."
  (when-let ((tme-internal-link (telega-tme-open (cdr link-spec) 'convert)))
    (or (string-prefix-p "tg:join?" tme-internal-link)
        (string-prefix-p "tg:msg_url?" tme-internal-link)
        (and (string-prefix-p "tg:privatepost?" tme-internal-link)
             ;; Link URL is not direct url to the CHAT's message
             (not (when-let* ((info (telega-chat--supergroup chat 'locally))
                              (cid (plist-get info :id)))
                    (string-prefix-p (format "tg:privatepost?channel=%d" cid)
                                     tme-internal-link))))
        (and (string-prefix-p "tg:resolve?" tme-internal-link)
             ;; Link URL is not direct url to the CHAT
             (not (when-let ((chat-username (telega-chat-username chat)))
                    (string-prefix-p (concat "tg:resolve?domain=" chat-username)
                                     tme-internal-link)))))))

(defun telega-adblock--link-cheating-p (link-spec)
  "Return non-nil if link is cheating on me.
Cheating means link text looks like regular url (like
http://blabla.com), but underlying url of the link points to site on
another domain."
  (let ((link-text (string-trim (car link-spec)))
        (link-url (cdr link-spec)))
    (and (string-match-p (eval-when-compile
                           (rx string-start "http" (? "s") "://"))
                         link-text)
         ;; NOTE: real url might have some additional trailing params,
         ;; such as utm and other stuff
         (not (string-prefix-p link-text link-url)))))

(defun telega-adblock-link-advert-p (chat link-spec)
  "Return non-nil if LINK-SPEC is an advertisement link.
LINK-SPEC is a cons cell, where car is text under the link and cdr is
an URL."
  (when (and
         (not (telega-adblock--link-internal-p chat link-spec))
         (or (telega-adblock--link-other-channel-p chat link-spec)
             (telega-adblock--link-cheating-p link-spec)))
    (if telega-adblock-verbose
        (message "telega: Blocking advert link: %s in %s"
                 (cdr link-spec) (telega-chat-title chat))
      (telega-debug "ADBLOCK: Blocking advert link: %s in %s"
                    (cdr link-spec) (telega-chat-title chat)))
    t))

(defun telega-adblock-msg-forwarded-p (msg)
  "Return non-nil if MSG is forwarded from another channel."
  (when-let ((fwd-origin (telega--tl-get msg :forward_info :origin))
             (orig-chat-id (when (equal "messageOriginChannel"
                                        (plist-get fwd-origin :@type))
                             (plist-get fwd-origin :chat_id))))
    ;; Allow self-forwards
    (not (eq orig-chat-id (plist-get msg :chat_id)))))

(defun telega-adblock-msg-has-advert-links-p (msg chat)
  "Return non-nil if MSG has at least one advert link."
  ;; NOTE: We group links by the URL, and block only if all
  ;; links to the URL are advertisements.
  (seq-some (lambda (url-group)
              (seq-every-p (apply-partially #'telega-adblock-link-advert-p chat)
                           (cdr url-group)))
            (seq-group-by #'cdr (telega-adblock-msg-extract-links msg))))

(defun telega-adblock-msg-ignore-p (msg)
  "Return non-nil if message MSG is advert message."
  (when-let ((chat (telega-msg-chat msg 'offline)))
    (and (telega-chat-match-p chat telega-adblock-for)
         (not (telega-msg-match-p msg telega-adblock-allow-msg-temex))
         (or (and telega-adblock-forwarded-messages
                  (telega-adblock-msg-forwarded-p msg))
             (telega-msg-match-p msg telega-adblock-block-msg-temex)
             (telega-adblock-msg-has-advert-links-p msg chat)))))

(defun telega-adblock--chat-order-if-last-msg-ignored (orig-fun chat &rest args)
  "Advice for `telega-chat-order' to return custom order.
Custom `telega-adblock-chat-order-if-last-message-ignored' is returned
for chats with last message blocked by adblock."
  (if (and telega-adblock-chat-order-if-last-message-ignored
           (telega-msg-match-p (plist-get chat :last_message)
             '(ignored telega-adblock-msg-ignore-p)))
      (progn
        ;; See https://t.me/emacs_telega/27884
        (cl-assert (stringp telega-adblock-chat-order-if-last-message-ignored))
        telega-adblock-chat-order-if-last-message-ignored)
    (apply orig-fun chat args)))

;;;###autoload
(define-minor-mode telega-adblock-mode
  "Global mode to block ads for `telega-adblock-for' chats."
  :init-value nil :global t :group 'telega-modes
  (if telega-adblock-mode
      (progn
        (add-hook 'telega-msg-ignore-predicates #'telega-adblock-msg-ignore-p)
        (advice-add 'telega-chat-order
                    :around #'telega-adblock--chat-order-if-last-msg-ignored))

    (advice-remove 'telega-chat-order
                   #'telega-adblock--chat-order-if-last-msg-ignored)
    (remove-hook 'telega-msg-ignore-predicates #'telega-adblock-msg-ignore-p)))

(provide 'telega-adblock)

;;; telega-adblock.el ends here

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
;; - {{{user-option(telega-adblock-same-link-count, 2)}}}
;; - {{{user-option(telega-adblock-block-msg-temex, 2)}}}
;; - {{{user-option(telega-adblock-allow-msg-temex, 2)}}}
;; - {{{user-option(telega-adblock-predicates, 2)}}}

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

(defcustom telega-adblock-max-distance 4
  "Maximum string-distance for self-link.
Used for heuristics to avoid blocking non-advert messages in some channels.
Set it to less value if you see some advert messages not being blocked."
  :type 'integer
  :group 'telega-adblock)

(defcustom telega-adblock-same-link-count 3
  "Number of links to the same resource.
Used by `telega-adblock-msg-multiple-same-links-p'."
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

(defcustom telega-adblock-predicates
  '(telega-adblock-msg-by-temex-p
    telega-adblock-msg-forwarded-p
    telega-adblock-msg-has-erid-p
    telega-adblock-msg-multiple-same-links-p
    telega-adblock-msg-has-reply-markup-p
    telega-adblock-msg-has-advert-links-p)
  "List of predicates to check message for advertisements.
Each predicate accepts single argument - message.
If any of predicates returns non-nil, then message contains advert."
  :type '(list function)
  :group 'telega-adblock)

(defcustom telega-adblock-block-msg-temex nil
  "Message temex for `telega-adblock-msg-by-temex-p' predicate."
  :type 'telega-msg-temex
  :options '((contains "#advert"))
  :group 'telega-adblock)

(defcustom telega-adblock-allow-msg-temex
  '(or is-reply-to-msg is-reply-to-story)
  "Message's matching this temex will be allowed."
  :type 'telega-msg-temex
  :group 'telega-adblock)

;; TODO: allow links to known chats
(defcustom telega-adblock-allow-links-to-known-chats t
  "Non-nil to not block messages with links to known chats."
  :type 'boolean
  :group 'telega-adblock)

(defvar telega-adblock-msg-extracted-links nil
  "Bound to the list of links extracted during `telega-adblock-msg-ignore-p'.")

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
  (when-let* ((tme-internal-link (telega-tme-open (cdr link-spec) 'offline))
              ;; NOTE: to avoid errors like:
              ;;  string-prefix-p: Wrong type argument: stringp,
              ;;  (:@type "internalLinkTypeWebApp" ..)
              (string-p (stringp tme-internal-link)))
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
  (when (and (not (telega-adblock--link-internal-p chat link-spec))
             (or (telega-adblock--link-other-channel-p chat link-spec)
                 (telega-adblock--link-cheating-p link-spec)))
    (if telega-adblock-verbose
        (message "telega: Blocking advert link: %s in %s"
                 (cdr link-spec) (telega-chat-title chat))
      (telega-debug "ADBLOCK: Blocking advert link: %s in %s"
                    (cdr link-spec) (telega-chat-title chat)))
    t))

(defun telega-adblock-msg-by-temex-p (msg)
  "Return non-nil if MSG matches `telega-adblock-block-msg-temex'."
  (telega-msg-match-p msg telega-adblock-block-msg-temex))

(defun telega-adblock-msg-forwarded-p (msg)
  "Return non-nil if MSG is forwarded from another channel."
  (when-let ((fwd-origin (telega--tl-get msg :forward_info :origin))
             (orig-chat-id (when (equal "messageOriginChannel"
                                        (plist-get fwd-origin :@type))
                             (plist-get fwd-origin :chat_id))))
    ;; Allow self-forwards
    (not (eq orig-chat-id (plist-get msg :chat_id)))))

(defun telega-adblock-msg-has-advert-links-p (msg)
  "Return non-nil if MSG has at least one advert link."
  ;; NOTE: We group links by the URL, and block only if all
  ;; links to the URL are advertisements.
  (let ((msg-chat (telega-msg-chat msg)))
    (seq-some (lambda (url-group)
                (seq-every-p (lambda (link-spec)
                               (telega-adblock-link-advert-p msg-chat link-spec))
                             (cdr url-group)))
              (seq-group-by #'cdr telega-adblock-msg-extracted-links))))

(defun telega-adblock-msg-has-erid-p (msg)
  "Return non-nil if MSG text contains ERID label."
  (or (telega-msg-match-p msg '(contains "\\<erid:? ?[a-zA-Z0-9]+\\>"))
      ;; NOTE: also check links in the message to have "erid" get
      ;; parameter
      (seq-some (lambda (link-spec)
                  (assoc "erid"
                         (url-parse-query-string
                          (or (cdr (url-path-and-query
                                    (url-generic-parse-url (cdr link-spec))))
                              ""))))
                telega-adblock-msg-extracted-links)))

(defun telega-adblock-msg-multiple-same-links-p (_msg)
  "Return non-nil if MSG has multiple links to the same resource."
  (>= (- (length telega-adblock-msg-extracted-links)
         (length (seq-uniq (mapcar #'cdr telega-adblock-msg-extracted-links))))
      3))

(defun telega-adblock-msg-has-reply-markup-p (msg)
  "Messages with reply markup buttons are usually an advert.
Because regular user can't send messages with reply markup buttons."
  (plist-get msg :reply_markup))

;; TODO
(defun telega-adblock-msg-multiple-messages-with-same-media-p (msg)
  "Return non-nil if MSG is sent to multiple channels at once.
To be marked as ignored it need to have at least one external link."
  ;; NOTE: (from TDLib dev) remote files are the same if `:unique_id'
  ;; is the same even if `:id' differs
  (when telega-adblock-msg-extracted-links
    ;; NOTE: `telega-adblock-msg-extracted-links' is non-nil if there
    ;; is at least one link
    ;; Forwarded messages does not count
    (let* ((chat (telega-msg-chat msg))
           (last-p (>= (plist-get msg :id)
                       (or (telega--tl-get chat :last_message :id) 0)))
           (media-file (telega-msg--content-file msg))
           (chats-list telega--ordered-chats)
           (other-messages nil))
      (when (and last-p media-file (not (plist-get msg :forward_info)))
        (while (and chats-list
                    (not (eq chat (car chats-list)))
                    (< (length other-messages)
                                  3     ;Trigger number
                                  ))
          (when-let* ((ochat (car chats-list))
                      (olast-msg (plist-get ochat :last_message))
                      (ofile (telega-msg--content-file olast-msg)))
            (setq other-messages (cons olast-msg other-messages)))))

      (>= (length other-messages) 3))))

(defun telega-adblock-msg-ignore-p (msg)
  "Return non-nil if message MSG is advert message."
  (and (telega-chat-match-p (telega-msg-chat msg) telega-adblock-for)
       (not (telega-msg-match-p msg telega-adblock-allow-msg-temex))
       (let ((telega-adblock-msg-extracted-links
              (telega-adblock-msg-extract-links msg))
             (telega-msg-ignore-predicates
              telega-adblock-predicates))
         (telega-msg-run-ignore-predicates msg))))

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

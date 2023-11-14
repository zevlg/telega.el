;;; telega-match.el --- Telega Matching Expressions  -*- lexical-binding: t -*-

;; Copyright (C) 2022 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Mon Feb 14 13:49:33 2022
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

;;; ellit-org: commentary
;;
;; Telega Match Expression (temex in short) is a verbal expression to
;; match TDLib objects.  Temex uses S-exp notation similar to ~rx~
;; package for regexps.
;;
;; Primitive Temex is a named predicate returning non-nil if matches
;; some object.  Primitive Temexes can be combined using ~and~, ~or~
;; or ~not~ temexes, so temex is a logical combination of other
;; temexes down to Primitive Temexes.
;;
;; ~telega-match-gen-predicate~ can be used to generate predicate
;; functions out of temex.
;;
;; Chat Temex examples:
;;   - ~(return t)~ ::
;;     Matches all chats.
;;
;;   - ~(or saved-messages (type channel bot))~ ::
;;     Matches bots/channels chats or "Saved Messages" chat.
;;
;;   - ~(and unmuted (unread 10) (mention 1))~ ::
;;     Matches unmuted chats with at least 10 unread messages and at
;;     least one message with unread mention.
;;
;; Message Temex examples:
;;   - ~(sender me)~ ::
;;     Matches all messages sent by me.
;;
;;   - ~(or (prop :contains_unread_mention) unread-reactions)~ ::
;;     Matches messages containing unread mention or reaction.
;;
;;   - ~(and (chat (type channel)) (type text) (contains "\shello\s"))~ ::
;;     Matches channel's text messages containing "hello" word.

;;; Code:
(require 'telega-chat)
(require 'telega-msg)
(require 'telega-user)
(require 'telega-info)

(defvar telega-temex-remap-list nil
  "List of temex remapping.
Each element is a list in form: (PREFIX TEMEX . REMAP-TEMEX)
Bind it to temporary remap primitive matchers.
Can be used to temporarily change temex behaviour.")
(defvar telega-temex-match-prefix nil
  "Symbol to use as temex prefix.
Prefix is an interned symbol, such as `chat', `msg', `user' or `sender'.")

(defmacro define-telega-matcher (prefix name args &rest body)
  "Define new Primitive Matcher with NAME.
ARGS specifies additional arguments to the predicate."
  (declare (doc-string 4) (indent 3))
  (let ((fsym (intern (format "telega-match--primitive-%s%S"
                              (if prefix
                                  (concat (symbol-name prefix) "-")
                                "")
                              name))))
    `(progn
       (defun ,fsym ,args ,@body)
       (setf (alist-get (quote ,name) (get (quote ,prefix) :temex-definitions))
             (function ,fsym)))))

(defsubst telega-match--temex-remap (prefix temex)
  (cddr (seq-find (lambda (re)
                    (and (eq (nth 0 re) prefix)
                         (equal (nth 1 re) temex)))
                  telega-temex-remap-list)))

(defsubst telega-match--temex-function (prefix temex)
  "Return predicate function definition for the primitive TEMEX."
  (or (cdr (assq temex (get prefix :temex-definitions)))
      (when prefix
        ;; Try non-prefix matchers
        (cdr (assq temex (get nil :temex-definitions))))
      (error "Primitive temex `%s%S' is undefined.
Use `define-telega-matcher' to define new matchers."
             (if prefix
                 (concat (symbol-name prefix) "-")
               "")
             temex)))

(defun telega-match-p (object temex)
  "Return non-nil if TEMEX matches OBJECT.
Takes `telega-temex-remap-alist' into account."
  (when temex
    ;; Try whole TEMEX remapping
    (setq temex (or (telega-match--temex-remap telega-temex-match-prefix temex)
                    temex))
    (let ((temex-name temex)
          (temex-args nil))
      (when (consp temex)
        (setq temex-name (car temex)
              temex-args (cdr temex)))
      (unless (symbolp temex-name)
        (error "telega: Invalid Matcher: %S" temex))

      (if-let ((temex-alias (telega-match--temex-remap
                             telega-temex-match-prefix temex-name)))
          ;; Try TEMEX remapping by name only
          (let ((telega-temex-remap-list nil))
            (telega-match-p object temex-alias))

        (apply (telega-match--temex-function
                telega-temex-match-prefix temex-name)
               object temex-args)))))


;;; Logical operations

;;; ellit-org: temex
;; - (return ~RET~) ::
;;   {{{temexdoc(nil, return, 2)}}}
(define-telega-matcher nil return (_object ret)
  "Matches if RET is non-nil and return RET as a result."
  ret)

;;; ellit-org: temex
;; - (eval ~SEXP~) ::
;;   {{{temexdoc(nil, eval, 2)}}}
(define-telega-matcher nil eval (_object sexp)
  "Matches if SEXP evaluates to non-nil, return result of evaluation."
  (eval sexp))

;;; ellit-org: temex
;; - (or ~TEMEX-LIST~...) ::
;;   {{{temexdoc(nil, or, 2)}}}
(define-telega-matcher nil or (obj &rest temex-list)
  "Matches if any matcher in the TEMEX-LIST matches."
  (let ((result nil))
    (while temex-list
      (when (setq result (telega-match-p obj (car temex-list)))
        (setq temex-list nil))
      (setq temex-list (cdr temex-list)))
    result))

;;; ellit-org: temex
;; - (and ~TEMEX-LIST~...) ::
;;   {{{temexdoc(nil, and, 2)}}}
(define-telega-matcher nil and (obj &rest temex-list)
  "Matches if all matchers in the TEMEX-LIST matches.
Also matches if TEMEX-LIST is empty."
  (let ((result t))
    (while temex-list
      (unless (setq result (telega-match-p obj (car temex-list)))
        (setq temex-list nil))
      (setq temex-list (cdr temex-list)))
    result))

;; NOTE: For backward compatibility, `all' as alias for `and'
;; In new code use `(return t)' temex
(define-telega-matcher nil all (obj &rest temex-list)
  (telega-match-p obj `(and ,@temex-list)))

;;; ellit-org: temex
;; - (not ~TEMEX~) ::
;;   {{{temexdoc(nil, not, 2)}}}
(define-telega-matcher nil not (obj temex)
  "Matches if TEMEX does not match."
  (not (telega-match-p obj temex)))

;;; ellit-org: temex
;; - (prop ~PROPERTY~) ::
;;   {{{temexdoc(nil, prop, 2)}}}
(define-telega-matcher nil prop (obj property)
  "Matches if given TDLib object has non-nil PROPERTY."
  (plist-get obj property))

;;; ellit-org: temex
;; - (call ~PREDICATE~) ::
;;   {{{temexdoc(nil, call, 2)}}}
(define-telega-matcher nil call (obj predicate)
  "Matches if PREDICATE called with TDLib object as argument returns non-nil."
  (funcall predicate obj))

;;; ellit-org: temex
;; - (ids ~ID-LIST~...) ::
;;   {{{temexdoc(nil, ids, 2)}}}
(define-telega-matcher nil ids (obj &rest id-list)
  "Matches if TDLib object's id is in the ID-LIST."
  (memq (plist-get obj :id) id-list))


;;; Chat Temexes
;;; ellit-org: chat-temex
;; - (type ~CHAT-TYPE-LIST~), {{{where-is(telega-filter-by-type,telega-root-mode-map)}}} ::
;;   {{{temexdoc(chat, type, 2)}}}
;;
;;   Every chat has a type.  Type is one of:
;;   - ~private~ Private chat with a Telegram user
;;   - ~secret~ Secret chat with a Telegram user
;;   - ~bot~ Chat with a Telegram bot
;;   - ~basicgroup~ Small chat group, could be upgraded to supergroup
;;   - ~supergroup~ Chat group with all the chat possibilities
;;   - ~channel~ Supergroup with unlimited members, where only admins can post messages
(define-telega-matcher chat type (chat &rest chat-type-list)
  "Matches if chat type is one of CHAT-TYPE-LIST."
  (memq (telega-chat--type chat) chat-type-list))

;;; ellit-org: chat-temex
;; - (name ~REGEXP~) ::
;;   {{{temexdoc(chat, name, 2)}}}
(define-telega-matcher chat name (chat regexp)
  "Matches if chat's title matches REGEXP."
  (or (string-match regexp (telega-chat-title chat))
      (let ((info (telega-chat--info chat 'locally)))
        (or (string-match regexp (or (telega-tl-str info :first_name) ""))
            (string-match regexp (or (telega-tl-str info :last_name) ""))
            (when-let ((usernames (plist-get info :usernames)))
              (seq-some (lambda (username)
                          (string-match regexp username))
                        (plist-get usernames :active_usernames)))))))

;;; ellit-org: chat-temex
;; - (search ~QUERY~), {{{where-is(telega-filter-by-search,telega-root-mode-map)}}} ::
;;   {{{temexdoc(chat, search, 2)}}}
(define-telega-matcher chat search (chat _query)
  "Matches if chat maches search QUERY."
  (memq chat telega--search-chats))

;;; ellit-org: chat-temex
;; - nearby, {{{where-is(telega-filter-by-nearby,telega-root-mode-map)}}} ::
;;   {{{temexdoc(chat, nearby, 2)}}}
(define-telega-matcher chat nearby (chat)
  "Matches if chat is nearby `telega-my-location'."
  (telega-chat-nearby-find (plist-get chat :id)))

;;; ellit-org: chat-temex
;; - (custom ~NAME~), {{{where-is(telega-filter-by-custom,telega-root-mode-map)}}} ::
;;   {{{temexdoc(chat, custom, 2)}}}
(define-telega-matcher chat custom (chat name)
  "Matches if custom filter with NAME matches."
  (let ((temex (cdr (assoc name telega-filters-custom))))
    (unless temex
      (error "No such custom chat filter \"%s\"" name))
    (telega-chat-match-p chat temex)))

;;; ellit-org: chat-temex
;; - (has-username [ ~USERNAME~ ]) ::
;;   {{{temexdoc(chat, has-username, 2)}}}
(define-telega-matcher chat has-username (chat &optional username)
  "Matches if chat has username associated with the chat.
If USERNAME is specified, then match only if chat has exact USERNAME."
  (when-let ((chat-username (telega-chat-username chat)))
    (or (not username)
        (equal username chat-username))))

;;; ellit-org: chat-temex
;; - is-public ::
;;   {{{temexdoc(chat, is-public, 2)}}}
(define-telega-matcher chat is-public (chat)
  "Matches if chat is a public chat.
Chat is considered public if it has a username."
  (telega-chat-match-p chat 'has-username))

;;; ellit-org: chat-temex
;; - (unread [ ~N~ ]), {{{where-is(telega-filter-by-unread,telega-root-mode-map)}}} ::
;;   {{{temexdoc(chat, unread, 2)}}}
(define-telega-matcher chat unread (chat &optional n)
  "Matches if chat has at least N unread messages.
By default N is 1.
Also matches chats marked as unread."
  (or (>= (plist-get chat :unread_count) (or n 1))
      (plist-get chat :is_marked_as_unread)))

;;; ellit-org: chat-temex
;; - (mention [ ~N~ ]), {{{where-is(telega-filter-by-mention,telega-root-mode-map)}}} ::
;;   {{{temexdoc(chat, mention, 2)}}}
(define-telega-matcher chat mention (chat &optional n)
  "Matches if chat has least N unread mentions.
By default N is 1."
  (>= (plist-get chat :unread_mention_count) (or n 1)))

;;; ellit-org: chat-temex
;; - muted ::
;;   {{{temexdoc(chat, muted, 2)}}}
(define-telega-matcher chat muted (chat)
  "Matches if chat has disabled notifications."
  (> (telega-chat-notification-setting chat :mute_for) 0))

;;; ellit-org: chat-temex
;; - temporary-muted ::
;;   {{{temexdoc(chat, temporary-muted, 2)}}}
(define-telega-matcher chat temporary-muted (chat)
  "Matches if chat is temporary muted."
  (let ((muted-for (telega-chat-notification-setting chat :mute_for)))
    (and (> muted-for 0)
         (< muted-for telega-mute-for-ever))))

;;; ellit-org: chat-temex
;; - unmuted, {{{where-is(telega-filter-by-unmuted,telega-root-mode-map)}}} ::
;;   {{{temexdoc(chat, unmuted, 2)}}}
(define-telega-matcher chat unmuted (chat)
  "Matches if chat has enabled notifications."
  (telega-chat-match-p chat '(not muted)))

;;; ellit-org: chat-temex
;; - important, {{{where-is(telega-filter-by-important,telega-root-mode-map)}}} ::
;;   {{{temexdoc(chat, important, 2)}}}
(define-telega-matcher chat important (chat)
  "Matches if chat is important.
Chat is important if it matches `telega-important-chat-temex' chat filter."
  (telega-chat-match-p chat telega-important-chat-temex))

;;; ellit-org: chat-temex
;; - (me-is-owner [ ~OR-ADMIN~ ]) ::
;;   {{{temexdoc(chat, me-is-owner, 2)}}}
(define-telega-matcher chat me-is-owner (chat &optional or-admin)
  "Matches if me is owner of the chat.
Only basicgroup, supergroup and channel can be owned.
If optional OR-ADMIN is specified, then match also if me is
administrator in the chat."
  (when-let ((status (telega-chat-member-my-status chat)))
    (memq (telega--tl-type status)
          (list 'chatMemberStatusCreator
                (when or-admin 'chatMemberStatusAdministrator)))))

;;; ellit-org: chat-temex
;; - me-is-member ::
;;   {{{temexdoc(chat, me-is-member, 2)}}}
(define-telega-matcher chat me-is-member (chat)
  "Matches if me is member of the chat.
Matches only basicgroup, supergroup or a channel."
  ;; NOTE: me can only be a member of chats that have tdlib chat list
  ;; position
  (unless (seq-empty-p (plist-get chat :positions))
    (when-let ((status (telega-chat-member-my-status chat)))
      (cl-ecase (telega--tl-type status)
        ((chatMemberStatusAdministrator chatMemberStatusMember)
         t)
        ((chatMemberStatusCreator chatMemberStatusRestricted)
         (plist-get status :is_member))
        ((chatMemberStatusLeft chatMemberStatusBanned)
         nil)))))

;;; ellit-org: chat-temex
;; - me-is-anonymous ::
;;   {{{temexdoc(chat, me-is-anonymous, 2)}}}
(define-telega-matcher chat me-is-anonymous (chat)
  "Matches if me is anonymous in the chat."
  (plist-get (telega-chat-member-my-status chat) :is_anonymous))

;;; ellit-org: chat-temex
;; - (has-avatar [ ~ANIMATED-P~ ])::
;;   {{{temexdoc(chat, has-avatar, 2)}}}
(define-telega-matcher chat has-avatar (chat &optional animated-p)
  "Matches if chat has chat photo.
For non-nil ANIMATED-P match only if avatar is animated."
  (when-let ((chat-photo (plist-get chat :photo)))
    (or (not animated-p)
        (plist-get chat-photo :has_animation))))

;;; ellit-org: chat-temex
;; - has-chatbuf, {{{where-is(telega-filter-by-has-chatbuf,telega-root-mode-map)}}} ::
;;   {{{temexdoc(chat, has-chatbuf, 2)}}}
(define-telega-matcher chat has-chatbuf (chat)
  "Matches if chat has corresponding chatbuf."
  (with-telega-chatbuf chat
    t))

;;; ellit-org: chat-temex
;; - (permission ~PERM~) ::
;;   {{{temexdoc(chat, permission, 2)}}}
(define-telega-matcher chat permission (chat perm)
  "Matches if chat has PERM set in chat permissions.
PERM could be one of listed in `telega-chat--chat-permissions'."
  (plist-get (plist-get chat :permissions) perm))

;;; ellit-org: chat-temex
;; - (my-permission ~PERM~) ::
;;   {{{temexdoc(chat, my-permission, 2)}}}
(define-telega-matcher chat my-permission (chat perm)
  "Matches if me has PERM permission in the chat.
PERM could be one of in `telega-chat--chat-permissions' list or in
`telega-chat--admin-permissions' list."
  (plist-get (telega-chat-member-my-permissions chat) perm))

;;; ellit-org: chat-temex
;; - verified, {{{where-is(telega-filter-by-verified,telega-root-mode-map)}}} ::
;;   {{{temexdoc(chat, verified, 2)}}}
(define-telega-matcher chat verified (chat)
  "Matches if chat is verified."
  (plist-get (telega-chat--info chat 'locally) :is_verified))

;;; ellit-org: chat-temex
;; - (restriction ~SUFFIX-LIST~...), {{{where-is(telega-filter-by-restriction,telega-root-mode-map)}}} ::
;;   {{{temexdoc(chat, restriction, 2)}}}
;;
;;   Chat restriction reason reported only if chat must be restricted
;;   by current client.  See
;;   [[https://github.com/tdlib/td/issues/1203][TDLib#1203]]
(define-telega-matcher chat restriction (chat &rest suffix-list)
  "Matches restricted chats.
SUFFIX-LIST is a list of suffixes to filter on.
Suffix can be one of:
- \"-all\"      - All platforms
- \"-ios\"      - For iOS devices
- \"-android\"  - For Android devices
- \"-wp\"       - Windows?

If SUFFIX-LIST is not specified, then match any restriction reason."
  (when-let ((reason (telega-tl-str (telega-chat--info chat 'locally)
                                    :restriction_reason)))
    (or (not suffix-list)
        (cl-find reason suffix-list
                 :test (lambda (string regexp)
                         (string-match-p regexp string))))))

;;; ellit-org: chat-temex
;; - top, {{{where-is(telega-filter-by-top,telega-root-mode-map)}}} ::
;;   {{{temexdoc(chat, top, 2)}}}
(define-telega-matcher chat top (chat)
  "Matches if chat is in top usage."
  (let ((category (cl-case (telega-chat--type chat)
                    (private 'Users)
                    (bot 'Bots)
                    ((basicgroup supergroup) 'Groups)
                    (channel 'Channels))))
    (memq chat (telega-chats-top category))))

;;; ellit-org: chat-temex
;; - saved-messages ::
;;   {{{temexdoc(chat, saved-messages, 2)}}}
(define-telega-matcher chat saved-messages (chat)
  "Matches only \"Saved Messages\" chat."
  (telega-me-p chat))

;;; ellit-org: chat-temex
;; - replies-messages ::
;;   {{{temexdoc(chat, replies-messages, 2)}}}
(define-telega-matcher chat replies-messages (chat)
  "Matches only \"Replies\" chat."
  (telega-replies-p chat))

;;; ellit-org: chat-temex
;; - tracking, {{{where-is(telega-filter-by-tracking,telega-root-mode-map)}}} ::
;;   {{{temexdoc(chat, tracking, 2)}}}
(define-telega-matcher chat tracking (chat)
  "Matches if chat is in tracking buffers list."
  (with-telega-chatbuf chat
    (member (buffer-name) tracking-buffers)))

;;; ellit-org: chat-temex
;; - (last-message ~MSG-TEMEX~)::
;;   {{{temexdoc(chat, last-message, 2)}}}
(define-telega-matcher chat last-message (chat msg-temex)
  "Matches if chat has last message and last message matches MSG-TEMEX."
  (when-let ((last-msg (plist-get chat :last_message)))
    (telega-msg-match-p last-msg msg-temex)))

;;; ellit-org: chat-temex
;; - (chat-list ~LIST-NAME~), {{{where-is(telega-filter-by-folder,telega-root-mode-map)}}} ::
;;   {{{temexdoc(chat, chat-list, 2)}}}
(define-telega-matcher chat chat-list (chat list-name)
  "Matches if chat is in chat list named LIST-NAME.
LIST-NAME is `main' or `archive' symbol, or string naming Chat Folder."
  (let (item key)
    (cond ((eq list-name 'main)
           (setq item '(:@type "chatListMain")
                 key (telega--tl-prop :list)))
          ((eq list-name 'archive)
           (setq item '(:@type "chatListArchive")
                 key (telega--tl-prop :list)))
          (t
           (when-let ((fi (telega-folder--chat-folder-info list-name)))
             (setq item (plist-get fi :id)
                   key (telega--tl-prop :list :chat_folder_id)))))
    (when-let ((pos (cl-find item (plist-get chat :positions)
                             :key key :test #'equal)))
      ;; NOTE: zero order means "chat has no position"
      (not (equal "0" (plist-get pos :order))))))

;;; ellit-org: chat-temex
;; - main ::
;;   {{{temexdoc(chat, main, 2)}}}
(define-telega-matcher chat main (chat)
  "Matches if chat from \"Main\" chat list."
  (telega-chat-match-p chat '(chat-list main)))

;;; ellit-org: chat-temex
;; - archive ::
;;   {{{temexdoc(chat, archive, 2)}}}
(define-telega-matcher chat archive (chat)
  "Matches if chat is archived, i.e. in \"Archive\" chat list."
  (telega-chat-match-p chat '(chat-list archive)))

;;; ellit-org: chat-temex
;; - is-known ::
;;   {{{temexdoc(chat, is-known, 2)}}}
(define-telega-matcher chat is-known (chat)
  "Matches if chat is known, i.e. in \"Main\" or \"Archive\" chat list."
  (telega-chat-match-p chat '(or main archive)))

;;; ellit-org: chat-temex
;; - (folder ~FOLDER-NAME~), {{{where-is(telega-filter-by-folder,telega-root-mode-map)}}} ::
;;   {{{temexdoc(chat, folder, 2)}}}
(define-telega-matcher chat folder (chat folder-name)
  "Matches if chat belongs to Folder named FOLDER-NAME."
  (telega-chat-match-p chat (list 'chat-list folder-name)))

;;; ellit-org: chat-temex
;; - has-scheduled-messages ::
;;   {{{temexdoc(chat, has-scheduled-messages, 2)}}}
(define-telega-matcher chat has-scheduled-messages (chat)
  "Matches if chat has scheduled messages."
  (plist-get chat :has_scheduled_messages))

;;; ellit-org: chat-temex
;; - has-action-bar ::
;;   {{{temexdoc(chat, has-action-bar, 2)}}}
(define-telega-matcher chat has-action-bar (chat)
  "Matches if chat has active action bar."
  (plist-get chat :action_bar))

;;; ellit-org: chat-temex
;; - has-reply-markup ::
;;   {{{temexdoc(chat, has-reply-markup, 2)}}}
(define-telega-matcher chat has-reply-markup (chat)
  "Matches if chat has reply markup message."
  (not (eq 0 (plist-get chat :reply_markup_message_id))))

;;; ellit-org: chat-temex
;; - can-get-statistics ::
;;   {{{temexdoc(chat, can-get-statistics, 2)}}}
(define-telega-matcher chat can-get-statistics (chat)
  "Matches if statistics available for the chat."
  (when (telega-chat-match-p chat '(type supergroup channel))
    (let ((full-info (telega--full-info (telega-chat--info chat))))
      (plist-get full-info :can_get_statistics))))

;;; ellit-org: chat-temex
;; - has-linked-chat ::
;;   {{{temexdoc(chat, has-linked-chat, 2)}}}
(define-telega-matcher chat has-linked-chat (chat)
  "Matches if chat is a supergroup and has a linked chat."
  (plist-get (telega-chat--info chat) :has_linked_chat))

;;; ellit-org: chat-temex
;; - has-discussion-group ::
;;   {{{temexdoc(chat, has-discussion-group, 2)}}}
(define-telega-matcher chat has-discussion-group (chat)
  "Matches if chat is a channel with a linked discussion group."
  (telega-chat-match-p chat '(and (type channel) has-linked-chat)))

;;; ellit-org: chat-temex
;; - has-location ::
;;   {{{temexdoc(chat, has-location, 2)}}}
(define-telega-matcher chat has-location (chat)
  "Matches if chat is a location-based supergroup."
  (plist-get (telega-chat--info chat) :has_location))

;;; ellit-org: chat-temex
;; - inactive-supergroups , {{{where-is(telega-filter-by-inactive-supergroups,telega-root-mode-map)}}} ::
;;   {{{temexdoc(chat, inactive-supergroups, 2)}}}
(define-telega-matcher chat inactive-supergroups (chat)
  "Matches if chat is an inactive supergroup."
  (memq chat telega--search-chats))

;;; ellit-org: chat-temex
;; - default-disable-notification ::
;;   {{{temexdoc(chat, default-disable-notification, 2)}}}
(define-telega-matcher chat default-disable-notification (chat)
  "Matches if CHAT has non-nil default disable notification setting."
  (plist-get chat :default_disable_notification))

;;; ellit-org: chat-temex
;; - fake-or-scam ::
;;   {{{temexdoc(chat, fake-or-scam, 2)}}}
(define-telega-matcher chat fake-or-scam (chat)
  "Matches if chat is fake or scam user or group."
  (let ((info (telega-chat--info chat)))
    (or (plist-get info :is_scam)
        (plist-get info :is_fake))))

;;; ellit-org: chat-temex
;; - (has-video-chat [ ~NON-EMPTY~ ]) ::
;;   {{{temexdoc(chat, has-video-chat, 2)}}}
(define-telega-matcher chat has-video-chat (chat &optional non-empty)
  "Matches if chat contains a live video chat.
If non-nil NON-EMPTY is specified, then match only if video chat is
not empty."
  (when-let* ((video-chat (plist-get chat :video_chat))
              (group-call-id (plist-get video-chat :group_call_id)))
    (and (not (zerop group-call-id))
         (or (null non-empty)
             (plist-get video-chat :has_participants)))))

;;; ellit-org: chat-temex
;; - has-favorite-messages ::
;;   {{{temexdoc(chat, has-favorite-messages, 2)}}}
(define-telega-matcher chat has-favorite-messages (chat)
  "Matches if chat has favorite messages."
  (cl-find (plist-get chat :id) telega--favorite-messages
           :key (telega--tl-prop :chat_id)))

;;; ellit-org: chat-temex
;; - has-message-ttl ::
;;   {{{temexdoc(chat, has-message-ttl, 2)}}}
(define-telega-matcher chat has-message-ttl (chat)
  "Matches if chat has enabled message auto-delete or self-destruct timer.
Return auto-deletion timer value."
  (when-let ((msg-ttl (plist-get chat :message_auto_delete_time)))
    (and (> msg-ttl 0) msg-ttl)))

;;; ellit-org: chat-temex
;; - is-broadcast-group ::
;;   {{{temexdoc(chat, is-broadcast-group, 2)}}}
(define-telega-matcher chat is-broadcast-group (chat)
  "Matches if chat is a broadcast group."
  (plist-get (telega-chat--info chat) :is_broadcast_group))

;;; ellit-org: chat-temex
;; - is-forum ::
;;   {{{temexdoc(chat, is-forum, 2)}}}
(define-telega-matcher chat is-forum (chat)
  "Matches if chat is a forum group."
  (plist-get (telega-chat--info chat) :is_forum))

;;; ellit-org: chat-temex
;; - has-sponsored-messages ::
;;   {{{temexdoc(chat, has-sponsored-messages, 2)}}}
(define-telega-matcher chat has-sponsored-messages (chat)
  "Matches if chat has sponsored messages.
BE AWARE: This filter will do blocking request for every chat."
  (when (telega-chat-match-p chat '(type channel))
    (telega--getChatSponsoredMessages chat)))

;;; ellit-org: chat-temex
;; - has-protected-content ::
;;   {{{temexdoc(chat, has-protected-content, 2)}}}
(define-telega-matcher chat has-protected-content (chat)
  "Matches if chat has protected content."
  (plist-get chat :has_protected_content))

;;; ellit-org: chat-temex
;; - has-default-sender ::
;;   {{{temexdoc(chat, has-default-sender, 2)}}}
(define-telega-matcher chat has-default-sender (chat)
  "Matches if chat allows choosing a message sender."
  (plist-get chat :message_sender_id))

;;; ellit-org: chat-temex
;; - can-send-or-post ::
;;   {{{temexdoc(chat, can-send-or-post, 2)}}}
(define-telega-matcher chat can-send-or-post (chat)
  "Matches if you can send or post messages to the chat.
You don't need te be a chat member to be able to send messages.
Chat might not be known (i.e. in your Main or Archive list) to post
messages into it. Use `is-known' chat temex to check chat is known."
  (or (let ((my-perms (telega-chat-member-my-permissions chat)))
        (or (plist-get my-perms :can_send_basic_messages)
            (plist-get my-perms :can_post_messages)))

      ;; Also, it is possible to send message to discussion group
      ;; without joining it
      (and (telega-chat-match-p chat '(type supergroup))
           (not (plist-get (telega-chat--info chat) :join_to_send_messages)))))

;;; ellit-org: chat-temex
;; - is-inline-bot ::
;;   {{{temexdoc(chat, is-inline-bot, 2)}}}
(define-telega-matcher chat is-inline-bot (chat)
  "Matches if corresponding bot accepts inline requests."
  (when (telega-chat-bot-p chat)
    (when-let ((user (telega-chat-user chat)))
      (telega--tl-get user :type :is_inline))))

;;; ellit-org: chat-temex
;; - (unread-reactions [ ~N~ ]) ::
;;   {{{temexdoc(chat, unread-reactions, 2)}}}
(define-telega-matcher chat unread-reactions (chat &optional n)
  "Matches if chat has least N unread reactions.
By default N is 1."
  (>= (or (plist-get chat :unread_reaction_count) 0) (or n 1)))

;;; ellit-org: chat-temex
;; - (has-active-stories [ ~UNREAD-P~ ]) ::
;;   {{{temexdoc(chat, has-active-stories, 2)}}}
(define-telega-matcher chat has-active-stories (chat &optional unread-p)
  "Matches if chat has non-expired stories available to you.
If UNREAD-P is non-nil then match only if there is at least one unread
non-expired story."
  (plist-get (telega-chat--info chat) (if unread-p
                                          :has_unread_active_stories
                                        :has_active_stories)))

;;; ellit-org: chat-temex
;; - (active-stories-list ~LIST~) ::
;;   {{{temexdoc(chat, active-stories-list, 2)}}}
(define-telega-matcher chat active-stories-list (chat list)
  "Matches if chat's active stories belongs to LIST.
LIST is one of `main' or `archive'."
  (when-let* ((active-stories (telega-chat--active-stories chat))
              (list-type (telega--tl-get active-stories :list :@type)))
    (cl-ecase list
      (main (equal list-type "storyListMain"))
      (archive (equal list-type "storyListArchive")))))

;;; ellit-org: chat-temex
;; - has-pinned-stories ::
;;   {{{temexdoc(chat, has-pinned-stories, 2)}}}
(define-telega-matcher chat has-pinned-stories (chat)
  "Matches if channel chat has pinned stories."
  (when (telega-chat-match-p chat '(type channel))
    (let ((full-info (telega--full-info (telega-chat--info chat))))
      (plist-get full-info :has_pinned_stories))))

;;; ellit-org: chat-temex
;; - can-send-stories ::
;;   {{{temexdoc(chat, can-send-stories, 2)}}}
(define-telega-matcher chat can-send-stories (chat)
  "Matches if you can post a story into chat."
  (memq chat telega--search-chats))

;;; ellit-org: chat-temex
;; - (user ~USER-TEMEX~) ::
;;   {{{temexdoc(chat, user, 2)}}}
(define-telega-matcher chat user (chat user-temex)
  "Matches non-bot private chat where corresponding user matches USER-TEMEX."
  (unless (telega-chat-bot-p chat)
    (when-let ((user (telega-chat-user chat)))
      (telega-user-match-p user user-temex))))

;;; ellit-org: chat-temex
;; - (bot-user ~USER-TEMEX~) ::
;;   {{{temexdoc(chat, bot-user, 2)}}}
(define-telega-matcher chat bot-user (chat user-temex)
  "Matches chat where corresponding bot user matches USER-TEMEX."
  (when (telega-chat-bot-p chat)
    (when-let ((user (telega-chat-user chat)))
      (telega-user-match-p user user-temex))))

;;; ellit-org: chat-temex
;; - (is-blocked [ ~BLOCK-LIST~ ]) ::
;;   {{{temexdoc(chat, is-blocked, 2)}}}
(define-telega-matcher chat is-blocked (chat &optional block-list)
  "Matches chat if chat is blocked in by the BLOCK-LIST.
BLOCK-LIST is one of `blockListMain' or `blockListStories'.
By default `blockListMain' is used."
  (when-let ((chat-block-list (plist-get chat :block_list)))
    (eq (telega--tl-type chat-block-list)
        (or block-list 'blockListMain))))


;;; User Temexes
;;; ellit-org: user-temex
;; - is-deleted ::
;;   {{{temexdoc(user, is-deleted, 2)}}}
(define-telega-matcher user is-deleted (user)
  "Matches if user account is deleted."
  (eq (telega-user--type user) 'deleted))

;;; ellit-org: user-temex
;; - is-bot ::
;;   {{{temexdoc(user, is-bot, 2)}}}
(define-telega-matcher user is-bot (user)
  "Matches if user is a bot."
  (telega-user-bot-p user))

;;; ellit-org: user-temex
;; - (status ~STATUS-LIST~...) ::
;;   {{{temexdoc(user, status, 2)}}}
;;
;;   Each element in ~STATUS-LIST~ is one of: "Online", "Offline",
;;   "Recently", "LastWeek", "LastMonth" or "Empty"
(define-telega-matcher user status (user &rest status-list)
  "Matches if user status is one of STATUS-LIST."
  (member (telega-user--seen user) status-list))

;;; ellit-org: user-temex
;; - online ::
;;   {{{temexdoc(user, is-online, 2)}}}
;;
;;   Same as ~(status "Online")~ user temex.
(define-telega-matcher user is-online (user)
  "Matches if user is online.
Does not match bots, because bots are always online."
  (telega-user-match-p user '(status "Online")))

;;; ellit-org: user-temex
;; - (contact [ ~MUTUAL-P~ ]), {{{where-is(telega-filter-by-contact,telega-root-mode-map)}}} ::
;;   {{{temexdoc(user, contact, 2)}}}
(define-telega-matcher user contact (user &optional mutual-p)
  "Matches if user is in my contacts list.
If MUTUAL-P is non-nil, then mach only if contact is mutual."
  (plist-get user (if mutual-p :is_mutual_contact :is_contact)))

;;; ellit-org: user-temex
;; - is-close-friend ::
;;   {{{temexdoc(user, is-close-friend, 2)}}}
(define-telega-matcher user is-close-friend (user)
  "Matches if user is my close friend."
  (plist-get user :is_close_friend))

;;; ellit-org: user-temex
;; - has-pinned-stories ::
;;   {{{temexdoc(user, has-pinned-stories, 2)}}}
(define-telega-matcher user has-pinned-stories (user)
  "Matches if user has pinned stories."
  (plist-get (telega--full-info user) :has_pinned_stories))

;;; ellit-org: user-temex
;; - (groups-in-common [ ~N~ ]) ::
;;   {{{temexdoc(user, groups-in-common, 2)}}}
(define-telega-matcher user groups-in-common (user &optional n)
  "Matches if user has at least N groups in common with me.
By default N is 1."
  (>= (plist-get (telega--full-info user) :group_in_common_count)
      (or n 1)))

;;; ellit-org: user-temex
;; - is-telega-patron ::
;;   {{{temexdoc(user, is-telega-patron, 2)}}}
(define-telega-matcher user is-telega-patron (user)
  "Matches if corresponding user is a telega patron."
  (telega-msg-sender-patron-p user))

;;; ellit-org: user-temex
;; - is-premium ::
;;   {{{temexdoc(user, is-premium, 2)}}}
(define-telega-matcher user is-premium (user)
  "Matches if corresponding user is a Telegram Premium user."
  (plist-get user :is_premium))

;;; ellit-org: user-temex
;; - has-private-forwards ::
;;   {{{temexdoc(user, has-private-forwards, 2)}}}
(define-telega-matcher user has-private-forwards (user)
  "Matches if user can't be linked in forwarded messages."
  (plist-get (telega--full-info user) :has_private_forwards))

;;; ellit-org: user-temex
;; - has-emoji-status ::
;;   {{{temexdoc(user, has-emoji-status, 2)}}}
(define-telega-matcher user has-emoji-status (user)
  "Matches if corresponding user set his current emoji status."
  (plist-get user :emoji_status))

;;; ellit-org: user-temex
;; - (username [ ~USERNAME-REGEXP~ ]) ::
;;   {{{temexdoc(user, username, 2)}}}
(define-telega-matcher user username (user username-regexp)
  "Matches if user's username matches USERNAME-REGEXP."
  (when-let ((username (telega-msg-sender-username user)))
    (string-match-p username-regexp username)))

;;; ellit-org: user-temex
;; - (chat ~CHAT-TEMEX~) ::
;;   {{{temexdoc(user, chat, 2)}}}
(define-telega-matcher user chat (user chat-temex)
  "Matches if me has private chat with USER matching CHAT-TEMEX."
  (when-let ((chat (telega-chat-get (plist-get user :id) 'offline)))
    (telega-chat-match-p chat chat-temex)))

;;; ellit-org: user-temex
;; - (is-blocked [ ~BLOCK-LIST~ ]) ::
;;   {{{temexdoc(user, is-blocked, 2)}}}
(define-telega-matcher user is-blocked (user &optional block-list)
  "Matches user blocked by the BLOCK-LIST.
BLOCK-LIST is one of `blockListMain' or `blockListStories'.
By default `blockListMain' is used."
  (let* ((block-list (or block-list 'blockListMain))
         (blocked-user-ids
          (cdr (alist-get block-list telega--blocked-user-ids-alist))))
    (or (memq (plist-get user :id) (cdr blocked-user-ids))
        (when-let ((user-block-list
                    (plist-get (telega--full-info user) :block_list)))
          (eq (telega--tl-type user-block-list) block-list)))))


;;; Message Temexes
;;; ellit-org: msg-temex
;; - (type ~MSG-TYPE-LIST~) ::
;;   {{{temexdoc(msg, type, 2)}}}
;;
;;   Every message has a content type.  Most notable message types
;;   are: ~Text~, ~Animation~, ~Audio~, ~Document~, ~Photo~,
;;   ~Sticker~, ~Video~, ~VideoNote~, ~VoiceNote~, ~Location~, etc.
(define-telega-matcher msg type (msg &rest msg-type-list)
  "Matches if message's content type is one of MSG-TYPE-LIST."
  (memq (intern (substring (telega--tl-get msg :content :@type)
                           ;; Strip "Message" prefix
                           7))
        msg-type-list))

;;; ellit-org: msg-temex
;; - seen ::
;;   {{{temexdoc(msg, seen, 2)}}}
(define-telega-matcher msg seen (msg)
  "Return non-nil if message has been viewed in the chat."
  (<= (plist-get msg :id)
      (let ((chat (telega-msg-chat msg)))
        (or (with-telega-chatbuf chat
              (telega-chatbuf--last-read-inbox-msg-id))
            (plist-get chat :last_read_inbox_message_id)))))

;;; ellit-org: msg-temex
;; - (unread-reactions [ ~N~ ]) ::
;;   {{{temexdoc(msg, unread-reactions, 2)}}}
(define-telega-matcher msg unread-reactions (msg &optional n)
  "Matches if message has at least N unread reactions.
By default N is 1."
  (>= (length (plist-get msg :unread_reactions)) (or n 1)))

;;; ellit-org: msg-temex
;; - has-chosen-reaction ::
;;   {{{temexdoc(msg, has-chosen-reaction, 2)}}}
(define-telega-matcher msg has-chosen-reaction (msg)
  "Matches if message has a reaction chosen by me."
  (telega-msg-chosen-reaction-types msg))

;;; ellit-org: msg-temex
;; - is-reply-to-msg ::
;;   {{{temexdoc(msg, is-reply-to-msg, 2)}}}
(define-telega-matcher msg is-reply-to-msg (msg)
  "Matches if message is a reply to some message."
  (when-let ((reply-to (plist-get msg :reply_to)))
    (eq 'messageReplyToMessage (telega--tl-type reply-to))))

;;; ellit-org: msg-temex
;; - is-reply-to-story ::
;;   {{{temexdoc(msg, is-reply-to-story, 2)}}}
(define-telega-matcher msg is-reply-to-story (msg)
  "Matches if message is a reply to a story."
  (when-let ((reply-to (plist-get msg :reply_to)))
    (eq 'messageReplyToStory (telega--tl-type reply-to))))

;;; ellit-org: msg-temex
;; - post-with-comments ::
;;   {{{temexdoc(msg, post-with-comments, 2)}}}
(define-telega-matcher msg post-with-comments (msg)
  "Matches if message is a channel post that can be commented."
  (and (plist-get msg :is_channel_post)
       (plist-get msg :can_get_message_thread)))

;;; ellit-org: msg-temex
;; - is-topic ::
;;   {{{temexdoc(msg, is-topic, 2)}}}
(define-telega-matcher msg is-topic (msg)
  "Matches if message is a forum topic message."
  (plist-get msg :is_topic_message))

;;; ellit-org: msg-temex
;; - is-thread ::
;;   {{{temexdoc(msg, is-thread, 2)}}}
(define-telega-matcher msg is-thread (msg)
  "Matches if message belongs to or starts a messages thread."
  (and (not (plist-get msg :is_topic_message))
       (or (plist-get msg :can_get_message_thread)
           (not (telega-zerop (plist-get msg :message_thread_id))))))

;;; ellit-org: msg-temex
;; - (web-page [ ~PROPNAME~ ]) ::
;;   {{{temexdoc(msg, web-page, 2)}}}
(define-telega-matcher msg web-page (msg &optional propname)
  "Matches messages with a webpage preview.
If PROPNAME is specified, then match only message with a webpage
having PROPNAME property."
  (when-let ((web-page (telega--tl-get msg :content :web_page)))
    (or (null propname)
        (plist-get web-page propname))))

;;; ellit-org: msg-temex
;; - (outgoing [ ~ANY-STATE-P~ ]) ::
;;   {{{temexdoc(msg, outgoing, 2)}}}
(define-telega-matcher msg outgoing (msg &optional any-state-p)
  "Matches if message is an outgoing message.
This temex differs from `(sender me)', matching any outgoing messages,
including anonymous messages to channels created by me."
  (and (plist-get msg :is_outgoing)
       (or any-state-p
           ;; i.e. sent successfully
           (not (plist-get msg :sending_state)))))

;;; ellit-org: msg-temex
;; - (ignored [ ~REASON~ ]) ::
;;   {{{temexdoc(msg, ignored, 2)}}}
(define-telega-matcher msg ignored (msg &optional reason)
  "Matches if message is an ignored message.
If REASON is specified, then match only if has been ignored by REASON
function."
  (when-let ((ignored-by
              (or (plist-get msg :ignored-p)
                  (when msg
                    (let ((last-ignored (plist-get (telega-msg-chat msg)
                                                   :telega-last-ignored)))
                      (when (eq (plist-get msg :id) (car last-ignored))
                        (cdr last-ignored)))))))
    (eq ignored-by (or reason ignored-by))))

;;; ellit-org: msg-temex
;; - (contains ~REGEXP~ ) ::
;;   {{{temexdoc(msg, contains, 2)}}}
(define-telega-matcher msg contains (msg regexp)
  "Matches if message's text or caption contains REGEXP.
Matching ignores case."
  (when-let ((msg-text (telega-msg-content-text msg)))
    (let ((case-fold-search t))
      (string-match-p regexp msg-text))))

;;; ellit-org: msg-temex
;; - (chat ~CHAT-TEMEX~) ::
;;   {{{temexdoc(msg, chat, 2)}}}
(define-telega-matcher msg chat (msg chat-temex)
  "Matches if message's chat matches CHAT-TEMEX."
  (telega-chat-match-p (telega-msg-chat msg) chat-temex))

;;; ellit-org: msg-temex
;; - (sender ~SENDER-TEMEX~) ::
;;   {{{temexdoc(msg, sender, 2)}}}
(define-telega-matcher msg sender (msg sender-temex)
  "Matches if message's sender matches SENDER-TEMEX."
  (telega-sender-match-p (telega-msg-sender msg) sender-temex))

;;; ellit-org: msg-temex
;; - is-deleted ::
;;   {{{temexdoc(msg, is-deleted, 2)}}}
(define-telega-matcher msg is-deleted (msg)
  "Matches deleted message."
  (plist-get msg :telega-is-deleted-message))

;;; ellit-org: msg-temex
;; - is-last ::
;;   {{{temexdoc(msg, is-last, 2)}}}
(define-telega-matcher msg is-last (msg)
  "Matches if message is the last message in chat."
  (telega-chat-match-p (telega-msg-chat msg)
    `(last-message (ids ,(plist-get msg :id)))))


;;; ellit-org: sender-temex
;; - me ::
;;   {{{temexdoc(sender, me, 2)}}}
(define-telega-matcher sender me (sender)
  "Matches if sender is me."
  (eq telega--me-id (plist-get sender :id)))

;;; ellit-org: sender-temex
;; - (is-blocked [ ~BLOCK-LIST~ ]) ::
;;   {{{temexdoc(sender, is-blocked, 2)}}}
(define-telega-matcher sender is-blocked (sender &optional block-list)
  "Matches if sender is blocked in the BLOCK-LIST.
BLOCK-LIST is one of `blockListMain', `blockListStories'.
By default `blockListMain' is used."
  (if (telega-user-p sender)
      (telega-user-match-p sender `(is-blocked ,block-list))
    (cl-assert (telega-chat-p sender))
    (telega-chat-match-p sender `(is-blocked ,block-list))))

;;; ellit-org: sender-temex
;; - (user ~USER-TEMEX~) ::
;;   {{{temexdoc(sender, user, 2)}}}
(define-telega-matcher sender user (sender user-temex)
  "Matches if sender is a user matching USER-TEMEX."
  (when (telega-user-p sender)
    (telega-user-match-p sender user-temex)))

;;; ellit-org: sender-temex
;; - (chat ~CHAT-TEMEX~) ::
;;   {{{temexdoc(sender, chat, 2)}}}
(define-telega-matcher sender chat (sender chat-temex)
  "Matches if sender is a chat matching CHAT-TEMEX."
  (when (telega-chat-p sender)
    (telega-chat-match-p sender chat-temex)))


;;; Topic matchers
(defconst telega-match-topic-as-chat-filters
  '(last-message mention unread-reactions muted temporary-muted)
  "List of Chat Temexes suitable for topics as well.")

;;; ellit-org: topic-temex
;; - (last-message ~MSG-TEMEX~) ::
;;   {{{temexdoc(topic, last-message, 2)}}}
(define-telega-matcher topic last-message (topic msg-temex)
  "Matches if topic's last message matches MSG-TEMEX."
  ;; NOTE: topic has same `:last_message' property as chat, so this
  ;; will work
  (telega-chat-match-p topic `(last-message ,msg-temex)))

;;; ellit-org: topic-temex
;; - (mention [ ~N~ ]) ::
;;   {{{temexdoc(topic, mention, 2)}}}
(define-telega-matcher topic mention (topic &optional n)
  "Matches if topic has least N unread mentions.
By default N is 1."
  ;; NOTE: topic has same `:unread_mention_count' property as chat, so
  ;; this will work
  (telega-chat-match-p topic `(mention ,n)))

;;; ellit-org: topic-temex
;; - (unread-reactions [ ~N~ ]) ::
;;   {{{temexdoc(topic, unread-reactions, 2)}}}
(define-telega-matcher topic unread-reactions (topic &optional n)
  "Matches if topic has least N unread reactions.
By default N is 1."
  ;; NOTE: topic has same `:unread_reaction_count' property as chat,
  ;; so this will work
  (telega-chat-match-p topic `(unread-reactions , n)))

;;; ellit-org: topic-temex
;; - muted ::
;;   {{{temexdoc(topic, muted, 2)}}}
(define-telega-matcher topic muted (topic)
  "Matches if topic has disabled notifications."
  ;; NOTE: topic has same `:notification_settings' property as chat,
  ;; so this will work
  (telega-chat-match-p topic 'muted))

;;; ellit-org: topic-temex
;; - temporary-muted ::
;;   {{{temexdoc(topic, temporary-muted, 2)}}}
(define-telega-matcher topic temporary-muted (topic)
  "Matches if topic is temporary muted."
  ;; NOTE: topic has same `:notification_settings' property as chat,
  ;; so this will work
  (telega-chat-match-p topic 'temporary-muted))

;;; ellit-org: topic-temex
;; - (creator ~SENDER-TEMEX~) ::
;;   {{{temexdoc(topic, creator, 2)}}}
(define-telega-matcher topic creator (topic sender-temex)
  "Matches if topic's creator matches SENDER-TEMEX."
  (telega-sender-match-p
      (telega-msg-sender (telega--tl-get topic :info :creator_id))
    sender-temex))

;;; ellit-org: topic-temex
;; - (chat ~CHAT-TEMEX~) ::
;;   {{{temexdoc(topic, chat, 2)}}}
(define-telega-matcher topic chat (topic chat-temex)
  "Matches if topic's chat matches CHAT-TEMEX."
  (telega-chat-match-p (telega-topic-chat topic) chat-temex))

;;; ellit-org: topic-temex
;; - is-general ::
;;   {{{temexdoc(topic, is-general, 2)}}}
(define-telega-matcher topic is-general (topic)
  "Matches if topic is a general topic in a chat."
  (telega--tl-get topic :info :is_general))

;;; ellit-org: topic-temex
;; - is-outgoing ::
;;   {{{temexdoc(topic, is-outgoing, 2)}}}
(define-telega-matcher topic is-outgoing (topic)
  "Matches if topic has been created by me."
  (telega--tl-get topic :info :is_outgoing))

;;; ellit-org: topic-temex
;; - is-closed ::
;;   {{{temexdoc(topic, is-closed, 2)}}}
(define-telega-matcher topic is-closed (topic)
  "Matches if topic is closed."
  (telega--tl-get topic :info :is_closed))

;;; ellit-org: topic-temex
;; - is-hidden ::
;;   {{{temexdoc(topic, is-hidden, 2)}}}
(define-telega-matcher topic is-hidden (topic)
  "Matches if topic is hidden.
for General topic only."
  (telega--tl-get topic :info :is_hidden))

;;; ellit-org: topic-temex
;; - is-most-recent ::
;;   {{{temexdoc(topic, is-most-recent, 2)}}}
(define-telega-matcher topic is-most-recent (topic)
  "Matches if last message in the chat is made to topic."
  (when-let ((topic-msg (plist-get topic :last_message))
             (chat-msg (plist-get (telega-topic-chat topic) :last_message)))
    (telega-msg-id= topic-msg chat-msg)))


;;; ellit-org: story-temex
;; - (chat ~CHAT-TEMEX~) ::
;;   {{{temexdoc(story, chat, 2)}}}
(define-telega-matcher story chat (story chat-temex)
  "Matches if story is sent by CHAT matching CHAT-TEMEX."
  (when-let ((chat (telega-story-chat story 'offline)))
    (telega-chat-match-p chat chat-temex)))

;;; ellit-org: story-temex
;; - (contains ~REGEXP~) ::
;;   {{{temexdoc(story, contains, 2)}}}
(define-telega-matcher story contains (story regexp)
  "Matches if story's caption contains REGEXP."
  (when-let ((caption (telega-tl-str story :caption)))
    (let ((case-fold-search t))
      (string-match-p regexp caption))))

;;; ellit-org: story-temex
;; - seen ::
;;   {{{temexdoc(story, seen, 2)}}}
(define-telega-matcher story seen (story)
  "Matches if story has been viewed."
  (when-let* ((chat (telega-story-chat story))
              (active-stories (telega-chat--active-stories chat)))
    (<= (plist-get story :id)
        (plist-get active-stories :max_read_story_id))))

;;; ellit-org: story-temex
;; - is-video ::
;;   {{{temexdoc(story, is-video, 2)}}}
(define-telega-matcher story is-video (story)
  "Matches if story has video content."
  (eq 'storyContentVideo (telega--tl-type (plist-get story :content))))

;;; ellit-org: story-temex
;; - is-photo ::
;;   {{{temexdoc(story, is-photo, 2)}}}
(define-telega-matcher story is-photo (story)
  "Matches if story has photo content."
  (eq 'storyContentPhoto (telega--tl-type (plist-get story :content))))

(provide 'telega-match)

;;; telega-match.el ends here

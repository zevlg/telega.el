;;; telega-chat.el --- Chat mode for telega  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Apr 19 19:59:51 2018
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
;; Chatbuf is a Emacs buffer showing some Telegram chat.  Chatbuf
;; consists of a list of chat messages and an input for your messages
;; to send.  Press
;; {{{where-is(telega-describe-message,telega-msg-button-map)}}} to
;; get detailed description of the message at point.
;;
;; ~visual-line-mode~ and ~visual-fill-column-mode~ are enabled by
;; default in chat buffers, to word-wrap and fill message content. You
;; might want to tune ~visual-fill-column-extra-text-width~ custom
;; option if message's header does not fit into
;; ~telega-chat-fill-column~ for some reason.
;;
;; Avoid setting ~truncate-lines~ to non-nil value in the chatbufs
;; (and using modes that does so), unless you know what you are doing,
;; you will get confusing results.
;;
;; Note for
;; [[https://en.wikipedia.org/wiki/Right-to-left_script][RTL]] users:
;; unlike rootbuf, chatbufs disables bidirectional display reordering
;; by default, so RTL text will look reversed in chatbufs.  To enable
;; bidi in chatbufs customize your
;; ~telega-chat-bidi-display-reordering~ user option.
;;
;; Important customizable options:
;; - {{{user-option(telega-chat-fill-column, 2)}}}
;; - {{{user-option(telega-chat-use-date-breaks-for, 2)}}}

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'ring)
(require 'url-util)
(require 'seq)
(require 'dired)                      ; dired-get-marked-files
(require 'mailcap)                    ; mailcap-extension-to-mime

(require 'telega-core)
(require 'telega-tdlib)
(require 'telega-msg)
(require 'telega-ins)
(require 'telega-voip)                  ;telega-voip-call
(require 'telega-notifications)
(require 'telega-sticker)
(require 'telega-company)
(require 'telega-i18n)
(require 'telega-tme)
(require 'telega-sort)
(require 'telega-filter)
(require 'telega-modes)

(require 'visual-fill-column)

;; shutup compiler
(defvar company-backends)
(declare-function company-complete "company")
(declare-function company-begin-backend "company" (backend &optional callback))
(declare-function company-call-backend "company" (&rest args))

;; telega-tdlib-events.el depends on telega-chat.el
(declare-function telega--on-updateDeleteMessages "telega-tdlib-events" (event))
(declare-function telega-chat--update "telega-tdlib-events" (chat &rest events))

;; telega-info.el depends on telega-chat.el
(declare-function telega--info "telega-info" (tlobj-type tlobj-id &optional locally-p))
(declare-function telega--full-info "telega-info" (tlobj &optional _callback))

;; telega-root.el depends on telega-chat.el
(declare-function telega--check-buffer-switch "telega-root")
(declare-function telega-root--keep-cursor-at-chat "telega-root" (chat))

;;; Chatbuf vars
(defvar telega-chatbuf--ewoc nil
  "Ewoc for for the current chat buffer.")
(make-variable-buffer-local 'telega-chatbuf--ewoc)

(defvar telega-chatbuf--input-ring nil
  "The chat input history ring.")
(make-variable-buffer-local 'telega-chatbuf--input-ring)

(defvar telega-chatbuf--input-idx nil
  "The index to the current item in the chat input ring.")
(make-variable-buffer-local 'telega-chatbuf--input-idx)

(defvar telega-chatbuf--input-pending nil
  "Non-nil if last input is not yet commited.
Real value is the pending input string.")
(make-variable-buffer-local 'telega-chatbuf--input-pending)

(defvar telega-chatbuf--prompt-button nil "Input prompt button.")
(make-variable-buffer-local 'telega-chatbuf--prompt-button)

(defvar telega-chatbuf--history-loading nil
  "Non-nil if history has been requested.
Actual value is `:@extra` value of the call to load history.")
(make-variable-buffer-local 'telega-chatbuf--history-loading)

(defvar telega-chatbuf--vvnote-msg nil
  "Active (playing/paused) voice/video note message for the chatbuf.")
(make-variable-buffer-local 'telega-chatbuf--vvnote-msg)

(defvar telega-chatbuf--my-action nil
  "My current action in chat buffer.")
(make-variable-buffer-local 'telega-chatbuf--my-action)

(defvar telega-chatbuf--msg-filter nil
  "Active messages filter in the chatbuf.
Plist with properties:
- `:title' displayed in  chatbuf footer
- `:tdlib-msg-filter' Could be a function or list representing
  TDLib SearchMessagesFilter.
- `:query' - query argument to `telega--searchChatMessages'
- `:sender' - sender argument to `telega--searchChatMessages'
- `:msg-position' - Message's at point position across filtered messages.
- `:total-count' - Number of total messages found.")
(make-variable-buffer-local 'telega-chatbuf--msg-filter)

(defvar telega-chatbuf--messages-compact-view nil
  "Non-nil to use compact view for messages in chatbuf.")
(make-variable-buffer-local 'telega-chatbuf--messages-compact-view)
(defvar telega-chatbuf--inhibit-filter-reset nil
  "List of inhibits for the message filter reset.
Elements could be `msg-filter' and `thread'.")

(defvar telega-chatbuf--messages-pop-ring nil
  "History of messages jumps.
Used for `M-g x' command.
Bind it to nil to avoid pushing message to the ring when jumping
across messages.")
(make-variable-buffer-local 'telega-chatbuf--messages-pop-ring)

;; Special variable used to set `telega-chatbuf--chat'
;; inside `telega-chat-mode'
(defvar telega-chat--preparing-buffer-for)


(defun telega-chat--set-uaprops (chat uaprops)
  "Set CHAT's user application properties to UAPROPS."
  (plist-put chat :uaprops uaprops)
  (let ((client-data (if uaprops (prin1-to-string uaprops) "")))
    (plist-put chat :client_data client-data)
    (telega-server--send
     (list :@type "setChatClientData"
           :chat_id (plist-get chat :id)
           :client_data client-data))))

(defun telega-chat-color (chat)
  "Return colors pair associated with CHAT.
If there is no CHAT color, then generate new and assign it to CHAT."
  (let ((colors (telega-chat-uaprop chat :color)))
    (when (= (length colors) 3)
      ;; NOTE: Colors from telega<0.6.12, regenerate them
      (setq colors nil))

    (or colors
        (setf (telega-chat-uaprop chat :color)
              (or (when-let ((cc (cl-find chat telega-rainbow-color-custom-for
                                          :test #'telega-chat-match-p
                                          :key #'car)))
                    (list (cdr cc) (cdr cc)))
                  (let ((cid (telega-chat-title chat)))
                    (list (funcall telega-rainbow-color-function cid 'light)
                          (funcall telega-rainbow-color-function cid 'dark))))))
    ))

(defun telega--ordered-chats-insert (chat)
  "Insert CHAT into `telega--ordered-chats' according active sorter."
  (if (or (null telega--ordered-chats)
          (telega-chat> chat (car telega--ordered-chats)))
      ;; Insert at the head ?
      (setq telega--ordered-chats (cons chat telega--ordered-chats))

    ;; NOTE: Binary search the place where to insert, starting from
    ;; the very last item in the `telega--ordered-chats'
    (cl-assert (not (null telega--ordered-chats)))
    (let ((idx (1- (length telega--ordered-chats)))
          (place (last telega--ordered-chats 1)))
      (unless (telega-chat> (car place) chat)
        ;; Need binary search
        (setq place telega--ordered-chats)
        (while (not (zerop idx))
          (let* ((middle-idx (/ idx 2))
                 (middle-place (nthcdr middle-idx place)))
            (when (telega-chat> (car middle-place) chat)
              (setq place middle-place))
            (setq idx middle-idx))))

      (cl-assert place)
      (setcdr place (cons chat (cdr place)))))
  telega--ordered-chats)

(defun telega-chat--ensure (chat)
  "Ensure CHAT resides in `telega--chats' and `telega--ordered-chats'.
Return chat from `telega--chats'."
  (let ((chat-id (plist-get chat :id)))
    (or (gethash chat-id telega--chats)
        (prog1
            chat

          (puthash chat-id chat telega--chats)

          ;; parse :client_data as plist, we use it to store
          ;; additional chat properties (user application properties)
          ;; NOTE: plist might contain strings with surropagated
          ;; pairs, so `telega-tl-str' is used, see
          ;; https://github.com/zevlg/telega.el/issues/94
          (when-let ((client-data (telega-tl-str chat :client_data)))
            (ignore-errors
              (plist-put chat :uaprops (car (read-from-string client-data)))))

          ;; Place chat in correct place inside `telega--ordered-chats'
          ;; NOTE: `:uaprops' might have order for the chat, thats why
          ;; we put it in ordered list *after* constructing `:uaprops'
          (telega--ordered-chats-insert chat)
          ))))

(defun telega-chat-get (chat-id &optional offline-p)
  "Get chat by its CHAT-ID.
If OFFLINE-P is non-nil then do not request the telega-server."
  (let ((chat (gethash chat-id telega--chats)))
    (when (and (not chat) (not offline-p))
      (setq chat (telega--getChat chat-id))
      (cl-assert chat nil "getChat timed out chat_id=%d" chat-id)
      (telega-chat--ensure chat))
    chat))

(defun telega-chat-by-username (username)
  "Find chat by its USERNAME."
  (cl-find username telega--ordered-chats
           :test #'string= :key #'telega-chat-username))

(defun telega-chat-me ()
  "Chat with myself, a.k.a Saved Messages."
  ;; NOTE: Saved Messages has same id as me user
  (telega-chat-get telega--me-id 'offline))

(defun telega-chat--info (chat &optional locally-p)
  "Return info structure for the CHAT.
It could be user, secretChat, basicGroup or supergroup.
If LOCALLY-P is non-nil, then return nil if chat info is not available
without request to the server."
  (let ((chat-type (plist-get chat :type)))
    (cl-ecase (telega--tl-type chat-type)
      (chatTypePrivate
       (telega--info 'user (plist-get chat-type :user_id) locally-p))
      (chatTypeSecret
       (telega--info 'secretChat (plist-get chat-type :secret_chat_id) locally-p))
      (chatTypeBasicGroup
       (telega--info 'basicGroup (plist-get chat-type :basic_group_id) locally-p))
      (chatTypeSupergroup
       (telega--info 'supergroup (plist-get chat-type :supergroup_id) locally-p)))))
(defalias 'telega-chat--secretchat 'telega-chat--info)
(defalias 'telega-chat--basicgroup 'telega-chat--info)
(defalias 'telega-chat--supergroup 'telega-chat--info)

(defun telega-chat--type (chat)
  "Return type of the CHAT.
Types are: `private', `secret', `bot', `basicgroup', `supergroup' or `channel'."
  (or (plist-get chat :telega-chat-type)
      (let* ((chat-type (plist-get chat :type))
             (type-sym (pcase (plist-get chat-type :@type)
                         ("chatTypePrivate" 'private)
                         ("chatTypeBasicGroup" 'basicgroup)
                         ("chatTypeSupergroup" 'supergroup)
                         ("chatTypeSecret" 'secret)))
             (tc-type (cond ((and (eq type-sym 'supergroup)
                                  (plist-get chat-type :is_channel))
                             'channel)
                            ((and (eq type-sym 'private)
                                  (telega-user-bot-p (telega-chat-user chat)))
                             'bot)
                            (t type-sym))))
        ;; NOTE: chat type won't change, so we can cache calculated
        ;; chat type
        (plist-put chat :telega-chat-type tc-type)
        tc-type)))

(defsubst telega-chat-bot-p (chat)
  "Return non-nil if CHAT is the chat with bot."
  (eq (telega-chat--type chat) 'bot))

(defun telega-chat-private-p (chat)
  "Return non-nil if CHAT is private.
Chats with bots are also considered private, use `telega-chat-bot-p'
to check chat with bots."
  (memq (telega-chat--type chat) '(private bot)))

(defun telega-chat-channel-p (chat)
  "Return non-nil if CHAT is channel."
  (eq (telega-chat--type chat) 'channel))

(defun telega-chat-secret-p (chat)
  "Return non-nil if CHAT is secret."
  (eq (telega-chat--type chat) 'secret))

(defun telega-chat-muted-p (chat)
  "Return non-nil if CHAT is muted."
  (telega-chat-match-p chat 'muted))

(defun telega-chat-user (chat)
  "For private CHAT return corresponding user."
  (when-let ((user-id (telega--tl-get chat :type :user_id)))
    (telega-user-get user-id)))

(defun telega-chat-admin-get (chat user)
  "Return \"chatAdministrator\" TDLib object for the USER.
Return nil if USER not administrator in the CHAT.
Works only for chats with active chatbuffer and fetched
administrators list."
  (with-telega-chatbuf chat
    (cl-find (plist-get user :id) telega-chatbuf--administrators
             :key (telega--tl-prop :user_id))))

(defun telega-chat-member-my-status (chat)
  "Return my status as Chat Member Status for the CHAT.
Only available for basicgroup and supergroup (including channels)."
  (when (memq (telega-chat--type chat) '(basicgroup supergroup channel))
    (plist-get (telega-chat--info chat) :status)))

(defun telega-chat-member-my-permissions (chat)
  "Return my member permissions in the CHAT.
Combines chat permissions and admin/owner permissions."
  (let ((perms (copy-sequence (cddr (plist-get chat :permissions)))))
    (when-let ((status (telega-chat-member-my-status chat)))
      (cl-case (telega--tl-type status)
        (chatMemberStatusCreator
         ;; NOTE: Owner of the chat has all the admins privs except
         ;; for `:is_anonymous' which is set separately
         (dolist (perm-spec telega-chat--admin-permissions)
           (plist-put perms (car perm-spec) t))
         (plist-put perms :is_anonymous (plist-get status :is_anonymous)))

        (chatMemberStatusAdministrator
         (telega--tl-dolist ((pname pval) (plist-get status :rights))
           (plist-put perms pname pval)))

        (chatMemberStatusRestricted
         (setq perms (plist-get status :permissions)))

        (chatMemberStatusLeft
         (setq perms nil))))
    perms))

(defun telega-chat-title (chat &optional no-badges)
  "Return title for the CHAT.
If NO-BADGES is specified, then do not attach any chat badges at the
end of the title."
  (let* ((info (telega-chat--info chat 'offline))
         (raw-title (or (when (telega-me-p chat)
                          (telega-i18n "lng_saved_messages"))
                        (when (telega-replies-p chat)
                          (telega-i18n "lng_replies_messages"))
                        (telega-tl-str chat :title)))
         (chat-user (unless raw-title (telega-chat-user chat)))
         (title0 (if chat-user
                    (telega-user-title chat-user 'full-name)
                  ;; NOTE: Channels we are banned in can have empty title
                  (or raw-title (format "CHAT-%d" (plist-get chat :id)))))
         (title
          (if no-badges
              title0
            (concat title0
                    ;; Badges
                    (when (plist-get info :is_premium)
                      (telega-symbol 'premium))
                    (when (plist-get info :is_scam)
                      (propertize (telega-i18n "lng_scam_badge") 'face 'error))
                    (when (plist-get info :is_fake)
                      (propertize (telega-i18n "lng_fake_badge") 'face 'error))
                    (when (plist-get info :is_verified)
                      (telega-symbol 'verified))
                    (when (telega-chat-match-p chat 'is-blocked)
                      (telega-symbol 'blocked))
                    ))))

    (if-let ((cctfun (cdr (cl-find chat telega-chat-title-custom-for
                                   :test #'telega-chat-match-p
                                   :key #'car))))
        (progn
          (cl-assert (functionp cctfun))
          (funcall cctfun title))
      title)))

(defun telega-chat-reply-markup-msg (chat &optional callback)
  "Return reply markup for the CHAT."
  (declare (indent 1))
  (let ((reply-markup-msg-id (plist-get chat :reply_markup_message_id)))
    (unless (zerop reply-markup-msg-id)
      (telega-msg-get chat reply-markup-msg-id callback))))

(defun telega-chatbuf--reply-markup-message-fetch ()
  "Asynchronously load reply markup message for CHAT.
Pass non-nil OFFLINE-P argument to avoid any async requests."
  (let* ((chat telega-chatbuf--chat)
         (reply-markup-msg-id (plist-get chat :reply_markup_message_id)))
    (if (zerop reply-markup-msg-id)
        (telega-chatbuf--footer-update)

      ;; Async load reply markup message
      (telega-chat-reply-markup-msg chat
        (lambda (rm-message &optional offline-p)
          (unless offline-p
            (telega-msg-cache
             (or rm-message
                 ;; deleted message
                 (list :id reply-markup-msg-id
                       :chat_id (plist-get chat :id)
                       :telega-is-deleted-message t))))
          (with-telega-chatbuf chat
            (telega-chatbuf--footer-update)))))))

(defun telega-chatbuf--admins-fetch ()
  "Asynchronously fetch and update `telega-chatbuf--administrators'."
  (cl-assert telega-chatbuf--chat)

  ;; NOTE: fetch admins not frequent as one time in a minute, to avoid
  ;; admins fetch/supergroup full-info update loop, see
  ;; https://github.com/tdlib/td/issues/1284
  ;;
  ;; We store last update time in `admins' entry in
  ;; `telega-chatbuf--fetch-alist'
  ;;
  ;; Also, admin right is required to to get admins list in channels
  (let ((chat telega-chatbuf--chat)
        (last-fetch-time (or (alist-get 'admins telega-chatbuf--fetch-alist) 0))
        (current-time (time-to-seconds)))
    (when (and (> (- current-time last-fetch-time) 60)
               (not (telega-chat-private-p chat))
               (not (telega-chat-secret-p chat))
               (or (not (telega-chat-channel-p chat))
                   (telega-chat-match-p chat '(me-is-owner or-admin))))
      (setf (alist-get 'admins telega-chatbuf--fetch-alist) current-time)
      (telega--getChatAdministrators chat
        (lambda (admins)
          (with-telega-chatbuf chat
            (setq telega-chatbuf--administrators admins)))))
    ))

(defun telega-chatbuf--pinned-messages-fetch ()
  "Asynchronously fetch pinned messages for chatbuf."
  (let ((chat telega-chatbuf--chat))
    (telega--searchChatMessages telega-chatbuf--chat
        '(:@type "searchMessagesFilterPinned") 0 0
      :callback
      (lambda (reply)
        (let ((msgs (plist-get reply :messages)))
          (plist-put chat :telega-pinned-messages (append msgs nil))

          ;; Possible clamp index of pinned message displayed in
          ;; modeline
          (let ((pin-idx (plist-get chat :telega-pinned-message-index)))
            (unless (and pin-idx (< pin-idx (length msgs)))
              (plist-put chat :telega-pinned-message-index 0)))

          (with-telega-chatbuf chat
            (telega-chatbuf--chat-update "pinned-messages")))))))

(defun telega-chatbuf--active-stories-fetch ()
  "Asynchronously fetch active stories for the chatbuf."
  (when (and (telega-chatbuf-match-p 'has-active-stories)
             (telega-chatbuf-match-p telega-story-show-active-stories-for)
             (not (telega-chat--active-stories telega-chatbuf--chat)))
    (let ((chat telega-chatbuf--chat))
      (telega--getChatActiveStories chat
        (lambda (active-stories)
          (setf (telega-chat--active-stories chat) active-stories)
          (with-telega-chatbuf chat
            (telega-chatbuf--chat-update "active-stories")))))))

(defun telega-chatbuf--pinned-stories-fetch ()
  "Asynchronously fetch pinned stories for the chatbuf."
  (when (telega-chatbuf-match-p telega-story-show-pinned-stories-for)
    (cond ((telega-chatbuf-match-p 'has-pinned-stories)
           (telega--getChatPinnedStories telega-chatbuf--chat nil nil
             (let ((chat telega-chatbuf--chat))
               (lambda (stories)
                 (plist-put chat :telega-pinned-stories
                            (append (plist-get stories :stories) nil))
                 (with-telega-chatbuf chat
                   (telega-chatbuf--chat-update "pinned-stories"))))))

          ((plist-get telega-chatbuf--chat :telega-pinned-stories)
           (plist-put telega-chatbuf--chat :telega-pinned-stories nil)
           (telega-chatbuf--chat-update "pinned-stories")))))

(defun telega-chatbuf--sponsored-messages-fetch ()
  "Asynchronously fetch sponsored messages for the chatbuf."
  (let* ((chat telega-chatbuf--chat)
         (tsm-orig (plist-get chat :telega-sponsored-messages)))
    (plist-put chat :telega-sponsored-messages nil)
    (when (telega-chat-match-p chat '(type channel))
      (telega--getChatSponsoredMessages telega-chatbuf--chat
        (lambda (reply)
          (plist-put chat :telega-sponsored-messages reply)
          (unless (equal tsm-orig reply)
            (with-telega-chatbuf chat
              (telega-chatbuf--chat-update "sponsored-messages"))))))))

(defun telega-chat-group-call (&optional chat)
  "Return group call for the chatbuf's voice chat.
If CHAT is specified, return group call for the CHAT."
  (telega-group-call-get (telega--tl-get (or chat telega-chatbuf--chat)
                                         :video_chat :group_call_id)))

(defun telega-chatbuf--video-chat-fetch ()
  "Asynchronously fetch video chat state for the chatbuf."
  (let* ((chat telega-chatbuf--chat)
         (group-call-id (telega--tl-get chat :video_chat :group_call_id)))
    (if (zerop group-call-id)
        (telega-chatbuf--chat-update "group-call")

      (telega-group-call-get group-call-id
        (lambda (_group-call_ignored)
          (with-telega-chatbuf chat
            (telega-chatbuf--chat-update "group-call")))))))

(defun telega-chats-top (category)
  "Return list of top chats used by CATEGORY.
CATEGORY is one of `Users', `Bots', `Groups', `Channels',
`InlineBots', `Calls', `ForwardChats'."
  (let ((top (assq category telega--top-chats))
        (currts (time-to-seconds (current-time))))
    (when (> currts (+ (or (cadr top) 0) 60))
      ;; XXX update only if last fetch is older then 60 seconds
      (setq top (list (time-to-seconds (current-time))
                      (telega--getTopChats (symbol-name category))))
      (setf (alist-get category telega--top-chats) top))
    (caddr top)))


;;; Chat buttons in root buffer
(defvar telega-chat-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "i") 'telega-describe-chat)
    (define-key map (kbd "h") 'telega-describe-chat)
    (define-key map (kbd "a") 'telega-chat-add-member)
    (define-key map (kbd "o") 'telega-chat-set-custom-order)
    (define-key map (kbd "r") 'telega-chat-toggle-read)
    (define-key map (kbd "d") 'telega-chat-delete)
    (define-key map (kbd "P") 'telega-chat-toggle-pin)
    (define-key map (kbd "^") 'telega-chat-toggle-pin)
    (define-key map (kbd "C") 'telega-chat-call)
    (define-key map (kbd "DEL") 'telega-chat-delete)
    (define-key map (kbd "TAB") 'telega-chat-button-toggle-view)
    map)
  "The key map for telega chat buttons.")

(define-button-type 'telega-chat
  :supertype 'telega
  :inserter telega-inserter-for-chat-button
  :action #'telega-chat--pop-to-buffer
  'keymap telega-chat-button-map)

(defun telega-chat--pop-to-buffer (chat &optional no-history-load)
  "Pop to CHAT's buffer.
NO-HISTORY-LOAD is  passed directly to `telega-chatbuf--get-create'.
Uses `telega-chat--display-buffer-action' as action in `pop-to-buffer.'
Return chatbuf."
  (prog1
      (pop-to-buffer (telega-chatbuf--get-create chat no-history-load)
                     telega-chat--display-buffer-action)
    ;; Force switch-in for non-interactive buffer switching
    (telega--check-buffer-switch)))

(defun telega-chat-toggle-pin (chat)
  "Toggle chat's pin state at point."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))
  (telega--toggleChatIsPinned chat))

(defun telega-chat-add-member (chat user &optional forward-limit)
  "Add USER to the CHAT."
  (interactive (list (or telega-chatbuf--chat
                         telega--chat
                         (telega-chat-at (point)))
                     (telega-completing-read-user "Add member: ")))
  (cl-assert user)
  (telega--addChatMember chat user forward-limit))

(defun telega-chat-remove-member (chat user &optional _ban)
  "Remove USER from the CHAT.
Specify non-nil BAN to ban this user in this CHAT."
  (interactive
   (let ((chat (or telega-chatbuf--chat
                   telega--chat
                   (telega-chat-at (point)))))
     (list chat
           (telega-completing-read-user
               "Remove member: "
             (telega--searchChatMembers chat ""))
           current-prefix-arg)))

  ;; TODO: ban (chatMemberStatusBanned :banned_until_date)
  (telega--setChatMemberStatus
   chat user (list :@type "chatMemberStatusLeft")))

(defun telega-chat-set-title (chat title)
  "Set CHAT's title to TITLE."
  (interactive
   (let ((chat (or telega-chatbuf--chat (telega-chat-at (point)))))
     (list chat (read-string "New title: " (telega-chat-title chat)))))
  (telega--setChatTitle chat title))

(defun telega-chat-set-message-ttl (chat)
  "Interactively set CHAT's message TTL setting."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))

  (let* ((ttl-table `((,(telega-i18n "lng_ttl_about_duration1") . 86400)
                      (,(telega-i18n "lng_ttl_about_duration2") . 604800)
                      (,(telega-i18n "lng_ttl_about_duration3") . 2678400)
                      (,(telega-i18n "lng_settings_ttl_after_off") . 0)
                      (,(telega-i18n "lng_settings_ttl_after_custom") . -1)))
         (title (concat (telega-chat-title chat) " "
                        (telega-i18n "lng_settings_ttl_title")))
         (ttl (if (telega-chat-secret-p chat)
                  (ceiling (read-number
                            (concat title
                                    " (" (substring (telega-i18n "lng_seconds"
                                                      :count 2)
                                                    2 ))))
                (cdr (assoc (completing-read (concat title ": ")
                                             (mapcar #'car ttl-table) nil t)
                            ttl-table)))))
    (when (< ttl 0)
      ;; Custom message ttl setting, see
      ;; `telega--setChatMessageAutoDeleteTime' for ttl value
      ;; restrictions
      (setq ttl (* 86400 (read-number
                          (concat title " (0-"
                                  (telega-i18n "lng_settings_ttl_after_days"
                                    :count 365)
                                  "): ")))))

    (telega--setChatMessageAutoDeleteTime chat ttl)))

(defun telega-chat-set-custom-order (chat order)
  "For the CHAT (un)set custom ORDER."
  (interactive
   (let ((chat (or telega-chatbuf--chat (telega-chat-at (point)))))
     (list chat (read-string "Custom Order [empty to unset]: "
                             (telega-chat-order chat)))))
  (if (string-empty-p order)
      (setq order nil)
    (unless (numberp (read order))
      (error "Invalid order, must contain only digits")))

  (setf (telega-chat-uaprop chat :order) order)
  ;; NOTE: Update chat forcing reorder
  (telega-chat--update chat 'reorder))

(defun telega-chat-call (chat)
  "Call to the user associated with the given private CHAT."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))

  ;; NOTE: If calling to secret chat, then use ordinary private chat
  ;; for calling
  (when (telega-chat-secret-p chat)
    (setq chat (telega-chat-get
                (plist-get (telega-chat--info chat) :user_id))))

  (unless (and (telega-chat-private-p chat)
               (not (telega-chat-bot-p chat)))
    (error "Can call only to users"))
  (let* ((telega-full-info-offline-p nil)
         (user (telega-chat-user chat))
         (full-info (telega--full-info user)))
    (when (plist-get full-info :has_private_calls)
      (error "%s can't be called due to their privacy settings"
             (telega-msg-sender-title user
               :with-avatar-p t
               :with-username-p t)))
    (unless (plist-get full-info :can_be_called)
      (error "%s can't be called"
             (telega-msg-sender-title user
               :with-avatar-p t
               :with-username-p t)))

    (telega-voip-call user)))

(defun telega-chat-share-my-contact (chat)
  "Share my contact info with CHAT."
  (interactive (list telega-chatbuf--chat))
  (unless chat
    (user-error "`telega-chat-share-my-contact' available only in chatbuf"))
  (telega--sendMessage chat (list :@type "inputMessageContact"
                                  :contact (telega-user-as-contact
                                            (telega-user-me)))))

(defun telega-chat-unpin-all-messages (chat)
  "Unpin all messages in the CHAT."
  (interactive (list telega-chatbuf--chat))
  (telega--unpinAllChatMessages chat))

(defun telega-describe-chat--inserter (chat)
  "Inserter for the CHAT description."
  (let ((chat-ava (telega-msg-sender-avatar-image chat)))
    (telega-ins--image chat-ava 0
                       :no-display-if (not telega-chat-show-avatars))
    (telega-ins--msg-sender chat :with-username-p 'telega-username)
    (when (telega-chat-match-p chat 'is-blocked)
      (telega-ins--with-face 'error
        (telega-ins " " telega-symbol-blocked "BLOCKED")))
    (telega-ins "\n")
    (telega-ins--image chat-ava 1
                       :no-display-if (not telega-chat-show-avatars))
    (telega-ins (if (telega-chat-match-p chat 'is-broadcast-group)
                    (telega-i18n "lng_rights_gigagroup_title")
                  (capitalize (symbol-name (telega-chat--type chat))))
                " ")
    (telega-ins--button "Open"
      :value chat
      :action #'telega-chat--pop-to-buffer)
    (when (telega-me-p chat)
      (telega-ins " ")
      ;; [Set Profile Photo] button
      (telega-ins--button (telega-i18n "lng_settings_upload")
        'action (lambda (_ignored)
                  (telega--setProfilePhoto
                   (telega-read-file-name "Profile Photo: " nil nil t)))))
    (when (telega-chat-match-p chat '(my-permission :can_invite_users))
      (telega-ins " ")
      (telega-ins--button "Add Member"
        :value chat
        :action (lambda (to-chat)
                  (telega-chat-add-member
                   to-chat (telega-completing-read-user "Add member: ")))))
    (when (telega-chat-match-p chat '(my-permission :can_change_info))
      (telega-ins " ")
      (telega-ins--button (telega-i18n "lng_profile_set_group_photo")
        :value chat
        :action (lambda (for-chat)
                  (let ((photo (telega-read-file-name "Chat Photo: " nil nil t)))
                    ;; NOTE: create a local copy of the file in case
                    ;; docker is used, see https://t.me/emacs_telega/42209
                    (telega--setChatPhoto
                        for-chat (telega-file-local-copy photo))))))

    ;; Archive/Unarchive
    (telega-ins " ")
    (telega-ins--button (if (telega-chat-match-p chat 'archive)
                            (telega-i18n "lng_archived_remove")
                          (telega-i18n "lng_archived_add"))
      :value chat
      :action #'telega-chat-toggle-archive)
    (telega-ins "\n"))
  (when (telega-chat-match-p chat 'is-broadcast-group)
    (telega-ins--help-message
     (telega-ins-i18n "lng_rights_gigagroup_about")))

  (telega-ins-fmt "Id: %s\n"
    (if telega-debug
        (format "(telega-chat-get %d)" (plist-get chat :id))
      (format "%d" (plist-get chat :id))))
  (when (telega-chat-match-p chat 'is-public)
    (let ((link (concat (or (plist-get telega--options :t_me_url)
                            "https://t.me/")
                        (telega-chat-username chat))))
      (insert "Public Link: ")
      (telega-ins--raw-button (telega-link-props 'url link 'face 'link)
        (telega-ins link))
      (insert "\n")))
  (telega-ins "Internal Link: ")
  (let ((internal-link (telega-tme-internal-link-to chat)))
    (telega-ins--raw-button (telega-link-props 'url internal-link 'face 'link)
      (telega-ins internal-link)))
  (telega-ins "\n")

  (telega-ins "Order")
  (when (telega-chat-uaprop chat :order)
    (telega-ins " (" (propertize "custom" 'face 'telega-shadow) ")"))
  (telega-ins ": " (telega-chat-order chat) "\n")

  ;; Messages TTL setting for the chat
  (let ((ttl (or (telega-chat-match-p chat 'has-message-ttl) 0))
        (can-change-ttl-p (telega-chat-match-p chat
                            '(or (type private secret)
                                 (my-permission :can_change_info)))))
    (when (or (> ttl 0) can-change-ttl-p)
      (telega-ins (concat (telega-i18n "lng_settings_ttl_title") ": ")
                  (if (> ttl 0)
                      (telega-duration-human-readable ttl 2)
                    (telega-i18n "lng_manage_messages_ttl_never")))
      (when can-change-ttl-p
        (telega-ins " ")
        (telega-ins--button (telega-i18n "lng_manage_messages_ttl_after_custom")
          :value chat
          :action #'telega-chat-set-message-ttl))
      (telega-ins "\n")
      (telega-ins--help-message
       (cond ((telega-chat-match-p chat '(type private secret))
              (telega-ins-i18n "lng_ttl_edit_about"
                :user (telega-chat-title chat)))
             ((telega-chat-match-p chat '(type channel))
              (telega-ins-i18n "lng_ttl_edit_about_channel"))
             (t
              (telega-ins-i18n "lng_ttl_edit_about_group"))))
      ))

  (telega-ins "\n")
  ;; Chat notification settings
  (let* ((notify-cfg (plist-get chat :notification_settings))
         (muted-p (telega-chat-muted-p chat))
         (show-preview-p (telega-chat-notification-setting
                          chat :show_preview))
         (disable-pin-msg-p (telega-chat-notification-setting
                             chat :disable_pinned_message_notifications))
         (disable-mentions-p (telega-chat-notification-setting
                             chat :disable_mention_notifications)))
    (telega-ins
     (propertize (telega-i18n "lng_settings_section_notify") 'face 'bold))
    ;; If any custom setting is enabled, then show [Reset] button
    (unless (cl-every (apply-partially #'plist-get notify-cfg)
                      '(:use_default_mute_for
                        :use_default_sound
                        :use_default_show_preview
                        :use_default_disable_pinned_message_notifications
                        :use_default_disable_mention_notifications))
      (telega-ins " ")
      (telega-ins--button "Reset"
        :value chat
        :action (lambda (chat)
                  (telega--setChatNotificationSettings chat
                    :use_default_mute_for t
                    :use_default_sound t
                    :use_default_show_preview t
                    :use_default_disable_pinned_message_notifications t
                    :use_default_disable_mention_notifications t))))
    (telega-ins "\n")
    (telega-ins--labeled "  " nil
      (telega-ins--button (if (not muted-p)
                              telega-symbol-heavy-checkmark
                            telega-symbol-blank-button)
        :value chat
        :action #'telega-chat-toggle-muted)
      (telega-ins " ")
      (telega-ins-fmt "Enabled (%s)"
        (propertize (if (plist-get notify-cfg :use_default_mute_for)
                        "default" "custom")
                    'face 'telega-shadow))
      (unless muted-p
        (telega-ins " ")
        (telega-ins--button "Mute For"
          :value chat
          :action (lambda (chat)
                    (telega-chat-toggle-muted
                     chat (telega-completing-read-mute-for
                           "Disable notifications for: ")))))
      (telega-ins "\n")

      ;; Show Preview
      (telega-ins--button (if show-preview-p
                              telega-symbol-heavy-checkmark
                            telega-symbol-blank-button)
        :value chat
        :action (lambda (chat)
                  (telega--setChatNotificationSettings chat
                    :use_default_show_preview nil
                    :show_preview (if show-preview-p :false t))))
      (telega-ins " ")
      (telega-ins-fmt "Show Preview (%s)"
        (propertize (if (plist-get notify-cfg :use_default_show_preview)
                        "default" "custom")
                    'face 'telega-shadow))
      (telega-ins "\n")

      (telega-ins--button (if disable-pin-msg-p
                              telega-symbol-heavy-checkmark
                            telega-symbol-blank-button)
        :value chat
        :action (lambda (chat)
                  (telega--setChatNotificationSettings chat
                    :use_default_disable_pinned_message_notifications nil
                    :disable_pinned_message_notifications
                    (if disable-pin-msg-p :false t))))
      (telega-ins " ")
      (telega-ins-fmt "Disable Pinned Message Notification (%s)"
        (propertize (if (plist-get notify-cfg :use_default_disable_pinned_message_notifications)
                        "default" "custom")
                    'face 'telega-shadow))
      (telega-ins "\n")

      (telega-ins--button (if disable-mentions-p
                              telega-symbol-heavy-checkmark
                            telega-symbol-blank-button)
        :value chat
        :action (lambda (chat)
                  (telega--setChatNotificationSettings chat
                    :use_default_disable_mention_notifications nil
                    :disable_mention_notifications
                    (if disable-mentions-p :false t))))
      (telega-ins " ")
      (telega-ins-fmt "Disable Mention Notification (%s)"
        (propertize (if (plist-get notify-cfg :use_default_disable_mention_notifications)
                        "default" "custom")
                    'face 'telega-shadow))
      (telega-ins "\n")
      ))
  (when (telega-chat-match-p chat '(my-permission :can_send_basic_messages))
    (telega-ins "Default Disable Notification: ")
    (telega-ins--button (if (plist-get chat :default_disable_notification)
                            telega-symbol-heavy-checkmark
                          telega-symbol-blank-button)
      :value chat
      :action (lambda (for-chat)
                (telega--toggleChatDefaultDisableNotification
                 for-chat (not (plist-get chat :default_disable_notification)))))
    (telega-ins "\n")
    (telega-ins--help-message
     (telega-ins "Used when you send a message to the chat.\n"
                 "Disables message notification on receiver side.\n"
                 "Use `C-c C-a "
                 (if (plist-get chat :default_disable_notification)
                     "enable-notification"
                   "disable-notification")
                 " RET' in chatbuf prompt to temporary "
                 (if (plist-get chat :default_disable_notification)
                     "enable"
                   "disable")
                 " notifications on receiver side at message send time.")))

  ;; Permissions for basicgroup and supergroup
  (when (telega-chat-match-p chat '(type basicgroup supergroup))
    (let ((my-perms (telega-chat-member-my-permissions chat)))
      (telega-ins
       (propertize (telega-i18n "lng_manage_peer_permissions") 'face 'bold)
       "\n")
      (telega-ins
       (propertize (telega-i18n "lng_rights_default_restrictions_header")
                   'face 'telega-shadow)
       "\n")
      (telega-ins--labeled "  " nil
        (dolist (perm-spec telega-chat--chat-permissions)
          (let ((perm-value (telega--tl-get chat :permissions (car perm-spec))))
            (when (member perm-spec telega-chat--chat-media-permissions)
              (when (equal perm-spec (car telega-chat--chat-media-permissions))
                (telega-ins-i18n "lng_rights_chat_send_media")
                (telega-ins-fmt " %d/%d:\n"
                  (seq-count (lambda (media-perm-spec)
                               (telega-chat-match-p chat
                                 (list 'permission (car media-perm-spec))))
                             telega-chat--chat-media-permissions)
                  (length telega-chat--chat-media-permissions)))
              (telega-ins "  "))

            (if (plist-get my-perms :can_restrict_members)
                (telega-ins--button (if perm-value
                                        telega-symbol-heavy-checkmark
                                      telega-symbol-blank-button)
                  :value chat
                  :action (lambda (chat)
                            (telega--setChatPermissions chat
                              (car perm-spec) (not perm-value))))
              (telega-ins (if perm-value
                              telega-symbol-ballout-check
                            telega-symbol-ballout-empty)))
            (telega-ins " " (telega-i18n (cdr perm-spec)))
            (telega-ins "\n"))))))

  (telega-ins "\n")
  (let ((info-spec
         (assq (telega-chat--type chat)
               `((private ,(telega-i18n "lng_info_user_title")
                          telega-info--insert-user)
                 (bot ,(telega-i18n "lng_info_bot_title")
                      telega-info--insert-user)
                 (secret "SecretChat" telega-info--insert-secretchat)
                 (basicgroup "BasicGroup" telega-info--insert-basicgroup)
                 (supergroup "SuperGroup" telega-info--insert-supergroup)
                 (channel ,(telega-i18n "lng_info_channel_title")
                          telega-info--insert-supergroup)))))
    (cl-assert info-spec)
    (telega-ins--with-face 'bold
      (telega-ins (nth 1 info-spec)))
    (telega-ins "\n")
    (funcall (nth 2 info-spec) (telega-chat--info chat) chat))

  (when telega-debug
    (telega-ins "\n---DEBUG---\n")
    (telega-ins (propertize "Chat: " 'face 'bold)
                (format "%S" chat) "\n")
    (telega-ins (propertize "Info: " 'face 'bold)
                (format "%S" (telega-chat--info chat))))
  )

(defun telega-describe-chat (chat)
  "Show info about chat at point."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))
  (with-telega-help-win "*Telegram Chat Info*"
    (setq telega--chat chat)
    (telega-describe-chat--inserter chat)

    (setq telega--help-win-param chat)
    (setq telega--help-win-inserter #'telega-describe-chat--inserter)
    ))

(defun telega-describe-chat--maybe-redisplay (chat)
  "If CHAT info buffer exists and visible, then redisplay it."
  (telega-help-win--maybe-redisplay "*Telegram Chat Info*" chat))

(defun telega-chat-with (chat-or-user)
  "Start messaging with CHAT-OR-USER."
  (interactive
   ;; NOTE:
   ;;  - Include only known chats, i.e. in Main or Archive list
   ;;  - Include only contact users
   (let* ((chats (telega-sort-chats
                  telega-chat-completing-sort-criteria
                  (telega-filter-chats telega--ordered-chats 'is-known)))
          (users
           (cl-remove-if-not
            (lambda (user)
              (and (plist-get user :is_contact)
                   (not (telega-chat-get (plist-get user :id) 'offline))))
            (hash-table-values (alist-get 'user telega--info)))))
     (list (telega-completing-read-msg-sender
            "Chat with: " (nconc chats users)))))

  (when (telega-user-p chat-or-user)
    (setq chat-or-user (or (telega-chat-get (plist-get chat-or-user :id))
                           (telega--createPrivateChat chat-or-user))))

  (telega-chat--pop-to-buffer chat-or-user))

(defun telega-chat-join-by-link (link)
  "Join chat by invitation LINK."
  (interactive "sJoin chat by invite link: ")
  (telega-chat--pop-to-buffer (telega--joinChatByInviteLink link)))

(defun telega-chat-toggle-muted (chat &optional muted-for)
  "Toggle mute for the CHAT.
If MUTED-FOR is specified, set it as `:mute_for' notification setting."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))
  (telega--setChatNotificationSettings chat
    :use_default_mute_for nil
    :mute_for (or muted-for (if (telega-chat-muted-p chat)
                                0
                              telega-mute-for-ever))))

(defun telega-chat-toggle-archive (chat)
  "Archive or Unarchive CHAT."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))
  (telega--addChatToList
   chat (list :@type (if (telega-chat-match-p chat 'archive)
                         "chatListMain"
                       "chatListArchive"))))

(defun telega-chat-toggle-read (chat)
  "Toggle chat as read/unread."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))
  (let ((unread-count (plist-get chat :unread_count))
        (unread-mentions-count (plist-get chat :unread_mention_count))
        (unread-reactions-count (plist-get chat :unread_reaction_count))
        (marked-unread-p (plist-get chat :is_marked_as_unread)))
    (if (or (> unread-count 0) (> unread-mentions-count 0)
            (> unread-reactions-count 0) marked-unread-p)
        (progn
          ;; Toggle chat as readed
          (when marked-unread-p
            (telega--toggleChatIsMarkedAsUnread chat))
          (when (> unread-count 0)
            (telega--viewMessages chat (list (plist-get chat :last_message))
              :source '(:@type "messageSourceChatList")
              :force t))
          ;; NOTE: reading messages can change mentions count, so
          ;; force all mentions are read
          (telega--readAllChatMentions chat)
          (telega--readAllChatReactions chat))

      ;; Toggle chat is unread
      (unless marked-unread-p
        (telega--toggleChatIsMarkedAsUnread chat))
      )))

(defun telega-chats-filtered-kill-chatbuf ()
  "Kill chatbuf for all filtered chats."
  (interactive)
  (let ((filtered-chatbufs
         (cl-remove-if-not
          (lambda (buf)
            (telega-chat-match-active-p (telega-chatbuf--chat buf)))
          (telega-chat-buffers))))
    (unless filtered-chatbufs
      (user-error "No chats with chatbuf to kill"))
    (when (and (y-or-n-p (telega-i18n "telega_query_kill_chatbufs"
                           :count (length filtered-chatbufs)))
               ;; NOTE: If no filter is applied, ask once more time
               (or (not (telega-filter-default-p))
                   (y-or-n-p (telega-i18n "telega_query_kill_anyway"))))
      (dolist (buf filtered-chatbufs)
        (kill-buffer buf)))))

(defun telega-chats-filtered-toggle-read (&optional _force)
  "Apply `telega-chat-toggle-read' to all currently filtered chats."
  (interactive
   (list (y-or-n-p (telega-i18n "telega_query_read_chats"
                     :count (length telega--filtered-chats)))))
  ;; NOTE: If no filter is applied, ask once more time
  (when (or (not (telega-filter-default-p))
            (y-or-n-p (telega-i18n "telega_query_read_anyway")))
    (mapc 'telega-chat-toggle-read telega--filtered-chats)))

(defun telega-chat-leave (chat &optional keep-chatbuf)
  "Leave the CHAT."
  (interactive
   (list (or telega-chatbuf--chat (telega-chat-at (point))) nil))

  (cl-case (telega-chat--type chat)
    (secret (telega--closeSecretChat (telega-chat--info chat)))
    ((private bot) 'no-op)
    (t (telega--leaveChat chat)))

  ;; Kill corresponding chat buffer
  (unless keep-chatbuf
    (with-telega-chatbuf chat
      (kill-buffer (current-buffer)))))

(defun telega-chat-delete (chat)
  "Delete CHAT.
Query about everything.  Leaving CHAT, blocking corresponding
user and delete all the chat history.
Use `telega-chat-leave' to just leave the CHAT."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))
  (when (and chat
             (yes-or-no-p
              (concat (telega-i18n "telega_action_cant_undone") ".\n"
                      (telega-i18n "telega_query_delete_chat"
                        :title (telega-chat-title chat)))))
    (telega-chat-leave chat 'keep-chatbuf)
    (setq telega-deleted-chats
          (cl-pushnew chat telega-deleted-chats))

    ;; Block corresponding user, so he could not initiate any incoming
    ;; messages
    (when (and (telega-chat-private-p chat)
               (not (telega-chat-match-p chat '(is-blocked blockListMain))))
      (when (yes-or-no-p
             (concat (telega-i18n "lng_blocked_list_confirm_text"
                       :name (telega-chat-title chat))
                     " "))
        (telega-msg-sender-block chat)))

    (when (and (or (plist-get chat :can_be_deleted_only_for_self)
                   (plist-get chat :can_be_deleted_for_all_users))
               ;; NOTE: `telega--deleteChatHistory' cannot be used
               ;; in channels and public supergroups
               (not (telega-chat-match-p chat
                      '(or (type channel)
                           (and (type supergroup) is-public))))
               (telega-read-im-sure-p
                (telega-i18n "telega_query_delete_chat_history"
                  :title (telega-chat-title chat))))
      (let ((revoke-p (and (plist-get chat :can_be_deleted_for_all_users)
                           (y-or-n-p "Delete history for all members? "))))
        (telega--deleteChatHistory chat 'remove-from-list revoke-p)))

    ;; Kill corresponding chat buffer
    (with-telega-chatbuf chat
      (kill-buffer (current-buffer)))))

(defun telega-chat-create (chat-type)
  "Interactively create new chat of CHAT-TYPE.
CHAT-TYPE is one of \"basicgroup\", \"supergroup\", \"forum\", \"channel\",
\"secret\", \"location-supergroup\", \"location-channel\".
Return newly created chat."
  (interactive (list (funcall telega-completing-read-function
                              "Chat Type: "
                              (list "basicgroup" "supergroup" "forum"
                                    "channel" "secret"
                                    "location-supergroup" "location-channel")
                              nil t)))

  (cond ((string= chat-type "basicgroup")
         (let ((title (read-string "Chat Title: "))
               (users (telega-completing-read-user-list "Add users")))
           (telega--createNewBasicGroupChat
            title users #'telega-chat--pop-to-buffer)))

        ((string= chat-type "secret")
         (let ((user (telega-completing-read-user "Secret chat with: ")))
           (telega-chat--pop-to-buffer
            (telega--createNewSecretChat user))))

        (t
         ;; Supergroup
         (let ((title (read-string "Chat Title: "))
               (desc (read-string "Chat Description: "))
               (loc (when (or (string= chat-type "location-supergroup")
                              (string= chat-type "location-channel"))
                      (let ((chat-loc (telega-read-location "Chat Location"))
                            (chat-address (read-string "Chat Address: ")))
                        (list :@type "chatLocation"
                              :location (cons :@type (cons "location" chat-loc))
                              :address chat-address)))))

           (telega--createNewSupergroupChat title
             :forum-p (string= chat-type "forum")
             :channel-p (or (string= chat-type "channel")
                            (string= chat-type "location-channel"))
             :description desc
             :location loc
             :callback #'telega-chat--pop-to-buffer)))))

(defun telega-chat-upgrade-to-supergroup (chat)
  "Upgrade basic group CHAT from basicgroup to supergroup."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))
  (telega--upgradeBasicGroupChatToSupergroupChat
   chat #'telega-chat--pop-to-buffer))

(defun telega-chat-transfer-ownership (chat)
  "Transfer CHAT's ownership TO-USER."
  (declare (indent 1))
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))

  (let* ((admins (mapcar #'telega-user-get
                         (mapcar (telega--tl-prop :user_id)
                                 (telega--getChatAdministrators chat))))
        (to-user (telega-completing-read-user "To Admin: "
                   (cl-remove-if #'telega-me-p admins))))

    ;; NOTE: check chat ownership can be transferred
    (unless (eq (telega--tl-type (telega--canTransferOwnership))
                'canTransferOwnershipResultOk)
      (user-error (concat
                   (telega-i18n "lng_rights_transfer_check_about"
                     :user (telega-msg-sender-title to-user
                             :with-avatar-p t
                             :with-username-p t))
                   "\n"
                   (telega-i18n "lng_rights_transfer_check_session") "\n"
                   (telega-i18n "lng_rights_transfer_check_password") "\n"
                   (telega-i18n "lng_rights_transfer_check_later"))))

    (unless (telega-read-im-sure-p
             (concat (telega-i18n "lng_rights_transfer_about"
                       :group (telega-chat-title chat)
                       :user (telega-msg-sender-title to-user
                               :with-avatar-p t
                               :with-username-p t))
                     "\n"
                     (telega-i18n "lng_rights_transfer_sure") "?"))
      (user-error "Ownership transfer canceled"))

    (let ((pass (password-read
                 (concat (telega-i18n "lng_rights_transfer_password_description")
                         "\n" "Telegram Password: "))))
      (telega--transferChatOwnership chat to-user pass
        (lambda (result)
          (when (eq (telega--tl-type result) 'ok)
            (message
             (telega-i18n (if (telega-chat-channel-p chat)
                              "lng_rights_transfer_done_channel"
                            "lng_rights_transfer_done_group")
               :user (telega-msg-sender-title to-user
                       :with-avatar-p t
                       :with-username-p t)))))))
    ))

(defun telega-chat-set-description (chat descr)
  "Update CHAT's description."
  (interactive (let* ((chat (or telega-chatbuf--chat (telega-chat-at (point))))
                      (telega-full-info-offline-p nil)
                      (full-info (telega--full-info (telega-chat--info chat))))
                 (list chat
                       (read-string "Description: "
                                    (telega-tl-str full-info :description)))))
  (telega--setChatDescription chat descr))

(defun telega-chat-report (chat reason)
  "Report a CHAT having inappropriate content."
  (interactive
   (list (or telega-chatbuf--chat (telega-chat-at (point)))
         (apply telega-completing-read-function
                "Report Reason: "
                '("Spam" "Violence" "Pornography" "ChildAbuse"
                  "Copyright" "UnrelatedLocation" "Fake" "Custom")
                nil t)))
  (let ((text (when (string= "Custom" reason)
                (read-string "Custom Report Reason: "))))
    (telega--reportChat chat reason text)))

(defun telega-chats-filtered-delete (&optional force)
  "Apply `telega-chat-delete' to all currently filtered chats.
Do it only if FORCE is non-nil."
  (interactive (list (yes-or-no-p
                      (format "%s.\nDelete %d chats? "
                              (telega-i18n "telega_action_cant_undone")
                              (length telega--filtered-chats)))))
  (when force
    (mapc #'telega-chat-delete telega--filtered-chats)))

(defun telega-saved-messages (arg)
  "Switch to \"Saved Messages\" chat buffer.
If \"Saved Messages\" chat is not opened, then open it.
If `\\[universal-argument]' is specified, then goto prompt otherwise
keep the point, where it is."
  (interactive "P")
  (telega-chat--pop-to-buffer (telega-chat-me))
  (when arg
    (goto-char (point-max))))

(defun telega-switch-buffer (chat)
  "Interactively switch to CHAT's buffer.
Switch only if CHAT is opened, i.e. has corresponding chatbuf."
  (interactive
   (list (progn
           (unless telega--chat-buffers-alist
             (user-error "No chatbufs to switch"))
           (telega-completing-read-msg-sender
            "Telega chat: "
            ;; NOTE: if current buffer is chatbuf, then exclude it
            ;; from the list, because it is strange if it appers first
            ;; in the list
            (let ((telega-sort--inhibit-order t))
              (telega-sort-chats
               telega-chat-switch-buffer-sort-criteria
               (delq telega-chatbuf--chat
                     (mapcar #'car telega--chat-buffers-alist))))))))
  (let ((buffer (cdr (assq chat telega--chat-buffers-alist))))
    (unless buffer
      (user-error "telega: Chat has no corresponding chatbuf"))
    (switch-to-buffer buffer)))

(defun telega-switch-important-chat (chat)
  "Switch to important CHAT if any.
If `\\[universal-argument] is used, then select first chat if
multiple chats are important."
  (interactive
   (list (let ((ichats (telega-filter-chats telega--ordered-chats 'important)))
           (cond ((null ichats)
                  (user-error "No important chats"))
                 ((or (= 1 (length ichats)) current-prefix-arg)
                  (car ichats))
                 (t
                  (telega-completing-read-chat "Important Chat: " ichats))))))

  (telega-chat--pop-to-buffer chat))

(defun telega-switch-unread-chat (chat)
  "Switch to next unread message in next unread CHAT.
CHAT considered unread if matches `telega-unread-chat-temex'."
  (interactive
   (list (or (car (telega-filter-chats telega--ordered-chats
                    telega-unread-chat-temex))
             (user-error "No unread chats"))))

  (let ((last-msg-id (or (plist-get chat :last_read_inbox_message_id) 0)))
    (if (zerop last-msg-id)
        (telega-chat--pop-to-buffer chat)
      (telega-chat--goto-msg chat last-msg-id 'highlight))))


;;; Chat Buffer
(defcustom telega-chat-preview-mode-from-last-message t
  "Non-nil to load history from last message in preview mode."
  :type 'boolean
  :group 'telega-modes)

(defvar telega-chat-preview-mode-lighter
  (concat " " (telega-symbol 'mode) "Preview")
  "Lighter for the `telega-chat-preview-mode'.")

(define-minor-mode telega-chat-preview-mode
  "Minor mode to preview chat history."
  :init-value nil
  :lighter telega-chat-preview-mode-lighter

  (if telega-chat-preview-mode
      (telega--closeChat telega-chatbuf--chat)
    (telega--openChat telega-chatbuf--chat))

  (telega-chatbuf--chat-update "preview-mode"))

(defvar telega-chatbuf--origin-recenter-command
  (lookup-key global-map (kbd "C-l"))
  "Command used to run on `C-l'.")

(defvar telega-chat-mode-hook nil
  "Hook run when telega chat buffer is created.")

(defvar telega-chat-dnd-protocol-alist
  '(("^file:"     . telega-chatbuf-dnd-attach)
    ("^https?://" . telega-chatbuf-dnd-attach))
  "The functions to call when a drop in chatbuf is made.
See `dnd-protocol-alist' for more information.  When nil, behave
as in other buffers.  Changing this option is effective only for
new Chat buffers.")
(defvar dnd-protocol-alist)

(defvar telega-chatbuf-fastnav-map
  (let ((map (make-sparse-keymap)))
    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-next-unread-reaction,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-next-unread-reaction, 2)}}}
    (define-key map (kbd "!") 'telega-chatbuf-next-unread-reaction)
    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-goto-date,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-goto-date, 2)}}}
    (define-key map (kbd "d") 'telega-chatbuf-goto-date)
    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-history-beginning,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-history-beginning, 2)}}}
    (define-key map (kbd "<") 'telega-chatbuf-history-beginning)
    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-read-all,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-read-all, 2)}}}
    (define-key map (kbd ">") 'telega-chatbuf-read-all)
    (define-key map (kbd "r") 'telega-chatbuf-read-all)
    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-next-unread-mention,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-next-unread-mention, 2)}}}
    (define-key map (kbd "m") 'telega-chatbuf-next-unread-mention)
    (define-key map (kbd "@") 'telega-chatbuf-next-unread-mention)
    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-next-unread,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-next-unread, 2)}}}
    (define-key map (kbd "u") 'telega-chatbuf-next-unread)
    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-goto-pinned-message,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-goto-pinned-message, 2)}}}
    (define-key map (kbd "P") 'telega-chatbuf-goto-pinned-message)
    (define-key map (kbd "^") 'telega-chatbuf-goto-pinned-message)
    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-goto-pop-message,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-goto-pop-message, 2)}}}
    (define-key map (kbd "x") 'telega-chatbuf-goto-pop-message)
    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-next-favorite,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-next-favorite, 2)}}}
    ;;   See [[#favorite-messages][Favorite Messages]] for details.
    (define-key map (kbd "*") 'telega-chatbuf-next-favorite)
    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-goto-video-chat,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-goto-video-chat, 2)}}}
    ;;   See [[#video-chats][Video Chats]] for details.
    (define-key map (kbd "v") 'telega-chatbuf-goto-video-chat)

    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-inplace-search,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-inplace-search, 2)}}}
    (define-key map (kbd "s") 'telega-chatbuf-inplace-search)
    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-inplace-search-next,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-inplace-search-next, 2)}}}
    (define-key map (kbd "n") 'telega-chatbuf-inplace-search-next)
    ;;; ellit-org: chatbuf-fastnav-bindings
    ;; - {{{where-is(telega-chatbuf-inplace-search-prev,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-inplace-search-prev, 2)}}}
    (define-key map (kbd "p") 'telega-chatbuf-inplace-search-prev)
    map)
  "Keymap for fast navigation commands in the chatbuf.")

(defvar telega-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") 'telega-chatbuf-recenter-1)

    ;; C-M-[ - cancels edit/reply
    (define-key map (kbd "\e\e") 'telega-chatbuf-cancel-dwim)
    (define-key map (kbd "C-c C-k") 'telega-chatbuf-cancel-dwim)
    (define-key map (kbd "C-M-c") 'telega-chatbuf-cancel-dwim)
    (define-key map (kbd "C-M-a") 'telega-chatbuf-beginning-of-thing)

    (define-key map (kbd "C-c ?") 'telega-describe-chat)

    (define-key map (kbd "RET") 'telega-chatbuf-newline-or-input-send)
    (define-key map (kbd "M-p") 'telega-chatbuf-edit-prev)
    (define-key map (kbd "M-n") 'telega-chatbuf-edit-next)
    (define-key map (kbd "M-r") 'telega-chatbuf-input-search)

    ;;; ellit-org: chatbuf-attach-bindings
    ;; - {{{where-is(telega-chatbuf-attach,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-attach,2)}}}
    (define-key map (kbd "C-c C-a") 'telega-chatbuf-attach)
    ;;; ellit-org: chatbuf-attach-bindings
    ;; - {{{where-is(telega-chatbuf-attach-media,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-attach-media,2)}}}
    (define-key map (kbd "C-c C-f") 'telega-chatbuf-attach-media)
    ;;; ellit-org: chatbuf-attach-bindings
    ;; - {{{where-is(telega-chatbuf-attach-clipboard,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-attach-clipboard,2)}}}
    (define-key map (kbd "C-c C-v") 'telega-chatbuf-attach-clipboard)

    ;;; ellit-org: chatbuf-filtering-bindings
    ;; - {{{where-is(telega-chatbuf-filter,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-filter,2)}}}
    (define-key map (kbd "C-c /") 'telega-chatbuf-filter)
    ;;; ellit-org: chatbuf-filtering-bindings
    ;; - {{{where-is(telega-chatbuf-filter-cancel,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-filter-cancel, 2)}}}
    (define-key map (kbd "C-c C-c") 'telega-chatbuf-filter-cancel)
    ;;; ellit-org: chatbuf-filtering-bindings
    ;; - {{{where-is(telega-chatbuf-inplace-search-query,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-inplace-search-query, 2)}}}
    (define-key map (kbd "C-c C-r") 'telega-chatbuf-inplace-search-query)
    ;;; ellit-org: chatbuf-filtering-bindings
    ;; - {{{where-is(telega-chatbuf-inplace-search-query-forward,telega-chat-mode-map)}}} ::
    ;;   {{{fundoc(telega-chatbuf-inplace-search-query-forward, 2)}}}
    (define-key map (kbd "C-c C-s") 'telega-chatbuf-inplace-search-query-forward)

    ;; jumping around links
    (define-key map (kbd "TAB") 'telega-chatbuf-complete-or-next-link)
    (define-key map (kbd "<backtab>") 'telega-chatbuf-prev-link)

    ;; Additional prefix keymaps
    (define-key map (kbd "M-g") telega-chatbuf-fastnav-map)
    map))

(define-button-type 'telega-prompt
  :supertype 'telega
  :inserter 'telega-ins
  'face 'telega-chat-prompt
  'rear-nonsticky t

  ;; NOTE: To make input method works under message buttons,
  ;; See `quail-input-method' for details
  'read-only t
  'front-sticky t

  'cursor-intangible t
  'field 'telega-prompt)

(defun telega-chatbuf--first-msg ()
  "Return first message inserted in chat buffer."
  ;; Find first non-telegaMessage message in the chatbuf
  ;; telegaMessage have :id = -1
  (let ((node (ewoc-nth telega-chatbuf--ewoc 0)))
    (while (and node (< (plist-get (ewoc-data node) :id) 0))
      (setq node (ewoc-next telega-chatbuf--ewoc node)))
    (when node
      (ewoc-data node))))

(defun telega-chatbuf--last-msg ()
  "Return last message inserted in chat buffer."
  ;; Find last non-telegaMessage message in the chatbuf
  ;; telegaMessage have :id = -1
  (let ((node (ewoc-nth telega-chatbuf--ewoc -1)))
    (while (and node (< (plist-get (ewoc-data node) :id) 0))
      (setq node (ewoc-prev telega-chatbuf--ewoc node)))
    (when node
      (ewoc-data node))))

(defun telega-chatbuf--last-message-id ()
  "Return id of the last message in the chatbuf.
Take threads and topics into account."
  (or (when-let ((topic (telega-chatbuf--thread-topic)))
        (telega--tl-get topic :last_message :id))
      (when-let ((thread-msg (telega-chatbuf--thread-msg)))
        (telega--tl-get thread-msg
                        :interaction_info :reply_info :last_message_id))
      (telega--tl-get telega-chatbuf--chat :last_message :id)))

(defun telega-chatbuf--last-msg-loaded-p ()
  "Return non-nil if chat's last message is shown."
  (or (telega-chatbuf--history-state-get :newer-loaded)
      (let ((last-chat-msg-id (telega-chatbuf--last-message-id))
            (last-chatbuf-msg-id (plist-get (telega-chatbuf--last-msg) :id)))
        (or (and last-chat-msg-id last-chatbuf-msg-id
                 (<= last-chat-msg-id last-chatbuf-msg-id))
            ;; Or no messages in the chat at all
            (and (not last-chat-msg-id) (not last-chatbuf-msg-id))))))

(defun telega-chatbuf--last-read-inbox-msg-id ()
  "Return last read inbox message id.
Takes into account `telega-chatbuf--thread-msg'."
  (or (when-let ((topic (telega-chatbuf--thread-topic)))
        ;; NOTE: topics are not yet fully supported by TDLib,
        ;; so `:last_read_inbox_message_id' might be 0
        (plist-get topic :last_read_inbox_message_id))

      (when-let* ((thread-msg (telega-chatbuf--thread-msg))
                  ;; NOTE: `:last_read_inbox_message_id'==0 in thread
                  ;; means nothing has been read in this thread yet
                  (thread-last-read-msg-id
                   (telega--tl-get thread-msg
                                   :interaction_info :reply_info
                                   :last_read_inbox_message_id)))
        (if (zerop thread-last-read-msg-id)
            (plist-get thread-msg :id)
          thread-last-read-msg-id))

      (plist-get telega-chatbuf--chat :last_read_inbox_message_id)))

(defun telega-chatbuf--unread-message-count ()
  "Return number of unread message in the chatbuf.
Takes into account thread/topic."
  (if-let ((thread-msg (telega-chatbuf--thread-msg)))
      ;; HACK: since thread's info does not have update event in
      ;; TDLib, it's `:unread_message_count' is not reliable, so we
      ;; apply heuristics to determines number of unread messages in
      ;; the thread.
      (let* ((unread-count
              (plist-get (telega-chatbuf--thread-info)
                         :unread_message_count))
             (reply-info
              (telega--tl-get thread-msg :interaction_info :reply_info))
             (last-read-msg-id
              (plist-get reply-info :last_read_inbox_message_id))
            (last-msg-id
             (plist-get reply-info :last_message_id))
            (heuristic-unread-count
             (/ (- last-msg-id last-read-msg-id) telega-msg-id-step)))
        (if (< heuristic-unread-count unread-count)
            heuristic-unread-count
          unread-count))

    (plist-get (or (telega-chatbuf--thread-topic) telega-chatbuf--chat)
               :unread_count)))

(defun telega-chatbuf--unread-mention-count ()
  "Return number of messages with unread mentions in the chatbuf.
Takes into account thread/topic."
  ;; TODO: ordinary threads support
  (plist-get (or (telega-chatbuf--thread-topic) telega-chatbuf--chat)
             :unread_mention_count))

(defun telega-chatbuf--unread-reaction-count ()
  "Return number of messages with unread reactions in the chatbuf.
Takes into account thread/topic."
  ;; TODO: ordinary threads support
  (plist-get (or (telega-chatbuf--thread-topic) telega-chatbuf--chat)
             :unread_reaction_count))

(defun telega-chatbuf--msg-observable-p (msg &optional node)
  "Return non-nil if MSG is observable in the chatbuf."
  (unless node
    (setq node (telega-chatbuf--node-by-msg-id (plist-get msg :id))))
  (when node
    (telega-button--observable-p (ewoc-location node))))

(defun telega-chatbuf--msg-view (msg &optional temex)
  "View message MSG in the chatbuf.
View only if message matches TEMEX or not yet viewed."
  (unless temex
    (setq temex '(or (prop :contains_unread_mention)
                     unread-reactions
                     (not seen))))

  (when (and (not (telega-msg-internal-p msg))
             (telega-msg-match-p msg temex))
    (telega--viewMessages telega-chatbuf--chat (list msg)
      :source (cond (telega-chat-preview-mode
                     '(:@type "messageSourceHistoryPreview"))
                    ((telega-chatbuf--thread-topic)
                     '(:@type "messageSourceForumTopicHistory"))
                    ((telega-chatbuf--thread-msg)
                     '(:@type "messageSourceMessageThreadHistory"))
                    (t '(:@type "messageSourceChatHistory"))))))

(defun telega-chatbuf--manage-point (&optional point only-prompt-p)
  "Manage current chatbuf's point.
Possibly view some messages at point."
  (let* ((point (or point (point)))
         (message (or (unless only-prompt-p (telega-msg-at point))
                      (when (>= point telega-chatbuf--input-marker)
                        (telega-chatbuf--last-msg)))))
    (when message
      (telega-chatbuf--msg-view message))))

;; Dirtiness collections for different parts of a chatbuf
(defvar telega-chatbuf--dirtiness-header-line nil)
(make-variable-buffer-local 'telega-chatbuf--dirtiness-header-line)
(defvar telega-chatbuf--dirtiness-mode-line nil)
(make-variable-buffer-local 'telega-chatbuf--dirtiness-mode-line)
(defvar telega-chatbuf--dirtiness-footer nil)
(make-variable-buffer-local 'telega-chatbuf--dirtiness-footer)
(defvar telega-chatbuf--dirtiness-prompt nil)
(make-variable-buffer-local 'telega-chatbuf--dirtiness-prompt)

(defvar telega-chatbuf--dirtiness-symbol nil
  "Bind this to initialize dirtiness.")
(defmacro telega-chatbuf--dirtiness-init (&rest dirtiness)
  "Initialize dirtiness.
Add DIRTINESS into the variable denoted by `telega-chatbuf--dirtiness-symbol'."
  `(when telega-chatbuf--dirtiness-symbol
     (set telega-chatbuf--dirtiness-symbol
          (append (symbol-value telega-chatbuf--dirtiness-symbol)
                  (quote ,dirtiness)))))

(defun telega-chatbuf-header-concat (&rest header-format)
  "If all strings in a HEADER-FORMAT is non-empty return HEADER-FORMAT.
If HEADER-FORMAT contains at least one empty string, return nil.
Use this to surrond header with some prefix and suffix."
  (when (seq-every-p (lambda (elem)
                       (and elem (not (string-empty-p elem))))
                     header-format)
    (apply #'concat header-format)))

(defun telega-chatbuf-header-msg-filter (&optional no-cancel-button-p)
  "Format message filter for the chatbuf."
  (telega-chatbuf--dirtiness-init
   "msg-filter")

  (when telega-chatbuf--msg-filter
    (telega-ins--as-string
     (unless no-cancel-button-p
       (telega-ins--button (propertize "✕" 'face 'bold)
         'action (lambda (_ignored)
                   (telega-chatbuf-filter-cancel)))
       (telega-ins " "))
     (telega-ins "Messages Filter: ")
     (telega-ins--with-face 'bold
       (telega-ins (telega-tl-str telega-chatbuf--msg-filter :title)))
     (when-let ((sender (plist-get telega-chatbuf--msg-filter :sender)))
       (telega-ins " by ")
       (telega-ins--raw-button
           (telega-link-props 'sender sender 'type 'telega)
         (telega-ins--msg-sender sender
           :with-avatar-p t
           :with-username-p t
           :with-brackets-p t)))
     (when-let ((total (plist-get telega-chatbuf--msg-filter :total-count)))
       (telega-ins-fmt " (total: %d)" total)))))

(defun telega-chatbuf-header-preview-mode ()
  (telega-chatbuf--dirtiness-init "preview-mode")
  (when telega-chat-preview-mode
    telega-chat-preview-mode-lighter))

(defun telega-chatbuf-header-highlight-text ()
  (telega-chatbuf--dirtiness-init "highlight-text")
  (when telega-highlight-text-mode
    (concat telega-highlight-text-mode-lighter ": "
            (propertize telega-highlight-text-regexp
                        'face 'telega-highlight-text-face))))

(defun telega-chatbuf-header-thread (&optional max-width)
  "Formatter for the chatbuf's thread."
  (telega-chatbuf--dirtiness-init "thread")

  (when telega-chatbuf--thread
    (telega-ins--as-string
     (telega-ins--with-attrs (list :max (or max-width 40)
                                   :align 'left :elide t)
       (if-let ((topic (telega-chatbuf--thread-topic)))
           (progn
             ;; (telega-ins (telega-i18n "lng_forum_topic_title") ": ")
             (telega-ins--with-face 'telega-shadow
               (telega-ins (telega-symbol 'right-arrow) (telega-symbol 'topic)))
             (telega-ins--topic-title topic 'with-icon))

         (telega-ins "Thread: ")
         (telega-ins--content-one-line (telega-chatbuf--thread-msg)))))))

(defun telega-ins--chat-action-bar-button (chat action-bar)
  "Inserter for the TL ChatActionBar."
  (declare (indent 1))
  (cl-ecase (telega--tl-type action-bar)
    (chatActionBarReportSpam
     (telega-ins--button (if (telega-chat-match-p chat
                               '(type basicgroup supergroup channel))
                             (telega-i18n "lng_report_spam_and_leave")
                           (telega-i18n "lng_report_spam"))
       'action (lambda (_ignore)
                 (telega--reportChat chat "Spam")
                 (telega-chat-delete chat))))

    (chatActionBarReportUnrelatedLocation
     (telega-ins--button (telega-i18n "lng_report_location")
       'action (lambda (_ignore)
                 (telega--reportChat chat "UnrelatedLocation"))))

    (chatActionBarInviteMembers
     (telega-ins--button "Invite Users"
       'action (lambda (_ignored)
                 (let ((new-users (telega-completing-read-user-list
                                      "Invite new users")))
                   (dolist (user new-users)
                     (telega-chat-add-member chat user))))))

    (chatActionBarReportAddBlock
     (telega-ins--chat-action-bar-button chat
       '(:@type "chatActionBarReportSpam"))
     (telega-ins " ")
     (telega-ins--chat-action-bar-button chat
       '(:@type "chatActionBarAddContact"))
     (telega-ins " ")
     (telega-ins--button (telega-i18n "lng_new_contact_block")
       :value (telega-chat-user chat)
       :action #'telega-msg-sender-block))

    (chatActionBarAddContact
     (telega-ins--button (telega-i18n "lng_new_contact_add")
       :value (telega-user-as-contact (telega-chat-user chat))
       :action #'telega--addContact))

    (chatActionBarSharePhoneNumber
     (telega-ins--button (telega-i18n "lng_new_contact_share")
       :value (telega-chat-user chat)
       :action #'telega--sharePhoneNumber))

    (chatActionBarJoinRequest
     (telega-ins-i18n (if (plist-get action-bar :is_channel)
                          "lng_new_contact_from_request_channel"
                        "lng_new_contact_from_request_group")
       :user (telega-ins--as-string
              (telega-ins--msg-sender chat
                :with-avatar-p t
                :with-username-p t
                :with-brackets-p t))
       :name (telega-tl-str action-bar :title))
     (telega-ins " ")
     (telega-ins--date-full (plist-get action-bar :request_date))
     )))

(defun telega-chatbuf-footer-action-bar ()
  "Formatter for the action bar."
  (telega-chatbuf--dirtiness-init "updateChatActionBar")

  (when-let ((action-bar (plist-get telega-chatbuf--chat :action_bar)))
    (telega-ins--as-string
     (telega-ins--button "✕"
       :value telega-chatbuf--chat
       :action #'telega--removeChatActionBar)
     (telega-ins " ActionBar: ")
     (telega-ins--chat-action-bar-button telega-chatbuf--chat action-bar)
     )))

(defun telega-chatbuf-footer-active-stories ()
  "Formatter for the chatbuf's active stories."
  (telega-chatbuf--dirtiness-init
   "active-stories"
   "updateChatActiveStories")

  (when (and (telega-chatbuf-match-p telega-story-show-active-stories-for)
             (not (plist-get telega-chatbuf--hidden-headers :active-stories)))
    (when-let* ((active-stories
                 (telega-chat--active-stories telega-chatbuf--chat))
                (stories (plist-get active-stories :stories)))
      (unless (seq-empty-p stories)
        (telega-ins--as-string
         (telega-ins--button "✕"
           'action (lambda (_ignored)
                     (plist-put telega-chatbuf--hidden-headers :active-stories t)
                     (telega-chatbuf--chat-update "active-stories")))
         (telega-ins " " (telega-i18n "lng_stories_row_count"
                           :count (seq-length stories))
                     ":")
         (seq-doseq (story-info stories)
           (if-let* ((chat-id (plist-get telega-chatbuf--chat :id))
                     (story-id (plist-get story-info :story_id))
                     (story (telega-story-get chat-id story-id 'offline)))
               (progn
                 (telega-ins " ")
                 (telega-button--insert 'telega story
                   :inserter #'telega-ins--story-one-line
                   :action #'telega-story-open
                   'help-echo
                   (telega-ins--as-string
                    (telega-ins--one-lined
                     (telega-ins--fmt-text (plist-get story :caption))))))

             ;; NOTE: need to fetch story structure to display it
             (telega--getStory chat-id story-id nil
               (lambda (story)
                 (with-telega-chatbuf (telega-story-chat story)
                   (telega-chatbuf--chat-update "active-stories"))))))
         )))))

(defun telega-chatbuf-footer-pinned-stories ()
  (telega-chatbuf--dirtiness-init
   "pinned-stories"
   "updateChatActiveStories"            ; For story's `seen' status
   )

  (when (and (telega-chatbuf-match-p telega-story-show-pinned-stories-for)
             (not (plist-get telega-chatbuf--hidden-headers :pinned-stories)))
    (when-let ((pinned-stories
                (plist-get telega-chatbuf--chat :telega-pinned-stories)))
      (telega-ins--as-string
       (telega-ins--button "✕"
         'action (lambda (_ignored)
                   (plist-put telega-chatbuf--hidden-headers :pinned-stories t)
                   (telega-chatbuf--chat-update "active-stories")))
       (telega-ins (telega-symbol 'pin) " "
                   (telega-i18n "lng_stories_row_count"
                     :count (seq-length pinned-stories))
                   ": ")
       (seq-doseq (story pinned-stories)
         (telega-button--insert 'telega story
           :inserter (lambda (story)
                       (telega-ins--story-content-one-line story nil t))
           :action #'telega-story-open))))))

(defun telega-chatbuf-footer-active-vvnote ()
  "Formatter for the active voice/video note currently playing in the chatbuf."
  (when telega-chatbuf--vvnote-msg
    (telega-ins--as-string
     (let* ((proc (plist-get telega-chatbuf--vvnote-msg :telega-ffplay-proc))
            (proc-status (when (process-live-p proc)
                           (process-status proc)))
            (played (when proc-status
                      (plist-get (process-plist proc) :progress)))
            (sender (or (telega-msg-sender telega-chatbuf--vvnote-msg)
                        (telega-msg-chat telega-chatbuf--vvnote-msg))))
       (when played ;(memq proc-status '(run stop))
         (telega-ins--button (propertize "✕" 'face 'bold)
           'action (lambda (_ignored)
                     (telega-ffplay-stop)))
         (telega-ins " Active vvnote: ")
         (telega-ins--raw-button
             (list 'action (lambda (_button)
                             (if (eq proc-status 'run)
                                 (telega-ffplay-pause proc)
                               (telega-ffplay-resume proc)))
                   'face nil)
           (if (eq proc-status 'run)
               (telega-ins (telega-symbol 'pause))
             (telega-ins (telega-symbol 'play)))
           (telega-ins " ")
           (telega-ins (telega-msg-sender-title sender
                         :with-username-p t)))
         )))))

(defun telega-chatbuf-footer-active-video-chat ()
  "Formatter for the active video chat in the chatbuf."
  (telega-chatbuf--dirtiness-init
   "group-call"
   "updateChatVideoChat"                ;video_chat
   )

  (when-let* ((video-chat (plist-get telega-chatbuf--chat :video_chat))
              (video-chat-visible-p
               (and (not (plist-get telega-chatbuf--hidden-headers :video-chat))
                    (let* ((active-p (plist-get video-chat :has_participants))
                           (display-spec (cdr (assq (if active-p 'active 'passive)
                                                    telega-video-chat-display))))
                      (memq 'footer display-spec))))
              (group-call (telega-group-call-get
                              (plist-get video-chat :group_call_id))))
    (telega-ins--as-string
     (telega-ins--button (propertize "✕" 'face 'bold)
       'action (lambda (_ignored)
                 (plist-put telega-chatbuf--hidden-headers :video-chat t)
                 (telega-chatbuf--chat-update "group-call")))
     (telega-ins " ")
     (telega-ins (telega-i18n "lng_group_call_title")
                 ": "
                 (or (telega-tl-str group-call :title)
                     (propertize "No title" 'face 'telega-shadow))
                 " ")
     (if (plist-get group-call :is_joined)
         (telega-ins--button "Leave"
           :value group-call
           :action #'telega-group-call-leave)
       (telega-ins--button "Join"
         :value group-call
         :action #'telega-group-call-join))
     (telega-ins "\n")

     ;; Group Call scheduled?
     (when (> (plist-get group-call :scheduled_start_date) 0)
       (telega-ins "   " (telega-symbol 'alarm)
                   (telega-i18n "lng_group_call_scheduled_status") ": ")
       (telega-ins--date-iso8601
        (plist-get group-call :scheduled_start_date))
       ;; Start Now
       (telega-ins " ")
       (telega-ins--button "Start Now"
         :value group-call
         :action #'telega--startScheduledGroupCall)
       (telega-ins "\n"))

     (when (and (plist-get video-chat :has_participants)
                (not (zerop (plist-get group-call :participant_count))))
       (telega-ins "   " (telega-i18n "lng_group_call_members"
                           :count (plist-get group-call :participant_count))
                   ": ")
       (seq-doseq (recent-speaker (plist-get group-call :recent_speakers))
         (telega-ins--image
          (telega-group-call--participant-image recent-speaker)))
       (telega-ins " ")
       (telega-ins--button "Show"
         :value group-call
         :action #'telega-describe-group-call))
     )))

(defun telega-chatbuf-footer-invite-forbidden-users ()
  "List of users that can be added to group only via invite link."
  ;; List of users that restricts adding them to the group
  (when-let ((add-forbidden-users (plist-get telega-chatbuf--chat
                                             :telega-add-member-forbidden-users))
             (can-invite-p (telega-chatbuf-match-p
                            '(my-permission :can_invite_users))))
    (telega-ins--as-string
     (telega-ins--button "✕"
       'action (lambda (_ignored)
                 (plist-put telega-chatbuf--chat
                            :telega-add-member-forbidden-users nil)
                 (telega-chatbuf--footer-update)))
     (telega-ins " ")
     (telega-ins-i18n "lng_via_link_group_many"
       :count (length add-forbidden-users))
     (telega-ins "\n")
     (seq-doseq (user add-forbidden-users)
       (telega-ins "  ")
       (telega-ins--raw-button (telega-link-props 'sender user 'type 'telega)
         (let ((column1 (/ (+ telega-chat-fill-column 10 1) 2)))
           (telega-ins--with-attrs (list :min column1
                                         :max column1
                                         :align 'left)
             (telega-ins--msg-sender user
               :with-avatar-p t
               :with-username-p 'telega-username
               :with-brackets-p t))))
       (telega-ins " ")
       (telega-ins--button (telega-i18n "lng_via_link_send")
         :value user
         :action #'telega-chatbuf-invite-user-via-link)
       ))))

(defun telega-chatbuf-footer-auto-delete-messages ()
  "Formatter for auto delete messages timer."
  (telega-chatbuf--dirtiness-init "updateChatMessageAutoDeleteTime")

  ;; Auto-delete messages timer
  (when-let ((msg-ttl (telega-chatbuf-match-p 'has-message-ttl)))
    (concat (telega-symbol 'flames)
            (telega-i18n "lng_manage_messages_ttl_title") ": "
            (telega-duration-human-readable msg-ttl 1 'long))))

(defun telega-chatbuf-footer-reply-markup-buttons ()
  "Formatter for the reply markup buttons in the chat buffer."
  (telega-chatbuf--dirtiness-init "updateChatReplyMarkup")

  (when-let ((markup-msg (telega-chat-reply-markup-msg telega-chatbuf--chat)))
    (unless (telega-msg-match-p markup-msg 'is-deleted)
      (telega-ins--as-string
       (telega-ins--labeled
           (concat (telega-symbol 'keyboard) telega-symbol-nbsp) nil
         (telega-ins--reply-markup markup-msg 'force))))))

(defun telega-chatbuf-footer-restriction-reason ()
  "Formatter for the chat restriction reasons."
  ;; Chat's restriction reason
  (when-let ((reason
              (telega-tl-str (telega-chat--info telega-chatbuf--chat 'locally)
                             :restriction_reason)))
    (telega-ins--as-string
     (telega-ins--with-face 'bold
       (telega-ins "Chat is restricted:"))
     (telega-ins "\n")
     (telega-ins reason))))

(defun telega-chatbuf-footer-bot-description ()
  "Formatter for the bot description if history is empty."
  (when-let* ((bot-with-no-history-p
               (and (telega-chat-bot-p telega-chatbuf--chat)
                    (not (plist-get telega-chatbuf--chat :last_message))))
              (ci (telega-chat--info telega-chatbuf--chat t))
              (fi (telega--full-info ci))
              (desc (telega-tl-str (plist-get fi :bot_info) :description)))
    (telega-ins--as-string
     (telega-ins--with-face 'bold
       (telega-ins-i18n "lng_bot_description"))
     (telega-ins "\n")
     (telega-ins desc))))

(defun telega-chatbuf-footer-sponsored-messages ()
  "Formatter for the sponsored messages."
  (telega-chatbuf--dirtiness-init "sponsored-messages")

  (when-let ((sponsored-messages
              (plist-get telega-chatbuf--chat :telega-sponsored-messages)))
    (telega-ins--as-string
     (seq-doseq (sponsored-msg (plist-get sponsored-messages :messages))
       (telega-ins--chat-sponsored-message
        telega-chatbuf--chat sponsored-msg)))))

(defun telega-chatbuf-footer-prompt-delim (&optional with-actions-p
                                                     with-loading-p)
  "Formatter for the chatbuf prompt delimiter."
  (telega-chatbuf--dirtiness-init
   "history-loading"
   "updateChatLastMessage"
   "updateChatReadInbox"
   "updateChatAction")

  (telega-ins--as-string
   (when telega-chatbuf--messages-compact-view
     (telega-ins "\n"))
   (let* ((column telega-chat-fill-column)
          (column1 (/ column 2))
          (column2 (- column column1))
          (fill-symbol (if (or (null telega-chatbuf--ewoc)
                               (telega-chatbuf--last-msg-loaded-p))
                           'underline-bar
                         'underline-bar-partial))
          (actions (when with-actions-p
                     (telega-chat--actions
                      telega-chatbuf--chat (telega-chatbuf--message-thread-id)))))
     (telega-ins (telega-symbol fill-symbol))
     (telega-ins--with-attrs (list :min (- column1 2)
                                   :max (- column1 2)
                                   :align 'left
                                   :align-symbol fill-symbol
                                   :elide t
                                   :elide-trail (/ column1 2))
       (when actions
         (telega-ins "(")
         (telega-ins--actions actions)
         (telega-ins ")")))
     (telega-ins (telega-symbol fill-symbol))

     ;; Chat's additional info part
     (telega-ins (telega-symbol fill-symbol))
     (telega-ins--with-attrs (list :min (- column2 2)
                                   :max (- column2 2)
                                   :align 'right
                                   :align-symbol fill-symbol
                                   :elide t
                                   :elide-trail (/ column2 2))
       (when (and with-loading-p telega-chatbuf--history-loading)
         (telega-ins "[" (telega-i18n "lng_profile_loading") "]")))
     (telega-ins (telega-symbol fill-symbol))
     )))

(defun telega-chatbuf-footer-join-button ()
  "Insert JOIN/BLOCK/START/etc button."
  (telega-chatbuf--dirtiness-init
   "bot-start-parameter"
   "updateChatBlockList"                ;affects 'is-blocked
   "updateChatLastMessage"              ;affects 'is-known
   "updateChatPosition"                 ;affects 'is-known

   ;; TODO: possibly "updateBasicGroup" and "updateSupergroup" affects
   ;; 'me-is-member
   )

  (when (or telega-chatbuf--bot-start-parameter
            (and (not (telega-chat-match-p telega-chatbuf--chat 'is-known))
                 ;; Not a discussion group for some channel
                 (not (and (telega-chatbuf--thread-msg)
                           (telega-chatbuf-match-p 'can-send-or-post)))))
    (when-let ((button-text
                (cond ((telega-chat-match-p telega-chatbuf--chat 'is-blocked)
                       (if (telega-chat-bot-p telega-chatbuf--chat)
                           (upcase (telega-i18n "lng_profile_restart_bot"))
                         (upcase (telega-i18n "lng_unblock_button"))))

                      ((telega-chat-bot-p telega-chatbuf--chat)
                       (concat (upcase (telega-i18n "lng_bot_start"))
                               (when telega-chatbuf--bot-start-parameter
                                 " ")
                               (when telega-chatbuf--bot-start-parameter
                                 telega-chatbuf--bot-start-parameter)))

                      ((and (not (telega-chat-private-p telega-chatbuf--chat))
                            (not (telega-chat-secret-p telega-chatbuf--chat)))
                       (upcase (telega-i18n "lng_group_invite_join"))))))
      (telega-ins--as-string
       (telega-ins--button (concat "   " button-text "   ")
         'action (lambda (_ignored)
                   (cl-assert (not (telega-chat-secret-p telega-chatbuf--chat)))

                   (if (telega-chat-private-p telega-chatbuf--chat)
                       (progn
                         (telega-msg-sender-unblock telega-chatbuf--chat)
                         (when (telega-chat-bot-p telega-chatbuf--chat)
                           (telega--sendBotStartMessage
                            (telega-chat-user telega-chatbuf--chat)
                            telega-chatbuf--chat
                            telega-chatbuf--bot-start-parameter)
                           (setq telega-chatbuf--bot-start-parameter nil)))

                     (telega--joinChat telega-chatbuf--chat))))
       ))))

(defun telega-chat--aux-inline-reply-symbol (&optional aux-quote)
  "Return symbol to use as reply title."
  (cond ((and aux-quote (memq 'reply-quote telega-chat-aux-inline-symbols))
         (telega-symbol 'reply-quote))
        ((memq 'reply telega-chat-aux-inline-symbols)
         (telega-symbol 'reply))
        (t
         (telega-i18n "lng_in_reply_to"))))

(defun telega-chatbuf-footer-aux-plist ()
  "Display aux prompt defined by `telega-chatbuf--aux-plist'."
  (telega-chatbuf--dirtiness-init "aux-plist")

  (when telega-chatbuf--aux-plist
    (telega-ins--as-string
     (telega-ins--with-attrs  (list :max (- telega-chat-fill-column 1)
                                    :elide t
                                    :face 'telega-chat-prompt)
       (telega-ins--button (telega-symbol 'button-close)
         'action (lambda (_ignored)
                   (telega-chatbuf-cancel-aux))
         'help-echo (lambda (_ignored)
                      (telega-help-message--cancel-aux 'aux-prompt)))

       (cl-case (plist-get telega-chatbuf--aux-plist :aux-type)
         (edit
          (telega-ins
           "| " (telega-i18n "lng_edit_message") ": ")
          (telega-ins--aux-msg-one-line
              (plist-get telega-chatbuf--aux-plist :aux-msg)))

         (reply
          (let ((aux-msg (plist-get telega-chatbuf--aux-plist :aux-msg))
                (aux-quote (plist-get telega-chatbuf--aux-plist
                                      :aux-reply-quote)))
            (telega-ins
             "| " (telega-chat--aux-inline-reply-symbol aux-quote) " ")
            (telega-ins--aux-msg-one-line aux-msg
              :with-username t
              :remove 'message)
            (if aux-quote
                (telega-ins--with-face 'telega-entity-type-blockquote
                  (telega-ins--fmt-text aux-quote aux-msg))
              (telega-ins--content-one-line aux-msg)))))
       ))))

(defun telega-chatbuf--footer ()
  "Generate string to be used as ewoc's footer."
  (let ((footer (format-mode-line telega-chat-footer-format
                                  nil nil (current-buffer))))
    (add-text-properties 0 (length footer) '(read-only t front-sticky t) footer)
    footer))

(defun telega-chatbuf--footer-update ()
  "Redisplay chatbuf's footer."
  ;; NOTE: This keeps point where it is
  (with-telega-buffer-modify
   (if (< (ewoc-location (ewoc--footer telega-chatbuf--ewoc))
          (point)
          telega-chatbuf--input-marker)
       (telega-save-cursor
         (telega-ewoc--set-footer
             telega-chatbuf--ewoc (telega-chatbuf--footer)))
     (save-excursion
       (telega-ewoc--set-footer
           telega-chatbuf--ewoc (telega-chatbuf--footer))))))

(defun telega-chatbuf--check-focus-change0 (&optional new-focus-state)
  "Debounced version of `telega-chatbuf--check-focus-change'.
If NEW-FOCUS-STATE is specified, then focus state is forced."
  (when (derived-mode-p 'telega-chat-mode)
    (unless new-focus-state
      (setq telega-chatbuf--focus-debounce-timer nil))
    (let ((new-focus-state (or new-focus-state (telega-focus-state))))
      (unless (eq telega-chatbuf--focus-status new-focus-state)
        (setq telega-chatbuf--focus-status new-focus-state)
        (if new-focus-state
            (telega-chatbuf--switch-in)
          (telega-chatbuf--switch-out 'focus-out))))))

(defun telega-chatbuf--check-focus-change ()
  "Called when frame showing chatbuf changes its focus."
  ;; NOTE: we treat focus changes the same way as buffer switching
  ;; However we apply debouncing logic as described in the docstring
  ;; to `after-focus-change-function' when loosing focus, so there
  ;; will be no instant switch-out/switch-in if focus bounces
  (cl-assert (derived-mode-p 'telega-chat-mode))
  (if-let ((focus-status (telega-focus-state)))
      (telega-chatbuf--check-focus-change0 focus-status)
    (when telega-chatbuf--focus-debounce-timer
      (cancel-timer telega-chatbuf--focus-debounce-timer))
    (setq telega-chatbuf--focus-debounce-timer
          (run-with-timer telega-focus-out-debounce-internal nil
                          #'telega-chatbuf--check-focus-change0))))

(defun telega-chatbuf-msg--sensor-func (window oldpos dir)
  "Sensor function to trigger hover in/out hook for the message at point'."
  ;; NOTE: Run sensor logic only in focused frame, to avoid managing
  ;; point (and viewing messages) if new message added at point in
  ;; frame without focus
  ;; See https://t.me/emacs_telega/36301
  (when (telega-focus-state (window-frame window))
    (when-let* ((pos (if (eq dir 'entered) (point) oldpos))
                (msg (telega-msg-at pos)))
      (unless (telega-msg-internal-p msg)
        (if (eq dir 'entered)
            (progn
              (telega-chatbuf--manage-point pos)
              (telega-chatbuf--filter-msg-position-load msg)
              (run-hook-with-args 'telega-msg-hover-in-hook msg))

          (run-hook-with-args 'telega-msg-hover-out-hook msg))
        ))))

(defun telega-chatbuf-msg--pp (msg)
  "Pretty printer for MSG button inserted in a chatbuf."
  (let* ((chat (telega-msg-chat msg))
         (msg-inserter
          (cond ((and (telega-msg-match-p msg 'is-deleted)
                      (telega-chat-match-p chat
                        telega-chat-show-deleted-messages-for))
                 #'telega-ins--message-deleted)

                ((telega-msg-match-p msg 'ignored)
                 (when telega-ignored-messages-visible
                   #'telega-ins--message-ignored))

                ;; NOTE: check for messages grouping by sender
                ((and (telega-chat-match-p chat telega-chat-group-messages-for)
                      (> (point) 3)
                      (when-let ((prev-msg (telega-msg-at (- (point) 2)))
                                 (trim-regexp (rx (1+ (or " " "\n")))))
                        ;; Only if MSG's header is pretty the same as
                        ;; for PREV-MSG
                        (and (not (telega-msg-internal-p prev-msg))
                             (not (telega-msg-internal-p msg))
                             (not (telega-msg-special-p prev-msg))
                             (< (- (plist-get msg :date)
                                   (plist-get prev-msg :date))
                                telega-chat-group-messages-timespan)
                             ;; NOTE: Different senders might have same name
                             (equal (plist-get msg :sender_id)
                                    (plist-get prev-msg :sender_id))
                             (string-prefix-p
                              (string-trim-right
                               (telega-ins--as-string
                                (telega-ins--message-header msg chat))
                               trim-regexp)
                              (string-trim-right
                               (telega-ins--as-string
                                (telega-ins--message-header prev-msg chat))
                               trim-regexp)))))
                 #'telega-ins--message-no-header)

                (t telega-inserter-for-msg-button))))
    (when msg-inserter
      (telega-button--insert 'telega-msg msg
        :inserter msg-inserter
        'cursor-sensor-functions '(telega-chatbuf-msg--sensor-func)))

    ;; NOTE: we insert newline outside the button to provide
    ;; msg hover-in/hover-out hooks handled by sensor function.
    ;; Without separator, sensor function won't be triggered
    (unless telega-chatbuf--messages-compact-view
      (telega-ins--with-props '(read-only t front-sticky t)
        (telega-ins "\n")))
    ))

(define-derived-mode telega-chat-mode nil '((:eval (telega-symbol 'mode)) "Chat")
  "The mode for telega chat buffer.

Message bindings (cursor on message):
\\{telega-msg-button-map}
Global chat bindings:
\\{telega-chat-mode-map}"
  :group 'telega-chat
  (setq telega-chatbuf--chat telega-chat--preparing-buffer-for
        telega-chatbuf--messages-pop-ring
        (make-ring telega-chat-messages-pop-ring-size)
        telega-chatbuf--input-ring (make-ring telega-chat-input-ring-size)
        telega-chatbuf--input-idx nil
        telega-chatbuf--input-pending nil
        telega-chatbuf--history-loading nil
        telega-chatbuf--inline-query nil
        telega-chatbuf--vvnote-msg nil
        telega-chatbuf--my-action nil
        telega-chatbuf--administrators nil
        telega-chatbuf--group-call-users nil
        telega-chatbuf--bot-start-parameter nil
        )

  ;; Make usernames with "_" be completable
  (modify-syntax-entry ?\_ "w" telega-chat-mode-syntax-table)

  ;; Process earch line seperately to check for bidi paragraph
  ;; See https://github.com/zevlg/telega.el/issues/45#issuecomment-462160553
  (setq bidi-display-reordering telega-chat-bidi-display-reordering)
  (setq bidi-paragraph-separate-re "^")
  (setq bidi-paragraph-start-re "^")

  (erase-buffer)
  (setq-local nobreak-char-display nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local window-point-insertion-type t)
  (setq-local next-line-add-newlines nil)
  (setq-local next-screen-context-lines 0) ; do not scroll if point at `eobp'
  (setq-local scroll-conservatively telega-chat-scroll-conservatively)
  ;; NOTE: To make `M-x scroll-down RET' jump to the beginning, making
  ;; telega load older messages
  (setq-local scroll-error-top-bottom t)
  (when (featurep 'dnd)
    (setq-local dnd-protocol-alist
                (append telega-chat-dnd-protocol-alist dnd-protocol-alist)))

  ;; To make `M-x visual-fill-column-mode RET' to work out-of-box
  (setq fill-column telega-chat-fill-column)
  (visual-line-mode 1)

  ;; Enable filling by default to resemble old style telega formatting
  ;; We give a little bit extra space for filling column ot accomodate
  ;; inaccuracies in message header width calculation
  (setq visual-fill-column-extra-text-width '(0 . 1))
  (visual-fill-column-mode 1)

  (cursor-sensor-mode 1)
  (cursor-intangible-mode 1)
  ;; NOTE: Positive `line-spacing' creates stripes in the images.
  ;; See https://github.com/zevlg/telega.el/issues/347
  (setq line-spacing 0)

  (when telega-chat-header-line-underline
    (setq-local x-underline-at-descent-line t)
    (face-remap-add-relative 'header-line :underline t))

  (setq telega-chatbuf--ewoc
        (ewoc-create (telega-ewoc--gen-pp #'telega-chatbuf-msg--pp) nil nil t))
  (goto-char (point-max))

  (setq telega-chatbuf--prompt-button
        (telega-button--insert 'telega-prompt ">>> "))

  ;; user's input starts just after the prompt
  (setq telega-chatbuf--input-marker (point-marker))

  (add-hook 'window-scroll-functions 'telega-chatbuf--window-scroll nil t)
  (add-hook 'post-command-hook 'telega-chatbuf--post-command nil t)
  (add-hook 'kill-buffer-hook 'telega-chatbuf--killed nil t)
  (when (boundp 'after-focus-change-function)
    (add-function :after (local 'after-focus-change-function)
                  'telega-chatbuf--check-focus-change))

  (setq telega--chat-buffers-alist
        (cl-pushnew (cons telega-chatbuf--chat (current-buffer))
                    telega--chat-buffers-alist))

  (setq-local company-backends telega-company-backends)

  ;; Initialize chatbuf headers
  (let ((telega-chatbuf--dirtiness-symbol 'telega-chatbuf--dirtiness-header-line))
    (telega-chatbuf--header-line-update))
  (let ((telega-chatbuf--dirtiness-symbol 'telega-chatbuf--dirtiness-mode-line))
    (telega-chatbuf--mode-line-update))
  (let ((telega-chatbuf--dirtiness-symbol 'telega-chatbuf--dirtiness-footer))
    (telega-chatbuf--footer-update))
  (let ((telega-chatbuf--dirtiness-symbol 'telega-chatbuf--dirtiness-prompt))
    (telega-chatbuf--prompt-update))
  )

(defun telega-chatbuf--set-action (action)
  "Set my chatbuf action to ACTION"
  (when (stringp action)
    (cl-assert (member action '("Typing" "RecordingVideo" "UploadingVideo"
                                "RecordingVoiceNote" "UploadingVoiceNote"
                                "UploadingPhoto" "UploadingDocument"
                                "ChoosingLocation" "ChoosingContact"
                                "StartPlayingGame" "RecordingVideoNote"
                                "UploadingVideoNote" "WatchingAnimations"
                                "Cancel")))
    (setq action (list :@type (concat "chatAction" action))))
  ;; NOTE: special case is for `chatActionUploadingVideoNote', it
  ;; might have additional `:progress' argument.  In this case, pass
  ;; it directly as list to `telega-chatbuf--set-action'

  (unless (equal telega-chatbuf--my-action action)
    (let ((cancel-p (eq (telega--tl-type action) 'chatActionCancel)))
      (setq telega-chatbuf--my-action (unless cancel-p action)))

    ;; Update it on server as well
    (telega--sendChatAction telega-chatbuf--chat action)))

(defmacro with-telega-chatbuf-action (action &rest body)
  "Execute BODY setting current action to ACTION.
Recover previous active action after BODY execution."
  (declare (indent 1))
  (let ((actsym (gensym "action")))
    `(let ((,actsym (plist-get telega-chatbuf--my-action :@type)))
       (telega-chatbuf--set-action ,action)
       (unwind-protect
           (progn ,@body)
         (telega-chatbuf--set-action
          (or (and ,actsym (substring ,actsym 10))
              "Cancel"))))))

(defun telega-chatbuf-editing-msg ()
  "Return message currently editing."
  (when (eq 'edit (plist-get telega-chatbuf--aux-plist :aux-type))
    (plist-get telega-chatbuf--aux-plist :aux-msg)))

(defun telega-chatbuf-replying-msg ()
  "Return message currently replying."
  (when (eq 'reply (plist-get telega-chatbuf--aux-plist :aux-type))
    (plist-get telega-chatbuf--aux-plist :aux-msg)))

(defun telega-chatbuf-replying-imr ()
  "Return `inputMessageReplyToMessage' structure if replying to a message."
  (when (eq 'reply (plist-get telega-chatbuf--aux-plist :aux-type))
    (let ((aux-msg (plist-get telega-chatbuf--aux-plist :aux-msg)))
      (list :@type "inputMessageReplyToMessage"
            ;; NOTE: chat_id=0, when replying in the same chat/topic
            ;; must be 0 in secret chats
            :chat_id (if (= (plist-get telega-chatbuf--chat :id)
                            (plist-get aux-msg :chat_id))
                         0
                       (plist-get aux-msg :chat_id))
            :message_id (plist-get aux-msg :id)
            :quote (plist-get telega-chatbuf--aux-plist :aux-reply-quote)))))

(defun telega-chatbuf--window-scroll (window display-start)
  "Mark some messages as read while scrolling."
  (with-current-buffer (window-buffer window)
    (when (not (eq window (selected-window)))
      ;; Mark last message as read if point hits the prompt
      (telega-chatbuf--manage-point (window-point window) 'if-inside-prompt)

      ;; If scrolling in inactive window (with C-M-v) we might need to
      ;; fetch new history if point near the buffer bottom
      (when (and (> display-start (- (point-max) 2000))
                 (not telega-chatbuf--history-loading)
                 (telega-chatbuf--need-newer-history-p))
        (telega-chatbuf--load-newer-history)))

    ;; NOTE: If all chatbuf messages are read, then mark sponsored
    ;; message as viewed as well
    (when-let ((sponsored-messages
                (plist-get telega-chatbuf--chat :telega-sponsored-messages))
               (sponsored-views
                (or (plist-get telega-chatbuf--chat :telega-sponsored-views) 0)))
      (when (and (telega-chatbuf--last-msg-loaded-p)
                 (pos-visible-in-window-p
                  (ewoc-location (ewoc--footer telega-chatbuf--ewoc))))
        (when (zerop sponsored-views)
          (seq-doseq (sponsored-msg (plist-get sponsored-messages :messages))
            (telega--viewSponsoredMessage telega-chatbuf--chat sponsored-msg)))

        (plist-put telega-chatbuf--chat :telega-sponsored-views
                   (1+ sponsored-views))
        (when (> sponsored-views 0)
          (plist-put telega-chatbuf--chat :telega-sponsored-messages nil))))
    ))

(defun telega-chatbuf--post-command ()
  "Chabuf `post-command-hook' function."
  ;; Possible view last message if point inside the prompt
  (telega-chatbuf--manage-point (point) 'if-inside-prompt)

  ;; Check that all atachements are valid (starting/ending chars are
  ;; ok) and remove invalid attachments
  (let ((attach (telega--region-by-text-prop
                 telega-chatbuf--input-marker 'telega-attach)))
    (while attach
      (if (and (get-text-property (car attach) 'attach-open-bracket)
               (get-text-property (1- (cdr attach)) 'attach-close-bracket))
          ;; Valid
          (setq attach (telega--region-by-text-prop
                        (cdr attach) 'telega-attach))

        ;; Invalid attachment, remove it from input
        (delete-region (car attach) (cdr attach))
        (setq attach (telega--region-by-text-prop
                      (car attach) 'telega-attach)))))

  ;; If point moves inside prompt, move it at the beginning of input.
  (when (and (>= (point) telega-chatbuf--prompt-button)
             (< (point) telega-chatbuf--input-marker))
    (goto-char telega-chatbuf--input-marker))

  ;; If point moves near the beginning of chatbuf, then request for
  ;; the older history
  (when (and (< (point) 2000)
             (not telega-chatbuf--history-loading)
             (telega-chatbuf--need-older-history-p))
    (telega-chatbuf--load-older-history))

  ;; If point moves near the end of the chatbuf, then request for
  ;; newer history
  ;; NOTE: Do not load newer history if prompt is active (reply or
  ;; edit)
  (when (and (> (point) (- (point-max) 2000))
             (not telega-chatbuf--history-loading)
             (telega-chatbuf--need-newer-history-p))
    (telega-chatbuf--load-newer-history))

  ;; Finally, when input is probably changed by above operations,
  ;; update chat's action after command execution.
  (let ((input-p (telega-chatbuf-has-input-p)))
    (cond ((and (not telega-chatbuf--my-action) input-p)
           (telega-chatbuf--set-action "Typing"))
          ((and telega-chatbuf--my-action (not input-p))
           (telega-chatbuf--set-action "Cancel")))

    ;; If there is active draft_message and input is empty then clear
    ;; the draf
    (when (and (plist-get telega-chatbuf--chat :draft_message)
               (not input-p))
      (telega--setChatDraftMessage telega-chatbuf--chat)))
  )

(defun telega-chatbuf--name (chat)
  "Return uniquified name for the CHAT buffer."
  (let* ((bufname (concat (telega-symbol 'telegram)
                          (when (telega-chat-secret-p chat)
                            (telega-symbol 'lock))
                          (telega-ins--as-string
                           (telega-ins--msg-sender chat
                             :with-username-p t
                             :with-brackets-p t
                             :with-title-faces-p nil))
                          (when (plist-get chat :is_pinned)
                            (telega-symbol 'pin))
                          (when (plist-get chat :has_scheduled_messages)
                            (telega-symbol 'alarm))))
         (buf (get-buffer bufname)))
    ;; NOTE: Multiple chats could have same BUFNAME, uniquify it by
    ;; adding unique suffix, in case other chat occupies BUFNAME
    ;; See https://github.com/zevlg/telega.el/issues/158
    (if (and (buffer-live-p buf)
             (not (eq chat (telega-chatbuf--chat buf))))
        (concat bufname
                "<"
                (or (telega-chat-username chat)
                    (number-to-string (plist-get chat :id)))
                ">")
      bufname)))

(defun telega-chatbuf--unblock-start-join-action (&optional _ignored_button)
  "[START] [UNBLOCK] or [JOIN] or button has been pressed."
  (cl-assert (not (telega-chat-secret-p telega-chatbuf--chat)))

  ;; NOTE: do async calls, update chatbuf prompt
  ;; on-updateUserFullInfo, on-updateBasicGroup or on-updateSupergroup
  (if (telega-chat-private-p telega-chatbuf--chat)
      (progn
        (telega-msg-sender-unblock telega-chatbuf--chat)
        (when (telega-chat-bot-p telega-chatbuf--chat)
          (telega--sendBotStartMessage
           (telega-chat-user telega-chatbuf--chat)
           telega-chatbuf--chat telega-chatbuf--bot-start-parameter)
          (setq telega-chatbuf--bot-start-parameter nil)))

    (telega--joinChat telega-chatbuf--chat)))

(defun telega-chatbuf-prompt-bot-menu-button ()
  "Return line for bot's menu button."
  (when-let* ((info (telega-chat--info telega-chatbuf--chat 'offline))
              (telega-full-info-offline-p nil)
              (full-info (telega--full-info info))
              (bot-info (plist-get full-info :bot_info))
              (menu-button (plist-get bot-info :menu_button)))
    (telega-tl-str menu-button :text)))

(defun telega-chatbuf-prompt-default-sender-avatar ()
  "Return one line avatar for default message sender to the chatbuf."
  (when (telega-chatbuf-match-p 'has-default-sender)
    (let ((chat telega-chatbuf--chat))
      (telega-ins--as-string
       (telega-ins--image
        (telega-msg-sender-avatar-image-one-line
         (telega-msg-sender (plist-get chat :message_sender_id))))))))

(defun telega-chatbuf-prompt-body ()
  "Return body for the chatbuf input prompt."
  (telega-chatbuf--dirtiness-init
   "thread"                             ; telega-chatbuf--thread-msg

   ;; TODO: possibly "updateBasicGroup" and "updateSupergroup" affects
   ;; 'me-is-member and 'me-is-anonymous
   )

  (cond ((telega-chatbuf-match-p 'me-is-anonymous)
         (telega-i18n "telega_chat_prompt_anonymous"))

        ((and (telega-chatbuf--thread-msg)
              (not (telega-chatbuf-match-p 'me-is-member)))
         (telega-i18n "telega_chat_prompt_comment"))

        ((telega-chatbuf-match-p '(and (type channel)
                                       (my-permission :can_post_messages)))
         (telega-i18n "telega_chat_prompt_broadcast"))

        ((telega-chatbuf-match-p 'has-default-sender)
         (telega-symbol 'right-arrow))))

(defun telega-chatbuf-prompt-chat-avatar ()
  "Return chatbuf's avatar for the one line usage."
  (telega-chatbuf--dirtiness-init
   "updateChatPhoto"
   "updateChatMessageSender")

  (let ((chat telega-chatbuf--chat))
    (telega-ins--as-string
     (telega-ins--image
      (telega-msg-sender-avatar-image-one-line chat)))))

(defun telega-chatbuf-prompt-topic (&optional max-width with-topic-title-p)
  "Return current topic title for the prompt."
  (telega-chatbuf--dirtiness-init "thread")

  (when-let ((topic (telega-chatbuf--thread-topic)))
    (telega-ins--as-string
     (telega-ins--with-attrs (list :max max-width :align 'left :elide t
                                   :face 'telega-shadow)
       (telega-ins (telega-symbol 'topic))
       (telega-ins--topic-icon topic)
       (when with-topic-title-p
         (telega-ins--topic-title topic))))))

(defun telega-chatbuf--prompt-update (&optional reset-aux)
  "Update chatbuf's prompt.
If RESET-AUX is specified, then reset aux prompt."
  (with-telega-buffer-modify
   (telega-save-excursion
     (when reset-aux
       (setq telega-chatbuf--aux-plist nil)
       (telega-chatbuf--chat-update "aux-plist"))

     (let ((prompt (format-mode-line telega-chat-prompt-format
                                     nil nil (current-buffer))))
       (telega-button--update-value
        telega-chatbuf--prompt-button
        (if (telega-chatbuf-match-p 'can-send-or-post)
            prompt
          (propertize prompt 'face 'telega-shadow)))))))

(defun telega-chatbuf--prompt-reset ()
  "Reset prompt to initial state in chat buffer."
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (telega-chatbuf--prompt-update 'reset)))

(defun telega-chatbuf--input-draft-update (&optional force)
  "Update chatbuf's input to display draft message.
If FORCE is specified, then set input draft unconditionally,
otherwise set draft only if chatbuf input is also draft."
  (let* ((chat telega-chatbuf--chat)
         (draft-msg (plist-get chat :draft_message))
         (reply-msg-id (plist-get draft-msg :reply_to_message_id)))
    (if (and reply-msg-id (not (zerop reply-msg-id)))
        (unless (eq (plist-get (telega-chatbuf-replying-msg) :id)
                    reply-msg-id)
          (telega-msg-get chat reply-msg-id
            (lambda (msg &optional _ignored)
              (save-excursion (telega-msg-reply msg)))))
      ;; Reset only if replying, but `:reply_to_message_id' is not
      ;; specified, otherwise keep the aux, for example editing
      (when (telega-chatbuf-replying-msg)
        (telega-chatbuf--prompt-reset)))

    ;; NOTE: update draft only if current chatbuf input is marked as
    ;; draft (or empty), otherwise draft update may change current
    ;; input
    (when (or force (not (telega-chatbuf-has-input-p))
              (telega-chatbuf--input-draft-p))
      (with-telega-buffer-modify
       (telega-save-cursor
         (telega-chatbuf--input-delete)
         (goto-char telega-chatbuf--input-marker)
         (telega-ins--with-props '(:draft-input-p t)
           (telega-ins--fmt-text
            (telega--tl-get draft-msg :input_message_text :text))))))))

(defun telega-chatbuf--load-initial-history ()
  "Load initial history in the chatbuf."
  (telega-chatbuf--clean)

  (cond ((and telega-chat-preview-mode
              telega-chat-preview-mode-from-last-message)
         (telega-chatbuf-read-all))

        ;; NOTE: if thread loads from the first message, then insert
        ;; thread starter message
        ((when-let ((thread-last-read-msg-id
                     (telega--tl-get (telega-chatbuf--thread-msg)
                                     :interaction_info :reply_info
                                     :last_read_inbox_message_id)))
           (zerop thread-last-read-msg-id))
         (telega-chatbuf--older-history-loaded)
         (telega-chatbuf--load-newer-history))

        ;; All messages are read
        ((let ((last-read-msg-id (telega-chatbuf--last-read-inbox-msg-id)))
           ;; NOTE: `:last_read_inbox_message_id' == 0 if chat has never
           ;; been opened before (i.e. new to user), in this case we
           ;; consider all messages in the chat are read
           (or (telega-zerop last-read-msg-id)
               (eq last-read-msg-id (telega-chatbuf--last-message-id))))
         (telega-chatbuf-read-all))

        ((and (= 1 (plist-get telega-chatbuf--chat :unread_count))
              (telega-chatbuf--append-new-message-p
               (plist-get telega-chatbuf--chat :last_message)))
         (let ((last-msg (plist-get telega-chatbuf--chat :last_message)))
           (telega-chatbuf--insert-messages (list last-msg) 'append-new)
           (telega-chatbuf--goto-loaded-msg (plist-get last-msg :id) nil
             (lambda (button)
               (when (and (eq (telega-msg-at button)
                              (telega-chatbuf--last-msg))
                          (telega-button--observable-p
                           telega-chatbuf--input-marker))
                 (goto-char (point-max))
                 (telega-chatbuf--manage-point))))
           (telega-chatbuf--load-older-history)))

        (t
         (telega-chatbuf-next-unread))
        ))

(defun telega-chatbuf--get-create (chat &optional no-history-load)
  "Get or create chat buffer for the CHAT.
If NO-HISTORY-LOAD is specified, do not try to load history."
  (let ((bufname (telega-chatbuf--name chat)))
    (or (get-buffer bufname)
        (with-current-buffer (generate-new-buffer bufname)
          (let ((telega-chat--preparing-buffer-for chat))
            (telega-chat-mode))
          (telega--openChat chat)

          ;; Show the draft message if any, see
          ;; https://github.com/zevlg/telega.el/issues/80
          (when (plist-get chat :draft_message)
            (telega-chatbuf--input-draft-update 'force))

          ;; Asynchronously fetch some chat info
          (telega-chatbuf--admins-fetch)
          (telega-chatbuf--pinned-messages-fetch)
          (telega-chatbuf--active-stories-fetch)
          (telega-chatbuf--pinned-stories-fetch)
          (telega-chatbuf--sponsored-messages-fetch)
          (unless (zerop (telega--tl-get chat :video_chat :group_call_id))
            (telega-chatbuf--video-chat-fetch))
          (unless (zerop (plist-get chat :reply_markup_message_id))
            (telega-chatbuf--reply-markup-message-fetch))

          ;; Start from last read message
          ;; see https://github.com/zevlg/telega.el/issues/48
          (unless no-history-load
            (telega-chatbuf--load-initial-history))

          ;; Manage chat buffers, by killing least recent chat buffer
          ;; in case number of chatbufs exceeds
          ;; `telega-chat-buffers-limit'
          (telega-chat-buffers-manage (current-buffer))

          ;; Openning chat may affect filtering, see `opened' filter
          (telega-chat--update chat)

          (current-buffer)))))

(defun telega-chatbuf--need-older-history-p ()
  "Return non-nil if older history can be loaded."
  (not (telega-chatbuf--history-state-get :older-loaded)))

(defun telega-chatbuf--older-history-loaded ()
  "In chatbuf set mark, that all older history has been loaded."
  (when (telega-chatbuf--need-older-history-p)
    (message "telega: loaded all older messages")

    (telega-chatbuf--history-state-set :older-loaded t)

    ;; Insert thread starter message if any, because it is not
    ;; included in the results of the `getMessageThreadHistory' call
    (when (telega-chatbuf--thread-msg)
      (telega-chatbuf--insert-messages
       (list (telega-chatbuf--thread-msg)
             (telega-msg-create-internal
              telega-chatbuf--chat
              (telega-fmt-text (telega-i18n "lng_replies_discussion_started")
                               '(:@type "textEntityTypeBold"))))
       'prepend))))

(defun telega-chatbuf--newer-history-loaded ()
  "In chatbuf set mark, that all newer messages in the history has been loaded."
  (telega-chatbuf--history-state-set :newer-loaded t))

(defun telega-chatbuf--need-newer-history-p ()
  "Return non-nil if newer history can be loaded."
  (and (not (telega-chatbuf--last-msg-loaded-p))
       (if (telega-chatbuf--history-state-get :newer-freezed)
           (< (- (point-max) (point)) 2000)
         t)
       ;; Not editing or replying
       (not telega-chatbuf--aux-plist)
       (not (telega-chatbuf-has-input-p))))

;;; chatbuf modeline formatters
(defun telega-chatbuf-mode-line-video-chat (&optional max-width)
  "Format [Video Chat] button for chat's group call."
  (telega-chatbuf--dirtiness-init
   "group-call"
   "updateChatVideoChat"                ;video_chat
   )

  (when (telega-chatbuf-match-p 'has-video-chat)
    (let* ((active-p (telega--tl-get telega-chatbuf--chat
                                     :video_chat :has_participants))
           (display-spec (cdr (assq (if active-p 'active 'passive)
                                    telega-video-chat-display))))
      (when (memq 'modeline display-spec)
        (when-let ((group-call (telega-chat-group-call telega-chatbuf--chat)))
          (telega-ins--as-string
           (telega-ins--with-attrs (list :max max-width :align 'left :elide t)
             (telega-ins--with-props
                 (list 'local-map (eval-when-compile
                                    (make-mode-line-mouse-map
                                     'mouse-1 #'telega-chatbuf-goto-video-chat))
                       'mouse-face 'mode-line-highlight
                       'help-echo (telega-i18n "telega_chat_modeline_video_chat_help"
                                    :mouse "mouse-1"))
               (telega-ins
                (number-to-string (plist-get group-call :participant_count))
                (propertize (telega-symbol (if active-p
                                               'video-chat-active
                                             'video-chat-passive))
                            'face 'telega-shadow)
                ": "
                (or (telega-tl-str group-call :title)
                    (propertize "No title" 'face 'telega-shadow)))))
           ))))))

(defun telega-chatbuf-mode-line-discuss ()
  "Format [Discuss] button for chat buffer modeline."
  ;; NOTE: changing discussion chat is extremely rare event, so don't
  ;; subscribe to it
  ;; (telega-chatbuf--dirtiness-init
  ;;  "updateSupergroup")
  (when (telega-chatbuf-match-p 'has-linked-chat)
    (telega-ins--as-string
     (telega-ins--with-props
         (list 'local-map (eval-when-compile
                            (make-mode-line-mouse-map
                             'mouse-1 #'telega-chatbuf-goto-linked-chat))
               'mouse-face 'mode-line-highlight
               'help-echo (telega-i18n "telega_chat_modeline_discuss_help"
                            :mouse "mouse-1"))
       (telega-ins (telega-symbol 'linked))
       (telega-ins-i18n (if (telega-chatbuf-match-p '(type channel))
                            "lng_channel_discuss"
                          "lng_manage_linked_channel")))
     )))

(defun telega-chatbuf-mode-line-unread ()
  "Format unread/mentions/reactions string for chat buffer modeline."
  (telega-chatbuf--dirtiness-init
   "updateChatReadInbox"                ;unread_count
   "updateChatUnreadMentionCount"       ;unread_mention_count
   "updateMessageMentionRead"           ;unread_mention_count
   "updateChatUnreadReactionCount"      ;unread_reaction_count
   "updateMessageUnreadReactions"       ;unread_reaction_count
   )

  (let ((unread-count (telega-chatbuf--unread-message-count))
        (mention-count (telega-chatbuf--unread-mention-count))
        (reaction-count (telega-chatbuf--unread-reaction-count)))
    (concat
     (when (> unread-count 0)
       (propertize (telega-i18n "telega_chat_modeline_unread"
                     :unread_count unread-count)
                   'face 'bold
                   'local-map (eval-when-compile
                                (make-mode-line-mouse-map
                                 'mouse-1 #'telega-chatbuf-read-all))
                   'mouse-face 'mode-line-highlight
                   'help-echo (telega-i18n "telega_chat_modeline_unread_help"
                                :mouse "mouse-1")))
     (when (> mention-count 0)
       (concat
        (when (> unread-count 0) " ")
        (propertize (concat "@" (number-to-string mention-count))
                    'face 'telega-mention-count
                    'local-map (eval-when-compile
                                 (make-mode-line-mouse-map
                                  'mouse-1 'telega-chatbuf-next-unread-mention))
                    'mouse-face 'mode-line-highlight
                    'help-echo (telega-i18n "telega_chat_modeline_mention_help"
                                 :mouse "mouse-1"))))
     (when (> reaction-count 0)
       (concat
        (when (or (> unread-count 0) (> mention-count 0)) " ")
        (propertize (concat (telega-symbol 'reaction)
                            (number-to-string reaction-count))
                    'face 'telega-mention-count
                    'local-map (eval-when-compile
                                 (make-mode-line-mouse-map
                                  'mouse-1 'telega-chatbuf-next-unread-reaction))
                    'mouse-face 'mode-line-highlight
                    'help-echo (telega-i18n "telega_chat_modeline_reaction_help"
                                 :mouse "mouse-1"))))
     )))

(defun telega-chatbuf-mode-line-marked ()
  "Format string for marked messages in chatbuffer."
  (telega-chatbuf--dirtiness-init
   "marked-messages")

  (let ((marked-count (length telega-chatbuf--marked-messages)))
    (unless (zerop marked-count)
       (propertize (telega-i18n "telega_chat_modeline_marked"
                     :marked_count marked-count)
                   'face 'error
                   'local-map (eval-when-compile
                                (make-mode-line-mouse-map
                                 'mouse-1 #'telega-chatbuf-msg-marks-toggle))
                   'mouse-face 'mode-line-highlight
                   'help-echo (telega-i18n "telega_chat_modeline_marked_help"
                                :mouse "mouse-1"))
       )))

(defun telega-chatbuf-mode-line-members (&optional use-icons-p)
  "Format members string for chat buffer modeline.
If ICONS-P is non-nil, then use icons for members count."
  (telega-chatbuf--dirtiness-init
   "updateChatOnlineMemberCount"
   )

  (let ((member-count
         (or (plist-get (telega-chat--info telega-chatbuf--chat) :member_count)
             0))
        (online-count
         (or (plist-get telega-chatbuf--chat :x-online-count)
             0)))
    (unless (zerop member-count)
      (if (not use-icons-p)
          (telega-i18n "telega_chat_modeline_members"
            :member_count member-count :count online-count)

        (concat
         (number-to-string member-count)
         (propertize (telega-symbol 'member) 'face 'telega-shadow)
         (unless (zerop online-count)
           (concat ", " (number-to-string online-count)
                   (telega-symbol 'online-status)))))
      )))

(defun telega-chatbuf-mode-line-pinned-messages (&optional max-width)
  "Format pinned messages for chatbuf modeline.
Pinned messages are only available for full history chat, without
topic or thread filtering."
  (telega-chatbuf--dirtiness-init
   "thread"
   "pinned-messages")

  (when-let* ((no-thread-p (zerop (telega-chatbuf--message-thread-id)))
              (pinned-messages (plist-get telega-chatbuf--chat
                                          :telega-pinned-messages))
              (pinned-msg-idx (plist-get telega-chatbuf--chat
                                         :telega-pinned-message-index))
              (pin-msg (nth pinned-msg-idx pinned-messages))
              (pin-msg-live-p (not (telega-msg-match-p pin-msg 'is-deleted))))
    ;; NOTE: Adjust MAX-WIDTH taking into account length of the
    ;; `telega-symbol-pin'
    (setq max-width (+ (or max-width 15)
                       (string-width (telega-symbol 'pin))))
    (telega-ins--as-string
     (telega-ins--with-attrs (list :max max-width :align 'left :elide t)
       (telega-ins--with-props
           (list 'local-map (make-mode-line-mouse-map
                             'mouse-1
                             #'telega-chatbuf-goto-pinned-message)
                 'mouse-face 'mode-line-highlight
                 'help-echo (telega-i18n "telega_chat_modeline_pinned_msg_help")
                 :mouse "mouse-1")
         (telega-ins (telega-symbol 'pin))
         (when (> (length pinned-messages) 1)
           (telega-ins-fmt "(%d/%d)"
             (1+ pinned-msg-idx) (length pinned-messages)))
         (let ((telega-use-images nil)
               (telega-emoji-use-images nil))
           ;; NOTE: avoid using images for emojis, because modeline
           ;; height might differ from default height, and modeline
           ;; will increase its height
           (telega-ins--content-one-line pin-msg))))
     )))

(defun telega-chatbuf-mode-line-topic (&optional max-width)
  "Format current topic title for chat buffer modeline."
  (when-let ((topic (telega-chatbuf--thread-topic)))
    (setq max-width (+ (or max-width 15) (length "Topic")))

    (telega-ins--as-string
     (telega-ins " [")
     (telega-ins--with-attrs (list :max max-width :align 'left :elide t)
       (telega-ins--with-face 'error
         (telega-ins "Topic"))
       (telega-ins ": ")
       (telega-ins (telega-symbol 'topic))
       (telega-ins--topic-title topic telega-use-images))
     (telega-ins "]"))))

(defun telega-chatbuf-mode-line-thread-message (&optional max-width)
  "Format a thread starter message for chat buffer modeline."
  (when-let ((thread-msg (telega-chatbuf--thread-msg)))
    ;; NOTE: Adjust MAX-WIDTH taking into account length of the
    ;; `telega-symbol-pin'
    (setq max-width (+ (or max-width 15) (length "Thread")))
    (telega-ins--as-string
     (telega-ins " [")
     (telega-ins--with-attrs (list :max max-width :align 'left :elide t)
       (telega-ins--with-props
           (list 'local-map (make-mode-line-mouse-map
                             'mouse-1
                             #'telega-chatbuf-goto-thread-message)
                 'mouse-face 'mode-line-highlight
                 'help-echo (telega-i18n "telega_chat_modeline_thread_msg_help")
                 :mouse "mouse-1")
         (telega-ins (propertize "Thread" 'face 'error) ": ")
         (let ((telega-use-images nil)
               (telega-emoji-use-images nil))
           ;; NOTE: avoid using images for emojis, because modeline
           ;; height might differ from default height, and modeline
           ;; will increase its height
           (telega-ins--content-one-line thread-msg))))
       (telega-ins "]"))))

(defun telega-chatbuf-mode-line-messages-filter ()
  "Format currently applied messages filter."
  (when telega-chatbuf--msg-filter
    (telega-ins--as-string
     (telega-ins--with-face 'error
       (telega-ins "Filter"))
     (telega-ins ": ")
     (telega-ins--with-face 'bold
       (telega-ins (telega-tl-str telega-chatbuf--msg-filter :title)))
     (when-let ((sender (plist-get telega-chatbuf--msg-filter :sender)))
       (telega-ins " by ")
       (telega-ins--msg-sender sender
         :with-avatar-p t
         :with-username-p t))
     )))

(defun telega-chatbuf-mode-line-buffer-name (&optional width
                                                       with-online-status-p)
  "Format chat buffer name to fit into WIDTH.
If WITH-ONLINE-STATUS-P is non-nil then also append chat user online status."
  (when with-online-status-p
    (telega-chatbuf--dirtiness-init "updateUserStatus"))

  (let ((chatbuf-name (buffer-name)))
    (telega-ins--as-string
     (telega-ins--with-attrs (list :max width
                                   :elide t
                                   :elide-trail 1 ;closing bracket
                                   )
       (telega-ins chatbuf-name)
       (when (and with-online-status-p
                  (telega-chatbuf-match-p '(and (user is-online)
                                                (not saved-messages))))
         (telega-symbol 'online-status))))))

(defun telega-chatbuf-mode-line-online-status ()
  "Format online status for the private chatbuf."
  (when (and (telega-chat-private-p telega-chatbuf--chat)
             (not (telega-me-p telega-chatbuf--chat))
             (telega-user-online-p
              (telega-chat-user telega-chatbuf--chat)))
    (telega-symbol 'online-status)))

(defun telega-chatbuf-mode-line-messages-ttl ()
  "Format TTL messages settings for the chatbuf."
  (telega-chatbuf--dirtiness-init "updateChatMessageAutoDeleteTime")

  (when-let ((ttl (telega-chatbuf-match-p 'has-message-ttl)))
    (when (or (telega-chat-secret-p telega-chatbuf--chat)
              (not (zerop ttl)))
      (apply #'propertize
             (concat (telega-symbol 'flames) "TTL: "
                     (if (zerop ttl)
                         (telega-i18n "lng_manage_messages_ttl_never")
                       (telega-duration-human-readable ttl 2)))
             ;; NOTE: can change TTL only in secret chats or if have
             ;; `:can_change_info' admin permission
             (when (telega-chatbuf-match-p '(or (type private secret)
                                                (my-permission :can_change_info)))
               (list 'local-map (eval-when-compile
                                  (make-mode-line-mouse-map
                                   'mouse-1 #'telega-chat-set-message-ttl))
                     'mouse-face 'mode-line-highlight
                     'help-echo (telega-i18n "lng_ttl_edit_about_group"))))
      )))

(defun telega-chatbuf--header-line-update ()
  "Update header-line for the chatbuf."
  (cl-assert telega-chatbuf--chat)
  (when telega-chat-header-line-format
    (setq header-line-format
          (telega-format-mode-line telega-chat-header-line-format 'header-line))
    (force-mode-line-update)))

(defun telega-chatbuf--mode-line-update ()
  "Update mode-line for the chatbuf."
  (cl-assert telega-chatbuf--chat)
  (telega-chatbuf--dirtiness-init
   "history-loading")
  (setq mode-line-process
        (when telega-chatbuf--history-loading
          (concat "[" (telega-i18n "lng_profile_loading") "]")))

  (setq mode-line-buffer-identification
        (telega-format-mode-line telega-chat-mode-line-format 'mode-line))
  (force-mode-line-update))

(defun telega-chatbuf--chat-update (&rest dirtiness)
  "Chatbuf metainfo has been updated.
This might affect chatbuf's headerline, modeline, footer or prompt formatting.
Examine `:telega-dirtiness' property and update corresponding chatbuf parts."
  (let ((header-line-p nil)
        (mode-line-p nil)
        (footer-p nil)
        (prompt-p nil))
    (dolist (et (nconc dirtiness
                       (plist-get telega-chatbuf--chat :telega-dirtiness)))
      (setq header-line-p (or header-line-p
                              (member et telega-chatbuf--dirtiness-header-line))
            mode-line-p (or mode-line-p
                            (member et telega-chatbuf--dirtiness-mode-line))
            footer-p (or footer-p
                         (member et telega-chatbuf--dirtiness-footer))
            prompt-p (or prompt-p
                         (member et telega-chatbuf--dirtiness-prompt))))

    (when header-line-p
      (telega-chatbuf--header-line-update))
    (when mode-line-p
      (telega-chatbuf--mode-line-update))
    (when footer-p
      (telega-chatbuf--footer-update))
    (when prompt-p
      (telega-chatbuf--prompt-update))
    ))

(defun telega-chatbuf--input-idx-valid-p (idx)
  "Return non-nil if input history position IDX is valid."
  (and (>= idx 0) (< idx (ring-length telega-chatbuf--input-ring))))

(defun telega-chatbuf-input-goto (idx)
  "Put input history item of the absolute history position IDX."
  ;; Save any pending input
  (unless telega-chatbuf--input-idx
    (setq telega-chatbuf--input-pending (telega-chatbuf-input-string)))

  (setq telega-chatbuf--input-idx idx)
  (telega-chatbuf--input-delete)
  (goto-char (point-max))

  (if (and idx (not (ring-empty-p telega-chatbuf--input-ring)))
      (insert (ring-ref telega-chatbuf--input-ring idx))

    ;; Restore pending input
    (when telega-chatbuf--input-pending
      (insert telega-chatbuf--input-pending))))

(defun telega-chatbuf-input-restore ()
  "Restore pending input."
  (when telega-chatbuf--input-idx
    (telega-chatbuf--input-delete)
    (when telega-chatbuf--input-pending
      (goto-char (point-max))
      (insert telega-chatbuf--input-pending))
    (setq telega-chatbuf--input-idx nil)))

(defun telega-chatbuf-input-prev (n)
  "Goto N previous items in chat input history."
  (interactive "p")
  (let ((idx (if (and telega-chatbuf--input-idx
                      (telega-chatbuf-has-input-p))
                 (+ telega-chatbuf--input-idx n)
               (1- n))))
    ;; clamp IDX
    (cond ((< idx 0)
           (setq idx nil)) ;; restory pending input
          ((>= idx (ring-length telega-chatbuf--input-ring))
           (setq idx (1- (ring-length telega-chatbuf--input-ring)))))
    (telega-chatbuf-input-goto idx)))

(defun telega-chatbuf-input-next (n)
  "Goto next N's item in chat input history."
  (interactive "p")
  (when (and telega-chatbuf--input-idx
             (telega-chatbuf-has-input-p))
    (telega-chatbuf-input-prev (- n))))

(defun telega-chatbuf-input-match (regexp forward-p)
  "Move point to previous Goto previous match."
  (let ((found (funcall (if forward-p 're-search-forward 're-search-backward)
                        regexp
                        (if forward-p (point-max) telega-chatbuf--input-marker)
                        t)))
    (unless found
      (let* ((step (if forward-p -1 1))
             (idx (if telega-chatbuf--input-idx
                      (+ telega-chatbuf--input-idx step)
                    0)))
        (while (and (telega-chatbuf--input-idx-valid-p idx)
                    (not (string-match
                          regexp (ring-ref telega-chatbuf--input-ring idx))))
          (cl-incf idx step))
        (when (telega-chatbuf--input-idx-valid-p idx)
          (telega-chatbuf-input-goto idx)
          (when forward-p
            (goto-char telega-chatbuf--input-marker))
          (telega-chatbuf-input-match regexp forward-p))))))

(defun telega-chatbuf--minibuf-post-command ()
  "Function to search chatbuf history input."
  (cl-assert (minibufferp))
  (let ((regexp (buffer-substring (minibuffer-prompt-end) (point))))
    (if (string-empty-p regexp)
        (with-telega-chatbuf telega-minibuffer--chat
          (telega-chatbuf-input-restore))

      (unless (string= telega-minibuffer--string regexp)
        (setq telega-minibuffer--string regexp)
        (with-telega-chatbuf telega-minibuffer--chat
          (telega-chatbuf-input-restore)
          (telega-chatbuf-input-match regexp nil))))))

(defun telega-chatbuf--input-search-prev (&optional forward-p)
  "For `C-r' in minibuffer."
  (interactive)
  (cl-assert (minibufferp))
  (let ((regexp telega-minibuffer--string))
    (with-telega-chatbuf telega-minibuffer--chat
      (telega-chatbuf-input-match regexp forward-p))))

(defun telega-chatbuf--input-search-next ()
  "For `C-s' in minibuffer."
  (interactive)
  (telega-chatbuf--input-search-prev 'forward))

(defun telega-chatbuf--input-search-cancel ()
  "Cancel input search results."
  (interactive)
  (cl-assert (minibufferp))
  (delete-region (minibuffer-prompt-end) (point))
  (exit-minibuffer))

(defun telega-chatbuf--input-search-accept ()
  "Accept input search results."
  (interactive)
  (cl-assert (minibufferp))
  (exit-minibuffer))

(defun telega-chatbuf--input-search-input-prev (&optional forward-p)
  (interactive)
  (cl-assert (minibufferp))
  (delete-region (minibuffer-prompt-end) (point))

  (let ((prompt-input (with-telega-chatbuf telega-minibuffer--chat
                        (telega-chatbuf-input-prev (if forward-p -1 1))
                        (telega-chatbuf-input-string))))
    (insert prompt-input)))

(defun telega-chatbuf--input-search-input-next ()
  (interactive)
  (telega-chatbuf--input-search-input-prev 'forward))

(defvar telega-chatbuf--input-search-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") 'telega-chatbuf--input-search-cancel)
    (define-key map (kbd "C-r") 'telega-chatbuf--input-search-prev)
    (define-key map (kbd "C-s") 'telega-chatbuf--input-search-next)
    (define-key map (kbd "M-p") 'telega-chatbuf--input-search-input-prev)
    (define-key map (kbd "M-n") 'telega-chatbuf--input-search-input-next)
    (define-key map (kbd "RET") 'telega-chatbuf--input-search-accept)
    map))

(defun telega-chatbuf-input-search ()
  "Search for REGEX in chat input history."
  (interactive)
  ;; Save pending input
  (setq telega-chatbuf--input-idx nil
        telega-chatbuf--input-pending (telega-chatbuf-input-string))

  (let* ((telega-minibuffer--string "")
         (telega-minibuffer--chat telega-chatbuf--chat)
         (regexp (minibuffer-with-setup-hook
                     (lambda ()
                       (add-hook 'post-command-hook
                                 'telega-chatbuf--minibuf-post-command t t))
                   (read-from-minibuffer "History input search: " nil
                                         telega-chatbuf--input-search-map))))
    (when (string-empty-p regexp)
      ;; Restore saved pending input if canceled
      (telega-chatbuf--input-delete)
      (goto-char (point-max))
      (insert telega-chatbuf--input-pending))
    ))

(defun telega-chatbuf-edit-next (markup-arg &optional backward)
  "Edit message sent next to currently editing.
MARKUP-ARG could be used to select markup to edit message.
See `telega-msg-edit' for details."
  (interactive "P")
  (let* ((msg-temex '(and (prop :can_be_edited)
                          (sender me)))
         (edit-msg (telega-chatbuf-editing-msg))
         (last-msg (telega-chatbuf--last-msg))
         (last-sent-msg
          (if (and backward
                   (not edit-msg)
                   (telega-msg-match-p last-msg msg-temex))
              last-msg
            (telega-chatbuf--next-msg (or edit-msg last-msg)
              msg-temex backward))))
    (if last-sent-msg
        (telega-msg-edit last-sent-msg markup-arg)

      (if (and edit-msg (not backward))
          (telega-chatbuf-cancel-aux 'delete-input)
        (user-error "Nothing to edit")))))

(defun telega-chatbuf-edit-prev (markup-arg)
  "Edit previously sent message.
MARKUP-ARG could be used to select markup to edit message.
See `telega-msg-edit' for details."
  (interactive "P")
  (telega-chatbuf-edit-next markup-arg 'backward))

(defun telega-chatbuf-beginning-of-thing (&optional arg)
  "Move backward to the beginning of the chat input or message."
  (interactive "p")
  (when (> (point) telega-chatbuf--input-marker)
    (goto-char telega-chatbuf--input-marker)
    (cl-decf arg))

  (when (> arg 0)
    (beginning-of-defun arg)))

(defun telega-chatbuf-set-language (language-code)
  "Associate chat with the language code."
  (interactive
   (list (telega-completing-read-language-code
          (concat "Language"
                  (when telega-chatbuf-language-code
                    (format " (current is %S)" telega-chatbuf-language-code))
                  ": "))))
  (setq telega-chatbuf-language-code language-code))

(defun telega-chatbuf--redisplay-node (node)
  "Redisplay NODE in chatbuffer.
Try to keep point at its position."
  ;; NOTE: MSG-BUTTON could be `nil' if message is ignored and not displayed
  (when-let ((msg-button (button-at (ewoc-location node))))
    (with-telega-buffer-modify
     (telega-save-window-start (button-start msg-button) (button-end msg-button)
       (if (eq (telega-msg-at (point)) (ewoc--node-data node))
           (telega-save-cursor
             (ewoc-invalidate telega-chatbuf--ewoc node))
         (telega-save-excursion
           (ewoc-invalidate telega-chatbuf--ewoc node))))))

  (let ((chat-win (get-buffer-window)))
    (if (not chat-win)
        (telega-buffer--hack-win-point)

      ;; Redetect cursor sensor
      (set-window-parameter chat-win 'cursor-sensor--last-state nil)
      ;; Inhibit button's help message
      (let ((telega-help-messages nil)
            (telega-msg-hover-in-hook nil)
            (telega-msg-hover-out-hook nil))
        (cursor-sensor--detect chat-win)))))

(defun telega-chatbuf--insert-messages (messages how)
  "Insert MESSAGES into chatbuf.
HOW could be `prepend' or `append', or `append-new'.
Return last inserted ewoc node."
  (with-telega-deferred-events
    (with-telega-buffer-modify
    (let* ((use-date-breaks-p
            (telega-chatbuf-match-p telega-chat-use-date-breaks-for))
           (node (if (eq how 'prepend)
                     (ewoc--header telega-chatbuf--ewoc)
                   (or (ewoc-nth telega-chatbuf--ewoc -1)
                       (ewoc--header telega-chatbuf--ewoc))))
           (saved-point (if (or (eq how 'prepend)
                                (and (eq how 'append-new)
                                     (>= (point) telega-chatbuf--input-marker)
                                     (not (telega-chatbuf--history-state-get
                                           :newer-freezed))))
                            (copy-marker (point) t)
                          (point)))
           ;; State of the current button if prepending
           (chat-win (get-buffer-window (current-buffer)))
           (msg-button (button-at (point)))
           (msg-button-was-observable-p
            (when (and (eq how 'prepend) chat-win msg-button)
              (telega-button--observable-p msg-button)))
           )
      (unwind-protect
          (seq-doseq (msg messages)
            (run-hook-with-args 'telega-chatbuf-pre-msg-insert-hook msg)
            ;; Track the uploading progress
            ;; see: https://github.com/zevlg/telega.el/issues/60
            (telega-msg--track-file-uploading-progress msg)

            ;; Possibly download sound file for the animated emoji
            ;; message, to be played instantly when message is opened
            (when (and (eq telega-emoji-animated-play 'with-sound)
                       (telega-msg-match-p msg '(type AnimatedEmoji)))
              (when-let ((sound (telega--tl-get
                                 msg :content :animated_emoji :sound)))
                (telega-file--download sound)))

            ;; Ensure cached message (if any) and node data is the same
            ;; object, so message can be modified inplace
            (telega-msg-cache msg)

            (cl-assert node)
            ;; Maybe insert date break, such as
            ;; -----(28 December 2020)-----
            (let ((node-msg (ewoc--node-data node)))
              (when (and use-date-breaks-p
                         (telega-msg-p node-msg)
                         (plist-get node-msg :date)
                         (plist-get msg :date)
                         ;; 3-6 elements are for DAY MONTH YEAR
                         (not (equal (seq-subseq (decode-time
                                                  (plist-get node-msg :date))
                                                 3 6)
                                     (seq-subseq (decode-time
                                                  (plist-get msg :date))
                                                 3 6))))
                (setq node (ewoc-enter-after
                            telega-chatbuf--ewoc node
                            (telega-msg-create-internal
                             telega-chatbuf--chat
                             (telega-fmt-text
                              (telega-ins--as-string
                               (telega-ins--date-full (plist-get msg :date)))
                              '(:@type "textEntityTypeBold")))))))

            (setq node (ewoc-enter-after telega-chatbuf--ewoc node msg))
            ;; NOTE: for outgoing message
            ;; `telega-chatbuf-post-msg-insert-hook' will be called on
            ;; "updateMessageSendSucceeded" event
            (unless (plist-get msg :sending_state)
              (run-hook-with-args 'telega-chatbuf-post-msg-insert-hook msg)))

        (goto-char saved-point))

      ;; If message at point was visible - keep it visible
      (when (and (memq msg-button-was-observable-p '(full top))
                 (equal msg-button (button-at (point))))
        (telega-button--make-observable msg-button))

      node))))

(defun telega-chatbuf--prepend-messages (messages)
  "Insert MESSAGES at the beginning of the chat buffer.
First message in MESSAGE will be first message at the beginning."
  (telega-chatbuf--insert-messages messages 'prepend))

(defun telega-chatbuf--append-new-message-p (msg)
  "Return non-nil if incoming message MSG should be appended."
  ;; NOTE: `:last_message' could be already updated in the chat
  ;; with the id of the MSG, so check for it
  ;; Also, do not insert new messages while loading history messages,
  ;; see https://github.com/zevlg/telega.el/issues/91
  (when (and (not telega-chatbuf--history-loading)
             (or (telega-chatbuf--last-msg-loaded-p)
                 (eq (telega-chatbuf--last-message-id)
                     (plist-get msg :id))))
    (cond ((or telega-chatbuf--msg-filter
               ;; Allow loading few more messages when newer history
               ;; is freezed
               (and (telega-chatbuf--history-state-get :newer-freezed)
                    (> (- (point-max) (point)) 2000)))
           ;; Update history state by side-effect
           (telega-chatbuf--history-state-delete :newer-loaded)
           (telega-chatbuf--chat-update "history-loading")
           nil)

          (telega-chatbuf--thread
           (eq (telega-chatbuf--message-thread-id)
               (plist-get msg :message_thread_id)))

          (t t))))

(defun telega-chatbuf--node-by-msg-id (msg-id)
  "In current chatbuffer find message button with MSG-ID."
  ;; NOTE: message IDs are monotonically grows from first to the last
  ;; message. If MSG-ID is closer to the first message's id, then to
  ;; the last one, then search from the beginning, otherwise search
  ;; from the end
  (when-let ((first-msg (telega-chatbuf--first-msg))
             (last-msg (telega-chatbuf--last-msg)))
    (when (and (>= msg-id (plist-get first-msg :id))
               (<= msg-id (plist-get last-msg :id)))
      (telega-ewoc--find
       telega-chatbuf--ewoc msg-id #'= (telega--tl-prop :id) nil
       (if (< (- msg-id (plist-get first-msg :id))
              (- (plist-get last-msg :id) msg-id))
           ;; MSG-ID is closer to the beginning
           #'ewoc--node-next
         #'ewoc--node-prev)))))

(defun telega-chatbuf--next-msg (msg msg-temex &optional backward-p)
  "Return message next to MSG matching MSG-TEMEX.
If BACKWARD-P is non-nil, then return previous message.
Return nil, if not found."
  (declare (indent 1))
  (with-telega-chatbuf (telega-msg-chat msg)
    (let* ((predicate (telega-match-gen-predicate 'msg msg-temex))
           (mnode (if (telega-msg-internal-p msg)
                      ;; NOTE: internal messages use same `:id' so
                      ;; can't search by `:id'
                      (telega-ewoc--find-by-data telega-chatbuf--ewoc msg)
                    (telega-chatbuf--node-by-msg-id (plist-get msg :id))))
           (mnode1 (if backward-p
                       (ewoc-prev telega-chatbuf--ewoc mnode)
                     (ewoc-next telega-chatbuf--ewoc mnode)))
           (nnode (and mnode1
                       (telega-ewoc--find-if
                        telega-chatbuf--ewoc predicate nil mnode1
                        (if backward-p #'ewoc--node-prev #'ewoc--node-next)))))
      (when nnode
        (ewoc--node-data nnode)))))

(defun telega-chatbuf--read-outbox (old-last-read-outbox-msgid)
  "Redisplay chat messages affected by read-outbox change.
OLD-LAST-READ-OUTBOX-MSGID is old value for chat's
`:last_read_outbox_message_id'."
  (let ((node (ewoc--footer telega-chatbuf--ewoc)))
    (while (and (setq node (ewoc-prev telega-chatbuf--ewoc node))
                (< old-last-read-outbox-msgid
                   (plist-get (ewoc-data node) :id)))
      (when (plist-get (ewoc-data node) :is_outgoing)
        (telega-chatbuf--redisplay-node node)))))

(defun telega-chatbuf--load-history (&optional from-msg-id offset limit
                                               callback)
  "Load chatbuf's history.
If FROM-MSG-ID is specified, then cancel last history load and
start loading messages from FROM-MSG-ID.
OFFSET and LIMIT are passed directly to `getChatHistory'.
CALLBACK is called after history has been loaded with single
argument - total number of loaded messages."
  (declare (indent 3))
  (when (and from-msg-id telega-chatbuf--history-loading)
    ;; Cancel currently active history load
    (telega-server--callback-put telega-chatbuf--history-loading 'ignore)
    (setq telega-chatbuf--history-loading nil))

  (unless telega-chatbuf--history-loading
    (unless from-msg-id
      (setq from-msg-id (plist-get (telega-chatbuf--first-msg) :id)
            offset 0))
    (unless from-msg-id
      ;; NOTE: Mark newer history is loaded in advance
      (telega-chatbuf--newer-history-loaded)
      (setq from-msg-id (telega-chatbuf--last-message-id)
            offset -1))

    (when from-msg-id
      ;; Asynchronously load chat history
      (let* ((chat telega-chatbuf--chat)
             (history-callback
              (lambda (history)
                (with-telega-chatbuf chat
                  ;; NOTE: some messages might be already inserted in
                  ;; the chatbuf, so prepend older messages before
                  ;; first message in the chatbuf, and append newer
                  ;; messages after last message in the chatbuf
                  (let ((messages (plist-get history :messages))
                        (first-msg (telega-chatbuf--first-msg))
                        (last-msg (telega-chatbuf--last-msg)))
                    (if (and first-msg last-msg)
                        (let ((after-last (seq-take-while
                                           (lambda (elem)
                                             (> (plist-get elem :id)
                                                (plist-get last-msg :id)))
                                           messages))
                              (before-first (seq-drop-while
                                             (lambda (elem)
                                               (>= (plist-get elem :id)
                                                   (plist-get first-msg :id)))
                                             messages)))
                          (telega-chatbuf--insert-messages
                           (nreverse before-first) 'prepend)
                          (telega-chatbuf--insert-messages
                           (nreverse after-last) 'append))
                      (telega-chatbuf--insert-messages
                       (nreverse messages) 'prepend))

                    ;; NOTE: Message insertation might trigger history
                    ;; loading, thats why
                    ;; `telega-chatbuf--history-loading' is reseted
                    ;; only after all the messages are inserted?
                    (setq telega-chatbuf--history-loading nil)
                    (when (zerop (length messages))
                      (if (>= offset 0)
                          (telega-chatbuf--older-history-loaded)
                        (telega-chatbuf--newer-history-loaded)))
                    (when callback
                      (funcall callback (plist-get history :total_count)))
                    (telega-chatbuf--chat-update "history-loading"))))))
        (setq telega-chatbuf--history-loading
              (cond (telega-chatbuf--msg-filter
                     (telega--searchChatMessages chat
                         (plist-get telega-chatbuf--msg-filter :tdlib-msg-filter)
                         from-msg-id offset
                       :query (plist-get telega-chatbuf--msg-filter :query)
                       :limit limit
                       :sender (plist-get telega-chatbuf--msg-filter :sender)
                       :callback history-callback))
                    ((not (zerop (telega-chatbuf--message-thread-id)))
                     (telega--getMessageThreadHistory
                         chat (telega-chatbuf--message-thread-id)
                         from-msg-id offset limit
                       history-callback))
                    (t
                     (telega--getChatHistory
                         chat from-msg-id offset
                         (or limit telega-chat-history-limit) nil
                       history-callback)))))
      (telega-chatbuf--chat-update "history-loading")
      )))

(defun telega-chatbuf--load-older-history (&optional callback)
  "In chat buffer load older messages.
CALLBACK if non-nil, then called with total number of loaded messages."
  (if (and telega-chatbuf--msg-filter
           (functionp (plist-get telega-chatbuf--msg-filter :tdlib-msg-filter)))
      ;; no-op: chatbuf messages filter is a function
      nil

    ;; NOTE: if there is no messages in the chatbuf, then show last
    ;; message first and then start loading the history
    (when (and (not telega-chatbuf--history-loading)
               (not (or telega-chatbuf--msg-filter
                        telega-chatbuf--thread))
               ;; And no messages are loaded
               (not (telega-chatbuf--first-msg)))
      (when-let ((msg (plist-get telega-chatbuf--chat :last_message)))
        (when (telega-chatbuf--append-new-message-p msg)
          (telega-chatbuf--insert-messages (list msg) 'append-new))))

    (cl-assert (telega-chatbuf--need-older-history-p))
    (telega-chatbuf--load-history nil nil nil callback)
    ))

(defun telega-chatbuf--load-newer-history ()
  "In chat buffer load newer messages."
  (if (and telega-chatbuf--msg-filter
           (functionp (plist-get telega-chatbuf--msg-filter :tdlib-msg-filter)))
      ;; no-op: chatbuf messages filter is a function
      nil

    (cl-assert (telega-chatbuf--need-newer-history-p))
    (when (and (not telega-chatbuf--history-loading)
               (telega-chatbuf--last-msg))
      (telega-chatbuf--load-history (plist-get (telega-chatbuf--last-msg) :id)
          (- 1 telega-chat-history-limit) telega-chat-history-limit))))

(defun telega-chatbuf-cancel-markup (begin end)
  "Cancel markup for the given region in the chatbuf input."
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list telega-chatbuf--input-marker (point-max))))
  (unless (and (>= begin telega-chatbuf--input-marker)
               (> end telega-chatbuf--input-marker))
    (user-error "telega: Can cancel markup only inside chatbuf input"))
  (remove-text-properties begin end '(face nil :tl-entity-type nil)))

(defun telega-chatbuf-cancel-aux (&optional arg)
  "Cancel current aux prompt.
If prefix ARG is given, also delete input."
  (interactive "P")
  (telega-chatbuf--prompt-reset)
  (when arg
    (telega-chatbuf--input-delete)))

(defun telega-chatbuf-cancel-dwim ()
  "Cancel in Do What I Mean manner.
Call `telega-chatbuf-cancel-markup' if region is active.
Call `telega-chatbuf-cancel-aux' if replying/editing to a message.
Otherwise clear chatbuf input."
  (interactive)
  (cond ((region-active-p)
         (call-interactively #'telega-chatbuf-cancel-markup))
        (telega-chatbuf--aux-plist
         (call-interactively #'telega-chatbuf-cancel-aux))
        (t
         (telega-chatbuf--input-delete))))

(defun telega-help-message--cancel-aux (what)
  "Show help about canceling reply/edit in echo area."
  (telega-help-message what "%s to cancel %S"
    (telega-keys-description 'telega-chatbuf-cancel-aux telega-chat-mode-map)
    what))

(defun telega-chatbuf--input-imcs (markup-name &optional input)
  "Convert input to input message contents list.
MARKUP-NAME names a markup function from
`telega-chat-markup-functions' to be used for input formatting."
  (cl-assert (or (null markup-name)
                 (assoc markup-name telega-chat-markup-functions)))
  (let ((markup-function (cdr (assoc markup-name telega-chat-markup-functions)))
        (attaches (telega--split-by-text-prop
                      (or input (telega-chatbuf-input-string)) 'telega-attach))
        (disable-webpage-preview telega-chat-send-disable-webpage-preview)
        result)
    (while attaches
      (let* ((text (car attaches))
             (attach (get-text-property 0 'telega-attach text)))
        (cond
         ((not attach)
          ;; Simple text
          ;; Check the limit first
          (when (> (length text)
                   (plist-get telega--options :message_text_length_max))
            (error "Message length exceedes %d limit"
                   (plist-get telega--options :message_text_length_max)))

          ;; NOTE: blank messages are not allowed
          ;; see https://github.com/zevlg/telega.el/issues/359
          (unless (string-blank-p text)
            (push (list :@type "inputMessageText"
                        :text (telega-string-fmt-text text markup-function)
                        :disable_web_page_preview
                        (if disable-webpage-preview t :false)
                        :clear_draft t)
                  result)))

         ;; Special attachment to disable web-page preview
         ((eq (telega--tl-type attach) 'telegaDisableWebpagePreview)
          (setq disable-webpage-preview t))

          ;; Some real attachment:
          ;; 1) If attachment followed by plain text, then it might be
          ;; a caption for the attachment, in this case add caption
          ;; to the attachment.
          ;; 2) Special case is for forwarded messages, new caption can
          ;; be supplied for the forwarded message only if forwarded
          ;; message as copy and original caption is removed (`C-u C-u
          ;; f' behaviour)
         (t
          (when (and (or (memq (telega--tl-type attach)
                               '(inputMessageAnimation
                                 inputMessageAudio
                                 inputMessageDocument
                                 inputMessagePhoto
                                 inputMessageVideo
                                 inputMessageVoiceNote))
                         ;; New caption for the forwarded message?
                         (and (eq (telega--tl-type attach) 'telegaForwardMessage)
                              (plist-get attach :send_copy)
                              (plist-get attach :remove_caption)))
                     (cadr attaches)
                     (not (get-text-property 0 'telega-attach (cadr attaches))))
            ;; NOTE: there is caption limit in telegram
            ;; Attach the caption
            (when (> (length (cadr attaches))
                     (plist-get telega--options :message_caption_length_max))
              (error "Caption exceedes %d limit"
                     (plist-get telega--options :message_caption_length_max)))

            (let ((cap (telega-string-fmt-text (cadr attaches) markup-function)))
              (setq attach (plist-put attach :caption cap)))
            (setq attaches (cdr attaches)))
          (push attach result))))

      (setq attaches (cdr attaches)))
    (nreverse result)))

(defun telega-chatbuf--input-options (imc)
  "Convert IMC to send options.
Return valid \"messageSendOptions\"."
  (cl-ecase (telega--tl-type imc)
    (telegaScheduledMessage
     (let ((timestamp (plist-get imc :timestamp)))
       (list :@type "messageSendOptions"
             :scheduling_state
             (if timestamp
                 (list :@type "messageSchedulingStateSendAtDate"
                       :send_date timestamp)
               (list :@type "messageSchedulingStateSendWhenOnline")))))

    (telegaDisableNotification
       (list :@type "messageSendOptions"
             :disable_notification (plist-get imc :disable_notification)))
    ))

(defun telega-chatbuf--input-imc-cancel-upload-ahead (imc)
  "For file used in IMC cancel its ahead uploading."
  (when-let* ((file-prop-alist '((inputMessageDocument  . :document)
                                 (inputMessagePhoto     . :photo)
                                 (inputMessageVideo     . :video)
                                 (inputMessageAudio     . :audio)
                                 (inputMessageVideoNote . :video_note)
                                 (inputMessageVoiceNote . :voice_note)
                                 (inputMessageAnimation . :animation)))
              (file-prop (cdr (assq (telega--tl-type imc) file-prop-alist)))
              (ifile (plist-get imc file-prop))
              (upload-ahead-file
               (get-text-property 0 'telega-upload-ahead-file
                                  (plist-get ifile :@type))))
    (telega--cancelPreliminaryUploadFile upload-ahead-file)))

(defun telega-chatbuf-input-send (markup-name)
  "Send chatbuf input to the chat.
If called interactively, number of `\\[universal-argument]' before
command determines index in `telega-chat-input-markups' of markup to
use.  For example `C-u RET' will use
`(nth 1 telega-chat-input-markups)' markup."
  (interactive (list (if (and current-prefix-arg (listp current-prefix-arg))
                         (nth (round (log (car current-prefix-arg) 4))
                              telega-chat-input-markups)
                       (car telega-chat-input-markups))))

  (let* ((input (telega-chatbuf-input-string))
         (imcs (telega-chatbuf--input-imcs markup-name input))
         (replying-imr (telega-chatbuf-replying-imr))
         (editing-msg (telega-chatbuf-editing-msg))
         (options nil)
         (send-imcs nil))
    ;; NOTE: Allow removing captions, see
    ;; https://github.com/zevlg/telega.el/issues/252
    (when (and (null imcs)
               editing-msg
               (telega--tl-get editing-msg :content :caption))
      (setq imcs (list (list :@type "inputMessageText"
                             :text (telega-string-fmt-text "")))))

    ;; Send the input by traversing IMCS and sending composed
    ;; SEND-IMCS
    (while imcs
      (cond
       (editing-msg
        ;; Possible reschedule the message
        (when (eq 'telegaScheduledMessage (telega--tl-type (car imcs)))
          (unless (plist-get editing-msg :scheduling_state)
            (user-error "telega: Can't reschedule non-scheduled message"))
          (let ((timestamp (plist-get (car imcs) :timestamp)))
            (telega--editMessageSchedulingState editing-msg
              (if timestamp
                  (list :@type "messageSchedulingStateSendAtDate"
                        :send_date timestamp)
                (list :@type "messageSchedulingStateSendWhenOnline"))))
          (setq imcs (cdr imcs)))

        (when (> (length imcs) 1)
          (user-error "telega: Multiple input messages while edit"))

        (setq send-imcs (seq-take imcs 1))
        (let ((edit-mc (plist-get editing-msg :content))
              (imc (car send-imcs)))
          (cond ((and ;(eq (telega--tl-type imc) 'inputMessageLocation)
                  (eq (telega--tl-type edit-mc) 'messageLocation))
                 (telega--editMessageLiveLocation
                  editing-msg (plist-get imc :location)
                  :sync-p (not telega-chat-send-messages-async)))

                ((and (eq (telega--tl-type imc) 'inputMessageText)
                      (eq (telega--tl-type edit-mc) 'messageText))
                 (telega--editMessageText
                  editing-msg imc
                  :sync-p (not telega-chat-send-messages-async)))

                ((eq (telega--tl-type imc) 'inputMessageText)
                 (telega--editMessageCaption
                  editing-msg (plist-get imc :text)
                  :sync-p (not telega-chat-send-messages-async)))

                (t
                 (telega--editMessageMedia
                  editing-msg imc
                  :sync-p (not telega-chat-send-messages-async))))))

       ;; Messages can be sent as album if:
       ;; - All messages are photos or videos
       ;; - All messages are documents
       ;; - All messages are audio
       ;; NOTE: cl-every returns `t' on empty list
       ;;
       ;; NOTE: maximum 10 messages can be grouped to album
       ;; See https://t.me/emacs_telega/22918
       ((and (> (length imcs) 1)
             (let ((album-types
                    (cl-find (telega--tl-type (car imcs))
                             '((inputMessagePhoto inputMessageVideo)
                               (inputMessageDocument)
                               (inputMessageAudio))
                             :test #'memq)))
               (setq send-imcs
                     (seq-take (seq-take-while
                                (lambda (imc)
                                  (memq (telega--tl-type imc) album-types))
                                imcs)
                               10))))
        (telega--sendMessageAlbum
         telega-chatbuf--chat send-imcs replying-imr options
         :sync-p (not telega-chat-send-messages-async)))

       ;; NOTE: TDLib will automatically group messages to albums when
       ;; forwarding multiple messages.  Message IDS must be in strictly
       ;; increasing order, otherwise TDLib triggers an error
       ((and (> (length imcs) 1)
             (not replying-imr)
             (let ((msg-id 0)
                   (chat-id (telega--tl-get (car imcs) :message :chat_id))
                   (send-copy (plist-get (car imcs) :send_copy))
                   (rm-caption (plist-get (car imcs) :remove_caption)))
               (setq send-imcs
                     (seq-take-while
                      (lambda (imc)
                        (and (eq (telega--tl-type imc) 'telegaForwardMessage)
                             (eq chat-id (telega--tl-get imc :message :chat_id))
                             (not (plist-get imc :caption))
                             (equal send-copy (plist-get imc :send_copy))
                             (equal rm-caption (plist-get imc :remove_caption))
                             ;; Check for strictly increasing ID order
                             (when (> (telega--tl-get imc :message :id) msg-id)
                               (setq msg-id (telega--tl-get imc :message :id)))))
                      imcs))))
        (telega--forwardMessages
         telega-chatbuf--chat
         (telega-msg-chat (plist-get (car send-imcs) :message))
         (mapcar (telega--tl-prop :message) send-imcs) options
         (plist-get (car send-imcs) :send_copy)
         (plist-get (car send-imcs) :remove_caption)
         :sync-p (not telega-chat-send-messages-async)))

     (t
      (setq send-imcs (seq-take imcs 1))
      (let ((imc (car send-imcs)))
        (cl-case (telega--tl-type imc)
          (telegaInlineQuery
           (telega--sendInlineQueryResultMessage
            telega-chatbuf--chat imc replying-imr options
            :sync-p (not telega-chat-send-messages-async)))

          (telegaForwardMessage
           (let* ((msg (plist-get imc :message))
                  (copy-opts
                   (nconc (list :@type "messageCopyOptions"
                                ;; NOTE: force copying if replying
                                ;; to message.  TDLib 1.7.10 can
                                ;; forward copy as reply to a
                                ;; message
                                :send_copy
                                (if (or (plist-get imc :send_copy) replying-imr)
                                    t :false)
                                :replace_caption
                                (if (plist-get imc :remove_caption)
                                    t :false))
                          (when-let ((new-cap (plist-get imc :caption)))
                            (list :new_caption new-cap))))
                  (fwd-imc (list :@type "inputMessageForwarded"
                                 :from_chat_id (plist-get msg :chat_id)
                                 :message_id (plist-get msg :id)
                                 :copy_options copy-opts)))
             (telega--sendMessage
              telega-chatbuf--chat
              fwd-imc replying-imr options
              :sync-p (not telega-chat-send-messages-async))
             (when (plist-get imc :unmark-after-sent)
               (telega-msg-unmark msg))))

          ((telegaScheduledMessage telegaDisableNotification)
           ;; Merge new imc options into existing options
           (telega--tl-dolist ((prop value) (telega-chatbuf--input-options imc))
             (setq options (plist-put options prop value))))

          (telegaChatTheme
           (telega--setChatTheme
            telega-chatbuf--chat (or (plist-get imc :name) "")))

          (telegaDelimiter
           ;; No-op, just delimits messages
           )

          (t (telega--sendMessage
              telega-chatbuf--chat imc replying-imr options
              :sync-p (not telega-chat-send-messages-async)))))))

      ;; NOTE: Cancell all file upload ahead, initiated by
      ;; attachements in `send-imcs' See
      ;; https://github.com/tdlib/td/issues/1348#issuecomment-752465634
      ;; NOTE: Currently this does not cancel uploads, as noted in
      ;; https://github.com/tdlib/td/issues/1348#issuecomment-752654650
      (dolist (imc send-imcs)
        (telega-chatbuf--input-imc-cancel-upload-ahead imc))

      ;; Continue traversing, stripping SEND-IMCS from IMCS
      ;; Each cond clause above must set SEND-IMCS
      (cl-assert (> (length send-imcs) 0))
      (setq imcs (last imcs (- (length imcs) (length send-imcs)))
            send-imcs nil))

    ;; Recover prompt to initial state
    (telega-chatbuf--input-delete)
    (telega-chatbuf--prompt-reset)

    ;; Save input to history
    (unless (string-empty-p input)
      (ring-insert telega-chatbuf--input-ring input)
      (setq telega-chatbuf--input-idx nil
            telega-chatbuf--input-pending nil))))

(defun telega-chatbuf-newline-or-input-send ()
  "Insert newline or send chatbuf input.
Behaviour depends on point position and value for
`telega-chat-send-message-on-ret'."
  (interactive)
  (if (or (eq 'always telega-chat-send-message-on-ret)
          (eobp)                        ;point at the end of the prompt
          (and (eq 'if-at-the-end-or-C-u telega-chat-send-message-on-ret)
               current-prefix-arg))
      (call-interactively #'telega-chatbuf-input-send)
    (call-interactively #'newline)))

(defun telega-chatbuf-input-insert (imc)
  "Insert input content defined by IMC into chatbuf input.
IMC might be a plain string or attachment specification."
  ;; Check that point is in input area, otherwise move to the end
  (when (< (point) telega-chatbuf--input-marker)
    (goto-char (point-max)))

  (when (and (> (point) telega-chatbuf--input-marker)
             (get-text-property (point) 'telega-attach))
    (telega-ins " "))
  (if (stringp imc)
      (telega-ins imc)
    ;; NOTE: Put special properties `attach-open-bracket' and
    ;; `attach-close-bracket' to be used by
    ;; `telega-chatbuf--post-command' to determine if part of
    ;; attachment is deleted by `delete-char' or `backward-delete'
    (telega-ins--with-props
        `(telega-attach ,imc face telega-chat-input-attachment)
      (telega-ins--with-props '(cursor-intangible t)
        (when (telega-ins-prefix
                  (propertize (car telega-symbol-attach-brackets)
                              'attach-open-bracket t)
                (telega-ins--input-content-one-line imc))
          (telega-ins (cdr telega-symbol-attach-brackets))))
      (telega-ins--with-props '(attach-close-bracket t rear-nonsticky t)
        (telega-ins " ")))))

(defun telega-chatbuf-input-has-attaches-p ()
  "Return non-nil if chatbuf's input has some attaches."
  (let ((attaches (telega--split-by-text-prop
                   (telega-chatbuf-input-string) 'telega-attach)))
    (not (and (= (length attaches) 1)
              (not (get-text-property 0 'telega-attach (car attaches)))))))

(defun telega-chatbuf--enable-compact-media-view ()
  "Enable compact view for media messages.
For filters from `telega-chat-message-filters-as-media'."
  (setq telega-chatbuf--messages-compact-view t)
  (setq-local telega-inserter-for-msg-button
              #'telega-ins--message-media-compact)
  (setq-local telega-ignored-messages-visible nil))

(defun telega-chatbuf--disable-compact-media-view ()
  "Disable compact view for media messages.
For filters from `telega-chat-message-filters-as-media'."
  (setq telega-chatbuf--messages-compact-view nil)
  (kill-local-variable 'telega-inserter-for-msg-button)
  (kill-local-variable 'telega-ignored-messages-visible))

(defun telega-chatbuf--filter-msg-position-load (message)
  "Asynchronously update MESSAGE's position in active message filter."
  (when (and telega-chatbuf--msg-filter
             ;; Check someone displays messages filter
             (or (member "msg-filter" telega-chatbuf--dirtiness-header-line)
                 (member "msg-filter" telega-chatbuf--dirtiness-mode-line)
                 (member "msg-filter" telega-chatbuf--dirtiness-footer)
                 (member "msg-filter" telega-chatbuf--dirtiness-prompt)))
    (let ((tdlib-msg-filter
           (plist-get telega-chatbuf--msg-filter :tdlib-msg-filter)))
      (when (and (listp tdlib-msg-filter)
                 ;; NOTE: From TDLib docs:
                 ;; searchMessagesFilterEmpty,
                 ;; searchMessagesFilterUnreadMention,
                 ;; searchMessagesFilterUnreadReaction, and
                 ;; searchMessagesFilterFailedToSend are
                 ;; unsupported in `getChatMessagePosition' method
                 (not (memq (telega--tl-type tdlib-msg-filter)
                            '(searchMessagesFilterEmpty
                              searchMessagesFilterUnreadMention
                              searchMessagesFilterUnreadReaction
                              searchMessagesFilterFailedToSend))))
        (telega-chatbuf--filter-msg-position-loading-cancel)
        (plist-put telega-chatbuf--msg-filter :msg-position
                   (cons 'loading
                         (telega--getChatMessagePosition
                             message tdlib-msg-filter nil
                           (lambda (position)
                             (with-telega-chatbuf (telega-msg-chat message)
                               (when telega-chatbuf--msg-filter
                                 (plist-put telega-chatbuf--msg-filter
                                            :msg-position position)
                                 (telega-chatbuf--chat-update "msg-filter")))))))
        ))))

(defun telega-chatbuf--filter-msg-position-loading-cancel ()
  "Cancel pending request to current message position."
  (when-let ((position (plist-get telega-chatbuf--msg-filter :msg-position)))
    (when (consp position)
      (cl-assert (eq 'loading (car position)))
      (telega-server--callback-put (cdr position) 'ignore))))

(defun telega-chatbuf--filter-reset (&optional no-chat-update-p)
  "Reset `telega-chatbuf--msg-filter' in the chatbuf.
If NO-CHAT-UPDATE-P, do not trigger chatbuf's headers update."
  (unless (or (and (listp telega-chatbuf--inhibit-filter-reset)
                   (memq 'msg-filter telega-chatbuf--inhibit-filter-reset))
              telega-chatbuf--inhibit-filter-reset)
    (telega-chatbuf--disable-compact-media-view)
    (telega-chatbuf--filter-msg-position-loading-cancel)
    (setq telega-chatbuf--msg-filter nil)

    (unless no-chat-update-p
      (telega-chatbuf--chat-update "msg-filter"))))

(defun telega-chatbuf--thread-reset (&optional no-chat-update-p)
  "Reset `telega-chatbuf--thread' in the chatbuf.
If NO-CHAT-UPDATE-P, do not trigger chatbuf's headers update."
  (unless (or (and (listp telega-chatbuf--inhibit-filter-reset)
                   (memq 'thread telega-chatbuf--inhibit-filter-reset))
              telega-chatbuf--inhibit-filter-reset)
    (setq telega-chatbuf--thread nil)

    (unless no-chat-update-p
      (telega-chatbuf--chat-update "thread"))))

(defun telega-chatbuf--clean ()
  "Remove all messages displayed in chatbuf."
  (telega-ewoc--clean telega-chatbuf--ewoc)
  (setq telega-chatbuf--history-state-plist nil))

(defun telega-chatbuf-history-beginning ()
  "Jump to the first message in the chat history."
  ;; See https://github.com/tdlib/td/issues/195
  (interactive)
  (if (not (telega-chatbuf--need-older-history-p))
      (goto-char (point-min))

    (telega-chatbuf--clean)
    (telega-chatbuf--load-history
        10
        ;; NOTE: For `searchChatMessages' limit must be greater than -offset
        (- 1 telega-chat-history-limit)
        telega-chat-history-limit
      (lambda (_ignored)
        (telega-chatbuf--older-history-loaded)
        (goto-char (point-min))))))

(defun telega-chatbuf-recenter-1 (arg)
  "Recenter for chatbuf.
Call `(recenter -1)' if point is at prompt, otherwise call `recenter' as-is."
  (interactive "P")
  (if (and (not arg) (<= telega-chatbuf--input-marker (point)))
      (recenter -1)
    (when (commandp telega-chatbuf--origin-recenter-command)
      (call-interactively telega-chatbuf--origin-recenter-command))))

(defun telega-chatbuf-read-all ()
  "Jump to the last message in the chat history and mark all messages as read."
  (interactive)
  (unless (telega-chatbuf--last-msg-loaded-p)
    (telega-chatbuf--clean)
    (telega-chatbuf--load-older-history))

  (goto-char (point-max)))

(defun telega-chatbuf-msg-marks-toggle ()
  "Unmark all marked messages in chatbuf."
  (interactive)
  (let ((marked-messages))
    (if telega-chatbuf--marked-messages
        (setq marked-messages telega-chatbuf--marked-messages
              telega-chatbuf--marked-messages-1
              telega-chatbuf--marked-messages
              telega-chatbuf--marked-messages
              nil)

      (setq telega-chatbuf--marked-messages
            telega-chatbuf--marked-messages-1
            marked-messages telega-chatbuf--marked-messages))

    (dolist (msg marked-messages)
      (telega-msg-redisplay msg))

    (telega-chatbuf--chat-update "marked-messages")))

(defun telega-chatbuf-next-unread (&optional button-callback)
  "Goto next uneard message in chat.
BUTTON-CALLBACK - callback to call with single argument - message
button."
  (declare (indent 0))
  (interactive)
  (let ((telega-chatbuf--inhibit-filter-reset '(msg-filter thread)))
    (telega-chat--goto-msg telega-chatbuf--chat
        (telega-chatbuf--last-read-inbox-msg-id) nil
      (lambda ()
        ;; NOTE:
        ;; - deleted messages can't be marked as read, so point will
        ;;   stuck at deleted messag, so we just skip such messages
        ;; - `telega-button-forward' returns nil if there is no button
        ;;   matching predicate.  In this case just move to the prompt
        (let ((button (telega-button-forward 1
                        (lambda (button)
                          (when-let ((msg (telega-msg-at button)))
                            (and (not (telega-msg-internal-p msg))
                                 (not (telega-msg-match-p msg 'is-deleted)))))
                        'interactive)))
          (if button
              (when button-callback
                (funcall button-callback button))
            (goto-char (point-max))))))))

(defun telega-chatbuf-next-unread-mention ()
  "Goto next unread mention in chat buffer.
If there is no unread mentions, then search for last mention starting
from message at point."
  (interactive)

  (let* ((has-unread-mentions-p
          (not (zerop (plist-get telega-chatbuf--chat :unread_mention_count))))
         (reply
          (telega--searchChatMessages telega-chatbuf--chat
              (if has-unread-mentions-p
                  '(:@type "searchMessagesFilterUnreadMention")
                '(:@type "searchMessagesFilterMention"))
              (if has-unread-mentions-p
                  0
                (or (plist-get (telega-msg-at (point)) :id) 0))
              0
            :limit 1))
         (next-unread-mention-msg
          (car (append (plist-get reply :messages) nil))))
    (unless next-unread-mention-msg
      (user-error "telega: Can't fetch next unread mention message"))
    (telega-msg-goto next-unread-mention-msg 'highlight)
    ))

(defun telega-chatbuf-next-unread-reaction ()
  "Goto next unread reaction in chat buffer."
  (interactive)
  (let* ((unread-reactions
          (plist-get telega-chatbuf--chat :unread_reaction_count))
         (reply
          (unless (zerop unread-reactions)
            (telega--searchChatMessages telega-chatbuf--chat
                '(:@type "searchMessagesFilterUnreadReaction")
                0 0
              :limit 1)))
         (next-unread-reaction-msg
          (car (append (plist-get reply :messages) nil))))
    (unless next-unread-reaction-msg
      (user-error "telega: No messages with unread reaction"))
    (telega-msg-goto next-unread-reaction-msg 'highlight)
    ))

(defun telega-chat-favorite-messages (chat)
  "Return entries from the `telega--favorite-messages' for the CHAT."
  (cl-remove (plist-get chat :id) telega--favorite-messages
             :test-not 'eq :key #'car))

(defun telega-chatbuf-next-favorite ()
  "Goto next favorite message."
  (interactive)
  (let* ((fav-ids
          ;; NOTE: Sort favorite messages ids by id decreasing order
          (sort (mapcar #'cadr
                        (telega-chat-favorite-messages telega-chatbuf--chat))
                #'>))
         (next-fav-id
          (or (cl-find (or (plist-get (telega-msg-at (point)) :id) -1)
                       fav-ids :test #'<)
              ;; wrap to first one
              (car fav-ids))))
    (unless next-fav-id
      (user-error "No favorite messages in the chat"))
    (telega-chat--goto-msg telega-chatbuf--chat next-fav-id 'highlight)))

(defun telega-chatbuf-goto-reply-markup-message ()
  "Goto chat's reply markup message."
  (interactive)
  (let ((reply-markup-msg
         (telega-chat-reply-markup-msg telega-chatbuf--chat)))
    (unless reply-markup-msg
      (user-error "telega: No reply markup message for this chat"))

    (telega-msg-goto-highlight reply-markup-msg)))

(defun telega-chat-linked-chat (chat)
  "Return linked chat for the CHAT.
Return nil if CHAT has no linked chat."
  (let ((supergroup (telega-chat--info chat))
        (telega-full-info-offline-p nil))
    (when (plist-get supergroup :has_linked_chat)
      (telega-chat-get
       (plist-get (telega--full-info supergroup) :linked_chat_id) 'offline))))

(defun telega-chatbuf-goto-linked-chat ()
  "Goto chat linked to current chat buffer channel."
  (interactive)
  (let ((linked-chat (telega-chat-linked-chat telega-chatbuf--chat)))
    (unless linked-chat
      (user-error "telega: %s has no linked chat"
                  (telega-chat-title telega-chatbuf--chat)))

    (telega-chat--pop-to-buffer linked-chat)))

(defun telega-chatbuf-goto-pinned-message ()
  "Goto next pinned message for the chatbuffer."
  (interactive)
  (let* ((pinned-messages (plist-get telega-chatbuf--chat
                                     :telega-pinned-messages))
         (pinned-msg-idx (plist-get telega-chatbuf--chat
                                    :telega-pinned-message-index))
         (pinned-msg (nth pinned-msg-idx pinned-messages)))
    (unless pinned-msg
      (user-error "telega: No pinned messages in this chat"))

    ;; Update the index for next
    ;; `telega-chatbuf-goto-pinned-message'
    (setq pinned-msg-idx (1+ pinned-msg-idx))
    (unless (< pinned-msg-idx (length pinned-messages))
      (setq pinned-msg-idx 0))
    (plist-put telega-chatbuf--chat
               :telega-pinned-message-index pinned-msg-idx)
    (telega-chatbuf--chat-update "pinned-messages")

    (telega-msg-goto-highlight pinned-msg)))

(defun telega-chatbuf-goto-thread-message ()
  "Goto current thread's root message."
  (interactive)
  (unless (telega-chatbuf--thread-msg)
    (user-error "telega: No thread filtering in chatbuf"))

  (telega-msg-goto-highlight (telega-chatbuf--thread-msg)))

(defun telega-chatbuf-goto-pop-message ()
  "Pop message from `telega-chatbuf--messages-pop-ring' and goto it."
  (interactive)
  (when (ring-empty-p telega-chatbuf--messages-pop-ring)
    (user-error "telega: No messages to pop to"))

  (let ((pop-to-msg (ring-remove telega-chatbuf--messages-pop-ring 0)))
    (message "telega: %d messages left in messages ring"
             (ring-length telega-chatbuf--messages-pop-ring))
    ;; NOTE: by binding `telega-chatbuf--messages-pop-ring' to nil, we
    ;; avoid putting current message into
    ;; `telega-chatbuf--messages-pop-ring'
    (let ((telega-chatbuf--messages-pop-ring nil))
      (telega-msg-goto-highlight pop-to-msg))))

(defun telega-chatbuf-goto-video-chat ()
  "Goto video chat associated with the chat."
  (interactive)
  (let ((group-call (telega-chat-group-call telega-chatbuf--chat)))
    (unless group-call
      (user-error "telega: No group call associated with the chat"))
    (telega-describe-group-call group-call)))

(defun telega-chatbuf-goto-date (date)
  "Goto last message before DATE timestamp."
  (interactive (list (telega-read-timestamp "History at" 'only-date)))
  (telega--getChatMessageByDate (plist-get telega-chatbuf--chat :id) date
    (lambda (msg)
      (if (not msg)
          (message "telega: No chat history at %s"
                   (format-time-string "%Y-%m-%d" (seconds-to-time date)))
        (telega-msg-goto-highlight msg)))))

;;; Attaching stuff to the input
(defun telega-chatbuf-attach-location (location &optional live-secs)
  "Attach location to the chatbuf input.
If `\\[universal-argument]' is given, then attach live location."
  (interactive (list (with-telega-chatbuf-action "ChoosingLocation"
                       (if current-prefix-arg
                           (telega-read-live-location "Live Location")
                         (telega-read-location "Location")))
                     (when current-prefix-arg
                       (let* ((choices `(("1 min" . 60)
                                         ("15 min" . ,(* 15 60))
                                         ("1 hour" . ,(* 60 60))
                                         ("8 hours" . ,(* 8 60 60))))
                              (live-for (funcall telega-completing-read-function
                                                 "Live for: "
                                                 (mapcar 'car choices) nil t)))
                         (cdr (assoc live-for choices))))))

  (telega-chatbuf-input-insert
   (nconc (list :@type "inputMessageLocation"
                :location (cons :@type (cons "location" location)))
          (when live-secs
            (list :live_period live-secs)))))

(defun telega-chatbuf-attach-contact (contact)
  "Attach CONTACT user to the chatbuf input."
  (interactive
   (with-telega-chatbuf-action "ChoosingContact"
     (let* ((contacts (telega--getContacts))
            (names-alist (mapcar (lambda (user)
                                   (cons (telega-msg-sender-title user
                                           :with-avatar-p t
                                           :with-username-p t)
                                         user))
                                 contacts))
            (name (funcall telega-completing-read-function
                           "Contact: " (mapcar 'car names-alist) nil t))
            (user (cdr (assoc name names-alist))))
       (cl-assert user)
       (list (telega-user-as-contact user)))))

    (telega-chatbuf-input-insert
     (list :@type "inputMessageContact"
           :contact contact)))

(defun telega-chatbuf--gen-input-file (filename &optional file-type
                                                preview-p upload-ahead-callback)
  "Generate InputFile using FILENAME.
FILENAME can also be an HTTP or HTTPS url.
If PREVIEW-P is non-nil, then generate preview image.
UPLOAD-AHEAD-CALLBACK is callback for file updates, when uploading
ahead in case `telega-chat-upload-attaches-ahead' is non-nil."
  (if (string-match-p "^https?://" filename)
      (list :@type "inputFileRemote" :id filename)

    ;; Local file
    (setq filename (telega-file-local-copy filename))
    (let ((preview (when (and preview-p (> (telega-chars-xheight 1) 1))
                     (telega-create-image
                      filename
                      (when (fboundp 'imagemagick-types) 'imagemagick)
                      nil
                      :scale 1.0 :ascent 'center
                      :height (telega-chars-xheight 1))))
          (upload-ahead-file
           (when telega-chat-upload-attaches-ahead
             (telega-file--upload filename file-type 16 upload-ahead-callback))))
      (list :@type (propertize "inputFileLocal"
                               'telega-preview preview
                               'telega-upload-ahead-file upload-ahead-file)
            :path filename))))

(defun telega-chatbuf-attach-file (filename &optional preview-p
                                            content-type-detect-p)
  "Attach FILENAME as document to the chatbuf input.
If CONTENT-TYPE-DETECT-P is specified, then FILENAME's content type is
automatically detected."
  (interactive (list (telega-read-file-name "Attach file: ")))
  (let ((ifile (telega-chatbuf--gen-input-file filename 'Document preview-p)))
    (telega-chatbuf-input-insert
     (list :@type "inputMessageDocument"
           :document ifile
           :disable_content_type_detection
           (if content-type-detect-p :false t)))))

(defun telega-chatbuf-attach-photo (filename &optional tl-ttl spoiler-p)
  "Attach FILENAME as photo to the chatbuf input."
  (interactive (list (telega-read-file-name
                      (concat (telega-i18n "lng_attach_photo") ": "))))
  (let ((ifile (telega-chatbuf--gen-input-file filename 'Photo t))
        (img-size (ignore-errors
                    ;; NOTE: tty-only might trigger error here
                    (image-size
                     (telega-create-image
                      filename (when (fboundp 'imagemagick-types) 'imagemagick)
                      nil :scale 1.0)
                     t (telega-x-frame)))))
    (telega-chatbuf-input-insert
     (nconc (list :@type "inputMessagePhoto"
                  :photo ifile)
            (when img-size
              (list :width (car img-size)
                    :height (cdr img-size)))
            (when tl-ttl
              (list :self_destruct_type tl-ttl))
            (when spoiler-p
              (list :has_spoiler t))))))

(defun telega-chatbuf-attach-spoiler-photo (filename)
  "Attach photo marked with spoiler."
  (interactive (list (telega-read-file-name
                      (concat (telega-i18n "lng_attach_photo") ": "))))
  (telega-chatbuf-attach-photo filename nil 'with-spoiler))

(defun telega-chatbuf-attach-ttl-photo (filename tl-ttl)
  "Attach a file as self destructing photo.
This attachment can be used only in private chats."
  (interactive (list (telega-read-file-name
                      (concat (telega-symbol 'flames)
                              (telega-i18n "lng_attach_photo") ": "))
                     (telega-read-self-destruct-timer "Self desctruct in")))
  (telega-chatbuf-attach-photo filename tl-ttl))

(defun telega-chatbuf-attach-video (filename &optional tl-ttl spoiler-p)
  "Attach FILENAME as video to the chatbuf input."
  (interactive (list (telega-read-file-name "Video: ")))
  ;; NOTE: `telega-chatbuf--gen-input-file' might return another path
  ;; (accessible by docker) while FILENAME might not be accessible by
  ;; docker.  Docker might be used in `telega-ffplay-get-resolution'
  ;; and `telega-ffplay-get-duration'
  (let* ((ifile (telega-chatbuf--gen-input-file filename 'Video))
         (i-filename (plist-get ifile :path))
         (resolution (telega-ffplay-get-resolution i-filename)))
    (telega-chatbuf-input-insert
     (nconc (list :@type "inputMessageVideo"
                  :video ifile
                  :duration (round (telega-ffplay-get-duration i-filename))
                  :supports_streaming t)
            (when resolution
              (list :width (car resolution) :height (cdr resolution)))
            (when tl-ttl
              (list :self_destruct_type tl-ttl))
            (when spoiler-p
              (list :has_spoiler t))))))

(defun telega-chatbuf-attach-spoiler-video (filename)
  "Attach video marked with spoiler."
  (interactive (list (telega-read-file-name "Video: ")))
  (telega-chatbuf-attach-video filename nil 'with-spoiler))

(defun telega-chatbuf-attach-ttl-video (filename tl-ttl)
  "Attach a file as self destructing video.
This attachment can be used only in private chats."
  (interactive (list (telega-read-file-name
                      (concat (telega-symbol 'flames)
                              "Video: "))
                     (telega-read-self-destruct-timer "Self desctruct in")))
  (telega-chatbuf-attach-video filename tl-ttl))

(defun telega-chatbuf-attach-audio (filename)
  "Attach FILENAME as audio to the chatbuf input."
  (interactive (list (telega-read-file-name "Audio: ")))
  ;; NOTE: Comments about `i-filename' see in
  ;; `telega-chatbuf-attach-video'
  (let* ((ifile (telega-chatbuf--gen-input-file filename 'Audio))
         (i-filename (plist-get ifile :path))
         (metadata (telega-ffplay-get-metadata i-filename)))
    (telega-chatbuf-input-insert
     (list :@type "inputMessageAudio"
           :audio ifile
           :title (cdr (assoc "title" metadata))
           :performer (cdr (assoc "artist" metadata))
           :duration (round (telega-ffplay-get-duration i-filename))
           ))))

(defun telega-chatbuf-attach-media (filename &optional as-file-p)
  "Attach FILENAME as media, detecting media type by FILENAME extension.
If `\\[universal-argument] is given, then attach as file.
If AS-FILE-P is `preview', then attach as file with preview.  FILENAME
must be a photo in this case."
  (interactive (list (telega-read-file-name "Attach Media File: ")
                     current-prefix-arg))
  (let ((file-mime (or (unless as-file-p
                         (mailcap-extension-to-mime
                          (or (file-name-extension filename) "")))
                       "telega/unknown")))
    (cond ((string= "image/gif" file-mime)
           (telega-chatbuf-attach-animation filename))
          ((string-prefix-p "image/" file-mime)
           (telega-chatbuf-attach-photo filename))
          ((string-prefix-p "audio/" file-mime)
           (telega-chatbuf-attach-audio filename))
          ((string-prefix-p "video/" file-mime)
           (telega-chatbuf-attach-video filename))
          ((string-match-p "^https?://" filename)
           (telega-chatbuf-input-insert filename))
          (t
           (telega-chatbuf-attach-file filename (eq as-file-p 'preview))))))

(defun telega-chatbuf-attach-video-note (as-file-p)
  "Attach a (circled) video note to the chatbuf input.
If `\\[universal-argument] is given, then attach existing file as
video-note.  Otherwise record video note inplace.
`telega-vvnote-video-record-args' is used as arguments to ffmpeg to
record video notes."
  (interactive "P")
  ;; TODO: start video note generation process
  ;; see https://github.com/tdlib/td/issues/126

  ;; NOTE: Comments about `i-filename' see in the
  ;; `telega-chatbuf-attach-video'
  (let* ((filename (with-telega-chatbuf-action "RecordingVideoNote"
                     (if as-file-p
                         (telega-read-file-name "Video Note: ")
                       (telega-vvnote-video--record))))
         (ifile (telega-chatbuf--gen-input-file filename 'VideoNote))
         (i-filename (plist-get ifile :path))
         (frame1 (plist-get telega-vvnote-video--preview :first-frame)))
    (telega-chatbuf-input-insert
     (nconc
      (list :@type "inputMessageVideoNote"
            :duration (round (telega-ffplay-get-duration i-filename))
            :length 240
            :video_note ifile)
      (when frame1
        `(:thumbnail
          (:@type "inputThumbnail"
                  :thumbnail (:@type "inputFileLocal" :path ,frame1)
                  :width 240
                  :height 240)))))))

(defun telega-chatbuf-attach-voice-note (as-file-p)
  "Attach a voice note to the chatbuf input.
If `\\[universal-argument] is given, then attach existing file as
voice-note.  Otherwise record voice note inplace.
`telega-vvnote-voice-cmd' is used to record voice notes."
  (interactive "P")
  ;; TODO: start voice note generation process
  ;; see https://github.com/tdlib/td/issues/126

  ;; NOTE: Comments about `i-filename' see in the
  ;; `telega-chatbuf-attach-video'
  (let* ((filename (with-telega-chatbuf-action "RecordingVoiceNote"
                     (if as-file-p
                         (telega-read-file-name "Voice Note: ")
                       (telega-vvnote-voice--record))))
         (ifile (telega-chatbuf--gen-input-file filename 'VoiceNote))
         (i-filename (plist-get ifile :path)))
    (telega-chatbuf-input-insert
     (list :@type "inputMessageVoiceNote"
           :waveform (telega-vvnote--waveform-for-file i-filename)
           :duration (round (telega-ffplay-get-duration i-filename))
           :voice_note ifile))))

(defun telega-chatbuf-attach-clipboard (doc-p)
  "Attach clipboard image to the chatbuf as photo.
If `\\[universal-argument]' is given, then attach clipboard as document."
  (interactive "P")
  (let* ((selection-coding-system 'no-conversion) ;for rawdata
         (temporary-file-directory telega-temp-dir)
         (tmpfile (telega-temp-name "clipboard" ".png"))
         (coding-system-for-write 'binary))
    (if (eq system-type 'darwin)
        (progn
          ;; NOTE: On MacOS, try extracting clipboard using pngpaste
          (unless (executable-find "pngpaste")
            (error "Please install pngpaste to paste images"))
          (unless (= 0 (telega-screenshot-with-pngpaste tmpfile))
            (error "No image in CLIPBOARD")))
      (write-region (or (gui-get-selection 'CLIPBOARD 'image/png)
                        (error "No image in CLIPBOARD"))
                    nil tmpfile nil 'quiet))
    (telega-chatbuf-attach-media tmpfile (when doc-p 'preview))))

(defun telega-chatbuf-attach-screenshot (&optional n chat)
  "Attach screenshot to the chatbuf input.
If numeric prefix arg N is given, then take screenshot in N seconds.
If `\\[universal-argument]' is given, then take screenshot of the screen area.
Multiple `\\[universal-argument]' increases delay before taking
screenshot of the area.
Uses `telega-screenshot-function' to take a screenshot."
  (interactive (list (or current-prefix-arg 1) telega-chatbuf--chat))

  ;; NOTE: use float N value as special, to make screenshot of the
  ;; area, `log' returns float
  (when (listp n)
    (setq n (log (car n) 4)))

  (if (and (> n 0)
           ;; NO delays for "pngpaste"
           (not (eq telega-screenshot-function
                    'telega-screenshot-with-pngpaste)))
      (progn
        (message "Telega: taking screenshot in %d seconds" n)
        (run-with-timer 1 nil 'telega-chatbuf-attach-screenshot (1- n) chat))

    ;; Make a screenshot
    (message nil)
    (let* ((temporary-file-directory telega-temp-dir)
           (tmpfile (telega-temp-name "screenshot" ".png")))
      (funcall telega-screenshot-function tmpfile (floatp n))
      (when (file-exists-p tmpfile)
        ;; NOTE: Screenshot successfully taken
        (telega-chat--pop-to-buffer chat)
        (x-focus-frame (window-frame (get-buffer-window)))
        (telega-chatbuf-attach-media tmpfile)))))

(defun telega-chatbuf-sticker-insert (sticker)
  "Attach STICKER to the input."
  (let ((thumb (plist-get sticker :thumbnail))
        (preview (telega-sticker--image sticker)))
    ;; Scale down preview to single char
    (plist-put (cdr preview) :scale (/ 1.0 (car telega-sticker-size)))

    (telega-chatbuf-input-insert
     (list :@type "inputMessageSticker"
           :width (plist-get sticker :width)
           :height (plist-get sticker :height)
           ;; Use remote thumbnail and sticker files
           :thumbnail (list :@type "inputThumbnail"
                            :width (plist-get thumb :width)
                            :height (plist-get thumb :height)
                            :thumbnail (list :@type "inputFileId"
                                             :id (telega--tl-get thumb :photo :id)))
           ;; NOTE: 'telega-preview used in `telega-ins--input-file'
           ;; to insert document/photo/sticker preview
           :sticker (list :@type (propertize "inputFileId" 'telega-preview preview)
                          :id (telega--tl-get sticker :sticker :id))
           ))
    ))

(defun telega-chatbuf-custom-emoji-insert (sticker &optional emoji)
  "Insert custom emoji STICKER into chatbuf.
EMOJI - emoji string to use instead of emoji associated with the STICKER."
  (cl-assert (telega-custom-emoji-sticker-p sticker))
  (telega-chatbuf-input-insert
   (propertize
    (or emoji (telega-tl-str sticker :emoji))
    :tl-entity-type (list :@type "textEntityTypeCustomEmoji"
                          :custom_emoji_id (telega-custom-emoji-id sticker))
    'display (when telega-use-images
               (telega-sticker--image sticker))
    'rear-nonsticky t)))

(defun telega-chatbuf-animation-insert (animation)
  "Attach ANIMATION to the input."
  (let ((thumb (plist-get animation :thumbnail))
        (preview (telega-animation--create-image animation)))
    ;; Scale down preview to single char
    (plist-put (cdr preview) :scale (/ 1.0 telega-animation-height))

    (telega-chatbuf-input-insert
     (list :@type "inputMessageAnimation"
           :width (plist-get animation :width)
           :height (plist-get animation :height)
           :duration (plist-get animation :duration)
           ;; Use remote thumbnail and animation files
           :thumbnail (list :@type "inputThumbnail"
                            :width (plist-get thumb :width)
                            :height (plist-get thumb :height)
                            :thumbnail (list :@type "inputFileId"
                                             :id (telega--tl-get thumb :photo :id)))
           ;; NOTE: 'telega-preview used in `telega-ins--input-file'
           ;; to insert document/photo/sticker/animation preview
           :animation (list :@type (propertize "inputFileId"
                                               'telega-preview preview)
                            :id (telega--tl-get animation :animation :id))
           ))
    ))

(defun telega-chatbuf-attach-sticker-by-emoji ()
  "If chatbuf has single emoji input, then popup stickers win.
Intended to be added to `post-command-hook' in chat buffer.
Or to be called directly.
Return non-nil if input has single emoji."
  (interactive)

  (telega-emoji-init)
  (let ((emoji (buffer-substring (save-excursion (telega-emoji-backward) (point))
                                 (point))))
    (when (telega-emoji-p emoji)
      ;; NOTE: Do nothing in case sticker's help win is exists and
      ;; have same emoji
      (let ((buf (get-buffer "*Telegram Stickers*")))
        (when (or (called-interactively-p 'interactive)
                  (not (buffer-live-p buf))
                  (not (with-current-buffer buf
                         (string= emoji telega-help-win--emoji))))
          (telega-sticker-choose-emoji emoji telega-chatbuf--chat
                                       (not (string= (telega-chatbuf-input-string)
                                                     emoji)))))
      t)))

(defun telega-chatbuf-attach-custom-emoji ()
  "Interactively attach a custom emoji."
  (interactive)
  (telega-custom-emoji-choose #'telega-chatbuf-custom-emoji-insert))

(defun telega-chatbuf-attach-sticker (fav-or-recent-p)
  "Attach a sticker.
If `\\[universal-argument]' is given, then attach recent or
favorite sticker.  Otherwise choose a sticker from installed
sticker sets."
  (interactive "P")
  (if fav-or-recent-p
      (telega-sticker-choose-favorite-or-recent telega-chatbuf--chat)

    ;; NOTE: We display buffer with stickers before it is actually
    ;; used, to allow user select a way how help window displays.  See
    ;; https://t.me/emacs_telega/41685 for details

    (let* ((tss-window (save-selected-window
                         (display-buffer
                          (get-buffer-create "*Telegram Sticker Set*"))))
           (tss-buffer (window-buffer tss-window))
           (sset (telega-stickerset-completing-read "Sticker set: "))
           (chat telega-chatbuf--chat))
      (when (or (not (buffer-live-p tss-buffer))
                (not (with-current-buffer tss-buffer
                       (and (eq telega-help-win--stickerset sset)
                            (eq telega--chat chat)))))
        (telega-describe-stickerset sset telega-chatbuf--chat))

      (select-window
       (temp-buffer-window-show tss-buffer)))))

(defun telega-chatbuf-attach-animation (&optional animation-file)
  "Attach an animation.
If `\\[universal-argument]' is given, then attach animation from
a file, otherwise choose animation from list of saved animations."
  (interactive (when current-prefix-arg
                 (list (telega-read-file-name "Animation File: "))))
  ;; NOTE: Comments about `i-filename' see in the
  ;; `telega-chatbuf-attach-video'
  (if animation-file
      (let* ((ifile (telega-chatbuf--gen-input-file animation-file 'Animation))
             (i-filename (plist-get ifile :path))
             (resolution (telega-ffplay-get-resolution i-filename)))
        (telega-chatbuf-input-insert
         (nconc (list :@type "inputMessageAnimation"
                      :animation ifile
                      :duration
                      (round (telega-ffplay-get-duration i-filename)))
                (when resolution
                  (list :width (car resolution) :height (cdr resolution))))))

    (telega-animation-choose-saved telega-chatbuf--chat)))

(defun telega-chatbuf-attach-gif (gif-file)
  "Attach GIF-FILE as animation to the chatbuf input."
  (interactive (list (telega-read-file-name "GIF File: ")))
  (telega-chatbuf-attach-animation gif-file))

(defun telega-chatbuf-attach-inline-bot-query (&optional no-empty-search)
  "Popup results with inline bot query.
Intended to be added to `post-command-hook' in chat buffer.
Or to be called directly.
Return non-nil if input has inline bot query.
If NO-EMPTY-SEARCH is non-nil, then do not perform empty query search."
  (interactive)
  (let ((input (telega-chatbuf-input-string)))
    (when (string-match "^@\\([^ ]+\\)[ \t]+\\(.*\\)" input)
      (let* ((username (match-string 1 input))
             (query (match-string 2 input))
             (uchat (telega--searchPublicChat username))
             (bot-user (and uchat
                            (telega-chat-bot-p uchat)
                            (telega-chat-user uchat)))
             (bot (plist-get bot-user :type))
             (inline-help (telega-tl-str bot :inline_query_placeholder)))
        (when (plist-get bot :is_inline)
          ;; Start querying the bot
          (unless (and (string-empty-p query) no-empty-search)
            (telega-inline-bot-query bot-user query telega-chatbuf--chat))

          ;; Display the inline help
          (when (string-empty-p query)
            (telega-momentary-display
             (propertize inline-help 'face 'telega-shadow)))
          t)))))

(defun telega-chatbuf-attach-poll (question anonymous-p allow-multiple-answers-p
                                            &rest options)
  "Attach poll to the chatbuf input.
Can be used only in group chats.
QUESTION - Title of the poll.
ANONYMOUS-P - Non-nil to create anonymous poll.
ALLOW-MULTIPLE-ANSWERS-P - Non-nil to allow multiple answers.
OPTIONS - List of strings representing poll options."
  (interactive
   (let ((poll-q (read-string
                  (concat (telega-i18n "lng_polls_public")
                          " "
                          (telega-i18n "lng_polls_create_question")
                          ": ")))
         (optidx 1) opt poll-opts)
     (while (not (string-empty-p
                  (setq opt (read-string
                             (format "Option %d): " optidx)))))
       (setq poll-opts (append poll-opts (list opt)))
       (cl-incf optidx))
     (nconc (list poll-q
                  (y-or-n-p
                   (concat (telega-i18n "lng_polls_create_anonymous") "? "))
                  (y-or-n-p
                   (concat (telega-i18n "lng_polls_create_multiple_choice") "? ")))
            poll-opts)))

  (telega-chatbuf-input-insert
   (list :@type "inputMessagePoll"
         :question question
         :is_anonymous (if anonymous-p t :false)
         :type (list :@type "pollTypeRegular"
                     :allow_multiple_answers
                     (if allow-multiple-answers-p t :false))
         :options (apply 'vector options))))

(defun telega-chatbuf-attach-scheduled (timestamp)
  "Mark content as scheduled.
Send following message at TIMESTAMP.
If `\\[universal-argument]' is given and chat is private and
online status of the corresponding user is known, then send
message when user gets online."
  (interactive (list (unless current-prefix-arg
                       (telega-read-timestamp "Send time: "))))

  (telega-chatbuf-input-insert
   (list :@type "telegaScheduledMessage"
         :timestamp timestamp))
  (when (eobp)
    (telega-momentary-display
     (propertize (telega-i18n "telega_scheduled_help") 'face 'telega-shadow))))

(defun telega-chatbuf-attach-toggle-disable-notification (disable-p)
  "Toggle disable-notification chat option for the subsequent chatbuf input.
Use this attachment to disable/enable notification on the receiver side."
  (interactive
   (list (not (plist-get telega-chatbuf--chat :default_disable_notification))))

  (telega-chatbuf-input-insert
   (list :@type "telegaDisableNotification"
         :disable_notification disable-p))
  (when (eobp)
    (telega-momentary-display
     (propertize (telega-i18n (if disable-p
                                  "telega_disable_notification_help"
                                "telega_enable_notification_help"))
                 'face 'telega-shadow))))

(defun telega-chatbuf-attach-disable-webpage-preview ()
  "Disable webpage preview for the following text message."
  (interactive)
  (telega-chatbuf-input-insert
   (list :@type "telegaDisableWebpagePreview"))
  (when (eobp)
    (telega-momentary-display
     (propertize (telega-i18n "telega_disable_webpage_preview_help")
                 'face 'telega-shadow))))

(defun telega-chatbuf-attach-send-by (msg-sender)
  "Set sender for the following message."
  (interactive
   (let ((avail-senders
          (telega--getChatAvailableMessageSenders telega-chatbuf--chat)))
     (unless avail-senders
       (user-error "telega: No message senders available"))
     (list (telega-completing-read-msg-sender
            "Send message by: " avail-senders))))

  (telega-chatbuf-input-insert
   (list :@type "telegaSendBy" :sender_id msg-sender))
  (when (eobp)
    (telega-momentary-display (propertize "Message" 'face 'telega-shadow))))

(defun telega-chatbuf-attach-dice (emoji)
  "Attach random dice roll message."
  (interactive (list (funcall telega-completing-read-function
                              "Dice Emoji: "
                              telega--dice-emojis nil t)))
  (telega-chatbuf-input-insert
   (list :@type "inputMessageDice"
         :emoji emoji
         :clear_draft t)))

(defun telega-chatbuf-attach-markup (markup-name &optional markup-text)
  "Attach MARKUP-TEXT using MARKUP-NAME into chatbuf.
Using this type of attachment it is possible to intermix multiple
markups in the chatbuf input.
Markups are defined in the `telega-chat-markup-functions' user option."
  (interactive (list (funcall telega-completing-read-function
                              "Markup: "
                              (mapcar #'car telega-chat-markup-functions)
                              nil t)))
  (let ((markup-func (cdr (assoc markup-name telega-chat-markup-functions))))
    (telega-chatbuf-input-insert
     (telega-string-as-markup (or markup-text "") markup-name markup-func))
    (backward-char 1)))

(defun telega-chatbuf-attach (attach-type)
  "Attach something to the chatbuf input.
`\\[universal-argument]' is passed directly to the attachment function.
See `telega-chat-attach-commands' for available attachment types."
  (interactive
   (list (funcall telega-completing-read-function
                  "Attachment type: "
                  (mapcar #'car (seq-filter
                                 (lambda (cmd-spec)
                                   (telega-chatbuf-match-p (nth 1 cmd-spec)))
                                 telega-chat-attach-commands))
                  nil t)))

  (let ((cmd (nth 2 (assoc attach-type telega-chat-attach-commands))))
    (cl-assert (commandp cmd))
    (call-interactively cmd)))

(defun telega-chatbuf-invite-user-via-link (user)
  "Add USER to chatbuf via sending invite link in private chat."
  (interactive (list (telega-completing-read-user
                         (concat (telega-i18n "lng_via_link_send") ": "))))

  (let* ((chatbuf-info (telega-chat--info telega-chatbuf--chat))
         (telega-full-info-offline-p nil)
         (chatbuf-full-info (telega--full-info chatbuf-info))
         (chatbuf-link (plist-get chatbuf-full-info :invite_link))
         (invite-link (plist-get chatbuf-link :invite_link))
         (user-chat (telega--createPrivateChat user)))
    (telega--sendMessage
     user-chat (list :@type "inputMessageText"
                     :text (telega-string-fmt-text invite-link))
     nil nil
     :callback (lambda (&rest _ignored)
                 (message (telega-ins--as-string
                           (telega-ins "telega: sent ")
                           (telega-ins invite-link)
                           (telega-ins (telega-symbol 'right-arrow))
                           (telega-ins--msg-sender user
                             :with-avatar-p t
                             :with-username-p 'telega-username)))))

    (plist-put telega-chatbuf--chat :telega-add-member-forbidden-users
               (delq user (plist-get telega-chatbuf--chat
                                     :telega-add-member-forbidden-users)))
    (telega-chatbuf--footer-update)))

(declare-function eaf-get-path-or-url "eaf")

(defun telega-buffer-file-send (file chat &optional as-file-p)
  "Prepare FILE to be sent as document or photo to CHAT.
If `\\[universal-argument]' is specified, then always send as a file.
Otherwise FILE type is automatically detected.
If called interactively, then file associated with current buffer
is used as FILE.
If current buffer is dired, then send all marked files."
  (interactive
   (let ((file
          (or (buffer-file-name)
              (when (derived-mode-p 'dired-mode)
                (seq-filter #'file-regular-p (dired-get-marked-files)))
              ;; Support for EAF
              ;; see https://github.com/manateelazycat/emacs-application-framework/issues/675
              (when (derived-mode-p 'eaf-mode)
                (eaf-get-path-or-url))
              (user-error (concat "Can't send current buffer, "
                                  "it does not have corresponding file"))))
         (as-file-p current-prefix-arg))
     (list file
           (telega-completing-read-chat
            (format "Send %s(%s) to chat: "
                    (cond ((listp file)
                           (format "%d FILES" (length file)))
                          (t "FILE"))
                    (if as-file-p
                        "as file"
                      "autodetect")))
           as-file-p)))

  (cl-assert chat)
  (with-current-buffer (telega-chat--pop-to-buffer chat)
    (let ((inhibit-read-only t)
          (buffer-undo-list t)
          (files (if (listp file) file (list file))))
      ;; NOTE: `telega-chatbuf-attach-XX' might do sync calls to
      ;; TDLib, so we protect it from intermetiate TDLib events
      ;; handling with `with-telega-deferred-events'
      (with-telega-deferred-events
        (dolist (file files)
          (goto-char (point-max))
          (telega-chatbuf-attach-media file as-file-p)))
      )))

(defun telega-chatbuf--switch-out (&optional focus-out-p)
  "Called when switching from chat buffer.
FOCUS-OUT-P is non-nil if called when chatbuf's frame looses focus."
  (telega-debug "Switch %s%s: %s" (propertize "OUT" 'face 'bold)
                (if focus-out-p " (focus out)" "") (buffer-name))

  (when (telega-chatbuf-has-input-p)
    (when telega-chatbuf--my-action
      (telega-chatbuf--set-action "Cancel"))

    ;; NOTE: Set draft only if there is no attaches in the input,
    ;; otherwise setting/updating draft will screw up text properties
    (unless (telega-chatbuf-input-has-attaches-p)
      (telega--setChatDraftMessage
       telega-chatbuf--chat
       (list :@type "draftMessage"
             :reply_to_message_id
             (or (plist-get (telega-chatbuf-replying-msg) :id) 0)
             :input_message_text
             (list :@type "inputMessageText"
                   :text (telega-string-fmt-text
                          (telega-chatbuf-input-string)))))))

  ;; If point is in the prompt, than use `:prompt-draft' or
  ;; `:prompt-empty' as value, otherwise use current point as value
  (telega-chatbuf--history-state-set
   :newer-freezed
   (if (and (>= (point) telega-chatbuf--input-marker)
            (not telega-chat-preview-mode))
       (if (telega-chatbuf-has-input-p)
           :prompt-draft
         :prompt-empty)
     (point)))
  )

(defun telega-chatbuf--switch-in ()
  "Called when switching to chat buffer."
  (telega-debug "Switch %s: %s" (propertize "IN" 'face 'bold)
                (buffer-name))

  (when-let ((goto-msg (telega-chatbuf--history-state-get :goto-msg)))
    (telega-chatbuf--history-state-delete :goto-msg)
    (telega-chatbuf--history-state-delete :newer-freezed)
    (telega-chat--goto-msg telega-chatbuf--chat (plist-get goto-msg :id)))

  ;; NOTE: Recover freezed state, i.e.
  ;; 1) Keep point as is if it was not in the prompt at the time of
  ;;    newer history freeze
  ;; 2) Jump to last unread message if point was in the chatbuf prompt
  ;;    and newer
  (when-let ((newer-freezed (telega-chatbuf--history-state-get :newer-freezed)))
    (telega-chatbuf--history-state-delete :newer-freezed)

    (cond ((or (eq newer-freezed :prompt-empty)
               (eq newer-freezed :prompt-draft))
           ;; NOTE: chatbuf might already have an input, for example
           ;; after taking a screenshot or leaving a draft
           (unless (telega-chatbuf-has-input-p)
             (telega-chatbuf-next-unread
               (lambda (button)
                 (telega-button--make-observable button 'force)))))

          (t
           ;; Redetect cursor sensor to hover in any message at point
           (when-let ((chat-win (get-buffer-window)))
             (set-window-parameter chat-win 'cursor-sensor--last-state nil)
             (cursor-sensor--detect chat-win))
           )))

  ;; May affect rootbuf sorting if `chatbuf-recency' criteria is used
  (when (memq 'chatbuf-recency telega--sort-criteria)
    (telega-chat--update telega-chatbuf--chat 'reorder))

  ;; See docstring for `telega-root-keep-cursor'
  (when (eq telega-root-keep-cursor 'track)
    (telega-root--keep-cursor-at-chat telega-chatbuf--chat))
  )

(defun telega-chatbuf--killed ()
  "Called when chat buffer is killed."
  ;; Close chat and cancel any active action and actualizes the draft
  ;; see https://t.me/emacs_telega/6708
  ;; "telega-server" might not be running at the moment, so ignore any
  ;; errors
  (ignore-errors
    (unless telega-chat-preview-mode
      (telega--closeChat telega-chatbuf--chat))
    (telega-chatbuf--switch-out))

  ;; Stop any voice/video notes, see
  ;; https://github.com/zevlg/telega.el/issues/49
  (when telega-chatbuf--vvnote-msg
    (telega-ffplay-stop))

  ;; NOTE: chatbuffer might be left from other telega start, so it
  ;; will throw "Ewoc node not found" error - ignore it
  (ignore-errors
    (telega-chat--update telega-chatbuf--chat))

  ;; See docstring for `telega-root-keep-cursor'
  (when (eq telega-root-keep-cursor 'track)
    (telega-root--keep-cursor-at-chat telega-chatbuf--chat))

  (setq telega--chat-buffers-alist
        (assq-delete-all telega-chatbuf--chat telega--chat-buffers-alist))

  (run-hooks 'telega-chatbuf-kill-hook))

;;;###autoload
(defun telega-chatbuf-input-as-region-advice (orig-region-func start end &rest args)
  "Advice for commands accepting region.
If point is inside telega chatbuf input, then call region command
with input prompt as region."
  (when (and (not (region-active-p))
             (derived-mode-p 'telega-chat-mode)
             (>= (point) telega-chatbuf--input-marker))
    (setq start telega-chatbuf--input-marker
          end (point-max)))
  (apply orig-region-func start end args))

(defun telega-chatbuf--activate-vvnote-msg (msg)
  "Activate voice/video note message MSG in the chatbuf.
MSG can be nil in case there is no active voice/video note message."
  (cl-assert telega-chatbuf--chat)
  (setq telega-chatbuf--vvnote-msg msg)
  (telega-chatbuf--footer-update))


;;; Message commands
(defun telega-msg-redisplay (msg &optional node)
  "Redisplay the message MSG.
NODE is already calculated ewoc NODE, or nil."
  (interactive (list (telega-msg-at (point))))

  (with-telega-chatbuf (telega-msg-chat msg)
    ;; Force view of the message if `L' is pressed
    (when (called-interactively-p 'interactive)
      (telega-chatbuf--msg-view msg '(return force-view)))

    ;; Redisplay footer in case active voice note is redisplayed
    (when (eq msg telega-chatbuf--vvnote-msg)
      (telega-chatbuf--footer-update))

    (when-let ((msg-node (or node (telega-chatbuf--node-by-msg-id
                                   (plist-get msg :id)))))
      (telega-chatbuf--redisplay-node msg-node))))

(defun telega-msg-reply (msg &optional other-chat-p quote-fmt-text)
  "Start replying to MSG.
If `\\[universal-argument]' is specified, then reply in other chat."
  (interactive (list (telega-msg-for-interactive)
                     current-prefix-arg
                     (when (region-active-p)
                       (prog1
                           (telega-string-fmt-text
                            (buffer-substring (region-beginning) (region-end)))
                         (deactivate-mark)))))

  (when (and other-chat-p
             (not (plist-get msg :can_be_replied_in_another_chat)))
    (user-error "telega: Can't reply to this message in another chat"))

  (let ((chat (if other-chat-p
                  (telega-completing-read-chat
                   (concat (telega-i18n "lng_reply_in_another_chat") ": "))
                (telega-msg-chat msg))))
    (telega-chat--pop-to-buffer chat)   ; Make sure chatbuf is shown
    (with-telega-chatbuf chat
      (setq telega-chatbuf--aux-plist (list :aux-type 'reply :aux-msg msg
                                            :aux-reply-quote quote-fmt-text))
      (telega-chatbuf--chat-update "aux-plist")

      (telega-chatbuf--prompt-update)
      (goto-char (point-max))

      (telega-help-message--cancel-aux 'reply))))

(defun telega-msg-reply-in-another-chat (msg quote-region)
  "Reply to a message MSG in another chat."
  (interactive (list (telega-msg-for-interactive)
                     (when (region-active-p)
                       (cons (region-beginning) (region-end)))))
  (telega-msg-reply msg 'in-another-chat quote-region))

(defconst telega-msg-edit--markup-specs
  '(("markdown1" . telega--fmt-text-markdown1)
    ("markdown2" . telega--fmt-text-markdown2)
    ("org" . telega--fmt-text-org)))

(defun telega-msg-edit (msg &optional markup-arg)
  "Start editing the MSG.
If called interactively, number of `\\[universal-argument]' before
command determines index in the `telega-chat-input-markups' of markup to
use for editing.  For example `C-u RET' will use
`(nth 1 telega-chat-input-markups)' markup for editing."
  (interactive (list (telega-msg-for-interactive) current-prefix-arg))

  (unless (plist-get msg :can_be_edited)
    (error "Message can't be edited"))

  (with-telega-chatbuf (telega-msg-chat msg)
    ;; Allow editing deleted messages as new one
    ;; See https://github.com/zevlg/telega.el/issues/194
    (if (telega-msg-match-p msg 'is-deleted)
        (telega-chatbuf-cancel-aux)

      (setq telega-chatbuf--aux-plist (list :aux-type 'edit :aux-msg msg))
      (telega-chatbuf--chat-update "aux-plist")
      (telega-chatbuf--prompt-update))

    ;; Replace any input text with edited message
    (delete-region telega-chatbuf--input-marker (point-max))
    (goto-char (point-max))

    ;; Insert message's text or attachment caption
    ;; Possibly use "markdown2" markup for the text
    (let* ((telega-inhibit-telega-display-by t)
           (content (plist-get msg :content))
           (orig-fmt-text (or (plist-get content :text)
                              (plist-get content :caption)))
           (markup-name (if (and markup-arg (listp markup-arg))
                            (nth (round (log (car markup-arg) 4))
                                 telega-chat-input-markups)
                          (car telega-chat-input-markups)))
           (markup-fmt-text-func
            (cdr (assoc markup-name telega-msg-edit--markup-specs)))
           (markup-str (funcall (or markup-fmt-text-func #'telega--fmt-text-faces)
                                orig-fmt-text)))
      ;; NOTE: Scheduling state also can be edited
      (when-let ((scheduling-state (plist-get msg :scheduling_state)))
        (telega-chatbuf-input-insert
         (list :@type "telegaScheduledMessage"
               :timestamp (cl-ecase (telega--tl-type scheduling-state)
                            (messageSchedulingStateSendAtDate
                             (plist-get scheduling-state :send_date))
                            (messageSchedulingStateSendWhenOnline
                             nil)))))

      ;; NOTE: if text does not changes, then no markup in the text,
      ;; can edit text AS-IS
      (if (or (null markup-fmt-text-func)
              (string= markup-str (plist-get orig-fmt-text :text)))
          ;; Insert msg text AS-IS
          (telega-ins (telega--desurrogate-apply markup-str))

        ;; Insert msg text as markup input attachment
        (cl-assert (stringp markup-name))
        (telega-chatbuf-attach-markup
         markup-name (telega--desurrogate-apply markup-str))))

    (telega-help-message--cancel-aux 'edit)))

(defun telega-chatbuf-attach-fwd-msg (msg &optional send-copy-p rm-cap-p)
  "Attach MSG as foward message into chatbuf's input."
  (telega-chatbuf-input-insert
   (list :@type "telegaForwardMessage"
         :message msg
         :send_copy send-copy-p
         :remove_caption (and rm-cap-p
                              ;; NOTE: `:remove_caption' is used in
                              ;; messages splitting logic, so mark
                              ;; forwarded messages with
                              ;; `:remove_caption' only for messages
                              ;; having caption
                              (telega-msg-match-p msg
                                '(type Animation Audio Document
                                       Photo Video VoiceNote)))
         :unmark-after-sent (telega-msg-marked-p msg)))

  (when (and send-copy-p rm-cap-p (eobp))
    (telega-momentary-display
     (propertize (telega-i18n "telega_forward_new_caption_help")
                 'face 'telega-shadow))))

(defun telega-msg-forward-marked-or-at-point (&optional send-copy-p rm-cap-p)
  "Forward marked messages or message at point.
If `\\[universal-argument]' is given, then forward message copy.
If `\\[universal-argument]' `\\[universal-argument]' is given,
then forward message copy without caption."
  (interactive (list current-prefix-arg
                     (> (prefix-numeric-value current-prefix-arg) 4)))
  (when (telega-chat-secret-p telega-chatbuf--chat)
    (user-error "telega: Can't forward messages from secret chat"))
  (when-let ((messages (or (reverse telega-chatbuf--marked-messages)
                           (when-let ((msg-at-point (telega-msg-at (point))))
                             (list msg-at-point)))))
    ;; NOTE: ExcludeEvery
    (unless (seq-every-p (telega--tl-prop :can_be_forwarded) messages)
      (user-error "telega: Forwarding is not allowed"))

    (let ((chat (telega-completing-read-chat
                 (concat (telega-symbol 'forward) "Forward"
                         (when send-copy-p " Copy")
                         (when rm-cap-p " NewCap")
                         (when (> (length messages) 1)
                           (format " (%d marked)" (length messages)))
                         " to: ")
                 ;; NOTE: Forward only to known/comments chats we can
                 ;; write/post to.
                 (telega-filter-chats telega--ordered-chats
                   '(and (or is-known has-chatbuf) can-send-or-post)))))
      ;; NOTE: unmark all messages if forwarding marked messages
      (when telega-chatbuf--marked-messages
        (telega-chatbuf-msg-marks-toggle))

      (telega-chat--pop-to-buffer chat)
      (with-telega-chatbuf chat
        (goto-char (point-max))
        ;; NOTE: Forward messages in the id order, see
        ;; https://github.com/zevlg/telega.el/issues/271
        (dolist (msg (cl-sort messages #'< :key (telega--tl-prop :id)))
          (telega-chatbuf-attach-fwd-msg msg send-copy-p rm-cap-p))))))

(defun telega-msg-forward-marked-or-at-point-to-multiple-chats (chats)
  "Forward marked messages or message at point to multiple CHATS."
  (interactive (list (telega-completing-read-chat-list "Forward to Chats")))
  (when-let* ((messages
               (cl-sort (or telega-chatbuf--marked-messages
                            (when-let ((msg-at-point (telega-msg-at (point))))
                              (list msg-at-point)))
                        #'< :key (telega--tl-prop :id)))
              (from-chat (telega-msg-chat (car messages))))
    (when (y-or-n-p (format "Forward %d messages to %d chats? "
                            (length messages) (length chats)))
      ;; NOTE: unmark all messages if forwarding marked messages
      (when telega-chatbuf--marked-messages
        (telega-chatbuf-msg-marks-toggle))

      (dolist (chat chats)
        (telega--forwardMessages chat from-chat messages)))))

(defun telega-msg-delete0 (msg &optional revoke)
  (cl-assert (eq (telega--tl-type msg) 'message))
  (if (telega-msg-match-p msg 'is-deleted)
      ;; NOTE: `d' is pressed on deleted message
      ;; (`telega-chat-show-deleted-messages-for' is non-nil)
      ;; Generate pseudo-event to delete the message
      (let ((telega-chat-show-deleted-messages-for nil))
        (telega--on-updateDeleteMessages
         (list :chat_id (plist-get msg :chat_id)
               :is_permanent t
               :message_ids (vector (plist-get msg :id)))))

    (telega--deleteMessages (list msg) revoke)))

(defun telega-chatbuf-marked-messages-delete (revoke)
  "Delete marked messages in chatbuf.
If `\\[universal-argument]' is specified, then kill
messages (delete for me only), otherwise revoke message (delete
for everyone).
If chatbuf is supergroups, channels or secret chat, then always revoke."
  (interactive (list (or (telega-chatbuf-match-p
                          '(type supergroup channel secret))
                         (not current-prefix-arg))))

  (when-let ((marked-messages telega-chatbuf--marked-messages))
    (when (yes-or-no-p (telega-i18n (if revoke
                                        "telega_query_revoke_marked_messages"
                                      "telega_query_kill_marked_messages")
                         :count (length marked-messages)))
      (setq telega-chatbuf--marked-messages nil)
      (dolist (msg marked-messages)
        (telega-msg-delete0 msg revoke))

      (telega-chatbuf--chat-update "marked-messages"))))

(defun telega-msg-delete-marked-or-at-point (revoke)
  "Deletes some messages.
If some messages are marked, then delete them.  Otherwise delete
message at point.
If `\\[universal-argument]' REVOKE is specified, then kill
messages (delete for me only), otherwise revoke message (delete
for everyone).
REVOKE forced to non-nil for supergroup, channel or a secret chat."
  (interactive (list (or (telega-chatbuf-match-p
                          '(type supergroup channel secret))
                         (not current-prefix-arg))))

  (if (and (not (telega-msg-at-down-mouse-3))
           telega-chatbuf--marked-messages)
      (telega-chatbuf-marked-messages-delete revoke)

    (when-let ((msg (telega-msg-for-interactive)))
      (if (telega-msg-match-p msg 'is-deleted)
          ;; Purge already deleted message
          (telega-msg-delete0 msg)

        (when (y-or-n-p (telega-i18n (if revoke
                                         "telega_query_revoke_message"
                                       "telega_query_kill_message")))
          (telega-msg-delete0 msg revoke)

          (if (telega-chatbuf-match-p telega-chat-show-deleted-messages-for)
              (telega-help-message 'double-delete
                  "Press %s once again to hide deleted message"
                (substitute-command-keys (format "\\[%S]" this-command)))
            (telega-help-message 'show-deleted
                "JFYI see `telega-chat-show-deleted-messages-for'")))))))

(defun telega-chatbuf-complete ()
  "Complete thing at chatbuf input."
  (interactive)
  (or (when (functionp telega-chat-input-complete-function)
        (funcall telega-chat-input-complete-function))
      ;; 1) Try all company backends for completions
      (if (and (boundp 'company-mode) company-mode)
          ;; Use company-mode for completion
          (when-let ((backend (telega-company--grab-backend 'backend)))
            (company-begin-backend backend)
            (company-complete)
            t)
        ;; Use capf for completion, `completion-at-point' returns
        ;; non-nil if completion at point is performed
        (let ((completion-at-point-functions
               (cons 'telega-chatbuf-complete-at-point
                     completion-at-point-functions)))
          (completion-at-point)))
      ;; 2) Try to complete bot's inline query
      (call-interactively 'telega-chatbuf-attach-inline-bot-query)
      ;; 3) Try completing emoji to sticker
      (call-interactively 'telega-chatbuf-attach-sticker-by-emoji)

      ;; TODO: add other completions
      ))

(defun telega-chatbuf-completion-candidates (prefix)
  "Return list of completion candidates for current user input."
  ;; NOTE: for empty PREFIX string we can't decide which company
  ;; backend to use to fetch candidates
  (unless (string-empty-p prefix)
    (when-let* ((chat telega-chatbuf--chat)
                (company-backend (with-temp-buffer
                                   (setq telega-chatbuf--chat chat)
                                   ;; NOTE: `telega-company-grab-botcmd' uses
                                   ;; `telega-chatbuf--input-marker'
                                   (setq telega-chatbuf--input-marker (point))
                                   (insert prefix)
                                   (telega-company--grab-backend 'backend))))
      (company-call-backend 'candidates prefix))))

(defun telega-chatbuf-complete-at-point ()
  "Function suitable for use by `completion-at-point-functions'.
Works only if `company' feature is provided."
  (interactive)
  (when-let ((has-company-p (featurep 'company))
             (prefix (telega-company--grab-backend 'prefix)))
    (list (- (point) (length (car prefix))) (point)
          (completion-table-with-cache
           #'telega-chatbuf-completion-candidates))))

(defun telega-chatbuf-next-link (n)
  (interactive "p")
  ;; TODO: maybe be more smarter about links
  (telega-button-forward n
    (lambda (button)
      (and
       ;; Skip internal telega messages, such at (Discussion Started)
       (not (when-let ((msg (telega-msg-at button)))
              (telega-msg-internal-p msg)))
       (not (eq (button-type button) 'telega-prompt))))))

(defun telega-chatbuf-prev-link (n)
  (interactive "p")
  (when (<= telega-chatbuf--input-marker (point))
    (goto-char (ewoc-location (ewoc--footer telega-chatbuf--ewoc))))
  (telega-chatbuf-next-link (- n)))

(defun telega-chatbuf-complete-or-next-link ()
  "Complete username at point, or jump to next link."
  (interactive)
  (if (<= telega-chatbuf--input-marker (point))
      (call-interactively #'telega-chatbuf-complete)
    (call-interactively #'telega-chatbuf-next-link)))

(defun telega-chat-generate-invite-link (chat)
  "Generate invite link for CHAT.
If called interactively then copy generated link into the kill ring."
  (interactive (list (or telega-chatbuf--chat (telega-chat-at (point)))))

  (let ((link (telega--replacePrimaryChatInviteLink chat)))
    (when (called-interactively-p 'interactive)
      (kill-new link)
      (message "Invite link: %s (copied into kill ring)" link))
    link))

(defun telega-chatbuf--goto-loaded-msg (msg-id &optional highlight callback)
  "In chatbuf goto message denoted by MSG-ID.
Goto message only if it is already displayed in the chatbuf.
If HIGHLIGHT is non-nil, then momentary highlight the message.
Return non-nil on success."
  (declare (indent 2))
  (when-let ((node (telega-chatbuf--node-by-msg-id msg-id)))
    (ewoc-goto-node telega-chatbuf--ewoc node)

    ;; NOTE: node could exists, where button is not!  This is true for
    ;; ignored messages if `telega-ignored-messages-visible' is nil
    (when-let ((msg-button (button-at (point))))
      (telega-button--make-observable msg-button)
      (when highlight
        (cl-assert (eq (button-type msg-button) 'telega-msg))
        (with-no-warnings
          (pulse-momentary-highlight-region
           (button-start msg-button) (button-end msg-button))))

      (when callback
        (funcall callback msg-button)))
    t))

(defun telega-chatbuf--goto-msg (msg-id &optional highlight callback)
  "In CHAT goto message denoted by MSG-ID.
If HIGHLIGHT is non-nil then highlight with fading background color.
This call is asynchronous, and might require history fetching.
CALLBACK is called after point is moved to the message with MSG-ID."
  (declare (indent 2))

  ;; 1. Put message at point (if id differs from MSG-ID) into
  ;;    messages ring
  ;; 2. If message seen in chatbuf, jump to it
  ;; 3. Otherwise, fetch history containing message and jump to it
  (when (ring-p telega-chatbuf--messages-pop-ring)
    (when-let (msg-at-point (telega-msg-at (point)))
      (unless (eq msg-id (plist-get msg-at-point :id))
        (ring-insert telega-chatbuf--messages-pop-ring msg-at-point)

        (telega-help-message 'msg-ring-pop
            "%s to jump back"
          (telega-keys-description
           'telega-chatbuf-goto-pop-message telega-chat-mode-map))
        )))

  ;; NOTE: message with MSG-ID might be already deleted, so load
  ;; history only if:
  ;;   - MSG-ID is less then id of the first message shown in
  ;;     chatbuf and older history might be loaded
  ;;   - MSG-ID is greater then id of the last message shown in
  ;;     chatbuf and newer history might be loaded
  (if (or (zerop msg-id)
          (telega-chatbuf--goto-loaded-msg msg-id highlight)
          (when-let ((first-msg (telega-chatbuf--first-msg))
                     (last-msg (telega-chatbuf--last-msg)))
            (when (not (or (and (< msg-id (plist-get first-msg :id))
                                (telega-chatbuf--need-older-history-p))
                           (and (> msg-id (plist-get last-msg :id))
                                (telega-chatbuf--need-newer-history-p))))
              (cond ((< msg-id (plist-get first-msg :id))
                     (goto-char (point-min)))
                    ((> msg-id (plist-get last-msg :id))
                     (goto-char (point-max)))
                    (t
                     (message "telega: %s (MSG-ID=%S)"
                              (telega-i18n "lng_message_not_found")
                              msg-id)

                     ;; NOTE: Move point to the message before
                     ;; deleted message with MSG-ID
                     (goto-char (point-max))
                     (when-let ((prev-node
                                 (telega-ewoc--find
                                  telega-chatbuf--ewoc
                                  msg-id #'> (telega--tl-prop :id)
                                  nil #'ewoc--node-prev)))
                       (ewoc-goto-node telega-chatbuf--ewoc prev-node))))
              t)))
      (when callback
        (funcall callback))

    (telega-chatbuf--filter-reset)
    (telega-chatbuf--thread-reset)
    (telega-chatbuf--clean)
    (telega-chatbuf--load-history
        msg-id (- (/ telega-chat-history-limit 2)) nil
      (lambda (_ignored)
        (telega-chatbuf--goto-loaded-msg msg-id highlight)
        (when callback
          (funcall callback))))))

(defun telega-chat--goto-msg (chat msg-id &optional highlight callback)
  "Pop chatbuf for the CHAT and goto message denoted by MSG-ID."
  (declare (indent 3))

  ;; 1. Put message at point into messages ring
  ;; 2. If message seen in chatbuf, jump to it
  ;; 3. Otherwise, fetch history containing message and jump to it
  (with-current-buffer (telega-chat--pop-to-buffer chat :no-history)
    (telega-chatbuf--goto-msg msg-id highlight callback)))

(defun telega-chat--goto-thread (chat thread-id
                                      &optional reply-msg-id)
  "For CHAT open chatbuf viewing thread defined by THREAD-ID."
  (cl-assert (not (telega-zerop thread-id)))
  (let* ((telega-server-call-timeout 3.0)
         (thread-info (or (telega--getMessageThread chat thread-id)
                          (error "Thread not available, try later")))
         (thread-chat (telega-chat-get
                       (plist-get thread-info :chat_id) 'offline))
         (thread-msg
          (car (last (append (plist-get thread-info :messages) nil)))))
    (with-current-buffer (telega-chat--pop-to-buffer thread-chat :no-history)
      (telega-chatbuf-filter-by-thread
       (plist-put thread-msg :telega-thread-info thread-info)
       (when reply-msg-id :no-history))
      (when reply-msg-id
        (let ((telega-chatbuf--inhibit-filter-reset '(thread)))
          (telega-chat--goto-msg thread-chat reply-msg-id 'highlight))))))


(defconst telega-chat--tl-message-filters
  '(("photo"
     (return t) (:@type "searchMessagesFilterPhoto"))
    ("photo-video"
     (return t) (:@type "searchMessagesFilterPhotoAndVideo"))
    ("url"
     (return t) (:@type "searchMessagesFilterUrl"))
    ("doc"
     (return t) (:@type "searchMessagesFilterDocument"))
    ("file"
     (return t) (:@type "searchMessagesFilterDocument"))
    ("gif"
     (return t) (:@type "searchMessagesFilterAnimation"))
    ("audio"
     (return t) (:@type "searchMessagesFilterAudio"))
    ("video"
     (return t) (:@type "searchMessagesFilterVideo"))
    ("voice-note"
     (return t) (:@type "searchMessagesFilterVoiceNote"))
    ("video-note"
     (return t) (:@type "searchMessagesFilterVideoNote"))
    ("voice-video-note"
     (return t) (:@type "searchMessagesFilterVoiceAndVideoNote"))
    ("chat-photo"
     (return t) (:@type "searchMessagesFilterChatPhoto"))
    ("mention"
     (return t) (:@type "searchMessagesFilterMention"))
    ("unread-mention"
     (return t) (:@type "searchMessagesFilterUnreadMention"))
    ("unread-reaction"
     (return t) (:@type "searchMessagesFilterUnreadReaction"))
    ("failed-to-send"
     (return t) (:@type "searchMessagesFilterFailedToSend"))
    ("pinned"
     (return t) (:@type "searchMessagesFilterPinned")))
  "List of message filters defined by TDLib.")

(defconst telega-chat--message-filters
  `(("scheduled"
     has-scheduled-messages telega-chatbuf-filter-scheduled)
    ("search"
     (return t) telega-chatbuf-filter-search)
    ("by-sender"
     (or (not (type channel))
         ;; NOTE: Members in channel available only for admins
         (me-is-owner or-admin))
     telega-chatbuf-filter-by-sender)
    ("hashtag"
     (return t) telega-chatbuf-filter-hashtag)
    ("topic"
     is-forum telega-chatbuf-filter-by-topic)
    ,@telega-chat--tl-message-filters))

(defun telega-chatbuf--read-filter (prompt msg-filter-specs)
  "Read a message filter from MSG-FILTER-SPECS."
  (let ((msg-filter-spec
         (assoc (funcall telega-completing-read-function
                         prompt
                         (mapcar #'car (cl-remove-if-not
                                        (lambda (spec)
                                          (telega-chatbuf-match-p (nth 1 spec)))
                                        msg-filter-specs))
                         nil t)
                msg-filter-specs)))
    (list :title (nth 0 msg-filter-spec)
          :tdlib-msg-filter (nth 2 msg-filter-spec))))

(defun telega-chatbuf-filter (msg-filter)
  "Enable chat message filtering MSG-FILTER."
  (interactive (list (telega-chatbuf--read-filter
                      "Chat Messages Filter: "
                      telega-chat--message-filters)))

  (cl-assert (and msg-filter (plist-get msg-filter :tdlib-msg-filter)))
  (if (commandp (plist-get msg-filter :tdlib-msg-filter) 'for-interactive)
      (call-interactively (plist-get msg-filter :tdlib-msg-filter))

    (cl-assert (listp (plist-get msg-filter :tdlib-msg-filter)))
    (setq telega-chatbuf--msg-filter msg-filter)

    (let* ((msg-at-point (telega-msg-at (point)))
           (msg-id (unless (telega-msg-internal-p msg-at-point)
                     (plist-get msg-at-point :id))))
      (telega-chatbuf--clean)
      (telega-chatbuf--load-history msg-id -1 nil
        (lambda (total-messages)
          (when telega-chatbuf--msg-filter
            (plist-put telega-chatbuf--msg-filter :total-count total-messages)
            (telega-chatbuf--chat-update "msg-filter"))
          (when msg-id
            (telega-chatbuf--goto-loaded-msg msg-id)))))

    (when (member (plist-get msg-filter :title)
                  telega-chat-message-filters-as-media)
      (telega-chatbuf--enable-compact-media-view))

    (telega-help-message 'msg-filter-cancel
        "%s to cancel messages filtering"
      (telega-keys-description
       'telega-chatbuf-filter-cancel telega-chat-mode-map)))

  (telega-chatbuf--chat-update "msg-filter"))

(defun telega-chatbuf-filter-by-topic (topic &optional no-history-load)
  "Show only messages belonging to TOPIC."
  (interactive
   (if (telega-chat-match-p telega-chatbuf--chat 'is-forum)
       (list (telega-completing-read-topic telega-chatbuf--chat "Topic: ") nil)
   (user-error "Can't read topic for non-forum chat")))

  (telega-chatbuf-filter-by-thread topic no-history-load))

(defun telega-chatbuf-filter-by-thread (&optional thread no-history-load)
  "Start filtering messages showing only messages that belongs to THREAD.
THREAD is either forum topic or message starting a thread.
If THREAD is nil, then reset filtering by thread.
If NO-HISTORY-LOAD is specified, do not load history."
  (setq telega-chatbuf--thread thread)
  (telega-chatbuf--chat-update "thread")

  ;; NOTE from TDLib 1.8.19: need to preload message designated by
  ;; message thread id before loading history
  (unless (telega-msg-get telega-chatbuf--chat
              (telega-chatbuf--message-thread-id))
    (telega--getMessage (plist-get telega-chatbuf--chat :id)
        (telega-chatbuf--message-thread-id)))

  (telega-chatbuf--clean)
  (unless no-history-load
    (telega-chatbuf--load-initial-history))
  )

(defun telega-chatbuf-thread-cancel (&rest _ignored)
  "Cancel filtering by thread."
  (interactive)
  (telega-chatbuf-filter-by-thread nil))

(defun telega-chatbuf-filter-search (&optional query by-sender-p)
  "Interactively search for messages in chatbuf.
If `\\[universal-argument]' is given, then search for QUERY sent
by some chat member, member name is queried."
  (interactive "sSearch Query: \nP")
  (let ((by-sender (when by-sender-p
                     (telega-completing-read-chat-member
                      "Sent by: " telega-chatbuf--chat))))
    (telega-chatbuf-filter
     (list :title (format "search \"%s\"" query)
           :tdlib-msg-filter '(:@type "searchMessagesFilterEmpty")
           :query query
           :sender by-sender))

    (when (and query (not (string-empty-p query)))
      (telega-highlight-text (regexp-quote query)))
    ))

(defun telega-chatbuf-filter-by-sender ()
  "Show only messages send by some member, member is queried."
  (interactive)
  (telega-chatbuf-filter-search "" 'by-sender))

(defun telega-chatbuf-filter-hashtag (hashtag &optional by-sender-p)
  "Show only messages marked with HASHTAG.
If `\\[universal-argument]' is given, then search for HASHTAG
sent by some chat member, member name is queried."
  (interactive (list (funcall telega-completing-read-function
                              "Hashtag: #" (telega--searchHashtags ""))
                     current-prefix-arg))
  (telega-chatbuf-filter-search
   (concat (unless (string-prefix-p "#" hashtag) "#") hashtag)
   by-sender-p))

(defun telega-chatbuf-filter-scheduled ()
  "Show only scheduled messages."
  (interactive)

  (let ((scheduled-messages
         (telega--getChatScheduledMessages telega-chatbuf--chat)))
    (telega-chatbuf--filter-reset 'no-update)
    (telega-chatbuf--thread-reset 'no-update)
    (telega-chatbuf--clean)
    (setq telega-chatbuf--msg-filter
          (list :title "scheduled"
                :tdlib-msg-filter #'telega-chatbuf-filter-scheduled
                :total-count (length scheduled-messages)))
    (telega-chatbuf--chat-update "thread" "msg-filter")

    (telega-chatbuf--insert-messages (nreverse scheduled-messages) 'prepend)))

(defun telega-chatbuf-filter-cancel (&optional thread-cancel-p)
  "Cancel current messages filtering.
If point is at some message, then keep point on this message after reseting.
If `\\[universal-argument]' is given, then cancel thread filtering as well."
  (interactive "P")
  (when (or telega-chatbuf--msg-filter
            (and thread-cancel-p telega-chatbuf--thread))
    ;; NOTE: if point is at some message, then keep this
    ;; message visible, otherwise load initial history
    (let ((msg-at-point (or (telega-msg-at (point))
                            (and thread-cancel-p (telega-chatbuf--thread-msg)))))
      (when telega-chatbuf--msg-filter
        (telega-chatbuf--filter-reset))
      (when (and thread-cancel-p telega-chatbuf--thread)
        (telega-chatbuf--thread-reset))
      (telega-chatbuf--clean)
      (if (and msg-at-point
               ;; NOTE: Ignore scheduled/internal messages, see
               ;; https://github.com/zevlg/telega.el/issues/250
               (not (telega-msg-internal-p msg-at-point))
               (not (plist-get msg-at-point :sending_state))
               (not (plist-get msg-at-point :scheduling_state)))
          (progn
            (telega-chatbuf--insert-messages (list msg-at-point) 'append)
            (telega-chatbuf--load-newer-history))
          ;; (telega-chat--goto-msg
          ;;     telega-chatbuf--chat (plist-get msg-at-point :id) 'highlight)

        (telega-chatbuf--load-initial-history))))

  ;; Make sure text highlighting is disabled, in case
  ;; `telega-chatbuf-filter-search' filter has been used.
  (telega-highlight-text-mode -1)
  (telega-chatbuf--chat-update "highlight-text")
  )


;;; Inplace searching mode
(defconst telega-chat--inplace-search-filters
  `(("query"
     (return t) telega-chatbuf-inplace-search-query)
    ("by-sender"
     (or (not (type channel))
         ;; NOTE: Members in channel available only for admins
         (me-is-owner or-admin))
     telega-chatbuf-inplace-search-by-sender)
    ,@telega-chat--tl-message-filters))

(defvar telega-chatbuf--inplace-search-filter nil
  "Plist with inplace searching params.")
(make-variable-buffer-local 'telega-chatbuf--inplace-search-filter)

(defun telega-chatbuf-inplace-search (isearch-filter &optional forward-p)
  "Search backward in the chatbuf.
If `\\[universal-argument]' is given, then search forward instead."
  (interactive (let ((is-forward current-prefix-arg))
                 (list (telega-chatbuf--read-filter
                        (format "Telega-search (%s) for: "
                                (if is-forward "forward" "backward"))
                        telega-chat--inplace-search-filters)
                       is-forward)))

  (if (commandp (plist-get isearch-filter :tdlib-msg-filter) 'for-interactive)
      (call-interactively (plist-get isearch-filter :tdlib-msg-filter))

    (message "telega: %s" (telega-i18n "lng_profile_loading"))
    (setq telega-chatbuf--inplace-search-filter isearch-filter)
    (let ((msg-at-point (telega-msg-at (point))))
      (telega--searchChatMessages telega-chatbuf--chat
          (plist-get isearch-filter :tdlib-msg-filter)
          (or (plist-get msg-at-point :id) 0)
          (if forward-p -2 0)
        :query (plist-get isearch-filter :query)
        :limit 3
        :sender (plist-get isearch-filter :by-sender)
        :callback
        (lambda (reply)
          (let ((next-msg (car (append (plist-get reply :messages) nil))))
            (if (or (not next-msg)
                    (eq (plist-get next-msg :id) (plist-get msg-at-point :id)))
                (message "telega: \"%s\" not found"
                         (plist-get isearch-filter :query))

              (message "")
              (telega-msg-goto-highlight next-msg))))
        ))))

(defun telega-chatbuf-inplace-search-query (query &optional by-sender-p forward-p)
  "Inplace search backward for the QUERY.
If `\\[universal-argument]' is given, search for the messages
containing QUERY sent by specified sender.
To navigate across searching results use
`\\<telega-chat-mode-map>\\[telega-chatbuf-inplace-search-prev]' and
`\\<telega-chat-mode-map>\\[telega-chatbuf-inplace-search-next]'
bindings."
  (interactive "sTelega-search (backward): \nP")
  (let ((by-sender (when by-sender-p
                     (telega-completing-read-chat-member
                      "Sent by: " telega-chatbuf--chat))))
    (telega-chatbuf-inplace-search
     (list :title (format "search for %s" query)
           :tdlib-msg-filter '(:@type "searchMessagesFilterEmpty")
           :query query
           :by-sender by-sender)
     forward-p)

    (when (and query (not (string-empty-p query)))
      (telega-highlight-text (regexp-quote query))
      (telega-chatbuf--chat-update "highlight-text"))
    ))

(defun telega-chatbuf-inplace-search-query-forward (query &optional by-sender-p)
  "Inplace search forward for the QUERY.
If `\\[universal-argument]' is given, search for the messages
containing QUERY sent by specified sender."
  (interactive "sTelega-search (forward): \nP")
  (telega-chatbuf-inplace-search-query query by-sender-p 'forward))

(defun telega-chatbuf-inplace-search-prev (&optional forward-p)
  "Continue searching."
  (interactive)
  (if telega-chatbuf--inplace-search-filter
      (telega-chatbuf-inplace-search
       telega-chatbuf--inplace-search-filter forward-p)
    (call-interactively #'telega-chatbuf-inplace-search)))

(defun telega-chatbuf-inplace-search-next ()
  "Continue inplace searching with last search."
  (interactive)
  (telega-chatbuf-inplace-search-prev 'forward))


;;; Chat Themes
(defun telega-chat-theme--choosen-action (theme)
  "Execute action when chat theme BUTTON is pressed."
  (let ((chat telega--chat))
    ;; NOTE: Kill help win before modifying chatbuffer, because it
    ;; recovers window configuration on kill
    (quit-window 'kill-buffer)
    (with-telega-chatbuf chat
      (telega-chatbuf-input-insert
       (list :@type "telegaChatTheme"
             :name (telega-tl-str theme :name))))))

(defun telega-ins--chat-theme (theme)
  "Inserter for chat THEME."
  (let* ((lightp (eq (frame-parameter nil 'background-mode) 'light))
         (_theme-settings
          (plist-get theme (if lightp :light_settings :dark_settings))))
    ;; TODO: generate and insert SVG reflecting color values in the
    ;; THEME-SETTINGS
    (telega-ins (telega-tl-str theme :name))
    ))

(defun telega-describe-chat-themes (&optional themes for-chat)
  "Describe chat themes."
  (let ((themes (or themes telega--chat-themes))
        (help-window-select t))
    (with-telega-help-win "*Telegram Chat Themes*"
      (visual-line-mode 1)
      ;; NOTE: Non-nil `auto-window-vscroll' make C-n jump to the end
      ;; of the buffer
      (set (make-local-variable 'auto-window-vscroll) nil)

      (setq telega--chat for-chat)
      (setq telega--help-win-param themes)

      (telega-ins--with-face 'bold
        (telega-ins-i18n "lng_chat_theme_title"))
      (when (telega-tl-str for-chat :theme_name)
        (telega-ins " ")
        (telega-ins--button (telega-i18n "lng_chat_theme_reset")
          :value nil
          :action #'telega-chat-theme--choosen-action))
      (telega-ins "\n")
      (telega-ins--help-message
       (telega-ins-i18n "lng_chat_theme_title_about"
         :user (telega-chat-title for-chat)))
      (telega-ins "\n")

      (seq-doseq (theme themes)
        (telega-button--insert 'telega theme
          :inserter #'telega-ins--chat-theme
          :action #'telega-chat-theme--choosen-action)
        (telega-ins " "))
      )))

(defun telega-chatbuf-attach-chat-theme ()
  "Interactively attach new chat theme to the chat buffer."
  (interactive)
  (telega-describe-chat-themes telega--chat-themes telega-chatbuf--chat))


;;; Chatbuf DND
(defun telega-dnd--filename-icloud-p (filename)
  "Return non-nil if FILENAME is stored remotely in iCloud."
  (let ((fdir (file-name-directory filename))
        (fbase (file-name-nondirectory filename)))
    (file-exists-p (expand-file-name (concat "." fbase ".icloud") fdir))))

(defun telega-chatbuf-dnd-attach (uri _action)
  "DND open function for chat buffer."
  (cl-assert telega-chatbuf--chat)
  (if-let ((filename (dnd-get-local-file-name uri)))
      (cond ((file-exists-p filename)
             (telega-chatbuf-attach-media filename))
            ((telega-dnd--filename-icloud-p filename)
             (error "File %S is stored in the iCloud.  Please download it first"
                    filename))
            (t
             (error "File %S is inaccessible" filename)))

    (cl-assert (string-match-p "https?://" uri))
    (telega-chatbuf-attach-media uri))
  'private)


;; Chat Event Log
(defun telega-chatevent-log-filter (&rest filters)
  "Return chat event log filter.
FILTERS are:
`:message_edits', `:message_deletions', `:message_pins',
`:member_joins', `:member_leaves', `:member_invites',
`:member_promotions', `:member_restrictions', `:info_changes',
`:setting_changes'."
  (apply 'nconc (list :@type "chatEventLogFilters")
         (mapcar (lambda (filter) (list filter t)) filters)))

(provide 'telega-chat)

;;; telega-chat.el ends here

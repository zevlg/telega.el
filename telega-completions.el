;;; telega-completions.el --- CAPF completions for telega  -*- lexical-binding:t -*-

;; Copyright (C) 2024 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: 2024
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

;; Completion helpers and CAPF (completion-at-point-functions) based
;; completions for telega chat buffers.  CAPF-specific adapters are
;; defined below and use `external-completion' to fetch candidates
;; from TDLib.
;;
;; The `external-completion' style means the LOOKUP function is
;; synchronous but interruptible: it waits for TDLib via
;; `while-no-input', so if the user keeps typing the wait is aborted
;; and the buffer-local cache is returned instead.
;;
;; Provides:
;;  `telega-capf-emoji'            -- :<name>: emoji (local list)
;;  `telega-capf-telegram-emoji'   -- :<name>: emoji via TDLib searchEmojis
;;  `telega-capf-username'         -- @username / @@admin mentions
;;  `telega-capf-hashtag'          -- #hashtag via searchHashtags
;;  `telega-capf-botcmd'           -- /bot-command at start of input
;;  `telega-capf-quick-reply'      -- /shortcut in private chats
;;  `telega-capf-markdown-precode' -- ```language blocks
;;
;; Setup:
;;   (add-hook 'telega-chat-mode-hook #'telega-completions-setup-capf)

;;; Code:
(require 'cl-lib)

(require 'telega-info)

(declare-function external-completion-table "external-completion"
                  (category lookup &optional metadata
                            try-completion-function))
(declare-function external-completion--call "external-completion"
                  (op string table pred point))

;;; Internal: interruptible TDLib lookup

(defun telega-completions--ensure-external-completion ()
  "Ensure `external-completion' is available.
Load the built-in/ELPA package when present.  On older Emacs
versions, install a local polyfill with the same protocol."
  (or (featurep 'external-completion)
      (require 'external-completion nil t)
      (progn
        (add-to-list 'completion-styles-alist
                     '(external
                       external-completion--try-completion
                       external-completion--all-completions
                       "Ad-hoc completion style provided by the completion table."))

        (defun external-completion-table (category lookup
                                                   &optional metadata
                                                   try-completion-function)
          "Make completion table using the `external' completion style."
          (let ((probe (alist-get category completion-category-defaults)))
            (if probe
                (cl-assert (equal '(external) (alist-get 'styles probe))
                           nil "Category `%s' must only use `external' style"
                           category)
              (push `(,category (styles external))
                    completion-category-defaults)))
          (let ((cache (make-hash-table :test #'equal)))
            (cl-flet ((lookup-internal (string point)
                        (let* ((key (cons string point))
                               (probe (gethash key cache 'external--notfound)))
                          (if (eq probe 'external--notfound)
                              (puthash key (funcall lookup string point) cache)
                            probe))))
              (lambda (string pred action)
                (pcase action
                  (`metadata
                   `(metadata (category . ,category) . ,metadata))
                  (`(external-completion--tryc . ,point)
                   `(external-completion--tryc
                     . ,(if try-completion-function
                            (funcall try-completion-function
                                     string
                                     point
                                     (lookup-internal string point))
                          (cons string point))))
                  (`(external-completion--allc . ,point)
                   (let ((all (lookup-internal string point)))
                     `(external-completion--allc
                       . ,(if pred (cl-remove-if-not pred all) all))))
                  (`(boundaries . ,_) nil)
                  (_method
                   (let ((all (lookup-internal string (length string))))
                     (complete-with-action action all string pred))))))))

        (defun external-completion--call (op string table pred point)
          (when (functionp table)
            (let ((res (funcall table string pred (cons op point))))
              (when (eq op (car-safe res))
                (cdr res)))))

        (defun external-completion--try-completion (string table pred point)
          (external-completion--call 'external-completion--tryc
                                     string table pred point))

        (defun external-completion--all-completions (string table pred point)
          (external-completion--call 'external-completion--allc
                                     string table pred point))

        (provide 'external-completion)
        t)))

(defvar-local telega-completions--cache (make-hash-table :test #'equal)
  "Buffer-local cache: (TYPE . QUERY-STRING) -> candidate list.
Used as fallback when `while-no-input' aborts a TDLib wait.")

(defun telega-completions--lookup (type async-fn query)
  "Call ASYNC-FN with QUERY and wait interruptibly for the result.
ASYNC-FN is called as (ASYNC-FN QUERY CALLBACK).
While waiting, yields to user input via `while-no-input' and
`accept-process-output'.  If the user types before the result
arrives, returns the cached value for (TYPE . QUERY) instead.
On success, updates the cache and returns fresh candidates."
  (let ((result nil)
        (done nil))
    (funcall async-fn query
             (lambda (candidates)
               (setq result candidates
                     done t)))
    (let ((outcome
           (while-no-input
             (while (and (not done)
                         (accept-process-output
                          (telega-server--proc)
                          telega-server-call-timeout)))
             result)))
      (if (eq outcome t)
          ;; Interrupted by user input — return stale cache
          (gethash (cons type query) telega-completions--cache '())
        ;; Got fresh result — update cache and return
        (puthash (cons type query) outcome telega-completions--cache)
        outcome))))


;;; Shared completion helpers
(defun telega-completions--emoji-candidates (prefix)
  "Return local emoji candidates matching PREFIX."
  (telega-emoji-init)
  (cl-remove-if-not
   (lambda (emoji-name)
     (or (string-prefix-p prefix emoji-name)
         (and telega-completions-emoji-fuzzy-match
              (string-match-p
               (regexp-quote (concat "-" (substring prefix 1)))
               emoji-name))))
   telega-emoji-candidates))

(defun telega-completions--emoji-annotation (emoji)
  "Return annotation string for EMOJI."
  (concat "  " (if telega-emoji-use-images
                   (propertize "EE" 'display
                               (telega-emoji-create-svg emoji))
                 emoji)))

(defun telega-completions--emoji-post-completion (candidate emoji)
  "Replace completed CANDIDATE with EMOJI."
  (delete-region (- (point) (length candidate)) (point))
  (insert emoji))

(defun telega-completions--telegram-emoji-candidates (&optional prefix suffix)
  "Return callback to search Telegram emoji candidates.
Surround emoji keyword with PREFIX and SUFFIX."
  (lambda (query callback)
    ;; NOTE: `telega--searchEmojis' will return nil on empty search
    (if (string-empty-p query)
        (funcall callback nil)

      (let* ((closed-p (string-suffix-p ":" query))
             (search-text 
              ;; Strip off leading ":" and replace `-' with spaces
              ;; before the search, so one could use `:i-love-you' for
              ;; example
              (replace-regexp-in-string
               "-" " "
               (substring query 1 (and closed-p -1)))))
      (telega--searchEmojis search-text
        :callback
        (lambda (emoji-keywords)
          (funcall callback
                   (mapcar (lambda (ek)
                             (propertize
                              (concat prefix
                                      (telega-tl-str ek :keyword)
                                      ;; (replace-regexp-in-string
                                      ;;  " " "-" (telega-tl-str ek :keyword))
                                      suffix)
                              'telega-emoji (telega-tl-str ek :emoji)))
                           emoji-keywords))))))))

(defun telega-completions--language-names ()
  "Return completion candidates for markdown code block languages."
  (let* ((all-buffers (buffer-list))
         (modes
          (seq-uniq (seq-filter #'symbolp (mapcar #'cdr auto-mode-alist))))
         (indexed-modes
          (mapcar (lambda (mode)
                    (cons mode (seq-count
                                (lambda (buffer)
                                  (eq (buffer-local-value 'major-mode buffer)
                                      mode))
                                all-buffers)))
                  modes))
         (sorted-modes
          (mapcar #'car (cl-sort indexed-modes #'> :key #'cdr))))
    (delq nil
          (mapcar (lambda (mode)
                    (let ((mode-name (symbol-name mode)))
                      (when (string-suffix-p "-mode" mode-name)
                        (substring mode-name 0 -5))))
                  sorted-modes))))

(defun telega-completions--markdown-precode-post-completion (&rest _ignored)
  "Insert trailing code fence structure after markdown language completion."
  (if (save-excursion (re-search-forward "^```" nil 'noerror))
      (forward-char)
    (insert "\n")
    (save-excursion (insert "\n```"))))

(defun telega-completions--hashtag-query (input)
  "Return TDLib hashtag query for INPUT."
  (if (string-prefix-p "#" input)
      (substring input 1)
    input))

(defun telega-completions--hashtag-candidates (hashtags)
  "Return completion candidates built from HASHTAGS."
  (mapcar (lambda (hashtag)
            (concat "#" hashtag))
          hashtags))

(defun telega-completions--hashtag-search (input &optional callback)
  "Search hashtag completion candidates for INPUT.
If CALLBACK is non-nil, invoke it asynchronously with the candidate list."
  (let ((query (telega-completions--hashtag-query input)))
    (if callback
        (telega--searchHashtags
         query
         :callback
         (lambda (hashtags)
           (funcall callback
                    (telega-completions--hashtag-candidates hashtags))))
      (telega-completions--hashtag-candidates
       (telega--searchHashtags query)))))

(defun telega-completions--username-admin-p (input)
  "Return non-nil if INPUT requests administrator completion."
  (string-prefix-p "@@" input))

(defun telega-completions--username-query (input)
  "Return TDLib query string for username INPUT."
  (cond ((string-prefix-p "@@" input)
         (substring input 2))
        ((string-prefix-p "@" input)
         (substring input 1))
        (t input)))

(defun telega-completions--username-filter (input)
  "Return TDLib members filter for username INPUT."
  (if (telega-completions--username-admin-p input)
      '(:@type "chatMembersFilterAdministrators")
    (list :@type "chatMembersFilterMention"
          :topic_id (telega-chatbuf--MessageTopic))))

(defun telega-completions--username-candidate (member input)
  "Build a completion candidate for MEMBER using INPUT.
Return nil if MEMBER can't be represented safely."
  (when member
    (let* ((label (or (ignore-errors
                        (telega-msg-sender-username member 'with-@))
                      (ignore-errors
                        (telega-msg-sender-title member))))
           (candidate
            (if (and (telega-completions--username-admin-p input)
                     (stringp label)
                     (string-prefix-p "@" label))
                (concat "@" label)
              label)))
      (when (and (stringp candidate)
                 (not (string-empty-p candidate)))
        (propertize candidate
                    'telega-member member
                    'telega-input input)))))

(defun telega-completions--username-member-candidates (members input)
  "Return mention candidates built from MEMBERS for INPUT."
  (delq nil
        (mapcar (lambda (member)
                  (telega-completions--username-candidate member input))
                (cl-remove-if
                 (telega-match-gen-predicate 'sender
                                             '(or is-blocked
                                                  (user is-deleted)))
                 members))))

(defun telega-completions--username-extra-candidates (input &optional
                                                            with-nonmembers-p)
  "Return non-member username candidates for INPUT.
If WITH-NONMEMBERS-P is non-nil, also include usernames for chats
matching `telega-completions-username-complete-nonmember-for' chat
temex."
  (unless (telega-completions--username-admin-p input)
    (nconc (when with-nonmembers-p
             ;; NOTE: In case there is no candidates, and INPUT starts
             ;; some username from Main chat list, then complete it
             (cl-remove-if-not
              (lambda (username)
                (and username
                     (string-prefix-p input username 'ignore-case)))
              (mapcar (lambda (chat-user)
                        (telega-msg-sender-username chat-user 'with-@))
                      (telega-filter-chats (telega-chats-list)
                        telega-completions-username-complete-nonmember-for))))

           (seq-remove
            (lambda (botname)
              (not (string-prefix-p input botname 'ignore-case)))
            (seq-union telega--recent-inline-bots
                       telega-known-inline-bots
                       #'string=)))))

(defun telega-completions--username-candidates (members input)
  "Return completion candidates from MEMBERS for INPUT."
  (let ((cands (telega-completions--username-member-candidates members input)))
    (nconc cands
           (telega-completions--username-extra-candidates
            input (unless cands 'with-nonmembers)))))

(defun telega-completions--username-search (chat input &optional callback)
  "Search CHAT mention candidates for INPUT.
Use COMPLETE-NONMEMBER-FOR to extend results with non-member usernames.
If CALLBACK is non-nil, invoke it asynchronously with the candidate list."
  (let ((query (telega-completions--username-query input))
        (filter (telega-completions--username-filter input)))
    (if callback
        (telega--searchChatMembers
         chat query filter
         :callback
         (lambda (members)
           (funcall callback
                    (telega-completions--username-candidates members input))))
      (telega-completions--username-candidates
       (telega--searchChatMembers chat query filter)
       input))))

(defun telega-completions--username-annotation (candidate)
  "Return annotation for the username CANDIDATE."
  (when-let* ((member (or (get-text-property 0 'telega-member candidate)
                          (telega-user--by-username candidate))))
    (telega-ins--as-string
     (telega-ins "  ")
     (telega-ins--msg-sender member
       :with-avatar-p telega-completions-username-show-avatars)
     (when-let* ((_ (telega-user-p member))
                 (admin (telega-chat-admin-get telega-chatbuf--chat member)))
       (telega-ins--with-face 'telega-shadow
         (telega-ins " ("
                     (or (telega-tl-str admin :custom_title)
                         (if (plist-get admin :is_owner)
                             (telega-i18n "lng_owner_badge")
                           (telega-i18n "lng_admin_badge")))
                     ")"))))))

(defun telega-completions--username-post-completion (arg &optional _status)
  "Insert completed username ARG."
  (when-let* ((member (and (get-text-property 0 'telega-input arg)
                           (get-text-property 0 'telega-member arg)))
              (_ (telega-user-p member)))
    (delete-region (- (point) (length arg)) (point))
    (when-let ((name (seq-some (lambda (name-fmt)
                                 (telega-user-title member name-fmt 'raw))
                               telega-completions-username-prefer-name)))
      (telega-ins
       (cond
        ((string-prefix-p "@" name) name)
        ((member telega-completions-username-markup '("markdown1" "markdown2"))
         (telega-string-as-markup
             (format "[%s](tg://user?id=%d)" name (plist-get member :id))
             telega-completions-username-markup
             (cdr (assoc telega-completions-username-markup
                         telega-chat-markup-functions))))
        ((equal telega-completions-username-markup "org")
         (telega-string-as-markup
             (propertize (format "[[tg://user?id=%d][%s]]"
                                 (plist-get member :id) name)
                         'org-link t)
             telega-completions-username-markup
             (cdr (assoc telega-completions-username-markup
                         telega-chat-markup-functions))))
        (t
         (propertize name
                     :tl-entity-type (list :@type "textEntityTypeMentionName"
                                           :user_id (plist-get member :id))
                     'face 'telega-entity-type-mention
                     'rear-nonsticky nil
                     'front-sticky nil))))))

  (let ((input (telega-chatbuf-input-string)))
    (insert " ")
    (when (or (member input telega-known-inline-bots)
              (member input telega--recent-inline-bots))
      (telega-chatbuf-complete-inline-bot-query 'no-search))))

(defun telega-completions--bot-commands-list (bot-commands &optional suffix)
  "Return completion candidates for BOT-COMMANDS with optional SUFFIX."
  (mapcar (lambda (bot-cmd)
            (propertize (concat "/" (telega-tl-str bot-cmd :command) suffix)
                        'telega-annotation
                        (telega-ins--as-string
                         (telega-ins--with-attrs
                             (list :max (/ telega-chat-fill-column 2) :elide t)
                           (telega-ins (telega-tl-str bot-cmd :description))))))
          bot-commands))

(defun telega-completions--bot-commands (chat)
  "Return bot command completion candidates for CHAT."
  (let* ((info (telega-chat--info chat))
         (telega-full-info-offline-p nil)
         (full-info (telega--full-info info)))
    (if (telega-chatbuf-match-p '(type bot))
        (telega-completions--bot-commands-list
         (telega--tl-get full-info :bot_info :commands))
      (mapcar (lambda (bot-commands)
                (telega-completions--bot-commands-list
                 (plist-get bot-commands :commands)
                 (telega-msg-sender-username
                  (telega-user-get
                   (plist-get bot-commands :bot_user_id))
                  'with-@)))
              (plist-get full-info :bot_commands)))))

(defun telega-completions--annotation (candidate)
  "Return generic completion annotation stored on CANDIDATE."
  (get-text-property 0 'telega-annotation candidate))

(defun telega-completions--quick-replies ()
  "Return quick reply shortcut candidates."
  (mapcar (lambda (qr)
            (propertize
             (concat "/" (telega-tl-str qr :name))
             'telega-qr qr
             'telega-annotation
             (telega-ins--as-string
              (telega-ins--content-one-line (plist-get qr :first_message)))))
          telega--quick-replies))

(defun telega-completions--quick-reply-annotation (candidate)
  "Return annotation for quick reply CANDIDATE."
  (or (telega-completions--annotation candidate)
      (when-let* ((qr (or (get-text-property 0 'telega-qr candidate)
                          (telega-quick-reply-by-name
                           (string-trim candidate "/")))))
        (telega-ins--as-string
         (telega-ins--content-one-line (plist-get qr :first_message))
         (let ((nmessages (length (plist-get qr :messages))))
           (when (> nmessages 1)
             (telega-ins--with-face 'telega-shadow
               (telega-ins " +" (telega-i18n "lng_forum_messages"
                                  :count (1- nmessages))))))))))


;;; Internal: bounds helpers

(defun telega-capf--bounds-for-char (char)
  "Return (START . END) for a completion token starting with CHAR before point.
Handles repeated leading CHARs (e.g. @@).  Returns nil if not applicable."
  (let ((end (point)))
    (save-excursion
      (when (looking-at-p "\\>")
        (skip-syntax-backward "w"))
      ;; At this point we are right after the trigger char(s).
      ;; Check char-before FIRST (same logic as telega-company-grab-single-char),
      ;; then skip backward over all repeated CHARs to find the true start.
      (when (equal (char-before) char)
        (skip-chars-backward (char-to-string char))
        ;; CHAR must not be in the middle of the word
        (unless (looking-at-p "\\>")
          (cons (point) end))))))

(defun telega-capf--botcmd-bounds ()
  "Return (START . END) if point is in a /command at start of chatbuf input."
  (when (and telega-chatbuf--input-marker
             (save-excursion
               (re-search-backward "/[^ ]*" telega-chatbuf--input-marker t))
             (= (match-beginning 0) telega-chatbuf--input-marker))
    (cons (match-beginning 0) (point))))

(defun telega-capf--emoji-name-bounds (&optional allow-spaces-p bound)
  "Return (START . END) where emoji name starts.
Non-nil ALLOW-SPACES-P allows spaces for the closed (having trailing
colon) emoji."
  (let ((end (point))
        (bound (or bound (1- telega-chatbuf--input-marker))))
    (save-excursion
      (when (and (or (re-search-backward
                      (rx (or bol space)
                          (group ":" (1+ (not (or space ":"))) (? ":")))
                      bound 'no-error)
                     ;; Try closed emoji if spaces allowed
                     (and allow-spaces-p
                          (re-search-backward
                           (rx (or bol space) (group ":" (1+ (not ":"))) ":")
                           bound 'no-error)))
                 (equal end (match-end 0)))
        (cons (match-beginning 1) end)))))

;;; CAPF: local emoji (:<name>:)
(defun telega-capf-emoji ()
  "CAPF for emoji completion using the local emoji list."
  (telega-emoji-init)
  (when-let* ((bounds (telega-capf--emoji-name-bounds))
              (start (car bounds))
              (end (cdr bounds))
              (candidates
               (telega-completions--emoji-candidates
                (buffer-substring-no-properties start end))))
    (list start end candidates
          :exclusive 'no
          :annotation-function
          (lambda (en)
            (telega-completions--emoji-annotation
             (cdr (assoc en telega-emoji-alist))))
          :exit-function
          (lambda (candidate _status)
            (when-let* ((emoji (cdr (assoc candidate telega-emoji-alist))))
              (telega-completions--emoji-post-completion
               candidate emoji))))))

;;; CAPF: Telegram emoji (:<name>: via TDLib searchEmojis)

(defun telega-capf-telegram-emoji ()
  "CAPF for emoji completion using TDLib searchEmojis."
  (when-let* ((bounds (telega-capf--emoji-name-bounds 'allow-spaces))
              (start (car bounds))
              (end (cdr bounds)))
    (telega-completions--ensure-external-completion)
    (let ((table
            (external-completion-table
             'telega-emoji
             (lambda (string _point)
               (telega-completions--lookup
                'telegram-emoji
                (telega-completions--telegram-emoji-candidates ":" ":")
                string)))))
      (list start end table
            :exclusive 'no
            :annotation-function
            (lambda (candidate)
              (let ((emoji (get-text-property 0 'telega-emoji candidate)))
                (telega-completions--emoji-annotation emoji)))
            :exit-function
            (lambda (candidate _status)
              (when-let* ((emoji (get-text-property 0 'telega-emoji candidate)))
                (telega-completions--emoji-post-completion
                 candidate emoji)))))))

;;; CAPF: username / @@admin mention

(defun telega-capf--username-table (chat)
  "Return completion table for username mentions in CHAT."
  (telega-completions--ensure-external-completion)
  (external-completion-table
   'telega-username
   (lambda (string _point)
     (telega-completions--lookup
      (if (telega-completions--username-admin-p string)
          'username-admin
        'username)
      (lambda (_query cb)
        (telega-completions--username-search chat string cb))
      (telega-completions--username-query string)))))

(defun telega-capf-username ()
  "CAPF for @username and @@admin mention completions."
  (when-let* ((bounds (telega-capf--bounds-for-char ?\@))
              (start (car bounds))
              (end (cdr bounds)))
    (cl-assert (> end start))
    (let ((table (telega-capf--username-table telega-chatbuf--chat)))
      (list start end table
            :exclusive 'no
            :annotation-function #'telega-completions--username-annotation
            :exit-function #'telega-completions--username-post-completion))))

;;; CAPF: hashtag (#hashtag)

(defun telega-capf-hashtag ()
  "CAPF for #hashtag completions via TDLib searchHashtags."
  (when-let* ((bounds (telega-capf--bounds-for-char ?\#))
              (start (car bounds))
              (end (cdr bounds)))
    (cl-assert (> end start))
    (telega-completions--ensure-external-completion)
    (let ((table
            (external-completion-table
             'telega-hashtag
             (lambda (string _point)
               (telega-completions--lookup
                'hashtag
                (lambda (_query cb)
                  (telega-completions--hashtag-search string cb))
                (telega-completions--hashtag-query string))))))
      (list start end table
            :exclusive 'no
            :exit-function (lambda (&rest _ignored) (insert " "))))))


;;; CAPF: bot commands (/command)

(defun telega-capf-botcmd ()
  "CAPF for /bot-command completions at start of chatbuf input."
  (when-let* ((bounds (telega-capf--botcmd-bounds))
              (start (car bounds))
              (end (cdr bounds)))
    (list start end
          (all-completions (buffer-substring-no-properties start end)
                           (telega-completions--bot-commands
                            telega-chatbuf--chat))
          :exclusive 'no
          :annotation-function #'telega-completions--annotation)))


;;; CAPF: quick reply shortcuts (/name in private chats)

(defun telega-capf-quick-reply ()
  "CAPF for /quick-reply shortcut completions (private chats only)."
  (when-let* ((_ (telega-chatbuf-match-p '(type private)))
              (bounds (telega-capf--bounds-for-char ?/))
              (start (car bounds))
              (end (cdr bounds))
              (collection (telega-completions--quick-replies)))
    (list start end
          (all-completions (buffer-substring-no-properties start end)
                           collection)
          :exclusive 'no
          :annotation-function #'telega-completions--quick-reply-annotation)))


;;; CAPF: markdown code block language (```lang)

(defun telega-capf-markdown-precode ()
  "CAPF for ```language completions in code blocks."
  (when-let* ((end (point))
              (start (save-excursion
                       (and (re-search-backward "```\\([^`\t\n ]*\\)"
                                                (line-beginning-position) t)
                            (match-beginning 1)))))
    (list start end
          (all-completions (buffer-substring-no-properties start end)
                           (telega-completions--language-names))
          :exclusive 'no
          :exit-function
          #'telega-completions--markdown-precode-post-completion)))


;;; Setup

(defun telega-completions-setup-capf (&optional append-p)
  "Setup `completion-at-point' to use `telega-completions-capf-functions'.
If APPEND-P is non-nil, then append telega capf functions to existing,
otherwise fully replace them.
Intended for use in `telega-chat-mode-hook'."
  (interactive)
  (setq-local completion-at-point-functions
              (append telega-completions-capf-functions
                      (when append-p
                        completion-at-point-functions)))
  (setq-local telega-chat-input-complete-functions
              (append telega-chat-input-complete-functions
                      (list #'completion-at-point)))
  )

(provide 'telega-completions)

;;; telega-completions.el ends here

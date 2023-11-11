;;; telega-bridge-bot.el --- Replace bridge bot user        -*- lexical-binding: t; -*-

;; Copyright (C) 2023 by Henry Sun.

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
;; ** /telega-bridge-bot.el/ -- Replace bridge bot user.
;;
;; This package advises some telega functions to replace the message
;; bridge bot (like [[https://t.me/matrix_t2bot][~@matrix_t2bot~]]), to
;; make it look like the user itself.  Currently only works for matrix
;; bridge bot, but can be extended to other bridges by adding more
;; handlers to the ~telega-bridge-bot--counterparty-handler-plist~ .
;;
;; Enable it with:
;;
;; #+begin_src emacs-lisp
;; (require 'telega-bridge-bot)
;; #+end_src
;;
;; and customize the ~telega-bridge-bot-bridge-info-plist~ to specify the
;; relationship between the bridge bot and the chat.
;;
;; For example, if you want to replace the bridge bot
;; [[https://t.me/matrix_t2bot][~@matrix_t2bot~]] in the Telegram chat
;; [[https://t.me/emacs_china][~@emacs_china~]], you can set the following:
;;
;; #+begin_src emacs-lisp
;; (setq telega-bridge-bot-bridge-info-plist
;;       '(-1001773572820 ; id of the @emacs_china
;;         (420415423 ; id of the @matrix_t2bot
;;          ;; will fetch member info with this matrix room id
;;          (:type :matrix :chat-id "!EGzPXoyqkJdTByDCjD:mozilla.org"))))
;; #+end_src
;;
;; If you want download avatar from matrix, you should also set
;; ~telega-bridge-bot-matrix-access-token~ or set
;; ~telega-bridge-bot-matrix-user~ then put the access token in ~.authinfo~ ,
;; the host should be ~matrix.org~ and the user should be the same as
;; ~telega-bridge-bot-matrix-user~.
;; This is required because only user in the room can access the member info.

;;; Code:
(require 'telega)

;; shutup compiler
(defvar url-http-end-of-headers)

;; Customizable variables
(defgroup telega-bridge-bot nil
  "Customisation for telega bridge bot."
  :prefix "telega-bridge-bot-"
  :group 'telega)

(defcustom telega-bridge-bot-matrix-user nil
  "Matrix username used to get matrix access token from authinfo."
  :type 'string
  :group 'telega-bridge-bot)

(defcustom telega-bridge-bot-matrix-access-token nil
  "Matrix access token."
  :type 'string
  :group 'telega-bridge-bot)

(defcustom telega-bridge-bot-matrix-members-expires 3600
  "Matrix members expires time in seconds."
  :type 'integer
  :group 'telega-bridge-bot)

(defcustom telega-bridge-bot-matrix-avatar-expires 43200
  "Matrix avatar expires time in seconds."
  :type 'integer
  :group 'telega-bridge-bot)

(defcustom telega-bridge-bot-bridge-info-plist nil
  "Associate chats and bots information."
  :type '(plist :key-type (integer :tag "Chat ID")
                :value-type (plist :key-type (integer :tag "Bot ID")
                                   :value-type (plist :key-type (symbol :tag "Info Key")
                                                      :value-type (sexp :tag "Info Value"))))
  :group 'telega-bridge-bot)

(defvar telega-bridge-bot--counterparty-handler-plist
  '(:matrix (:fetch-user
             telega-bridge-bot--matrix-fetch-user
             :text-spliter
             telega-bridge-bot--matrix-text-spliter
             :file-spliter
             telega-bridge-bot--matrix-file-spliter)))

(defvar telega-bridge-bot--matrix-room-cache nil)
(defvar telega-bridge-bot--matrix-room-cache-last-modified-plist '())

(defvar telega-bridge-bot--matrix-host "https://matrix-client.matrix.org")
(defvar telega-bridge-bot--matrix-joined-members-endpoint
  "%s/_matrix/client/v3/rooms/%s/joined_members?access_token=%s") ; room id, access token
(defvar telega-bridge-bot--matrix-media-download-endpoint
  "%s/_matrix/media/v3/download/%s") ; mxc id
(defvar telega-bridge-bot--matrix-media-thumbnail-endpoint
  "%s/_matrix/media/v3/thumbnail/%s?width=96&height=96&method=crop") ; mxc id

(defun telega-bridge-bot--download-async (url filename callback)
  "Download URL to FILENAME asynchronously.
CALLBACK is called when download finished."
  (url-retrieve
   url
   (lambda (_status path cb)
     (goto-char (point-min))
     (when (string-match "200 OK" (buffer-string))
       (let ((inhibit-message t))
         (re-search-forward "\r?\n\r?\n")
         (write-region (point) (point-max) path)
         (funcall cb))))
   (list filename callback)
   'silent))


;;; matrix

(defun telega-bridge-bot--get-matrix-access-token ()
  "Return matrix access token for HOST and USERNAME."
  (if telega-bridge-bot-matrix-access-token
      telega-bridge-bot-matrix-access-token
    (auth-source-pick-first-password
     :host "matrix.org" :user telega-bridge-bot-matrix-user)))

(defun telega-bridge-bot--matrix-mxc-id (mxc-url)
  "Return mxc id from MXC-URL."
  ;; drop "mxc://" prefix in mxc-url
  (substring mxc-url 6))

(defun telega-bridge-bot--matrix-fetch-joined-members (matrix-room-id &optional force-update callback-func)
  "Fetch joined members by MATRIX-ROOM-ID.
If FORCE-UPDATE is non-nil, force update the cache.
The result will be stored in `telega-bridge-bot--matrix-room-cache' async.
CALLBACK-FUNC is called when fetch finished or cache exists."
  (let* ((cache-key (intern matrix-room-id))
         (cache
          (or (plist-get telega-bridge-bot--matrix-room-cache cache-key)
              (make-hash-table :test 'equal))))
    (if-let* ((update-p
               (or force-update
                   (time-less-p
                    (plist-get telega-bridge-bot--matrix-room-cache-last-modified-plist cache-key)
                    (time-subtract (current-time)
                                   telega-bridge-bot-matrix-members-expires))
                   (hash-table-empty-p cache)))
              (url-request-method "GET")
              (url-request-extra-headers '(("Content-Type" . "application/json")))
              (access-token (telega-bridge-bot--get-matrix-access-token))
              (url (format telega-bridge-bot--matrix-joined-members-endpoint
                           telega-bridge-bot--matrix-host
                           matrix-room-id
                           access-token)))
        ;; fetch joined members asynchronously
        (url-retrieve
         url
         (lambda (_status cb-cache cb-cache-key cb-func)
           (goto-char url-http-end-of-headers)
           (when-let* ((json-object-type 'hash-table)
                       (json-array-type 'list)
                       (json-key-type 'string)
                       (json (json-read))
                       (joined-members (gethash "joined" json))) ; skip if joined_members is empty
             (dolist (member (hash-table-keys joined-members))
               (when-let* ((not-t2bot? (not (string-suffix-p ":t2bot.io" member)))
                           (avatar-url (gethash "avatar_url" (gethash member joined-members)))
                           (display-name (gethash "display_name" (gethash member joined-members))))
                 (puthash display-name avatar-url cb-cache)))
             ;; update cache
             (setq telega-bridge-bot--matrix-room-cache
                   (plist-put telega-bridge-bot--matrix-room-cache cb-cache-key cb-cache))
             (setq
              telega-bridge-bot--matrix-room-cache-last-modified-plist
              (plist-put
               telega-bridge-bot--matrix-room-cache-last-modified-plist
               cb-cache-key
               (current-time)))
             (when (functionp cb-func)
               (funcall cb-func cb-cache))))
         (list cache cache-key callback-func)
         'silent)
      ;; use cache
      (funcall callback-func cache))))

(defun telega-bridge-bot--matrix-fetch-user (chat-id msg-id matrix-room-id display-name file-path
                                                     &optional force-update)
  "Fetch user of the CHAT-ID MSG-ID pair in MATRIX-ROOM-ID with DISPLAY-NAME.
User avatar file will be saved to FILE-PATH.
If FORCE-UPDATE is non-nil, force update the file."
  (telega-bridge-bot--matrix-fetch-joined-members
   matrix-room-id
   force-update
   (lambda (members)
     (telega-bridge-bot--matrix-fetch-user-callback
      chat-id
      msg-id
      members
      display-name
      file-path
      force-update))))

(defun telega-bridge-bot--matrix-fetch-user-callback (chat-id msg-id members display-name file-path
                                                              &optional force-update)
  "Fetch user of the CHAT-ID MSG-ID pair in MEMBERS with DISPLAY-NAME.
User avatar file will be saved to FILE-PATH.
If FORCE-UPDATE is non-nil, force update the file."
  (when-let* ((name (telega--desurrogate-apply display-name 'no-props))
              (avatar-url (gethash name members))
              (mxc-id (telega-bridge-bot--matrix-mxc-id avatar-url))
              (url (format telega-bridge-bot--matrix-media-thumbnail-endpoint
                           telega-bridge-bot--matrix-host
                           mxc-id))
              (url-request-extra-headers '(("Accept" . "image/jpeg"))))
    (when (and
           (file-exists-p file-path)
           (or
            force-update
            (time-less-p
             (file-attribute-modification-time (file-attributes file-path))
             (time-subtract (current-time) telega-bridge-bot-matrix-avatar-expires))))
      (delete-file file-path))
    (telega-bridge-bot--download-async
     url
     file-path
     (lambda () (telega-bridge-bot--download-async-callback chat-id msg-id)))))

(defun telega-bridge-bot--matrix-text-spliter (text)
  "Split TEXT into username and message."
  (let* ((name-and-body (split-string text ": "))
         (name (car name-and-body))
         (body (string-join (cdr name-and-body) ": ")))
    (if (and (string-equal name "matterbridge")
             (string-match "^\\[\\(.*?\\)\\]" body))
        ;; get the username in []
        (list
         (concat (match-string 1 body) " <matterbridge>") ; username
         (substring body (+ (match-end 0) 1))) ; message
      (list name body))))

(defun telega-bridge-bot--matrix-file-spliter (text)
  "Split TEXT into username and message."
  ;; Split with file type prefix
  (split-string text " sent \\(a file\\|an image\\|a video\\): "))


;;; bridge

(defun telega-bridge-bot--fetch-user (telega-msg-id telega-chat-id telega-bot-id counterparty-username
                                                    &optional force-update)
  "Fetch user of the TELEGA-MSG-ID.
By TELEGA-CHAT-ID, TELEGA-BOT-ID and COUNTERPARTY-USERNAME.
If FORCE-UPDATE is non-nil, force update the file."
  (when-let* ((counterparty-info (telega-bridge-bot--counterparty-info telega-chat-id telega-bot-id))
              (profile-photo-path
               (telega-bridge-bot--profile-photo-path telega-chat-id telega-bot-id counterparty-username))
              (counterparty-type (plist-get counterparty-info :type))
              (counterparty-chat-id (plist-get counterparty-info :chat-id))
              (fetch-user-function
               (telega--tl-get telega-bridge-bot--counterparty-handler-plist
                               counterparty-type
                               :fetch-user)))
    (funcall
     fetch-user-function
     telega-chat-id telega-msg-id counterparty-chat-id counterparty-username profile-photo-path force-update)))

(defun telega-bridge-bot--counterparty-info (telega-chat-id telega-bot-id)
  "Get the counterparty info from `telega-bridge-bot-bridge-info-plist'.
TELEGA-CHAT-ID is the chat id that TELEGA-BOT-ID is in."
  (plist-get
   (plist-get telega-bridge-bot-bridge-info-plist telega-chat-id)
   telega-bot-id))


;;; telega

(defun telega-bridge-bot--remove-username (fmt-text body)
  "Return FMT-TEXT containing only BODY."
  (telega-fmt-text-substring fmt-text (- (length body))))

(defun telega-bridge-bot--user-id (chat-id bot-id username &optional string)
  "Return bridge user id for CHAT-ID, BOT-ID and USERNAME.
Return a string if STRING is non-nil."
  (let ((id (secure-hash
             'sha256
             (concat
              (number-to-string chat-id) "-"
              (number-to-string bot-id) "-"
              username))))
    (if string id (intern id))))

(defun telega-bridge-bot--profile-photo-path (chat-id bot-id username)
  "Return profile photo path for CHAT-ID, BOT-ID and USERNAME."
  (concat
   (file-name-as-directory (expand-file-name "profile_photos" telega-database-dir))
   "telega-bridge-bot-"
   (telega-bridge-bot--user-id chat-id bot-id username 'string)))

(defun telega-bridge-bot--file-id (path)
  "Return file id based on PATH and modification time seconds."
  (let* ((mtime (file-attribute-modification-time (file-attributes path)))
         (mtime-seconds (float-time mtime))
         (mtime-seconds-string (number-to-string mtime-seconds)))
    (intern (secure-hash 'sha256 (concat mtime-seconds-string "-" path)))))

(defun telega-bridge-bot--user (chat-id bot-id username)
  "Return a user by CHAT-ID BOT-ID and USERNAME."
  (let* ((bot-username
          (telega-msg-sender-username (telega-user-get bot-id)))
         (profile-photo-path
          (telega-bridge-bot--profile-photo-path chat-id bot-id username))
         (profile-photo-id
          (telega-bridge-bot--file-id profile-photo-path))
         (base-user
          (list
           :@type "user"
           :id bot-id
           :telega-bridge-bot-user-signature (list chat-id bot-id username)
           :first_name username
           :last_name ""
           :usernames
           (list
            :@type
            "usernames"
            :active_usernames (vector bot-username))
           :type '(:@type "userTypeRegular"))))
    (if (file-exists-p profile-photo-path)
        (append
         base-user
         (list
          :profile_photo
          (list
           :@type
           "profilePhoto"
           :id profile-photo-id
           :small
           (list
            :@type
            "file"
            :id profile-photo-id
            :size 7460
            :expected_size 7460
            :local
            (list
             :@type
             "localFile"
             :path
             profile-photo-path
             :can_be_downloaded t
             :can_be_deleted t
             :is_downloading_active nil
             :is_downloading_completed t
             :download_offset 0
             :downloaded_prefix_size 7460
             :downloaded_size 7460)
            :telega-file-recency 0))))
      base-user)))

(defun telega-bridge-bot--update-user-info (msg-id chat-id bot-id username &optional force-update)
  "Update user of the MSG-ID.
By CHAT-ID BOT-ID and USERNAME, return bridge sender id.
It will recaculate the profile photo path and file id,
you can run this function after user profile photo file created or changed.
If FORCE-UPDATE is non-nil, force update the user info."
  (let* ((sender-id (telega-bridge-bot--user-id chat-id bot-id username))
         (info-hash (alist-get 'user telega--info))
         (user (gethash sender-id info-hash)))
    (when (or
           force-update
           (not user)
           (not (plist-get user :profile_photo)))
      (telega-bridge-bot--fetch-user msg-id chat-id bot-id username force-update)
      (puthash sender-id (telega-bridge-bot--user chat-id bot-id username) info-hash))
    sender-id))

(defun telega-bridge-bot--download-async-callback (chat-id msg-id)
  "Callback for `telega-bridge-bot--download-async'.
Will update CHAT-ID MSG-ID when download completed."
  (when-let* ((msg (telega-msg-get (list :id chat-id) msg-id))
              (user-outdated (telega-msg-sender msg))
              (user-signature (plist-get user-outdated :telega-bridge-bot-user-signature))
              (user-id (apply 'telega-bridge-bot--user-id user-signature))
              (user (apply 'telega-bridge-bot--user user-signature))) ; recreate user
    (puthash user-id user (alist-get 'user telega--info))
    (telega-msg-redisplay msg)))

(defun telega-bridge-bot--update-fmt-text (msg)
  "Update sender id and remove duplicated username in MSG."
  (when-let* ((msg-id (telega--tl-get msg :id))
              (chat-id (telega--tl-get msg :chat_id))
              (bot-id (telega--tl-get (telega-msg-sender msg) :id))
              (counterparty-info (telega-bridge-bot--counterparty-info chat-id bot-id)) ; check if it is a bridge bot
              (counterparty-type (plist-get counterparty-info :type))
              (content (telega--tl-get msg :content))
              (text-p (eq (telega--tl-type content) 'messageText))
              (content-text (telega--tl-get content :text))
              (fmt-text-p (eq (telega--tl-type content-text) 'formattedText))
              (content-text-text (telega--tl-get content-text :text)) ; get the msg text
              (spliter (telega--tl-get
                        telega-bridge-bot--counterparty-handler-plist
                        counterparty-type :text-spliter))
              (name-and-body (funcall spliter content-text-text))
              (name (car name-and-body))
              (body (cadr name-and-body))) ; skip if no body
    (let ((sender-id (telega-bridge-bot--update-user-info msg-id chat-id bot-id name)))
      ;; replace sender
      (plist-put msg :sender_id (list :@type "messageSenderUser" :user_id sender-id))
      ;; remove duplicated username in body
      ;; we don't use body directly here
      ;; because then we only need to make sure the name is correct
      ;; makes it easier to write the split function
      (plist-put
       content :text
       (telega-bridge-bot--remove-username content-text body)))))

(defun telega-bridge-bot--update-file (msg)
  "Update sender id and remove file caption in MSG."
  (when-let* ((msg-id (telega--tl-get msg :id))
              (chat-id (telega--tl-get msg :chat_id))
              (bot-id (telega--tl-get (telega-msg-sender msg) :id))
              (counterparty-info (telega-bridge-bot--counterparty-info chat-id bot-id)) ; check if it is a bridge bot
              (counterparty-type (plist-get counterparty-info :type))
              (content (telega--tl-get msg :content))
              (content-caption (telega--tl-get content :caption))
              (fmt-text-p (eq (telega--tl-type content-caption) 'formattedText))
              (content-caption-text (telega--tl-get content-caption :text)) ; get the msg text
              (spliter (telega--tl-get
                        telega-bridge-bot--counterparty-handler-plist
                        counterparty-type :file-spliter))
              (name-and-body (funcall spliter content-caption-text))
              (name (car name-and-body))
              (body (cadr name-and-body))) ; skip if no body
    (let ((sender-id (telega-bridge-bot--update-user-info msg-id chat-id bot-id name)))
      ;; replace sender
      (plist-put msg :sender_id (list :@type "messageSenderUser" :user_id sender-id))
      ;; remove caption
      (plist-put content :caption nil))))

(defun telega-bridge-bot--update-forwarded (msg)
  "Update forwarded sender id and remove duplicated username in MSG."
  (when-let* ((msg-id (telega--tl-get msg :id))
              (chat-id (telega--tl-get msg :chat_id))
              (forward-info (telega--tl-get msg :forward_info))
              (fwd-info-p (eq (telega--tl-type forward-info) 'messageForwardInfo))
              (bot-id (telega--tl-get forward-info :origin :sender_user_id))
              (counterparty-info (telega-bridge-bot--counterparty-info chat-id bot-id)) ; check if it is a bridge bot
              (counterparty-type (plist-get counterparty-info :type))
              (content (telega--tl-get msg :content))
              (content-text (telega--tl-get content :text))
              (fmt-text-p (eq (telega--tl-type content-text) 'formattedText))
              (content-text-text (telega--tl-get content-text :text)) ; get the msg text
              (spliter (telega--tl-get
                        telega-bridge-bot--counterparty-handler-plist
                        counterparty-type :text-spliter))
              (name-and-body (funcall spliter content-text-text))
              (name (car name-and-body))
              (body (cadr name-and-body))) ; skip if no body
    (let ((sender-id (telega-bridge-bot--update-user-info msg-id chat-id bot-id name)))
      ;; replace sender
      (plist-put forward-info :origin (list :@type "messageOriginUser" :sender_user_id sender-id))
      ;; remove duplicated username in body
      (plist-put
       content :text
       (telega-bridge-bot--remove-username content-text body)))))

(defun telega-bridge-bot--update-msg (msg &rest _)
  "Replace the sender in MSG with the other party's sender."
  (when-let ((no-modify? (not (telega--tl-get msg :telega-bridge-bot-modified))))
    (let ((update-functions '(telega-bridge-bot--update-fmt-text
                              telega-bridge-bot--update-forwarded
                              telega-bridge-bot--update-file))
          (bridge-sender nil))
      ;; break if one of the update function return non-nil
      (while (and (not bridge-sender) update-functions)
        (setq bridge-sender (funcall (pop update-functions) msg)))
      (when bridge-sender
        (plist-put msg :telega-bridge-bot-modified t)))))

(defun telega-ins--aux-msg-one-line! (fun &rest args)
  "Advice function for `telega-ins--aux-msg-one-line'.
FUN is the original function,
ARGS is the arguments passed the the FUN."
  (let ((msg (car args)))
    ;; update msg first
    (telega-bridge-bot--update-msg msg)
    (if-let* ((modified? (plist-get msg :telega-bridge-bot-modified))
              (sender (telega-msg-sender msg))
              (sender-name
               (concat
                (telega-user-title sender 'full-name) " "
                (telega-user-title sender 'username))))
        ;; if msg sender is a bridge bot then we want to display
        ;; the user title and the username in one line message
        (cl-letf (((symbol-function 'telega-msg-sender-username)
                   (lambda (&rest _) sender-name)))
          (apply fun args))
      (apply fun args))))



(advice-add 'telega-ins--aux-msg-one-line :around #'telega-ins--aux-msg-one-line!)
(advice-add 'telega-chatbuf-msg--pp :before #'telega-bridge-bot--update-msg)
(advice-add 'telega-msg--replied-message-fetch-callback :before
            (lambda (_msg replied-msg)
              (telega-bridge-bot--update-msg replied-msg)))

(provide 'telega-bridge-bot)
;;; telega-bridge-bot.el ends here

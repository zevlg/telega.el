;;; telega-msg.el --- Messages for telega  -*- lexical-binding:t -*-

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri May  4 03:49:22 2018
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

;;

;;; Code:
(require 'telega-core)
(require 'telega-customize)
(require 'telega-ffplay)                ; telega-ffplay-run

(defvar telega-msg-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map [remap self-insert-command] 'ignore)
    (define-key map (kbd "n") 'telega-button-forward)
    (define-key map (kbd "p") 'telega-button-backward)

    (define-key map (kbd "i") 'telega-describe-message)
    (define-key map (kbd "r") 'telega-msg-reply)
    (define-key map (kbd "e") 'telega-msg-edit)
    (define-key map (kbd "f") 'telega-msg-forward)
    (define-key map (kbd "d") 'telega-msg-delete)
    (define-key map (kbd "k") 'telega-msg-delete)
    (define-key map (kbd "l") 'telega-msg-redisplay)
    (define-key map (kbd "R") 'telega-msg-resend)
    (define-key map (kbd "S") 'telega-msg-save)
    (define-key map (kbd "DEL") 'telega-msg-delete)
    map))

(define-button-type 'telega-msg
  :supertype 'telega
  :inserter telega-inserter-for-msg-button
  'read-only t
  'keymap telega-msg-button-map
  'action 'telega-msg-button--action)

(defun telega-msg-button--action (button)
  "Action to take when chat BUTTON is pressed."
  (let* ((msg (button-get button :value))
         (content (plist-get msg :content)))
    (telega--openMessageContent msg)

    ;; TODO: do some useful action to open message
    (cl-case (telega--tl-type content)
      (messageSticker
       (telega-describe-stickerset
        (telega-stickerset-get (telega--tl-get content :sticker :set_id))
        nil (telega-msg-chat msg)))
      (messageVideo
       (let ((video (plist-get content :video)))
         ;; NOTE: `telega-file--download-monitoring' triggers callback
         ;; in case file is already downloaded
         (telega-file--download-monitoring
          video :video 32
          (lambda (file)
            (telega-msg-redisplay msg)
            (when (telega-file--downloaded-p file)
              (apply 'telega-ffplay-run
                     (telega--tl-get file :local :path) nil
                     telega-video-ffplay-args))))))
      (messagePhoto
       ;; TODO: view highres image
       )
      )))

(defun telega-msg--pp (msg)
  "Pretty printer for MSG button."
  (telega-button--insert 'telega-msg msg)
  (telega-ins "\n"))

(defun telega-msg-root--pp (msg)
  "Pretty printer for MSG button shown in root buffer."
  (let ((visible-p (telega-filter-chats nil (list (telega-msg-chat msg)))))
    (when visible-p
      (telega-button--insert 'telega-msg msg
        :inserter 'telega-ins--root-msg
        :action 'telega-msg-goto)
      (telega-ins "\n"))))

(defun telega-msg--get (chat-id msg-id)
  "Get message by CHAT-ID and MSG-ID pair."
  ;; Optimisation for formatting messages with reply
  (or (with-telega-chatbuf (telega-chat-get chat-id)
        (gethash msg-id telega-chatbuf--messages))

      (let ((reply (telega-server--call
                    (list :@type "getMessage"
                          :chat_id chat-id
                          :message_id msg-id))))
        ;; Probably message already deleted
        (unless (eq (telega--tl-type reply) 'error)
          reply))))

(defsubst telega-msg-list-get (tl-obj-Messages)
  "Return messages list of TL-OBJ-MESSAGES represeting `Messages' object."
  (mapcar #'identity (plist-get tl-obj-Messages :messages)))

(defun telega-msg-at (&optional pos)
  "Return current message at point."
  (let ((button (button-at (or pos (point)))))
    (when (and button (eq (button-type button) 'telega-msg))
      (button-get button :value))))

(defsubst telega-msg-chat (msg)
  "Return chat for the MSG."
  (telega-chat-get (plist-get msg :chat_id)))

(defun telega-msg-reply-msg (msg)
  "Return message MSG replying to."
  (let ((reply-to-msg-id (plist-get msg :reply_to_message_id)))
    (unless (zerop reply-to-msg-id)
      (telega-msg--get (plist-get msg :chat_id) reply-to-msg-id))))

(defsubst telega-msg-goto (msg &optional highlight)
  "Goto message MSG."
  (telega-chat--goto-msg
   (telega-msg-chat msg) (plist-get msg :id) highlight))

(defun telega--openMessageContent (msg)
  "Open content of the message MSG."
  (telega-server--send
   (list :@type "openMessageContent"
         :chat_id (plist-get msg :chat_id)
         :message_id (plist-get msg :id))))

(defun telega--getPublicMessageLink (chat-id msg-id &optional for-album)
  "Get https link to public message."
  (telega-server--call
   (list :@type "getPublicMessageLink"
         :chat_id chat-id
         :message_id msg-id
         :for_album (or for-album :false))))

(defun telega--deleteMessages (chat-id message-ids &optional revoke)
  "Delete message by its id"
  (telega-server--send
   (list :@type "deleteMessages"
         :chat_id chat-id
         :message_ids (cl-map 'vector 'identity message-ids)
         :revoke (or revoke :false))))

(defun telega--forwardMessages (chat-id from-chat-id message-ids
                                        &optional disable-notification
                                        from-background as-album)
  "Forwards previously sent messages.
Returns the forwarded messages.
Return nil if message can't be forwarded."
  (error "`telega--forwardMessages' Not yet implemented"))

(defun telega--searchMessages (query last-msg &optional callback)
  "Search messages by QUERY.
Specify LAST-MSG to continue searching from LAST-MSG searched.
If CALLBACK is specified, then do async call and run CALLBACK
with list of chats received."
  (let ((ret (telega-server--call
              (list :@type "searchMessages"
                    :query query
                    :offset_date (or (plist-get last-msg :date) 0)
                    :offset_chat_id (or (plist-get last-msg :chat_id) 0)
                    :offset_message_id (or (plist-get last-msg :id) 0)
                    :limit 100)
              (and callback
                   `(lambda (reply)
                      (funcall ',callback (telega-msg-list-get reply)))))))
      (if callback
          ret
        (telega-msg-list-get ret))))

(defun telega-file--update-msg (file msg)
  "Callback for downloading/uploading the FILE'.
Update message as file downloading/uploading progresses."
  ;; NOTE: MSG's place of FILE already has been uploaded, so we need
  ;; just to redisplay the MSG
  (with-telega-chat-buffer (telega-chat-get (plist-get msg :chat_id))
    (let ((node (telega-chatbuf--node-by-msg-id (plist-get msg :id))))
      (when node
        (ewoc-invalidate telega-chatbuf--ewoc node)))))

(defun telega-msg-chat-title (msg)
  "Title of the message's chat."
  (telega-chat-title (telega-msg-chat msg) 'with-username))

(defsubst telega-msg-sender (msg)
  "Return sender (if any) for message MSG."
  (let ((sender-uid (plist-get msg :sender_user_id)))
    (unless (zerop sender-uid)
      (telega-user--get sender-uid))))

(defsubst telega-msg-by-me-p (msg)
  "Return non-nil if sender of MSG is me."
  (= (plist-get msg :sender_user_id) telega--me-id))

;; DEPRECATED
(defun telega-msg-sender-shortname (msg &optional suffix)
  "Short name of the MSG sender."
  (let ((sender-uid (plist-get msg :sender_user_id)))
    (unless (zerop sender-uid)
      (concat (telega-user--name (telega-user--get sender-uid) 'short)
              (or suffix "")))))

;; DEPRECATED
(defun telega-msg-sender-name (msg)
  "Title of the MSG sender."
  (let ((sender-uid (plist-get msg :sender_user_id)))
    (unless (zerop sender-uid)
      (telega-user--name (telega-user--get sender-uid)))))

;; DEPRECATED
(defun telega-msg-sender-ava-h (msg)
  "High stripe of the MSG sender's avatar."
  ;; TODO: sender avatar high stripe
  (let ((sender-uid (plist-get msg :sender_user_id)))
    (unless (zerop sender-uid)
      (let* ((user (telega-user--get sender-uid))
             (ufn (telega--desurrogate-apply (plist-get user :first_name)))
             (uln (telega--desurrogate-apply (plist-get user :last_name)))
             (res ""))
        (unless (string-empty-p ufn)
          (setq res (capitalize (substring ufn 0 1))))
        (unless (string-empty-p uln)
          (setq res (concat res (capitalize (substring uln 0 1)))))
        (when (= 1 (length res))
          (setq res (concat res res)))
        (when (and (string-empty-p res)
                   (eq (telega-user--type user) 'deleted))
          (setq res "DU"))
        (concat "(" res ")")))))

(defun telega-msg-sender-ava-l (msg)
  "Low stripe of the MSG sender's avatar."
  ;; TODO: sender avatar low stripe
  (let ((sender-uid (plist-get msg :sender_user_id)))
    (unless (zerop sender-uid)
      "    ")))

(defun telega-msg-caption (msg)
  "Display MSG's caption if any.
PREFIX and SUFFIX specifies addons in case caption is used."
  (let ((cap (telega--tl-get msg :content :caption)))
    (telega-msg--ents-to-props
     (plist-get cap :text) (plist-get cap :entities))))

(defun telega-msg-web-page (msg)
  "Format webPage embedded into MSG."
  (let* ((web-page (telega--tl-get msg :content :web_page))
         (sitename (plist-get web-page :site_name))
         (title (plist-get web-page :title))
         (desc (plist-get web-page :description))
         (instant-view-p (plist-get web-page :has_instant_view))
         (photo (plist-get web-page :photo)))
    (when web-page
      (concat
       (unless (string-empty-p sitename)
         (concat "\n" telega-symbol-vertical-bar
                 (propertize sitename 'face 'telega-webpage-sitename)))
       (unless (string-empty-p title)
         (concat "\n" telega-symbol-vertical-bar
                 (propertize title 'face 'telega-webpage-title)))
       (unless (string-empty-p desc)
         (telega-fmt-eval
          `("\n"
            ,telega-symbol-vertical-bar
            (identity :fill left
                      :fill-prefix ,telega-symbol-vertical-bar
                      :fill-column ,(- fill-column 10)))
          desc))
       (when photo
         (concat "\n" telega-symbol-vertical-bar
                 (telega-photo-format (plist-get web-page :photo))))
       (cl-case (intern (plist-get web-page :type))
         (photo
          ;; no-op, already displayed above
          )
         (article
          ;; nothing to display
          )
         (t (concat "\n" telega-symbol-vertical-bar
                    "<unsupported webPage:"
                    (plist-get web-page :type) ">")))
       (when instant-view-p
         (concat "\n" "[  " telega-symbol-thunder
                 "INSTANT VIEW"
                 "  ]"))
       ))))

(defun telega-msg-reply-markup (msg)
  "Format :reply_markup for the message MSG."
  (let ((reply-markup (plist-get msg :reply_markup)))
    (when reply-markup
      (concat "\n" "<unsupported replyMarkup:"
              (substring (plist-get reply-markup :@type) 11) ">"))
    ))

(defun telega-msg-photo-one-line (msg)
  "Format photo in MSG as one-line."
  (let* ((thumb (telega-photo--lowres
                 (telega--tl-get msg :content :photo)))
         (thumb-file (plist-get thumb :photo))
         (thumb-img (and thumb-file
                         (apply #'telega-photo-file-format
                                thumb-file
                                'one-line
                                telega-msg-photo-props))))
    (concat telega-symbol-photo " " thumb-img " " (telega-msg-caption msg))))

(defun telega-msg-photo (msg)
  "Format photo message."
  (cl-assert (eq (telega--tl-type (plist-get msg :content)) 'messagePhoto))

  (let* ((thumb (telega-photo--best (telega--tl-get msg :content :photo)))
         (thumb-file (plist-get thumb :photo))
         (thumb-path (telega--tl-get thumb-file :local :path)))
    (concat telega-symbol-photo " "
            ;; Photo itself or downloading progress
            (if (telega-file--downloaded-p thumb-file)
                (concat (apply 'propertize (telega-short-filename thumb-path)
                               (telega-link-props 'file thumb-path))
                        (format " (%dx%d)\n"
                                (plist-get thumb :width)
                                (plist-get thumb :height))
                        (apply #'telega-photo-file-format
                               thumb-file
                               nil
                               telega-msg-photo-props))

              (unless (telega-file--downloading-p thumb-file)
                (telega-file--download
                 (plist-get thumb-file :id) nil #'telega-file--update-msg msg))

              (telega-msg-downloading-progress msg thumb-file))

            ;; Photo caption
            (telega-prefix "\n" (telega-msg-caption msg)))))

(defun telega-msg-downloading-progress (msg file)
  "For message MSG format local FILE downloading progress."
  (let ((filesize (plist-get file :size))
        (local (plist-get file :local)))
    (let* ((dsize (plist-get local :downloaded_size))
           (dpart (/ (float dsize) filesize))
           (percents (round (* (/ (float dsize) filesize) 100))))
      (concat
       (format "[%-10s%d%%]"
               (make-string (round (* dpart 10)) ?\.)
               (round (* dpart 100)))
       " "
       (apply 'propertize "[Cancel]"
              (telega-link-props
               'cancel-download
               (list (plist-get file :id)
                     #'telega-file--update-msg
                     msg)))))))

(defun telega-msg-uploading-progress (msg file)
  "For message MSG format remote FILE uploading progress."
  (let ((filesize (plist-get file :size))
        (remote (plist-get file :local)))
    (let* ((usize (plist-get remote :uploaded_size))
           (upart (/ (float dsize) filesize))
           (percents (round (* (/ (float dsize) filesize) 100))))
      (concat
       (format "[%-10s%d%%]"
               (make-string (round (* upart 10)) ?\.)
               (round (* upart 100)))
       " "
       (apply 'propertize "[Cancel]"
              (telega-link-props
               'cancel-upload
               (list (plist-get file :id)
                     #'telega-file--update-msg
                     msg)))))))

;; DEPRECATED
(defun telega-msg-document (msg)
  "Format document of the message."
  (cl-assert (eq (telega--tl-type (plist-get msg :content)) 'messageDocument))

  (let* ((document (telega--tl-get msg :content :document))
         (file (plist-get document :document)))
    (concat telega-symbol-attachment " "
            (plist-get document :file_name)
            " (" (file-size-human-readable (plist-get file :size)) ") "

            ;; File status:
            ;;   /link/to-file         if file has been uploaded/downloaded
            ;;   [...   20%] [Cancel]  if upload/download in progress
            ;;   [Download]            if no local copy
            (cond ((telega-file--uploading-p file)
                   (telega-fmt--file-progress file 'upload))
                  ((telega-file--downloading-p file)
                   (telega-fmt--file-progress file 'download))
                  ((telega-file--downloaded-p file)
                   (let ((file-path (telega--tl-get file :local :path)))
                     (apply 'propertize (telega-short-filename file-path)
                            (telega-link-props 'file file-path))))
                  (t (apply 'propertize "[Download]"
                            (telega-link-props
                             'download
                             document :document msg))))

            ;; Caption
            (telega-prefix "\n" (telega-msg-caption msg)))))

;; DEPRECATED
(defun telega-msg-text-one-line (msg)
  "Format message text for one line formatting.
Format without rendering web-page."
  (cl-assert (eq (telega--tl-type (plist-get msg :content)) 'messageText))

  (let ((text (telega--tl-get msg :content :text)))
    (telega-msg--ents-to-props
     (plist-get text :text)
     (plist-get text :entities))))

;; DEPRECATED
(defun telega-msg-text (msg)
  "Format text of the message MSG."
  (concat
   (telega-msg-text-one-line msg)
   (telega-msg-web-page msg)))

;; DEPRECATED
(defun telega-msg-format (msg)
  "Return formatted text for the MSG."
  (concat
   (let ((content (plist-get msg :content)))
     (cl-case (telega--tl-type content)
       (messageText
        (telega-msg-text msg))
       (messageDocument
        (telega-msg-document msg))
       (messagePhoto
        (telega-msg-photo msg))
       (t (concat (format "<unsupported message %S>" (telega--tl-type content))
                  (telega-prefix "\n" (telega-msg-caption msg))))))

   (telega-msg-reply-markup msg)))

;; DEPRECATED
(defun telega-msg-format-one-line (msg)
  "Format MSG's text as one line."
  (replace-regexp-in-string
   "\n" " " (cl-case (telega--tl-type (plist-get msg :content))
              (messageText
               (telega-msg-text-one-line msg))
              (messagePhoto
               (telega-msg-photo-one-line msg))
              (t (telega-msg-format msg)))))

(defun telega-msg-sender-admin-status (msg)
  (let ((admins-tl (telega-server--call
                    (list :@type "getChatAdministrators"
                          :chat_id (plist-get msg :chat_id)))))
    (when (cl-find (plist-get msg :sender_user_id)
                   (plist-get admins-tl :user_ids)
                   :test #'=)
      " (admin)")))

(defun telega-msg-sender-status (msg)
  "Format MSG sender status.
Makes heavy online requests without caching, be carefull."
  (let ((ucm (telega-server--call
              (list :@type "getChatMember"
                    :chat_id (plist-get msg :chat_id)
                    :user_id (plist-get msg :sender_user_id)))))
    (telega-fmt-chat-member-status (plist-get ucm :status))))

;; DEPRECATED
(defun telega-msg-text-with-timestamp (msg fill-prefix)
  `((telega-msg-format
     :fill left
     :fill-prefix ,fill-prefix
     :fill-column ,telega-chat-fill-column
     :min ,telega-chat-fill-column
     :align left)
    (telega-msg-timestamp :align right :min 10)
    (telega-msg-outgoing-status :face telega-msg-outgoing-status)
    "\n"))

;; DEPRECATED
(defun telega-msg-button--format-same (msg)
  "Fromat message when previous msg is from the same sender."
  `(,telega-msg-full-prefix
    ,@(telega-msg-inline-reply msg telega-msg-full-prefix)
    ,@(telega-msg-text-with-timestamp msg telega-msg-full-prefix)))

;; DEPRECATED
(defun telega-msg-button--format-action (msg)
  "Format chat's action message, such as messageChatAddMembers."
  (let ((content (plist-get msg :content))
        (sender (telega-user--get (plist-get msg :sender_user_id))))
    `((("--("
        ,(cl-case (telega--tl-type content)
           (messageContactRegistered
            (concat (telega-user--name sender)
                    " joined the Telegram"))
           (messageChatAddMembers
            ;; If sender matches
            (let ((user-ids (plist-get content :member_user_ids)))
              (if (and (= 1 (length user-ids))
                       (= (plist-get sender :id) (aref user-ids 0)))
                  (concat
                   (telega-user--name sender 'name)
                   " joined the group")
                (concat
                 (telega-user--name sender 'name)
                 " invited "
                 (mapconcat 'telega-user--name
                            (mapcar 'telega-user--get user-ids)
                            ", ")))))
           (messageChatJoinByLink
            (concat (telega-user--name sender)
                    " joined the group via invite link"))
           (messageChatDeleteMember
            (concat (telega-user--name
                     (telega-user--get (plist-get content :user_id)))
                    " left the group"))
           (messageChatChangeTitle
            (concat (telega-user--name sender)
                    " renamed group to \"" (plist-get content :title) "\""))
           (t "<unsupported chat action: %S>" (telega--tl-type content)))
        ")--")
       :min ,telega-chat-fill-column
       :align center
       :align-symbol "-")
      (telega-msg-timestamp :align right :min 10)
      "\n")))

;; DEPRECATED
(defun telega-msg-button--format (msg &optional prev-msg)
  "Produce fmt spec for the message MSG.
PREV-MSG is non-nil if there any previous message exists."
  (cond ((plist-get msg :is_channel_post)
         (telega-msg-button--format-channel msg))

        ((memq (telega--tl-type (plist-get msg :content))
               (list 'messageChatAddMembers 'messageChatJoinByLink
                     'messageChatDeleteMember 'messageChatChangeTitle
                     'messageContactRegistered))
         (telega-msg-button--format-action msg))

        (t (if (and prev-msg
                    (zerop (plist-get msg :edit_date))
                    (= (plist-get msg :sender_user_id)
                       (plist-get prev-msg :sender_user_id))
                    (string= (telega-msg-via-bot msg)
                             (telega-msg-via-bot prev-msg)))
               (telega-msg-button--format-same msg)
             (telega-msg-button--format-full msg)))))

(defsubst telega-msg-button--format-aux (title &optional msg)
  `((("| " ,title ": "
      ,(telega-msg-sender-shortname msg "> ")
      ,(telega-msg-format-one-line msg))
     :max ,telega-chat-fill-column
     :elide t
     :face telega-chat-prompt)
    "\n"))

(defun telega-msg-button--format-aux-reply (msg)
  (telega-msg-button--format-aux "Reply" msg))

(defun telega-msg-button--format-aux-edit (msg)
  (telega-msg-button--format-aux "Edit" msg))

(defun telega-msg--entity-to-properties (entity text)
  (let ((ent-type (plist-get entity :type)))
    (cl-case (telega--tl-type ent-type)
      (textEntityTypeMention
       (list 'face 'telega-entity-type-mention))
      (textEntityTypeMentionName
       (telega-link-props 'user (plist-get ent-type :user_id)
                          'telega-entity-type-mention))
      (textEntityTypeHashtag
       (telega-link-props 'hashtag text))
      (textEntityTypeBold
       (list 'face 'telega-entity-type-bold))
      (textEntityTypeItalic
       (list 'face 'telega-entity-type-italic))
      (textEntityTypeCode
       (list 'face 'telega-entity-type-code))
      (textEntityTypePre
       (list 'face 'telega-entity-type-pre))
      (textEntityTypePreCode
       (list 'face 'telega-entity-type-pre))

      (textEntityTypeUrl
       (telega-link-props 'url text 'telega-entity-type-texturl))
      (textEntityTypeTextUrl
       (telega-link-props 'url (plist-get ent-type :url)
                          'telega-entity-type-texturl))
      )))

(defun telega-msg--ents-to-props (text entities)
  "Convert message TEXT with text ENTITIES to propertized string."
  (mapc (lambda (ent)
          (let* ((beg (plist-get ent :offset))
                 (end (+ (plist-get ent :offset) (plist-get ent :length)))
                 (props (telega-msg--entity-to-properties
                         ent (substring text beg end))))
            (when props
              (add-text-properties beg end props text))))
        entities)
  text)

(defun telega--formattedText (text &optional markdown)
  "Convert TEXT to `formattedTex' type.
If MARKDOWN is non-nil then format TEXT as markdown."
  (if markdown
      (telega-server--call
       (list :@type "parseTextEntities"
             :text text
             :parse_mode (list :@type "textParseModeMarkdown")))
    (list :@type "formattedText"
          :text text :entities [])))


(defun telega-msg-save (msg)
  "Save messages's MSG media content to a file."
  (interactive (list (telega-msg-at (point))))
  (let ((content (plist-get msg :content)))
    (cl-case (telega--tl-type content)
      (t (error "TODO: `telega-msg-save'")))))

(defun telega-describe-message (msg)
  "Show info about message at point."
  (interactive (list (telega-msg-at (point))))
  (with-telega-help-win "*Telegram Message Info*"
    (let ((chat-id (plist-get msg :chat_id))
          (msg-id (plist-get msg :id)))
      (telega-ins "Date(ISO8601): ")
      (telega-ins--date-iso8601 (plist-get msg :date) "\n")
      (telega-ins-fmt "Chat-id: %d\n" chat-id)
      (telega-ins-fmt "Message-id: %d\n" msg-id)
      (let ((sender-uid (plist-get msg :sender_user_id)))
        (unless (zerop sender-uid)
          (telega-ins "Sender: ")
          (insert-text-button (telega-user--name (telega-user--get sender-uid))
                              :telega-link (cons 'user sender-uid))
          (telega-ins "\n")))
      (when (telega-chat--public-p (telega-chat-get chat-id) 'supergroup)
        (let ((link (plist-get (telega--getPublicMessageLink chat-id msg-id) :link)))
          (telega-ins "Link: ")
          (insert-text-button link :telega-link (cons 'url link)
                              'action 'telega-open-link-action)
          (telega-ins "\n")))

      (when telega-debug
        (telega-ins-fmt "MsgSexp: (telega-msg--get %d %d)\n" chat-id msg-id))

      (when telega-debug
        (telega-ins-fmt "\nMessage: %S\n" msg))
      )))



(provide 'telega-msg)

;;; telega-msg.el ends here

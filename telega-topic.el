;;; telega-topic.el --- Forums support for telega  -*- lexical-binding: t -*-

;; Copyright (C) 2022 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sun Nov  6 02:12:07 2022
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
;; Telegram allows creating forums with multiple distinct topics.  Use
;; {{{kbd(M-x telega-chat-create RET forum RET)}}} to create forums.
;; 
;; NOTE: forums and topics are not fully supported by =telega= at moment.

;;; Code:
(require 'telega-core)

(declare-function telega-chat--mark-dirty "telega-tdlib-events" (chat &optional event))
(declare-function telega-topic-button-action "telega-root" (topic))

(defvar telega-topic--default-icons nil
  "Cached list of topic icons which can be used by all users.")

(defun telega-forum-topic-icon-custom-emoji-id (forum-topic)
  "Return custom emoji id for the FORUM-TOPIC."
  (let ((cid (telega--tl-get forum-topic :info :icon :custom_emoji_id)))
    (unless (equal "0" cid)
      cid)))

(defun telega-forum-topic-color (forum-topic)
  "Return color of the FORUM-TOPIC icon."
  (format "#%06x" (telega--tl-get forum-topic :info :icon :color)))

(defun telega-forum-topic-avatar-image (topic &optional cheight)
  "Return avatar image for the TOPIC."
  (if-let* ((cid (telega-forum-topic-icon-custom-emoji-id topic))
            (sticker (telega-custom-emoji-get cid)))
      (telega-sticker--image sticker)

    ;; Fallback to svg icon
    (let* ((cheight (or cheight 1))
           (xh (telega-chars-xheight 1))
           (xw (telega-chars-xwidth 2))
           (svg (telega-svg-create xw xh))
           (title (telega-tl-str (plist-get topic :info) :name))
           (general-p (telega-topic-match-p topic 'is-general))
           (badge (if general-p "#" (substring title 0 1)))
           (font-size (if general-p xh (/ xh 2))))
      (unless general-p
        ;; Draw topic icon
        (let* ((color (telega-forum-topic-color topic))
               (co (telega-color-name-as-hex-2digits
                    (color-darken-name color 40)))
               (c1 (telega-color-name-as-hex-2digits
                    (color-darken-name color 15)))
               (c2 (telega-color-name-as-hex-2digits
                    (color-darken-name color 30))))
          (svg-gradient svg "cgrad" 'linear (list (cons 0 c1) (cons xh c2)))
          (telega-svg-forum-topic-icon svg xw
            :stroke-width (/ xh 20.0)
            :stroke-color co
            :gradient "cgrad")))

      (svg-text svg badge
                :font-size font-size
                :font-weight "bold"
                :fill (if general-p
                          (telega-color-name-as-hex-2digits
                           (face-foreground 'telega-shadow nil t))
                        "white")
                :font-family "monospace"
                :x "50%"
                :text-anchor "middle"
                ;; XXX insane Y calculation
                :y (+ (/ font-size 3) (/ xw 2)))

      (telega-svg-image svg
        :scale 1.0
        :width (telega-cw-width (* 2 cheight))
        :max-height (telega-ch-height cheight)
        :ascent 'center
        :mask 'heuristic))))

(defun telega-topic-notification-setting (topic setting)
  (telega-chat-notification-setting (telega-topic-chat topic) setting topic))

(defun telega-topic-muted-p (topic)
  "Return non-nil if TOPIC is muted."
  (not (telega-zerop (telega-topic-notification-setting topic :mute_for))))

(defun telega--MessageTopic-id (msg-topic)
  "Return id of the MessageTopic MSG-TOPIC."
  (cl-ecase (telega--tl-type msg-topic)
    (messageTopicThread
     (plist-get msg-topic :message_thread_id))
    (messageTopicForum
     (plist-get msg-topic :forum_topic_id))
    (messageTopicDirectMessages
     (plist-get msg-topic :direct_messages_chat_topic_id))
    (messageTopicSavedMessages
     (plist-get msg-topic :saved_messages_topic_id))))

(defun telega-topic-id (topic)
  "Return TOPIC's id."
  (cl-ecase (telega--tl-type topic)
    (messageThreadInfo
     (plist-get topic :message_thread_id))
    ((savedMessagesTopic directMessagesChatTopic)
     (plist-get topic :id))
    (forumTopic
     (telega--tl-get topic :info :forum_topic_id))))

(defun telega-topic-chat-id (topic)
  "Return TOPIC's chat id."
  (cl-ecase (telega--tl-type topic)
    (savedMessagesTopic
     telega--me-id)
    ((messageThreadInfo directMessagesChatTopic)
     (plist-get topic :chat_id))
    (forumTopic
     (telega--tl-get topic :info :chat_id))))

(defun telega-topic--MessageSource (topic)
  "Return MessageSource structure for the TOPIC."
  (cl-ecase (telega--tl-type topic)
    (messageThreadInfo
     '(:@type "messageSourceMessageThreadHistory"))
    (savedMessagesTopic
     '(:@type "messageSourceOther"))
    (directMessagesChatTopic
     '(:@type "messageSourceDirectMessagesChatTopicHistory"))
    (forumTopic
     '(:@type "messageSourceForumTopicHistory"))))

(defun telega-topic-chat (topic)
  "Return chat for the TOPIC."
  (or (plist-get topic :telega-chat)
      (telega-chat-get (telega-topic-chat-id topic))))

(defun telega-topic-get (chat topic-id)
  "Return topic by TOPIC-ID in the CHAT."
  (alist-get topic-id (telega-chat-topics-alist chat)))

(defun telega-topic-brackets (topic)
  "Return pair of brackets to use for TOPIC."
  (or (when (eq 'savedMessagesTopic (telega--tl-type topic))
        (let ((smtype (plist-get topic :type)))
          (when (eq 'savedMessagesTopicTypeSavedFromChat
                    (telega--tl-type smtype))
            (telega-msg-sender-brackets
             (telega-chat-get (plist-get smtype :chat_id) 'offline)))))
      telega-topic-brackets))

(defun telega-topic--ensure (topic &optional chat)
  "Ensure TOPIC for CHAT is stored in the `telega--chat-topics'."
  (unless chat
    (setq chat (telega-topic-chat topic)))

  (if-let ((existing-topic (telega-topic-get chat (telega-topic-id topic))))
      ;; Update topic inplace
      (setcdr existing-topic (cdr topic))
    (setf (alist-get (telega-topic-id topic) (telega-chat-topics-alist chat))
          topic))

  (when (eq 'forumTopic (telega--tl-type topic))
    (telega-chat--forum-topics-icons-fetch chat topic))

  ;; Store back reference to chat in the `:telega-chat' property
  (plist-put topic :telega-chat chat)
  topic)

(defun telega-chat--forum-topics-fetch (chat &optional callback)
  "Fetch CHAT topics in async manner."
  (declare (indent 1))
  (let ((cb (lambda (reply)
              (plist-put chat :telega_topics_count
                         (plist-get reply :total_count))
              (seq-doseq (topic (plist-get reply :topics))
                (telega-topic--ensure topic))

              (telega-chat--mark-dirty chat 'topics)
              (when callback
                (funcall callback reply)))))
    (telega--getForumTopics chat ""
      :callback cb)))

(defun telega-chat--forum-topics-icons-fetch (chat &rest forum-topics)
  "Asynchronously fetch icons for the list of the FORUM-TOPICS."
  (when-let ((custom-emoji-ids
              ;; NOTE: Fetch only uncached custom emojis
              (seq-remove (lambda (cid)
                            (or (equal "0" cid) (telega-custom-emoji-get cid)))
                          (mapcar (telega--tl-prop :info :icon :custom_emoji_id)
                                  forum-topics))))
    (telega--getCustomEmojiStickers custom-emoji-ids
      (lambda (stickers)
        (seq-doseq (sticker stickers)
          (telega-custom-emoji--ensure sticker))

        (telega-chat--mark-dirty chat)))
    ))

(defun telega-chat--topics-load (chat &optional callback)
  "Load topics for the CHAT in async manner."
  (cond ((telega-chat-match-p chat 'saved-messages)
         (telega--loadSavedMessagesTopics
             (plist-get telega--options
                        :pinned_saved_messages_topic_count_max)
           callback))
        ((telega-chat-match-p chat 'is-direct-messages-group)
         (telega--loadDirectMessagesChatTopics chat
           nil callback))
        ((telega-chat-match-p chat 'is-forum)
         (telega-chat--forum-topics-fetch chat callback)
         )))

(defun telega-msg-topic (msg)
  "Return topic for the message MSG."
  (when-let ((msg-topic (plist-get msg :topic_id)))
    (telega-topic-get (telega-msg-chat msg)
                      (telega--MessageTopic-id msg-topic))))

(defun telega-topic-at (&optional pos)
  "Return topic at point POS."
  (let ((button (button-at (or pos (point)))))
    (when (and button (eq (button-type button) 'telega-topic))
      (button-get button :value))))

(defun telega-topic-goto (topic &optional start-msg-id)
  "Open TOPIC in a chatbuf.
If START-MSG-ID is specified, jump to the this message in the topic."
  (let* ((topic-chat (telega-topic-chat topic))
         (buffer (telega-chatbuf--get-create topic-chat :no-history)))
    ;; NOTE: pop to buffer after starting filtering by topic, to make
    ;; `telega-root--keep-cursor-at-chat' do the job for keeping
    ;; cursor at topic position
    (with-current-buffer buffer
      (telega-chatbuf-filter-by-topic topic start-msg-id))

    (telega-chat--pop-to-buffer topic-chat :no-history)))

(defun telega-describe-topic (topic)
  "Show info about TOPIC."
  (interactive (list (telega-topic-at (point))))
  (with-telega-help-win "*Telegram Topic Info*"
    (let ((chat (telega-topic-chat topic))
          (topic-info (plist-get topic :info)))
      (telega-ins-describe-item (telega-i18n "lng_forum_topic_title")
        (telega-ins--with-face 'telega-shadow
          (telega-ins (telega-symbol 'topic))
          (telega-ins--topic-title topic
            :with-icon-p t
            :with-maybe-pin-p t))
        (telega-ins " ")
        ;; TODO: [Open] button
        ;; (telega-ins--box-button "Open"
        ;;                     )
        )
      (telega-ins-describe-item "Chat"
        (telega-button--insert 'telega-chat chat
          :inserter #'telega-ins--chat
          :action #'telega-chat-button-action))
      (telega-ins-describe-item (telega-i18n "lng_link_header_short")
        (let* ((msg-link (telega--getForumTopicLink chat topic))
               (link-url (telega-tl-str msg-link :link)))
          (telega-ins--raw-button
              (telega-link-props 'url link-url 'face 'telega-link)
            (telega-ins link-url))))
      (telega-ins-describe-item "Created"
        (telega-ins--date (plist-get topic-info :creation_date) 'date-time))
      (telega-ins-describe-item (telega-i18n "lng_topic_author_badge")
        (telega-ins--msg-sender
            (telega-msg-sender (plist-get topic-info :creator_id))
          :with-avatar-p t
          :with-username-p t
          :with-brackets-p t))
      (telega-ins-describe-item "Last-Read-Outbox"
        (telega-ins-fmt "%S" (plist-get topic :last_read_outbox_message_id)))
      
      ;; TODO: more fields

      (when (and (listp telega-debug) (memq 'info telega-debug))
        (let ((print-length nil))
          (telega-ins "\n---DEBUG---\n")
          (telega-ins-fmt "TopicSexp: (telega-topic-get (telega-chat-get %d) %d)\n"
            (plist-get chat :id) (telega-topic-id topic))
          ))
      )))

(defun telega-msg-show-topic-info (msg)
  "Show MSG's topic info."
  (interactive (list (telega-msg-for-interactive)))
  (telega-describe-topic (telega-msg-topic msg)))


;;; Topic button
(defvar telega-topic-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "i") 'telega-describe-topic)
    (define-key map (kbd "h") 'telega-describe-topic)
    map)
  "The key map for telega topic buttons.")

(define-button-type 'telega-topic
  :supertype 'telega
  :inserter telega-inserter-for-topic-button
  :action #'telega-topic-button-action
  'keymap telega-topic-button-map)

(provide 'telega-topic)

;;; telega-topic.el ends here

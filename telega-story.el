;;; telega-story.el --- Support for native Telegram stories.  -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Jul 21 12:40:48 2023
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

(declare-function telega-root-view--update "telega-root" (on-update-prop &rest args))

(defun telega-story--ensure (story &optional no-root-update)
  "Ensure STORY being cached."
  (when telega-debug
    (cl-assert story))
  (puthash (cons (plist-get story :sender_chat_id)
                 (plist-get story :id))
           story telega--cached-stories)

  (unless no-root-update
    (telega-root-view--update :on-story-update story))
  story)

(defun telega-story-deleted-p (story)
  "Return non-nil if STORY has been expired or deleted."
  (or (telega--tl-error-p story)
      (plist-get story :telega-is-deleted-story)))

(gv-define-setter telega-story-deleted-p (value story)
  "Mark story as deleted."
  `(plist-put ,story :telega-is-deleted-story ,value))

(defun telega-story-get (chat-id story-id &optional offline-p)
  "Get story by CHAT-ID and STORY-ID.
If OFFLINE-P is non-nil then do not request the telega-server."
  (let ((story (gethash (cons chat-id story-id) telega--cached-stories)))
    (when (and (not story) (not offline-p))
      (setq story (telega--getStory chat-id story-id))
      (cl-assert story nil "getStory timed out chat_id/story_id=%d/%d"
                 chat-id story-id))
    story))

(defun telega-msg-story-get (msg &optional callback)
  "Return story associated with the message MSG.
If CALLBACK is not specified, then do not perform request to
telega-server, check only in messages cache.  If CALLBACK
is specified, it should accept two argument s - MESSAGE and
optional OFFLINE-P, non-nil OFFLINE-P means no request to the
telega-server has been made."
  ;; Story could be placed in the message itself, or might be a part
  ;; of web_page
  (let ((content (plist-get msg :content))
        chat-id story-id)
    (cl-case (telega--tl-type content)
      (messageStory
       (setq chat-id (plist-get content :story_sender_chat_id)
             story-id (plist-get content :story_id)))
      (messageText
       (let ((web-page (plist-get content :web_page)))
         (setq chat-id (plist-get web-page :story_sender_chat_id)
               story-id (plist-get web-page :story_id)))))
    (unless (or (telega-zerop chat-id) (telega-zerop story-id))
      (let ((story (telega-story-get chat-id story-id 'offline)))
        (if (or story (null callback))
            (if callback
                (funcall callback story 'offline-p)
              story)

          (cl-assert callback)
          (telega--getStory chat-id story-id nil callback))))))

(defun telega-msg--story-fetch (msg)
  "Fetch story associated with the message MSG."
  (telega-msg-story-get
   msg (apply-partially #'telega-msg--story-fetch-callback msg)))

(defun telega-msg--story-fetch-callback (msg _story &optional offline-p)
  "STORY associated with the message MSG has been fetched."
  (unless offline-p
    (telega-msg-redisplay msg)))

(defun telega-chat--active-stories (chat)
  ;; TODO
  )

(defun telega-story-open (story &optional for-msg)
  "Open and view STORY."
  (when (telega-story-deleted-p story)
    (user-error "telega: Can't open expired story"))

  (telega--openStory story)
  (let ((content (plist-get story :content)))
    (cl-ecase (telega--tl-type content)
      (storyContentPhoto
       (message "telega: TODO open photo story")
       )
      (storyContentVideo
       (telega-msg-open-video for-msg (plist-get content :video)))
      (storyContentUnsupported
       (user-error "telega: Can't open unsupported story content"))
      )))

(defun telega-ins--chat-active-stories (chat)
  "Inserter for CHAT's active stories."
  (when-let ((stories (telega-chat--active-stories chat)))
    ;; TODO
    t))

;;; ellit-org: minor-modes
;; ** telega-active-stories-mode
;;
;; Minor mode to display currently active stories from users in the
;; root buffer.
;;
;; ~telega-active-stories-mode~ is enabled by default.
(define-minor-mode telega-active-stories-mode
  "Global mode to display currently active stories in the root buffer."
  :init-value nil :global t :group 'telega-modes
  ;; TODO
  )

(provide 'telega-story)

;;; telega-story.el ends here

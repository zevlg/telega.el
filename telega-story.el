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

(defun telega-story-chat (story &optional offline-p)
  (telega-chat-get (plist-get story :sender_chat_id) offline-p))

(defun telega-story--content-file (story)
  "Return STORY content file."
  (let ((story-content (plist-get story :content)))
    (cl-case (telega--tl-type story-content)
      (storyContentPhoto
       (telega-file--renew
        (telega-photo--highres (plist-get story-content :photo))
        :photo))
      (storyContentVideo
       (telega-file--renew
        (plist-get story-content :video) :video)))))

(defun telega-story--download (story &optional priority
                                     progress-callback callback)
  "Download story, call CALLBACK when story downloads.
Call PROGRESS-CALLBACK while downloading story's content file.
Both callbacks are called with two arguments - story and file."
  (declare (indent 2))
  (when-let ((tl-file (telega-story--content-file story)))
    (when (and (telega-file--can-download-p tl-file)
               ;; NOTE: if file is already downloading and CALLBACK is
               ;; not provided, there is no need to start downloading
               (or callback (not (telega-file--downloading-p tl-file))))
      (telega-file--download tl-file priority
        (when (or progress-callback callback)
          (lambda (dfile)
            (cond ((and callback (telega-file--downloaded-p dfile))
                   (funcall callback story dfile))
                  ((and progress-callback (telega-file--downloading-p dfile))
                   (funcall progress-callback story dfile)))))))))

(defun telega-story--ensure (story &optional no-root-update)
  "Ensure STORY being cached."
  (when telega-debug
    (cl-assert story))
  (puthash (cons (plist-get story :sender_chat_id)
                 (plist-get story :id))
           story telega--cached-stories)

  ;; Possibly start preloading story's content
  (when (and telega-story-preload-for
             (telega-chat-match-p (telega-story-chat story)
               telega-story-preload-for))
    (telega-story--download story 5))

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
  (gethash (plist-get chat :id) telega--chat-active-stories))

(gv-define-setter telega-chat--active-stories (value chat)
  `(puthash (plist-get ,chat :id) ,value telega--chat-active-stories))

(defun telega-story--download-progress-callback (_story file)
  (cl-assert (not (telega-file--downloaded-p file)))
  (message "telega: Downloading story %d%% (%s)"
           (round (* (telega-file--downloading-progress file)
                     100))
           (telega-i18n "lng_media_save_progress"
             :ready (file-size-human-readable
                     (telega-file--downloaded-size file))
             :total (file-size-human-readable
                     (telega-file--size file))
             :mb "")))

(defun telega-story--download-open-callback (story file)
  "Open downloaded STORY."
  (cl-assert (telega-file--downloaded-p file))

  (telega--openStory story)
  (cl-ecase (telega--tl-type (plist-get story :content))
    (storyContentPhoto
     (telega-image-view-file file)
     (telega--closeStory story))

    (storyContentVideo
     (telega-video-player-run (telega--tl-get file :local :path) nil
       (lambda ()
         (telega--closeStory story))))))

(defun telega-story-open (story &optional _for-msg)
  "Open and view STORY."
  (when (telega-story-deleted-p story)
    (user-error "telega: Can't open expired story"))
  (unless (telega-story-match-p story '(or is-photo is-video))
    (user-error "telega: Can't open unsupported story content"))

  (telega-story--download story 32
    #'telega-story--download-progress-callback
    #'telega-story--download-open-callback))

(defun telega-svg-story-icon (svg width &rest args)
  "Generate a story icon using SYMBOL inside story icon."
  (declare (indent 2))
  (let ((ratio (/ width 32.0))
        (outline-left "M16.0,28.0 A12.0,12.0 0 0,1 16.0,4.0")
        (outline-right "M16.0,4.0 A12.0,12.0 0 0,1 16.0,28.0"))
    (telega-svg-apply-outline
     svg outline-left ratio
     (nconc (list :fill "none" :stroke "currentColor"
                  :stroke-width "3")
            args))
    (telega-svg-apply-outline
     svg outline-right ratio
     (nconc (list :fill "none" :stroke "currentColor"
                  :stroke-width "3" :stroke-dasharray "5.5"
                  :stroke-dashoffset "5.5")
            args))
    svg))

(defun telega-svg-story-icon-with-symbol (svg width symbol &rest args)
  "Generate story icon with SYMBOL inside."
  (apply #'telega-svg-story-icon svg width args)

  (let ((font-size (/ width 2.75)))
    (svg-text svg symbol
              :font-size font-size
              ;; :font-weight "bold"
              :fill "currentColor"
              :font-family "monospace"
              ;; XXX insane X/Y calculation
              :x (- (/ width 2) (/ font-size 1.6))
              :y (+ (/ font-size 3) (/ width 2))))
  svg)

;; TODO: not yet used
(defun telega-story-svg-squircle (svg size sender viewed-p &rest _args)
  "Generate svg story outline for the story SENDER."
  (let* ((x-shift 0);(/ size 10.0))
         (sw-passive 10)
         (sw-active (* sw-passive 2))
         (passive-color (telega-color-name-as-hex-2digits
                         (face-foreground 'shadow))))
    (unless viewed-p
      (cl-destructuring-bind (c1 c2) (telega-msg-sender-color sender)
        (apply #'telega-svg-raw-node
               svg 'linearGradient
               '((id . "a")
                 (x1 . 0) (y1 . 1) (x2 . 1) (y2 . 0))
               (mapcar (lambda (stop)
                         (dom-node 'stop `((offset . ,(format "%.1f" (car stop)))
                                           (stop-color . ,(cdr stop)))))
                       `((0 . ,(telega-color-name-as-hex-2digits c2))
                         (0.2 . ,(telega-color-name-as-hex-2digits c2))
                         (0.5 . ,(telega-color-name-as-hex-2digits c1))
                         (1 . ,(telega-color-name-as-hex-2digits c1)))))))

    (telega-svg-squircle svg x-shift x-shift
                         (- size x-shift x-shift) (- size x-shift x-shift)
      :stroke-width (if viewed-p sw-passive sw-active)
      :stroke-color (if viewed-p passive-color "url(#a)")
      :fill-color "none")
    svg))

(defun telega-story-preview--create-svg-one-line (story filename data-p
                                                        width height)
  "Create one line svg image for the story content."
  (let* ((svg-w (telega-chars-xwidth 2))
         (svg-h (min svg-w (telega-chars-xheight 1)))
         (svg (telega-svg-create svg-w svg-h))
         (pclip (telega-svg-clip-path svg "pclip"))
         (bw-filter (when (telega-story-match-p story 'seen)
                      (dom-node 'filter
                                `((id . "bw"))
                                (dom-node 'feColorMatrix
                                          '((type . "matrix")
                                            (values . ".33 .33 .33 0 0 .33 .33 .33 0 0 .33 .33 .33 0 0 0   0   0  1 0"))))))
         (margin 1))
    (when bw-filter
      (svg--append svg bw-filter))

    (telega-svg-squircle pclip margin margin
                         (- svg-w (* 2 margin)) (- svg-h (* 2 margin)))

    (telega-svg-embed-image-fitting svg filename data-p width height
                                    :filter (when bw-filter "url(#bw)")
                                    :clip-path "url(#pclip)")
    (when (telega-story-match-p story 'is-video)
      (telega-svg-red-play-triangle svg (when bw-filter "black")))

    (telega-svg-image svg :scale 1.0 :width svg-w :height svg-h
                      :ascent 'center :mask 'heuristic
                      :base-uri (if data-p "" filename))))

(defun telega-story-preview--create-image-one-line (story &optional for-msg)
  "Create a preview image for the STORY."
  (let ((content (plist-get story :content))
        ;; NOTE: inhibit caching, so story image will be updated on
        ;; `seen' story status change
        (telega-preview--inhibit-cached-preview t)
        (telega-preview--create-svg-one-line-function
         (apply-partially #'telega-story-preview--create-svg-one-line story)))
    (cl-case (telega--tl-type content)
      (storyContentPhoto
       (telega-photo-preview--create-image-one-line
        (plist-get content :photo)
        (when for-msg
          (telega-msg-chat for-msg 'offline))))
      (storyContentVideo
       (telega-video-preview--create-image-one-line
        (plist-get content :video)
        (when for-msg
          (telega-msg-chat for-msg 'offline)))))
    ))

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

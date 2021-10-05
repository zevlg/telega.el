;;; telega-stories.el --- Emacs Stories.  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2021 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Dec 22 00:47:53 2020
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
;; ** /telega-stories.el/ -- Display Emacs Stories in the dashboard
;;
;; Emacs Stories: share your Emacs experience with other Emacs users.
;;
;; Display recent [[https://t.me/emacs_stories][Emacs Stories]] in the
;; dashboard.  Enable it with:
;;
;; #+begin_src emacs-lisp
;; (require 'telega-stories)
;; (telega-stories-mode 1)
;; ;; "Emacs Stories" rootview
;; (define-key telega-root-mode-map (kbd "v e") 'telega-view-emacs-stories)
;; ;; Emacs Dashboard
;; (add-to-list 'dashboard-items '(telega-stories . 5))
;; #+end_src
;;
;; Apart from dashboard, ~telega-stories~ provides "Emacs Stories"
;; [[https://zevlg.github.io/telega.el/#rootbuf-view-switching][Root
;; View]].  To enable this view execute {{{kbd(M-x
;; telega-view-emacs-stories RET)}}} in the root buffer.
;;
;; If you see inappropriate content in some Emacs Story, please report
;; this story by pressing
;; {{{where-is(telega-stories-msg-report,telega-stories-keymap)}}} on
;; the story.
;;
;; For best performance consider newest Emacs28 with ~:base_uri~ svg
;; image property support.
;;
;; Screenshots of =telega-stories= in action:
;; [[https://zevlg.github.io/telega/emacs-stories-dashboard.png]]
;;
;; And screenshot of "Emacs Stories" root view:
;; [[https://zevlg.github.io/telega/emacs-stories-rootview.png]]
;;
;; Customizable options:
;; - {{{user-option(telega-stories-show, 2)}}}
;; - {{{user-option(telega-stories-height, 2)}}}
;; - {{{user-option(telega-stories-notify-if, 2)}}}
;; - {{{user-option(telega-stories-preload-content, 2)}}}
;; - {{{user-option(telega-stories-root-view-count, 2)}}}
;; - {{{user-option(telega-stories-root-view-keep-viewed, 2)}}}

;;; Code:
(require 'telega)

;; Customizable variables
(defgroup telega-stories nil
  "Customisation for Emacs Stories telega mode."
  :prefix "telega-stories-"
  :group 'telega)

(defcustom telega-stories-show 'unread
  "Show `all' or only `unread' stories."
  :type '(choice (const :tag "Only unread Emacs Stories" unread)
                 (const :tag "All Emacs Stories" all))
  :group 'telega-stories)

(defcustom telega-stories-height (if (consp telega-video-note-height)
                                     (car telega-video-note-height)
                                   telega-video-note-height)
  "Height in chars for Emacs Stories buttons"
  :type 'integer
  :group 'telega-stories)

(defcustom telega-stories-notify-if '(not unmuted)
  "Pop notification on new story if stories chat matches this Chat Filter."
  :type 'list
  :group 'telega-stories)

(defcustom telega-stories-delimiter " "
  "Delimiter between stories in the dashboard and rootview."
  :type 'string
  :group 'telega-stories)

(defcustom telega-stories-preload-content t
  "Preload content when Emacs Story is inserted, so can be viewed instantly."
  :type 'boolean
  :group 'telega-stories)

(defcustom telega-stories-root-view-count 12
  "Number of Emacs Stories to show in \"Emacs Stories\" rootview."
  :type 'integer
  :group 'telega-stories)

(defcustom telega-stories-root-view-keep-viewed t
  "Keep viewed stories in the \"Emacs Stories\" rootview.
Non-nil to keep story in the root view after story is viewed."
  :type 'boolean
  :group 'telega-stories)


;; Runtime variables
(defvar telega-stories-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "n") 'telega-button-forward)
    (define-key map (kbd "p") 'telega-button-backward)
    (define-key map (kbd "<tab>") 'telega-button-forward)
    (define-key map (kbd "<backtab>") 'telega-button-backward)
    (define-key map (kbd "SPC") 'telega-stories-msg-goto)
    (define-key map (kbd "!") 'telega-stories-msg-report)
    map)
  "Keymap for Emacs Stories buttons.")

(defconst telega-stories-group "@emacs_stories"
  "Telegram group where stories are posted.")
(defconst telega-stories--featured-chat-ids '(-1001223420888)
  "Ids of chats from where featured stories are forwarded.")

(defvar telega-stories--chat nil
  "Chat corresponding to `telega-stories-group'.")
(defvar telega-stories--admins nil
  "List of administrators in the @emacs_stories group.")

(defvar telega-stories--dashboard-list-size 10
  "List `list-size' from `telega-stories-insert'.
To get know how many stories to fetch.")
(defvar telega-stories--dashboard-items nil
  "List of Emacs Stories items displayed in the dashboard.
Used to update images inplace, when story is viewed.
Each element is cons cell, where car is a Emacs Story message,
and cdr is image displayed in the dashboard.")
(defvar telega-stories--dashboard-cached-icon nil
  "Cached Emacs Stories logo image.")

(defvar telega-stories--all-messages nil
  "List of all fetched story messages.")
(defvar telega-stories--show-messages nil
  "List of story messages to show, matching `telega-stories-show' setting.")

;;;###autoload
(define-minor-mode telega-stories-mode
  "Global mode to track Emacs Stories for updates."
  :init-value nil :global t :group 'telega-modes
  (if telega-stories-mode
      (progn
        (advice-add 'telega--on-updateDeleteMessages
                    :after 'telega-stories--on-message-delete)
        (advice-add 'telega--on-updateMessageContent
                    :after 'telega-stories--on-message-content-update)
        (advice-add 'telega--on-updateMessageSendSucceeded
                    :after 'telega-stories--on-message-send-succeeded)
        (add-hook 'telega-chat-post-message-hook
                  #'telega-stories--on-new-message)
        ;; Install this for telega restarts
        (add-hook 'telega-ready-hook
                  #'telega-stories--initialize)
        (when (telega-server-live-p)
          (telega-stories--initialize)))

    (remove-hook 'telega-ready-hook
                 #'telega-stories--initialize)
    (remove-hook 'telega-chat-post-message-hook
                 #'telega-stories--on-new-message)
    (advice-remove 'telega--on-updateMessageSendSucceeded
                   'telega-stories--on-message-send-succeeded)
    (advice-remove 'telega--on-updateMessageContent
                   'telega-stories--on-message-content-update)
    (advice-remove 'telega--on-updateDeleteMessages
                   'telega-stories--on-message-delete)
    (telega-stories--finalize)
    ))


;; Dashboard
(defun telega-stories-dashboard-insert (list-size)
  "Add at most LIST-SIZE important telega chats."
  (when (and (display-graphic-p) dashboard-set-heading-icons)
    ;; Insert Emacs Stories logo icon
    (unless telega-stories--dashboard-cached-icon
      (setq telega-stories--dashboard-cached-icon
            (find-image
             (list (list :type (when (fboundp 'imagemagick-types) 'imagemagick)
                         :file "etc/emacs-stories64.png"
                         :ascent 'center :mask 'heuristic
                         :height (telega-chars-xheight 1))
                   (list :type 'svg :file "etc/emacs-stories.svg"
                         :ascent 'center
                         :background (face-attribute 'default :background)
                         :height (telega-chars-xheight 1))))))
    (insert (propertize telega-symbol-telegram
                        'display telega-stories--dashboard-cached-icon)
            " "))
  ;; Avoid extra spaces insertation in case icons are used
  (let ((dashboard-set-heading-icons nil))
    (dashboard-insert-heading
     (concat (telega-i18n "telega_stories_heading") ":")
     (dashboard-get-shortcut 'telega-stories)))

  (cond ((not (telega-server-live-p))
         (telega-ins--with-face 'error
           (telega-ins "\n    --- "
                       (telega-i18n "telega_dashboard_telega_not_running")
                       " ---")))
        ((not telega-stories-mode)
         (telega-ins--with-face 'error
           (telega-ins "\n    --- "
                       "`telega-stories-mode' not enabled"
                       " ---")))
        (t
         (let ((imsgs (seq-take telega-stories--show-messages list-size)))
           (if (not imsgs)
               (telega-ins--with-face 'dashboard-no-items-face
                 (telega-ins "\n    --- "
                             (telega-i18n "telega_stories_no_stories")
                             " ---"))
             (insert "\n    ")
             (dolist (msg imsgs)
               (telega-stories--msg-pp msg))))))
  (dashboard-insert-shortcut
   (dashboard-get-shortcut 'telega-stories)
   (concat (telega-i18n "telega_stories_heading") ":"))
  )


;; Emacs Stories Runtime
(defun telega-stories--initialize ()
  "Initialize Emacs Stories runtime."
  (setq telega-stories--all-messages nil
        telega-stories--show-messages nil)

  (telega--searchPublicChat telega-stories-group
    (lambda (chat)
      (setq telega-stories--chat chat)
      ;; NOTE: open it to receive new messages
      (telega--openChat telega-stories--chat)
      (telega--getChatAdministrators telega-stories--chat
        (lambda (admins)
          (setq telega-stories--admins admins)))
      (telega-stories--async-fetch-stories))))

(defun telega-stories--finalize ()
  "Finalize Emacs Stories runtime."
  (setq telega-stories--all-messages nil
        telega-stories--show-messages nil)

  (when (and telega-stories--chat (telega-server-live-p))
    (telega--closeChat telega-stories--chat))
  (setq telega-stories--chat nil)

  ;; Update root view as well
  (with-telega-root-view-ewoc "stories" ewoc
    (telega-ewoc--clean ewoc)
    (telega-ewoc--set-footer
     ewoc (propertize "-- `telega-stories-mode' not enabled --"
                      'face 'error))
    ))

(defun telega-stories--async-fetch-stories (&optional from-msg-id
                                                      &rest msg-filters)
  "Asynchronously fetch older Emacs Story messages starting from FROM-MSG-ID.
If FROM-MSG-ID is not given, then search from the last story message
or from the beginning."
  (cl-assert telega-stories--chat)
  (when (< (length telega-stories--show-messages)
           (max telega-stories--dashboard-list-size
                telega-stories-root-view-count))
    ;; Need more stories
    (dolist (filter-type (or msg-filters
                             '("searchMessagesFilterPhotoAndVideo"
                               "searchMessagesFilterAnimation"
                               "searchMessagesFilterDocument"
                               ;;"searchMessagesFilterUrl"
                               )))
      (telega--searchChatMessages telega-stories--chat
          (list :@type filter-type)
          (if (string= "searchMessagesFilterUrl" filter-type)
              "asciinema.org"
            "")
          (or from-msg-id
              (plist-get (car (last telega-stories--all-messages)) :id)
              0)
          0                             ; offset
          (max telega-stories--dashboard-list-size
               telega-stories-root-view-count)
          nil
        (lambda (reply)
          (let ((stories-array (plist-get reply :messages)))
            (telega-debug "Emacs Stories: fetched %d stories for %S"
                          (length stories-array) filter-type)
            (seq-doseq (story stories-array)
              (telega-stories--on-new-message story 'no-notify))

            ;; NOTE: If searched got some messages and there is still
            ;; not enough story messages, then search for older
            ;; messages starting from oldest viewed message
            (when (> (length stories-array) 0)
              (let* ((last-fetched-story
                      (aref stories-array (1- (length stories-array))))
                     (continue-from-msg-id
                      (apply #'min (plist-get last-fetched-story :id)
                             (telega-chat-uaprop
                              telega-stories--chat :telega-stories-viewed-ids))))
                (cl-assert (> continue-from-msg-id 0))
                (telega-stories--async-fetch-stories
                 continue-from-msg-id filter-type))))))
      )))

(defun telega-stories-msg-view (msg &optional no-open-p no-fetch-p)
  "View Emacs Story message MSG.
If NO-OPEN-P is specified, then do not open content."
  (unless (or no-open-p current-prefix-arg)
    (let ((telega-animation-play-inline nil)) ; play animations externally
      (telega-msg-open-content msg)))

  ;; Add this story message to the list of viewed stories
  (let ((viewed-stories (telega-chat-uaprop
                         (telega-msg-chat msg) :telega-stories-viewed-ids)))
    (unless (memq (plist-get msg :id) viewed-stories)
      ;; NOTE: Store maximum 500 ids, so `uaprop' won't grow inifinitely
      (setf (telega-chat-uaprop (telega-msg-chat msg) :telega-stories-viewed-ids)
            (seq-take (cons (plist-get msg :id) viewed-stories) 500))))

  ;; Also update `telega-stories--show-messages' and possible
  ;; asynchronously fetch older stories
  (telega-stories--show-messages-reset)
  (unless no-fetch-p
    (telega-stories--async-fetch-stories))

  ;; Inplace update story image in the dashboard
  (cl-destructuring-bind (thumb thumb-prop)
      (telega-stories--msg-thumbnail-spec msg)
    (telega-media--image-update
     (cons msg #'telega-stories--msg-create-image)
     (cons thumb thumb-prop)
     :telega-story-image)
    (force-window-update))

  ;; NOTE: Update the rootview *before* removing message from
  ;; `telega-stories--show-messages', because update func checks
  ;; message is story as membership of the list
  (telega-root-view--update :on-message-update msg))

(defun telega-stories-msg-goto (msg)
  "Goto to Emacs Story message MSG, opening corresponding chat.
Show discussion thread for story MSG."
  ;; NOTE: Can't use `telega-msg-at' here, because button type is
  ;; `telega', not `telega-message'
  (interactive (list (button-get (button-at (point)) :value)))
  (telega-stories-msg-view msg 'no-open)
  (telega-msg-goto-highlight msg))

(defun telega-stories-msg-report (msg reason)
  "Report Emacs Story message MSG has inappropriate content."
  ;; NOTE: Can't use `telega-msg-at' here, because button type is
  ;; `telega', not `telega-message'
  (interactive (list (button-get (button-at (point)) :value)
                     (read-string "Story Report Reason [spam]: "
                                  nil nil "spam")))
  (telega--sendMessage
   telega-stories--chat
   (list :@type "inputMessageText"
         :text (telega-string-fmt-text (concat reason " #report")))
   msg nil)

  (message "telega: Story reported, thanks for your feedback."))

(defun telega-stories-view-loaded ()
  "Mark currently loaded stories as viewed."
  (interactive)

  (dolist (msg (copy-sequence telega-stories--show-messages))
    (unless (telega-stories--msg-viewed-p msg)
      (telega-stories-msg-view msg 'no-open 'no-fetch)))

  (telega-stories--async-fetch-stories))

(defun telega-stories-unview-all (&rest story-messages)
  "Pretend none of the STORY-MESSAGES are viewed.
If STORY-MESSAGES is not specified, unview all story messages."
  (interactive)
  (unless telega-stories--chat
    (user-error "telega-stories-mode not started"))
  (setf (telega-chat-uaprop telega-stories--chat :telega-stories-viewed-ids)
        (when story-messages
          (seq-difference (telega-chat-uaprop
                           telega-stories--chat :telega-stories-viewed-ids)
                          (mapcar (telega--tl-prop :id) story-messages))))

  ;; Empty root view, waiting for update
  (with-telega-root-view-ewoc "stories" ewoc
    (telega-ewoc--clean ewoc)
    (telega-ewoc--set-footer ewoc "Loading Emacs Stories.\n")
    (telega-loading--timer-start))

  ;; Refresh stories messages
  (setq telega-stories--all-messages nil
        telega-stories--show-messages nil)
  (telega-stories--async-fetch-stories)
  )

(defun telega-stories--msg-delete (msg)
  "Delete MSG from stories."
  (cl-assert msg)
  (setq telega-stories--show-messages
        (delq msg telega-stories--show-messages))
  (let ((telega-stories-root-view-keep-viewed nil))
    (telega-root-view--update :on-message-update msg))

  (setq telega-stories--all-messages
        (delq msg telega-stories--all-messages)))
  
(defun telega-stories--msg-featured-p (msg)
  "Return non-nil if MSG is a featured story.
Return featured chat id, if MSG is featured."
  (when-let* ((origin (telega--tl-get msg :forward_info :origin))
              (chat-id (cl-case (telega--tl-type origin)
                         (messageForwardOriginChat
                          (plist-get origin :sender_chat_id))
                         (messageForwardOriginChannel
                          (plist-get origin :chat_id)))))
    (car (memq chat-id telega-stories--featured-chat-ids))))

(defun telega-stories--msg-viewed-p (msg)
  "Return non-nil if Emacs Story MSG is viewed."
  (memq (plist-get msg :id)
        (telega-chat-uaprop
         (telega-msg-chat msg) :telega-stories-viewed-ids)))

(defun telega-stories--msg-with-story-tag-p (msg)
  "Return non-nil if MSG has #emacs_story or #story tag."
  (let ((content (plist-get msg :content)))
    (cl-case (telega--tl-type content)
      ((messagePhoto messageVideo messageAnimation messageDocument)
       (string-match-p
        "#\\(emacs_\\)?story" (or (telega-tl-str content :caption) "")))
      (messageText
       (string-match-p
        "#\\(emacs_\\)?story" (or (telega-tl-str content :text) ""))))))

(defun telega-stories--msg-by-admin-p (msg)
  "Return non-nil if MSG is sent by `telega-stories-group' admin."
  (let ((sender (telega-msg-sender msg)))
    (if (telega-chat-p sender)
        ;; sent by anonymous admin
        (eq (plist-get telega-stories--chat :id) (plist-get sender :id))
      ;; sent by ordinary user
      (cl-find (plist-get sender :id) telega-stories--admins
               :key (telega--tl-prop :user_id)))))

(defun telega-stories-msg-story-p (msg)
  "Return non-nil if MSG a Emacs Story message."
  (let ((msg-type (telega--tl-type (plist-get msg :content))))
    (and (eq (plist-get msg :chat_id)
             (plist-get telega-stories--chat :id))
         (memq msg-type '(messagePhoto messageVideo messageDocument
                                       messageAnimation))
         (or (eq telega-debug 'telega-stories)
             (plist-get msg :forward_info)
             (telega-stories--msg-with-story-tag-p msg))
         ;; Messages considered as Emacs Story:
         ;; 1. Nice Emacs screenshot
         ;; 2. Video Message with screencast not longer then 60 seconds
         ;;    (if sent by admin, then no longer then 120 seconds)
         ;; 3. Link to asciinema.org cast [NOT YET]
         ;; 4. Document (File) message with attached:
         ;;    - photo/video
         ;;    - asciinema cast
         (cl-ecase msg-type
           ((messageVideo messageAnimation)
            (let ((duration (telega--tl-get msg :content
                                            (if (eq 'messageVideo msg-type)
                                                :video :animation)
                                            :duration)))
              (or (<= duration 60)
                  (and (telega-stories--msg-by-admin-p msg)
                       (<= duration 120)))))
           (messageText
            (let ((web-page (telega--tl-get msg :content :web_page)))
              (and (equal "asciinema.org" (plist-get web-page :site_name))
                   (string-match-p "^https://asciinema.org/a/[0-9]+$"
                                   (or (telega-tl-str web-page :url) "")))))
           (t t))
         ;; Check thumbnail can be displayed
         (cl-destructuring-bind (thumb _thumb-prop)
             (telega-stories--msg-thumbnail-spec msg)
           (and thumb (or (not (eq 'thumbnail (telega--tl-type thumb)))
                          (equal '(:@type "thumbnailFormatJpeg")
                                 (plist-get thumb :format)))))
    )))

(defun telega-stories--msg-pp (msg)
  "Pretty printer for story message MSG."
  (telega-ins-prefix "\n"
    (telega-button--insert 'telega msg
      'keymap telega-stories-keymap
      :inserter (lambda (msg)
                  (cl-destructuring-bind (thumb thumb-prop)
                      (telega-stories--msg-thumbnail-spec msg)
                    (telega-ins--image
                     (telega-media--image
                      (cons msg #'telega-stories--msg-create-image)
                      (cons thumb thumb-prop)
                      'force :telega-story-image))))
      :action #'telega-stories-msg-view)
    ;; NOTE: start a new line if story does not fit into
    ;; `telega-root-fill-column'
    (> (telega-current-column) telega-root-fill-column))
  (telega-ins telega-stories-delimiter))

(defun telega-stories--msg-thumbnail-spec (msg)
  "Return thumbnail spec for the story message MSG.
Return list of three elements: (THUMB THUMB-PROP CONTENT-FILE)."
  (cl-ecase (telega--tl-type (plist-get msg :content))
    (messagePhoto
     (list (telega-photo--best
            (telega--tl-get msg :content :photo)
            (list 40 telega-stories-height
                  40 telega-stories-height))
           :photo))
    (messageVideo
     (list (telega--tl-get msg :content :video :thumbnail)
           :file))
    (messageDocument
     (list (telega--tl-get msg :content :document :thumbnail)
           :file))
    (messageAnimation
     (list (telega--tl-get msg :content :animation :thumbnail)
           :file))
    ))

(defun telega-stories--msg-create-image (msg &optional _file)
  "Generate svg image for story message MSG."
  (let* ((tfile
          (cl-destructuring-bind (thumb thumb-prop)
              (telega-stories--msg-thumbnail-spec msg)
            (telega-file--renew thumb thumb-prop)))
         (sender (telega-msg-sender msg))
         (title
            (propertize (or (telega-msg-sender-username sender 'with-@)
                            (telega-msg-sender-title sender))
                        :color (car (telega-msg-sender-color sender))))
         (viewed-p (telega-stories--msg-viewed-p msg))
         (size (telega-chars-xwidth (* 2 telega-stories-height)))
         (sw-passive (/ size 100.0))
         (sw-active (* sw-passive 2))
         (passive-color (telega-color-name-as-hex-2digits
                         (face-foreground 'shadow)))
         ;; NOTE: one more line for the `title'
         (title-height (telega-chars-xheight 1))
         (svg-height (+ size title-height))
         (svg (telega-svg-create size svg-height))
         (pclip (telega-svg-clip-path svg "pclip"))
         (base-dir (telega-directory-base-uri telega-temp-dir)))
    (unless viewed-p
      (apply #'telega-svg-raw-node
             svg 'linearGradient
             '((id . "a")
               (x1 . 0) (y1 . 1) (x2 . 1) (y2 . 0))
             (mapcar (lambda (stop)
                       (dom-node 'stop `((offset . ,(format "%.1f" (car stop)))
                                         (stop-color . ,(cdr stop)))))
                     `((0 . "#fd5") (0.1 . "#fd5")
                       (0.5 . "#ff543e") (1 . "#c837ab")))))

    (telega-svg-squircle svg 0 0 size size
                         :stroke-width (if viewed-p sw-passive sw-active)
                         :stroke-color (if viewed-p passive-color "url(#a)")
                         :fill-color "none")
    (let ((c-off (* sw-passive 3))
          (c-sz (- size (* sw-passive 6))))
      ;; outline
      (telega-svg-squircle svg c-off c-off c-sz c-sz
                           :stroke-width (/ sw-passive 2)
                           :stroke-color passive-color
                           :fill-color "none")
      ;; clip mask
      (telega-svg-squircle pclip c-off c-off c-sz c-sz))

    (if (telega-file--downloaded-p tfile)
        (let* ((photofile (telega--tl-get tfile :local :path))
               (photo-image (create-image photofile))
               (photo-size (image-size photo-image t))
               (img-type (plist-get :type (cdr photo-image))))
          ;; Adjust base-dir
          (setq base-dir (file-name-directory photofile))
          (cl-destructuring-bind (x-fit y-fit w-fit h-fit)
              (telega-svg-fit-into (car photo-size) (cdr photo-size)
                                   size size)
            (telega-svg-embed svg (list (file-relative-name photofile base-dir)
                                        base-dir)
                              (format "image/%S" img-type)
                              nil :x x-fit :y y-fit :width w-fit :height h-fit
                              :clip-path "url(#pclip)")))

      ;; Show progress
      (let ((progress (telega-file--downloading-progress tfile))
            (font-size title-height))
        (svg-text svg (format "%3d%%" (round (* progress 100)))
                  :font-size font-size
                  :font-weight "bold"
                  :fill "black"
                  :font-family "monospace"
                  ;; XXX insane X/Y calculation
                  :x (- (/ size 2) (* 2 (/ font-size 3)))
                  :y (+ (/ font-size 3) (/ size 2)))))

    ;; Draw play triangle
    (when (memq (telega--tl-type (plist-get msg :content))
                '(messageVideo messageAnimation))
      (let ((play-size (/ size 6)))
        (svg-polygon svg (list (cons (/ (- size play-size) 2)
                                     (/ (- size play-size) 2))
                               (cons (/ (- size play-size) 2)
                                     (/ (+ size play-size) 2))
                               (cons (/ (+ size play-size) 2)
                                     (/ size 2)))
                     :fill "red"
                     :opacity "0.75")))

    ;; Title icon (for featured chat or patron user) and title
    (let* ((font-size (round (/ title-height 1.5)))
           (patron-p (telega-msg-sender-patron-p sender))
           (featured-chat-id
            (or (if (and patron-p (telega-chat-p sender))
                    (plist-get sender :id)
                  (telega-stories--msg-featured-p msg))))
           (title-photo
            (cond (featured-chat-id
                   (plist-get (telega-chat-get featured-chat-id) :photo))
                  (patron-p
                   (cl-assert (telega-user-p sender))
                   (plist-get sender :profile_photo))))
           (title-tphoto
            (when title-photo
              (telega-file--renew title-photo :small)))
           ;; `title-xoff' is used if title icon is inserted
           (title-xoff nil))
      (when featured-chat-id
        (setq title (telega-i18n "telega_stories_featured")))

      (when (and title-tphoto
                 (telega-file--downloaded-p title-tphoto))
        (let* ((c-xoff (/ font-size 4))
               (c-yoff (- svg-height title-height (/ font-size 3)))
               (clip (telega-svg-clip-path svg "fcclip")))
          (svg-circle clip (+ c-xoff (/ title-height 2))
                      (+ c-yoff (/ title-height 2)) (/ title-height 2))
          ;; NOTE: embedd using data, so `base-path' can point anywhere
          (svg-embed svg (telega--tl-get title-tphoto :local :path)
                     "image/jpeg" nil
                     :x c-xoff :y c-yoff
                     :width title-height :height title-height
                     :clip-path "url(#fcclip)"))
        (setq title-xoff (+ title-height (/ font-size 2))))

      (svg-text svg title
                :font-size font-size
                :fill (telega-color-name-as-hex-2digits
                       (or (get-text-property 0 :color title)
                           (face-foreground 'telega-username)))
                :font-family "monospace"
                ;; XXX insane X/Y calculation
                :x (or title-xoff
                       (- (/ size 2) (* (length title) (/ font-size 3))))
                :y (- svg-height (/ font-size 1.5))))

    (telega-svg-image svg :scale 1.0 :width size :height svg-height
                      :ascent 'center
                      :mask 'heuristic
                      :base-uri (expand-file-name "dummy" base-dir))))

(defun telega-stories--msg-preload-content (msg)
  "Start downloading MSG story message's content."
  (let ((cfile (telega-msg--content-file msg)))
    (when (and (not (telega-file--downloaded-p cfile))
               (not (telega-file--downloading-p cfile)))
      ;; NOTE: Use higher priority for recent stories
      (telega-file--download cfile
          (max 16 (- 32 (cl-position msg telega-stories--show-messages)))))))

(defun telega-stories--show-messages-reset ()
  "Re-set `telega-stories--show-messages' according to `telega-stories-show'."
  (setq telega-stories--show-messages
        (cl-remove-if (when (eq 'unread telega-stories-show)
                        #'telega-stories--msg-viewed-p)
                      telega-stories--all-messages)))

(defun telega-stories--on-new-message (new-msg &optional ignore-notify-p)
  "If NEW-MSG is an unread Emacs Story message, then add it to the list.
If IGNORE-NOTIFY-P is non-nil, then do not pop notification."
  (when (telega-stories-msg-story-p new-msg)
    (telega-debug "Emacs Stories ADD story: %S" (plist-get new-msg :id))
    (cl-pushnew new-msg telega-stories--all-messages
                :test (lambda (msg1 msg2)
                        (eq (plist-get msg1 :id) (plist-get msg2 :id))))
    ;; NOTE: keep messages is message id order
    (setq telega-stories--all-messages
          (cl-sort telega-stories--all-messages #'>
                   :key (telega--tl-prop :id)))
    (telega-stories--show-messages-reset)

    ;; Start preloading story content, only for unread stories
    (when (and telega-stories-preload-content
               (not (telega-stories--msg-viewed-p new-msg))
               (memq new-msg
                     (seq-take telega-stories--show-messages
                               (max telega-stories--dashboard-list-size
                                    telega-stories-root-view-count))))
      (telega-stories--msg-preload-content new-msg))

    (telega-root-view--update :on-message-update new-msg)

    ;; Show notification for new Emacs Story
    (when (and telega-notifications-mode
               (not ignore-notify-p)
               (telega-chat-match-p telega-stories--chat
                                    telega-stories-notify-if))
      (telega-notifications--chat-msg0 new-msg nil
        :app-icon (telega-etc-file
                   (if (telega-stories--msg-featured-p new-msg)
                       "emacs-stories-featured.svg"
                     "emacs-stories.svg"))
        :title (telega-chat-title telega-stories--chat)
        ))
    ))

(defun telega-stories--on-message-send-succeeded (event)
  "Story message might change its id."
  (when (eq (plist-get telega-stories--chat :id)
            (telega--tl-get event :message :chat_id))
    (let ((old-story (cl-find (plist-get event :old_message_id)
                              telega-stories--all-messages
                              :key (telega--tl-prop :id))))
      (when old-story
        (telega-stories--msg-delete old-story))
      (telega-stories--on-new-message
       (plist-get event :message) (unless old-story 'no-notify)))))

(defun telega-stories--on-message-content-update (event)
  "Message content changed."
  (when (eq (plist-get telega-stories--chat :id)
            (plist-get event :chat_id))
    (when-let ((story (cl-find (plist-get event :message_id)
                               telega-stories--all-messages
                               :key (telega--tl-prop :id))))
      ;; Delete story, and then re-add it with new content
      (telega-stories--on-message-delete
       (list :chat_id (plist-get telega-stories--chat :id)
             :is_permanent t
             :message_ids (vector (plist-get story :id))))

      (plist-put story :content (plist-get event :new_content))
      (telega-stories--on-new-message story 'no-notify))))

(defun telega-stories--on-message-delete (event)
  "Possible story message has been deleted."
  (when (eq (plist-get telega-stories--chat :id)
            (plist-get event :chat_id))
    (when (plist-get event :is_permanent)
      (seq-doseq (msg-id (plist-get event :message_ids))
        (when-let ((story (cl-find msg-id
                                   telega-stories--all-messages
                                   :key (telega--tl-prop :id))))
          ;; NOTE: Force `story' message removal from root view
          (setq telega-stories--show-messages
                (delq story telega-stories--show-messages))
          (let ((telega-stories-root-view-keep-viewed nil))
            (telega-root-view--update :on-message-update story))

          (setq telega-stories--all-messages
                (delq story telega-stories--all-messages)))))))


;; "Emacs Stories" rootview (rv = rootview)
(defun telega-stories--rv-msg-update (_ewoc-name ewoc msg)
  "Message MSG has been updated, possible story need to be updated as well."
  (when (memq msg telega-stories--all-messages)
    ;; React only on story messages in the list
    (telega-save-cursor
      (if-let ((msg-node (telega-ewoc--find-by-data ewoc msg)))
          (if (or telega-stories-root-view-keep-viewed
                  (memq msg telega-stories--show-messages))
              (ewoc-invalidate ewoc msg-node)
            (ewoc-delete ewoc msg-node))

        (when (memq msg telega-stories--show-messages)
          ;; New visible story arrived
          (if-let ((before-node (telega-ewoc--find-if ewoc
                                  (lambda (story-msg)
                                    (< (plist-get story-msg :id)
                                       (plist-get msg :id))))))
              (ewoc-enter-before ewoc before-node msg)
            (ewoc-enter-last ewoc msg))))

      ;; Ensure view has `telega-stories-root-view-count' stories
      ;; displayed
      ;; We might delete or add stories at the end
      (let ((stories (ewoc-collect ewoc #'identity)))
        (cond ((> (length stories) telega-stories-root-view-count)
               ;; Delete last visible story
               (let ((last-node (ewoc-nth ewoc -1)))
                 (cl-assert last-node)
                 (ewoc-delete ewoc last-node)))
              ((< (length stories) telega-stories-root-view-count)
               ;; Add visible story to the end
               (when-let* ((last-node (ewoc-nth ewoc -1))
                           (last-msg (when last-node (ewoc-data last-node)))
                           (next-msgs
                            (if last-msg
                                (cdr (memq last-msg telega-stories--all-messages))
                              telega-stories--all-messages)))
                 (while next-msgs
                   (when (memq (car next-msgs) telega-stories--show-messages)
                     (ewoc-enter-last ewoc (car next-msgs))
                     (setq next-msgs nil))
                   (setq next-msgs (cdr next-msgs)))))
              ))

      ;; Footer
      (telega-ewoc--set-footer
       ewoc (if (telega-ewoc--empty-p ewoc)
                (telega-i18n "telega_stories_no_stories")
              ""))
      )))

(defun telega-view-emacs-stories (toggle-show-p)
  "View recent Emacs Stories.
If `\\[universal-argument] is given, then toggle `telega-stories-show'
option before viewing stories.  If it has value `unread', then set it
to `all', if it was `all', then set it to `unread'."
  (interactive "P")

  (unless telega-stories-mode
    (user-error "telega: Can't view Emacs Stories, \
`telega-stories-mode' not enabled"))

  (when toggle-show-p
    (setq telega-stories-show
          (if (eq 'all telega-stories-show) 'unread 'all)))
  (telega-stories--show-messages-reset)

  (telega-root-view--apply
   (list 'telega-view-emacs-stories
         (concat (telega-i18n "telega_stories_heading")
                 " (" (if (eq 'all telega-stories-show) "all" "unread") ")")
         (let ((stories (seq-take telega-stories--show-messages
                                  telega-stories-root-view-count)))
           (list :name "stories"
                 :pretty-printer #'telega-stories--msg-pp
                 :items stories
                 :footer (unless stories
                           (telega-i18n "telega_stories_no_stories"))
                 :on-message-update #'telega-stories--rv-msg-update)))))


;; Dashboard is not required, "Emacs Stories" root view might be used
;; to view stories
(when (require 'dashboard nil 'noerror)
  (add-to-list 'dashboard-item-generators
               '(telega-stories . telega-stories-dashboard-insert))
  (add-to-list 'dashboard-item-shortcuts
               '(telega-stories . "s")))

(provide 'telega-stories)

;;; telega-stories.el ends here

;;; telega-stories.el ---   -*- lexical-binding: t -*-

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
;; Display recent [[https://t.me/emacs_stories][Emacs Stories]] in the
;; dashboard.  Enable it with:
;;
;; #+begin_src emacs-lisp
;; (require 'telega-stories)
;; (telega-stories-mode 1)
;; (add-to-list 'dashboard-items '(telega-stories . 5))
;; #+end_src
;;
;; Apart from dashboard, ~telega-stories~ provides "Emacs Stories"
;; [[https://zevlg.github.io/telega.el/#rootbuf-view-switching][Root
;; View]].  To enable this view execute {{{kbd(M-x
;; telega-view-emacs-stories RET)}}} in the root buffer.
;;
;; If you see inappropriate content in the Emacs Story, please report
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
;; And screenshot of "Emacs Stories" rooot view:
;; [[https://zevlg.github.io/telega/emacs-stories-rootview.png]]
;;
;; Customizable options:
;; - {{{user-option(telega-stories-height, 2)}}}
;; - {{{user-option(telega-stories-preload-content, 2)}}}
;; - {{{user-option(telega-stories-root-view-count, 2)}}}
;; - {{{user-option(telega-stories-root-view-keep-viewed, 2)}}}

;;; Code:

(require 'telega)

;; Customizable variables
(defgroup telega-stories nil
  "Customisation for telega-server."
  :prefix "telega-stories-"
  :group 'telega)

(defcustom telega-stories-height telega-video-note-height
  "Height in chars for Emacs Stories buttons"
  :type 'integer
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

(defcustom telega-stories-root-view-story-delimiter " "
  "Delimiter between stories in \"Emacs Stories\" rootview."
  :type 'string
  :group 'telega-stories)


;; Runtime variables
(defvar telega-stories-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "n") 'telega-button-forward)
    (define-key map (kbd "p") 'telega-button-backward)
    (define-key map (kbd "<TAB>") 'telega-button-forward)
    (define-key map (kbd "<backtab>") 'telega-button-backward)
    (define-key map (kbd "SPC") 'telega-stories-msg-goto)
    (define-key map (kbd "!") 'telega-stories-msg-report)
    map)
  "Keymap for Emacs Stories buttons.")

(defconst telega-stories-group "@emacs_stories"
  "Telegram group where stories are posted.")

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
(defvar telega-stories--story-messages nil
  "List of recently fetched non-viewed Emacs Stories.")

;;;###autoload
(define-minor-mode telega-stories-mode
  "Global mode to track Emacs Stories for updates."
  :init-value nil :global t :group 'telega-modes
  (if telega-stories-mode
      (progn
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
    (telega-stories--finalize)
    ))


;; Dashboard
(defun telega-stories--svg-image (tfile title viewed-p &optional video-p)
  "Generate story svg image with Telegram file TFLILE embedded.
VIEWED-P is non-nil if story is viewed."
  (let* ((size (telega-chars-xheight telega-stories-height))
         (sw-passive (/ size 100.0))
         (sw-active (* sw-passive 2))
         (passive-color (telega-color-name-as-hex-2digits
                         (face-foreground 'shadow)))
         ;; NOTE: one more line for the `title'
         (svg-height (+ size (telega-chars-xheight 1)))
         (svg (telega-svg-create size svg-height))
         (pclip (telega-svg-clip-path svg "pclip"))
         (base-dir(telega-base-directory)))
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
            (font-size (/ size 10)))
        (svg-text svg (format "%3d%%" (round (* progress 100)))
                  :font-size font-size
                  :font-weight "bold"
                  :fill "black"
                  :font-family "monospace"
                  ;; XXX insane X/Y calculation
                  :x (/ size 2)
                  :y (+ (/ font-size 3) (/ size 2)))))

    ;; Draw play triangle
    (when video-p
      (let ((play-size (/ size 6)))
        (svg-polygon svg (list (cons (/ (- size play-size) 2)
                                     (/ (- size play-size) 2))
                               (cons (/ (- size play-size) 2)
                                     (/ (+ size play-size) 2))
                               (cons (/ (+ size play-size) 2)
                                     (/ size 2)))
                     :fill "red"
                     :opacity "0.75")))

    ;; title
    (let ((font-size (/ size 10)))
      (svg-text svg title
                :font-size font-size
                :fill (telega-color-name-as-hex-2digits
                       (or (get-text-property 0 :color title)
                           (face-foreground 'telega-username)))
                :font-family "monospace"
                ;; XXX insane X/Y calculation
                :x (/ (- size (* (length title) (/ font-size 1.5))) 2)
                :y (- svg-height font-size)))

    (telega-svg-image svg :scale 1.0 :width size :height svg-height
                      :ascent 'center
                      :mask 'heuristic
                      :base-uri (expand-file-name "dummy" base-dir))))

(defun telega-stories--msg-create-image (msg &optional _file)
  "Create image for story message MSG."
  (cl-destructuring-bind (thumb thumb-prop)
      (telega-stories--msg-thumbnail-spec msg)

    (telega-stories--svg-image
     (telega-file--renew thumb thumb-prop)
     (let ((sender (telega-msg-sender msg)))
       (propertize (or (telega-msg-sender-username sender 'with-@)
                       (telega-msg-sender-title sender))
                   :color (car (telega-msg-sender-color sender))))
     (telega-stories--msg-viewed-p msg)
     (memq (telega--tl-type (plist-get msg :content))
           '(messageVideo messageAnimation)))))

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

(defun telega-stories--dashboard-insert-msg (msg)
  "Insert Emacs Story message MSG into dashboard."
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
    :action #'telega-stories-msg-view))

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
                       "telega-stories-mode not enabled"
                       " ---")))
        (t
         (let ((imsgs (seq-take telega-stories--story-messages list-size)))
           (if (not imsgs)
               (telega-ins--with-face 'dashboard-no-items-face
                 (telega-ins "\n    --- "
                             (telega-i18n "telega_stories_no_stories")
                             " ---"))
             (insert "\n    ")
             (dolist (msg imsgs)
               (telega-stories--dashboard-insert-msg msg)
               (telega-ins " "))))))
  (dashboard-insert-shortcut
   (dashboard-get-shortcut 'telega-stories)
   (concat (telega-i18n "telega_stories_heading") ":"))
  )


;; Emacs Stories Runtime
(defun telega-stories--initialize ()
  "Initialize Emacs Stories runtime."
  (setq telega-stories--story-messages nil)

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
  (setq telega-stories--story-messages nil)

  (when (and telega-stories--chat (telega-server-live-p))
    (telega--closeChat telega-stories--chat))
  (setq telega-stories--chat nil))

(defun telega-stories--async-fetch-stories (&optional from-msg-id
                                                      &rest msg-filters)
  "Asynchronously fetch older Emacs Story messages starting from FROM-MSG-ID.
If FROM-MSG-ID is not given, then search from the last story message
or from the beginning."
  (cl-assert telega-stories--chat)
  (when (< (length telega-stories--story-messages)
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
              (plist-get (car (last telega-stories--story-messages)) :id)
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
              (telega-stories--on-new-message story))

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

(defun telega-stories--msg-viewed-p (msg)
  "Return non-nil if Emacs Story MSG is viewed."
  (memq (plist-get msg :id)
        (telega-chat-uaprop
         (telega-msg-chat msg) :telega-stories-viewed-ids)))

(defun telega-stories-msg-view (msg)
  "View Emacs Story message MSG."
  (unless current-prefix-arg
    (let ((telega-animation-play-inline nil)) ; play animations externally
      (telega-msg-open-content msg)))

  (let ((viewed-stories (telega-chat-uaprop
                         (telega-msg-chat msg) :telega-stories-viewed-ids)))
    (unless (memq (plist-get msg :id) viewed-stories)
      ;; NOTE: Store maximum 500 ids, so `uaprop' won't grow inifinitely
      (setf (telega-chat-uaprop (telega-msg-chat msg) :telega-stories-viewed-ids)
            (seq-take (cons (plist-get msg :id) viewed-stories) 500))))

  ;; Inplace update story image in the dashboard
  (cl-destructuring-bind (thumb thumb-prop)
      (telega-stories--msg-thumbnail-spec msg)
    (telega-media--image-update
     (cons msg #'telega-stories--msg-create-image)
     (cons thumb thumb-prop)
     :telega-story-image)
    (force-window-update))

  ;; NOTE: Update the rootview *before* removing message from
  ;; `telega-stories--story-messages', because update func checks
  ;; message is story if it is in the list
  (telega-root-view--update :on-message-update msg)

  ;; Also update `telega-stories--story-messages' and possible
  ;; asynchronously fetch older stories
  (setq telega-stories--story-messages
        (delq msg telega-stories--story-messages))
  (telega-stories--async-fetch-stories)
  )

(defun telega-stories-msg-goto (msg)
  "Goto to Emacs Story message MSG, opening corresponding chat."
  ;; NOTE: Can't use `telega-msg-at' here, because button type is
  ;; `telega', not `telega-message'
  (interactive (list (button-get (button-at (point)) :value)))
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

(defun telega-stories-unview-all ()
  "Pretend non of the stories are viewed."
  (interactive)
  (unless telega-stories--chat
    (user-error "telega-stories-mode not started"))
  (setf (telega-chat-uaprop telega-stories--chat :telega-stories-viewed-ids)
        nil)

  ;; Empty root view, waiting for update
  (with-telega-root-view-ewoc "stories" ewoc
    (telega-ewoc--clean ewoc)
    (telega-ewoc--set-footer ewoc "Loading Emacs Stories.\n")
    (telega-loading--timer-start))

  ;; Refresh stories messages
  (setq telega-stories--story-messages nil)
  (telega-stories--async-fetch-stories)
  )

(defun telega-stories--msg-with-story-tag-p (msg)
  "Return non-nil if MSG has #emacs_story or #story tag."
  (let ((content (plist-get msg :content)))
    (cl-case (telega--tl-type content)
      ((messagePhoto messageVideo messageDocument)
       (string-match-p
        "#\\(emacs_\\)?story" (or (telega-tl-str content :caption) "")))
      (messageText
       (string-match-p
        "#\\(emacs_\\)?story" (or (telega-tl-str content :text) ""))))))

(defun telega-stories--msg-by-admin-p (msg)
  "Return non-nil if MSG is sent by `telega-stories-group' admin."
  (let ((sender (telega-msg-sender msg)))
    (if (telega-chat-p sender)
        ;; sent by anononimous admin
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

(defun telega-stories--msg-preload-content (msg)
  "Start downloading MSG story message's content."
  (let ((cfile (telega-msg--content-file msg)))
    (when (and (not (telega-file--downloaded-p cfile))
               (not (telega-file--downloading-p cfile)))
      (telega-file--download cfile 16))))

(defun telega-stories--on-new-message (new-msg)
  "If NEW-MSG is an unread Emacs Story message, then add it to the list.
Return non-nil if NEW-MSG has been added."
  (when (and (telega-stories-msg-story-p new-msg)
             (not (telega-stories--msg-viewed-p new-msg)))
    (telega-debug "Emacs Stories ADD story: %S" (plist-get new-msg :id))
    (cl-pushnew new-msg telega-stories--story-messages
                :test (lambda (msg1 msg2)
                        (eq (plist-get msg1 :id) (plist-get msg2 :id))))
    ;; NOTE: keep messages is message id order
    (setq telega-stories--story-messages
          (cl-sort telega-stories--story-messages #'>
                   :key (telega--tl-prop :id)))

    ;; Start preloading story content
    (when telega-stories-preload-content
      (telega-stories--msg-preload-content new-msg))

    (telega-root-view--update :on-message-update new-msg)
    t))


;; "Emacs Stories" rootview (rv = rootview)
(defun telega-stories--rv-msg-visible-p (msg)
  "Return non-nil if msg is visible in the \"Emacs Stories\" rootview."
  (or telega-stories-root-view-keep-viewed
      (not (telega-stories--msg-viewed-p msg))))

(defun telega-stories--rv-msg-pp (msg)
  "Pretty printer story message MSG in the \"Emacs Stories\" rootview."
  ;; NOTE: only visible stories should apper in the root view ewoc
  (cl-assert (telega-stories--rv-msg-visible-p msg))
  (telega-stories--dashboard-insert-msg msg)
  (telega-ins telega-stories-root-view-story-delimiter))

(defun telega-stories--rv-msg-update (_ewoc-name ewoc msg)
  "Message MSG has been updated, possible story need to be updated as well."
  (when (memq msg telega-stories--story-messages)
    ;; React only on story messages in the list
    (telega-save-cursor
      (if-let ((msg-node (telega-ewoc--find-by-data ewoc msg)))
          (if (telega-stories--rv-msg-visible-p msg)
              (ewoc-invalidate ewoc msg-node)
            (ewoc-delete ewoc msg-node))

        (when (telega-stories--rv-msg-visible-p msg)
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
      (let ((stories (ewoc-collect ewoc #'telega-stories--rv-msg-visible-p)))
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
                                (cdr (memq last-msg
                                           telega-stories--story-messages))
                              telega-stories--story-messages)))
                 (while next-msgs
                   (when (telega-stories--rv-msg-visible-p (car next-msgs))
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

(defun telega-view-emacs-stories ()
  "View recent Emacs Stories."
  (interactive)

  (telega-root-view--apply
   (list 'telega-view-emacs-stories
         (telega-i18n "telega_stories_heading")
         (let ((stories (seq-take telega-stories--story-messages
                                  telega-stories-root-view-count)))
           (list :name "stories"
                 :pretty-printer #'telega-stories--rv-msg-pp
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

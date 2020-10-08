;;; telega-core.el --- Core functionality for telega  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Mon Apr 23 18:09:01 2018
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

;; Variables, macroses, defsubst and runtime goodies for telega

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'ring)
(require 'cursor-sensor)

(require 'telega-customize)

(declare-function telega-chat--info "telega-chat" (chat))
(declare-function telega-emoji-create-svg "telega-util" (emoji &optional c-height))
(declare-function telega-chats-compare "telega-sort" (criteria chat1 chat2))

(defvar telega--lib-directory nil
  "The directory from where this library was first loaded.")

(defconst telega-chat-types
  '(private secret basicgroup supergroup bot channel)
  "All types of chats supported by telega.")

;;; Runtime variables
(defvar telega--current-buffer nil
  "Buffer currently inserting into.
Bind this to make `telega-chars-xxx' family functions to work correctly.
Becase `telega-ins--as-string' uses temporary buffer.")

(defvar telega-msg-contains-unread-mention nil
  "Bind this variable when displaying message containing unread mention.")

(defvar telega--chat nil
  "Telega chat for the current buffer.
Used in some buffers to refer chat.")
(make-variable-buffer-local 'telega--chat)

(defvar telega--help-win-param nil
  "Parameter for the `telega--help-win-redisplay-func'.
Used in help buffers to store some additional data.")
(make-variable-buffer-local 'telega--help-win-param)

(defvar telega--help-win-inserter nil
  "Inserter function for the help win.
This function accepts exactly one argument - `telega--help-win-param'.")
(make-variable-buffer-local 'telega--help-win-inserter)

(defvar telega--help-win-dirty-p nil
  "Non-nil if help win need redisplay.
Used for optimisations.")
(make-variable-buffer-local 'telega--help-win-dirty-p)

(defvar telega--me-id nil "User id of myself.")
(defvar telega--gifbot-id nil "Bot used to search for animations.")
(defvar telega--imgbot-id nil "Bot used to search for photos.")
(defvar telega--options nil "Options updated from telega-server.")
(defvar telega--auth-state nil
  "Current Authorization state.")
(defvar telega--conn-state nil
  "Current connection state.")
(defvar telega--status "Not Started" "Status of the connection to telegram.")
(defvar telega--status-aux
  "Aux status used for long requests, such as fetching chats/searching/etc")
(defvar telega--chats nil "Hash table (id -> chat) for all chats.")
(defvar telega--pinned-messages nil "Hash table (id -> msg) for all chats.")
(defvar telega--actions nil "Hash table (chat-id -> alist-of-user-actions).")
(defvar telega--ordered-chats nil "Ordered list of all chats.")
(defvar telega--filtered-chats nil
  "Chats filtered by currently active filters.
Used to calculate numbers displayed in custom filter buttons.")
(defvar telega-deleted-chats nil
  "List of recently deleted chats.
Used for \"Recently Deleted Chats\" rootview.")

(defvar telega--dirty-chats nil
  "Chats need to be updated with `telega-chat--update'.
Each element is a list, where first element is chat, and rest
is events causing chat to be dirty.")
(defvar telega--filters nil "List of active filters.")
(defvar telega--undo-filters nil "List of undo entries.")
(defvar telega--sort-criteria nil "Active sorting criteria list.")
(defvar telega--sort-inverted nil "Non-nil if sorting is inverted.")

(defvar telega--info nil "Alist of (TYPE . INFO-TABLE).")
(defvar telega--full-info nil "Alist of (TYPE . FULL-INFO-TABLE).")

(defvar telega--top-chats nil
  "Alist of (CATEGORY LAST-UPDATE-TIME ..)
CATEGORY is one of `Users', `Bots', `Groups', `Channels',
`InlineBots', `Calls'")
(defvar telega--last-buffer nil
  "Used to track buffers switching.
So we can run the code when switching from chat buffer.")
(defvar telega--stickersets nil
  "Alist of seen sticker sets.
ID -> sticker set.
Take into account that ID is the string.")
(defvar telega--stickersets-installed-ids nil
  "List of ids for installed sticker sets.
Used by `telega-stickerset-installed-p'.")
(defvar telega--stickersets-installed nil
  "List of `stickerSetInfo' for installed sticker sets.")
(defvar telega--stickersets-trending nil
  "List of trending sticker sets info.")
(defvar telega--stickersets-system nil
  "List of system sticker sets, such as animated dices, animated emojis.")
(defvar telega--stickers-favorite nil
  "List of favorite stickers.")
(defvar telega--stickers-recent nil
  "List of recently used stickers.")
(defvar telega--stickers-recent-attached nil
  "List of recently attached stickers.")
(defvar telega--animations-saved nil
  "List of saved animations.")

(defvar telega--dice-emojis nil
  "List of supported emojis for random dice messages.")

;; Searching
(defvar telega-search-history nil
  "List of recent search queries.")
(defvar telega--search-chats nil
  "Result of last `telega--searchChats' or `telega--searchChatsOnServer'.")
(defvar telega--nearby-chats nil
  "List of nearby chats returned from `telega--searchChatsNearby'.")

(defvar telega--unread-message-count nil
  "Plist with counts for unread/unmuted messages.
Props are `:unread_count' and `:unread_unmuted_count'")
(defvar telega--unread-chat-count nil
  "Plist with counts for unread/unmuted chats.
Props are `:unread_count', `:unread_unmuted_count', `:marked_as_unread_count'
and `:marked_as_unread_unmuted_count'")

(defvar telega--chat-buffers-alist nil
  "Alist of chats and corresponding chatbuf.")
(defun telega-chat-buffers ()
  "Return list of all chatbufs."
  (mapcar #'cdr telega--chat-buffers-alist))

(defvar telega--files nil
  "Files hash FILE-ID -> (list FILE UPDATE-CALBACKS..).")
(defvar telega--files-updates nil
  "Hash of FILE-ID -> (list-of (UPDATE-CB CB-ARGS))
UPDATE-CB is callback to call when file updates.
UPDATE-CB is called with FILE and CB-ARGS as arguments.
UPDATE-CB should return non-nil to be removed after its being called.")
(defvar telega--proxy-pings nil
  "Alist for the proxy pings.
In form (PROXY-ID . TIMESTAMP SECONDS)")
(defvar telega-voip--alist nil
  "Alist of all calls currently in processing.
In form (ID . CALL)")
(defvar telega-voip--active-call nil
  "Currently active call.
Active call is either outgoing call or accepted incoming call.
Only one call can be currently active.")
(defvar telega--scope-notification-alist (cons nil nil)
  "Default notification settings for chats.
alist where key is one of `private', `group' or `channel'.")

(defvar telega-tdlib--chat-filters nil
  "List of chat filters received from TDLib.")
(defvar telega-tdlib--chat-list nil
  "Active tdlib chat list used for ordering.")

;; Minibuffer stuff used by chatbuf and stickers
(defvar telega-minibuffer--choices nil
  "Bind to list of choices.
Each element in form: (NAME SSET-ID)")
(defvar telega-minibuffer--chat nil
  "Bind to chat currently active.")
(defvar telega-minibuffer--string nil
  "Bind to Saved string entered to minibuffer.")

(defvar telega--ignored-messages-ring (make-ring 0)
  "Ring of ignored messages.
Use \\[execute-extended-command] telega-ignored-messages RET to
display the list.")


;;; Shared chat buffer local variables
(defvar telega-chatbuf--chat nil
  "Telega chat for the current chat buffer.")
(make-variable-buffer-local 'telega-chatbuf--chat)

(defun telega-chatbuf--chat (buffer)
  "Return chat corresponding chat BUFFER."
  (buffer-local-value 'telega-chatbuf--chat buffer))

(defvar telega-chatbuf--messages nil
  "Local cache for the messages.")
(make-variable-buffer-local 'telega-chatbuf--messages)

(defvar telega-chatbuf--marked-messages nil
  "List of marked messages.")
(make-variable-buffer-local 'telega-chatbuf--marked-messages)

(defvar telega-chatbuf--inline-query nil
  "Non-nil if some inline bot has been requested.
Actual value is `:@extra` value of the call to inline bot.")
(make-variable-buffer-local 'telega-chatbuf--inline-query)

(defvar telega-chatbuf--input-marker nil)
(make-variable-buffer-local 'telega-chatbuf--input-marker)

(defvar telega-chatbuf--administrators nil
  "List of administrators in chatbuf chat.
Asynchronously loaded when chatbuf is created.")
(make-variable-buffer-local 'telega-chatbuf--administrators)


(defun telega--init-vars ()
  "Initialize runtime variables.
Done when telega server is ready to receive queries."
  (setq telega--auth-state nil)
  (setq telega--conn-state nil)
  (setq telega--status "Disconnected")
  (setq telega--status-aux "")
  (setq telega--me-id -1)
  (setq telega--gifbot-id nil)
  (setq telega--imgbot-id nil)
  (setq telega--options
        ;; default limits
        (list :message_caption_length_max 1024
              :message_text_length_max 4096))
  (setq telega--chats (make-hash-table :test #'eq))
  (setq telega--pinned-messages (make-hash-table :test #'eq))
  (setq telega--top-chats nil)

  (setq telega--search-chats nil)
  (setq telega--nearby-chats nil)

  (setq telega-deleted-chats nil)
  (setq telega--ordered-chats nil)
  (setq telega--filtered-chats nil)
  (setq telega--dirty-chats nil)
  (setq telega--actions (make-hash-table :test 'eq))
  (setq telega--filters nil)
  (setq telega--undo-filters nil)
  (setq telega--sort-criteria nil)
  (setq telega--sort-inverted nil)
  (setq telega--info
        (list (cons 'user (make-hash-table :test 'eq))
              (cons 'secretChat (make-hash-table :test 'eq))
              (cons 'basicGroup (make-hash-table :test 'eq))
              (cons 'supergroup (make-hash-table :test 'eq))))
  (setq telega--full-info
        (list (cons 'user (make-hash-table :test 'eq))
              (cons 'basicGroup (make-hash-table :test 'eq))
              (cons 'supergroup (make-hash-table :test 'eq))))

  (setq telega--ignored-messages-ring
        (make-ring telega-ignored-messages-ring-size))
  (setq telega--unread-message-count nil)
  (setq telega--unread-chat-count nil)

  (setq telega--files (make-hash-table :test 'eq))
  (setq telega--files-updates (make-hash-table :test 'eq))

  (setq telega-voip--alist nil)
  (setq telega-voip--active-call nil)

  (setq telega--proxy-pings nil)
  (setq telega--scope-notification-alist nil)

  (setq telega--stickersets nil)
  (setq telega--stickersets-installed-ids nil)
  (setq telega--stickersets-installed nil)
  (setq telega--stickersets-trending nil)
  (setq telega--stickersets-system nil)
  (setq telega--stickers-favorite nil)
  (setq telega--stickers-recent nil)
  (setq telega--stickers-recent-attached nil)
  (setq telega--animations-saved nil)
  (setq telega--dice-emojis nil)

  (setq telega-tdlib--chat-filters nil)
  (setq telega-tdlib--chat-list nil)
  )

(defun telega-test-env (&optional quiet-p)
  "Test Emacs environment.
If QUIET-P is non-nil, then show success message in echo area.
Return non-nil if all tests are passed."
  (interactive "P")
  ;; 62bits for numbers is required
  ;; i.e. ./configure --with-wide-int
  (cl-assert (= most-positive-fixnum 2305843009213693951) nil
             "Emacs with wide ints (--with-wide-int) is required")
  (cl-assert (= (string-to-number "542353335") 542353335) nil
             (concat "Emacs with `(string-to-number \"542353335\") ==> 542353335'"
                     " is required"))

  ;; at least 25.1 emacs is required
  ;; see https://t.me/emacs_telega/1592
  (cl-assert (fboundp 'cursor-intangible-mode) nil
             "Emacs with `cursor-intangible-mode' is required")

  ;; For now stick with at least 26.1 Emacs
  (cl-assert (string-version-lessp "26.0" emacs-version) nil
             (format "At least Emacs 26.0 is required, but you have %s"
                     emacs-version))

  ;; imagemagick for images NOT required, we have now fallback in case
  ;; native image transforms available (newer Emacs)
  (cl-assert (or (image-type-available-p 'imagemagick)
                 (if (telega-x-frame)
                     (and (fboundp 'image-transforms-p)
                          (funcall 'image-transforms-p))
                   ;; For TTY-only emacs, images are not required
                   t))
             (concat "Emacs with `imagemagick' support is required."
                     " (libmagickcore, libmagickwand, --with-imagemagick)"))
  (cl-assert (image-type-available-p 'svg) nil
             "Emacs with `svg' support is required. (librsvg)")
  (unless quiet-p
    (message "Your Emacs is suitable to run telega.el"))
  t)

(defmacro telega-save-excursion (&rest body)
  "Execute BODY saving current point as moving marker."
  (declare (indent 0))
  (let ((pnt-sym (gensym)))
    `(let ((,pnt-sym (copy-marker (point) t)))
       (unwind-protect
           (progn ,@body)
         (goto-char ,pnt-sym)))))

(defmacro telega-save-cursor (&rest body)
  "Execute BODY saving cursor's line and column position."
  (declare (indent 0))
  (let ((line-sym (gensym "line"))
        (col-sym (gensym "col")))
    `(let ((,line-sym (+ (if (bolp) 1 0) (count-lines 1 (point))))
           (,col-sym (current-column)))
       (unwind-protect
           (progn ,@body)
         (goto-char (point-min))
         (cl-assert (> ,line-sym 0))
         (forward-line (1- ,line-sym))
         (move-to-column ,col-sym)))))

(defmacro with-telega-debug-buffer (&rest body)
  "Execute BODY only if `telega-debug' is non-nil, making debug buffer current."
  `(when telega-debug
     (with-current-buffer (get-buffer-create "*telega-debug*")
       (telega-save-excursion
         ,@body))))

(defmacro with-telega-root-buffer (&rest body)
  "Execute BODY setting current buffer to root buffer.
Inhibits read-only flag."
  (declare (indent 0))
  `(when (buffer-live-p (telega-root--buffer))
     (with-current-buffer telega-root-buffer-name
       (let ((inhibit-read-only t))
         (unwind-protect
             (progn ,@body)
           (set-buffer-modified-p nil))))))

(defmacro with-telega-chatbuf (chat &rest body)
  "Execute BODY setting current buffer to chat buffer of CHAT.
Executes BODY only if chat buffer already exists.
If there is no corresponding buffer, then do nothing.
Inhibits read-only flag."
  (declare (indent 1))
  (let ((bufsym (cl-gensym "buf"))
        (chatsym (cl-gensym "chat")))
    `(let* ((,chatsym ,chat)
            (,bufsym (if (and telega-chatbuf--chat
                              (eq telega-chatbuf--chat ,chatsym))
                         (current-buffer)
                       (cdr (assq ,chatsym telega--chat-buffers-alist)))))
       (when (buffer-live-p ,bufsym)
         (with-current-buffer ,bufsym
           (let ((inhibit-read-only t)
                 (buffer-undo-list t))
             ,@body))))))

(defmacro with-telega-help-win (buffer-or-name &rest body)
  "Execute BODY in help BUFFER-OR-NAME."
  (declare (indent 1))
  `(progn
     (with-help-window ,buffer-or-name)
     (redisplay)
     (with-help-window ,buffer-or-name
       (set-buffer standard-output)
       (cursor-sensor-mode 1)
       ,@body)))

(defun telega-help-win--maybe-redisplay (buffer-or-name for-param)
  "Possible redisplay help win with BUFFER-OR-NAME.
If BUFFER-OR-NAME exists and visible then redisplay it."
  (when-let ((help-buf (get-buffer buffer-or-name)))
    (with-current-buffer help-buf
      (when (and (eq for-param telega--help-win-param)
                 telega--help-win-inserter)
        (if-let ((help-win (get-buffer-window help-buf)))
            ;; Buffer is visible in some HELP-WIN
            (let ((w-start (window-start help-win))
                  (w-point (window-point help-win)))
              (telega-save-excursion
                (let ((inhibit-read-only t))
                  (setq telega--help-win-dirty-p nil)
                  (erase-buffer)
                  (funcall telega--help-win-inserter
                           telega--help-win-param)))
              (set-window-start help-win w-start)
              (set-window-point help-win w-point))

          ;; Buffer is not visible, mark it as dirty, so it will be
          ;; redisplayed when switched in
          (setq telega--help-win-dirty-p t))))))

(defmacro telega-help-message (help-name fmt &rest fmt-args)
  "Show once help message formatted with FMT and FMT-ARGS.
Show message only if `telega-help-messages' is non-nil."
  (declare (indent 2))
  `(when (and telega-help-messages
              (not (get 'telega-help-messages ,help-name)))
     (put 'telega-help-messages ,help-name t)
     (message (concat "Telega: " ,fmt) ,@fmt-args)))

(defsubst telega-debug (fmt &rest args)
  "Insert formatted string into debug buffer.
FMT and ARGS are passed directly to `format'."
  (with-telega-debug-buffer
   (goto-char (point-max))
   (insert (apply 'format (cons (concat "%d: " fmt "\n")
                                (cons (telega-time-seconds) args))))))

(defmacro telega--tl-type (tl-obj)
  `(intern (plist-get ,tl-obj :@type)))

(defmacro telega--tl-error-p (tl-obj)
  "Return non-nil if TL-OBJ is error object."
  `(eq (telega--tl-type ,tl-obj) 'error))

(defmacro telega--tl-get (obj prop1 &rest props)
  "`plist-get' which works with multiple arguments.
For example:
`(telega--tl-get obj :prop1 :prop2)' is equivalent to
`(plist-get (plist-get obj :prop1) :prop2)`"
  (let ((ret `(plist-get ,obj ,prop1)))
    (dolist (prop props)
      (setq ret (list 'plist-get ret prop)))
    ret))

(defmacro telega--tl-prop (prop1 &rest props)
  "Generate function to get property by PROP1 and PROPS.
Uses `telega--tl-get' to obtain the property."
  (let ((tl-obj-sym (cl-gensym "tl-obj")))
    `(lambda (,tl-obj-sym)
       (telega--tl-get ,tl-obj-sym ,prop1 ,@props))))

(defmacro telega--tl-dolist (bind &rest body)
  "Execute BODY by traversing plist.
BIND is in form ((PROP VAL) PLIST)."
  (declare (indent 1))
  `(cl-loop for ,(car bind) on ,(cadr bind)
            by #'cddr
            do (progn ,@body)))

(defsubst telega-file--ensure (file)
  "Ensure FILE is in `telega--files'.
Return FILE."
  (when telega-debug
    (cl-assert file))
  (puthash (plist-get file :id) file telega--files)
  file)

(defsubst telega-file--size (file)
  "Return FILE size."
  ;; NOTE: fsize is 0 if unknown, in this case esize is approximate
  ;; size
  (let ((fsize (plist-get file :size))
        (esize (plist-get file :expected_size)))
    (if (zerop fsize) esize fsize)))

(defsubst telega-file--downloaded-p (file)
  "Return non-nil if FILE has been downloaded."
  (telega--tl-get file :local :is_downloading_completed))

(defsubst telega-file--downloading-p (file)
  "Return non-nil if FILE is downloading right now."
  (telega--tl-get file :local :is_downloading_active))

(defsubst telega-file--can-download-p (file)
  "Return non-nil if FILE can be downloaded.
May return nil even when `telega-file--downloaded-p' returns non-nil."
  (telega--tl-get file :local :can_be_downloaded))

(defsubst telega-file--need-download-p (file)
  (and (telega-file--can-download-p file)
       (not (telega-file--downloaded-p file))))

(defsubst telega-file--downloading-progress (file)
  "Return progress of FILE downloading as float from 0 to 1."
  (color-clamp (/ (float (telega--tl-get file :local :downloaded_size))
                  (telega-file--size file))))

(defsubst telega-file--uploaded-p (file)
  "Return non-nil if FILE has been uploaded."
  (telega--tl-get file :remote :is_uploading_completed))

(defsubst telega-file--uploading-p (file)
  "Return non-nil if FILE is uploading right now."
  (telega--tl-get file :remote :is_uploading_active))

(defsubst telega-file--uploading-progress (file)
  "Return progress of FILE uploading as float from 0 to 1."
  (color-clamp (/ (float (telega--tl-get file :remote :uploaded_size))
                  (telega-file--size file))))

(defsubst telega--desurrogate-apply-part (part &optional keep-properties)
  "Apply PART's `telega-display'"
  (let ((part-display (get-text-property 0 'telega-display part)))
    (cond (part-display
           (if keep-properties
               ;; keep all properties except for `telega-display'
               ;; Apply `telega-emoji-p' property as well
               (let* ((part-props0 (telega-plist-del
                                    (text-properties-at 0 part) 'telega-display))
                      (emoji-p (plist-get part-props0 'telega-emoji-p))
                      (part-props1 (telega-plist-del part-props0 'telega-emoji-p))
                      ;; NOTE: we always create new cell for 'display
                      ;; property as in `image-insert', see comment
                      ;; about this in `image-insert' sources
                      (addon-props (when (and emoji-p
                                              telega-use-images
                                              telega-emoji-use-images)
                                     (list 'rear-nonsticky '(display)
                                           'display (cons 'image (cdr (telega-emoji-create-svg part-display)))))))
                 (apply 'propertize part-display (nconc part-props1 addon-props)))
             part-display))
          (keep-properties part)
          (t (substring-no-properties part)))))

(defsubst telega--desurrogate-apply-part-keep-properties (part)
  (telega--desurrogate-apply-part part 'keep-props))

(defsubst telega--desurrogate-apply (str &optional no-properties)
  "Apply `telega-display' properties to STR.
Resulting in new string with no surrogate pairs.
If NO-PROPERTIES is specified, then do not keep text properties."
  (mapconcat (if no-properties
                 #'telega--desurrogate-apply-part
               #'telega--desurrogate-apply-part-keep-properties)
             (telega--split-by-text-prop str 'telega-display) ""))

(defsubst telega--tl-unpack (obj)
  "Unpack TL object OBJ."
  obj)

(defsubst telega--tl-pack (obj)
  "Pack object OBJ."
  ;; Remove text props from strings, etc
  (cond ((stringp obj) (substring-no-properties obj))
        ((vectorp obj) (cl-map 'vector #'telega--tl-pack obj))
        ((listp obj) (mapcar #'telega--tl-pack obj))
        (t obj)))

(defun telega-tl-str (obj prop &optional no-properties)
  "Get property PROP from OBJ, desurrogating resulting string.
NO-PROPERTIES is passed directly to `telega--desurrogate-apply'.
Return nil for empty strings.
Also supports \"formattedText\" a value of the OBJ's PROP."
  (let* ((prop-val (plist-get obj prop))
         (text (if (and (listp prop-val)
                        (equal (plist-get prop-val :@type) "formattedText"))
                   (telega--fmt-text-faces prop-val)
                 prop-val))
         (ret (telega--desurrogate-apply text no-properties)))
    (unless (string-empty-p ret)
      ret)))

(defsubst telega-me-p (chat-or-user)
  "Return non-nil if CHAT-OR-USER is me."
  (eq telega--me-id (plist-get chat-or-user :id)))

(defmacro telega-svg-create (&rest args)
  ;; See https://t.me/emacs_telega/13764
  `(svg-create ,@args :xmlns:xlink "http://www.w3.org/1999/xlink"))


;;; Formatting
(defun telega-fmt-eval-fill (estr attrs)
  "Fill ESTR to `:fill-column' value from ATTRS.
Keeps newlines in ESTR.
Return list of strings."
  (let ((fill-column (- (or (plist-get attrs :fill-column) fill-column)
                        (length (plist-get attrs :fill-prefix)))))
    (apply #'nconc
           (mapcar (if (plist-get attrs :fill)
                       (lambda (str)
                         (split-string
                          (with-temp-buffer
                            (insert str)
                            (fill-region (point-min) (point-max)
                                         (plist-get attrs :fill) t)
                            (buffer-substring (point-min) (point-max)))
                          "\n"))
                     #'list)
                   (split-string estr "\n")))))

(defun telega-fmt-eval-truncate (estr attrs)
  "Apply `:max', `:elide-string' and `:elide-trail' properties from ATTRS to ESTR."
  (let* ((max (plist-get attrs :max))
         ;; NOTE: always apply elide
;         (elide (plist-get attrs :elide))
         (elide-str (or (plist-get attrs :elide-string) telega-symbol-eliding))
         (elide-trail (or (plist-get attrs :elide-trail) 0))
         (estr-trail (if (> elide-trail 0) (substring estr (- elide-trail)) ""))
         (estr-lead (truncate-string-to-width
                     estr (- max (string-width elide-str) elide-trail))))
    (concat estr-lead elide-str estr-trail)))
    ;;      result)
    ;; ;; Correct truncstr in case of multibyte chars
    ;; (while (and (not (string-empty-p estr-lead))
    ;;             (< max (string-width
    ;;                     (setq result (concat estr-lead elide-str estr-trail)))))
    ;;   (setq estr-lead (substring estr-lead 0 -1)))

    ;; result))

(defun telega-fmt-eval-align (estr attrs)
  "Apply `:min', `:align' and `:align-symbol' properties from ATTRS to ESTR."
  (let* ((min (plist-get attrs :min))
         (width (- min (string-width estr)))
         (align (plist-get attrs :align))
         (align-symbol (or (plist-get attrs :align-symbol) " "))
         (left "")
         (right ""))
    ;; Grow `left' and `right' until they have required width
    (while (< (string-width left) (/ width 2))
      (setq left (concat left align-symbol)))
    (while (< (string-width right) (- width (/ width 2)))
      (setq right (concat right align-symbol)))

    (cl-ecase align
      (left (concat estr left right))
      (right (concat left right estr))
      ((center centre) (concat left estr right)))))

(defun telega-fmt-eval-min-max (estr attrs)
  "Apply `:min' and `:max' properties from ATTRS to ESTR."
  (let ((max (plist-get attrs :max))
        (min (plist-get attrs :min))
        (estr-width (string-width estr)))
    (cond ((and max (> estr-width max))
           (telega-fmt-eval-truncate estr attrs))
          ((and min (< estr-width min))
           (telega-fmt-eval-align estr attrs))
          (t estr))))

(defun telega-fmt-eval-face (estr attrs)
  "Apply `:face' attribute from ATTRS to ESTR."
  (let ((face (plist-get attrs :face)))
    (when face
      (add-face-text-property 0 (length estr) face t estr))
    estr))

(defun telega-fmt-eval-attrs (estr attrs)
  "Apply all attributes ATTRS to ESTR."
  ;; Blackmagic for fast execution, but
  ;; NOTE:
  ;;  - Do not prefix first line
  ;;  - Do not prefix empty lines with blank prefix
  ;;  - If last string is empty, do not prefix it
  (let* ((fpx (plist-get attrs :fill-prefix))
         (fpx-blank-p (or (not fpx) (string-blank-p fpx)))
         (filled-estrs (telega-fmt-eval-fill estr attrs))
         (formatted-estrs (list (telega-fmt-eval-min-max
                                 (pop filled-estrs) attrs)))
         (festr-tail formatted-estrs))
    (while filled-estrs
      (let* ((estr (car filled-estrs))
             (estr-last-p (not (cdr filled-estrs)))
             (festr-elem
              (list (telega-fmt-eval-min-max
                     (concat (unless (and (string-empty-p estr)
                                          (or estr-last-p fpx-blank-p))
                               fpx)
                             estr)
                     attrs))))
        (setcdr festr-tail festr-elem)
        (setq festr-tail festr-elem)
        (setq filled-estrs (cdr filled-estrs))))
    (telega-fmt-eval-face
     (mapconcat #'identity formatted-estrs "\n")
     attrs)))

(defsubst telega-fmt-atom (atom)
  "Convert ATOM to string.
NIL yields empty string for the convenience."
  (cond ((stringp atom) atom)
        ((null atom) "")
        (t (with-output-to-string
             (princ atom)))))

(defun telega-fmt-eval-elem (elem value)
  "Format single element ELEM with VALUE."
  (let (attrs)
    (when (and (not (functionp elem)) (listp elem))
      (setq attrs (cdr elem)
            elem (car elem)))

    (telega-fmt-eval-attrs
     (cond ((functionp elem)
            (telega-fmt-atom (funcall elem value)))
           ((symbolp elem)
            (telega-fmt-atom (symbol-value elem)))
           ((listp elem)
            (telega-fmt-eval elem value))
           (t (telega-fmt-atom elem)))
     attrs)))

(defun telega-fmt-eval (fmt-spec value)
  "Evaluate simple format FMT-SPEC, applying it to VALUE."
  (when (functionp fmt-spec)
    (setq fmt-spec (funcall fmt-spec value)))

  (let ((fmt-result (if (stringp fmt-spec) fmt-spec "")))
    (while (consp fmt-spec)
      (when (car fmt-spec)
        (setq fmt-result
              (concat fmt-result
                      (telega-fmt-eval-elem (car fmt-spec) value))))
      (setq fmt-spec (cdr fmt-spec)))
    fmt-result))

(defsubst telega--time-at00 (timestamp &optional decoded-ts)
  "Return time at 00:00:001 at TIMESTAMP's day.
Optional DECODED-TS is the result of already applied `decode-time'."
  (let ((dt (or decoded-ts (decode-time timestamp))))
    (1+ (- (floor timestamp) (* 3600 (nth 2 dt)) (* 60 (nth 1 dt)) (nth 0 dt)))))

;;; Buttons for telega
(defun telega-button--ins-error (_val)
  "Default inserter for the `telega' button."
  (error "Button `:inserter' is unset"))

;; Make 'telega-button be separate (from 'button) type
(put 'telega-button 'type 'telega)
(put 'telega-button 'keymap button-map)
(put 'telega-button 'action 'telega-button--action)
(put 'telega-button 'rear-nonsticky t)
(put 'telega-button 'face nil)
(put 'telega-button :inserter 'telega-button--ins-error)
(put 'telega-button :value nil)
(put 'telega 'button-category-symbol 'telega-button)

(defun telega-button--action (button)
  "Run BUTTON's `:action' function on its `:value'.
Return t if `:action' has been called."
  (when-let ((telega-action (button-get button :action)))
    (funcall telega-action (button-get button :value))
    t))

(defun telega-button--sensor-func (_window oldpos dir)
  "Function to be used in `cursor-sensor-functions' text property.
Activates button if cursor enter, deactivates if leaves."
  (let ((inhibit-read-only t)
        (button-region (telega--region-with-cursor-sensor
                        (if (eq dir 'entered) (point) oldpos))))
    (when button-region
      (put-text-property (car button-region) (cdr button-region)
                         'face (if (eq dir 'entered)
                                   'telega-button-active
                                 'telega-button))
      (when (eq dir 'entered)
        (telega-button--help-echo (car button-region)))
      )))

;; `:help-echo' is also available for buttons
(defun telega-button--help-echo (button)
  "Show help message for BUTTON defined by `:help-echo' property."
  (let ((help-echo (or (button-get button :help-echo)
                       (button-get button 'help-echo))))
    (when (functionp help-echo)
      (setq help-echo (funcall help-echo (button-get button :value))))
    (when help-echo
      (message "%s" (eval help-echo)))))

(defun telega-button--insert (button-type value &rest props)
  "Insert telega button of BUTTON-TYPE with VALUE and PROPS."
  (declare (indent 2))
  (let ((button (apply 'make-text-button
                       (prog1 (point)
                         (funcall (or (plist-get props :inserter)
                                      (button-type-get button-type :inserter))
                                  value))
                       (point)
                       :type button-type
                       :value value
                       props)))
    (button-at button)))

(defmacro telega-button--change (button new-button)
  "Change BUTTON to NEW-BUTTON."
  (declare (indent 1))
  (let ((newbutton (gensym "newbutton")))
    `(let ((inhibit-read-only t))
       (goto-char (button-start ,button))
       (let ((,newbutton ,new-button))
         (delete-region (point) (button-end ,button))
         (set-marker ,button ,newbutton)))))

(defun telega-button--update-value (button new-value &rest props)
  "Update BUTTON's value to NEW-VALUE.
Additional properties PROPS are updated in button."
  (telega-button--change button
    (apply 'telega-button--insert (button-type button) new-value props)))

(defun telega-button--observable-p (button)
  "Return non-nil if BUTTON is observable in some window.
I.e. shown in some window, see `pos-visible-in-window-p'."
  (when (markerp button)
    (let ((bwin (get-buffer-window (marker-buffer button))))
      (and bwin
           (telega-focus-state (window-frame bwin))
           (pos-visible-in-window-p button bwin)))))

(defun telega-button--make-observable (button)
  "Make BUTTON observable in window."
  (unless (and (pos-visible-in-window-p (button-start button))
               (pos-visible-in-window-p (button-end button)))
    ;; NOTE:
    ;; - Button is not fully visible, recenter to make it
    ;;   visible
    ;; - `recenter' might signal error
    (let ((nlines (count-lines (button-start button) (button-end button))))
      (if (>= nlines (/ (window-height) 2))
          (ignore-errors (recenter (- nlines)))
        (ignore-errors (recenter))))))

(defun telega-button-forward (n &optional predicate no-error)
  "Move forward to N visible/active button.
If PREDICATE is specified, then forward only buttons for which
PREDICATE returns non-nil.  PREDICATE is called with single arg -
button.
If NO-ERROR, do not signal error if no further buttons could be
found.
If NO-ERROR is `recenter', then possible recenter, otherwise recenter only if NO-ERROR is nil."
  (declare (indent 1))
  (interactive "p")
  (let (button)
    (dotimes (_ (abs n))
      ;; NOTE: In Emacs26, there is no `no-error' argument for
      ;; `forward-button', so we use `ignore-errors' instead
      ;; See: https://t.me/emacs_telega/11931
      (while (and (setq button (if no-error
                                   (ignore-errors (forward-button (cl-signum n)))
                                 (forward-button (cl-signum n))))
                  (or (and predicate
                           (not (funcall predicate button)))
                      (button-get button 'invisible)
                      (button-get button 'inactive)))))

    ;; NOTE: Non-nil `no-error' is normally given on non-interactive
    ;; calls, so recenter only on interactive calls
    (when (and button (or (not no-error) (eq no-error 'recenter)))
      (telega-button--make-observable button)
      (telega-button--help-echo button))
    button))

(defun telega-button-backward (n &optional predicate no-error)
  "Move backward to N visible/active button.
PREDICATE and NO-ERROR are passed to `telega-button-forward'."
  (interactive "p")
  (telega-button-forward (- n) predicate no-error))


;;; Chats part
(defmacro telega-chat-uaprop-del (chat uaprop-name)
  "Delete custom CHAT property named UAPROP-NAME."
  `(telega-chat--set-uaprops
    ,chat (telega-plist-del (plist-get ,chat :uaprops) ,uaprop-name)))

(defmacro telega-chat-uaprop (chat uaprop-name)
  "Return value for CHAT's custom property with name UAPROP-NAME."
  `(plist-get (plist-get ,chat :uaprops) ,uaprop-name))

(gv-define-setter telega-chat-uaprop (value chat uaprop-name)
  "Set CHAT's user property UAPROP-NAME to VALUE.
Return VALUE."
  (let ((valsym (gensym "value")))
    `(let ((,valsym ,value))
       (telega-chat--set-uaprops
        ,chat (plist-put (plist-get ,chat :uaprops) ,uaprop-name ,valsym))
       ,valsym)))

(defsubst telega-chat-username (chat)
  "Return CHAT's username.
Return nil if no username is assigned to CHAT."
  (telega-tl-str (telega-chat--info chat) :username))

(defun telega-chat-position--list-name (position &optional no-props)
  "Return list name for the POSITION.
If NO-PROPS is non-nil, then remove properties from the resulting string."
  (let ((pos-list (plist-get position :list)))
    (if (eq (telega--tl-type pos-list) 'chatListFilter)
        (telega-tl-str (cl-find (plist-get pos-list :chat_filter_id)
                                telega-tdlib--chat-filters
                                :key (telega--tl-prop :id))
                       :title no-props)
      (intern (downcase (substring (plist-get pos-list :@type) 8))))))

(defun telega-chat-position (chat)
  "Return CHAT position in current `telega-tdlib--chat-list'."
  (cl-find telega-tdlib--chat-list (plist-get chat :positions)
           :key (telega--tl-prop :list)
           :test #'equal))

(defun telega-chat-at (&optional pos)
  "Return current chat at point."
  (let ((button (button-at (or pos (point)))))
    (when (and button (eq (button-type button) 'telega-chat))
      (button-get button :value))))

(defun telega-chat-order (chat &optional ignore-custom)
  "Return CHAT's order as string.
Order from `telega-tdlib--chat-list' position is used.
If CHAT has custom order, then return its custom order."
  (or (unless ignore-custom
        (telega-chat-uaprop chat :order))
      (plist-get (telega-chat-position chat) :order)
      "0"))

(defsubst telega-chat> (chat1 chat2)
  "Compare CHAT1 with CHAT2 according to `telega--sort-criteria'.
Return if CHAT1 is greater than CHAT2."
  (telega-chats-compare telega--sort-criteria
                        (if telega--sort-inverted chat2 chat1)
                        (if telega--sort-inverted chat1 chat2)))

(defsubst telega-chatbuf-has-input-p ()
  "Return non-nil if chatbuf has some input."
  (< telega-chatbuf--input-marker (point-max)))

(defsubst telega-chatbuf-input-string ()
  "Return non-nil if chatbuf has some input."
  (buffer-substring telega-chatbuf--input-marker (point-max)))

(defsubst telega-chatbuf--input-delete ()
  "Delete chatbuf's input."
  (delete-region telega-chatbuf--input-marker (point-max)))

(defsubst telega-chatbuf--input-draft-p ()
  "Return non-nil if chatbuf input is the draft.
Draft input is the input that have `:draft-input-p' property on both sides."
  (and (telega-chatbuf-has-input-p)
       (get-text-property telega-chatbuf--input-marker :draft-input-p)
       (get-text-property (1- (point-max)) :draft-input-p)))

(defmacro telega-chat-nearby-find (chat-id)
  "Find nearby chat in `telega--nearby-chats' by CHAT-ID."
  `(cl-find ,chat-id telega--nearby-chats :key (telega--tl-prop :chat_id)))

(defun telega-chat-nearby--ensure (nearby-chat)
  "Ensure NEARBY-CHAT is in `telega--nearby-chats'."
  (let ((nb-chat (telega-chat-nearby-find (plist-get nearby-chat :chat_id))))
    (if nb-chat
        (plist-put nb-chat :distance (plist-get nearby-chat :distance))
      (setq nb-chat nearby-chat)
      (setq telega--nearby-chats (cons nearby-chat telega--nearby-chats)))
    nb-chat))

(defun telega-chat-nearby-distance (chat)
  "Return distance in meters to the CHAT.
Return non-nil only if CHAT is nearby."
  (plist-get (telega-chat-nearby-find (plist-get chat :id)) :distance))


;;; Inserters part
(defun telega-ins (&rest args)
  "Insert all strings in ARGS.
Return non-nil if something has been inserted."
  (< (prog1 (point) (apply 'insert (cl-remove-if 'null args))) (point)))

(defmacro telega-ins-fmt (fmt &rest args)
  "Insert string formatted by FMT and ARGS.
Return t."
  (declare (indent 1))
  `(telega-ins (format ,fmt ,@args)))

(defmacro telega-ins-i18n (key &rest args)
  (declare (indent 1))
  `(telega-ins (telega-i18n ,key ,@args)))

(defmacro telega-ins--as-string (&rest body)
  "Execute BODY inserters and return result as a string."
  `(with-temp-buffer
     ,@body
     (buffer-string)))

(defmacro telega-ins--one-lined (&rest body)
  "Execute BODY making insertation one-lined.
It makes one line by replacing all newlines by spaces."
  `(telega-ins
    (replace-regexp-in-string
     "\n" " " (telega-ins--as-string ,@body))))

(defmacro telega-ins--with-attrs (attrs &rest body)
  "Execute inserters BODY applying ATTRS after insertation.
Return t."
  (declare (indent 1))
  `(telega-ins
    (telega-fmt-eval-attrs (telega-ins--as-string ,@body) ,attrs)))

(defmacro telega-ins--with-face (face &rest body)
  "Execute BODY highlighting result with FACE."
  (declare (indent 1))
  `(telega-ins--with-attrs (list :face ,face)
     ,@body))

(defmacro telega-ins--column (column fill-col &rest body)
  "Execute BODY at COLUMN filling to FILL-COL.
If COLUMN is nil or less then current column, then current column is used."
  (declare (indent 2))
  (let ((colsym (gensym "col"))
        (curcol (gensym "curcol")))
    `(let ((,colsym ,column)
           (,curcol (telega-current-column)))
       (when (or (null ,colsym) (< ,colsym ,curcol))
         (setq ,colsym ,curcol))

       (telega-ins (make-string (- ,colsym ,curcol) ?\s))
;       (move-to-column ,colsym t)
       (telega-ins--with-attrs
           (list :fill 'left
                 :fill-prefix (make-string ,colsym ?\s)
                 :fill-column ,fill-col)
         ,@body))))

(defmacro telega-ins--labeled (label fill-col &rest body)
  "Execute BODY filling it to FILL-COL, prefixing first line with LABEL."
  (declare (indent 2))
  `(progn
     (telega-ins ,label)
     (telega-ins--column nil ,fill-col
       ,@body)))

(defmacro telega-ins--raw-button (props &rest body)
  "Execute BODY creating text button with PROPS."
  (declare (indent 1))
  `(button-at (apply 'make-text-button (prog1 (point) ,@body) (point)
                     ,props)))

(defmacro telega-ins--with-props (props &rest body)
  "Execute inserters applying PROPS after insertation.
Return what BODY returns."
  (declare (indent 1))
  (let ((spnt-sym (gensym "pnt")))
    `(let ((,spnt-sym (point)))
       (prog1
           (progn ,@body)
         (add-text-properties ,spnt-sym (point) ,props)))))

(defmacro telega-ins-prefix (prefix &rest body)
  "In case BODY inserted anything then PREFIX is also inserted before BODY."
  (declare (indent 1))
  (let ((spnt-sym (gensym "pnt")))
    `(let ((,spnt-sym (point)))
       (when (progn ,@body)
         (save-excursion
           (goto-char ,spnt-sym)
           (telega-ins ,prefix))))))

(defun telega-ins--move-to-column (column)
  "Insert space aligned to COLUMN.
Uses `:align-to' display property."
  (let ((nwidth (- column (telega-current-column))))
    (telega-ins (propertize (make-string (if (> nwidth 0) nwidth 1) ?\s)
                            'display (list 'space :align-to column)))))

(provide 'telega-core)


;; Need update the value on every load, because value might change,
;; MELPA might change the directory
(setq telega--lib-directory
      (or (and load-file-name
               (file-name-directory load-file-name))
          default-directory))

;;; telega-core.el ends here

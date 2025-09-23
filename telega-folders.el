;;; telega-folders.el --- Telegram Folders for telega  -*- lexical-binding: t -*-

;; Copyright (C) 2020 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Sep 17 22:29:21 2020
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
;; [[https://telegram.org/blog/folders][Telegram has added]] a new
;; feature that allows users to organise chats into Chat Folders.
;;
;; Each folder can have unlimited number of pinned chats.
;;
;; Before Telegram had support for Chat Folders, =telega= implemented
;; custom chat label feature, resembling Chat Folders functionality.
;; But now custom chat label feature is deprecated in favor to Chat
;; Folders.  Use {{{kbd(M-x telega-folders-migrate-custom-labels
;; RET)}}} to migrate your custom labels into Chat Folders.

;;; Code:
(require 'format-spec)

(require 'telega-core)
(require 'telega-tdlib)

(declare-function telega-chatbuf--name "telega-chat" (chat))
(declare-function telega-chat--set-uaprops "telega-chat" (chat uaprops))


(defconst telega-folder-icon-names
  '("All" "Unread" "Unmuted" "Bots" "Channels" "Groups" "Private"
    "Custom" "Setup" "Cat" "Crown" "Favorite" "Flower" "Game"
    "Home" "Love" "Mask" "Party" "Sport" "Study" "Trade" "Travel" "Work")
  "List of available icon names for the folders.
See `telega-folder-icons-alist'")

(defun telega-chat-folders (chat)
  "Return list of Telegram folders CHAT is member of."
  (let (folders)
    (seq-doseq (pos (plist-get chat :positions))
      ;; NOTE: zero order means "chat has no position"
      (unless (equal "0" (plist-get pos :order))
        (let ((list-name (telega-chat-position--list-name pos)))
          (when (stringp list-name)
            (setq folders (cons list-name folders))))))
    (nreverse folders)))

(defun telega-folder-name--fetch-custom-emojis (folder)
  "For the FOLDER, ensure all custom emojis in its name are fetched."
  (when-let* ((fmt-text (telega--tl-get folder :name :text))
              (name-ce-ids
               (seq-uniq
                (delq nil
                      (mapcar (lambda (entity)
                                (let ((entity-type (plist-get entity :type)))
                                  (when (eq 'textEntityTypeCustomEmoji
                                            (telega--tl-type entity-type))
                                    (plist-get entity-type :custom_emoji_id))))
                              (plist-get fmt-text :entities)))))
              (fetch-ce-ids
               (seq-remove #'telega-custom-emoji-get name-ce-ids)))
    ;; NOTE: Fetch only uncached custom emojis
    (telega--getCustomEmojiStickers fetch-ce-ids
      (lambda (stickers)
        (seq-doseq (sticker stickers)
          (telega-custom-emoji--ensure sticker))))))

(defun telega-folder-name (folder &optional no-properties)
  "Return FOLDER's name."
  (telega-tl-str (plist-get folder :name) :text no-properties))

(defun telega-folder-names (&optional tdlib-filters no-properties)
  "Return list of names for all Telegram folders.
Specify TDLIB-FILTERS list to use alternative TDLib chat filters list."
  (mapcar (lambda (fi)
            (telega-folder-name fi no-properties))
          (or tdlib-filters telega-tdlib--chat-folders)))

(defun telega-folder--chat-folder-info (folder-name)
  "Return chatFolderInfo corresponding to FOLDER-NAME."
  (cl-find folder-name telega-tdlib--chat-folders
           :key (lambda (fi)
                  (telega-folder-name fi 'no-props))
           :test #'equal))

(defun telega-folder--tdlib-chat-list (folder-name)
  "Return tdlib chat list for folder with FOLDER-NAME.
Return nil if folder with FOLDER-NAME is not known by TDLib."
  (or (let ((cl-fspec (cdr (assoc folder-name telega-filters-custom))))
        (cond ((eq cl-fspec 'main) '(:@type "chatListMain"))
              ((eq cl-fspec 'archive) '(:@type "chatListArchive"))))
      (progn
        ;; Strip off folder symbol, in case FOLDER-NAME from custom
        ;; filter spec is used
        (when-let ((real-folder-name
                    (get-text-property 0 'telega-folder folder-name)))
          (setq folder-name real-folder-name))
        (when-let ((fi (telega-folder--chat-folder-info folder-name)))
          (list :@type "chatListFolder"
                :chat_folder_id (plist-get fi :id))))))

(defun telega-folder-format (fmt-spec folder-name &optional folder-info)
  "Format a folder of FOLDER-NAME using FMT-SPEC.
FMT-SPEC is a string containing:
%I - Replaced with folder's icon from `telega-folder-icon-names' or
     empty string if there is no icon.
%i - Replaced with folder's icon from `telega-folder-format' or
     `telega-symbol-folder' if there is no icon.
%f - Replaced with folder's title.
%F - Replaced with folder's icon from `telega-folder-icon-names'
     if icon is unique, or equivalent to %I%f.

In case icon is used in the formatting, it is propertized with
`telega-folder' property having value of FOLDER-NAME.  This
property is used in `telega-folder--tdlib-chat-list' to
correctly extract folder name."
  (unless folder-info
    (setq folder-info (telega-folder--chat-folder-info folder-name)))
  (let* ((ftitle (telega-folder-name folder-info))
         (ficon-name (telega-tl-str (plist-get folder-info :icon) :name))
         (ficon (cdr (assoc ficon-name telega-folder-icons-alist)))
         (ficon-emoji (when (and ficon telega-emoji-use-images)
                        (telega-symbol-emojify ficon)))
         (ficon-symbol (propertize (or ficon-emoji ficon (telega-symbol 'folder))
                                   'telega-folder folder-name))
         (icon-uniq-p
          (= (length (cl-remove-if-not
                      (lambda (fi)
                        (equal ficon-name
                               (telega-tl-str (plist-get fi :icon) :name)))
                      telega-tdlib--chat-folders))
             1)))
    (format-spec fmt-spec
                 (format-spec-make ?i ficon-symbol
                                   ?I (if ficon ficon-symbol "")
                                   ?f ftitle
                                   ?F (if (and ficon icon-uniq-p)
                                          ficon-symbol
                                        (concat (if ficon ficon-symbol "")
                                                ftitle))))))

(defun telega-folder-tag-face (folder-name &optional folder-info)
  "Return face to display folder tag for folder of FOLDER-NAME."
  (unless folder-info
    (setq folder-info (telega-folder--chat-folder-info folder-name)))

  (when-let ((palette (telega-palette-by-color-id
                       (plist-get folder-info :color_id))))
    (append (assq :foreground palette) (assq :background palette))))

(defun telega-folders-insert-as-tags (fmt-spec folders &optional force)
  "Inserter for the FOLDERS list in case folder tags are enabled.
If FORCE is specified, then `telega-tdlib--chat-folder-tags-p' is ignored."
  (when (and (or force telega-tdlib--chat-folder-tags-p) folders)
    (seq-doseq (folder folders)
      (let ((finfo (telega-folder--chat-folder-info folder)))
        ;; NOTE: `:color_id' == -1 means display as tag is disabled
        ;; for this folder
        (unless (eq (plist-get finfo :color_id) -1)
          (telega-ins--with-face (telega-folder-tag-face folder finfo)
            (telega-ins (telega-folder-format fmt-spec folder finfo)))
          (telega-ins " "))))
    t))

(defun telega-folders-insert-default (&optional fmt-spec)
  "Default inserter for the folders prefixing chat's title."
  (let ((fmt-spec (or fmt-spec (eval-when-compile
                                 (propertize "%F" 'face 'bold)))))
    (if telega-tdlib--chat-folder-tags-p
        (telega-folders-insert-as-tags fmt-spec telega-chat-folders)
      (when (cond ((> (length telega-chat-folders) 1)
                   (telega-ins (telega-symbol 'multiple-folders)))
                  (telega-chat-folders
                   (telega-ins (telega-folder-format
                                fmt-spec (car telega-chat-folders)))))
        (telega-ins (telega-symbol 'vertical-bar))))))

(defun telega-folder-create (folder-name icon-name chats)
  "Create new Telegram folder with name FOLDER-NAME."
  (interactive
   (list (read-string (concat (telega-i18n "lng_filters_create") ": "))
         (when (y-or-n-p "Associate icon with the folder? ")
           (telega-completing-read-folder-icon-name
            "Folder icon name: "))
         (telega-completing-read-chat-list
          (concat (telega-i18n "lng_filters_add_chats") ": "))))

  ;; NOTE: Folder must contain at least 1 chat, otherwise error=400 is
  ;; returned
  (when chats
    (telega--createChatFolder
     (nconc (list :@type "chatFolder"
                  :name (list :@type "chatFolderName"
                              :text (telega-fmt-text folder-name))
                  :included_chat_ids (cl-map 'vector (telega--tl-prop :id) chats))
            (when icon-name
              (list :icon (list :@type "chatFolderIcon" :name icon-name)))))))

(defun telega-folder-delete (folder-name)
  "Delete Telegram folder with FOLDER-NAME.
This won't delete any chat, just a folder."
  (interactive (list (telega-completing-read-folder "Delete Folder: ")))
  (when (y-or-n-p (format "Delete Folder \"%s\"? " folder-name))
    (telega--deleteChatFolder
     (plist-get (telega-folder--chat-folder-info folder-name) :id))))

(defun telega-folders-reorder (ordered-folder-names)
  "Reorder Telegram folders to be in ORDERED-FOLDER-NAMES order."
  (interactive (list (telega-completing-read-folder-list "Reorder Folders")))
  (let* ((ordered-ids (mapcar (telega--tl-prop :id)
                              (mapcar #'telega-folder--chat-folder-info
                                      ordered-folder-names)))
         (rest-ids (cl-remove-if (lambda (cl-id)
                                   (memq cl-id ordered-ids))
                                 (mapcar (telega--tl-prop :id)
                                         telega-tdlib--chat-folders))))
    (telega--reorderChatFolders (nconc ordered-ids rest-ids))))

(defun telega-folder-rename (folder-name new-folder-name &optional new-icon-name)
  "Assign new name and icon to the folder with FOLDER-NAME."
  (interactive (list (telega-completing-read-folder "Rename Folder: ")
                     (read-string "New Folder name: ")
                     (when (y-or-n-p "Associate new icon with the folder? ")
                       (telega-completing-read-folder-icon-name
                        "Folder icon name: "))))
  (let* ((folder-info (telega-folder--chat-folder-info folder-name))
         (tdlib-folder (telega--getChatFolder (plist-get folder-info :id))))
    (plist-put tdlib-folder
               :name (list :@type "chatFolderName"
                           :text (telega-fmt-text new-folder-name)))
    (when new-icon-name
      (plist-put tdlib-folder
                 :icon (list :@type "chatFolderIcon" :name new-icon-name)))

    (telega--editChatFolder (plist-get folder-info :id) tdlib-folder)))

(defun telega-folder-set-icon (folder-name new-icon-name)
  "For folder with FOLDER-NAME set new icon to NEW-ICON-NAME."
  (interactive
   (list (telega-completing-read-folder "Folder to set icon: ")
         (telega-completing-read-folder-icon-name "Folder icon name: ")))
  (telega-folder-rename folder-name folder-name new-icon-name))

(defun telega-chat-add-to-folder (chat folder-name)
  "Add CHAT to the Telegram folder named FOLDER-NAME.
You can add chat to multiple folders."
  (interactive
   (let ((chat-at (or telega-chatbuf--chat (telega-chat-at (point)))))
     (unless chat-at
       (user-error "No chat at point, move point to the chat button and repeat"))
     (list chat-at
           (telega-completing-read-folder
            (format "Add «%s» to Folder: "
                    (telega-ins--as-string
                     (telega-ins--with-attrs (list :max 20 :elide-trail 1)
                       (telega-ins
                        (telega-msg-sender-title-for-completion chat-at)))))))))

  (let ((folder-info (telega-folder--chat-folder-info folder-name)))
    (cl-assert folder-info)
    (telega--addChatToList
     chat (list :@type "chatListFolder"
                :chat_folder_id (plist-get folder-info :id)))))

(defun telega-chat-remove-from-folder (chat folder-name)
  "Remove CHAT from the folder named FOLDER-NAME."
  (interactive
   (let ((chat-at (or telega-chatbuf--chat (telega-chat-at (point)))))
     (unless chat-at
       (user-error "No chat at point"))
     (list chat-at
           (telega-completing-read-folder
            (format "Remove «%s» from Folder: " (telega-chatbuf--name chat-at))
            (telega-chat-folders chat-at)))))

  (let* ((chat-id (plist-get chat :id))
         (folder-info (telega-folder--chat-folder-info folder-name))
         (tdlib-folder (telega--getChatFolder (plist-get folder-info :id)))
         (inc-cids (append (plist-get tdlib-folder :included_chat_ids) nil))
         (exc-cids (append (plist-get tdlib-folder :excluded_chat_ids) nil)))
    (cl-assert tdlib-folder)

    ;; NOTE: Fix `tdlib-folder's `:name' in case it contains
    ;; surropagated pairs, to avoid error with code=400
    (telega-fmt-text-desurrogate (telega--tl-get tdlib-folder :name :text))

    (if (memq chat-id inc-cids)
        (plist-put tdlib-folder :included_chat_ids
                   (vconcat (delq chat-id inc-cids)))
      (plist-put tdlib-folder :excluded_chat_ids
                 (vconcat (cl-pushnew chat-id exc-cids))))
    (telega--editChatFolder (plist-get folder-info :id) tdlib-folder)))


;; Migration from deprecated custom chat labels into Chat Folders
(defun telega-folders--deprecated-custom-labels-list ()
  "Return list of any deprecated custom labels in use."
  (seq-uniq
   (cl-remove-if-not #'stringp
                     (mapcar (lambda (chat)
                               (telega-chat-uaprop chat :label))
                             (telega-chats-list)))))

(defun telega-folders-migrate-custom-labels ()
  "Migrate custom chat labels into Chat Folders."
  (interactive)
  (let ((folders (telega-folder-names)))
    (dolist (label (telega-folders--deprecated-custom-labels-list))
      (let ((chats (cl-remove-if-not (lambda (chat)
                                       (equal (telega-chat-uaprop chat :label)
                                              label))
                                     (telega-chats-list))))
        (if (member label folders)
            (when (yes-or-no-p
                   (format "Add %d chats into already existing «%s» Folder? "
                           (length chats) label))
              (dolist (chat chats)
                (telega-chat-add-to-folder chat label)))

          (when (yes-or-no-p
                 (format "Create new «%s» Folder and add %d chats into it? "
                         label (length chats)))
            (telega-folder-create label nil chats)))

        (when (yes-or-no-p
               (format "Remove «%s» custom label from %d chats? "
                       label (length chats)))
          (dolist (chat chats)
            (telega-chat--set-uaprops
             chat (telega-plist-del (plist-get chat :uaprops) :label)))))
      )))

(defun telega-folders-warn-if-custom-labels ()
  "Warn user about custom chat label deprecation."
  (when-let ((custom-labels (telega-folders--deprecated-custom-labels-list)))
    (display-warning 'telega (format "Telega custom labels are deprecated.\n\
Consider using `M-x telega-folders-migrate-custom-labels RET' to\n\
migrate your custom labels %S to Telegram Folders." custom-labels))))


;; Chat Folders Settings
(defun telega-folders-settings--redisplay (&rest _args)
  "If CHAT info buffer exists and visible, then redisplay it."
  (telega-help-win--maybe-redisplay "*Telega Folders Settings*" nil))

(defun telega-folders-settings--ins-folder (folder-info)
  "Inserter for the FOLDER-INFO in the Folders settings buffer."
  (let ((folder-name (telega-folder-name folder-info)))
    (telega-ins (telega-folder-format "%i %f" folder-name folder-info))
    (telega-ins--with-face 'telega-shadow
      (telega-ins " (")
      (telega-ins-i18n "lng_filters_chats_count"
        :count (length (telega-filter-chats (telega-chats-list)
                         (list 'folder folder-name))))
      (when (plist-get folder-info :is_shareable)
        (telega-ins " • ")
        (telega-ins-i18n "lng_filters_shareable_status"))
      (telega-ins ")"))
    (telega-ins--move-to-column 35)
    (if (eq (plist-get folder-info :color_id) -1)
        (telega-ins--with-face 'telega-shadow
          (telega-ins-i18n "lng_filters_tag_color_no"))
      (telega-ins--with-face (telega-folder-tag-face folder-name folder-info)
        (telega-ins "■")))))

(defun telega-folders-settings--inserter (&rest _ignored)
  "Inserter for the folder settings."
  (telega-ins--with-face 'bold
    (telega-ins-i18n "lng_filters_title"))
  (telega-ins "\n")
  (telega-ins--line-wrap-prefix "  "
    (telega-ins--text-button (if telega-tdlib--chat-folder-tags-p
                                 (telega-symbol 'checkbox-on)
                               (telega-symbol 'checkbox-off))
      'face 'telega-link
      'action (lambda (_button)
                (telega--toggleChatFolderTags
                    (not telega-tdlib--chat-folder-tags-p)
                  #'telega-folders-settings--redisplay)))
    (telega-ins " Show Folder Tags\n")
    (telega-ins--help-message
     (telega-ins "Display folder names for each chat in the chat list.")))

  (telega-ins "\n")
  (telega-ins--with-face 'bold
    (telega-ins-i18n "lng_filters_subtitle"))
  (telega-ins "\n")
  (telega-ins--line-wrap-prefix "  "
    (seq-doseq (folder-info telega-tdlib--chat-folders)
      (telega-folders-settings--ins-folder folder-info)
      (telega-ins "\n")))

  (telega-ins "\n")
  (telega-ins--with-face 'bold
    (telega-ins-i18n "lng_filters_recommended"))
  (telega-ins "\n")
  (telega-ins--line-wrap-prefix "  "
    (telega--getRecommendedChatFolders
     (telega--gen-ins-continuation-callback 'loading
       (lambda (recommended-folders)
         (seq-doseq (rfolder recommended-folders)
           (telega-folders-settings--ins-folder (plist-get rfolder :folder))
           (telega-ins " ")
           (telega-ins--box-button (telega-i18n "lng_filters_recommended_add")
             'action (lambda (_button)
                       (telega--createChatFolder
                        (plist-get rfolder :folder)
                        #'telega-folders-settings--redisplay)))
           (telega-ins "\n")
           (telega-ins--help-message
            (telega-ins (telega-tl-str rfolder :description))))))))
  )

(defun telega-folders-settings ()
  "Open folders settings."
  (interactive)
  (with-telega-help-win "*Telega Folders Settings*"
    (telega-folders-settings--inserter)

    (setq telega--help-win-param nil)
    (setq telega--help-win-inserter #'telega-folders-settings--inserter)))

(provide 'telega-folders)

;; Barf if deprecated custom labels are used
(add-hook 'telega-chats-fetched-hook #'telega-folders-warn-if-custom-labels)

;;; telega-folders.el ends here

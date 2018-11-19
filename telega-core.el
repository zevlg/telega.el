;;; telega-core.el --- Core functionality for telega.

;; Copyright (C) 2018 by Zajcev Evgeny.

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

;; Variables and runtime goodies for telega

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(eval-when-compile
  (require 'cl)) ;; for defsetf
(require 'ring)

(defconst telega-chat-types
  '(private secret basicgroup supergroup bot channel)
  "All types of chats supported by telega.")

;;; Runtime variables
(defvar telega--me-id nil "User id of myself.")
(defvar telega--options nil "Options updated from telega-server.")
(defvar telega--status "Not Started" "Status of the connection to telegram.")
(defvar telega--chats nil "Hash table (id -> chat) for all chats.")
(defvar telega--ordered-chats nil "Ordered list of all chats.")
(defvar telega--filtered-chats nil
  "Chats filtered by currently active filters.")
(defvar telega--filters nil "List of active filters.")
(defvar telega--undo-filters nil "List of undo entries.")

(defvar telega--info nil "Alist of (TYPE . INFO-TABLE)")
(defvar telega--full-info nil "Alist of (TYPE . FULL-INFO-TABLE)")

(defvar telega--logo-image-cache nil "Cached loaded logo image.")
(defvar telega--unread-count 0 "Total number of unread messages.")
(defvar telega--unread-unmuted-count 0 "Total number of unread/unmuted messages.")

(defvar telega--chat-buffers nil "List of all chat buffers.")
(defvar telega--files-downloading nil
  "List of elements in form (FILE-ID CHAT-ID MSG-ID).
Where CHAT-ID and MSG-ID denotes place where FILE-ID is used in.
Used to update messages when file updates.")

(defvar telega--ignored-messages-ring (make-ring 0)
  "Ring of ignored messages.
Use M-x telega-ignored-messages RET to display the list.")

(defun telega--init-vars ()
  "Initialize runtime variables.
Done when telega server is ready to receive queries."
  (setq telega--me-id -1)
  (setq telega--options nil)
  (setq telega--chats (make-hash-table :test 'eq))
  (setq telega--ordered-chats nil)
  (setq telega--filtered-chats nil)
  (setq telega--filters nil)
  (setq telega--undo-filters nil)
  (setq telega--info
        (list (cons 'user (make-hash-table :test 'eq))
              (cons 'secretChat (make-hash-table :test 'eq))
              (cons 'basicGroup (make-hash-table :test 'eq))
              (cons 'supergroup (make-hash-table :test 'eq))))
  (setq telega--full-info
        (list (cons 'user (make-hash-table :test 'eq))
              (cons 'basicGroup (make-hash-table :test 'eq))
              (cons 'supergroup (make-hash-table :test 'eq))))

  (setq telega--files-downloading nil)
  (setq telega--ignored-messages-ring
        (make-ring telega-ignored-messages-ring-size)))

(defmacro with-telega-debug-buffer (&rest body)
  "Execute BODY only if telega-debug is enabled making debug buffer current."
  `(when telega-debug
     (with-current-buffer (get-buffer-create "*telega-debug*")
       (telega-save-excursion
         ,@body))))

(defsubst telega-debug (fmt &rest args)
  (with-telega-debug-buffer
   (goto-char (point-max))
   (insert (apply 'format (cons (concat fmt "\n") args)))))

(defmacro telega--tl-type (tl-obj)
  `(intern (plist-get ,tl-obj :@type)))
(defsetf telega--tl-type (tl-obj) (type-sym)
  `(plist-put ,tl-obj :@type (symbol-name ',type-sym)))

(defmacro telega-save-excursion (&rest body)
  "Save current point as moving marker."
  (let ((pnt-sym (gensym)))
    `(let ((,pnt-sym (copy-marker (point) t)))
       (unwind-protect
           (progn ,@body)
         (goto-char ,pnt-sym)))))

(defmacro telega--tl-prop (prop)
  "Generates function to get property PROP."
  (let ((tl-obj-sym (cl-gensym "tl-obj")))
    `(lambda (,tl-obj-sym)
       (plist-get ,tl-obj-sym ,prop))))

(defun telega--tl-desurrogate (str)
  "Decode surrogate pairs in STR string.
Attach `display' text property to surrogated regions."
  (dotimes (idx (1- (length str)))
    (let ((high (aref str idx))
          (low (aref str (1+ idx))))
      (when (and (>= high #xD800) (<= high #xDBFF)
                 (>= low #xDC00) (<= low #xDFFF))
        (add-text-properties
         idx (+ idx 2) (list 'display (char-to-string
                                       (+ (lsh (- high #xD800) 10) (- low #xDC00) #x10000))
                             'telega-desurrogate t)
         str))))
  str)

(defun telega--desurrogate-apply (str)
  "Apply `telega-desurrogate' properties to STR.
Resulting in new string with no surrogate pairs."
  (let ((ret "") (beg 0) (fin (length str)) end)
    (while (setq end (text-property-any beg fin 'telega-desurrogate t str))
      (setq ret (concat ret (substring-no-properties str beg end)
                        (get-text-property end 'display str))
            beg (+ end 2)))
    (concat ret (substring-no-properties str beg end))))

(defsubst telega--tl-unpack (obj)
  "Unpack (i.e. desurrogate strings) object OBJ."
  (cond ((stringp obj) (telega--tl-desurrogate obj))
        ((vectorp obj) (cl-map 'vector 'telega--tl-unpack obj))
        ((listp obj) (mapcar 'telega--tl-unpack obj))
        (t obj)))

(defsubst telega--tl-pack (obj)
  "Pack object OBJ."
  ;; Remove text props from strings, etc
  (cond ((stringp obj) (substring-no-properties obj))
        ((vectorp obj) (cl-map 'vector 'telega--tl-pack obj))
        ((listp obj) (mapcar 'telega--tl-pack obj))
        (t obj)))


;; Files
(defun telega-file--get (file-id)
  (telega-server--call
   (list :@type "getFile" :file_id file-id)))

(defun telega-file--download (file-id &optional priority)
  "Asynchronously downloads a file from the cloud.
`telega--on-updateFile' will be called to notify about the
download progress and successful completion of the download."
  (telega-server--call
   (list :@type "downloadFile"
         :file_id file-id
         :priority (or priority 32))))

(defun telega-file--cancel-download (file-id &optional only-if-pending)
  "Stops the downloading of a file with FILE-ID.
If ONLY-IF-PENDING is non-nil then stop downloading only if it
hasn't been started, i.e. request hasn't been sent to server."
  (telega-server--send
   (list :@type "cancelDownloadFile"
         :file_id file-id
         :only_if_pending (or only-if-pending :false))))

(defun telega-file--delete (file-id)
  "Delete file from cache."
  (telega-server--send
   (list :@type "deleteFile"
         :file_id file-id)))

(defun telega-file--get-path-or-start-download (file chat-id msg-id)
  "Download file or return it."

  (let* ((local (plist-get file :local))
         (is-downloading-completed-p (plist-get local :is_downloading_completed))
         (is-downloading-active-p (plist-get local :is_downloading_active)))

    (if is-downloading-completed-p
        (plist-get local :path)

      (when (not is-downloading-active-p)
        (let ((file-id (plist-get file :id)))
          (pushnew (list file-id chat-id msg-id) telega--files-downloading)
          (telega-file--download file-id)
          nil)))))

(defsubst telega-file--update-message (file)
  "Update message associated with FILE."
  (let ((link (assq (plist-get file :id) telega--files-downloading)))
    (when link
      (telega-msg--update-file (nth 1 link) (nth 2 link) file)

      (let ((local (plist-get file :local)))
        (when (plist-get local :is_downloading_completed)
          (setq telega--files-downloading
                (delete link telega--files-downloading))
          (message "Downloading completed: %s" (plist-get local :path)))))))

(defun telega--on-updateFile (event)
  "File has been updated, call all the associated hooks."
  (telega-file--update-message (plist-get event :file)))


;;; Formatting
(defun telega-fmt-eval-fill (estr attrs)
  "Fill ESTR to :fill-column.
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
  (let* ((max (plist-get attrs :max))
         (elide (plist-get attrs :elide))
         (elide-str (or (plist-get attrs :elide-string) telega-eliding-string))
         (elide-trail (or (plist-get attrs :elide-trail) 0))
         (estr-trail (if (> elide-trail 0) (substring estr (- elide-trail)) ""))
         (estr-lead (substring estr 0 (- max (length elide-str) elide-trail)))
         result)
    ;; Correct truncstr in case of multibyte chars
    (while (and (not (string-empty-p estr-lead))
                (< max (string-width
                        (setq result (concat estr-lead elide-str estr-trail)))))
      (setq estr-lead (substring estr-lead 0 -1)))

    result))

(defun telega-fmt-eval-align (estr attrs)
  (let* ((min (plist-get attrs :min))
         (width (- min (string-width estr)))
         (align (plist-get attrs :align))
         (align-char (or (plist-get attrs :align-char) ?\s))
         (left (make-string (/ width 2) align-char))
         (right (make-string (- width (/ width 2)) align-char)))
    (ecase align
      (left (concat estr left right))
      (right (concat left right estr))
      ((center centre) (concat left estr right)))))

(defun telega-fmt-eval-min-max (estr attrs)
  "Apply `:min' and `:max' properties to ESTR."
  (let ((max (plist-get attrs :max))
        (min (plist-get attrs :min))
        (estr-width (string-width estr)))
    (cond ((and max (> estr-width max))
           (telega-fmt-eval-truncate estr attrs))
          ((and min (< estr-width min))
           (telega-fmt-eval-align estr attrs))
          (t estr))))

(defun telega-fmt-eval-fill-prefix (estr attrs)
  (concat (or (plist-get attrs :fill-prefix) "") estr))

(defun telega-fmt-eval-face (estr attrs)
  "Apply `:face' attribute to ESTR."
  (let ((face (plist-get attrs :face)))
    (if face
        (propertize estr 'face face)
      estr)))

(defun telega-fmt-eval-attrs (estr attrs)
  "Apply all attributes to ESTR."
  (let ((formatted-estrs
         (mapcar (lambda (estrline)
                   (telega-fmt-eval-face
                    (telega-fmt-eval-min-max
                     (telega-fmt-eval-fill-prefix estrline attrs) attrs)
                    attrs))
                 (telega-fmt-eval-fill estr attrs))))
    ;; NOTE: strip prefix on the first line
    (mapconcat #'identity
               (cons (substring (car formatted-estrs)
                                (length (plist-get attrs :fill-prefix)))
                     (cdr formatted-estrs)) "\n")))

(defsubst telega-fmt-atom (atom)
  "Convert ATOM to string.
NIL yields empty string for the convenience."
  (cond ((stringp atom) atom)
        ((null atom) "")
        (t (with-output-to-string
             (princ atom)))))

(defun telega-fmt-eval-elem (elem value)
  "Format single element ELEM."
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
  "Return time at 00:00:001 at TIMESTAMP's day."
  (let ((dt (or decoded-ts (decode-time timestamp))))
    (1+ (- timestamp (* 3600 (nth 2 dt)) (* 60 (nth 1 dt)) (nth 0 dt)))))

(defun telega-fmt-timestamp (timestamp)
  "Format unix TIMESTAMP to human readable form."
  ;; - HH:MM      if today
  ;; - Mon/Tue/.. if on this week
  ;; - DD.MM.YY   otherwise
  (let* ((dtime (decode-time timestamp))
         (current-ts (time-to-seconds (current-time)))
         (ctime (decode-time current-ts))
         (today00 (telega--time-at00 current-ts ctime)))
    (if (> timestamp today00)
        (format "%02d:%02d" (nth 2 dtime) (nth 1 dtime))

      (let* ((week-day (nth 6 ctime))
             (mdays (+ week-day
                       (- (if (< week-day telega-week-start-day) 7 0)
                          telega-week-start-day)))
             (week-start00 (telega--time-at00
                            (- current-ts (* mdays 24 3600)))))
        (if (> timestamp week-start00)
            (nth (nth 6 dtime) telega-week-day-names)

          (format "%02d.%02d.%02d"
                  (nth 3 dtime) (nth 4 dtime) (- (nth 5 dtime) 2000))))
      )))

(defun telega-fmt-timestamp-iso8601 (timestamp)
  (format-time-string "%FT%T%z" timestamp))

(defun telega-fmt-labeled-text (label text &optional fill-col)
  "Format TEXT filling it, prefix with LABEL."
  (telega-fmt-eval
   `(,label (identity :fill left
                      :fill-prefix ,(make-string (length label) ?\s)
                      :fill-column ,fill-col))
   text))

(defun telega-fmt-chat-member-status (status)
  "Format chat member STATUS."
  (case (telega--tl-type status)
    (chatMemberStatusCreator " (creator)")
    (chatMemberStatusAdministrator " (admin)")
    (chatMemberStatusBanned " (banned)")
    (chatMemberStatusRestricted " (restricted)")
    (chatMemberStatusLeft " (left)")
    (t "")))

(defun telega-fmt-chat-member (member)
  "Formatting for the chat MEMBER."
  (let ((user (telega-user--get (plist-get member :user_id)))
        (joined (plist-get member :joined_chat_date)))
    (list
     (telega-user--name user)
     (telega-fmt-chat-member-status (plist-get member :status))
     (unless (zerop joined)
       (concat " joined at " (telega-fmt-timestamp joined))))))

;;; Buttons for telega

(defun telega-button--format-error (msg)
  (error "Button `:format' is unset."))

;; Make 'telega-button be separate (from 'button) type
(put 'telega-button 'type 'telega)
(put 'telega-button 'keymap button-map)
(put 'telega-button 'action 'ignore)
(put 'telega-button 'rear-nonsticky t)
(put 'telega-button :format 'telega-button--format-error)
(put 'telega-button :value nil)
(put 'telega 'button-category-symbol 'telega-button)

(defun telega-button-properties (button)
  "Return all BUTTON properties specific for this type of buttons."
  (let ((text-props (text-properties-at button))
        (type-plist (symbol-plist
                     (button-category-symbol (button-type button)))))
    (cl-loop for (key _) on text-props by 'cddr
             if (or (plist-member type-plist key)
                    (memq key '(button category)))
             nconc (list key (plist-get text-props key)))))

(defmacro telega-button-foreach0 (advance-func butt-type args &rest body)
  "Run point accross all buttons of BUTTON-TYPE.
Run BODY for each found point.
Use `cl-block' and `cl-return' to prematurely stop the iteration.
Run under `save-excursion' to preserve point."
  (let ((button (car args)))
    `(let ((,button (button-at (point))))
       (while ,button
         (goto-char ,button)
         (when (eq (button-type ,button) ,butt-type)
           ,@body)
         (setq ,button (,advance-func ,button))))))
(put 'telega-button-foreach0 'lisp-indent-function 'defun)

(defmacro telega-button-foreach (butt-type args &rest body)
  "Run point accross all buttons of BUTTON-TYPE.
Run BODY for each found point.
Use `cl-block' and `cl-return' to prematurely stop the iteration.
Run under `save-excursion' to preserve point."
  `(telega-button-foreach0 next-button ,butt-type ,args ,@body))
(put 'telega-button-foreach 'lisp-indent-function 'defun)

(defun telega-button-find (butt-type &rest value)
  "Find button by BUTT-TYPE and its VALUE.
If VALUE is not specified, then find fist one button of BUTT-TYPE."
  (cl-block 'button-found
    (telega-button-foreach butt-type (button)
      (when (or (null value) (eq (button-get button :value) (car value)))
        (cl-return-from 'button-found button)))))

(defun telega-button-insert (button-type &rest props)
  "Insert button of BUTTON-TYPE with properties PROPS.
Return newly created button."
  (let ((value (plist-get props :value))
        (button-fmt (or (plist-get props :format)
                        (button-type-get button-type :format))))
    (button-at
     (apply #'insert-text-button
            (telega-fmt-eval button-fmt value)
            :type button-type props))))
(put 'telega-button-insert 'lisp-indent-function 'defun)

(defun telega-button-delete (button)
  "Delete the BUTTON."
  (let ((inhibit-read-only t))
    (delete-region (button-start button) (button-end button))))

(defun telega-button-move (button point &optional new-label)
  "Move BUTTON to POINT location."
  (let ((inhibit-read-only t))
    (unless new-label
      (setq new-label
            (buffer-substring (button-start button) (button-end button))))
    (goto-char point)
    (telega-button-delete button)
    (let ((cpnt (point)))
      (insert new-label)
      (set-marker button cpnt))))

(defun telega-button--redisplay (button)
  "Redisplay the BUTTON contents."
  (telega-button-move button (button-start button)
                      (apply #'propertize
                             (telega-fmt-eval (button-get button :format)
                                              (button-get button :value))
                             (telega-button-properties button))))

(defun telega-button-forward (n)
  "Move forward to N visible/active button."
  (interactive "p")
  (let (button)
    (dotimes (_ (abs n))
      (while (and (setq button (forward-button (cl-signum n)))
                  (or (button-get button 'invisible)
                      (button-get button 'inactive)))))
    (when (= (following-char) ?\[)
      (forward-char 1))

    (when (button-get button :help-format)
      (message (telega-fmt-eval (button-get button :help-format)
                                (button-get button :value))))
    button))

(defun telega-button-backward (n)
  "Move backward to N visible/active button."
  (interactive "p")
  (telega-button-forward (- n)))

(defun telega-completing-titles ()
  "Return list of titles ready for completing."
  (let ((result))
    (dolist (chat (telega-filter-chats 'all))
      (setq result (pushnew (telega-chat--title chat 'withusername) result
                            :test #'string=)))
    (dolist (user (hash-table-values (cdr (assq 'user telega--full-info))))
      (setq result (pushnew (telega-user--name user) result :test #'string=)))
    (nreverse result)))

(defun telega-link-props (link-type link-to &optional face)
  "Generate props for link button openable with `telega-open-link-action'."
  (assert (memq link-type '(url file user hashtag download cancel-download)))
  (list 'action 'telega-open-link-action
        'face (or face 'telega-link)
        :telega-link (cons link-type link-to)))

(defun telega-open-link-action (button)
  "Browse url at point."
  (let ((link (button-get button :telega-link)))
    (ecase (car link)
      (user (with-help-window " *Telegram User Info*"
              (set-buffer standard-output)
              (telega-info--insert-user
               (telega-user--get (cdr link)))))
      (url (browse-url (cdr link)))
      (file (find-file (cdr link)))

      ;; `link' for download/cancel-download is (FILE-ID CHAT-ID MSG-ID)
      (download
       (pushnew (cdr link) telega--files-downloading)
       (telega-file--download (car (cdr link))))

      (cancel-download
       (telega-file--cancel-download (car (cdr link)))
       (telega-file--update-message
        (telega-file--get (car (cdr link))))
       (setq telega--files-downloading
             (delete (cdr link) telega--files-downloading))))))

(provide 'telega-core)

;;; telega-core.el ends here

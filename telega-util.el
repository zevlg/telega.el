;;; telega-util.el --- Utility functions for telega

;; Copyright (C) 2018 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sat Apr 21 03:56:02 2018
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

;; Utility functions to be used by telega

;;; Code:

(require 'ewoc)
(require 'cl-lib)
(require 'files)                        ; `locate-file'

;; code taken from
;; https://emacs.stackexchange.com/questions/14420/how-can-i-fix-incorrect-character-width
(defun telega-symbol-set-widths (symbol-widths-alist)
  "Add symbol widths from SYMBOL-WIDTHS-ALIST to `char-width-table'.
Use it if you have formatting issues."
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table)))
  (dolist (pair symbol-widths-alist)
    (let ((width (car pair))
          (symbols (cdr pair))
          (table (make-char-table nil)))
      (dolist (symbol-str symbols)
        (set-char-table-range table (string-to-char symbol-str) width))
      (optimize-char-table table)
      (set-char-table-parent table char-width-table)
      (setq char-width-table table))))

(defun telega-duration-human-readable (seconds)
  "Convert SECONDS to human readable string."
  (concat (when (>= seconds 3600)
            (prog1 (format "%dh:" (/ seconds 3600))
              (setq seconds (% seconds 3600))))
          (when (>= seconds 60)
            (prog1 (format "%dm:" (/ seconds 60))
              (setq seconds (% seconds 60))))
          (format "%ds" seconds)))

(defun telega-etc-file (filename)
  "Return absolute path to FILENAME from etc/ directory in telega."
  (locate-file (concat "etc/" filename) load-path))

(defun telega-link-props (link-type link-to &optional face)
  "Generate props for link button openable with `telega-link--button-action'."
  (cl-assert (memq link-type '(url file user hashtag download cancel-download
                                   upload cancel-upload)))

  (list 'action 'telega-link--button-action
        'face (or face 'telega-link)
        :telega-link (cons link-type link-to)))

(defun telega-link--button-action (button)
  "Browse url at point."
  (let ((link (button-get button :telega-link)))
    (telega-debug "Action on link: %S" link)
    (cl-ecase (car link)
      (user (with-help-window "*Telegram User Info*"
              (set-buffer standard-output)
              (telega-info--insert-user
               (telega-user--get (cdr link)))))
      (url (browse-url (cdr link)))
      (file (find-file (cdr link)))

      ;; `link' for download is (PLACE PROP MSG)
      (download
       (telega-file--download-monitoring
        (cadr link) (caddr link)
        'telega-file--update-msg (cadddr link)))
      ;; `link' for cancel-download is FILE-ID
      (cancel-download
       (telega--cancelDownloadFile (cadr link)))

      ;; `link' for upload is (PLACE PROP MSG)
      (upload
       (telega-file--upload-monitoring
        (cadr link) (caddr link)
        'telega-file--update-msg (cadddr link)))
      ;; `link' for cancel-upload is FILE-ID
      (cancel-upload
       (telega--cancelUploadFile (cadr link)))
      )))

(defun telega--entity-to-properties (entity text)
  "Convert telegram ENTITY to emacs text properties to apply to TEXT."
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

(defun telega--entities-apply (entities text)
  "Apply telegram ENTITIES to TEXT."
  (mapc (lambda (ent)
          (let* ((beg (plist-get ent :offset))
                 (end (+ (plist-get ent :offset) (plist-get ent :length)))
                 (props (telega--entity-to-properties
                         ent (substring text beg end))))
            (when props
              (add-text-properties beg end props text))))
        entities)
  text)

(defun telega--properties-to-entities (text)
  "Convert propertiezed TEXT to telegram ENTITIES."
  ;; TODO: convert text properties to tl text entities
  )

(defun telega--merge-face (start end face &optional object)
  "Merge OBJECT's face property at START and END by adding FACE."
  (let ((val (if (listp face) face (list face))) next prev)
    (while (/= start end)
      (setq next (next-single-property-change start 'face object end)
	    prev (get-text-property start 'face object))
      (put-text-property start next 'face
			 (append val (if (listp prev) prev (list prev)))
			 object)
      (setq start next))))

(defun telega-completing-titles ()
  "Return list of titles ready for completing."
  (let ((result))
    (dolist (chat (telega-filter-chats 'all))
      (setq result (cl-pushnew (telega-chat--title chat 'withusername) result
                               :test #'string=)))
    (dolist (user (hash-table-values (cdr (assq 'user telega--info))))
      (setq result (cl-pushnew (telega-user--name user) result
                               :test #'string=)))
    (nreverse result)))


;; ewoc stuff
(defun telega-ewoc--find-node (ewoc predicate)
  "Find EWOC's node by PREDICATE run on node's data."
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((node (ewoc--node-nth dll 1))
       (footer (ewoc--footer ewoc))
       (inhibit-read-only t))
    (cl-block 'ewoc-node-found
      (while (not (eq node footer))
        (when (funcall predicate (ewoc--node-data node))
          (cl-return-from 'ewoc-node-found node))
        (setq node (ewoc--node-next dll node))))))

(defun telega-ewoc--find-node-by-data (ewoc data)
  "Find EWOC's node by its DATA."
  (telega-ewoc--find-node ewoc (lambda (node-data) (eq node-data data))))

(defun telega-ewoc--set-header (ewoc header)
  "Set EWOC's new HEADER."
  ;; NOTE: No ewoc API to change just header :(
  ;; only `ewoc-set-hf'
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((head (ewoc--header ewoc))
       (hf-pp (ewoc--hf-pp ewoc)))
    (setf (ewoc--node-data head) header)
    (ewoc--refresh-node hf-pp head dll)))

(defun telega-ewoc--set-footer (ewoc footer)
  "Set EWOC's new HEADER."
  ;; NOTE: No ewoc API to change just header :(
  ;; only `ewoc-set-hf'
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((foot (ewoc--footer ewoc))
       (hf-pp (ewoc--hf-pp ewoc)))
    (setf (ewoc--node-data foot) footer)
    (ewoc--refresh-node hf-pp foot dll)))

(provide 'telega-util)

;;; telega-util.el ends here

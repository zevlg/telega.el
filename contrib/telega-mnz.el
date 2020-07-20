;;; telega-mnz.el --- Display code (and other Emacs content) inside telega buffers  -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Evgeny Zajcev.
;; Copyright (c) 2020 Would (oldosfan).
;; Copyright (c) 2020 Brett Gilio.

;; telega-mnz.el is part of telega.el.

;; telega.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; telega.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with telega.el. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; ** /telega-mnz.el/ -- Display Emacs content inside Telega messages.
;; To use telega-mnz, simply ~(require 'telega-mnz)~.
;; You can attach code using `telega-mnz-attach-code' or `telega-mnz-send-code'.

;;; Code:

(require 'telega)
(require 'cl-lib)

(defvar telega-mnz-modes '(emacs-lisp-mode
			   lisp-mode
			   scheme-mode
			   c-mode
			   c++-mode
			   objc-mode
			   java-mode
			   python-mode
			   clojure-mode
			   outline-mode
			   org-mode
			   text-mode
			   sml-mode
			   tuareg-mode
			   rust-mode
			   go-mode
			   javascript-mode
			   clojurescript-mode
			   ruby-mode
			   perl-mode
			   cperl-mode
			   sh-mode
			   octave-mode
			   lisp-data-mode
			   diff-mode
			   kotlin-mode
			   scala-mode)
  "A list of major modes to be used inside read-major-mode prompts.")

(defvar telega-mnz-edit-map (make-sparse-keymap)
  "The keymap to be used when editing code messages.")

(defvar telega-mnz--inside-p nil
  "Will return `t' when inside a code block.")

(define-key telega-mnz-edit-map (kbd "C-c C-c") #'exit-recursive-edit)
(define-key telega-mnz-edit-map (kbd "C-x C-s") #'exit-recursive-edit)
(define-key telega-mnz-edit-map (kbd "C-c C-z") #'telega-mnz--cancel)
(define-key telega-mnz-edit-map (kbd "C-c C-k") #'telega-mnz--cancel)

(defun telega-mnz--render-text-in-mode-buffer (mode text)
  "Return a string with TEXT rendered in a buffer with MODE enabled."
  (condition-case-unless-debug nil
      (with-current-buffer (get-buffer-create "*Telega Mnz Fontification*")
	(erase-buffer)
	(insert text)
	(if (and (symbolp mode)
		 (commandp mode))
	    (funcall mode)
	  (ignore-errors
	    (mapc #'funcall mode)))
	(when font-lock-mode
	  (font-lock-fontify-buffer))
	(when jit-lock-mode
	  (jit-lock-fontify-now))
	(prog1 (propertize (buffer-string) 'syntax-table (syntax-table))
	  (kill-buffer (get-buffer "*Telega Mnz Fontification*"))))
    (error (format "!<<Error during code block fontification>>!\n%s\n%s"
		   (prin1-to-string text) text))))

(defun telega-mnz--mode (language)
  "Return mode used to highlight code for LANGUAGE."
  (let ((mode (ignore-errors (car (read-from-string language)))))
    (when (or (sequencep mode)
	      (and (symbolp mode)
		   (commandp (intern (concat (symbol-name mode) "-mode")))))
      (if (symbolp mode)
	  (intern (concat (symbol-name mode) "-mode"))
	mode))))

(defun telega-mnz--entities-as-faces-advice (oldfun entities text)
  "Advice for `telega--entities-as-faces' to fontify code blocks.
OLDFUN ##advice-super-doc.
ENTITIES TEXT ##advice-doc."
  (let ((new-text (funcall oldfun entities text)))
    (seq-doseq (ent entities)
      (let ((ent-type (plist-get ent :type))
            (mode nil))
        (when (and (eq (telega--tl-type ent-type) 'textEntityTypePreCode)
                   (setq mode (telega-mnz--mode (plist-get ent-type :language))))
          (let* ((ent-start (plist-get ent :offset))
                 (ent-stop (+ ent-start (plist-get ent :length))))
            (setq new-text
                  (concat (substring new-text 0 ent-start)
			  (propertize
                           (telega-mnz--render-text-in-mode-buffer
                            mode (substring new-text ent-start ent-stop))
			   :telega-code-start ent-start
			   :telega-code-end ent-stop)
                          (substring new-text ent-stop)))))))
    new-text))


(defun telega-mnz--extract-data-from-message (msg start end)
  "Extract telega-mnz data from the message MSG.
START should be the contents of the start text prop.
END should be the contents of the END text prop.
The returned data will be a pair of (MODE . TEXT) if available,
else nil."
  (ignore-errors
    (let* ((content (telega--tl-get msg :content))
	   (text (telega--tl-get content :text))
	   (str (telega--tl-get text :text))
	   (entities (telega--tl-get text :entities))
	   (first-entity (aref (cl-remove-if-not (lambda (e)
						   (and (equal (plist-get e :offset)
							       start)
							(equal (plist-get e :length)
							       (- end start))))
						 entities) 0))
	   (language (telega-mnz--mode (telega--tl-get first-entity :type :language))))
      (when language
	(cons language
	      (cl-subseq str (plist-get first-entity :offset)
			 (+ (plist-get first-entity :offset)
			    (plist-get first-entity :length))))))))

(defun telega-mnz--cancel ()
  "Cancel editing the current message."
  (interactive)
  (setq telega-mnz--inside-p nil)
  (exit-recursive-edit))

(defun telega-mnz--open-buffer (mnz-message &optional read-only before-text after-text
					    old-entities)
  "Open an edit buffer for MNZ-MESSAGE.
The buffer will be read-only if READ-ONLY is non-nil.
AFTER-TEXT will be appended.
BEFORE-TEXT will be prepended.
OLD-ENTITIES will be the old list of entities"
  (when telega-mnz--inside-p
    (error "Already inside an edit block"))
  (let ((buf (generate-new-buffer "*Telega Edit*"))
	(telega-mnz--inside-p t))
    (prog1 (save-window-excursion
	     (delete-other-windows)
	     (with-selected-window (split-window-below)
	       (switch-to-buffer buf)
	       (hack-local-variables)
	       (if (and (symbolp (car mnz-message))
			(commandp (car mnz-message)))
		   (funcall (car mnz-message))
		 (mapc #'funcall (car mnz-message)))
	       (insert (cdr mnz-message))
	       (use-local-map (copy-keymap telega-mnz-edit-map))
	       (set-keymap-parent (current-local-map) telega-mnz-edit-map)
	       (setq buffer-read-only read-only)
	       (if read-only (setq header-line-format "To exit, hit C-c C-c, C-x C-s, C-c C-k, or C-x C-z.")
		 (setq header-line-format
		       "To save this message, hit C-c C-c or C-x C-s, or M-x exit-recursive-edit RET.  \
To cancel saving this message, hit C-c C-z or C-c C-k."))
	       (recursive-edit)
	       (when (and (not read-only)
			  telega-mnz--inside-p)
		 `(:@type
		   "inputMessageText"
		   :telega-mode ,(prin1-to-string (car mnz-message))
		   :text (:@type
			  "formattedText"
			  :text ,(concat
				  (or before-text "")
				  (string-to-vector (buffer-string))
				  (or after-text "")) ;; TODO: dl macro
			  :entities
			  [,@(mapc (lambda (i)
				     (let ((offset (plist-get i :offset)))
				       (when (>= offset (length before-text))
					 (plist-put i :offset (- offset (- (length (cdr mnz-message))
									   (length (buffer-string))))))))
				   old-entities)
			   (:@type
			    "textEntity"
			    :offset ,(length before-text)
			    :length ,(1- (point-max))
			    :type (:@type
				   "textEntityTypePreCode"
				   :language
				   ,(if (symbolp (car mnz-message))
					(car (split-string (symbol-name
							    (car mnz-message)) "-mode"))
				      (prin1-to-string (car mnz-message)))))]
			  :@extra ,telega-server--extra)
		   :clear_draft t))))
      (kill-buffer buf))))

(defun telega-mnz--input-content-one-line-advice (oldfun imc)
  "If IMC is an inputMethodText, return an appropriate string.
Call OLDFUN with IMC otherwise"
  (cl-case (telega--tl-type imc)
    (inputMessageText
     (telega-ins--one-lined
      (telega-ins
       (concat "Code block: "
	       (plist-get imc :telega-mode)))))
    (t (funcall oldfun imc))))

(defun telega-mnz--edit-message-advice (oldfn msg)
  "Advice for `telega-msg-edit'.
OLDFN ##advice-super-doc.
MSG ##advice-doc."
  (save-excursion
    (when (eq (point) (line-beginning-position))
      (back-to-indentation)
      (forward-char 1))
    (setq msg (telega-msg-at (point)))
    (if (and (get-text-property (point) :telega-code-start)
	     (get-text-property (point) :telega-code-end))
	(if (telega-mnz--extract-data-from-message
	     msg
	     (get-text-property (point) :telega-code-start)
	     (get-text-property (point) :telega-code-end))
	    (let* ((data (telega-mnz--extract-data-from-message
			  msg
			  (get-text-property (point) :telega-code-start)
			  (get-text-property (point) :telega-code-end)))
		   (res (telega-mnz--open-buffer
			 data
			 (not (plist-get msg :can_be_edited))
			 (cl-subseq (telega--tl-get msg :content :text :text) 0
				    (get-text-property (point) :telega-code-start))
			 (cl-subseq (telega--tl-get msg :content :text :text)
				    (get-text-property (point) :telega-code-end))
			 (cl-remove-if (lambda (plist)
					 (and (eq (plist-get plist :offset)
						  (get-text-property (point) :telega-code-start))
					      (eq (plist-get plist :length)
						  (- (get-text-property (point) :telega-code-end)
						     (get-text-property (point) :telega-code-start)))))
				       (telega--tl-get msg :content :text :entities)))))
	      (if res
		  (telega--editMessageText
		   (telega-msg-chat msg)
		   msg res)
		(ding)))
	  (funcall oldfn msg))
      (funcall oldfn msg))))

(defun telega-mnz--read-major-mode ()
  "Read major mode from the user."
  (let* ((mm (mapcar #'symbol-name (cl-remove-if-not #'functionp telega-mnz-modes)))
	 (mnm (funcall telega-completing-read-function "Major mode: " mm)))
    (if (and (functionp (car (read-from-string mnm)))
	     (eq (cdr (read-from-string mnm)) (length mnm)))
	(car (read-from-string mnm))
      (error "Invalid major mode"))))

(defun telega-mnz-send-code (&optional prefix)
  "Prompt the user to send a piece of code.
If PREFIX is non-nil, the contents will be inserted into
the chatbuf input prompt."
  (interactive "p")
  (let* ((lmajor-mode (telega-mnz--read-major-mode))
	 (result (telega-mnz--open-buffer (cons lmajor-mode ""))))
    (when result
      (if (and prefix
	       (not (zerop prefix)))
	  (progn
	    (telega--sendMessage telega-chatbuf--chat result
				 (telega-chatbuf--replying-msg))
	    (telega-chatbuf--prompt-reset))
	(telega-chatbuf-input-insert result)))))

(defun telega-mnz-attach-code ()
  "Wrapper to call `telega-mnz-send-code' with no argument."
  (interactive)
  (telega-mnz-send-code))

(defun telega-mnz--parse-sexp-hook ()
  "Set `parse-sexp-lookup-properties' to t in a buffer-local way."
  (set (make-local-variable 'parse-sexp-lookup-properties) t))

(advice-add 'telega--entities-as-faces :around
            #'telega-mnz--entities-as-faces-advice)

(advice-add 'telega-msg-edit :around
	    #'telega-mnz--edit-message-advice)

(advice-add 'telega-ins--input-content-one-line :around
	    #'telega-mnz--input-content-one-line-advice)

(add-hook 'telega-chat-mode-hook #'telega-mnz--parse-sexp-hook)

(define-key telega-chat-mode-map (kbd "C-c '") #'telega-mnz-send-code)

(unless (assoc "code" telega-chat-attach-commands)
  (add-to-list 'telega-chat-attach-commands
               (list "code" nil #'telega-mnz-attach-code)
               'append))

(provide 'telega-mnz)

;;; telega-mnz.el ends here

;;; telega-mnz.el --- Display code (and other Emacs content) inside telega buffers  -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Evgeny Zajcev.
;; Copyright (c) 2020 Would (oldosfan).
;; Copyright (c) 2020 Brett Gilio.

;; Package-Requires: ((telega "0.6.30") (language-detection 0.1.0))

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

;;; ellit-org:
;; ** /telega-mnz.el/ -- Display Emacs content inside Telega messages.
;;
;; To use =telega-mnz=, simply ~(require 'telega-mnz)~.  There is no
;; global minor mode for =telega-mnz=, so you can't disable it after
;; ~(require 'telega-mnz)~.
;;
;; You can attach code using {{{kbd(M-x telega-mnz-attach-code RET)}}}
;; or {{{kbd(M-x telega-mnz-send-code RET)}}}.
;;
;; Optionally depends on =language-detection= Emacs package.  If
;; =language-detection= is available, then laguage could be detected
;; automatically for code blocks without language explicitly
;; specified.  Install =language-detection= with {{{kbd(M-x
;; package-install RET language-detection RET)}}}
;;
;; =telega-mnz= installs
;; {{{where-is(telega-mnz-send-region-as-code,telega-prefix-map)}}}
;; binding into [[#telega-prefix-map][telega prefix map]] to attach
;; region as code to a chatbuf.
;;
;; Also, =telega-mnz= installs ~code~ [[#attaching_media][media
;; attachment type]], use it with {{{kbd(C-c C-a code RET)}}} in
;; chatbuf.

;;; Code:

(require 'telega)
(require 'cl-lib)

;;; ellit-org:
;;
;; Customizable options:
;; - {{{user-option(telega-mnz-mode-for, 2)}}}
(defcustom telega-mnz-mode-for 'all
  "*Chat filter for `global-telega-mnz-mode'.
Global mnz mode enabled `telega-mnz-mode' only for chats matching
this chat filter."
  :type 'list
  :group 'telega-modes)

;;; ellit-org:
;; - {{{user-option(telega-mnz-keep-pre-face, 2)}}}
(defcustom telega-mnz-keep-pre-face t
  "Non-nil to keep `telega-entity-type-pre' face on the highlighted text."
  :type 'boolean
  :group 'telega-modes)

;;; ellit-org:
;; - {{{user-option(telega-mnz-edit-code-block, 2)}}}
(defcustom telega-mnz-edit-code-block 'query
  "How to edit message containing mnz code blocks."
  :type '(choice (const :tag "Edit whole message" nil)
                 (const :tag "Ask user what to edit" 'query)
                 (const :tag "Always edit code block at point" t))
  :group 'telega-modes)

;;; ellit-org:
;; - {{{user-option(telega-mnz-use-language-detection, 2)}}}
(defcustom telega-mnz-use-language-detection (fboundp 'language-detection-string)
  "*Non-nil to use `language-detection' for blocks without specified language."
  :type 'boolean
  :group 'telega-modes)

(defcustom telega-mnz-edit-display-buffer-action
  '((display-buffer-reuse-window display-buffer-same-window))
  "Action value when poping to code edit buffer.
See docstring for `display-buffer' for the values."
  :type 'cons
  :group 'telega-modes)

(defvar telega-mnz-languages
  '((ada . ada-mode)
    (awk . awk-mode)
    (c . c-mode)
    (clojure . clojure-mode)
    (cpp . c++-mode)
    (csharp . csharp-mode)
    (scheme . scheme-mode)              ; not in language-detection
    (css . css-mode)
    (dart . dart-mode)
    (delphi . delphi-mode)
    (diff . diff-mode)                  ; not in language-detection
    (emacslisp . emacs-lisp-mode)
    (erlang . erlang-mode)
    (fortran . fortran-mode)
    (go . go-mode)
    (groovy . groovy-mode)
    (haskell . haskell-mode)
    (html . html-mode)
    (java . java-mode)
    (javascript . javascript-mode)
    (json . json-mode)
    (kotlin . kotlin-mode)              ; not in language-detection
    (latex . latex-mode)
    (lisp . lisp-mode)
    (lua . lua-mode)
    (matlab . matlab-mode)
    (objc . objc-mode)
    (octave . octave-mode)              ; not in language-detection
    (org . org-mode)                    ; not in language-detection
    (outline . outline-mode)            ; not in language-detection
    (perl . perl-mode)
    (php . php-mode)
    (prolog . prolog-mode)
    (python . python-mode)
    (r . ess-r-mode)
    (ruby . ruby-mode)
    (rust . rust-mode)
    (scala . scala-mode)
    (shell . sh-mode)
    (smalltalk . smalltalk-mode)
    (sml . sml-mode)
    (sql . sql-mode)
    (swift . swift-mode)
    (visualbasic . visual-basic-mode)
    (xml . xml-mode))
  "Alist of languages mapping to Emacs modes.
Most of these languages available for language detection.")

(defvar telega-mnz--inside-p nil
  "Will be bound to t when inside code block.")

(defvar telega-mnz-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'exit-recursive-edit)
    (define-key map (kbd "C-x C-s") #'exit-recursive-edit)
    (define-key map (kbd "C-c C-z") #'telega-mnz-edit-cancel)
    (define-key map (kbd "C-c C-k") #'telega-mnz-edit-cancel)
    map)
  "The keymap to be used when editing code messages.")

(defun telega-mnz--render-text-for-mode (text mode)
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
        (let ((ret (propertize (buffer-string) 'syntax-table (syntax-table))))
          (when telega-mnz-keep-pre-face
            (add-face-text-property 0 (length ret)
                                    'telega-entity-type-pre 'append ret))
          (cl-assert (= (length text) (length ret)))
          (prog1 ret
            (kill-buffer (get-buffer "*Telega Mnz Fontification*")))))

    (error (format "!<<Error during code block fontification>>!\n%s\n%s"
                   (prin1-to-string text) text))))

(defun telega-mnz--language-for-mode (mode)
  "Return language string for Emacs major MODE."
  (symbol-name (or (car (cl-find mode telega-mnz-languages :key #'cdr)) mode)))

(defun telega-mnz--mode-for-language (language &optional code-text)
  "Return Emacs mode suitable to edit LANGUAGE code.
CODE-TEXT is optional text for the code, used for automatic
language detection."
  (or (alist-get (intern language) telega-mnz-languages)
      (let ((modes-list (mapcar #'cdr telega-mnz-languages)))
        ;; 1. makes language such at "c++" work
        ;; 2. makes language such as "erlang-mode" work
        (or (car (memq (intern (concat language "-mode")) modes-list))
            (car (memq (intern language) modes-list))))

      ;; Try language detection
      (when (and telega-mnz-use-language-detection
                 (fboundp 'language-detection-string)
                 code-text)
        (alist-get (funcall #'language-detection-string code-text)
                   telega-mnz-languages))))

(defun telega-mnz--mode-for-entity (ent ent-text)
  "Return Emacs major mode to highlight code in the ENT.
Return non-nil if no highlighting should be done for the ENT."
  (cl-assert ent)
  (cl-assert (eq (telega--tl-type (plist-get ent :type)) 'textEntityTypePreCode))
  (when-let ((language (telega-tl-str (plist-get ent :type) :language)))
    (telega-mnz--mode-for-language
     language (telega--desurrogate-apply ent-text 'no-props))))

(defun telega-mnz--fmt-text-faces (oldfun fmt-text &optional msg)
  "Advice for `telega--fmt-text-faces' to fontify code blocks.
OLDFUN ##advice-super-doc.
ENTITIES TEXT ##advice-doc."
  (let ((new-text (funcall oldfun fmt-text msg)))
    (when (and msg
               (with-telega-chatbuf (telega-msg-chat msg 'offline)
                 telega-mnz-mode))
      ;; Apply mnz code blocks highlighting
      (seq-doseq (ent (plist-get fmt-text :entities))
        (when (eq (telega--tl-type (plist-get ent :type)) 'textEntityTypePreCode)
          (when-let* ((ent-start (plist-get ent :offset))
                      (ent-stop (+ ent-start (plist-get ent :length)))
                      (ent-text (substring new-text ent-start ent-stop))
                      (mode (telega-mnz--mode-for-entity ent ent-text)))
            (setq new-text
                  (concat (substring new-text 0 ent-start)
                          (propertize
                           (telega-mnz--render-text-for-mode ent-text mode)
                           :telega-mnz-cb (list :mode mode :ent ent))
                          (substring new-text ent-stop)))))))
    new-text))

(defun telega-mnz-edit-cancel ()
  "Cancel editing the current message."
  (interactive)
  (setq telega-mnz--inside-p nil)
  (exit-recursive-edit))

(defun telega-mnz-edit--code (lang &optional code)
  "Edit CODE for language LANG in the separate buffer.
Return code after edition.
Return nil if edition has been canceled."
  (when telega-mnz--inside-p
    (user-error "Already editing code block"))
  (let ((buf (generate-new-buffer "*Telega Mnz Edit*"))
        (telega-mnz--inside-p t))

  ))

(defun telega-mnz--open-buffer (mnz-message &optional read-only before-text after-text
                                            old-entities)
  "Open an edit buffer for MNZ-MESSAGE.
The buffer will be read-only if READ-ONLY is non-nil.
AFTER-TEXT will be appended.
BEFORE-TEXT will be prepended.
OLD-ENTITIES will be the old list of entities"
  (when telega-mnz--inside-p
    (user-error "Already editing code block"))
  (let ((buf (generate-new-buffer "*Telega Mnz Edit*"))
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
               (setq header-line-format
                     (format (if read-only
                                 "To exit, hit %s or %s."
                               "To save this message, hit %s.  \
To cancel saving this message, hit %s.")
                             (telega-keys-description #'exit-recursive-edit)
                             (telega-keys-description #'telega-mnz-edit-cancel)))
               (recursive-edit)
               (when (and (not read-only) telega-mnz--inside-p)
                 `(:@type
                   "inputMessageText"
                   :telega-mnz-cb-mode ,(prin1-to-string (car mnz-message))
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
  "If IMC is an inputMessageText, return an appropriate string.
Call OLDFUN with IMC otherwise"
  (if (plist-get imc :telega-mnz-cb-mode)
      (progn
        (cl-assert (eq (telega--tl-type imc) 'inputMessageText))
        (telega-ins--one-lined
         (telega-ins "Code block: "
                     (plist-get imc :telega-mnz-cb-mode))))

    ;; Default behaviour
    (funcall oldfun imc)))

(defun telega-mnz--msg-code-block-at (msg &optional pnt)
  "Return mnz code block at point PNT, extracting data from message MSG.
If PNT is nil, then current point is used.
Return nil, if no code block at PNT or `telega-mnz-mode' is not
enabled in corresponding chatbuf."
  (when (with-telega-chatbuf (telega-msg-chat msg 'offline)
          telega-mnz-mode)
    (unless pnt
      (setq pnt (if (bolp)
                    (save-excursion
                      (back-to-indentation)
                      (forward-char 1)
                      (point))
                  (point))))
    (cl-assert (eq msg (telega-msg-at pnt)))
    (get-text-property pnt :telega-mnz-cb)))

(defun telega-mnz--msg-code-block-edit (msg mnz-cb)
  "Edit mnz code block MNZ-CB.
Return formattedText."
  (let ((mode (plist-get mnz-cb :mode)))
    )
  )

(defun telega-mnz-msg-edit (msg)
  "telega-mnz awared command to edit message MSG."
  (interactive (list (telega-msg-at (point))))

  (if-let* ((mnz-cb (telega-mnz--msg-code-block-at msg))
            (_edit-p (or (when (eq telega-mnz-edit-code-block 'query)
                           (y-or-n-p (format "Edit «%s» code block? "
                                             (plist-get mnz-cb :mode))))
                         telega-mnz-edit-code-block)))
      (if-let* ((msg-fmt-text (telega--tl-get msg :content :text))
                (cb-fmt-text (telega-mnz--msg-code-block-edit msg mnz-cb))
                (cb-ent (plist-get mnz-cb :ent))
                (cb-start (plist-get cb-ent :offset))
                (cb-stop (+ cb-start (plist-get cb-ent :length)))
                (imc (list :@type "inputMessageText"
                           :text (telega-fmt-text-desurrogate
                                  (telega-fmt-text-concat
                                   (telega-fmt-text-substring
                                    msg-fmt-text 0 cb-start)
                                   cb-fmt-text
                                   (telega-fmt-text-substring
                                    msg-fmt-text cb-stop))))))
          (telega--editMessageText (telega-msg-chat msg) msg imc)
        (ding))

    ;; Default behaviour
    (telega-msg-edit msg)))

(defun telega-mnz--code-formatted-text (mode code)
  "Convert CODE in Emacs MODE to formattedText."
  (telega-string-fmt-text
   (concat "```" (telega-mnz--language-for-mode mode) "\n"
           (replace-regexp-in-string (regexp-quote "```") "\\'\\'\\'"
                                     (telega-strip-newlines code))
           "\n```")
   2))

(defun telega-mnz-attach-region-as-code (beg end)
  "Attach region in current buffer to some chatbuf's input as code."
  (interactive "*r")
  (let ((lang (telega-mnz--language-for-mode major-mode))
        (code (buffer-substring-no-properties beg end))
        (chat (telega-completing-read-chat "Attach code to chat: ")))
    (with-current-buffer (telega-chat--pop-to-buffer chat)
      (telega-chatbuf-input-insert
       (concat (when (telega-chatbuf-has-input-p) "\n")
               "```" lang "\n"
               (telega-strip-newlines code)
               "\n``` ")))))

(defun telega-mnz--code-markup-fmt (lang code)
  "Format code block CODE of language LANG to formattedText."
  (list :@type "formattedText"
        :text code
        :entities (vector
                   (list :@type "textEntity"
                         :offset 0
                         :length (telega-string-fmt-text-length code)
                         :type (list :@type "textEntityTypePreCode"
                                     :language lang)))))

(defun telega-mnz-attach-code (language code)
  "Attach a code block into chatbuf input."
  (interactive
   (when-let* ((lang (funcall telega-completing-read-function "Language: "
                              (mapcar #'symbol-name
                                      (mapcar #'car telega-mnz-languages))
                              ;; Filter only available languages
                              (lambda (lang-name)
                                (commandp
                                 (alist-get (intern lang-name)
                                            telega-mnz-languages)))))
               (mode (telega-mnz--mode-for-language language))
               (result (telega-mnz--open-buffer mode)))
     (list lang code)))

  ;; Ensure newline at the end of the code
  (unless (string-suffix-p "\n" code)
    (setq code (concat code "\n")))

  (telega-chatbuf-input-insert
   (telega-string-as-markup
    code (format "code: %s" language)
    (apply-partially #'telega-mnz--code-markup-fmt language))))

(define-minor-mode telega-mnz-mode
  "Toggle code highlight minor mode."
  :init-value nil
  :lighter " ◁Mnz"
  :group 'telega-modes

  (if telega-mnz-mode
      (progn
        (setq-local parse-sexp-lookup-properties t)
        )

    (kill-local-variable 'parse-sexp-lookup-properties)
    ))

(defun telega-mnz-mode--maybe (&optional arg)
  (when (telega-chat-match-p telega-chatbuf--chat telega-mnz-mode-for)
    (telega-mnz-mode arg)))

;;;###autoload
(define-minor-mode global-telega-mnz-mode
  "Global mode to highliting code in the messages."
  :init-value nil :global t :group 'telega-modes
  (if global-telega-mnz-mode
      (progn
        (add-hook 'telega-chat-mode-hook 'telega-mnz-mode--maybe)
        (dolist (buf telega--chat-buffers)
          (with-current-buffer buf
            (telega-mnz-mode--maybe 1))))

    (remove-hook 'telega-chat-mode-hook 'telega-mnz-mode--maybe)
    (dolist (buf telega--chat-buffers)
      (with-current-buffer buf
        (telega-mnz-mode -1)))))

(advice-add 'telega--fmt-text-faces :around
            #'telega-mnz--fmt-text-faces)


(define-key telega-prefix-map (kbd "'") #'telega-mnz-attach-region-as-code)
(define-key telega-chat-mode-map (kbd "C-c '") #'telega-mnz-attach-code)
(define-key telega-msg-button-map [remap telega-msg-edit] #'telega-mnz-msg-edit)

;; For `C-c C-a code RET'
(unless (assoc "code" telega-chat-attach-commands)
  (add-to-list 'telega-chat-attach-commands
               (list "code" nil #'telega-mnz-attach-code) 'append))

(provide 'telega-mnz)

;;; telega-mnz.el ends here

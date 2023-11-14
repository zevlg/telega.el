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
;; Global minor mode to highlight code blocks inside messages.
;;
;; Can be enabled globally in all chats matching
;; ~telega-mnz-mode-for~ (see below) chat filter with
;; ~(global-telega-mnz-mode 1)~ or by adding:
;;
;; #+begin_src emacs-lisp
;; (require 'telega-mnz)
;; (add-hook 'telega-load-hook 'global-telega-mnz-mode)
;; #+end_src
;;
;; Optionally depends on =language-detection= Emacs package.  If
;; =language-detection= is available, then laguage could be detected
;; automatically for code blocks without language explicitly
;; specified.  Install =language-detection= with {{{kbd(M-x
;; package-install RET language-detection RET)}}}
;;
;; =telega-mnz= installs {{{kbd(')}}}
;; (~telega-mnz-send-region-as-code~) binding into
;; [[#telega-prefix-map][telega prefix map]] to attach region as code
;; to a chatbuf.
;;
;; Also, =telega-mnz= installs ~code~ [[#attaching-media][media
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
Global mnz mode enables `telega-mnz-mode' only for chats matching
this chat filter."
  :type 'telega-chat-temex
  :group 'telega-modes)

;;; ellit-org:
;; - {{{user-option(telega-mnz-keep-pre-face, 2)}}}
(defcustom telega-mnz-keep-pre-face t
  "Non-nil to keep `telega-entity-type-pre' face on the highlighted text."
  :type 'boolean
  :group 'telega-modes)

;;; ellit-org:
;; - {{{user-option(telega-mnz-entity-types, 2)}}}
(defcustom telega-mnz-entity-types
  '(textEntityTypePre textEntityTypePreCode textEntityTypeCode)
  "List of entity types for which mnz performs highlighting."
  :type '(list symbol)
  :group 'telega-modes)

;;; ellit-org:
;; - {{{user-option(telega-mnz-edit-code-block, 2)}}}
(defcustom telega-mnz-edit-code-block 'query
  "How to edit message containing mnz code blocks."
  :type '(choice (const :tag "Edit whole message" nil)
                 (const :tag "Ask user what to edit" query)
                 (const :tag "Always edit code block at point" t))
  :group 'telega-modes)

;;; ellit-org:
;; - {{{user-option(telega-mnz-use-language-detection, 2)}}}
(defcustom telega-mnz-use-language-detection
  (when (fboundp 'language-detection-string)
    '(200 . 4))
  "*Non-nil to use `language-detection' for blocks without specified language.
Could be also a number, meaning that language detection is done
only for code larger then this number of chars."
  :type '(choice (boolean :tag "Enable/Disable")
                 (integer :tag "Minimum number of chars")
                 (cons (integer :tag "Minimum number of chars")
                       (integer :tag "Minimum number of lines")))
  :group 'telega-modes)

;;; ellit-org:
;; - {{{user-option(telega-mnz-edit-display-buffer-action, 2)}}}
(defcustom telega-mnz-edit-display-buffer-action
  '((display-buffer-below-selected))
  "Action value when poping to code edit buffer.
See docstring for `display-buffer' for the value meaning."
  :type (get 'display-buffer-alist 'custom-type)
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
  "The keymap to be used when editing mnz code blocks.")

(defun telega-mnz--render-text-for-mode (text mode)
  "Return a string with TEXT rendered in a buffer with MODE enabled."
  (condition-case-unless-debug nil
      (with-current-buffer (get-buffer-create "*Telega Mnz Fontification*")
        (erase-buffer)
        (insert text)
        ;; NOTE: suppress annoying messages from some major modes
        (let ((inhibit-message t))
          (if (and (symbolp mode)
                   (commandp mode))
              (funcall mode)
            (ignore-errors
              (mapc #'funcall mode))))
        ;; NOTE: font-lock might trigger errors, for example:
        ;;   (telega-mnz--render-text-for-mode "$ head -n2 /tmp/pechatnaya-forma.doc\n<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<?mso-application progid=\"Word.Document\"?>" 'xml-mode)
        ;;   ==>
        ;;   Debugger entered--Lisp error: (error "Invalid search bound (wrong side of point)")
        ;;     search-backward("<" 2 t)
        (ignore-errors
          (when font-lock-mode
            (font-lock-ensure))
          (when jit-lock-mode
            (jit-lock-fontify-now)))

        (let ((ret (propertize (buffer-string) 'syntax-table (syntax-table))))
          (when telega-mnz-keep-pre-face
            (add-face-text-property 0 (length ret)
                                    'telega-entity-type-pre 'append ret))
          (cl-assert (= (length text) (length ret)))
          (prog1 ret
            (kill-buffer (get-buffer "*Telega Mnz Fontification*")))))

    (error (message "telega-mnz: Error during code block fontification")
           text)))

(defun telega-mnz--language-for-mode (mode)
  "Return language string for Emacs major MODE."
  (symbol-name (or (car (cl-find mode telega-mnz-languages :key #'cdr)) mode)))

(defun telega-mnz--mode-for-language (language &optional code-text)
  "Return Emacs mode suitable to edit LANGUAGE code.
CODE-TEXT is optional text for the code, used for automatic
language detection.
If LANGUAGE is nil, then CODE-TEXT should be provided, and
language-detection is used in this case, used for
`textEntityTypePre' and `textEntityTypeCode' entities."
  (or (when language
        (cl-assert (stringp language))
        (or (alist-get (intern language) telega-mnz-languages)
            (let ((modes-list (mapcar #'cdr telega-mnz-languages)))
              ;; 1. makes language such at "c++" work
              ;; 2. makes language such as "erlang-mode" work
              (or (car (memq (intern (concat language "-mode")) modes-list))
                  (car (memq (intern language) modes-list))))))

      ;; Try language detection
      (when (and (fboundp 'language-detection-string)
                 code-text
                 ;; NOTE: Check code is large enough in case
                 ;; `telega-mnz-use-language-detection' is int or cons cell
                 (cond ((integerp telega-mnz-use-language-detection)
                        (>= (length code-text) telega-mnz-use-language-detection))
                       ((consp telega-mnz-use-language-detection)
                        (and (>= (length code-text)
                                 (car telega-mnz-use-language-detection))
                             (>= (length (string-split code-text nil t))
                                 (cdr telega-mnz-use-language-detection))))
                       (t telega-mnz-use-language-detection)))
        (alist-get (funcall #'language-detection-string code-text)
                   telega-mnz-languages))))

(defun telega-mnz--mode-for-entity (ent ent-text)
  "Return Emacs major mode to highlight code in the entity ENT.
ENT-TEXT is the entity text.
Return nil if no highlighting should be done for this entity."
  (cl-assert ent)
  (cl-assert (memq (telega--tl-type (plist-get ent :type))
                   telega-mnz-entity-types))
  ;; NOTE: use `language-detection' for `textEntityTypePre' entities
  (telega-mnz--mode-for-language
   (telega-tl-str (plist-get ent :type) :language)
   (telega--desurrogate-apply ent-text 'no-props)))

(defun telega-mnz--formatted-text (text entity-type)
  "Return TEXT as formattedText marking it with ENTITY-TYPE."
  (list :@type "formattedText"
        :text text
        :entities (vector
                   (list :@type "textEntity"
                         :offset 0
                         :length (telega-string-fmt-text-length text)
                         :type entity-type))))

(defun telega-mnz--code-markup-fmt (lang code)
  "Format code block CODE of language LANG to formattedText."
  (telega-mnz--formatted-text
   code (list :@type "textEntityTypePreCode" :language lang)))

(defvar telega-mnz-mode-lighter
  (concat " " (telega-symbol 'mode) "Mnz")
  "Lighter for the `telega-mnz-mode'.")

(define-minor-mode telega-mnz-mode
  "Toggle code highlight minor mode."
  :init-value nil
  :lighter telega-mnz-mode-lighter
  :group 'telega-modes

  (if telega-mnz-mode
      (progn
        (setq-local parse-sexp-lookup-properties t)
        )

    (kill-local-variable 'parse-sexp-lookup-properties)
    ))

(defun telega-mnz--fmt-text-faces (oldfun fmt-text &optional msg)
  "Advice for `telega--fmt-text-faces' to highlight code blocks.
OLDFUN ##advice-super-doc.
FMT-TEXT MSG ##advice-doc."
  (let ((new-text (funcall oldfun fmt-text msg)))
    (when (and msg
               (with-telega-chatbuf (telega-msg-chat msg 'offline)
                 telega-mnz-mode))
      ;; Apply mnz code blocks highlighting
      (seq-doseq (ent (plist-get fmt-text :entities))
        (when (memq (telega--tl-type (plist-get ent :type))
                    telega-mnz-entity-types)
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

(defun telega-mnz--recursive-edit-code (mode &optional code
                                             point-offset read-only-p)
  "Edit CODE in a separate buffer using Emacs major MODE.
Return code after edition.
Return nil if edition has been canceled.
If READ-ONLY-P is non-nil, then open buffer as read only."
  (when telega-mnz--inside-p
    (user-error "Already editing code block"))
  (let ((buf (generate-new-buffer "*Telega Mnz Edit*"))
        (telega-mnz--inside-p t))
    (unwind-protect
        (save-window-excursion
          (pop-to-buffer buf telega-mnz-edit-display-buffer-action)
          (hack-local-variables)

          (when code
            (save-excursion
              (insert code))
            (when point-offset
              (goto-char point-offset)))
          ;; Enable corresponding Emacs mode
          (dolist (mode-cmd (if (commandp mode)
                                (list mode)
                              (cl-assert (listp mode))
                              (cl-assert (cl-every #'commandp mode))
                              mode))
            (funcall mode-cmd))
          (setq buffer-read-only read-only-p)
          ;; NOTE: Construct and use proper keymap by mergin mode's
          ;; keymap and `telega-mnz-edit-map'
          (let ((map (copy-keymap telega-mnz-edit-map)))
            (set-keymap-parent map (current-local-map))
            (use-local-map map))
          (setq header-line-format
                (format (if read-only-p
                            "To exit, hit %s or %s."
                          "To save, hit %s.  \
To cancel, hit %s.")
                        (telega-keys-description #'exit-recursive-edit)
                        (telega-keys-description #'telega-mnz-edit-cancel)))
          (recursive-edit)
          (when (and (not read-only-p) telega-mnz--inside-p)
            (buffer-string)))

      (when (buffer-name buf)
        (kill-buffer buf)))
    ))

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
  "Edit mnz code block MNZ-CB for the MSG message.
Return edited code as string."
  (let ((mode (plist-get mnz-cb :mode))
        (ent-start (telega--tl-get mnz-cb :ent :offset))
        (ent-length (telega--tl-get mnz-cb :ent :length))
        (msg-text (telega--tl-get msg :content :text :text)))
    (telega-mnz--recursive-edit-code
     mode (substring msg-text ent-start (+ ent-start ent-length))
     nil                                ; TODO: calculate point-offset
     (not (telega--tl-get msg :can_be_edited)))
    ))

(defun telega-mnz-msg-edit (msg &optional markup-arg)
  "Command to edit message MSG in a telega-mnz aware way."
  (interactive (list (telega-msg-at (point)) current-prefix-arg))

  (if-let* ((mnz-cb (telega-mnz--msg-code-block-at msg))
            (edit-p
             (if (eq telega-mnz-edit-code-block 'query)
                 (y-or-n-p (format "%s «%s» code block? (`n' to edit message)"
                                   (if (plist-get msg :can_be_edited)
                                       "Edit" "View")
                                   (plist-get mnz-cb :mode)))
               telega-mnz-edit-code-block)))
      (if-let* ((msg-fmt-text (telega--tl-get msg :content :text))
                (new-code (telega-mnz--msg-code-block-edit msg mnz-cb))
                (cb-ent (plist-get mnz-cb :ent))
                (cb-start (plist-get cb-ent :offset))
                (cb-stop (+ cb-start (plist-get cb-ent :length)))
                (imc (list :@type "inputMessageText"
                           :text (telega-fmt-text-desurrogate
                                  (telega-fmt-text-concat
                                   (telega-fmt-text-substring
                                    msg-fmt-text 0 cb-start)
                                   (telega-fmt-text
                                    new-code (plist-get cb-ent :type))
                                   (telega-fmt-text-substring
                                    msg-fmt-text cb-stop))))))
          (telega--editMessageText msg imc)
        (ding))

    ;; Default behaviour
    (telega-msg-edit msg markup-arg)))

(defun telega-mnz--chatbuf-attach-internal (language code)
  "Attach CODE of LANGUAGE to the chatbuf input."
  (cl-assert (and (stringp language) (stringp code)))
  ;; Ensure newline at the end of the code
  (unless (string-suffix-p "\n" code)
    (setq code (concat code "\n")))

  (telega-chatbuf-input-insert
   (telega-string-as-markup
    code (format "code: %s" language)
    (lambda (code-text)
      (telega-fmt-text code-text
                       (list :@type "textEntityTypePreCode"
                             :language language))))))

(defun telega-mnz-chatbuf-attach-code (language)
  "Interactively attach a code of the LANGUAGE into chatbuf input.
For non-interactive code attach, use `telega-mnz--chatbuf-attach-internal'."
  (interactive
   (list (funcall telega-completing-read-function "Language: "
                  (mapcar #'symbol-name
                          (mapcar #'car telega-mnz-languages))
                  ;; Filter only available languages
                  (lambda (lang-name)
                    (commandp
                     (alist-get (intern lang-name)
                                telega-mnz-languages))))))

  ;; NOTE: nil value for CODE means interactive editing has been
  ;; canceled
  (when-let ((code (telega-mnz--recursive-edit-code
                    (or (telega-mnz--mode-for-language language)
                        'fundamental-mode))))
    (telega-mnz--chatbuf-attach-internal language code)))

(defun telega-mnz-attach-region-as-code (beg end)
  "Attach region in current buffer to some chatbuf's input as code.
BEG is the beginning of the region.
END is the end of the region."
  (interactive "r")
  (let ((lang (telega-mnz--language-for-mode major-mode))
        (code (buffer-substring-no-properties beg end))
        (chat (telega-completing-read-chat "Attach code to chat: ")))
    (with-current-buffer (telega-chat--pop-to-buffer chat)
      (telega-mnz--chatbuf-attach-internal lang (telega-strip-newlines code)))))

(defun telega-mnz-mode--maybe (&optional arg)
  "Enable `telega-mnz-mode' if the current chatbuf is applicable.
Current chatbuf is applicable if it matches `telega-mnz-mode-for' chat filter.
ARG is passed directly to function `telega-mnz-mode'."
  (when (telega-chat-match-p telega-chatbuf--chat telega-mnz-mode-for)
    (telega-mnz-mode arg)))

;;;###autoload
(define-minor-mode global-telega-mnz-mode
  "Global mode to highliting code in the messages."
  :init-value nil :global t :group 'telega-modes
  (if global-telega-mnz-mode
      (progn
        (add-hook 'telega-chat-mode-hook 'telega-mnz-mode--maybe)
        (dolist (buf (telega-chat-buffers))
          (with-current-buffer buf
            (telega-mnz-mode--maybe 1))))

    (remove-hook 'telega-chat-mode-hook 'telega-mnz-mode--maybe)
    (dolist (buf (telega-chat-buffers))
      (with-current-buffer buf
        (telega-mnz-mode -1)))))


(advice-add 'telega--fmt-text-faces :around
            #'telega-mnz--fmt-text-faces)

(define-key telega-prefix-map (kbd "'") #'telega-mnz-attach-region-as-code)
(define-key telega-chat-mode-map (kbd "C-c '") #'telega-mnz-chatbuf-attach-code)
(define-key telega-msg-button-map [remap telega-msg-edit] #'telega-mnz-msg-edit)

;; For `C-c C-a code RET'
(unless (assoc "code" telega-chat-attach-commands)
  (add-to-list 'telega-chat-attach-commands
               '("code" (return t) telega-mnz-chatbuf-attach-code) 'append))

(provide 'telega-mnz)

;;; telega-mnz.el ends here

;;; index-html-gen.el --- index.html generator  -*- lexical-binding:t -*-

(require 'org)
(require 'ox)
(require 'ellit-org)
(let ((load-prefer-newer t))
  (require 'telega))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (emacs-lisp . t)))

;; Don’t ask to execute a code block.
(setq org-confirm-babel-evaluate nil)

(setq org-export-with-broken-links 'mark)

;; No Author/Created postamble block
(setq org-html-postamble nil)

;; 
(let ((toc-entry (assoc "Table of Contents" org-export-dictionary)))
  (setcdr toc-entry
          (cons (list "telega" :html (format "<img id=\"toc-logo\" src=\"https://raw.githubusercontent.com/zevlg/telega.el/master/etc/telega-logo.svg\"/>&nbsp;Telega Manual (v%s)" telega-version))
                (cdr toc-entry))))

(setq org-export-default-language "telega")

(defun telega-org-title-gen-custom-id (origfun datum)
  (let ((ret (funcall origfun datum)))
    (if (and (eq (org-element-type datum) 'headlines)
             (not (assq 'custom-id ret)))
        (cons (cons 'custom-id
                    (ellit-org-toc--ref-github-style
                     (org-element-property :value datum)))
              ret)
      ret)))

(advice-add 'org-export-search-cells
            :around 'telega-org-title-gen-custom-id)

;; Use custom links
;(setq org-html-self-link-headlines t)

(defconst telega-octicon-svg "<svg class=\"octicon octicon-link\" viewBox=\"0 0 16 16\" version=\"1.1\" width=\"16\" height=\"16\" aria-hidden=\"true\"><path fill-rule=\"evenodd\" d=\"M7.775 3.275a.75.75 0 001.06 1.06l1.25-1.25a2 2 0 112.83 2.83l-2.5 2.5a2 2 0 01-2.83 0 .75.75 0 00-1.06 1.06 3.5 3.5 0 004.95 0l2.5-2.5a3.5 3.5 0 00-4.95-4.95l-1.25 1.25zm-4.69 9.64a2 2 0 010-2.83l2.5-2.5a2 2 0 012.83 0 .75.75 0 001.06-1.06 3.5 3.5 0 00-4.95 0l-2.5 2.5a3.5 3.5 0 004.95 4.95l1.25-1.25a.75.75 0 00-1.06-1.06l-1.25 1.25a2 2 0 01-2.83 0z\"></path></svg>")

(defun telega-org-html-headline (origfunc headline contents info)
  (let* ((id (org-html--reference headline info))
         (headline-func
          (lambda (&rest args)
            (concat (format "<a class=\"self-link\" href=\"#%s\">" id)
                    telega-octicon-svg "</a>"
                    (apply #'org-html-format-headline-default-function args))))
         (orig-headline-func (plist-get info :html-format-headline-function)))
    (plist-put info :html-format-headline-function headline-func)
    (unwind-protect
        (funcall origfunc headline contents info)
      (plist-put info :html-format-headline-function orig-headline-func))
    ))

(advice-add 'org-html-headline
            :around 'telega-org-html-headline)

(defun telega-org-export-html (input-file output-file)
  "Export org INPUT-FILE to html OUTPUT-FILE."
  (with-current-buffer (find-file-noselect input-file)
    (org-export-to-file 'html output-file)))

;; Local Variables:
;; no-byte-compile: t
;; End:

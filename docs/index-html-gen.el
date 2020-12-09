(require 'org)
(require 'ox)
(require 'ellit-org)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (emacs-lisp . t)))

;; Don’t ask to execute a code block.
(setq org-confirm-babel-evaluate nil)

(setq org-export-with-broken-links 'mark)

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

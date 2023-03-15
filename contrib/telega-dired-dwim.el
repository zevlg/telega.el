;;; telega-dired-dwim.el --- Attach files from dired to telega chatbuf.  -*- lexical-binding:t -*-

;;; Commentary:

;;; ellit-org:
;; ** /telega-dired-dwim.el/ -- Attach files from dired in DWIM style
;;
;; This package advises ~dired-do-copy~ to attach files into visible chatbuf.
;; 
;; In dired, mark files you want to attach and press {{{kbd(C)}}}.  If
;; you have some chatbuf visible, marked files will be attached in
;; that chatbuf.

;;; Code:
(require 'telega)
(require 'dired)

(defun telega-dwim-chatbuf ()
  (get-window-with-predicate
   (lambda (window)
     (with-current-buffer (window-buffer window)
       (eq major-mode 'telega-chat-mode)))))

(defun dired-telega-copy ()
  (when-let*
      ((files (seq-filter #'file-regular-p (dired-get-marked-files)))
       (chatbuf (telega-dwim-chatbuf)))
    (select-window chatbuf)
    (mapc #'telega-chatbuf-attach-file files)))

(advice-add 'dired-do-copy :before-until
            (lambda (&rest _)
              (and (telega-dwim-chatbuf)
                   (dired-telega-copy)
                   t)))

(provide 'telega-dired-dwim)

;;; telega-dired-dwim.el ends here

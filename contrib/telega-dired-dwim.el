;;; telega-dired-dwim.el --- Attach files from dired to telega chatbuf.  -*- lexical-binding:t -*-

;;; Commentary:

;; ** /telega-dired-dwim.el/ -- Adds advice around dired-do-copy to support attaching files from dired to telega in DWIM style.

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

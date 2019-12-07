;;; Commentary:
;;
;; This packages is used for DEVELOPMENT purposes only. If you are
;; wanting to install telega from GNU Guix, please refer to the
;; relevant section in the README.
;;
;; This recipe is reflective of the latest recipe (including pending patches)
;; for upstream GNU Guix: https://git.savannah.gnu.org/cgit/guix.git
;;
;; This file was adapted by Brett Gilio from the original by Pierre Neidhardt.
;;;


;;; Instructions:
;;
;; To use as the basis for a development environment, run:
;;
;;   guix environment -l guix.scm
;;
;; To run it as an arbitrary local build:
;;
;;   guix build -f guix.scm
;;;


;;; Code:

(use-modules (gnu packages emacs)
	     (gnu packages emacs-xyz)
	     (gnu packages messaging)
	     (gnu packages python)
	     (guix build utils)
	     (guix build-system gnu)
	     (guix gexp)
	     ((guix licenses) #:prefix license:)
	     (guix packages)
	     (ice-9 match)
	     (ice-9 popen)
	     (ice-9 rdelim)
	     (srfi srfi-1)
	     (srfi srfi-26))

(define %source-dir (dirname (dirname (current-filename))))

(define git-file?
  (let* ((pipe (with-directory-excursion %source-dir
                 (open-pipe* OPEN_READ "git" "ls-files")))
         (files (let loop ((lines '()))
                  (match (read-line pipe)
                    ((? eof-object?)
                     (reverse lines))
                    (line
                     (loop (cons line lines))))))
         (status (close-pipe pipe)))
    (lambda (file stat)
      (match (stat:type stat)
        ('directory #t)
        ((or 'regular 'symlink)
         (any (cut string-suffix? <> file) files))
        (_ #f)))))

(define-public emacs-telega
  (package
    (name "emacs-telega")
    (version "0.0.0") ; since this is a checkout
    (source (local-file %source-dir #:recursive? #t #:select? git-file?))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:modules ((guix build gnu-build-system)
		  ((guix build emacs-build-system) #:prefix emacs:)
		  (guix build utils)
		  (guix build emacs-utils))
       #:imported-modules (,@%gnu-build-system-modules
			   (guix build emacs-build-system)
			   (guix build emacs-utils))
       #:phases
       (modify-phases %standard-phases
	 (add-after 'unpack 'prefix-patch
	   (lambda _
	     (substitute* "server/Makefile"
	       (("CC=cc")
		"CC=gcc")
	       (("INSTALL_PREFIX=\\$\\(HOME\\)/.telega")
		(string-append "INSTALL_PREFIX=" (assoc-ref %outputs "out")
			       "/bin"))
	       ;; Manually invoke `run_tests.py` after install phase.
	       (("python3 run_tests.py")
		""))
	     #t))
	 ;; Modify telega-util to reflect unique dir name in
	 ;; `telega-install-data' phase.
	 (add-after 'unpack 'telega-data-patch
	   (lambda _
	     (substitute* "telega-util.el"
	       (("\\(concat \"etc/\" filename\\) telega--lib-directory")
		"(concat \"telega-data/\" filename)
                    (locate-dominating-file telega--lib-directory
                                            \"telega-data\")"))
	     #t))
	 ;; The telega test suite checks for a version of Emacs
	 ;; compiled with imagemagick and svg support. Since we
	 ;; are using `emacs-minimal`, this step will fail.
	 ;; Grok the failing test, and remove problematic assertions.
	 (add-after 'unpack 'ert-suite-patch
	   (lambda _
	     (substitute* "telega-core.el"
	       (("\\(image-type-available-p 'imagemagick\\) nil")
		"t")
	       (("\\(image-type-available-p 'svg\\) nil")
		"t"))
	     #t))
	 ;; The server test suite has a hardcoded path.
	 ;; Reset this behavior to use the proper path.
	 (add-after 'unpack 'server-suite-patch
	   (lambda _
	     (substitute* "server/run_tests.py"
	       (("~/.telega/telega-server")
		(string-append (assoc-ref %outputs "out")
			       "/bin/telega-server")))
	     #t))
	 (add-after 'install 'run-server-suite
	   (lambda _
	     (invoke "python3" "server/run_tests.py")
	     #t))
	 (delete 'configure)
	 
	 ;; Build emacs-side using `emacs-build-system'
	 (add-after 'compress-documentation 'emacs-add-source-to-load-path
	   (assoc-ref emacs:%standard-phases 'add-source-to-load-path))
	 (add-after 'emacs-add-source-to-load-path 'emacs-install
	   (assoc-ref emacs:%standard-phases 'install))
	 ;; This step installs subdir /etc, which contains images, sounds and
	 ;; various other data, next to the site-lisp dir.
	 (add-after 'emacs-install 'telega-install-data
	   (lambda* (#:key outputs #:allow-other-keys)
	     (copy-recursively
	      "etc"
	      (string-append (assoc-ref outputs "out")
			     "/share/emacs/telega-data/"))
	     #t))
	 (add-after 'telega-install-data 'emacs-build
	   (assoc-ref emacs:%standard-phases 'build))
	 (add-after 'emacs-build 'emacs-make-autoloads
	   (assoc-ref emacs:%standard-phases 'make-autoloads)))))
    (propagated-inputs
     `(("emacs-visual-fill-column" ,emacs-visual-fill-column)))
    (native-inputs
     `(("tdlib" ,tdlib)
       ("emacs" ,emacs-minimal)
       ("python" ,python)))
    (synopsis "GNU Emacs client for the Telegram messenger")
    (description
     "Telega is full-featured, unofficial client for the Telegram messaging
platform for GNU Emacs.")
    (home-page "https://github.com/zevlg/telega.el")
    (license license:gpl3+)))

emacs-telega

;;; End.

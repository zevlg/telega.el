EMACS=emacs -Q -batch -f package-initialize -L ../ -L ../../ellit-org.el -l ellit-org

DOC_EL_SOURCES=../telega.el ../telega-*.el ../contrib/*.el

all: index.html

telega-manual.org: telega-ellit.org telega-note.org $(DOC_EL_SOURCES)
	$(EMACS) --eval '(progn (defvar telega-note-file "telega-note.org") (let ((load-prefer-newer t) (debug-on-error t)) (load-library "telega") (ellit-org-export "telega-ellit.org" "telega-manual.org")))'

index.html:
	cp telega-note-release.org telega-note.org
	make telega-manual.org
	$(EMACS) -L . -l index-html-gen.el --eval '(telega-org-export-html "index-html.org" "index.html")'

index-master.html:
	cp telega-note-master.org telega-note.org
	make telega-manual.org
	$(EMACS) -L . -l index-html-gen.el --eval '(telega-org-export-html "index-html.org" "index-master.html")'

.PHONY: index.html index-master.html 

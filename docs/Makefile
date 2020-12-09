EMACS=emacs

DOC_EL_SOURCES=../telega.el ../telega-*.el ../contrib/*.el

all: index.html

telega-manual.org: telega-ellit.org $(DOC_EL_SOURCES)
	$(EMACS) -Q -batch -f package-initialize -L ../ -l telega -L ../../ellit-org.el -l ellit-org --eval '(let ((debug-on-error t)) (ellit-org-export "telega-ellit.org" "telega-manual.org"))'

index.html: index-html.org telega-manual.org
	$(EMACS) index-html.org -Q -batch -f package-initialize -L ../../ellit-org.el -L . -l index-html-gen.el --funcall org-html-export-to-html --kill

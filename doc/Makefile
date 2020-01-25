EMACS=emacs

DOC_EL_SOURCES=../telega-root.el ../telega-filter.el ../telega-chat.el

all: telega-manual.org

telega-manual.org: telega-ellit.org $(DOC_EL_SOURCES)
	$(EMACS) -Q -batch -f package-initialize -L ../ -L ../../ellit-org.el -l ellit-org -l telega --eval '(let ((debug-on-error t)) (ellit-org-file-org "telega-ellit.org" "telega-manual.org"))'

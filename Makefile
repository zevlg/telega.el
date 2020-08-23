EMACS=emacs -Q

all: elisp-all doc-all server-all

test: elisp-test server-test

clean: elisp-clean doc-clean server-clean

# Elisp

sources = $(wildcard telega*.el)
objects = $(sources:.el=.elc)

elisp-all: $(objects)

.el.elc:
	$(EMACS) -batch -L . -f package-initialize -f byte-compile-file

# Doc

doc-all:
	$(MAKE) -C doc all

doc-clean:
	$(MAKE) -C doc clean

# Server

server-all:
	$(MAKE) -C server all

server-test:
	$(MAKE) -C server test

server-clean:
	$(MAKE) -C server clean



doc server:
	$(MAKE) -C $@

test-server:
	$(MAKE) -C server test

test: test.el
	$(EMACS) -batch -L . -f package-initialize -l ert -l test.el \
	         -f ert-run-tests-batch-and-exit
	$(MAKE) -C server $@


#EL_SOURCES=$(wildcard telega*.el)

compile:
	$(EMACS) -batch -L . -f package-initialize --eval '(byte-recompile-directory "." 0 t)'

doc:
	$(MAKE) -C doc

clean:
	@rm -vf *.elc contrib/*.elc
	$(MAKE) -C server $@

.PHONY: $(SERVER) clean test doc

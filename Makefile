EMACS=emacs -Q

SERVER_TARGETS=telega-server install

$(SERVER_TARGETS):
	$(MAKE) -C server $@

server-reinstall:
	$(MAKE) -C server clean
	$(MAKE) -C server install

test: test.el
	$(EMACS) -batch -L . -f package-initialize -l ert -l test.el \
	         -f ert-run-tests-batch-and-exit
	$(MAKE) -C server $@

#EL_SOURCES=$(wildcard telega*.el)

compile:
	$(EMACS) -batch -L . -f package-initialize --eval '(byte-recompile-directory "." 0 t)'

docs:
	$(MAKE) -C docs

clean:
	@rm -vf *.elc contrib/*.elc
	$(MAKE) -C server $@

.PHONY: $(SERVER) clean test docs

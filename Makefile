EMACS=emacs -Q

SERVER_TARGETS=telega-server install

$(SERVER_TARGETS):
	$(MAKE) -C server $@

test: test.el
	$(EMACS) -batch -L . -f package-initialize -l ert -l test.el \
	         -f ert-run-tests-batch-and-exit
	$(MAKE) -C server $@

#EL_SOURCES=$(wildcard telega*.el)

compile:
	$(EMACS) -batch -L . -f package-initialize --eval '(byte-recompile-directory "." 0 t)'

clean:
	@rm -vf *.elc
	$(MAKE) -C server $@

.PHONY: $(SERVER) clean test

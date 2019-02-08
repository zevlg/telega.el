EMACS=emacs -Q

SERVER_TARGETS=telega-server install

$(SERVER_TARGETS):
	$(MAKE) -C server $@

test: test.el
	$(EMACS) -batch -L . -l ert -l test.el \
	         -f ert-run-tests-batch-and-exit
	$(MAKE) -C server $@

#EL_SOURCES=$(wildcard telega*.el)

compile:
	$(EMACS) -batch -L . -f batch-byte-compile telega.el telega-*.el

clean:
	@rm -vf *.elc
	$(MAKE) -C server $@

.PHONY: $(SERVER) clean test

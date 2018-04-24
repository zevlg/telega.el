EMACS=emacs -Q

SERVER_TARGETS=telega-server clean install

$(SERVER_TARGETS):
	$(MAKE) -C server $@

test: test.el
	$(EMACS) -batch -L `pwd` -l ert -l test.el \
	         -f ert-run-tests-batch-and-exit

.PHONY: $(SERVER) test

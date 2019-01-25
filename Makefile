EMACS=emacs -Q

SERVER_TARGETS=telega-server install

$(SERVER_TARGETS):
	$(MAKE) -C server $@

test: test.el
	$(EMACS) -batch -L `pwd` -l ert -l test.el \
	         -f ert-run-tests-batch-and-exit
	$(MAKE) -C server $@

#EL_SOURCES=$(wildcard telega*.el)
EL_SOURCES=telega.el telega-core.el telega-root.el \
  telega-server.el telega-util.el telega-notifications.el \
  telega-chat.el telega-ins.el telega-filter.el telega-info.el \
  telega-media.el telega-msg.el telega-customize.el \
  telega-voip.el telega-webpage.el telega-vvnote.el telega-ffplay.el \
  telega-user.el

compile: $(EL_SOURCES)
	$(EMACS) -batch -L `pwd` -f batch-byte-compile $?

clean:
	@rm -vf *.elc
	$(MAKE) -C server $@

.PHONY: $(SERVER) clean test

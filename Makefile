EMACS=emacs -Q

SERVER_TARGETS=telega-server install

all: telega-server compile

$(SERVER_TARGETS):
	$(MAKE) -C server $@

server-reinstall:
	$(MAKE) -C server clean
	$(MAKE) -C server install

test: test.el
	$(EMACS) -batch -L . -l etc/telega-make \
	         -f telega-run-tests
	$(MAKE) -C server $@

EL_SOURCES=$(wildcard telega*.el)
ELC_FILES=$(patsubst %.el,%.elc,$(EL_SOURCES))

%.elc: %.el
	$(EMACS) -batch -L . -f package-initialize -f batch-byte-compile $<

elc: $(ELC_FILES)

compile:
	$(EMACS) -batch -L . -l etc/telega-make \
	         -f telega-byte-compile-everything

docs:
	$(MAKE) -C docs

clean:
	@rm -vf *.elc etc/*.elc docs/*.elc contrib/*.elc
	$(MAKE) -C server $@

.PHONY: $(SERVER) clean test docs

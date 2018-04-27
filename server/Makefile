CC=cc
CFLAGS=-Wall -g -pthread

PROG=telega-server

$(PROG): telega-server.c
	$(CC) $(CFLAGS) -o $(PROG) telega-server.c -ltdjson

install:
	@install -v -d $(HOME)/.telega
	@install -v $(PROG) $(HOME)/.telega

clean:
	@rm -vf $(PROG)

.PHONY: clean install

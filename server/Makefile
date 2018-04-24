CC=cc
CFLAGS=-Wall -g -pthread

PROG=telega-server

$(PROG): telega-server.c
	$(CC) $(CFLAGS) -o $(PROG) telega-server.c -ltdjson

install:
	@install -v -d $(HOME)/.telega1
	@install -v $(PROG) $(HOME)/.telega1

clean:
	@rm -vf $(PROG)

.PHONY: clean install

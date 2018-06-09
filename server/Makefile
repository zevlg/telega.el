CC=cc
CFLAGS=-Wall -g -pthread
LDFLAGS=-L/usr/local/lib -ltdjson

PROG=telega-server

$(PROG): telega-server.c
	$(CC) $(CFLAGS) -o $(PROG) telega-server.c $(LDFLAGS)

install: $(PROG)
	@install -v -d $(HOME)/.telega
	@install -v $(PROG) $(HOME)/.telega

clean:
	@rm -vf $(PROG)

.PHONY: clean install

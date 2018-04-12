CC=cc
CFLAGS=-Wall -g -pthread

PROG=telega-server

$(PROG): telega-server.c
	$(CC) $(CFLAGS) -o $(PROG) telega-server.c -ltdjson

.PHONY: clean

clean:
	rm -f $(PROG)

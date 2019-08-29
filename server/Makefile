CC=cc
CFLAGS=-I/usr/local/include -Wall -g -pthread
LDFLAGS=-L/usr/local/lib -ltdjson

PROG=telega-server
SOURCES=telega-server.c telega-dat.c telega-pngext.c

ifdef WITH_VOIP
CC=c++
CFLAGS+=-std=c++11 -DWITH_VOIP
LDFLAGS+=-ltgvoip
SOURCES+=telega-voip.cpp
endif

$(PROG): $(SOURCES)
	$(CC) $(CFLAGS) -o $(PROG) $(SOURCES) $(LDFLAGS)

install: $(PROG)
	@install -v -d $(HOME)/.telega
	@install -v $(PROG) $(HOME)/.telega

test:
	@python3 run_tests.py

clean:
	@rm -vf $(PROG)

.PHONY: clean install

CC=cc

ifndef LIBS_PREFIX
LIBS_PREFIX=/usr/local
endif

CFLAGS=-I$(LIBS_PREFIX)/include -Wall -g -pthread
LDFLAGS=-L$(LIBS_PREFIX)/lib -Wl,-rpath,$(LIBS_PREFIX)/lib -ltdjson

PROG=telega-server
SOURCES=telega-server.c telega-dat.c telega-pngext.c

ifdef WITH_VOIP
CC=c++
CFLAGS+=-std=c++11 -DWITH_VOIP -DTGVOIP_NO_STDOUT_LOGS
LDFLAGS+=-ltgvoip
SOURCES+=telega-voip.cpp
endif

ifdef WITH_TON
CFLAGS+=-DWITH_TON
LDFLAGS+=-ltonlibjson
endif

ifndef INSTALL_PREFIX
INSTALL_PREFIX=$(HOME)/.telega
endif

$(PROG): $(SOURCES)
	$(CC) $(CFLAGS) -o $(PROG) $(SOURCES) $(LDFLAGS)

install: $(PROG)
	@install -d $(INSTALL_PREFIX)
	@install $(PROG) $(INSTALL_PREFIX)

test:
	@python3 run_tests.py

clean:
	@rm -vf $(PROG)

.PHONY: clean install

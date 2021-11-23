CC=cc

ifndef LIBS_PREFIX
CFLAGS=`pkg-config --cflags tdjson`
LDFLAGS=`pkg-config --libs tdjson`
else
CFLAGS=-I$(LIBS_PREFIX)/include
LDFLAGS=-L$(LIBS_PREFIX)/lib -Wl,-rpath,$(LIBS_PREFIX)/lib -ltdjson
endif

CFLAGS+=-Wall -g -pthread

PROG=telega-server
SOURCES=telega-server.c telega-dat.c telega-pngext.c

ifeq ($(shell pkg-config --exists appindicator3-0.1 && echo yes), yes)
CFLAGS+=-DWITH_APPINDICATOR
CFLAGS+=`pkg-config --cflags appindicator3-0.1`
LDFLAGS+=`pkg-config --libs appindicator3-0.1`
SOURCES+=telega-appindicator.c
endif

ifdef WITH_VOIP
CC=c++
CFLAGS+=-std=c++11 -DWITH_VOIP -DTGVOIP_NO_STDOUT_LOGS
LDFLAGS+=-ltgvoip
SOURCES+=telega-voip.cpp
endif

ifndef INSTALL_PREFIX
INSTALL_PREFIX=$(HOME)/.telega
endif

$(PROG): $(SOURCES)
	$(CC) $(CFLAGS) -o $(PROG) $(SOURCES) $(LDFLAGS)

install: $(PROG)
	@install -d $(INSTALL_PREFIX)
	@install -v $(PROG) $(INSTALL_PREFIX)

test:
	@python3 run_tests.py

clean:
	@rm -vf $(PROG)

.PHONY: clean install

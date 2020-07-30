PREFIX = /usr/local
BINPREFIX = $(PREFIX)/bin

prog = telega-server

sources := $(wildcard *.c)
objects := $(sources:.c=.o)

LDLIBS += -ltdjson
LDFLAGS += -pthread

ifdef TGVOIP
sources += telega-voip.cpp
objects += telega-voip.o

LDLIBS += -ltgvoip
endif

.PHONY: all install test clean

all: $(prog)

$(prog): $(objects)
	$(CC) $(LDFLAGS) $^ $(LDLIBS) -o $@

.c.o:
	$(CC) $(CFLAGS) -c -o $@ $<

.cpp.o:
	$(CXX) $(CXXFLAGS) -c -o $@ $<

install: all
	install -D -m 755 $(prog) $(DESTDIR)$(BINPREFIX)

test: all
	python3 run_tests.py

clean:
	$(RM) $(prog) $(objects)

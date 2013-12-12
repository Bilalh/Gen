OS:=$(shell uname -s)

ifeq ($(OS),Darwin)
FLAGS=-Wall -g -std=gnu99
CC=clang
else
CFLAGS=-Wall -g   -fdiagnostics-show-caret -ftrack-macro-expansion -std=gnu99
CC=gcc
endif

LIBS=-lm
TARGETS=cputimeout

cputimeout: cputimeout.o get_process_info.o
	$(CC) $(CFLAGS) $(LIBS) $^ -o $@

clean:
	rm -f *~ *.o $(TARGETS)

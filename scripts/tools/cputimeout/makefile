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

cputimeout: cputimeout.o get_process_info.o list.o
	$(CC) $(CFLAGS) $(LIBS) $^ -o $@

# to force rebuilds when either changes
get_process_info.o : get_process_info_linux.c get_process_info_apple.c

clean:
	rm -f *~ *.o $(TARGETS)

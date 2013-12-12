CFLAGS=-Wall -g   -fdiagnostics-show-caret -ftrack-macro-expansion -std=gnu99
LIBS=-lm
TARGETS=cputimeout
CC=gcc

cputimeout: cputimeout.o
	$(CC) $(CFLAGS) $(LIBS) $^ -o $@

clean:
	rm -f *~ *.o $(TARGETS)

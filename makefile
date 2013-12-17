OS:=$(shell uname -s)

OBJS =  cputimeout.o get_process_info.o list.o
LIBS=-lm
TARGETS=cputimeout

ifeq ($(OS),Darwin)
CFLAGS=-Wall -g -std=gnu99 ${EXTRA_CFLAGS}
CC=clang
OBJS += get_process_info_apple.o
else
CFLAGS=-Wall -ggdb  -fdiagnostics-show-caret -ftrack-macro-expansion -std=gnu99 ${EXTRA_CFLAGS}
CC=gcc
OBJS +=  get_process_info_linux.o
endif

all : ${TARGETS}

cputimeout: $(OBJS)
	${CC} ${CFLAGS} ${LIBS} $^ -o $@

testing: test.o get_process_info.o list.o get_process_info_linux.o
	${CC} ${CFLAGS} ${LIBS} $^ -o $@


clean:
	rm -f *~ *.o ${TARGETS}

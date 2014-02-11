OS:=$(shell uname -s)

OBJS =  cputimeout.o get_process_info.o list.o
LIBS=-lm
TARGETS=cputimeout

ifeq ($(OS),Darwin)
CFLAGS=-Wall  -g -std=gnu99 ${EXTRA_CFLAGS}
CC=clang
OBJS += get_process_info_apple.o
else
GCC_EXTRA=-fdiagnostics-show-caret -ftrack-macro-expansion
CFLAGS=-Wall -ggdb  ${GCC_EXTRA} -std=gnu99 ${EXTRA_CFLAGS}
CC=gcc
OBJS +=  get_process_info_linux.o
endif

all : ${TARGETS}

cputimeout: $(OBJS)
	${CC} ${CFLAGS} -o $@ $^ ${LIBS}


clean:
	rm -f *~ *.o ${TARGETS}

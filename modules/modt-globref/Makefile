
ROOT = ../..

CC      = gcc
LD      = gcc
CFLAGS  = -ggdb3 -Wall
LDFLAGS =

all: modt-globref.so modt-globref.doc

%.so: %.o
	$(CC) -shared $(LDFLAGS) -o $@ $<

%.o: %.c
	gcc $(CFLAGS) -I$(ROOT)/src -fPIC -c $<

%.doc: %.c
	$(ROOT)/lib-src/make-docfile $< > $@

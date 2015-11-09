
ROOT = ../..

CC      = gcc
LD      = gcc
CFLAGS  = -ggdb3 -Wall
LDFLAGS =

all: modt-vector.so modt-vector.doc

%.so: %.o
	$(CC) -shared $(LDFLAGS) -o $@ $<

%.o: %.c
	gcc $(CFLAGS) -I$(ROOT)/src -fPIC -c $<

%.doc: %.c
	$(ROOT)/lib-src/make-docfile $< > $@

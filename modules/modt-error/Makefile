
ROOT = ../..

CC      = gcc
LD      = gcc
CFLAGS  = -fsanitize=address -ggdb3 -Wall
LDFLAGS = -fsanitize=address

all: modt-error.so modt-error.doc

%.so: %.o
	$(CC) -shared $(LDFLAGS) -o $@ $<

%.o: %.c
	gcc $(CFLAGS) -I$(ROOT)/src -fPIC -c $<

%.doc: %.c
	$(ROOT)/lib-src/make-docfile $< > $@

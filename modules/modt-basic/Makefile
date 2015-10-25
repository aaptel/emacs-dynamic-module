
ROOT = ../..

CC      = gcc
LD      = gcc
CFLAGS  = -ggdb3 -Wall -fsanitize=address
LDFLAGS = -fsanitize=address

all: modt-basic.so modt-basic.doc

%.so: %.o
	$(CC) -shared $(LDFLAGS) -o $@ $<

%.o: %.c
	gcc $(CFLAGS) -I$(ROOT)/src -fPIC -c $<

%.doc: %.c
	$(ROOT)/lib-src/make-docfile $< > $@

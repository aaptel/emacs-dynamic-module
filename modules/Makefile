
all: fmod.so fmod.doc

%.so: %.o
	gcc -shared -o $@ $<

%.o: %.c
	gcc -ggdb3 -Wall -I../src -I../lib -fPIC -c $<

%.doc: %.c
	../lib-src/make-docfile $< > $@

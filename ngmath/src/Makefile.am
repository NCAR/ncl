# This is an automake file for NCL
# Ed Hartnett 9/5/18

SUBDIRS = lib bin examples

libngmathbd.o: ngmathbd.o
	ld -r -o $@ $^

ngmathbd.o: ngmathbd.f
	$(FC) -c ${FCFLAGS} -o $@ $^

# Install this "lib" file.
robjdir = $(prefix)/lib/ncarg/robj
robj_HEADERS = libngmathbd.o

EXTRA_DIST = ngmathbd.f

CLEANFILES = *.o

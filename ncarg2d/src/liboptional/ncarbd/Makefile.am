# This is an automake file for NCL.
# Ed Hartnett 9/2/18

libncarbd.o: ncarbd.o
	ld -r -o $@ $^

ncarbd.o: ncarbd.f
	$(FC) -c ${FCFLAGS} -o $@ $^

# Install this "lib" file.
robjdir = $(prefix)/lib/ncarg/robj
robj_HEADERS = libncarbd.o

EXTRA_DIST = ncarbd.f

CLEANFILES = *.o

# This is an automake file for NCL.
# Ed Hartnett 9/2/18

libconras.o: conras.o conpms.o
	ld -r -o $@ $^

conras.o: conras.f
	$(FC) -c ${FCFLAGS} -o $@ $^
conpms.o: conpms.f
	$(FC) -c ${FCFLAGS} -o $@ $^

# Install this "lib" file.
robjdir = $(prefix)/lib/ncarg/robj
robj_HEADERS = libconras.o

EXTRA_DIST = conras.f conpms.f

CLEANFILES = *.o

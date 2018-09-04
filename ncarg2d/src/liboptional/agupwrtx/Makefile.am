# This is an automake file for NCL.
# Ed Hartnett 9/2/18

libagupwrtx.o: agpwrt.o
	ld -r -o $@ $^

agpwrt.o: agpwrt.f
	$(FC) -c ${FCFLAGS} -o $@ $^

# Install this "lib" file.
robjdir = $(prefix)/lib/ncarg/robj
robj_HEADERS = libagupwrtx.o

EXTRA_DIST = agpwrt.f

CLEANFILES = *.o

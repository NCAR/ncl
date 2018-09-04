# This is an automake file for NCL.
# Ed Hartnett 9/2/18

libconraq.o: conraq.o contor.o
	ld -r -o $@ $^

conraq.o: conraq.f
	$(FC) -c ${FCFLAGS} -o $@ $^
contor.o: contor.f
	$(FC) -c ${FCFLAGS} -o $@ $^

# Install this "lib" file.
robjdir = $(prefix)/lib/ncarg/robj
robj_HEADERS = libconraq.o

EXTRA_DIST = conraq.f contor.f

CLEANFILES = *.o

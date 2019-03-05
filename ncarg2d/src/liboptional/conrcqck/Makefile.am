# This is an automake file for NCL.
# Ed Hartnett 9/2/18

libconrcqck.o: calcnt.o  clgen.o  conbd.o  conrec.o  ezcntr.o  maxmin.o  quick.o
	ld -r -o $@ $^

calcnt.o: calcnt.f
	$(FC) -c ${FCFLAGS} -o $@ $^
clgen.o: clgen.f
	$(FC) -c ${FCFLAGS} -o $@ $^
conbd.o: conbd.f
	$(FC) -c ${FCFLAGS} -o $@ $^
conrec.o: conrec.f
	$(FC) -c ${FCFLAGS} -o $@ $^
ezcntr.o: ezcntr.f
	$(FC) -c ${FCFLAGS} -o $@ $^
maxmin.o: maxmin.f
	$(FC) -c ${FCFLAGS} -o $@ $^
quick.o: quick.f
	$(FC) -c ${FCFLAGS} -o $@ $^

# Install this "lib" file.
robjdir = $(prefix)/lib/ncarg/robj
robj_HEADERS = libconrcqck.o

EXTRA_DIST = calcnt.f clgen.f conbd.f conrec.f ezcntr.f maxmin.f	\
quick.f

CLEANFILES = *.o

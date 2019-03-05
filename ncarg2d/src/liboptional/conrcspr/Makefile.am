# This is an automake file for NCL.
# Ed Hartnett 9/2/18

libconrcspr.o: calcnt.o  clgen.o  conbd.o  conrec.o  crdrln.o  ezcntr.o  minmax.o  reord.o  stline.o
	ld -r -o $@ $^

calcnt.o: calcnt.f
	$(FC) -c ${FCFLAGS} -o $@ $^
clgen.o: clgen.f
	$(FC) -c ${FCFLAGS} -o $@ $^
conbd.o: conbd.f
	$(FC) -c ${FCFLAGS} -o $@ $^
conrec.o: conrec.f
	$(FC) -c ${FCFLAGS} -o $@ $^
crdrln.o: crdrln.f
	$(FC) -c ${FCFLAGS} -o $@ $^
ezcntr.o: ezcntr.f
	$(FC) -c ${FCFLAGS} -o $@ $^
minmax.o: minmax.f
	$(FC) -c ${FCFLAGS} -o $@ $^
reord.o: reord.f
	$(FC) -c ${FCFLAGS} -o $@ $^
stline.o: stline.f
	$(FC) -c ${FCFLAGS} -o $@ $^

# Install this "lib" file.
robjdir = $(prefix)/lib/ncarg/robj
robj_HEADERS = libconrcspr.o

EXTRA_DIST = calcnt.f clgen.f conbd.f conrec.f crdrln.f ezcntr.f	\
minmax.f reord.f stline.f

CLEANFILES = *.o

# This is an automake file for NCL.
# Ed Hartnett 9/2/18

libdashsmth.o: cfvld.o  curved.o  dashbd.o  dashdb.o  dashdc.o  drawpv.o  fdvdld.o  frstd.o  lastd.o  lined.o  reset.o  vectd.o
	ld -r -o $@ $^

cfvld.o: cfvld.f
	$(FC) -c ${FCFLAGS} -o $@ $^
curved.o: curved.f
	$(FC) -c ${FCFLAGS} -o $@ $^
dashbd.o: dashbd.f
	$(FC) -c ${FCFLAGS} -o $@ $^
dashdb.o: dashdb.f
	$(FC) -c ${FCFLAGS} -o $@ $^
dashdc.o: dashdc.f
	$(FC) -c ${FCFLAGS} -o $@ $^
drawpv.o: drawpv.f
	$(FC) -c ${FCFLAGS} -o $@ $^
fdvdld.o: fdvdld.f
	$(FC) -c ${FCFLAGS} -o $@ $^
frstd.o: frstd.f
	$(FC) -c ${FCFLAGS} -o $@ $^
lastd.o: lastd.f
	$(FC) -c ${FCFLAGS} -o $@ $^
lined.o: lined.f
	$(FC) -c ${FCFLAGS} -o $@ $^
reset.o: reset.f
	$(FC) -c ${FCFLAGS} -o $@ $^
vectd.o: vectd.f
	$(FC) -c ${FCFLAGS} -o $@ $^

# Install this "lib" file.
robjdir = $(prefix)/lib/ncarg/robj
robj_HEADERS = libdashsmth.o

EXTRA_DIST = cfvld.f curved.f dashbd.f dashdb.f dashdc.f drawpv.f	\
fdvdld.f frstd.f lastd.f lined.f reset.f vectd.f

CLEANFILES = *.o

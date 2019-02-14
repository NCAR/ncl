# This is an automake file for NCL.
# Ed Hartnett 9/2/18

libdashline.o: cfvld.o  curved.o  dashbd.o  dashdb.o  drawpv.o  frstd.o  lastd.o  lined.o  vectd.o
	ld -r -o $@ $^

cfvld.o: cfvld.f
	$(FC) -c ${FCFLAGS} -o $@ $^
curved.o: curved.f
	$(FC) -c ${FCFLAGS} -o $@ $^
dashbd.o: dashbd.f
	$(FC) -c ${FCFLAGS} -o $@ $^
dashdb.o: dashdb.f
	$(FC) -c ${FCFLAGS} -o $@ $^
drawpv.o: drawpv.f
	$(FC) -c ${FCFLAGS} -o $@ $^
frstd.o: frstd.f
	$(FC) -c ${FCFLAGS} -o $@ $^
lastd.o: lastd.f
	$(FC) -c ${FCFLAGS} -o $@ $^
lined.o: lined.f
	$(FC) -c ${FCFLAGS} -o $@ $^
vectd.o: vectd.f
	$(FC) -c ${FCFLAGS} -o $@ $^

# Install this "lib" file.
robjdir = $(prefix)/lib/ncarg/robj
robj_HEADERS = libdashline.o

EXTRA_DIST = cfvld.f curved.f dashbd.f dashdb.f drawpv.f frstd.f	\
lastd.f lined.f vectd.f

CLEANFILES = *.o

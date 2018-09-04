# This is an automake file for NCL.
# Ed Hartnett 9/2/18

libdashsupr.o: cfvld.o  curved.o  cutup.o  dashbd1024.o  dashdb.o  dashdc.o  drawpv.o  fdvdld.o  frstd.o  intmsk.o  lastd.o  lined.o  markl.o  pwrtm.o  remove32.o  reset.o  vectd.o
	ld -r -o $@ $^

cfvld.o: cfvld.f
	$(FC) -c ${FCFLAGS} -o $@ $^
curved.o: curved.f
	$(FC) -c ${FCFLAGS} -o $@ $^
cutup.o: cutup.f
	$(FC) -c ${FCFLAGS} -o $@ $^
dashbd1024.o: dashbd1024.f
	$(FC) -c ${FCFLAGS} -o $@ $^
# dashbd.o: dashbd.f
# 	$(FC) -c ${FCFLAGS} -o $@ $^
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
intmsk.o: intmsk.f
	$(FC) -c ${FCFLAGS} -o $@ $^
lastd.o: lastd.f
	$(FC) -c ${FCFLAGS} -o $@ $^
lined.o: lined.f
	$(FC) -c ${FCFLAGS} -o $@ $^
markl.o: markl.f
	$(FC) -c ${FCFLAGS} -o $@ $^
pwrtm.o: pwrtm.f
	$(FC) -c ${FCFLAGS} -o $@ $^
remove32.o: remove32.f
	$(FC) -c ${FCFLAGS} -o $@ $^
# removecray.o: removecray.f
# 	$(FC) -c ${FCFLAGS} -o $@ $^
# remove.o: remove.f
# 	$(FC) -c ${FCFLAGS} -o $@ $^
reset.o: reset.f
	$(FC) -c ${FCFLAGS} -o $@ $^
vectd.o: vectd.f
	$(FC) -c ${FCFLAGS} -o $@ $^


# Install this "lib" file.
robjdir = $(prefix)/lib/ncarg/robj
robj_HEADERS = libdashsupr.o

EXTRA_DIST = cfvld.f curved.f cutup.f dashbd1024.f dashdb.f dashdc.f	\
drawpv.f fdvdld.f frstd.f intmsk.f lastd.f lined.f markl.f pwrtm.f	\
remove32.f reset.f vectd.f

CLEANFILES = *.o

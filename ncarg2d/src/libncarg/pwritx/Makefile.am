# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_pwritx.la

# Sources for convenience library. Still need to get openpw working.
libncarg_pwritx_la_SOURCES = ccheck.f dcheck.f gethol.f gtdgts.f	\
gtnum.f gtnumb.f gtsign.f htable.f mkmsk.f pwritx.f pwrx.f pwrxbd.f	\
xtch.f openpw.f

openpw.f:
	sed -e 's+SED_DBDIR+$(DBDIR)+' <openpw.f.2.sed >openpw.F
	$(CPP) $(CPPFLAGS) -o openpw.f openpw.F

EXTRA_DIST = openpw.f.2.sed

CLEANFILES = openpw.f openpw.F

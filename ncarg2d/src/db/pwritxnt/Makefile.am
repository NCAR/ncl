# This is part of the NCL package.
# Ed Hartnett 9/1/18

LDADD = ${top_builddir}/ncarg2d/src/libncarg/libncarg.la
LDADD += $(top_builddir)/ncarg2d/src/libncarg_gks/libncarg_gks.la
LDADD += ${top_builddir}/common/src/libncarg_c/libncarg_c.la
LDADD += ${top_builddir}/ngmath/src/lib/gridpack/libngmath.la

bin_PROGRAMS = pwritxnt

PWRITXNT = ${top_builddir}/ncarg2d/src/db/pwritxnt/pwritxnt

pwritxnt_SOURCES = ccheck.f creb15.f crebin.f dcheck.f dport.f	\
mkmsk.f pcrbin.f xtch.f

pwritdata: pwritxnt pwritxc1 pwritxd1 pwritxc2 pwritxd2
	./pwritxnt

# Install these data files.
ncarg_databasedir = $(prefix)/lib/ncarg/database
ncarg_database_HEADERS = pwritdata

EXTRA_DIST = pwritxc1 pwritxc2 pwritxd1 pwritxd2

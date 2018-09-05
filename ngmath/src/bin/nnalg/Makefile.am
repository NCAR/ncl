# This is an automake file for NCL
# Ed Hartnett 9/5/18
LDADD = ${top_builddir}/ncarg2d/src/libncarg/libncarg.la
LDADD += $(top_builddir)/ncarg2d/src/libncarg_gks/libncarg_gks.la
LDADD += ${top_builddir}/common/src/libncarg_c/libncarg_c.la
LDADD += ${top_builddir}/ngmath/src/lib/gridpack/libngmath.la

# bin_PROGRAMS = nnalg

# nnalg_SOURCES = nadisp.f  nadraw.f  naskln.f  nasort.f

nadisp.f: nadisp.f.sed
	cp nadisp.f.sed nadisp.F
	${CPP} ${CPPFLAGS} -E -o nadisp.f nadisp.F

CLEANFILES = *.o nadisp.F

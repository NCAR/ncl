# This is an automake file for NCL.
# Ed Hartnett 9/2/18

LDADD = $(top_builddir)/ncarg2d/src/libncarg_gks/awi/ggkwdr_stub.o
LDADD += ${top_builddir}/ncarg2d/src/libncarg/libncarg.la
LDADD += $(top_builddir)/ncarg2d/src/libncarg_gks/libncarg_gks.la
LDADD += ${top_builddir}/common/src/libncarg_c/libncarg_c.la
LDADD += ${top_builddir}/ngmath/src/lib/gridpack/libngmath.la

bin_PROGRAMS = tlocal tgks0a

tlocal_SOURCES = tlocal32.f tiand.f tior.f lcm.f tgbyte.f tshift.f	\
tsbyte.f mskout.f

tgks0a_SOURCES = tgks0a.f

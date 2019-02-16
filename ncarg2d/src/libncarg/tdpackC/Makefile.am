# This is an automake file for NCL
# Ed Hartnett 8/30/18

LDADD = ${top_builddir}/common/src/libncarg_c/libncarg_c.la

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_tdpackc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_tdpackc_la_SOURCES = c_tdclrs.c c_tddtri.c c_tdez2d.c		\
c_tdez3d.c c_tdgeti.c c_tdgetr.c c_tdgrds.c c_tdgrid.c c_tdgtrs.c	\
c_tdinit.c c_tditri.c c_tdlbla.c c_tdlbls.c c_tdline.c c_tdlnpa.c	\
c_tdmtri.c c_tdotri.c c_tdpara.c c_tdplch.c c_tdprpa.c c_tdprpi.c	\
c_tdprpt.c c_tdseti.c c_tdsetr.c c_tdsort.c c_tdstri.c c_tdstrs.c	\
c_tdctri.c c_tdlndp.c c_tdlpdp.c c_tdcurv.c c_tdcudp.c

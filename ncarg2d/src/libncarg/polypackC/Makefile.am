# This is an automake file for NCL
# Ed Hartnett 8/30/18

LDADD = ${top_builddir}/common/src/libncarg_c/libncarg_c.la

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_polypackc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_polypackc_la_SOURCES = c_ppditr.c c_ppintr.c c_ppplcl.c	\
c_ppuntr.c c_ppppap.c c_ppunpo.c c_ppinpo.c c_ppdipo.c

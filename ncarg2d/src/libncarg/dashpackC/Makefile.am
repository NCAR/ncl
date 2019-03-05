# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_dashpackc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_dashpackc_la_SOURCES = c_dpcurv.c c_dpdraw.c c_dpfrst.c	\
c_dpgetc.c c_dpgeti.c c_dpgetr.c c_dplast.c c_dpline.c c_dpsetc.c	\
c_dpseti.c c_dpsetr.c c_dpsmth.c c_dpvect.c

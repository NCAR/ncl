# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_velvctc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_velvctc_la_SOURCES = c_ezvec.c c_velvct.c c_velvec.c		\
c_vvectr.c c_vvgetc.c c_vvgeti.c c_vvgetr.c c_vvinit.c c_vvrset.c	\
c_vvsetc.c c_vvseti.c c_vvsetr.c

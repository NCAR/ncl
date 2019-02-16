# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_areasc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_areasc_la_SOURCES = c_ardrln.c c_aredam.c c_argeti.c		\
c_argtai.c c_arinam.c c_armvam.c c_arpram.c c_arscam.c c_arseti.c	\
c_arsetr.c c_argetr.c c_ardbpa.c

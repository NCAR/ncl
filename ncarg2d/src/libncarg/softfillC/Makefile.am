# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_softfillc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_softfillc_la_SOURCES = c_sfgetc.c c_sfgeti.c c_sfgetp.c	\
c_sfgetr.c c_sfnorm.c c_sfsetc.c c_sfseti.c c_sfsetp.c c_sfsetr.c	\
c_sfsgfa.c c_sfwrld.c

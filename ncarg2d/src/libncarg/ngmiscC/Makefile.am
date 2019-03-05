# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_ngmiscc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_ngmiscc_la_SOURCES = c_ngckop.c c_ngdots.c c_nggcog.c		\
c_nggetc.c c_nggeti.c c_nggetr.c c_nggsog.c c_ngmftc.c c_ngpict.c	\
c_ngpswk.c c_ngreop.c c_ngritd.c c_ngsetc.c c_ngseti.c c_ngsetr.c	\
c_ngsrat.c c_ngwsym.c c_nglogo.c c_ngezlogo.c c_dnggcog.c

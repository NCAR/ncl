# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_conpackc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_conpackc_la_SOURCES = c_cpback.c c_cpcica.c c_cpclam.c		\
c_cpcldm.c c_cpcldr.c c_cpcltr.c c_cpcnrc.c c_cpezct.c c_cpgetc.c	\
c_cpgeti.c c_cpgetr.c c_cplbam.c c_cplbdr.c c_cpmviw.c c_cpmvrw.c	\
c_cppkcl.c c_cppklb.c c_cprect.c c_cprset.c c_cpsetc.c c_cpseti.c	\
c_cpsetr.c c_cpsprs.c c_cpsps1.c c_cpsps2.c

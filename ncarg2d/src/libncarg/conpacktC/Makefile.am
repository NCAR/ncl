# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_conpacktc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_conpacktc_la_SOURCES = c_ctback.c c_ctcica.c c_ctclam.c	\
c_ctcldm.c c_ctcldr.c c_ctcltr.c c_ctgetc.c c_ctgeti.c c_ctgetr.c	\
c_ctlbam.c c_ctlbdr.c c_ctmesh.c c_ctmviw.c c_ctmvrw.c c_ctpkcl.c	\
c_ctpklb.c c_ctrset.c c_ctsetc.c c_ctseti.c c_ctsetr.c c_cttdbf.c	\
c_cttdbm.c c_cttdca.c c_cttddm.c c_cttmrg.c c_cttmtl.c

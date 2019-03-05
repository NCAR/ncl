# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_supportc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_supportc_la_SOURCES = c_encd.c c_entsr.c c_nerro.c c_eprin.c	\
c_errof.c c_msbsf1.c c_msbsf2.c c_msceez.c c_mskrv1.c c_mskrv2.c	\
c_msntvl.c c_msshch.c c_mssrf1.c c_mssrf2.c c_mstrms.c c_q8qst4.c	\
c_retsr.c c_semess.c c_seter.c c_icloem.c c_icfell.c c_idbvip.c		\
c_idsfft.c

# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_plotcharc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_plotcharc_la_SOURCES = c_pcdlsc.c c_pcgetc.c c_pcgeti.c	\
c_pcgetr.c c_pchiqu.c c_pcloqu.c c_pcmequ.c c_pcmpxy.c c_pcpnwi.c	\
c_pcrset.c c_pcsetc.c c_pcseti.c c_pcsetr.c c_plchhq.c c_plchlq.c	\
c_plchmq.c

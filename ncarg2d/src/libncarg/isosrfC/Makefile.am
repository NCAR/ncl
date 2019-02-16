# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_isosrfc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_isosrfc_la_SOURCES = c_ezisos.c c_isosrf.c c_trn32i.c		\
c_issetr.c c_isseti.c c_ispltf.c c_isgetr.c c_isgeti.c c_istr32.c	\
c_pwrzi.c

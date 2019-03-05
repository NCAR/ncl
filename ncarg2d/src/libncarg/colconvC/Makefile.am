# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_colconvc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_colconvc_la_SOURCES = c_hlsrgb.c c_hsvrgb.c c_rgbhls.c	\
c_rgbhsv.c c_rgbyiq.c c_yiqrgb.c

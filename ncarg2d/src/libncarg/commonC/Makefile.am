# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_commonc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_commonc_la_SOURCES = array.c chk_ret_str.c

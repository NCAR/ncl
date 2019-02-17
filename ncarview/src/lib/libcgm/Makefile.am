# This is an automake file for NCL.
# Ed Hartnett 9/6/18

lib_LTLIBRARIES = libcgm.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

libcgm_la_SOURCES = cgm_tools.c meta_edit.c misc.c mem_file.c name.c

noinst_HEADERS = cgm_tools.h cgmdef.h

# Header get installed in special subdirectory.
ncargdir = $(prefix)/include/ncarg
ncarg_HEADERS = cgmdef.h

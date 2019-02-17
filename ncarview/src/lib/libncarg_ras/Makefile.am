# This is an automake file for NCL.
# Ed Hartnett 9/6/18

lib_LTLIBRARIES = libncarg_ras.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

libncarg_ras_la_SOURCES = abekas.c cleartext.c error.c memory.c		\
ntsc.c rasls.c sgiraster.c avsraster.c color.c generic.c misc.c		\
palette.c raster.c sunraster.c composite.c hdf.c netcdf.c rasterop.c	\
xwd.c binary.c dither.c hppcl.c nrif.c resample.c yuv.c

include_HEADERS = ncarg_ras.h


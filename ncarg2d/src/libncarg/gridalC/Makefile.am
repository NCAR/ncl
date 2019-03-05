# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_gridalc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_gridalc_la_SOURCES = c_gacolr.c c_gagetc.c c_gageti.c c_gagetr.c c_gasetc.c c_gaseti.c \
	c_gasetr.c c_grid.c c_gridal.c c_gridl.c c_halfax.c c_labmod.c c_perim.c \
	c_periml.c c_tick4.c c_ticks.c

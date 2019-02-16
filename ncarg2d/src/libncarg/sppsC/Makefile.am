# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_sppsc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_sppsc_la_SOURCES = c_clsgks.c c_curve.c c_fl2int.c c_frame.c	\
c_frstpt.c c_getset.c c_getusv.c c_line.c c_opngks.c c_plotif.c		\
c_point.c c_points.c c_set.c c_setusv.c c_sflush.c c_vector.c		\
c_wtstr.c c_plotit.c c_pwrit.c c_cfux.c c_cfuy.c c_cmfx.c c_cmfy.c	\
c_cufx.c c_cufy.c c_cpux.c c_cpuy.c c_cpfx.c c_cpfy.c c_cmux.c		\
c_cmuy.c c_kfpx.c c_kfpy.c c_kfmx.c c_kfmy.c c_kupx.c c_kupy.c		\
c_kumx.c c_kumy.c c_kpmx.c c_kpmy.c c_kmpx.c c_kmpy.c

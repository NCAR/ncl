# This is an automake file for NCL
# Ed Hartnett 8/30/18

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_autogrphc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_autogrphc_la_SOURCES = c_agback.c c_agbnch.c c_agcurv.c	\
c_agdshn.c c_aggetc.c c_aggetf.c c_aggeti.c c_aggetp.c c_aggetr.c	\
c_agrstr.c c_agsave.c c_agsetc.c c_agsetf.c c_agseti.c c_agsetp.c	\
c_agsetr.c c_agstup.c c_anotat.c c_displa.c c_ezmxy.c c_ezmy.c		\
c_ezxy.c c_ezy.c

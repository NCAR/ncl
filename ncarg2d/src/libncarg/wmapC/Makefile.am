# This is an automake file for NCL
# Ed Hartnett 8/30/18

LDADD = ${top_builddir}/common/src/libncarg_c/libncarg_c.la

# Build this convenience library.
noinst_LTLIBRARIES = libncarg_wmapc.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Sources for convenience library.
libncarg_wmapc_la_SOURCES = c_wmbarb.c c_wmdflt.c c_wmdrft.c		\
c_wmdrrg.c c_wmgetc.c c_wmgeti.c c_wmgetr.c c_wmgtln.c c_wmlabc.c	\
c_wmlabs.c c_wmlabt.c c_wmlabw.c c_wmlgnd.c c_wmrgwt.c c_wmsetc.c	\
c_wmseti.c c_wmsetr.c c_wmstnm.c c_wmw2nx.c c_wmw2ny.c c_wmvect.c	\
c_wmvlbl.c c_wmvmap.c

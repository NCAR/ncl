# This is an automake file for NCL.
# Ed Hartnett 9/6/18

lib_LTLIBRARIES = libNGctrans.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Find cgmdef.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarview/src/lib/libcgm

# Find cgm_tools.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarview/src/lib/libcgm

# Find ncarg_ras.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarview/src/lib/libncarg_ras

libNGctrans_la_SOURCES = bitops.c buffer.c clip.c ctrans.c default.c	\
fill.c fontlist.c format.c gcap.c gcread.c in.c rast.c readfont.c	\
rgbtohls.c text.c translate.c env.c soft_fill.c cell.c commondev.c	\
gcapdev.c gcaprast.c rastdev.c hatch.c pal.c ctrans_api.c cgmmisc.c	\
misc.c devices.c X11_class0.c X11_class1.c X11_class3.c X11_class4.c	\
X11_class6.c X11_class7.c Xcrm.c xresources.c clear_text.c

include_HEADERS = ctrans.h

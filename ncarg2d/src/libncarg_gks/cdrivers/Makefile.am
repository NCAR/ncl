# This is an automake file for NCL.
# Ed Hartnett 8/31/18

# Build this convenience library.
noinst_LTLIBRARIES = libbasegks.la

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

libbasegks_la_SOURCES = fort_c.c gksc.c transform.c gksP.c argb.c	\
gks_device.c ctxt_device.c ctxt.c ctxt_conv.c xdevice.c font.c text.c	\
x_conv.c xattribute.c xcontrol.c xinquiry.c xoutput.c psdevice.c ps.c	\
ps_conv.c psfill.c pdf.c pdfdevice.c pdf_conv.c pdffill.c		\
pixcontrol.c pixoutput.c pixdevice.c pix_conv.c pixattribute.c		\
pixinquiry.c pixwrite_xwd.c pixwrite_png.c cro_device.c cro.c		\
cro_conv.c crotiff.c crodisplay.c

noinst_HEADERS = argb.h croddi.h cro.h ctxt_device.h font.h gksc.h	\
gks.h pdfddi.h pdf.h pix_device.h ps_device.h text.h xattribute.h	\
x_device.h common.h cro_device.h crotiff.h ctxt.h fort_c.h		\
gks_device.h pdf_device.h pixddi.h psddi.h ps.h transform.h xddi.h	\
x.h

# Header get installed in special subdirectory.
ncargdir = $(prefix)/include/ncarg
ncarg_HEADERS = gksP.h

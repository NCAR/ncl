#######################################################################
#
#               Copyright (C)  1990 - All Rights Reserved
#		University Corporation for Atmospheric Research
#               NCAR GRAPHICS  Unix Version 3.01
#
#	This Makefile was created by the "ymake" utility.
#	If you wish to make changes in the "Makefile" or "makefile", 
#	do so by making changes to "yMakefile" or "ymakefile" 
#       and executing "make local-Makefile".
#
#######################################################################

SYSTEM_INCLUDE		= Sun4

BINDIR		= /usr/local/bin
LIBDIR		= /usr/local/lib
MANDIR		= /usr/man/manl
MAN_SECTION	= l
XAPPDIR		= /usr/lib/X11/app-defaults
BITSDIR		= /usr/include/X11/bitmaps

INCLUDEDIR	= $(NCARG)/include
DBDIR		= $(LIBDIR)
FONTCAPDIR	= $(LIBDIR)/fontcaps
GRAPHCAPDIR	= $(LIBDIR)/graphcaps
EXAMPLEDIR	= $(LIBDIR)/ncargexamples
TESTDIR		= $(LIBDIR)/ncargtests

GENERICDIR	= $(NCARG)/generic

CONFIGDIR	= $(NCARG)/config
YMAKE		= $(CONFIGDIR)/ymake -I$(CONFIGDIR)

MAKE		= make
FLOAT_OPT	=
OS		=
ARCH		=
CC		= cc
CC_LD		= $(CC)
CDEBUG		=
COPT		= -g
CMISC           = -Bstatic
CFLAGS          = $(OS) $(ARCH) $(CDEFINES) $(CDEBUG) $(COPT) $(CMISC)\
	$(FLOAT_OPT)
LD_CFLAGS	= $(CFLAGS)
CPP		= /lib/cpp
FCPP            = /lib/cpp
F77		= f77
F77_LD		= $(F77)
FDEBUG		= -g
FOPT		=
FMISC           = -Bstatic
FFLAGS          = $(OS) $(ARCH) $(FDEFINES) $(FDEBUG) $(FOPT) $(FMISC)\
	$(FLOAT_OPT)
LD_FFLAGS	= $(FFLAGS)
LD		= ld
LD_REL		= $(LD)
AR		= ar
AR_QUICK	= $(AR) q
AR_CHECK	= $(AR) vru
AR_SRC          = ar
AR_SRC_QUICK    = $(AR_SRC) q
AR_SRC_CHECK    = $(AR_SRC) vru
RM		= rm -f
CAT		= cat
CP		= cp
MV		= mv -f
RANLIB		= ranlib
NO_OP		= touch -c
INSTALL		= install -c
INSTALL_RM	= $(RM)
INSTALL_BIN	= 0755
INSTALL_MAN	= 0644
INSTALL_APP	= 0644
INSTALL_BIT	= 0644
INSTALL_LIB	= 0644
INSTALL_DB	= 0644

GRAPHC		= $(BINDIR)/graphc
FONTC		= $(BINDIR)/fontc

FSPLIT		= fsplit
CALLCONV	= $(NCARG)/utils/callconv
SHELL		= /bin/sh

#
#	Makefile for libraster - libraries for dealing with
#	a variety of raster image formats.
#

NCARG		= ../../../..
LOCINCDIR	= /usr/local/include
MYNAME		= libncarg_ras.a
INCLUDES	= ncarg_ras.h

HDF_OBJS =	hdf.o
HDF_SRCS =	hdf.c
HDF_DEFS = 	-DBuildRasterHDF

HPLJ_OBJS =	hplj.o
HPLJ_SRCS =	hplj.c
HPLJ_DEFS =	-DBuildRasterHPLaser

NRIF_OBJS =	nrif.o
NRIF_SRCS =	nrif.c
NRIF_DEFS = 	-DBuildRasterNrif

SUN_OBJS =	sunraster.o
SUN_SRCS =	sunraster.c
SUN_DEFS = 	-DBuildRasterSun

XWD_OBJS =	xwd.o
XWD_SRCS =	xwd.c
XWD_DEFS = 	-DBuildRasterXWD

PARALLAX_OBJS =	parallax.o
PARALLAX_SRCS =	parallax.c
PARALLAX_DEFS =	-DBuildRasterParallax

AVS_OBJS =	avsraster.o
AVS_SRCS =	avsraster.c
AVS_DEFS =	-DBuildRasterAVS

CDEFINES =	-I$(LOCINCDIR) \
	$(HDF_DEFS) $(HPLJ_DEFS) $(NRIF_DEFS) $(SUN_DEFS) \
	$(XWD_DEFS) $(PARALLAX_DEFS) $(AVS_DEFS)

DEVICE_OBJS =	$(HDF_OBJS) $(HPLJ_OBJS) $(NRIF_OBJS) $(SUN_OBJS) \
	$(XWD_OBJS) $(PARALLAX_OBJS) $(AVS_OBJS)

DEVICE_SRCS =	$(HDF_SRCS) $(HPLJ_SRCS) $(NRIF_SRCS) $(SUN_SRCS) \
	$(XWD_SRCS) $(PARALLAX_SRCS) $(AVS_SRCS)

GENERIC_OBJS =	raster.o cleartext.o error.o misc.o palette.o dither.o \
	$(DEVICE_OBJS)

GENERIC_SRCS =	raster.c cleartext.c error.c misc.c palette.c dither.c \
	$(DEVICE_SRCS)

OBJS		= $(GENERIC_OBJS) $(OTHER_OBJS)
SRCS		= $(GENERIC_SRCS) $(OTHER_SRCS)

all:: $(OBJS)
	@-$(RM) $(LIBDIR)/$(MYNAME)
	@$(AR_QUICK) $(LIBDIR)/$(MYNAME) $(OBJS)
	$(RANLIB) $(LIBDIR)/$(MYNAME)

install::

Makefiles:: local-Makefile

local-Makefile:
	@$(YMAKE)

clean::
	@-$(RM) *.o core

cleanbin:: clean

dither.o:	ncarg_ras.h
error.o:	ncarg_ras.h
hdf.o:		ncarg_ras.h options.h
misc.o:		ncarg_ras.h
nrif.o:		ncarg_ras.h nrif.h
palette.o:	ncarg_ras.h
rasinfo.o:	ncarg_ras.h
raster.o:	ncarg_ras.h options.h devices.h
sunraster.o:	ncarg_ras.h sunraster.h
xwd.o:		ncarg_ras.h
parallax.o:	ncarg_ras.h parallax.h
hplj.o:		ncarg_ras.h hplj.h
cleartext.o:	ncarg_ras.h
avsraster.o:	ncarg_ras.h avsraster.h

rasinfo: rasinfo.o
	$(CC) $(CFLAGS) -o rasinfo rasinfo.o -lncarg_ras -ldf

install::
	@$(INSTALL) $(INCLUDES) $(LOCINCDIR)

lint:
	lint -I$(LOCINCDIR) $(SRCS) 1> lint.out 2>&1


#######################################################################
#
#	This Makefile was created by the "ymake" utility.
#	If you wish to make changes in the "Makefile" or "makefile", 
#	do so by making changes to "yMakefile" or "ymakefile" 
#       and executing "make local-Makefile".
#
#######################################################################

SYSTEM_INCLUDE		= Sun4

PARFIL_INIT     = /u1/clyne/testlib/params3.1.3
PARFIL_PERM     = /u1/clyne/testlib/params3.1.3

BINDIR		= /u1/clyne/testbin
LIBDIR		= /u1/clyne/testlib
MANDIR		= /u1/clyne/testman
MAN_SECTION     = l
TMPDIR          = /tmp
XAPPDIR		= /u1/clyne/testlib
BITSDIR         = /usr/include/X11/bitmaps

INCLUDEDIR      = $(NCARG)/include
DBDIR           = $(LIBDIR)
FONTCAPDIR      = $(LIBDIR)/fontcaps
GRAPHCAPDIR     = $(LIBDIR)/graphcaps
EXAMPLEDIR      = $(LIBDIR)/ncargexamples
TESTDIR         = $(LIBDIR)/ncargtests

GENERICDIR      = $(NCARG)/generic

<<<<<<< Makefile
CONFIGDIR       = $(NCARG)/config
YMAKE           = $(CONFIGDIR)/ymake -I$(CONFIGDIR)

MAKE            = make
FLOAT_OPT       =
OS              =
ARCH            =
CC              = cc
CC_LD           = $(CC)
CDEBUG          =
COPT            = -g
CMISC           =
=======
MAKE		= make
FLOAT_OPT	=
OS		=
ARCH		=
CC		= cc
CC_LD		= $(CC)
CDEBUG		=
COPT		= -O
CMISC           = -Bstatic
>>>>>>> 1.5
CFLAGS          = $(OS) $(ARCH) $(CDEFINES) $(CDEBUG) $(COPT) $(CMISC)\
<<<<<<< Makefile
	$(FLOAT_OPT)
LD_CFLAGS       = $(CFLAGS)
CPP             = /lib/cpp
=======
		  $(FLOAT_OPT)
LD_CFLAGS	= $(CFLAGS)
CPP		= /lib/cpp
>>>>>>> 1.5
FCPP            = /lib/cpp
F77             = f77
F77_LD          = $(F77)
FDEBUG          =
FOPT            = -O
FMISC           = -Bstatic
FFLAGS          = $(OS) $(ARCH) $(FDEFINES) $(FDEBUG) $(FOPT) $(FMISC)\
<<<<<<< Makefile
	$(FLOAT_OPT)
LD_FFLAGS       = $(FFLAGS)
LD              = ld
LD_REL          = $(LD)
AR              = ar
AR_QUICK        = $(AR) q
AR_CHECK        = $(AR) vru
=======
		  $(FLOAT_OPT)
LD_FFLAGS	= $(FFLAGS)
LD		= ld
LD_REL		= $(LD)
AR		= ar
AR_QUICK	= $(AR) q
AR_CHECK	= $(AR) vru
>>>>>>> 1.5
AR_SRC          = ar
AR_SRC_QUICK    = $(AR_SRC) q
AR_SRC_CHECK    = $(AR_SRC) vru
RM              = rm -f
CAT             = cat
CP              = cp
MV              = mv -f
RANLIB          = ranlib
NO_OP           = touch -c
INSTALL         = install -c
INSTALL_RM      = $(RM)
INSTALL_BIN     = 0755
INSTALL_MAN     = 0644
INSTALL_APP     = 0644
INSTALL_BIT     = 0644
INSTALL_LIB     = 0644
INSTALL_DB      = 0644

GRAPHC          = $(BINDIR)/graphc
FONTC           = $(BINDIR)/fontc

FSPLIT          = fsplit
CALLCONV        = $(NCARG)/utils/callconv
SHELL           = /bin/sh

#	$Id: Makefile,v 1.6 1992-02-12 14:23:49 clyne Exp $
#
#	Makefile for libraster - libraries for dealing with
#	a variety of raster image formats.
#

NCARG		= ../../../..
LOCINCDIR	= ../../include
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

CDEFINES =	-I$(LOCINCDIR) \
		$(HDF_DEFS) $(HPLJ_DEFS) $(NRIF_DEFS) $(SUN_DEFS) \
		$(XWD_DEFS) $(PARALLAX_DEFS) $(AVS_DEFS)

DEVICE_OBJS =	$(HDF_OBJS) $(HPLJ_OBJS) $(NRIF_OBJS) $(SUN_OBJS) \
		$(XWD_OBJS) $(PARALLAX_OBJS) $(AVS_OBJS)

DEVICE_SRCS =	$(HDF_SRCS) $(HPLJ_SRCS) $(NRIF_SRCS) $(SUN_SRCS) \
		$(XWD_SRCS) $(PARALLAX_SRCS) $(AVS_SRCS)

<<<<<<< Makefile
GENERIC_OBJS =	raster.o cleartext.o error.o misc.o palette.o dither.o \
		$(DEVICE_OBJS)
=======
GENERIC_OBJS =	raster.o cleartext.o error.o misc.o palette.o \
		dither.o resample.o composite.o ntsc.o \
		$(DEVICE_OBJS)
>>>>>>> 1.5

<<<<<<< Makefile
GENERIC_SRCS =	raster.c cleartext.c error.c misc.c palette.c dither.c \
		$(DEVICE_SRCS)
=======
GENERIC_SRCS =	raster.c cleartext.c error.c misc.c palette.c \
		dither.c resample.c composite.c ntsc.c \
		$(DEVICE_SRCS)
>>>>>>> 1.5

OBJS		= $(GENERIC_OBJS) $(OTHER_OBJS)
SRCS		= $(GENERIC_SRCS) $(OTHER_SRCS)

EQUALHACK	= =

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
hdf.o:          ncarg_ras.h options.h Makefile
	$(CC) $(CFLAGS) -DTMPDIR$(EQUALHACK)'"$(TMPDIR)"' -c $*.c

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
ntsc.o:		ncarg_ras.h ntsc.h

saber_src: $(SRCS)
	#setopt load_flags $(CFLAGS)
	#load $(SRCS) -ldf

saber_obj: $(OBJS)
	#setopt load_flags $(CFLAGS)
	#load $(OBJS) -ldf

rasinfo: rasinfo.o
	$(CC) $(CFLAGS) -o rasinfo rasinfo.o -lncarg_ras -ldf

install::
	@$(INSTALL) $(INCLUDES) $(LOCINCDIR)

lint:
	lint -I$(LOCINCDIR) $(SRCS) 1> lint.out 2>&1


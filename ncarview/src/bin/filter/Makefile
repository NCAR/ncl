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

SYSTEM_INCLUDE		= Sun3

BINDIR		= /u1/clyne/bin
LIBDIR		= /u1/clyne/lib
MANDIR		= /u1/clyne/man
MAN_SECTION	= l
XAPPDIR		= /usr/lib/X11/app-defaults
BITSDIR		= /usr/include/X11/bitmaps

INCLUDEDIR	= $(NCARG)/include
DBDIR		= $(LIBDIR)
FONTCAPDIR	= $(LIBDIR)/fontcaps
GRAPHCAPDIR	= $(LIBDIR)/graphcaps
EXAMPLEDIR	= $(LIBDIR)/ncargexamples
TESTDIR		= $(LIBDIR)/ncargtests
EXAMPLEDIR	= $(LIBDIR)/ncargexamples

GENERICDIR	= $(NCARG)/generic

CONFIGDIR	= $(NCARG)/config
YMAKE		= $(CONFIGDIR)/ymake -I$(CONFIGDIR)

MAKE		= make
FLOAT_OPT	= -fswitch
OS		=
ARCH		=
CC		= cc
CC_LD		= $(CC)
CDEBUG		=
COPT		= -g
CMISC           =
CFLAGS          = $(OS) $(ARCH) $(CDEFINES) $(CDEBUG) $(COPT) $(CMISC)\
	$(FLOAT_OPT)
LD_CFLAGS	= $(CFLAGS)
CPP		= /lib/cpp
FCPP            = /lib/cpp
F77		= f77
F77_LD		= $(F77)
FDEBUG		=
FOPT		= -O
FMISC           =
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
#
#	Makefile for cgm filters -- filter between NCAR CGM and vanilla CGM
#
#

NCARG = ../../../..

MYNAME	= filter

LIBS		= $(LIBDIR)/libcgm.a $(LIBDIR)/libncarv.a

CDEFINES	= -I../../include

CGM2_SRCS	= cgm2ncgm.c bsd-sysv.c
NCAR2_SRCS	= ncgm2cgm.c bsd-sysv.c

CGM2_OBJ	= cgm2ncgm.o bsd-sysv.o
NCAR2_OBJ	= ncgm2cgm.o bsd-sysv.o

all::	cgm2ncgm ncgm2cgm

cgm2ncgm: $(CGM2_OBJ)
	$(CC_LD) $(LD_CFLAGS) -o cgm2ncgm $(CGM2_OBJ) $(LIBS)

install:: cgm2ncgm
	-$(INSTALL) -m $(INSTALL_BIN) cgm2ncgm $(BINDIR)
	@-$(RM) cgm2ncgm

clean::
	@-$(RM) cgm2ncgm

cleanbin:: clean

ncgm2cgm: $(NCAR2_OBJ)
	$(CC_LD) $(LD_CFLAGS) -o ncgm2cgm $(NCAR2_OBJ) $(LIBS)

install:: ncgm2cgm
	-$(INSTALL) -m $(INSTALL_BIN) ncgm2cgm $(BINDIR)
	@-$(RM) ncgm2cgm

clean::
	@-$(RM) ncgm2cgm

cleanbin:: clean

Makefiles:: local-Makefile

local-Makefile:
	@$(YMAKE)

clean::
	@-$(RM) *.o core

cleanbin:: clean

lint:
	$(LINT) $(CDEFINES) $(NCAR2_SRCS) 1> lint.out 2>&1
	$(LINT) $(CDEFINES) $(CGM2_SRCS) 1>> lint.out 2>&1

cgm2ncgm.o:	filter.h


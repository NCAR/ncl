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
#	Makefile for idt -- Interactive Image Display Tool
#
#

NCARG = ../../../..
BINDIR_LOCAL = ../../../bin

MYNAME	= idt

LOCLIBDIR	= ../../../lib
LOCINCDIR	= ../../include

TRANSLATOR	= ictrans

CDEFINES	= -I$(LOCINCDIR)

XLIBS	= \
	-lXaw -lXmu -lXt -lXext -lX11

LOCLIB	= \
	$(LIBDIR)/libcgm.a $(LIBDIR)/libncarv.a

LIBS	= $(LOCLIB) $(XLIBS)

GENLIBS = -lm

EQUALHACK = =

OBJS	= \
	w_idt.o w_file.o file.o glob.o w_display.o display.o w_dialog.o \
	commands.o talkto.o xrubber.o scroll.o w_text.o spooler.o
SRCS	= \
	w_idt.c w_file.c file.c glob.c w_display.c display.c w_dialog.c \
	commands.c talkto.c xrubber.c scroll.c w_text.c spooler.c

all::	$(MYNAME)

$(MYNAME): $(OBJS)
	$(CC_LD) $(LD_CFLAGS) -o $(MYNAME) $(OBJS) $(LIBS)

install:: $(MYNAME)
	-$(INSTALL) -m $(INSTALL_BIN) $(MYNAME) $(BINDIR_LOCAL)/$(SYSTEM_INCLUDE)
	@-$(RM) $(MYNAME)

clean::
	@-$(RM) $(MYNAME)

cleanbin:: clean

Makefiles:: local-Makefile

local-Makefile:
	@$(YMAKE)

clean::
	@-$(RM) *.o core

cleanbin:: clean

lint:	$(HDR) $(SRCS)
	lint $(CDEFINES) -DTRANSLATOR$(EQUALHACK)'"$(TRANSLATOR)"' \
	-DBINDIR$(EQUALHACK)'"$(BINDIR)"' \
	$(LINTLIBS) $(SRCS)  1> lint.out 2>&1

# Files that need to know installation directories
#talkto.o:     Makefile
#	$(CC) $(CFLAGS) -DTRANSLATOR$(EQUALHACK)'"$(TRANSLATOR)"' \
#		-DBINDIR$(EQUALHACK)'"$(BINDIR)"'  -c $*.c

talkto.o:     Makefile
	$(CC) $(CFLAGS) -DTRANSLATOR$(EQUALHACK)'"$(TRANSLATOR)"' -c $*.c


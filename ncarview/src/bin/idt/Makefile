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
CFLAGS          = $(OS) $(ARCH) $(CDEFINES) $(CDEBUG) $(COPT) $(CMISC)\
	$(FLOAT_OPT)
LD_CFLAGS       = $(CFLAGS)
CPP             = /lib/cpp
FCPP            = /lib/cpp
F77             = f77
F77_LD          = $(F77)
FDEBUG          =
FOPT            = -O
FMISC           = -Bstatic
FFLAGS          = $(OS) $(ARCH) $(FDEFINES) $(FDEBUG) $(FOPT) $(FMISC)\
	$(FLOAT_OPT)
LD_FFLAGS       = $(FFLAGS)
LD              = ld
LD_REL          = $(LD)
AR              = ar
AR_QUICK        = $(AR) q
AR_CHECK        = $(AR) vru
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

#
#
#	$Id: Makefile,v 1.8 1992-02-13 18:36:39 clyne Exp $
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

#
#	default path to x app defaults directory (only used if params file
#	not found)
#
XAPPDIR_DEFAULT	= /usr/lib/X11/app-defaults
#
#	default path to ictrans (only used if params file not found)
#
BINDIR_DEFAULT	= $(BINDIR)

CDEFINES	= -I$(LOCINCDIR) -Bstatic

XLIBS	= \
	-lXaw -lXmu -lXt -lXext -lX11

LOCLIB	= \
	$(LIBDIR)/libcgm.a $(LIBDIR)/libncarv.a

LIBS	= $(LOCLIB) $(XLIBS)

GENLIBS = -lm

EQUALHACK = =

OBJS	= \
	w_idt.o w_file.o file.o glob.o w_display.o display.o w_dialog.o \
	commands.o talkto.o xrubber.o scroll.o w_text.o spooler.o w_icon.o \
	xt_env.o kludge.o

SRCS	= \
	w_idt.c w_file.c file.c glob.c w_display.c display.c w_dialog.c \
	commands.c talkto.c xrubber.c scroll.c w_text.c spooler.c w_icon.c \
	xt_env.c kludge.c

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

#
#	w_idt.c needs to know the name of the translator
#
w_idt.o:     Makefile
	$(CC) $(CFLAGS) -DTRANSLATOR$(EQUALHACK)'"$(TRANSLATOR)"' \
	-DBINDIR_DEFAULT$(EQUALHACK)'"$(BINDIR_DEFAULT)"' -c $*.c

#
#	xt_env.c needs to know the default location of the idt resoure file
#
xt_env.o:     Makefile
	$(CC) $(CFLAGS) -DXAPPDIR_DEFAULT$(EQUALHACK)'"$(XAPPDIR_DEFAULT)"' \
								-c $*.c

lint:	$(HDR) $(SRCS)
	lint  -I$(LOCINCDIR) -DTRANSLATOR$(EQUALHACK)'"$(TRANSLATOR)"' \
		-DBINDIR_DEFAULT$(EQUALHACK)'"$(BINDIR_DEFAULT)"' \
		-DXAPPDIR_DEFAULT$(EQUALHACK)'"$(XAPPDIR_DEFAULT)"' \
		$(LINTLIBS) $(SRCS)  1> lint.out 2>&1


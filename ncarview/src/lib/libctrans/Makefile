#######################################################################
#
#	This Makefile was created by the "ymake" utility.
#	If you wish to make changes in the "Makefile" or "makefile", 
#	do so by making changes to "yMakefile" or "ymakefile" 
#       and executing "make local-Makefile".
#
#######################################################################

SYSTEM_INCLUDE		= HPUX

VERSION         = 3.1.3
PARFIL_INIT     = /red/tmp/clyne/lib/pararms
PARFIL_PERM     = /red/tmp/clyne/lib/pararms

BINDIR          = /red/tmp/clyne/bin
LIBDIR          = /red/tmp/clyne/lib
MANDIR          = /red/tmp/clyne/man
MAN_SECTION     = l
TMPDIR          = /tmp

XLIBDIR         = /usr/lib/X11R4
XINCDIR         = /usr/include/X11R4
XAPPDIR         = /red/tmp/clyne/lib

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
OS              = -DSYSV
ARCH            =
XPATH           = -I$(XINCDIR) -L$(XLIBDIR)
VDEFINE         = -DVERSION='"$(VERSION)"'
CC              = cc
CC_LD           = $(CC)
CDEBUG          =
COPT            = -g
CMISC           =
CFLAGS          = $(OS) $(ARCH) $(CDEFINES) $(CDEBUG) $(COPT) $(CMISC)\
	$(FLOAT_OPT) $(XPATH) $(VDEFINE)
LD_CFLAGS       = $(CFLAGS)
CPP             = /lib/cpp
FCPP            = $(NCARG)/utils/fcpp $(CPP)
FC              = f77
F77             = f77
F77_LD          = $(F77)
FDEBUG          =
FOPT            = -O
FMISC           =
FFLAGS          = $(ARCH) $(FDEFINES) $(FDEBUG) $(FOPT) $(FMISC)\
	$(FLOAT_OPT)
F77FLAGS        = $(FFLAGS)
LD_FFLAGS       = $(FFLAGS)
LD              = ld
LD_REL          = $(LD)
AR              = ar
AR_QUICK        = $(AR) ql
AR_CHECK        = $(AR) vrul
AR_SRC          = ar
AR_SRC_QUICK    = $(AR_SRC) q
AR_SRC_CHECK    = $(AR_SRC) vru
RM              = rm -f
CAT             = cat
CP              = cp
MV              = mv -f
RANLIB          = $(NO_OP)
NO_OP           = touch -c
INSTALL         = $(NCARG)/utils/install -c
INSTALL_RM      = $(RM)
INSTALL_BIN     = 0755
INSTALL_MAN     = 0644
INSTALL_APP     = 0644
INSTALL_LIB     = 0644
INSTALL_DB      = 0644

GRAPHC          = $(BINDIR)/graphc
FONTC           = $(BINDIR)/fontc
CALLCONV        = $(NCARG)/utils/callconv
SHELL           = /bin/sh

GKS_BUFFER_SIZE = 0

NCARG	= ../../../..
#
#	$Id: Makefile,v 1.13 1992-02-20 18:49:49 clyne Exp $
#

LOCINCDIR	= ../../include

MYNAME		= $(LIBDIR)/libctrans.o

INCLUDES	= cterror.h

X11SRCS	= $(DX11SRCS)
X11OBJS	= $(DX11OBJS)
X11DEFINES = -DX11

CTXTSRCS = $(DCTXTSRCS)
CTXTOBJS = $(DCTXTOBJS)
CTXTDEFINES = -DCTXT

EQUALHACK	= =

SRCS	= \
	$(GENSRCS) $(X11SRCS) $(CGISRCS) $(CTXTSRCS) \
	$(SUNVSRCS) $(SUNRSRCS) $(DEVSRCS) $(VMSSRCS)
OBJS	= \
	$(GENOBJS) $(X11OBJS) $(CGIOBJS) $(CTXTOBJS) \
	$(SUNVOBJS) $(SUNROBJS) $(DEVOBJS) $(VMSOBJS)

DDEFINES = $(CGIDEFINES) $(X11DEFINES) $(SUNDEFINES) $(CTXTDEFINES)

# Generic code and libraries for all versions of ctrans.

GENSRCS =  \
	bitops.c buffer.c clip.c ctrans.c default.c \
	fill.c fontlist.c format.c gcap.c gcread.c in.c rast.c \
	readfont.c rgbtohls.c text.c translate.c env.c \
	soft_fill.c cell.c bsd-sysv.c commondev.c gcapdev.c gcaprast.c \
	rastdev.c hatch.c pal.c ctrans_api.c error.c cgmmisc.c

GENOBJS	= \
	bitops.o buffer.o clip.o ctrans.o default.o \
	fill.o fontlist.o format.o gcap.o gcread.o in.o rast.o \
	readfont.o rgbtohls.o text.o translate.o env.o \
	soft_fill.o cell.o bsd-sysv.o commondev.o gcapdev.o gcaprast.o \
	rastdev.o hatch.o pal.o ctrans_api.o error.o cgmmisc.o

DEVSRCS	= cterror.c devices.c
DEVOBJS	= cterror.o devices.o

# Declare code and Libraries for SunCGI.

DCGISRCS = cgi.c cgi_cell_sim.c

DCGIOBJS = cgi.o cgi_cell_sim.o

# Declare code and macros for X11R3

DX11SRCS = \
	X11_class0.c X11_class1.c X11_class2.c X11_class3.c \
	X11_class4.c X11_class5.c X11_class6.c X11_class7.c \
	Xcrm.c

DX11OBJS = \
	X11_class0.o X11_class1.o X11_class2.o X11_class3.o \
	X11_class4.o X11_class5.o X11_class6.o X11_class7.o \
	Xcrm.o

DSUNVSRCS = \
	sun_view.c

DSUNVOBJS = \
	sun_view.o

DSUNRSRCS = \
	sunraster.c

DSUNROBJS = \
	sunraster.o

# Declare code for Clear Text driver.

DCTXTSRCS = clear_text.c

DCTXTOBJS = clear_text.o

CDEFINES	= -I$(LOCINCDIR) -I/usr/local/include  $(DDEFINES)

all::
	@$(CAT) Copyright

all::
	@$(CP) $(INCLUDES) $(LOCINCDIR)

all::	$(MYNAME)

$(MYNAME):	$(OBJS)
	$(LD_REL) -r -o $(MYNAME) $(OBJS)

install::

Makefiles:: local-Makefile

local-Makefile:
	@$(YMAKE)

clean::
	@-$(RM) *.o core

cleanbin:: clean

saber_src: $(SRCS)
	#setopt load_flags $(CFLAGS) $(CINCS)
	#load $(SRCS) $(LIBS)

saber_obj: $(OBJS)
	#setopt load_flags $(CFLAGS) $(CINCS)
	#load $(OBJS) $(LIBS)

lint: $(HDR) $(SRCS)
	lint $(CDEFINES) $(LINTLIBS) $(SRCS)  1> lint.out 2>&1
#	lint $(SRCS) $(LIBS) > lint.out

cterror.o:	Makefile
	$(CC) $(CFLAGS) $(DDEFINES) -DTMPDIR$(EQUALHACK)'"$(TMPDIR)"' -c $*.c


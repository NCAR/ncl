#######################################################################
#
#	This Makefile was created by the "ymake" utility.
#	If you wish to make changes in the "Makefile" or "makefile", 
#	do so by making changes to "yMakefile" or "ymakefile" 
#       and executing "make local-Makefile".
#
#######################################################################

SYSTEM_INCLUDE		= Sun3

VERSION         = 3.1.3
PARFIL_INIT     = /usr/local/lib/ncargparams
PARFIL_PERM     = /usr/local/lib/ncargparams

BINDIR          = /usr/local/bin
LIBDIR          = /usr/local/lib
MANDIR          = /usr/man/manl
MAN_SECTION     = l
TMPDIR          = /tmp

XLIBDIR         = /usr/lib
XINCDIR         = /usr/include
XAPPDIR         = /usr/lib/X11/app-defaults

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
FLOAT_OPT       = -fswitch
OS              =
ARCH            =
XPATH           =
VDEFINE         = -DVERSION='"$(VERSION)"'
CC              = cc
CC_LD           = $(CC)
CDEBUG          =
COPT            = -O
CMISC           =
CFLAGS          = $(OS) $(ARCH) $(CDEFINES) $(CDEBUG) $(COPT) $(CMISC)\
	$(FLOAT_OPT) $(XPATH) $(VDEFINE)
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
INSTALL_LIB     = 0644
INSTALL_DB      = 0644

GRAPHC          = $(BINDIR)/graphc
FONTC           = $(BINDIR)/fontc

FSPLIT          = fsplit
CALLCONV        = $(NCARG)/utils/callconv
SHELL           = /bin/sh

GKS_BUFFER_SIZE = 0

NCARG	= ../../../..

MYNAME	= impltest

COMMON_LIBS =	$(LIBDIR)/libncarg.a \
		$(LIBDIR)/libncarg_gks.a \
		$(LIBDIR)/libncarg_loc.a

LIBS = $(OTHER_LIBS) $(COMMON_LIBS)

GENERIC_SRCS = \
	 box.f lcm.f mskout.f implbd.f tcurve.f tgbyte.f tgks0a.f tiand.f\
	 tior.f tline.f tlocal.f tplot.f tploti.f tpnts.f tpoint.f tsbyte.f\
	 tset.f tshift.f tspps.f tvecto.f twtstr.f

GENERIC_OBJS = \
	 box.o lcm.o mskout.o implbd.o tcurve.o tgbyte.o tgks0a.o tiand.o\
	 tior.o tline.o tlocal.o tplot.o tploti.o tpnts.o tpoint.o tsbyte.o\
	 tset.o tshift.o tspps.o tvecto.o twtstr.o

TSPPS_OBJS = \
	tspps.o tpnts.o tset.o tvecto.o tcurve.o tline.o tpoint.o\
	twtstr.o tploti.o box.o implbd.o

TLOCAL_OBJS = \
	tlocal32.o tiand.o tior.o lcm.o tgbyte.o tshift.o tsbyte.o\
	mskout.o

all:: tspps tlocal tplot tgks0a

tspps: $(TSPPS_OBJS)
	$(F77_LD) $(LD_FFLAGS) -o tspps $(TSPPS_OBJS) $(LIBS)

install:: tspps
	-$(INSTALL) -m $(INSTALL_BIN) tspps $(BINDIR)
	@-$(RM) tspps

clean::
	@$(RM) tspps

cleanbin:: clean

tlocal: $(TLOCAL_OBJS)
	$(F77_LD) $(LD_FFLAGS) -o tlocal $(TLOCAL_OBJS) $(LIBS)

install:: tlocal
	-$(INSTALL) -m $(INSTALL_BIN) tlocal $(BINDIR)
	@-$(RM) tlocal

clean::
	@$(RM) tlocal

cleanbin:: clean

tplot: tplot.o
	$(F77_LD) $(LD_FFLAGS) -o tplot tplot.o $(LIBS)

install:: tplot
	-$(INSTALL) -m $(INSTALL_BIN) tplot $(BINDIR)
	@-$(RM) tplot

clean::
	@$(RM) tplot

cleanbin:: clean

tgks0a: tgks0a.o
	$(F77_LD) $(LD_FFLAGS) -o tgks0a tgks0a.o $(LIBS)

install:: tgks0a
	-$(INSTALL) -m $(INSTALL_BIN) tgks0a $(BINDIR)
	@-$(RM) tgks0a

clean::
	@$(RM) tgks0a

cleanbin:: clean

Makefiles:: local-Makefile

local-Makefile:
	@$(YMAKE)

clean::
	@-$(RM) *.o core

cleanbin:: clean

Update:
	@echo "Updating <$(MYNAME)> from Generic NCAR Graphics"
	@-$(RM) $(GENERIC_SRCS) $(GENERIC_OBJS)
	@echo "	File listing after removing old generics"
	@ls
	@echo "	Splitting Fortran source"
	@$(FSPLIT) $(NCARG)/generic/$(MYNAME)/src \
	| makemacro >FileList; $(RM) generic.f
	@if (test -x Generic2Unix) then \
	echo "	Applying local modifications"; Generic2Unix; fi


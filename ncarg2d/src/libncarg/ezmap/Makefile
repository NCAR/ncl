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

MYNAME	= ezmap

GENERIC_SRCS = \
	mapdrw.f mapeod.f mapfst.f mapgrd.f mapgtc.f mapgti.f \
	mapgtl.f mapgtr.f mapint.f mapit.f mapiq.f maplbl.f \
	maplot.f mappos.f maproj.f maprs.f maprst.f mapsav.f \
	mapset.f mapstc.f mapsti.f mapstl.f mapstr.f maptra.f \
	maptri.f maptrn.f \
	mapusr.f mapvec.f supcon.f supmap.f mapcem.f mapchi.f \
	idictl.f mapgci.f mapio.f maplmb.f maptre.f maptrp.f mapvp.f \
	mapbd.f

GENERIC_OBJS = \
	mapdrw.o mapeod.o mapfst.o mapgrd.o mapgtc.o mapgti.o \
	mapgtl.o mapgtr.o mapint.o mapit.o mapiq.o maplbl.o \
	maplot.o mappos.o maproj.o maprs.o maprst.o mapsav.o \
	mapset.o mapstc.o mapsti.o mapstl.o mapstr.o maptra.o \
	maptri.o maptrn.o \
	mapusr.o mapvec.o supcon.o supmap.o mapcem.o mapchi.o \
	idictl.o mapgci.o mapio.o maplmb.o maptre.o maptrp.o mapvp.o \
	mapbd.o

UNIX_SRCS = openmp.f

UNIX_OBJS = openmp.o

SOURCES = $(GENERIC_SRCS) $(UNIX_SRCS)

OBJECTS	= $(GENERIC_OBJS) $(UNIX_OBJS)

FCPP_OBJS	= openmp.f

all:: $(OBJECTS)
	@$(AR_QUICK) $(LIBDIR)/libncarg.a $(OBJECTS)

install:: $(OBJECTS)
	@$(AR_CHECK) $(LIBDIR)/libncarg.a $(OBJECTS)
	$(RANLIB) $(LIBDIR)/libncarg.a

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

clean::
	@-$(RM) openmp.f

clean::
	@-$(RM) $(FCPP_OBJS)


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

MYNAME  = pre2ncgm

GENERIC_SRCS = alover.f clipit.f frm.f garg.f intcnv.f lodctb.f \
	mmdef.f multi.f patdrw.f patset.f pltclp.f pltit.f pnt.f \
	posit.f pwt.f setop.f setrng.f trndt2.f trnmch.f twobyt.f \
	xmdef.f xmopdf.f

OTHER_SRCS = fndarg.f metred.f mninst.f trans.f trassn.f trlate.f trndat.f

GENERIC_OBJS = alover.o clipit.o frm.o garg.o intcnv.o lodctb.o \
	mmdef.o multi.o patdrw.o patset.o pltclp.o pltit.o pnt.o \
	posit.o pwt.o setop.o setrng.o trndt2.o trnmch.o twobyt.o \
	xmdef.o xmopdf.o

OTHER_OBJS = fndarg.o metred.o mninst.o trans.o trassn.o trlate.o trndat.o

SOURCES = $(GENERIC_SRCS) $(OTHER_SRCS)

OBJECTS = $(GENERIC_OBJS) $(OTHER_OBJS)

FCPP_OBJS	= fndarg.f metred.f mninst.f trans.f trassn.f \
		trlate.f trndat.f

LIBS = $(LIBDIR)/libncarg.a $(LIBDIR)/libncarg_gks.a $(LIBDIR)/libncarg_loc.a

all:: pre2ncgm.prog

pre2ncgm.prog: $(OBJECTS)
	$(F77_LD) $(LD_FFLAGS) -o pre2ncgm.prog $(OBJECTS) $(LIBS)

install:: pre2ncgm.prog
	-$(INSTALL) -m $(INSTALL_BIN) pre2ncgm.prog $(BINDIR)
	@-$(RM) pre2ncgm.prog

clean::
	@$(RM) pre2ncgm.prog

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

clean::
	@-$(RM) $(FCPP_OBJS)


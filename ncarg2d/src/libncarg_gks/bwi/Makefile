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

MYNAME	= bwi

FOPT	=

GENERIC_SRCS = \
	 g01bkd.f g01ca.f g01clp.f g01clw.f g01ctb.f g01d2r.f g01d2s.f\
	 g01diq.f g01dmp.f g01esc.f g01fa.f g01flb.f g01iac.f g01iws.f\
	 g01mio.f g01pat.f g01pbl.f g01pl.f g01pm.f g01sas.f g01seg.f\
	 g01siq.f g01snp.f g01tx.f g01wdr.f g01wkt.f gflcnv.f giargs.f\
	 ginlod.f gkasci.f gmflod.f gmpart.f gopnic.f gputmd.f gputni.f\
	 gputpr.f gputps.f gtnlen.f gupasf.f gupdva.f gupdvi.f gupdvr.f

GENERIC_OBJS = \
	 g01bkd.o g01ca.o g01clp.o g01clw.o g01ctb.o g01d2r.o g01d2s.o\
	 g01diq.o g01dmp.o g01esc.o g01fa.o g01flb.o g01iac.o g01iws.o\
	 g01mio.o g01pat.o g01pbl.o g01pl.o g01pm.o g01sas.o g01seg.o\
	 g01siq.o g01snp.o g01tx.o g01wdr.o g01wkt.o gflcnv.o giargs.o\
	 ginlod.o gkasci.o gmflod.o gmpart.o gopnic.o gputmd.o gputni.o\
	 gputpr.o gputps.o gtnlen.o gupasf.o gupdva.o gupdvi.o gupdvr.o

OTHER_SRCS = wks.c wks.h

OTHER_OBJS = wks.o

SOURCES = $(GENERIC_SRCS) $(OTHER_SRCS)

OBJECTS = $(GENERIC_OBJS) $(OTHER_OBJS)

all::  $(OBJECTS)
	@$(AR_QUICK) $(LIBDIR)/libncarg_gks.a  $(OBJECTS)

install::  $(OBJECTS)
	@$(AR_CHECK) $(LIBDIR)/libncarg_gks.a  $(OBJECTS)
	$(RANLIB) $(LIBDIR)/libncarg_gks.a

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

# A file containing C functions called from Fortran

wks.o:	wks.h

wks.c: wks.c.sed Makefile
	@$(CALLCONV) $(SYSTEM_INCLUDE) wks.c \
		opnwks clswks wrtwks rdwks begwks lstwks

wks.h: wks.h.sed Makefile
	@sed \
	-e 's+SED_GKS_BUFFER_SIZE+$(GKS_BUFFER_SIZE)+' \
	< wks.h.sed > wks.h

clean::
	@-$(RM) wks.c wks.h


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

FSPLIT	= ncarguncat

MYNAME	= eautog

GENERIC_SRCS = \
	 agex01.f agex02.f agex03.f agex04.f agex05.f agex06.f agex07.f\
	 agex08.f agex09.f agex10.f agex11.f agex12.f agex13.f agda13.dat

GENERIC_OBJS = \
	 agex01.o agex02.o agex03.o agex04.o agex05.o agex06.o agex07.o\
	 agex08.o agex09.o agex10.o agex11.o agex12.o agex13.o agda13.dat

install::
	@for file in $(GENERIC_SRCS); do\
	($(INSTALL) -m $(INSTALL_LIB) $$file $(EXAMPLEDIR));\
	done

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


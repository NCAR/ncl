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

MYNAME	= autogrph

GENERIC_SRCS = \
	 agaxis.f agback.f agbnch.f agchax.f agchcu.f agchil.f agchnl.f\
	 agctcs.f agctko.f agcurv.f agdash.f agdflt.f agdlch.f agdshn.f\
	 agexax.f agexus.f agezsu.f agfpbn.f agftol.f aggetc.f aggetf.f\
	 aggeti.f aggetp.f aggetr.f aggtch.f aginit.f agkurv.f aglbls.f\
	 agmaxi.f agmini.f agnumb.f agppid.f agpwrt.f agqurv.f agrpch.f\
	 agrstr.f agsave.f agscan.f agsetc.f agsetf.f agseti.f agsetp.f\
	 agsetr.f agsrch.f agstch.f agstup.f agutol.f anotat.f displa.f\
	 ezmxy.f ezmy.f ezxy.f ezy.f idiot.f

GENERIC_OBJS = \
	 agaxis.o agback.o agbnch.o agchax.o agchcu.o agchil.o agchnl.o\
	 agctcs.o agctko.o agcurv.o agdash.o agdflt.o agdlch.o agdshn.o\
	 agexax.o agexus.o agezsu.o agfpbn.o agftol.o aggetc.o aggetf.o\
	 aggeti.o aggetp.o aggetr.o aggtch.o aginit.o agkurv.o aglbls.o\
	 agmaxi.o agmini.o agnumb.o agppid.o agpwrt.o agqurv.o agrpch.o\
	 agrstr.o agsave.o agscan.o agsetc.o agsetf.o agseti.o agsetp.o\
	 agsetr.o agsrch.o agstch.o agstup.o agutol.o anotat.o displa.o\
	 ezmxy.o ezmy.o ezxy.o ezy.o idiot.o

SOURCES = $(GENERIC_SRCS)

OBJECTS = $(GENERIC_OBJS)

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


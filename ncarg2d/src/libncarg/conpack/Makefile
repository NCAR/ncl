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

MYNAME	= conpack

GENERIC_SRCS = \
	 cpblda.f cpback.f cpchcf.f cpchcl.f cpchhl.f cpchil.f cpchll.f\
	 cpclam.f cpcldm.f cpcldr.f cpcnrc.f cpdrpl.f cpezct.f cpgetc.f\
	 cpgeti.f cpgetr.f cplbam.f cplbdr.f cpmpxy.f cppkcl.f cppklb.f\
	 cppklp.f cprect.f cprset.f cpsetc.f cpseti.f cpsetr.f cpsprs.f\
	 cpsps1.f cpsp1a.f cpsps2.f cpsp2a.f cpcflb.f cpcpag.f cpdrsg.f\
	 cpgiws.f cpgrws.f cphllb.f cpinit.f cpinlb.f cpinrc.f cpnumb.f\
	 cpplar.f cpplps.f cpranf.f cpsbst.f cpsort.f cpstls.f cptrcl.f\
	 cptreg.f cptres.f cptrev.f cpcltr.f cpcica.f cphchm.f cphchr.f\
	 cpscae.f cptroe.f cptrve.f cpwlam.f

GENERIC_OBJS	= \
	 cpblda.o cpback.o cpchcf.o cpchcl.o cpchhl.o cpchil.o cpchll.o\
	 cpclam.o cpcldm.o cpcldr.o cpcnrc.o cpdrpl.o cpezct.o cpgetc.o\
	 cpgeti.o cpgetr.o cplbam.o cplbdr.o cpmpxy.o cppkcl.o cppklb.o\
	 cppklp.o cprect.o cprset.o cpsetc.o cpseti.o cpsetr.o cpsprs.o\
	 cpsps1.o cpsp1a.o cpsps2.o cpsp2a.o cpcflb.o cpcpag.o cpdrsg.o\
	 cpgiws.o cpgrws.o cphllb.o cpinit.o cpinlb.o cpinrc.o cpnumb.o\
	 cpplar.o cpplps.o cpranf.o cpsbst.o cpsort.o cpstls.o cptrcl.o\
	 cptreg.o cptres.o cptrev.o cpcltr.o cpcica.o cphchm.o cphchr.o\
	 cpscae.o cptroe.o cptrve.o cpwlam.o

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

cpinit.o: cpinit.f
	$(F77) $(OS) $(FDEFINES) $(FDEBUG) $(FLOAT_OPT) -c cpinit.f


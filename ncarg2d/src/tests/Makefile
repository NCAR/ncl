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

NCARG	= ../../..

MYNAME	= tests

GENERIC_SRCS = \
	tagupw.f tareas.f tautog.f tcnqck.f tcnsmt.f tcnsup.f tcolcv.f \
	tconan.f tconaq.f tconas.f tconpa.f tconre.f tdashc.f tdashl.f \
	tdashp.f tdashs.f tezmap.f tezmpa.f tgflas.f tgrida.f thafto.f \
	thstgr.f tisohr.f tisosr.f tlblba.f tpltch.f tpwrtx.f tpwry.f \
	tpwrzi.f tpwrzs.f tpwrzt.f tsoftf.f tsrfac.f tstitl.f tstrml.f \
	tthree.f tvelvc.f

TESTS = \
	tagupw tareas tautog tcnqck tcnsmt tcnsup tcolcv \
	tconan tconaq tconas tconpa tconre tdashc tdashl \
	tdashp tdashs tezmap tezmpa tgflas tgrida thafto \
	thstgr tisohr tisosr tlblba tpltch tpwrtx tpwry \
	tpwrzi tpwrzs tpwrzt tsoftf tsrfac tstitl tstrml \
	tthree tvelvc

all::

install:: $(GENERIC_SRCS)
	@for name in $(GENERIC_SRCS); do\
	($(INSTALL) -m $(INSTALL_LIB) $$name $(TESTDIR) );\
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
	@echo "	Copying Generic source"
	@for name in $(TESTS); do\
	($(CP) $(NCARG)/generic/$$name/src $$name.f);\
	(chmod 644 $$name.f);\
	done
	@if (test -x Generic2Unix) then \
	echo "	Applying local modifications"; Generic2Unix; fi


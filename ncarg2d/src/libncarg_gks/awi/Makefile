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

MYNAME	= awi

# The Sun f77 optimizer appears to have troubles with the GKS system.

FOPT =

GENERIC_SRCS = \
	 gacwk.f gca.f gclks.f gclrwk.f gclsg.f gclwk.f gcrsg.f gcsgwk.f\
	 gdawk.f gdsg.f geclks.f gerhnd.f gerlog.f gesc.f gfa.f ggdp.f\
	 ggtitm.f giitm.f gopks.f gopwk.f gpl.f gpm.f gprec.f gqacwk.f\
	 gqasf.f gqcf.f gqchb.f gqchh.f gqchsp.f gqchup.f gqchw.f gqchxp.f\
	 gqclip.f gqcntn.f gqcr.f gqdsp.f gqeci.f gqegdp.f gqentn.f gqewk.f\
	 gqfaci.f gqfaf.f gqfai.f gqfais.f gqfasi.f gqgdp.f gqln.f gqlvks.f\
	 gqlwk.f gqlwsc.f gqmk.f gqmksc.f gqmntn.f gqnt.f gqops.f gqopsg.f\
	 gqopwk.f gqpa.f gqpaf.f gqparf.f gqpcr.f gqpfar.f gqplci.f gqplf.f\
	 gqpli.f gqpmci.f gqpmf.f gqpmi.f gqppar.f gqpplr.f gqppmr.f\
	 gqptxr.f gqpx.f gqpxa.f gqpxad.f gqsgus.f gqtxal.f gqtxci.f\
	 gqtxf.f gqtxfp.f gqtxi.f gqtxp.f gqtxx.f gqwkc.f gqwkca.f gqwkcl.f\
	 gqwkdu.f gqwks.f gqwkt.f grditm.f gsasf.f gschh.f gschsp.f gschup.f\
	 gschxp.f gsclip.f gscr.f gselnt.f gsfaci.f gsfai.f gsfais.f\
	 gsfasi.f gsln.f gslwsc.f gsmk.f gsmksc.f gspa.f gsparf.f gsplci.f\
	 gspli.f gspmci.f gspmi.f gstxal.f gstxci.f gstxfp.f gstxi.f\
	 gstxp.f gsvp.f gswkvp.f gswkwn.f gswn.f gtx.f gurec.f guwk.f\
	 gwitm.f gzckst.f gzckwk.f gzfmwk.f gzines.f gzinsl.f gzn2wx.f\
	 gzn2wy.f gzputr.f gzputs.f gztowk.f gzw2nx.f gzw2ny.f gksbd.f

GENERIC_OBJS = \
	 gacwk.o gca.o gclks.o gclrwk.o gclsg.o gclwk.o gcrsg.o gcsgwk.o\
	 gdawk.o gdsg.o geclks.o gerhnd.o gerlog.o gesc.o gfa.o ggdp.o\
	 ggtitm.o giitm.o gopks.o gopwk.o gpl.o gpm.o gprec.o gqacwk.o\
	 gqasf.o gqcf.o gqchb.o gqchh.o gqchsp.o gqchup.o gqchw.o gqchxp.o\
	 gqclip.o gqcntn.o gqcr.o gqdsp.o gqeci.o gqegdp.o gqentn.o gqewk.o\
	 gqfaci.o gqfaf.o gqfai.o gqfais.o gqfasi.o gqgdp.o gqln.o gqlvks.o\
	 gqlwk.o gqlwsc.o gqmk.o gqmksc.o gqmntn.o gqnt.o gqops.o gqopsg.o\
	 gqopwk.o gqpa.o gqpaf.o gqparf.o gqpcr.o gqpfar.o gqplci.o gqplf.o\
	 gqpli.o gqpmci.o gqpmf.o gqpmi.o gqppar.o gqpplr.o gqppmr.o\
	 gqptxr.o gqpx.o gqpxa.o gqpxad.o gqsgus.o gqtxal.o gqtxci.o\
	 gqtxf.o gqtxfp.o gqtxi.o gqtxp.o gqtxx.o gqwkc.o gqwkca.o gqwkcl.o\
	 gqwkdu.o gqwks.o gqwkt.o grditm.o gsasf.o gschh.o gschsp.o gschup.o\
	 gschxp.o gsclip.o gscr.o gselnt.o gsfaci.o gsfai.o gsfais.o\
	 gsfasi.o gsln.o gslwsc.o gsmk.o gsmksc.o gspa.o gsparf.o gsplci.o\
	 gspli.o gspmci.o gspmi.o gstxal.o gstxci.o gstxfp.o gstxi.o\
	 gstxp.o gsvp.o gswkvp.o gswkwn.o gswn.o gtx.o gurec.o guwk.o\
	 gwitm.o gzckst.o gzckwk.o gzfmwk.o gzines.o gzinsl.o gzn2wx.o\
	 gzn2wy.o gzputr.o gzputs.o gztowk.o gzw2nx.o gzw2ny.o gksbd.o

SOURCES = $(GENERIC_SRCS)

OBJECTS = $(GENERIC_OBJS)

FCPP_OBJS	= gopwk.f

all:: $(OBJECTS)
	@$(AR_QUICK) $(LIBDIR)/libncarg_gks.a $(OBJECTS)

install:: $(OBJECTS)
	@$(AR_CHECK) $(LIBDIR)/libncarg_gks.a $(OBJECTS)
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

clean::
	@-$(RM) gopwk.f

clean::
	@-$(RM) $(FCPP_OBJS)


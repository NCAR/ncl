#######################################################################
#
#	This Makefile was created by the "ymake" utility.
#	If you wish to make changes in the "Makefile" or "makefile", 
#	do so by making changes to "yMakefile" or "ymakefile" 
#       and executing "make local-Makefile".
#
#######################################################################

SYSTEM_INCLUDE		= Sun4

VERSION		= 3.2.alpha
BINROOT		= /u2/ncarg_test/sun4/bin
LIBROOT		= /u2/ncarg_test/sun4/lib
INCROOT		= /u2/ncarg_test/sun4/include
MANROOT		= /u2/ncarg_test/sun4/man
TMPROOT		= /tmp
LOCAL_LIB_SEARCH		= -L/X/X11/lib -L/usr/local/lib
LOCAL_INC_SEARCH		= -I/X/X11/include -I/usr/local/include

LIB_SEARCH        = -L$(LIBROOT) $(LOCAL_LIB_SEARCH)
INC_SEARCH        = -I$(INCROOT) $(LOCAL_INC_SEARCH)

NCARGDIR          = ncarg
BINDIR            = bin
LIBDIR            = lib
INCDIR            = include
MANDIR            = man
TMPDIR            = tmp
DBDIR             = database
FONTCAPDIR        = fontcaps
GRAPHCAPDIR       = graphcaps
EXAMPLESDIR       = examples
TUTORIALDIR       = tutorial
TESTSDIR          = tests
XAPPDIR           = xapp
CONFIGDIR         = config
GENERICDIR        = generic

BINPATH           = $(BINROOT)
LIBPATH           = $(LIBROOT)
#
MYLIBPATH         = $(HOME)/lib
LIBPATH		  = $(MYLIBPATH)
#
INCPATH           = $(INCROOT)/$(NCARGDIR)
TMPPATH           = $(TMPROOT)
MANPATH           = $(MANROOT)

MANLPATH          = $(MANPATH)/manl
MAN1PATH          = $(MANPATH)/man1
MAN3PATH          = $(MANPATH)/man3
MAN4PATH          = $(MANPATH)/man4
MAN5PATH          = $(MANPATH)/man5

LIBNCARGPATH      = $(LIBPATH)/$(NCARGDIR)
DBPATH            = $(LIBNCARGPATH)/$(DBDIR)
FONTCAPPATH       = $(LIBNCARGPATH)/$(FONTCAPDIR)
GRAPHCAPPATH      = $(LIBNCARGPATH)/$(GRAPHCAPDIR)
EXAMPLESPATH      = $(LIBNCARGPATH)/$(EXAMPLESDIR)
TESTSPATH         = $(LIBNCARGPATH)/$(TESTSDIR)
TUTORIALPATH      = $(LIBNCARGPATH)/$(TUTORIALDIR)
XAPPPATH          = $(LIBNCARGPATH)/$(XAPPDIR)

MANL_SECTION      = l
MAN1_SECTION      = 1
MAN3_SECTION      = 3
MAN4_SECTION      = 4
MAN5_SECTION      = 5

XLIB              = -lX11
EXTENSIONLIB      = -lXext
XTOOLLIB          = -lXt
XMULIB            = -lXmu
XAWLIB            = -lXaw
HDFLIB            = -ldf
XDRLIB            =
NETCDFLIB         = -lnetcdf $(XDRLIB)

GENERICPATH       = $(NCARG)/$(GENERICDIR)

CONFIGPATH        = $(NCARG)/$(CONFIGDIR)
YMAKE             = $(CONFIGPATH)/ymake -I$(CONFIGPATH)

MAKE              = make
FLOAT_OPT         =
OS                =
ARCH              = -DSUN
VDEFINE           = -DVERSION='"$(VERSION)"'
CC                = acc
CC_LD             = $(CC)
CDEBUG            =
COPT              =
CMISC             = -Xa -DNeedFuncProto
CINC              = $(INC_SEARCH)
CFLAGS            = $(OS) $(ARCH) $(CDEFINES) $(CDEBUG) $(COPT) $(CMISC)\
	$(FLOAT_OPT) $(CINC) $(VDEFINE)
LD_CFLAGS         = $(LIB_SEARCH) $(CFLAGS)
CPP               = /lib/cpp
FCPP              = /lib/cpp
F77               = f77
F77_LD            = $(F77)
FDEBUG            =
FOPT              = -g
FMISC             =
FFLAGS            = $(OS) $(ARCH) $(FDEFINES) $(FDEBUG) $(FOPT) $(FMISC)\
	$(FLOAT_OPT)
LD_FFLAGS         = $(LIB_SEARCH) $(FFLAGS)
LD                = ld
LD_REL            = $(LD)
AR                = ar
AR_QUICK          = $(AR) q
AR_CHECK          = $(AR) vru
AR_SRC            = ar
AR_SRC_QUICK      = $(AR_SRC) q
AR_SRC_CHECK      = $(AR_SRC) vru
RM                = rm -f
CAT               = cat
CP                = cp
MV                = mv -f
RANLIB            = ranlib
NO_OP             = touch -c
LINT              = alint
INSTALL           = install -c
INSTALL_RM        = $(RM)
INSTALL_BIN       = 0755
INSTALL_MAN       = 0644
INSTALL_APP       = 0644
INSTALL_LIB       = 0644
INSTALL_DB        = 0644
INSTALL_INC       = 0644

GRAPHC            = $(BINPATH)/graphc
FONTC             = $(BINPATH)/fontc

FSPLIT            = fsplit
CALLCONV          = $(NCARG)/utils/callconv
SHELL             = /bin/sh

GKS_BUFFER_SIZE   = 0

#
#	$Id: Makefile,v 1.2 1993-01-15 23:33:15 dbrown Exp $
#
NCARG	= ../../../..

MYNAME	= velvct

GENERIC_SRCS = \
	 vvectr.f velvct.f ezvec.f vvdraw.f velvec.f vvdata.f \
	 vvsetc.f vvseti.f vvsetr.f vvgetc.f vvgeti.f vvgetr.f \
	 vvmpxy.f vvumxy.f vvrset.f vvinit.f vvtext.f vvudmv.f \
	 mxf.f myf.f

GENERIC_OBJS = \
	 vvectr.o velvct.o ezvec.o vvdraw.o velvec.o vvdata.o \
	 vvsetc.o vvseti.o vvsetr.o vvgetc.o vvgeti.o vvgetr.o \
	 vvmpxy.o vvumxy.o vvrset.o vvinit.o vvtext.o vvudmv.o \
	 mxf.o myf.o

SOURCES = $(GENERIC_SRCS)

OBJECTS = $(GENERIC_OBJS)

all:: $(OBJECTS)
	@$(AR_QUICK) $(LIBPATH)/libncarg.a $(OBJECTS)

install:: $(OBJECTS)
	@$(AR_CHECK) $(LIBPATH)/libncarg.a $(OBJECTS)
	$(RANLIB) $(LIBPATH)/libncarg.a

Makefiles:: local-Makefile

local-Makefile:
	@$(YMAKE)

me:
	@$(YMAKE)

clean::
	@-$(RM) *.o core

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


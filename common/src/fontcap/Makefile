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

MYNAME	= fontcap

#
# the "all" target is a dummy because it depends upon
# the "fontc" program, which is not installed until the
# "install" cycle.
#
GENERIC_SRCS = \
	font1.fc font2.fc font3.fc font4.fc font5.fc \
	font6.fc font7.fc font8.fc .fc font9.fc font10.fc \
	font11.fc font12.fc font13.fc font14.fc font15.fc \
	font16.fc font17.fc font18.fc font19.fc font20.fc

FONTCAPS = \
	font1 font2 font3 font4 font5 \
	font6 font7 font8  font9 font10 \
	font11 font12 font13 font14 font15 \
	font16 font17 font18 font19 font20

all::

install:: $(FONTCAPS)

install::
	@$(MV) $(FONTCAPS) $(FONTCAPDIR)

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
	@for name in $(FONTCAPS); do\
	($(CP) $(NCARG)/generic/$$name/src $$name.fc);\
	done
	@if (test -x Generic2Unix) then \
	echo "	Applying local modifications"; Generic2Unix; fi

clean::
	@-$(RM) $(FONTCAPS)

font1: font1.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font2: font2.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font3: font3.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font4: font4.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font5: font5.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font6: font6.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font7: font7.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font8: font8.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font9: font9.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font10: font10.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font11: font11.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font12: font12.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font13: font13.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font14: font14.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font15: font15.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font16: font16.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font17: font17.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font18: font18.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font19: font19.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@

font20: font20.fc
	@echo "	Processing fontcap $@"
	@$(FONTC) $? $@


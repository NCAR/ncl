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

FSPLIT	= ncarguncat

MYNAME	= graphcap

#
# NOTE:		"t4006.gc" is not installed because it requires
#		an internal code change to graphc.
#
#		"form.gc" is just an example and not installed.
#
#		The "all" target is a dummy because it depends upon
#		the "graphc" program, which is not installed until the
#		"install" cycle.
#

GENERIC_SRCS = \
	 adm5.gc aed.a.gc aed.b.gc balsml.gc form.gc hp150.gc hp2648a.gc \
	 hp7475a.gc hp7510a.gc hplj100l.gc hplj100p.gc hplj150l.gc \
	 hplj150p.gc hplj300l.gc hplj300p.gc hplj75l.gc hplj75p.gc \
	 hpljo75l.gc hpljo75p.gc imagen.gc imagen.port.gc pc.mono.gc \
	 ps.color.gc ps.land.color.gc ps.land.mono.gc ps.mono.gc qms800.gc \
	 r6211.gc s100.gc t4006.gc t4010.gc t4025.gc t4105.gc t4107.gc \
	 t4107.seg.gc t4115.gc t4115.seg.gc tal1590.gc tekalike.gc \
	 versaterm.gc vt100.gc vt125.gc vt220.gc vt240.gc vt330.gc vt340.gc \
	 vt340w.gc

GRAPHCAPS = \
	 adm5 aed.a aed.b balsml hp150 hp2648a \
	 hp7475a hp7510a hplj100l hplj100p hplj150l \
	 hplj150p hplj300l hplj300p hplj75l hplj75p \
	 hpljo75l hpljo75p imagen imagen.port pc.mono \
	 ps.color ps.land.color ps.land.mono ps.mono qms800 \
	 r6211 s100 t4010 t4025 t4105 t4107 \
	 t4107.seg t4115 t4115.seg tal1590 tekalike \
	 versaterm vt100 vt125 vt220 vt240 vt330 vt340 \
	 vt340w

all::

install:: $(GRAPHCAPS)
	@$(MV) $(GRAPHCAPS) $(GRAPHCAPDIR)

adm5:  adm5.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

aed.a:  aed.a.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

aed.b:  aed.b.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

balsml:  balsml.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

form:  form.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

hp2648a:  hp2648a.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

hp150:  hp150.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

hp7475a:  hp7475a.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

hp7510a:  hp7510a.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

hplj100l:  hplj100l.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

hplj100p:  hplj100p.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

hplj150l:  hplj150l.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

hplj150p:  hplj150p.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

hplj300l:  hplj300l.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

hplj300p:  hplj300p.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

hplj75l:  hplj75l.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

hplj75p:  hplj75p.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

hpljo75l:  hpljo75l.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

hpljo75p:  hpljo75p.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

imagen:  imagen.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

imagen.port:  imagen.port.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

pc.mono:  pc.mono.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

ps.color:  ps.color.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

ps.land.color:  ps.land.color.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

ps.land.mono:  ps.land.mono.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

ps.mono:  ps.mono.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

qms800:  qms800.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

r6211:  r6211.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

s100:  s100.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

t4006:  t4006.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

t4010:  t4010.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

t4025:  t4025.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

t4105:  t4105.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

t4107:  t4107.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

t4107.seg:  t4107.seg.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

t4115:  t4115.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

t4115.seg:  t4115.seg.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

tal1590:  tal1590.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

tekalike:  tekalike.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

versaterm:  versaterm.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

vt100:  vt100.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

vt125:  vt125.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

vt220:  vt220.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

vt240:  vt240.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

vt330:  vt330.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

vt340:  vt340.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

vt340w:  vt340w.gc
	@echo "	Processing graphcap $@"
	@$(GRAPHC) $? $@

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
	@$(RM) $(GRAPHCAPS)


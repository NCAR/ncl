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

NCARG	= .

SUBDIRS	= common ncarg2d ncarview

NEWDIRS	= $(BINDIR) $(LIBDIR) $(MANDIR) $(FONTCAPDIR) $(GRAPHCAPDIR) \
	$(DBDIR) $(EXAMPLEDIR) $(TESTDIR)

all::	dirs

all::
	@for dir in $(SUBDIRS) ; do\
	(cd $$dir; echo "Making $$dir";\
	$(MAKE) $(MFLAGS));\
	done

Info:
	@echo " "
	@$(CAT) Version
	@echo " "
	@echo "NCAR Graphics - Current Installation Configuration"; \
	echo "	System                  " $(SYSTEM_INCLUDE);

	@echo "	NCAR View will include unsupported X applications

	@echo "	Binary Directory        " $(BINDIR); \
	echo "	Library Directory       " $(LIBDIR); \
	echo "	Data Base Directory     " $(DBDIR); \
	echo "	Manpage Directory       " $(MANDIR); \
	echo "	Examples Directory      " $(EXAMPLEDIR); \
	echo "	Test Directory          " $(TESTDIR); \
	echo "	f77 Flags               " $(FFLAGS); \
	echo "	cc Flags                " $(CFLAGS);

Config:
	@cd $(CONFIGDIR); $(MAKE) clean all

# The "All" target rebuilds the Makefile hierarchy, rebuilds
# all libraries, rebuilds all out-of-date executables, and
# finally installs everything.  It *does not* clean out the
# object code.  Its intended use is for rebuilding the system
# once you have made changes to your configuration file.

All:
	@echo ""
	@echo "Making <All>."
	@$(YMAKE)
	@$(MAKE) Info
	@$(MAKE) Makefiles
	@$(MAKE) all install
	@echo "Finished making <All>."

# The Everything target is a *complete* installation.  Makefiles
# are completely rebuilt for the current installation, object
# code is removed (clean target), and all binaries and libraries
# are rebuilt from the ground up.  Finally, things are installed.

Everything: Config
	@echo ""
	@echo "Making <Everything>."
	@$(YMAKE)
	@$(MAKE) Info
	@$(MAKE) Makefiles
	@$(MAKE) clean all install
	@echo "Finished making <Everything>."

# The Binaries target does the same thing as the Everything target plus it
# builds new binaries for the current system. Only execute "make Binaries"
# when you need to rebuild the binaries located in the binary distribution
# directory (currently ncarview/bin/$ARCH

Binaries: Config
	@echo ""
	@echo "Making <Binaries>."
	@$(YMAKE)
	@$(MAKE) Info
	@$(MAKE) Makefiles
	@$(MAKE) cleanbin all install
	@echo "Finished making <Binaries>."

dirs::
	@-if (test ! -d $(BINDIR)) then mkdir $(BINDIR); else continue; fi
	@-if (test ! -d $(BINDIR) || test ! -w $(BINDIR)) then \
	echo "Installation Error - $(BINDIR) is not writeable"; exit 1; \
	else continue; fi
	@-if (test ! -d $(LIBDIR)) then mkdir $(LIBDIR); else continue; fi
	@-if (test ! -d $(LIBDIR) || test ! -w $(LIBDIR)) then \
	echo "Installation Error - $(LIBDIR) is not writeable"; exit 1; \
	else continue; fi
	@-if (test ! -d $(MANDIR)) then mkdir $(MANDIR); else continue; fi
	@-if (test ! -d $(MANDIR) || test ! -w $(MANDIR)) then \
	echo "Installation Error - $(MANDIR) is not writeable"; exit 1; \
	else continue; fi
	@-if (test ! -d $(FONTCAPDIR)) then mkdir $(FONTCAPDIR); \
	else continue; fi
	@-if (test ! -d $(FONTCAPDIR) || test ! -w $(FONTCAPDIR)) then \
	echo "Installation Error - $(FONTCAPDIR) is not writeable"; exit 1; \
	else continue; fi
	@-if (test ! -d $(GRAPHCAPDIR)) then mkdir $(GRAPHCAPDIR); \
	else continue; fi
	@-if (test ! -d $(GRAPHCAPDIR) || test ! -w $(GRAPHCAPDIR)) then \
	echo "Installation Error - $(GRAPHCAPDIR) is not writeable"; exit 1; \
	else continue; fi
	@-if (test ! -d $(DBDIR)) then mkdir $(DBDIR); else continue; fi
	@-if (test ! -d $(DBDIR) || test ! -w $(DBDIR)) then \
	echo "Installation Error - $(DBDIR) is not writeable"; exit 1; \
	else continue; fi
	@-if (test ! -d $(EXAMPLEDIR)) then mkdir $(EXAMPLEDIR); \
	else continue; fi
	@-if (test ! -d $(EXAMPLEDIR) || test ! -w $(EXAMPLEDIR)) then \
	echo "Installation Error - $(EXAMPLEDIR) is not writeable"; exit 1; \
	else continue; fi
	@-if (test ! -d $(TESTDIR)) then mkdir $(TESTDIR); else continue; fi
	@-if (test ! -d $(TESTDIR) || test ! -w $(TESTDIR)) then \
	echo "Installation Error - $(TESTDIR) is not writeable"; exit 1; \
	else continue; fi

dirs::
	@-if (test ! -d $(XAPPDIR)) then mkdir $(XAPPDIR); else continue; fi
	@-if (test ! -d $(XAPPDIR) || test ! -w $(XAPPDIR)) then \
	echo "Installation Error - $(XAPPDIR) is not writeable"; exit 1; \
	else continue; fi

Makefiles:: local-Makefile

local-Makefile:
	@$(YMAKE)

Makefiles::
	@for dir in $(SUBDIRS) ; do\
	(cd $$dir; echo "Making Makefiles in $$dir";\
	$(MAKE) Makefiles $(MFLAGS));\
	done

install::
	@for dir in $(SUBDIRS) ; do\
	(cd $$dir; echo "Installing $$dir";\
	$(MAKE) install $(MFLAGS));\
	done

clean::
	@for dir in $(SUBDIRS) ; do\
	(cd $$dir; echo "Cleaning in $$dir";\
	$(MAKE) clean $(MFLAGS));\
	done

cleanbin::
	@for dir in $(SUBDIRS) ; do\
	(cd $$dir; echo "Cleaning in $$dir";\
	$(MAKE) cleanbin $(MFLAGS));\
	done


# This is an automake file for NCL.
# Ed Hartnett 9/2/18

LIBSEARCHUSER = $LDFLAGS
CTOFLIBSUSER = $LDFLAGS
CAIROLIBUSER = $LDFLAGS

CC_LDOPTIONS = $CFLAGS
FC_LDOPTIONS = $FCFLAGS

XLIB = @XLIB@
EXTRA_SYS_LIBS = @EXTRA_SYS_LIBS@

STD_DEFINES = @STD_DEFINES@
PROJECTDEFINES = @PROJECTDEFINES@

SYSTEM_INCLUDE = @SYSTEM_INCLUDE@
ROBJDIR = $(prefix)/lib/ncarg/robj
INCDIR = ${prefix}/include
LIBDIR = ${prefix}/lib
NCARGDIR = ${prefix}/lib/ncarg
EXAMPLESDIR = ${prefix}/lib/ncarg/examples
TESTSDIR = ${prefix}/lib/ncarg/database
TUTORIALDIR = ${prefix}/lib/ncarg/tutorial
DBDIR = ${prefix}/lib/ncarg/database
BINDIR = ${prefix}/bin

EXE_SUFFIX = @EXE_SUFFIX@

bin_SCRIPTS = ncargcc ncargex ncargf77 ncargfile ncargsrc ncargsrcy

EXTRA_DIST = ncargcc.csh ncargex.csh ncargf77.csh ncargfile.csh	\
ncargsrc.csh ncargsrcy.csh

# Install this script in a special place.
wwwdir = $(prefix)/lib/ncarg/ngwww
www_HEADERS = ngurl

ncargf77: ncargf77.csh
	@sed \
	-e 's+SED_FC+$(F77)+' \
	-e 's+SED_LIBNCARG_GKS+ncarg_gks+' \
	-e 's+SED_LIBSEARCHUSER+$(LIBSEARCHUSER)+' \
	-e 's+SED_LDFLAGS+$(FC_LDOPTIONS)+' \
	-e 's+SED_SYSTEM_INCLUDE+$(SYSTEM_INCLUDE)+' \
	-e 's+SED_XLIB+$(XLIB)+' \
	-e 's+SED_CAIROLIBUSER+$(CAIROLIBUSER)+' \
	-e 's+SED_LIBDIR+$(LIBDIR)+' \
	-e 's+SED_NCARGDIR+$(NCARGDIR)+' \
	-e 's+SED_LIBEXTRA+$(EXTRA_SYS_LIBS)+' \
	-e 's+SED_ROBJDIR+$(ROBJDIR)+' \
	<ncargf77.csh > ncargf77

ncargf90: ncargf77.csh
	@sed \
	-e 's+SED_FC+$(F77)+' \
	-e 's+SED_LIBNCARG_GKS+ncarg_gks+' \
	-e 's+SED_LIBSEARCHUSER+$(LIBSEARCHUSER)+' \
	-e 's+SED_LDFLAGS+$(FC_LDOPTIONS)+' \
	-e 's+SED_SYSTEM_INCLUDE+$(SYSTEM_INCLUDE)+' \
	-e 's+SED_XLIB+$(XLIB)+' \
	-e 's+SED_CAIROLIBUSER+$(CAIROLIBUSER)+' \
	-e 's+SED_LIBDIR+$(LIBDIR)+' \
	-e 's+SED_NCARGDIR+$(NCARGDIR)+' \
	-e 's+SED_LIBEXTRA+$(EXTRA_SYS_LIBS)+' \
	-e 's+SED_ROBJDIR+$(ROBJDIR)+' \
	<ncargf77.csh > ncargf90
ncargcc: ncargcc.csh
	sed \
	-e 's+SED_CC+$(CC)+' \
	-e 's+SED_LIBNCARG_GKS+ncarg_gks+' \
	-e 's+SED_LIBSEARCHUSER+$(LIBSEARCHUSER)+' \
	-e 's+SED_STDDEF+$(STD_DEFINES)+' \
	-e 's+SED_PROJDEF+$(PROJECTDEFINES)+' \
	-e 's+SED_LDCFLAGS+$(CC_LDOPTIONS)+' \
	-e 's+SED_SYSTEM_INCLUDE+$(SYSTEM_INCLUDE)+' \
	-e 's+SED_XLIB+$(XLIB)+' \
	-e 's+SED_CAIROLIBUSER+$(CAIROLIBUSER)+' \
	-e 's+SED_CTOFLIBSUSER+$(CTOFLIBSUSER)+' \
	-e 's+SED_LIBDIR+$(LIBDIR)+' \
	-e 's+SED_INCDIR+$(INCDIR)+' \
	-e 's+SED_NCARGDIR+$(NCARGDIR)+' \
	-e 's+SED_ROBJDIR+$(ROBJDIR)+' \
	<ncargcc.csh >ncargcc
ncargex: ncargex.csh
	@sed \
	-e 's+SED_EXE_SUFFIX+$(EXE_SUFFIX)+' \
	-e 's+SED_EXAMPLESDIR+$(EXAMPLESDIR)+' \
	-e 's+SED_TESTSDIR+$(TESTSDIR)+' \
	-e 's+SED_TUTORIALDIR+$(TUTORIALDIR)+' \
	<ncargex.csh >ncargex

ncargfile: ncargfile.csh
	@sed \
	-e 's+SED_DBDIR+$(DBDIR)+' \
	<ncargfile.csh >ncargfile

ncargsrc: ncargsrc.csh
	@sed \
	-e 's+SED_BINDIR+$(BINDIR)+' \
	<ncargsrc.csh >ncargsrc

ncargsrcy: ncargsrcy.csh
	@sed \
	-e 's+SED_LIBDIR+$(LIBDIR)+' \
	<ncargsrcy.csh >ncargsrcy

CLEANFILES = $(bin_SCRIPTS)

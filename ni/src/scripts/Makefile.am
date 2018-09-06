# This is an automake file for NCL
# Ed Hartnett 9/5/18

dist_bin_SCRIPTS = nhlcc nhlf77 nhlf90 ng4ex WRAPIT ncl_filedump	\
ncl_grib2nc ncl_convert2nc scrip_check_input

man1_MANS = ng4ex.m nhlcc.m nhlf77.m nhlf90.m

nhlf77: nhlf77.csh
	@sed \
	-e 's+SED_FC+$(F77)+' \
	-e 's+SED_LIBNCARG_GKS+ncarg_gks+' \
	-e 's+SED_LIBHLU+hlu+' \
	-e 's+SED_LIBSEARCHUSER+$(LIBSEARCHUSER)+' \
	-e 's+SED_INCSEARCHUSER+$(INCSEARCHUSER)+' \
	-e 's+SED_LDFFLAGS+$(FC_LDOPTIONS)+' \
	-e 's+SED_SYSTEM_INCLUDE+$(SYSTEM_INCLUDE)+' \
	-e 's+SED_XLIB+$(XLIB)+' \
	-e 's+SED_CAIROLIBUSER+$(CAIROLIBUSER)+' \
	-e 's+SED_LIBDIR+$(LIBDIR)+' \
	-e 's+SED_INCDIR+$(INCDIR)+' \
	-e 's+SED_NCDFLIBS+$(NCDFLIBS) $(NETCDF4LIB)+' \
	-e 's+SED_HDFLIB+$(HDFLIB)+' \
	-e 's+SED_NCARGDIR+$(NCARGDIR)+' \
	-e 's+SED_ROBJDIR+$(ROBJDIR)+' \
	<nhlf77.csh > nhlf77

nhlf90: nhlf77.csh
	@sed \
	-e 's+SED_FC+$(F77)+' \
	-e 's+SED_LIBNCARG_GKS+ncarg_gks+' \
	-e 's+SED_LIBHLU+hlu+' \
	-e 's+SED_LIBSEARCHUSER+$(LIBSEARCHUSER)+' \
	-e 's+SED_INCSEARCHUSER+$(INCSEARCHUSER)+' \
	-e 's+SED_LDFFLAGS+$(FC_LDOPTIONS)+' \
	-e 's+SED_SYSTEM_INCLUDE+$(SYSTEM_INCLUDE)+' \
	-e 's+SED_XLIB+$(XLIB)+' \
	-e 's+SED_CAIROLIBUSER+$(CAIROLIBUSER)+' \
	-e 's+SED_LIBDIR+$(LIBDIR)+' \
	-e 's+SED_INCDIR+$(INCDIR)+' \
	-e 's+SED_NCDFLIBS+$(NCDFLIBS) $(NETCDF4LIB)+' \
	-e 's+SED_HDFLIB+$(HDFLIB)+' \
	-e 's+SED_NCARGDIR+$(NCARGDIR)+' \
	-e 's+SED_ROBJDIR+$(ROBJDIR)+' \
	<nhlf77.csh > nhlf90

nhlcc: nhlcc.csh
	@sed \
	-e 's+SED_CC+$(CC)+' \
	-e 's+SED_LIBNCARG_GKS+ncarg_gks+' \
	-e 's+SED_LIBHLU+hlu+' \
	-e 's+SED_LIBSEARCHUSER+$(LIBSEARCHUSER)+' \
	-e 's+SED_INCSEARCHUSER+$(INCSEARCHUSER)+' \
	-e 's+SED_STDDEF+$(STD_DEFINES)+' \
	-e 's+SED_PROJDEF+$(PROJECTDEFINES)+' \
	-e 's+SED_LDCFLAGS+$(CC_LDOPTIONS)+' \
	-e 's+SED_CTOFLIBSUSER+$(CTOFLIBSUSER)+' \
	-e 's+SED_SYSTEM_INCLUDE+$(SYSTEM_INCLUDE)+' \
	-e 's+SED_XTOOLLIB+$(XTOOLLIB)+' \
	-e 's+SED_XMOTIFLIB+$(XMOTIFLIB)+' \
	-e 's+SED_NCDFLIBS+$(NCDFLIBS) $(NETCDF4LIB)+' \
	-e 's+SED_HDFLIB+$(HDFLIB)+' \
	-e 's+SED_XLIB+$(XLIB)+' \
	-e 's+SED_CAIROLIBUSER+$(CAIROLIBUSER)+' \
	-e 's+SED_LIBDIR+$(LIBDIR)+' \
	-e 's+SED_INCDIR+$(INCDIR)+' \
	-e 's+SED_NCARGDIR+$(NCARGDIR)+' \
	-e 's+SED_ROBJDIR+$(ROBJDIR)+' \
	<nhlcc.csh >nhlcc

ng4ex: ng4ex.csh
	@sed \
        -e 's+SED_EXE_SUFFIX+$(EXE_SUFFIX)+' \
	-e 's+SED_HLUEXDIR+$(HLUEXDIR)+' \
	-e 's+SED_NCLEXDIR+$(NCLEXDIR)+' \
	-e 's+SED_RESDIR+$(RESDIR)+' \
	-e 's+SED_DATADIR+$(DATADIR)+' \
	< ng4ex.csh > ng4ex

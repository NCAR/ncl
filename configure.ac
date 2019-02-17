# This is part of NCAR's NCL package. Copyright 2005-2018, see
# the COPYRIGHT file for more information.
# Ed Hartnett

# Recall that ${VAR-exp} expands to $VAR if var is set (even to null),
# and to exp otherwise.

# Running autoconf on this file will trigger a warning if autoconf is
# not at least the specified version.
AC_PREREQ([2.59])

# Initialize with name, version, and support email address.
AC_INIT([ncl], [6.5.0-development], [ncl-install@ucar.edu])

# Find out about the host we're building on.
AC_CANONICAL_HOST

# Find out about the target we're building for.
AC_CANONICAL_TARGET

AM_INIT_AUTOMAKE([foreign dist-zip subdir-objects])

# Keep libtool macros in an m4 directory.
AC_CONFIG_MACRO_DIR([m4])

# Set up libtool.
LT_PREREQ([2.4])
LT_INIT()

#AC_CONFIG_HEADERS([config.h])

# Find the C compiler.
AC_PROG_CC
AM_PROG_CC_C_O
AC_C_CONST
AC_PROG_CPP

# Find the Fortran compiler.
AC_PROG_FC
AC_PROG_F77

# Find the install program.
AC_PROG_INSTALL

# Check to see if any macros must be set to enable large (>2GB) files.
AC_SYS_LARGEFILE

# Does the user want to build NGMATH?
AC_MSG_CHECKING([whether we should build NGMATH])
AC_ARG_ENABLE([ngmath], [AS_HELP_STRING([--disable-ngmath],
              [do not build with NGMATH (disables most of build)])])
test "x$enable_ngmath" = xno || enable_ngmath=yes
AC_MSG_RESULT([$enable_ngmath])
AM_CONDITIONAL(BUILD_NGMATH, [test x$enable_ngmath = xyes])
if test x$enable_ngmath = xno; then
   enable_ncarg2d=no
fi

# Does the user want to build NCARG2D?
AC_MSG_CHECKING([whether we should build NCARG2D])
AC_ARG_ENABLE([ncarg2d], [AS_HELP_STRING([--disable-ncarg2d],
              [do not build with NCARG2D])])
test "x$enable_ncarg2d" = xno || enable_ncarg2d=yes
AC_MSG_RESULT([$enable_ncarg2d])
AM_CONDITIONAL(BUILD_NCARG2D, [test x$enable_ncarg2d = xyes])
if test $enable_ngmath = no -a $enable_ncarg2d = yes; then
   AC_MSG_ERROR([Cannot build NCARG2D if NGMATH is disabled.])
fi
if test x$enable_ncarg2d = xno; then
   enable_ncarview=no
fi

# Does the user want to build NCARVIEW?
AC_MSG_CHECKING([whether we should build NCARVIEW])
AC_ARG_ENABLE([ncarview], [AS_HELP_STRING([--disable-ncarview],
              [do not build with NCARVIEW])])
test "x$enable_ncarview" = xno || enable_ncarview=yes
AC_MSG_RESULT([$enable_ncarview])
AM_CONDITIONAL(BUILD_NCARVIEW, [test x$enable_ncarview = xyes])
if test $enable_ncarg2d = no -a $enable_ncarview = yes; then
   AC_MSG_ERROR([Cannot build NCARVIEW if NCARG2D is disabled.])
fi
if test x$enable_ncarview = xno; then
   enable_ncl=no
fi

# Does the user want to build NCL?
AC_MSG_CHECKING([whether we should build NCL])
AC_ARG_ENABLE([ncl], [AS_HELP_STRING([--disable-ncl],
              [do not build with NCL (only NCAR graphics library will be built)])])
test "x$enable_ncl" = xno || enable_ncl=yes
AC_MSG_RESULT([$enable_ncl])
AM_CONDITIONAL(BUILD_NCL, [test x$enable_ncl = xyes])
if test $enable_ncarview = no -a $enable_ncl = yes; then
   AC_MSG_ERROR([Cannot build NCL if NCARVIEW is disabled.])
fi

# Make sure to check for Xlibs in this old directory.
AC_CHECK_FILE([/usr/X11R6/lib], [LDFLAGS="-L/usr/X11R6/lib"])
AC_CHECK_LIB([X11], [XOpenDisplay])
AC_CHECK_LIB([Xext], [XShmAttach])
AC_CHECK_LIB([Xrender], [XRenderQueryExtension])

# Find required libraries and thier headers.
AC_CHECK_LIB([m], [floor], [], [AC_MSG_ERROR([math library is required])], [])
AC_CHECK_LIB([netcdf], [nc_create], [], [AC_MSG_ERROR([NetCDF library is required])], [])
AC_CHECK_LIB([jpeg], [jpeg_write_marker], [], [AC_MSG_ERROR([JPEG library is required])], [])
AC_CHECK_LIB([z], [deflate], [], [AC_MSG_ERROR([zlib library is required])], [])
AC_CHECK_LIB([cairo], [cairo_in_fill], [], [AC_MSG_ERROR([cairo library is required])], [])
AC_CHECK_LIB([png], [png_access_version_number], [], [AC_MSG_ERROR([PNG library is required])], [])
AC_CHECK_LIB([freetype], [FT_Init_FreeType], [], [AC_MSG_ERROR([Freetype2 library is required])], [])
AC_CHECK_HEADER([ft2build.h], [],  [AC_MSG_ERROR([Freetype2 header files are required])])
AC_CHECK_LIB([pixman-1], [pixman_blt], [], [AC_MSG_ERROR([Freetype2 library is required])], [])
AC_CHECK_LIB([udunits2], [ut_compare], [], [AC_MSG_ERROR([UDUNITS2 library is required])], [])
AC_CHECK_LIB([expat], [XML_ParserCreate], [], [AC_MSG_ERROR([expat library is required])], [])
#AC_CHECK_LIB([sz], [SZ_encoder_enabled], [], [AC_MSG_ERROR([szlib library is required])], [])

# HDF4 use is optional.
use_hdf4=yes
AC_CHECK_LIB([df], [Hclose], [], [use_hdf4=no])
AC_CHECK_LIB([mfhdf], [SDcreate], [], [use_hdf4=no])
AC_MSG_CHECKING([whether HDF4 will be used])
AC_MSG_RESULT($use_hdf4)
if test x$use_hdf4 = xyes; then
   AC_DEFINE([BuildHDF4])
fi
AM_CONDITIONAL(USE_HDF4, [test x$use_hdf4 = xyes])

# HDF-EOS library is optional. HDF4 is required for HDF-EOS.
use_hdfeos=no
if test $use_hdf4 = yes; then
   use_hdfeos=yes
   AC_CHECK_LIB([hdfeos], [GDcreate], [], [use_hdfeos=no])
   AC_CHECK_LIB([Gctp], [gctp], [], [use_hdfeos=no])
fi
AC_MSG_CHECKING([whether HDF-EOS will be used])
AC_MSG_RESULT($use_hdfeos)
if test x$use_hdfeos = xyes; then
   AC_DEFINE([BuildHDFEOS])
fi
AM_CONDITIONAL(USE_HDFEOS, [test x$use_hdfeos = xyes])

# GRIB2 use is optional. Check for jasper and grib2c libraries.
use_grib2=yes
AC_CHECK_LIB([jasper], [jas_memdump], [], [use_grib2=no])
AC_CHECK_LIB([grib2c], [getgridindex], [], [use_grib2=no])
AC_MSG_CHECKING([whether GRIB2 will be used])
AC_MSG_RESULT($use_grib2)
AM_CONDITIONAL(USE_GRIB2, [test x$use_grib2 = xyes])
if test x$use_grib2 = xyes; then
   AC_DEFINE([BuildGRIB2])
fi

# Map project library PROJ is optional. Check for it.
use_proj4=yes
AC_CHECK_LIB([proj], [pj_laea], [], [use_proj4=no])
AC_MSG_CHECKING([whether PROJ4 will be used])
AC_MSG_RESULT($use_proj4)
AM_CONDITIONAL(USE_PROJ4, [test x$use_proj4 = xyes])

# GDAL library is optional. Check for it.
use_gdal=yes
AC_CHECK_LIB([gdal], [OGR_G_AddPoint], [], [use_gdal=no])
AC_MSG_CHECKING([whether GDAL will be used])
AC_MSG_RESULT($use_gdal)
AM_CONDITIONAL(USE_GDAL, [test x$use_gdal = xyes])
if test x$use_gdal = xyes; then
   AC_DEFINE([BuildGDAL])
fi

# HDF5 library is optional. Check for it.
use_hdf5=yes
AC_CHECK_LIB([hdf5], [H5Fflush], [], [use_hdf5=no])
AC_MSG_CHECKING([whether HDF5 will be used])
AC_MSG_RESULT($use_hdf5)
if test x$use_hdf5 = xyes; then
   AC_DEFINE([BuildHDF5])
fi
AM_CONDITIONAL(USE_HDF5, [test x$use_hdf5 = xyes])

# Is pkg-config installed? If not, bail.
AC_CHECK_PROGS([PKG_CONFIG], [pkg-config])
if test -z "$PKG_CONFIG"; then
   AC_MSG_ERROR([Cannot find pkg-config utility. Install pkg-config and try again.])
fi

# Set these values.
AC_DEFINE([NCARGURL], ["ngurl"])
AC_DEFINE([NeedFuncProto])
AC_DEFINE([SYSV])
AC_DEFINE([_POSIX_SOURCE])
AC_DEFINE([_XOPEN_SOURCE])
AC_DEFINE([ByteSwapped])
AC_DEFINE([Linux])
AC_DEFINE([IBM])
AC_DEFINE([NGVERSION], ["6.5.0"])
AC_DEFINE([NCLVERSION], ["6.5.0"])
AC_DEFINE([DEFAULT_GKS_BUFSIZE], [0])
AC_DEFINE([NGTMPDIR], ["tmp"])
AC_DEFINE([MAJOR], [4])
AC_DEFINE([BuildPNG])
AC_DEFINE([SPOOL_TRANS], ["ctrans"])
AC_DEFINE([BINDIR], ["bin"])
AC_DEFINE([LIBDIR], ["lib"])
AC_DEFINE([FONTCAPDIR], ["fontcaps"])
AC_DEFINE([GRAPHCAPDIR], ["graphcaps"])
AC_DEFINE([NCARGDIR], ["ncarg"])
AC_DEFINE([USE_NETCDF4]) # for now always use netcdf-4
use_netcdf4=yes
AM_CONDITIONAL(USE_NETCDF4, [test x$use_netcdf4 = xyes])
AC_SUBST([LIBSEARCHUSER], [-L/usr/X11R6/lib64])
AC_SUBST([SYSTEMINCLUDE], [-L/usr/X11R6/lib64])

# These files need to be copied to the build directory.
AC_CONFIG_LINKS([ncarg2d/src/libncarg/conpack/CodeIftran:ncarg2d/src/libncarg/conpack/CodeIftran])
AC_CONFIG_LINKS([ncarg2d/src/libncarg/ezmapa/CodeIftran:ncarg2d/src/libncarg/ezmapa/CodeIftran])
AC_CONFIG_LINKS([ncarg2d/src/libncarg/softfill/CodeIftran:ncarg2d/src/libncarg/softfill/CodeIftran])
AC_CONFIG_LINKS([ncarg2d/src/libncarg/areas/CodeIftran:ncarg2d/src/libncarg/areas/CodeIftran])
AC_CONFIG_LINKS([ncarg2d/src/libncarg/conpackt/CodeIftran:ncarg2d/src/libncarg/conpackt/CodeIftran])
AC_CONFIG_LINKS([ncarg2d/src/libncarg/vaspackt/CodeIftran:ncarg2d/src/libncarg/vaspackt/CodeIftran])
AC_CONFIG_LINKS([ncarg2d/src/libncarg/polypack/CodeIftran:ncarg2d/src/libncarg/polypack/CodeIftran])
AC_CONFIG_LINKS([ncarg2d/src/bin/scripts/ncargcc.csh:ncarg2d/src/bin/scripts/ncargcc.csh])
AC_CONFIG_LINKS([ncarg2d/src/bin/scripts/ncargf77.csh:ncarg2d/src/bin/scripts/ncargf77.csh])
AC_CONFIG_LINKS([ncarg2d/src/bin/scripts/ncargex.csh:ncarg2d/src/bin/scripts/ncargex.csh])
AC_CONFIG_LINKS([ncarg2d/src/bin/scripts/ncargfile.csh:ncarg2d/src/bin/scripts/ncargfile.csh])
AC_CONFIG_LINKS([ncarg2d/src/bin/scripts/ncargsrc.csh:ncarg2d/src/bin/scripts/ncargsrc.csh])
AC_CONFIG_LINKS([ncarg2d/src/bin/scripts/ncargsrcy.csh:ncarg2d/src/bin/scripts/ncargsrcy.csh])

# These files will be created when the configure script is run.
AC_CONFIG_FILES([Makefile
                 common/Makefile
                 common/src/Makefile
                 common/src/libncarg_c/Makefile
                 common/src/libncarg_c/ncarg/c.h:common/src/libncarg_c/c.h.in
                 common/src/libncarg_c/ncarg_path.c
                 common/src/bin/Makefile
                 common/src/bin/ncargpath/Makefile
                 common/src/fontcap/Makefile
                 common/src/graphcap/Makefile
                 external/Makefile
                 external/fftpack5_dp/Makefile
                 external/blas/Makefile
                 external/sphere3.1_dp/Makefile
                 external/lapack/Makefile
                 ncarg2d/Makefile
                 ncarg2d/man/Makefile
                 ncarg2d/man/areas/Makefile
                 ncarg2d/man/autograph/Makefile
                 ncarg2d/man/bivar/Makefile
                 ncarg2d/man/colconv/Makefile
                 ncarg2d/man/conpack/Makefile
                 ncarg2d/man/dashline/Makefile
                 ncarg2d/man/dashpack/Makefile
                 ncarg2d/man/error_handling/Makefile
                 ncarg2d/man/ezmap/Makefile
                 ncarg2d/man/gflash/Makefile
                 ncarg2d/man/gks/Makefile
                 ncarg2d/man/gksC/Makefile
                 ncarg2d/man/gridall/Makefile
                 ncarg2d/man/halftone/Makefile
                 ncarg2d/man/histogram/Makefile
                 ncarg2d/man/isosurface/Makefile
                 ncarg2d/man/labelbar/Makefile
                 ncarg2d/man/ngmisc/Makefile
                 ncarg2d/man/other/Makefile
                 ncarg2d/man/plotchar/Makefile
                 ncarg2d/man/polypack/Makefile
                 ncarg2d/man/pwrite_family/Makefile
                 ncarg2d/man/conran_family/Makefile
                 ncarg2d/man/conrec_family/Makefile
                 ncarg2d/man/scrolled_title/Makefile
                 ncarg2d/man/softfill/Makefile
                 ncarg2d/man/spps/Makefile
                 ncarg2d/man/streamlines/Makefile
                 ncarg2d/man/surface/Makefile
                 ncarg2d/man/tdpack/Makefile
                 ncarg2d/man/threed/Makefile
                 ncarg2d/man/vectors/Makefile
                 ncarg2d/man/wmap/Makefile
                 ncarg2d/src/Makefile
                 ncarg2d/src/libncarg/Makefile
                 ncarg2d/src/libncarg/ncarg/ncargC.h:ncarg2d/src/libncarg/ncargC.h
                 ncarg2d/src/libncarg/Iftran/Makefile
                 ncarg2d/src/libncarg/areas/Makefile
                 ncarg2d/src/libncarg/autogrph/Makefile
                 ncarg2d/src/libncarg/colconv/Makefile
                 ncarg2d/src/libncarg/common/Makefile
                 ncarg2d/src/libncarg/concom/Makefile
                 ncarg2d/src/libncarg/conpack/Makefile
                 ncarg2d/src/libncarg/conpackt/Makefile
                 ncarg2d/src/libncarg/conran/Makefile
                 ncarg2d/src/libncarg/conrec/Makefile
                 ncarg2d/src/libncarg/conterp/Makefile
                 ncarg2d/src/libncarg/dashchar/Makefile
                 ncarg2d/src/libncarg/dashpack/Makefile
                 ncarg2d/src/libncarg/ezmap/Makefile
                 ncarg2d/src/libncarg/ezmapa/Makefile
                 ncarg2d/src/libncarg/ezmapb/Makefile
                 ncarg2d/src/libncarg/ezmapc/Makefile
                 ncarg2d/src/libncarg/gflash/Makefile
                 ncarg2d/src/libncarg/gridal/Makefile
                 ncarg2d/src/libncarg/hafton/Makefile
                 ncarg2d/src/libncarg/histgr/Makefile
                 ncarg2d/src/libncarg/isosrf/Makefile
                 ncarg2d/src/libncarg/isosrfhr/Makefile
                 ncarg2d/src/libncarg/labelbar/Makefile
                 ncarg2d/src/libncarg/ngmisc/Makefile
                 ncarg2d/src/libncarg/plotchar/Makefile
                 ncarg2d/src/libncarg/polypack/Makefile
                 ncarg2d/src/libncarg/pwritx/Makefile
                 ncarg2d/src/libncarg/pwrity/Makefile
                 ncarg2d/src/libncarg/pwrzi/Makefile
                 ncarg2d/src/libncarg/pwrzs/Makefile
                 ncarg2d/src/libncarg/pwrzt/Makefile
                 ncarg2d/src/libncarg/softfill/Makefile
                 ncarg2d/src/libncarg/spps/Makefile
                 ncarg2d/src/libncarg/srface/Makefile
                 ncarg2d/src/libncarg/stitle/Makefile
                 ncarg2d/src/libncarg/strmln/Makefile
                 ncarg2d/src/libncarg/support/Makefile
                 ncarg2d/src/libncarg/tdpack/Makefile
                 ncarg2d/src/libncarg/threed/Makefile
                 ncarg2d/src/libncarg/vaspackt/Makefile
                 ncarg2d/src/libncarg/velvct/Makefile
                 ncarg2d/src/libncarg/wmap/Makefile
                 ncarg2d/src/libncarg/areasC/Makefile
                 ncarg2d/src/libncarg/autogrphC/Makefile
                 ncarg2d/src/libncarg/colconvC/Makefile
                 ncarg2d/src/libncarg/conpackC/Makefile
                 ncarg2d/src/libncarg/conpacktC/Makefile
                 ncarg2d/src/libncarg/commonC/Makefile
                 ncarg2d/src/libncarg/dashcharC/Makefile
                 ncarg2d/src/libncarg/dashpackC/Makefile
                 ncarg2d/src/libncarg/ezmapCC/Makefile
                 ncarg2d/src/libncarg/gflashC/Makefile
                 ncarg2d/src/libncarg/gridalC/Makefile
                 ncarg2d/src/libncarg/histgrC/Makefile
                 ncarg2d/src/libncarg/isosrfC/Makefile
                 ncarg2d/src/libncarg/labelbarC/Makefile
                 ncarg2d/src/libncarg/ngmiscC/Makefile
                 ncarg2d/src/libncarg/plotcharC/Makefile
                 ncarg2d/src/libncarg/polypackC/Makefile
                 ncarg2d/src/libncarg/softfillC/Makefile
                 ncarg2d/src/libncarg/sppsC/Makefile
                 ncarg2d/src/libncarg/srfaceC/Makefile
                 ncarg2d/src/libncarg/stitleC/Makefile
                 ncarg2d/src/libncarg/supportC/Makefile
                 ncarg2d/src/libncarg/strmlnC/Makefile
                 ncarg2d/src/libncarg/tdpackC/Makefile
                 ncarg2d/src/libncarg/threedC/Makefile
                 ncarg2d/src/libncarg/velvctC/Makefile
                 ncarg2d/src/libncarg/wmapC/Makefile
                 ncarg2d/src/libncarg_gks/ncarg/gks.h:ncarg2d/src/libncarg_gks/gks.h
                 ncarg2d/src/libncarg_gks/ncarg/gksP.h:ncarg2d/src/libncarg_gks/cdrivers/gksP.h
                 ncarg2d/src/libncarg_gks/Makefile
                 ncarg2d/src/libncarg_gks/awi/Makefile
                 ncarg2d/src/libncarg_gks/bwi/Makefile
                 ncarg2d/src/libncarg_gks/wiss/Makefile
                 ncarg2d/src/libncarg_gks/cdrivers/Makefile
                 ncarg2d/src/libncarg_gks/awiC/Makefile
                 ncarg2d/src/liboptional/Makefile
                 ncarg2d/src/liboptional/conraq/Makefile
                 ncarg2d/src/liboptional/conras/Makefile
                 ncarg2d/src/liboptional/conrcqck/Makefile
                 ncarg2d/src/liboptional/conrcspr/Makefile
                 ncarg2d/src/liboptional/dashsmth/Makefile
                 ncarg2d/src/liboptional/dashline/Makefile
                 ncarg2d/src/liboptional/dashsupr/Makefile
                 ncarg2d/src/liboptional/agupwrtx/Makefile
                 ncarg2d/src/liboptional/ncarbd/Makefile
                 ncarg2d/src/bin/Makefile
                 ncarg2d/src/bin/scripts/Makefile
                 ncarg2d/src/bin/impltest/Makefile
                 ncarg2d/src/bin/ncargworld/Makefile
                 ncarg2d/src/bin/findg/Makefile
                 ncarg2d/src/bin/logos/Makefile
                 ncarg2d/src/bin/psfltrs/Makefile
                 ncarg2d/src/bin/pre2ncgm/Makefile
                 ncarg2d/src/bin/psplit/Makefile
                 ncarg2d/src/bin/tdpackdemo/Makefile
                 ncarg2d/src/bin/ezmapdemo/Makefile
                 ncarg2d/src/db/Makefile
                 ncarg2d/src/db/ezmap/Makefile
                 ncarg2d/src/db/plotchar/Makefile
                 ncarg2d/src/db/pwritxnt/Makefile
                 ncarg2d/src/db/ranfdata/Makefile
                 ncarg2d/src/db/ftfonts/Makefile
                 ncarg2d/src/examples/Makefile
                 ncarg2d/src/examples/eareas/Makefile
                 ncarg2d/src/examples/eautog/Makefile
                 ncarg2d/src/examples/ecolcv/Makefile
                 ncarg2d/src/examples/econpa/Makefile
                 ncarg2d/src/examples/econpt/Makefile
                 ncarg2d/src/examples/eezmap/Makefile
                 ncarg2d/src/examples/eezmpa/Makefile
                 ncarg2d/src/examples/elblba/Makefile
                 ncarg2d/src/examples/emisc/Makefile
                 ncarg2d/src/examples/epltch/Makefile
                 ncarg2d/src/examples/eppack/Makefile
                 ncarg2d/src/examples/esoftf/Makefile
                 ncarg2d/src/examples/espps/Makefile
                 ncarg2d/src/examples/esrfac/Makefile
                 ncarg2d/src/examples/estitl/Makefile
                 ncarg2d/src/examples/estrml/Makefile
                 ncarg2d/src/examples/etpack/Makefile
                 ncarg2d/src/examples/evaspt/Makefile
                 ncarg2d/src/examples/evelvc/Makefile
                 ncarg2d/src/examples/ewmap/Makefile
                 ncarg2d/src/fundamental/Makefile
                 ncarg2d/src/fundamental/fautograph/Makefile
                 ncarg2d/src/fundamental/fcolconv/Makefile
                 ncarg2d/src/fundamental/fdashline/Makefile
                 ncarg2d/src/fundamental/ffield_flow/Makefile
                 ncarg2d/src/fundamental/fgks/Makefile
                 ncarg2d/src/fundamental/fisosurface/Makefile
                 ncarg2d/src/fundamental/flabelbar/Makefile
                 ncarg2d/src/fundamental/fngmisc/Makefile
                 ncarg2d/src/fundamental/fplotchar/Makefile
                 ncarg2d/src/fundamental/fscrolled_title/Makefile
                 ncarg2d/src/fundamental/fsoftfill/Makefile
                 ncarg2d/src/fundamental/fspps/Makefile
                 ncarg2d/src/fundamental/fstreamlines/Makefile
                 ncarg2d/src/fundamental/fsurface/Makefile
                 ncarg2d/src/fundamental/fthreed/Makefile
                 ncarg2d/src/tests/Makefile
                 ncarg2d/src/tutorial/Makefile
                 ncarg2d/src/tutorial/class/Makefile
                 ncarg2d/src/tutorial/tareas/Makefile
                 ncarg2d/src/tutorial/tconpack/Makefile
                 ncarg2d/src/tutorial/tezmap/Makefile
                 ncarg2d/src/examplesC/Makefile
                 ncarg2d/src/examplesC/areas/Makefile
                 ncarg2d/src/examplesC/autograph/Makefile
                 ncarg2d/src/examplesC/bivar/Makefile
                 ncarg2d/src/examplesC/colconv/Makefile
                 ncarg2d/src/examplesC/conpack/Makefile
                 ncarg2d/src/examplesC/conpackt/Makefile
                 ncarg2d/src/examplesC/dashline/Makefile
                 ncarg2d/src/examplesC/dashpack/Makefile
                 ncarg2d/src/examplesC/ezmap/Makefile
                 ncarg2d/src/examplesC/gflash/Makefile
                 ncarg2d/src/examplesC/gks/Makefile
                 ncarg2d/src/examplesC/gridall/Makefile
                 ncarg2d/src/examplesC/histogram/Makefile
                 ncarg2d/src/examplesC/isosurface/Makefile
                 ncarg2d/src/examplesC/labelbar/Makefile
                 ncarg2d/src/examplesC/ngmisc/Makefile
                 ncarg2d/src/examplesC/plotchar/Makefile
                 ncarg2d/src/examplesC/polypack/Makefile
                 ncarg2d/src/examplesC/scrolled_title/Makefile
                 ncarg2d/src/examplesC/softfill/Makefile
                 ncarg2d/src/examplesC/spps/Makefile
                 ncarg2d/src/examplesC/streamlines/Makefile
                 ncarg2d/src/examplesC/surface/Makefile
                 ncarg2d/src/examplesC/tdpack/Makefile
                 ncarg2d/src/examplesC/threed/Makefile
                 ncarg2d/src/examplesC/vectors/Makefile
                 ncarg2d/src/examplesC/wmap/Makefile
                 ncarg2d/src/pdocexamples/Makefile
                 ncarg2d/src/pdocexamples/pgks/Makefile
                 ncarview/Makefile
                 ncarview/man/Makefile
                 ncarview/src/bin/Makefile
                 ncarview/src/lib/Makefile
                 ncarview/src/lib/libictrans/Makefile
                 ncarview/src/lib/libcgm/ncarg/cgm_tools.h:ncarview/src/lib/libcgm/cgm_tools.h
                 ncarview/src/lib/libcgm/Makefile
                 ncarview/src/lib/libcgm/ncarg/cgmdef.h:ncarview/src/lib/libcgm/cgmdef.h
                 ncarview/src/lib/libctrans/Makefile
                 ncarview/src/lib/libctrans/ncarg/ctrans.h:ncarview/src/lib/libctrans/ctrans.h
                 ncarview/src/lib/libncarg_ras/Makefile
                 ncarview/src/lib/libncarg_ras/ncarg/ncarg_ras.h:ncarview/src/lib/libncarg_ras/ncarg_ras.h
                 ncarview/src/Makefile
                 ncarview/app/Makefile
                 ngmath/src/bin/Makefile
                 ngmath/src/bin/nnalg/Makefile
                 ngmath/src/lib/Makefile
                 ngmath/src/lib/ncarg/ngmath.h:ngmath/src/lib/ngmath.h.in
                 ngmath/src/lib/gridpack/Makefile
                 ngmath/src/lib/gridpack/csagrid/Makefile
                 ngmath/src/lib/gridpack/cssgrid/Makefile
                 ngmath/src/lib/gridpack/dsgrid/Makefile
                 ngmath/src/lib/gridpack/fitgrid/Makefile
                 ngmath/src/lib/gridpack/natgrid/Makefile
                 ngmath/src/lib/gridpack/shgrid/Makefile
                 ngmath/src/examples/edsgrid/Makefile
                 ngmath/src/examples/efitgrid/Makefile
                 ngmath/src/examples/ecssgrid/Makefile
                 ngmath/src/examples/ecsagrid/Makefile
                 ngmath/src/examples/Makefile
                 ngmath/src/examples/enatgrid/Makefile
                 ngmath/src/examples/eshgrid/Makefile
                 ngmath/man/fitgrid/Makefile
                 ngmath/man/dsgrid/Makefile
                 ngmath/man/csagrid/Makefile
                 ngmath/man/Makefile
                 ngmath/man/shgrid/Makefile
                 ngmath/man/cssgrid/Makefile
                 ngmath/man/natgrid/Makefile
                 ngmath/src/Makefile
                 ngmath/Makefile
                 ni/src/lib/Makefile
                 ni/src/lib/hlu/Makefile
                 ni/src/lib/hlu/ncarg/hlu/AnnoManager.h:ni/src/lib/hlu/AnnoManager.h
                 ni/src/lib/hlu/ncarg/hlu/AnnoManagerP.h:ni/src/lib/hlu/AnnoManagerP.h
                 ni/src/lib/hlu/ncarg/hlu/App.h:ni/src/lib/hlu/App.h
                 ni/src/lib/hlu/ncarg/hlu/AppI.h:ni/src/lib/hlu/AppI.h
                 ni/src/lib/hlu/ncarg/hlu/AppP.h:ni/src/lib/hlu/AppP.h
                 ni/src/lib/hlu/ncarg/hlu/Base.h:ni/src/lib/hlu/Base.h
                 ni/src/lib/hlu/ncarg/hlu/BaseI.h:ni/src/lib/hlu/BaseI.h
                 ni/src/lib/hlu/ncarg/hlu/BaseP.h:ni/src/lib/hlu/BaseP.h
                 ni/src/lib/hlu/ncarg/hlu/CairoWorkstation.h:ni/src/lib/hlu/CairoWorkstation.h
                 ni/src/lib/hlu/ncarg/hlu/CairoWorkstationP.h:ni/src/lib/hlu/CairoWorkstationP.h
                 ni/src/lib/hlu/ncarg/hlu/Callbacks.h:ni/src/lib/hlu/Callbacks.h
                 ni/src/lib/hlu/ncarg/hlu/CallbacksP.h:ni/src/lib/hlu/CallbacksP.h
                 ni/src/lib/hlu/ncarg/hlu/CnRenderer.h:ni/src/lib/hlu/CnRenderer.h
                 ni/src/lib/hlu/ncarg/hlu/CnRendererP.h:ni/src/lib/hlu/CnRendererP.h
                 ni/src/lib/hlu/ncarg/hlu/CnStdRenderer.h:ni/src/lib/hlu/CnStdRenderer.h
                 ni/src/lib/hlu/ncarg/hlu/CnStdRendererP.h:ni/src/lib/hlu/CnStdRendererP.h
                 ni/src/lib/hlu/ncarg/hlu/CnTriMeshRenderer.h:ni/src/lib/hlu/CnTriMeshRenderer.h
                 ni/src/lib/hlu/ncarg/hlu/CnTriMeshRendererP.h:ni/src/lib/hlu/CnTriMeshRendererP.h
                 ni/src/lib/hlu/ncarg/hlu/color.h:ni/src/lib/hlu/color.h
                 ni/src/lib/hlu/ncarg/hlu/ContourPlot.h:ni/src/lib/hlu/ContourPlot.h
                 ni/src/lib/hlu/ncarg/hlu/ContourPlotI.h:ni/src/lib/hlu/ContourPlotI.h
                 ni/src/lib/hlu/ncarg/hlu/ContourPlotP.h:ni/src/lib/hlu/ContourPlotP.h
                 ni/src/lib/hlu/ncarg/hlu/ConvertersP.h:ni/src/lib/hlu/ConvertersP.h
                 ni/src/lib/hlu/ncarg/hlu/Convert.h:ni/src/lib/hlu/Convert.h
                 ni/src/lib/hlu/ncarg/hlu/ConvertP.h:ni/src/lib/hlu/ConvertP.h
                 ni/src/lib/hlu/ncarg/hlu/CoordApprox.h:ni/src/lib/hlu/CoordApprox.h
                 ni/src/lib/hlu/ncarg/hlu/CoordArrays.h:ni/src/lib/hlu/CoordArrays.h
                 ni/src/lib/hlu/ncarg/hlu/CoordArraysP.h:ni/src/lib/hlu/CoordArraysP.h
                 ni/src/lib/hlu/ncarg/hlu/CoordArrTableFloatP.h:ni/src/lib/hlu/CoordArrTableFloatP.h
                 ni/src/lib/hlu/ncarg/hlu/CoordArrTable.h:ni/src/lib/hlu/CoordArrTable.h
                 ni/src/lib/hlu/ncarg/hlu/CoordArrTableP.h:ni/src/lib/hlu/CoordArrTableP.h
                 ni/src/lib/hlu/ncarg/hlu/CurvilinearTransObj.h:ni/src/lib/hlu/CurvilinearTransObj.h
                 ni/src/lib/hlu/ncarg/hlu/CurvilinearTransObjP.h:ni/src/lib/hlu/CurvilinearTransObjP.h
                 ni/src/lib/hlu/ncarg/hlu/DataCommF.h:ni/src/lib/hlu/DataCommF.h
                 ni/src/lib/hlu/ncarg/hlu/DataComm.h:ni/src/lib/hlu/DataComm.h
                 ni/src/lib/hlu/ncarg/hlu/DataCommP.h:ni/src/lib/hlu/DataCommP.h
                 ni/src/lib/hlu/ncarg/hlu/DataItem.h:ni/src/lib/hlu/DataItem.h
                 ni/src/lib/hlu/ncarg/hlu/DataItemP.h:ni/src/lib/hlu/DataItemP.h
                 ni/src/lib/hlu/ncarg/hlu/DataMgrF.h:ni/src/lib/hlu/DataMgrF.h
                 ni/src/lib/hlu/ncarg/hlu/DataMgr.h:ni/src/lib/hlu/DataMgr.h
                 ni/src/lib/hlu/ncarg/hlu/DataMgrP.h:ni/src/lib/hlu/DataMgrP.h
                 ni/src/lib/hlu/ncarg/hlu/defs.h:ni/src/lib/hlu/defs.h
                 ni/src/lib/hlu/ncarg/hlu/Error.h:ni/src/lib/hlu/Error.h
                 ni/src/lib/hlu/ncarg/hlu/ErrorI.h:ni/src/lib/hlu/ErrorI.h
                 ni/src/lib/hlu/ncarg/hlu/ErrorP.h:ni/src/lib/hlu/ErrorP.h
                 ni/src/lib/hlu/ncarg/hlu/FormatI.h:ni/src/lib/hlu/FormatI.h
                 ni/src/lib/hlu/ncarg/hlu/FortranP.h:ni/src/lib/hlu/FortranP.h
                 ni/src/lib/hlu/ncarg/hlu/GraphicStyle.h:ni/src/lib/hlu/GraphicStyle.h
                 ni/src/lib/hlu/ncarg/hlu/GraphicStyleP.h:ni/src/lib/hlu/GraphicStyleP.h
                 ni/src/lib/hlu/ncarg/hlu/hlu.h:ni/src/lib/hlu/hlu.h
                 ni/src/lib/hlu/ncarg/hlu/hluP.h:ni/src/lib/hlu/hluP.h
                 ni/src/lib/hlu/ncarg/hlu/hluutil.h:ni/src/lib/hlu/hluutil.h
                 ni/src/lib/hlu/ncarg/hlu/ImageWorkstation.h:ni/src/lib/hlu/ImageWorkstation.h
                 ni/src/lib/hlu/ncarg/hlu/ImageWorkstationP.h:ni/src/lib/hlu/ImageWorkstationP.h
                 ni/src/lib/hlu/ncarg/hlu/IrregularPlot.h:ni/src/lib/hlu/IrregularPlot.h
                 ni/src/lib/hlu/ncarg/hlu/IrregularPlotP.h:ni/src/lib/hlu/IrregularPlotP.h
                 ni/src/lib/hlu/ncarg/hlu/IrregularTransObj.h:ni/src/lib/hlu/IrregularTransObj.h
                 ni/src/lib/hlu/ncarg/hlu/IrregularTransObjP.h:ni/src/lib/hlu/IrregularTransObjP.h
                 ni/src/lib/hlu/ncarg/hlu/IrregularType2TransObj.h:ni/src/lib/hlu/IrregularType2TransObj.h
                 ni/src/lib/hlu/ncarg/hlu/IrregularType2TransObjP.h:ni/src/lib/hlu/IrregularType2TransObjP.h
                 ni/src/lib/hlu/ncarg/hlu/LabelBar.h:ni/src/lib/hlu/LabelBar.h
                 ni/src/lib/hlu/ncarg/hlu/LabelBarP.h:ni/src/lib/hlu/LabelBarP.h
                 ni/src/lib/hlu/ncarg/hlu/Legend.h:ni/src/lib/hlu/Legend.h
                 ni/src/lib/hlu/ncarg/hlu/LegendP.h:ni/src/lib/hlu/LegendP.h
                 ni/src/lib/hlu/ncarg/hlu/LogLinPlot.h:ni/src/lib/hlu/LogLinPlot.h
                 ni/src/lib/hlu/ncarg/hlu/LogLinPlotP.h:ni/src/lib/hlu/LogLinPlotP.h
                 ni/src/lib/hlu/ncarg/hlu/LogLinTransObj.h:ni/src/lib/hlu/LogLinTransObj.h
                 ni/src/lib/hlu/ncarg/hlu/LogLinTransObjP.h:ni/src/lib/hlu/LogLinTransObjP.h
                 ni/src/lib/hlu/ncarg/hlu/MapDataHandler.h:ni/src/lib/hlu/MapDataHandler.h
                 ni/src/lib/hlu/ncarg/hlu/MapDataHandlerP.h:ni/src/lib/hlu/MapDataHandlerP.h
                 ni/src/lib/hlu/ncarg/hlu/MapPlot.h:ni/src/lib/hlu/MapPlot.h
                 ni/src/lib/hlu/ncarg/hlu/MapPlotP.h:ni/src/lib/hlu/MapPlotP.h
                 ni/src/lib/hlu/ncarg/hlu/MapRGDataHandler.h:ni/src/lib/hlu/MapRGDataHandler.h
                 ni/src/lib/hlu/ncarg/hlu/MapRGDataHandlerP.h:ni/src/lib/hlu/MapRGDataHandlerP.h
                 ni/src/lib/hlu/ncarg/hlu/MapTransObj.h:ni/src/lib/hlu/MapTransObj.h
                 ni/src/lib/hlu/ncarg/hlu/MapTransObjP.h:ni/src/lib/hlu/MapTransObjP.h
                 ni/src/lib/hlu/ncarg/hlu/MapV40DataHandler.h:ni/src/lib/hlu/MapV40DataHandler.h
                 ni/src/lib/hlu/ncarg/hlu/MapV40DataHandlerP.h:ni/src/lib/hlu/MapV40DataHandlerP.h
                 ni/src/lib/hlu/ncarg/hlu/MapV41DataHandler.h:ni/src/lib/hlu/MapV41DataHandler.h
                 ni/src/lib/hlu/ncarg/hlu/MapV41DataHandlerP.h:ni/src/lib/hlu/MapV41DataHandlerP.h
                 ni/src/lib/hlu/ncarg/hlu/MeshScalarField.h:ni/src/lib/hlu/MeshScalarField.h
                 ni/src/lib/hlu/ncarg/hlu/MeshScalarFieldP.h:ni/src/lib/hlu/MeshScalarFieldP.h
                 ni/src/lib/hlu/ncarg/hlu/MultiText.h:ni/src/lib/hlu/MultiText.h
                 ni/src/lib/hlu/ncarg/hlu/MultiTextP.h:ni/src/lib/hlu/MultiTextP.h
                 ni/src/lib/hlu/ncarg/hlu/NcgmWorkstation.h:ni/src/lib/hlu/NcgmWorkstation.h
                 ni/src/lib/hlu/ncarg/hlu/NcgmWorkstationP.h:ni/src/lib/hlu/NcgmWorkstationP.h
                 ni/src/lib/hlu/ncarg/hlu/NgSizeT.h:ni/src/lib/hlu/NgSizeT.h
                 ni/src/lib/hlu/ncarg/hlu/NresDB.h:ni/src/lib/hlu/NresDB.h
                 ni/src/lib/hlu/ncarg/hlu/pageutil.h:ni/src/lib/hlu/pageutil.h
                 ni/src/lib/hlu/ncarg/hlu/PDFWorkstation.h:ni/src/lib/hlu/PDFWorkstation.h
                 ni/src/lib/hlu/ncarg/hlu/PDFWorkstationP.h:ni/src/lib/hlu/PDFWorkstationP.h
                 ni/src/lib/hlu/ncarg/hlu/PlotManager.h:ni/src/lib/hlu/PlotManager.h
                 ni/src/lib/hlu/ncarg/hlu/PlotManagerI.h:ni/src/lib/hlu/PlotManagerI.h
                 ni/src/lib/hlu/ncarg/hlu/PlotManagerP.h:ni/src/lib/hlu/PlotManagerP.h
                 ni/src/lib/hlu/ncarg/hlu/Primitive.h:ni/src/lib/hlu/Primitive.h
                 ni/src/lib/hlu/ncarg/hlu/PrimitiveP.h:ni/src/lib/hlu/PrimitiveP.h
                 ni/src/lib/hlu/ncarg/hlu/PSWorkstation.h:ni/src/lib/hlu/PSWorkstation.h
                 ni/src/lib/hlu/ncarg/hlu/PSWorkstationP.h:ni/src/lib/hlu/PSWorkstationP.h
                 ni/src/lib/hlu/ncarg/hlu/ResList.h:ni/src/lib/hlu/ResList.h
                 ni/src/lib/hlu/ncarg/hlu/ResListP.h:ni/src/lib/hlu/ResListP.h
                 ni/src/lib/hlu/ncarg/hlu/ResourcesP.h:ni/src/lib/hlu/ResourcesP.h
                 ni/src/lib/hlu/ncarg/hlu/ScalarFieldFloatP.h:ni/src/lib/hlu/ScalarFieldFloatP.h
                 ni/src/lib/hlu/ncarg/hlu/ScalarField.h:ni/src/lib/hlu/ScalarField.h
                 ni/src/lib/hlu/ncarg/hlu/ScalarFieldP.h:ni/src/lib/hlu/ScalarFieldP.h
                 ni/src/lib/hlu/ncarg/hlu/Segments.h:ni/src/lib/hlu/Segments.h
                 ni/src/lib/hlu/ncarg/hlu/SphericalGeometryP.h:ni/src/lib/hlu/SphericalGeometryP.h
                 ni/src/lib/hlu/ncarg/hlu/SphericalTransObj.h:ni/src/lib/hlu/SphericalTransObj.h
                 ni/src/lib/hlu/ncarg/hlu/SphericalTransObjP.h:ni/src/lib/hlu/SphericalTransObjP.h
                 ni/src/lib/hlu/ncarg/hlu/StreamlinePlot.h:ni/src/lib/hlu/StreamlinePlot.h
                 ni/src/lib/hlu/ncarg/hlu/StreamlinePlotP.h:ni/src/lib/hlu/StreamlinePlotP.h
                 ni/src/lib/hlu/ncarg/hlu/Style.h:ni/src/lib/hlu/Style.h
                 ni/src/lib/hlu/ncarg/hlu/StyleP.h:ni/src/lib/hlu/StyleP.h
                 ni/src/lib/hlu/ncarg/hlu/SubStylesP.h:ni/src/lib/hlu/SubStylesP.h
                 ni/src/lib/hlu/ncarg/hlu/TextItem.h:ni/src/lib/hlu/TextItem.h
                 ni/src/lib/hlu/ncarg/hlu/TextItemP.h:ni/src/lib/hlu/TextItemP.h
                 ni/src/lib/hlu/ncarg/hlu/TickMark.h:ni/src/lib/hlu/TickMark.h
                 ni/src/lib/hlu/ncarg/hlu/TickMarkP.h:ni/src/lib/hlu/TickMarkP.h
                 ni/src/lib/hlu/ncarg/hlu/Title.h:ni/src/lib/hlu/Title.h
                 ni/src/lib/hlu/ncarg/hlu/TitleP.h:ni/src/lib/hlu/TitleP.h
                 ni/src/lib/hlu/ncarg/hlu/Transform.h:ni/src/lib/hlu/Transform.h
                 ni/src/lib/hlu/ncarg/hlu/TransformI.h:ni/src/lib/hlu/TransformI.h
                 ni/src/lib/hlu/ncarg/hlu/TransformP.h:ni/src/lib/hlu/TransformP.h
                 ni/src/lib/hlu/ncarg/hlu/TransObj.h:ni/src/lib/hlu/TransObj.h
                 ni/src/lib/hlu/ncarg/hlu/TransObjI.h:ni/src/lib/hlu/TransObjI.h
                 ni/src/lib/hlu/ncarg/hlu/TransObjP.h:ni/src/lib/hlu/TransObjP.h
                 ni/src/lib/hlu/ncarg/hlu/TriMeshTransObj.h:ni/src/lib/hlu/TriMeshTransObj.h
                 ni/src/lib/hlu/ncarg/hlu/TriMeshTransObjP.h:ni/src/lib/hlu/TriMeshTransObjP.h
                 ni/src/lib/hlu/ncarg/hlu/VarArg.h:ni/src/lib/hlu/VarArg.h
                 ni/src/lib/hlu/ncarg/hlu/VecAnno.h:ni/src/lib/hlu/VecAnno.h
                 ni/src/lib/hlu/ncarg/hlu/VecAnnoP.h:ni/src/lib/hlu/VecAnnoP.h
                 ni/src/lib/hlu/ncarg/hlu/VectorFieldFloatP.h:ni/src/lib/hlu/VectorFieldFloatP.h
                 ni/src/lib/hlu/ncarg/hlu/VectorField.h:ni/src/lib/hlu/VectorField.h
                 ni/src/lib/hlu/ncarg/hlu/VectorFieldP.h:ni/src/lib/hlu/VectorFieldP.h
                 ni/src/lib/hlu/ncarg/hlu/VectorPlot.h:ni/src/lib/hlu/VectorPlot.h
                 ni/src/lib/hlu/ncarg/hlu/VectorPlotP.h:ni/src/lib/hlu/VectorPlotP.h
                 ni/src/lib/hlu/ncarg/hlu/View.h:ni/src/lib/hlu/View.h
                 ni/src/lib/hlu/ncarg/hlu/ViewI.h:ni/src/lib/hlu/ViewI.h
                 ni/src/lib/hlu/ncarg/hlu/ViewP.h:ni/src/lib/hlu/ViewP.h
                 ni/src/lib/hlu/ncarg/hlu/Workspace.h:ni/src/lib/hlu/Workspace.h
                 ni/src/lib/hlu/ncarg/hlu/WorkspaceI.h:ni/src/lib/hlu/WorkspaceI.h
                 ni/src/lib/hlu/ncarg/hlu/WorkspaceP.h:ni/src/lib/hlu/WorkspaceP.h
                 ni/src/lib/hlu/ncarg/hlu/Workstation.h:ni/src/lib/hlu/Workstation.h
                 ni/src/lib/hlu/ncarg/hlu/WorkstationI.h:ni/src/lib/hlu/WorkstationI.h
                 ni/src/lib/hlu/ncarg/hlu/WorkstationP.h:ni/src/lib/hlu/WorkstationP.h
                 ni/src/lib/hlu/ncarg/hlu/XWorkstation.h:ni/src/lib/hlu/XWorkstation.h
                 ni/src/lib/hlu/ncarg/hlu/XWorkstationP.h:ni/src/lib/hlu/XWorkstationP.h
                 ni/src/lib/hlu/ncarg/hlu/XyPlot.h:ni/src/lib/hlu/XyPlot.h
                 ni/src/lib/hlu/ncarg/hlu/XyPlotP.h:ni/src/lib/hlu/XyPlotP.h
                 ni/src/lib/nfpfort/Makefile
                 ni/src/lib/nio/Makefile
                 ni/src/lib/nfp/Makefile
                 ni/src/ncl/Makefile
                 ni/src/nyacc/Makefile
                 ni/src/scripts/Makefile
                 ni/src/examples/Makefile
                 ni/src/db/Makefile
                 ni/src/Makefile
                 ni/src/mkwrap/Makefile
                 ni/src/contrib/Makefile
                 ni/Makefile
                 ])
AC_OUTPUT()

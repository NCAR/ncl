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

# Make sure to check for Xlibs in this old directory.
AC_CHECK_FILE([/usr/X11R6/lib], [LDFLAGS="-L/usr/X11R6/lib"])
AC_CHECK_LIB([X11], [XOpenDisplay])
AC_CHECK_LIB([Xext], [XShmAttach])

# Find required libraries.
AC_CHECK_LIB([m], [floor], [], [AC_MSG_ERROR([math library is required])], [])
AC_CHECK_LIB([netcdf], [nc_create], [], [AC_MSG_ERROR([NetCDF library is required])], [])
AC_CHECK_LIB([jpeg], [jpeg_write_marker], [], [AC_MSG_ERROR([JPEG library is required])], [])
AC_CHECK_LIB([z], [deflate], [], [AC_MSG_ERROR([zlib library is required])], [])
AC_CHECK_LIB([cairo], [cairo_in_fill], [], [AC_MSG_ERROR([cairo library is required])], [])
AC_CHECK_LIB([png], [png_access_version_number], [], [AC_MSG_ERROR([PNG library is required])], [])
AC_CHECK_LIB([freetype], [FT_Init_FreeType], [], [AC_MSG_ERROR([Freetype2 library is required])], [])
AC_CHECK_LIB([pixman-1], [pixman_blt], [], [AC_MSG_ERROR([Freetype2 library is required])], [])
AC_CHECK_LIB([udunits2], [ut_compare], [], [AC_MSG_ERROR([UDUNITS2 library is required])], [])
#AC_CHECK_LIB([sz], [SZ_encoder_enabled], [], [AC_MSG_ERROR([szlib library is required])], [])

# HDF4 use is optional.
use_hdf4=yes
AC_CHECK_LIB([df], [Hclose], [], [use_hdf4=no])
AC_CHECK_LIB([mfhdf], [SDcreate], [], [use_hdf4=no])
AC_MSG_CHECKING([whether HDF4 will be used])
AC_MSG_RESULT($use_hdf4)

# HDF-EOS library is optional. HDF4 is required for HDF-EOS.
use_hdfeos=no
if test $use_hdf4 = yes; then
   use_hdfeos=yes
   AC_CHECK_LIB([hdfeos], [GDcreate], [], [use_hdfeos=no])
   AC_CHECK_LIB([Gctp], [gctp], [], [use_hdfeos=no])
fi
AC_MSG_CHECKING([whether HDF-EOS will be used])
AC_MSG_RESULT($use_hdfeos)

# GRIB2 use is optional. Check for jasper and grib2c libraries.
use_grib2=yes
AC_CHECK_LIB([jasper], [jas_memdump], [], [use_grib2=no])
AC_CHECK_LIB([grib2c], [getgridindex], [], [use_grib2=no])
AC_MSG_CHECKING([whether GRIB2 will be used])
AC_MSG_RESULT($use_grib2)

# Map project library PROJ is optional. Check for it.
use_proj4=yes
AC_CHECK_LIB([proj], [pj_laea], [], [use_proj4=no])
AC_MSG_CHECKING([whether PROJ4 will be used])
AC_MSG_RESULT($use_proj4)

# GDAL library is optional. Check for it.
use_gdal=yes
AC_CHECK_LIB([gdal], [OGR_G_AddPoint], [], [use_gdal=no])
AC_MSG_CHECKING([whether GDAL will be used])
AC_MSG_RESULT($use_gdal)

# HDF5 library is optional. Check for it.
use_hdf5=yes
AC_CHECK_LIB([hdf5], [H5Fflush], [], [use_hdf5=no])
AC_MSG_CHECKING([whether HDF5 will be used])
AC_MSG_RESULT($use_hdf5)

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
AC_DEFINE([NCLVERSION], ["6.5.0"])

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

# These files will be created when the configure script is run.
AC_CONFIG_FILES([Makefile
                 common/Makefile
                 common/src/Makefile
                 common/src/libncarg_c/Makefile
                 common/src/libncarg_c/c.h
                 common/src/libncarg_c/ncarg_path.c
                 common/src/bin/Makefile
                 common/src/bin/ncargpath/Makefile
                 common/src/fontcap/Makefile
                 common/src/graphcap/Makefile
                 ncarg2d/Makefile
                 ncarg2d/man/Makefile
                 ncarg2d/src/Makefile
                 ncarg2d/src/libncarg/Makefile
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
                 ])
AC_OUTPUT()

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

AC_CONFIG_HEADERS([config.h])

# Find the C compiler.
AC_PROG_CC
AM_PROG_CC_C_O
AC_C_CONST

# Find the install program.
AC_PROG_INSTALL

# Check to see if any macros must be set to enable large (>2GB) files.
AC_SYS_LARGEFILE

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

# These files will be created when the configure script is run.
AC_CONFIG_FILES([Makefile
                 ncarg2d/Makefile
                 ncarg2d/src/Makefile
                 ncarg2d/man/Makefile
                 ])
AC_OUTPUT()

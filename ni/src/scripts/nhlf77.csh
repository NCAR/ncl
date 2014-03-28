#!/bin/csh -f
#
#	$Id: nhlf77.csh,v 1.6 2010-04-02 17:52:38 haley Exp $
#

#*********************************************#
#                                             #
# Make sure NCARG_ROOT is set for this script #
#                                             #
#*********************************************#
setenv NCARG_ROOT  `ncargpath root`

if ($status != 0) then
	exit 1
endif

set xlibs = "SED_XLIB"
set cairolib = "SED_CAIROLIBUSER"
set system   = "SED_SYSTEM_INCLUDE"
set f77       = "SED_FC"
set loadflags  = "SED_LDFFLAGS"
set libdir   = `ncargpath SED_LIBDIR`
set incdir   = `ncargpath SED_INCDIR`
set syslibdir = "SED_LIBSEARCHUSER"
set sysincdir = "SED_INCSEARCHUSER"
set ro       = "$libdir/SED_NCARGDIR/SED_ROBJDIR"
set newargv = "$f77 $loadflags"
set libpath = "-L$libdir $syslibdir"
set incpath = "-I$incdir $sysincdir"

#
# set up default libraries
#
set libncarg    = "-lncarg"
set libgks      = "-lSED_LIBNCARG_GKS"
set libmath     = ""
set libncarg_c  = "-lncarg_c"
set libhlu      = "-lSED_LIBHLU"
set ncarbd      = "$ro/libncarbd.o"
set ngmathbd    = "$ro/libngmathbd.o"
set extra_libs

set robjs
unset NGMATH_LD
unset NGMATH_BLOCKD_LD

foreach arg ($argv)
  switch ($arg)

  case "-ngmath":
    set libmath     = "-lngmath"
    breaksw

  case "-ncarbd":
    set robjs = "$robjs $ncarbd"
    set NGMATH_BLOCKD_LD
    breaksw

  case "-ngmathbd":
    set robjs = "$robjs $ngmathbd"
# Make sure the ngmath blockdata routine doesn't get loaded twice.
    unset NGMATH_BLOCKD_LD
    breaksw

  case "-netcdf":
  case "-cdf":
    set extra_libs = "$extra_libs SED_NCDFLIBS"
    breaksw

  case "-hdf":
    set extra_libs = "$extra_libs SED_HDFLIB"
    breaksw

  default:
    set newargv = "$newargv $arg"
  endsw
end

#
# If -ncarbd was set, *and* the ngmath library was loaded,
# then automatically take care of loading libngmathbd.o.
#
if ($?NGMATH_LD && $?NGMATH_BLOCKD_LD) then
  set robjs = "$robjs $ngmathbd"
endif

set ncarg_libs = "$libhlu $libncarg $libgks $libncarg_c $libmath"
set newargv = "$newargv $libpath $incpath $extra_libs $robjs $ncarg_libs $xlibs $cairolib"


echo $newargv
eval $newargv

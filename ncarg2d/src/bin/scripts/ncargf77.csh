#!/bin/csh -f
#
#   $Id: ncargf77.csh,v 1.13 1994-02-23 22:41:43 haley Exp $
#

#********************#
#                    #
#   NCARGF77 USAGE   #
#                    #
#********************#
if ($#argv < 1) then
  echo "usage: ncargf77 [-smooth] [-quick] [-super] [-agupwrtx] [-ictrans]"
  echo "                [-noX11] [Fortran 77 options] ... filename        "
  echo ""
  echo "See <man ncargf77>                                                "
  exit
endif

#*********************************************#
#                                             #
# Make sure NCARG_ROOT is set for this script #
#                                             #
#*********************************************#
setenv NCARG_ROOT  `ncargpath root`

set XLIBPATH = ""
set system   = "SED_SYSTEM_INCLUDE"
set fortran  = "SED_F77"
set libdir   = `ncargpath SED_LIBDIR`
set ro       = "$libdir/SED_NCARGDIR/SED_ROBJDIR"
set loadopts = ""
set libextra = ""

if (! -d "$libdir") then
  echo "Library directory <$libdir> does not exist."
  exit 1
endif

#
# Set up special cases
#
if ("$system" == "Sun3") then
  set loadopts = "-fswitch"
else if ("$system" == "Sun4Solaris") then
  set libextra = "/usr/ucblib/libucb.a"
endif    

set newargv = "$fortran $loadopts"

set ctrans_libs = ""
set stub_file   = ""

#
# set up default libraries
#
set libncarg  =  "$libdir/libncarg.a"
set libgks     = "$libdir/libncarg_gks.a"
set liblocal   = "$libdir/libncarg_loc.a"
set libncarg_c = "$libdir/libncarg_c.a"

set libmath  = "-lm"
set libX11     = "$XLIBPATH -lX11"

set smooth = "$ro/libdashsmth.o"
set quick  = "$ro/libdashline.o $ro/libconrcqck.o $ro/libconraq.o"
set super  = "$ro/libdashsupr.o $ro/libconrcspr.o $ro/libconras.o"

set libs

foreach arg ($argv)

    switch ($arg)

    case "-sungks":
        echo "Using Sun GKS"
        set libgks="-lgks77 -lgks -lsuntool -lsunwindow -lpixrect -lm"
        breaksw

    case "-smooth":
        echo "Smooth f77 of NCAR Graphics"
        set libs = "$libs $smooth"
        breaksw

    case "-super":
        echo "Super f77 of NCAR Graphics"
        set libs = "$libs $super"
        breaksw

    case "-quick":
        echo "Quick f77 of NCAR Graphics"
        set libs = "$libs $quick"
        breaksw

    case "-agupwrtx":
        echo "Autograph with PWRITX"
        set libs = "$libs $ro/libagupwrtx.o"
        breaksw

    case "-conransmooth":
        echo "Smooth Conran"
        set libs = "$libs $smooth"
        breaksw

    case "-conranquick":
        echo "Quick Conran"
        set libs = "$libs $ro/libconraq.o"
        breaksw

    case "-conransuper":
        echo "Super Conran"
        set libs = "$libs $ro/libconras.o $ro/libdashsupr.o"
        breaksw

    case "-conrecsmooth":
        echo "Smooth Conrec"
        set libs = "$libs $ro/libdashsmth.o"
        breaksw

    case "-conrecquick":
        echo "Quick Conrec"
        set libs = "$libs $ro/libconrcqck.o"
        breaksw

    case "-conrecsuper":
        echo "Super Conrec"
        set libs = "$libs $ro/libconrcspr.o $ro/libdashsupr.o"
        breaksw

    case "-dashsmooth":
        echo "Smooth Dash"
        set libs = "$libs $ro/libdashsmth.o"
        breaksw

    case "-dashquick":
    case "-dashline":
        echo "Quick Dash"
        set libs = "$libs $ro/libdashline.o"
        breaksw

    case "-dashsuper":
        echo "Super Dash"
        set libs = "$libs $ro/libdashsupr.o"
        breaksw

    case "-dashchar":
        echo "Normal Dash"
        breaksw

    case "-ictrans":
        echo "Output to ictrans"
        set ctrans_libs = `ctlib`
        set libX11   = ""
        set libmath  = ""
        set stub_file = $ro/ggkwdr_stub.o
        if ("$system" == "Sun4") then
            set libextra = "-L/usr/lang/SC1.0/ansi_lib -lansi"
        endif
        breaksw

    case "-noX11"
        set stub_file = $ro/ggkwdr_stub.o
        set libX11 = ""
        breaksw

    case "-*":
        set newargv = "$newargv $arg"
        breaksw

    default:
        set newargv = "$newargv $arg"
        breaksw

    endsw
end

set newargv = "$newargv $stub_file $ctrans_libs $libs $libncarg $libgks $libncarg_c $liblocal $libX11 $libmath $libextra"

echo $newargv
eval $newargv

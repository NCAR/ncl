#!/bin/csh -f
#
#   $Id: ncargf77.csh,v 1.24 1995-05-22 19:54:04 haley Exp $
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

set syslibdir = "SED_LIBSEARCH"
set xlib     = "SED_XLIB"
set system   = "SED_SYSTEM_INCLUDE"
set fortran  = "SED_F77"
set loadflags  = "SED_LDFLAGS"
set libdir   = `ncargpath SED_LIBDIR`
set ro       = "$libdir/SED_NCARGDIR/SED_ROBJDIR"
set libpath = "-L$libdir $syslibdir"

set libextra = ""

if (! -d "$libdir") then
  echo "Library directory <$libdir> does not exist."
  exit 1
endif

set newargv = "$fortran $loadflags"

set ctrans_libs = ""
set stub_file   = ""

#
# set up default libraries
#
set libncarg  =  "-lncarg"
set libgks     = "-lncarg_gks"
set libncarg_c = "-lncarg_c"
set ncarg_libs  = "$libncarg $libgks $libncarg_c"

set libmath  = "-lm"

set smooth = "$ro/libdashsmth.o"
set quick  = "$ro/libdashline.o $ro/libconrcqck.o $ro/libconraq.o"
set super  = "$ro/libdashsupr.o $ro/libconrcspr.o $ro/libconras.o"

set robjs

foreach arg ($argv)

    switch ($arg)

    case "-sungks":
        echo "Using Sun GKS"
        set libgks="-lgks77 -lgks -lsuntool -lsunwindow -lpixrect -lm"
        breaksw

    case "-smooth":
        echo "Smooth f77 of NCAR Graphics"
        set robjs = "$robjs $smooth"
        breaksw

    case "-super":
        echo "Super f77 of NCAR Graphics"
        set robjs = "$robjs $super"
        breaksw

    case "-quick":
        echo "Quick f77 of NCAR Graphics"
        set robjs = "$robjs $quick"
        breaksw

    case "-agupwrtx":
        echo "Autograph with PWRITX"
        set robjs = "$robjs $ro/libagupwrtx.o"
        breaksw

    case "-conransmooth":
        echo "Smooth Conran"
        set robjs = "$robjs $smooth"
        breaksw

    case "-conranquick":
        echo "Quick Conran"
        set robjs = "$robjs $ro/libconraq.o"
        breaksw

    case "-conransuper":
        echo "Super Conran"
        set robjs = "$robjs $ro/libconras.o $ro/libdashsupr.o"
        breaksw

    case "-conrecsmooth":
        echo "Smooth Conrec"
        set robjs = "$robjs $ro/libdashsmth.o"
        breaksw

    case "-conrecquick":
        echo "Quick Conrec"
        set robjs = "$robjs $ro/libconrcqck.o"
        breaksw

    case "-conrecsuper":
        echo "Super Conrec"
        set robjs = "$robjs $ro/libconrcspr.o $ro/libdashsupr.o"
        breaksw

    case "-dashsmooth":
        echo "Smooth Dash"
        set robjs = "$robjs $ro/libdashsmth.o"
        breaksw

    case "-dashquick":
    case "-dashline":
        echo "Quick Dash"
        set robjs = "$robjs $ro/libdashline.o"
        breaksw

    case "-dashsuper":
        echo "Super Dash"
        set robjs = "$robjs $ro/libdashsupr.o"
        breaksw

    case "-dashchar":
        echo "Normal Dash"
        breaksw

    case "-ictrans":
        echo "Output to ictrans"
        set ctrans_libs = `ctlib`
        set xlib   = ""
        set libmath  = ""
        set stub_file = $ro/ggkwdr_stub.o
        breaksw

    case "-noX11"
        set stub_file = $ro/ggkwdr_stub.o
        set xlib = ""
        breaksw

    case "-*":
        set newargv = "$newargv $arg"
        breaksw

    default:
        set newargv = "$newargv $arg"
        breaksw

    endsw
end

set newargv = "$newargv $stub_file $libpath $ctrans_libs $robjs $ncarg_libs $xlib $libmath $libextra"

echo $newargv
eval $newargv

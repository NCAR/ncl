#!/bin/csh -f
#
#   $Id: ncargf77.csh,v 1.15 1994-03-24 15:05:07 haley Exp $
#

#*********************************************#
#                                             #
# Make sure NCARG_ROOT is set for this script #
#                                             #
#*********************************************#
setenv NCARG_ROOT  `ncargpath root`

set XLIBPATH = ""
set system   = "SED_SYSTEM_INCLUDE"
set fortran  = "SED_F77"
set loadopt  = "SED_LDFLAGS"
set libdir   = `ncargpath SED_LIBDIR`
set ro       = "$libdir/SED_NCARGDIR/SED_ROBJDIR"
set libextra = ""

if (! -d "$libdir") then
  echo "Library directory <$libdir> does not exist."
  exit 1
endif

if ("$system" == "Sun4Solaris") then
  set libextra = "/usr/ucblib/libucb.a"
endif    

set newargv = "$fortran $loadopt"

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

if ("$system" == "AlphaOSF1") then
set smooth = "$ro/dashsmth/*.o"
set quick  = "$ro/dashline/*.o $ro/conrcqck/*.o $ro/conraq/*.o"
set super  = "$ro/dashsupr/*.o $ro/conrcspr/*.o $ro/conras/*.o"
else 
set smooth = "$ro/libdashsmth.o"
set quick  = "$ro/libdashline.o $ro/libconrcqck.o $ro/libconraq.o"
set super  = "$ro/libdashsupr.o $ro/libconrcspr.o $ro/libconras.o"
endif

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
        if ($system == "AlphaOSF1") then
            set libs = "$libs $ro/agupwrtx/*.o"
        else
            set libs = "$libs $ro/libagupwrtx.o"
        endif
        breaksw

    case "-conransmooth":
        echo "Smooth Conran"
        set libs = "$libs $smooth"
        breaksw

    case "-conranquick":
        echo "Quick Conran"
        if ($system == "AlphaOSF1") then
            set libs = "$libs $ro/conraq/*.o"
        else
            set libs = "$libs $ro/libconraq.o"
        endif
        breaksw

    case "-conransuper":
        echo "Super Conran"
        if ($system == "AlphaOSF1") then
            set libs = "$libs $ro/conras/*.o $ro/dashsupr/*.o"
        else
            set libs = "$libs $ro/libconras.o $ro/libdashsupr.o"
        endif
        breaksw

    case "-conrecsmooth":
        echo "Smooth Conrec"
        if ($system == "AlphaOSF1") then
            set libs = "$libs $ro/dashsmth/*.o"
        else
            set libs = "$libs $ro/libdashsmth.o"
        endif
        breaksw

    case "-conrecquick":
        echo "Quick Conrec"
        if ($system == "AlphaOSF1") then
            set libs = "$libs $ro/conrcqck/*.o"
        else
            set libs = "$libs $ro/libconrcqck.o"
        endif
        breaksw

    case "-conrecsuper":
        echo "Super Conrec"
        if ($system == "AlphaOSF1") then
            set libs = "$libs $ro/conrcspr/*.o $ro/dashsupr/*.o"
        else
            set libs = "$libs $ro/libconrcspr.o $ro/libdashsupr.o"
        endif
        breaksw

    case "-dashsmooth":
        echo "Smooth Dash"
        if ($system == "AlphaOSF1") then
            set libs = "$libs $ro/dashsmth/*.o"
        else
            set libs = "$libs $ro/libdashsmth.o"
        endif
        breaksw

    case "-dashquick":
    case "-dashline":
        echo "Quick Dash"
        if ($system == "AlphaOSF1") then
            set libs = "$libs $ro/dashline/*.o"
        else
            set libs = "$libs $ro/libdashline.o"
        endif
        breaksw

    case "-dashsuper":
        echo "Super Dash"
        if ($system == "AlphaOSF1") then
             set libs = "$libs $ro/dashsupr/*.o"
        else
            set libs = "$libs $ro/libdashsupr.o"
        endif
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

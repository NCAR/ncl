#!/bin/csh -f
#
#	$Id: ncargf77.csh,v 1.4 1993-01-13 16:50:23 haley Exp $
#

set system="SED_SYSTEM_INCLUDE"
set fortran="SED_F77"
set loadopts = "SED_LD_FFLAGS"
set l = `ncargpath SED_LIBDIR`
if ($status != 0) then
        exit 1
endif

if (! -d "$l") then
  echo "Library directory <$l> does not exist."
  exit 1
endif

set newargv = "$fortran $loadopts"

set ctrans_libs = ""

#
# set up default libraries
#
if ("$system" == "Ardent" || "$system" == "AIX370") then
  set libncarg  =  "$l/libncarbd.o $l/libncarg.a"
else
  set libncarg  =  "$l/libncarg.a"
endif
set libgks     = "$l/libncarg_gks.a"
set liblocal   = "$l/libncarg_loc.a"
set libncarg_c = "$l/libncarg_c.a"

set lib_extern = "-lX11 -lm"

set smooth = "$l/libdashsmth.o"
set quick = "$l/libdashline.o $l/libconrcqck.o $l/libconraq.o"
set super = "$l/libdashsupr.o $l/libconrcspr.o $l/libconras.o"

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
        set libs = "$libs $l/libagupwrtx.o"
        breaksw

    case "-conransmooth":
        echo "Smooth Conran"
        set libs = "$libs $smooth"
        breaksw

    case "-conranquick":
        echo "Quick Conran"
        set libs = "$libs $l/libconraq.o"
        breaksw

    case "-conransuper":
        echo "Super Conran"
        set libs = "$libs $l/libconras.o $l/libdashsupr.o"
        breaksw

    case "-conrecsmooth":
        echo "Smooth Conrec"
        set libs = "$libs $l/libdashsmth.o"
        breaksw

    case "-conrecquick":
        echo "Quick Conrec"
        set libs = "$libs $l/libconrcqck.o"
        breaksw

    case "-conrecsuper":
        echo "Super Conrec"
        set libs = "$libs $l/libconrcspr.o $l/libdashsupr.o"
        breaksw

    case "-dashsmooth":
        echo "Smooth Dash"
        set libs = "$libs $l/libdashsmth.o"
        breaksw

    case "-dashquick":
    case "-dashline":
        echo "Quick Dash"
        set libs = "$libs $l/libdashline.o"
        breaksw

    case "-dashsuper":
        echo "Super Dash"
        set libs = "$libs $l/libdashsupr.o"
        breaksw

    case "-dashchar":
        echo "Normal Dash"
        breaksw

    case "-ictrans"
        echo "Output to ictrans"
        set ctrans_libs = `ctlib`
        breaksw

    case "-*":
        set newargv = "$newargv $arg"
        breaksw

    default:
        set newargv = "$newargv $arg"
        breaksw

  endsw
end

set newargv = "$newargv $ctrans_libs $libs $libncarg $libgks $libncarg_c $liblocal $lib_extern"

echo $newargv
eval $newargv

#!/bin/csh -f
#
#	$Id: ncargcc.csh,v 1.10 1992-12-08 23:00:17 haley Exp $
#

set system="SED_SYSTEM_INCLUDE"
set cc="SED_CC"
set loadopts = "SED_LD_CFLAGS"
set l = `ncargpath SED_LIBDIR`
if ($status != 0) then
        exit 1
endif

if (! -d "$l") then
  echo "Library directory <$l> does not exist."
  exit 1
endif

set newargv = "$cc $loadopts"

set ctrans_libs = ""

# set up default libraries

if ("$system" == "Ardent" || "$system" == "AIX370") then
  set libncarg    =       "$l/libncarbd.o $l/libncarg.a"
else
  set libncarg    =       "$l/libncarg.a"
endif

set libgks	=	"$l/libncarg_gks.a"
set liblocal	=	"$l/libncarg_loc.a"
set libcbind    = "$l/libncargC.a $l/libncarg_gksC.a"

if ($system == "Cray2" || $system == "Cray") then
  set f77libs     =       "-L/lib -lf -lio -lm -lp -lsci -lu -lc"
else if ($system == "Sun4") then
  set f77libs     =       "-Bstatic -L/usr/lang/SC1.0 -lF77 -lV77 -lm"
else if ($system == "Sun3") then
  set f77libs     =       "-L/usr/lang/SC1.0 -lF77 -lV77 /usr/lib/fswitch/libm.a"
else if ($system == "RS6000") then
  set f77libs     =       "-lm -lxlf"
else if ($system == "DECRISC") then
  set f77libs     =       "-lots -lfor -lF77 -lI77 -lU77 -lutil -li -lm -lUfor"
else if ($system == "HPUX") then
  set f77libs     =       "-lf -lm"
else if ($system == "SGI4D") then
  set f77libs     =       "-lF77 -lI77 -lU77 -lisam -lm -lc"
else
  set f77libs     =       "-lF77 -lI77 -lU77 -lm"
endif

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
        set libcbind = "$libcbind"
		breaksw

	case "-conransuper":
		echo "Super Conran"
		set libs = "$libs $l/libconras.o $l/libdashsupr.o"
        set libcbind = "$libcbind"
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
		echo "Quick Dash"
		set libs = "$libs $l/libdashline.o"
		breaksw

	case "-dashsuper":
		echo "Super Dash"
		set libs = "$libs $l/libdashsupr.o"
		breaksw

	case "-ictrans"
		echo "Output to ictrans"
		set ctrans_libs = `ctlib`
		breaksw


	default:
		set newargv = "$newargv $arg"

	endsw

end

set newargv = "$newargv $ctrans_libs $libs $libcbind $libncarg $libgks $liblocal $f77libs"

echo $newargv
eval $newargv

#!/bin/csh -f
#
#	$Id: ncargcc.csh,v 1.33 1994-05-03 13:54:37 haley Exp $
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

set XLIBPATH = ""
set system   = "SED_SYSTEM_INCLUDE"
set cc       = "SED_CC"
set defines  = "SED_STDDEF SED_PROJDEF"
set loadopt  = "SED_LDCFLAGS"
set libdir   = `ncargpath SED_LIBDIR`
set incdir   = `ncargpath SED_INCDIR`
set ro       = "$libdir/SED_NCARGDIR/SED_ROBJDIR"

if (! -d "$libdir") then
  echo "Library directory <$libdir> does not exist."
  exit 1
endif

if (! -d "$incdir") then
  echo "Include directory <$incdir> does not exist."
  exit 1
endif

set libextra = ""

set newargv = "$cc -I$incdir $defines $loadopt"

set ctrans_libs = ""
set stub_file   = ""

if ($system == "Cray2" || $system == "Cray") then
  set f77libs     = "-L/lib -lf -lm -lp -lsci -lu -lc"
else if ($system == "Sun4") then
  set f77libs     = "-L/usr/lang/SC1.0 -Bstatic -lF77 -Bdynamic -lV77 -lm -lc"
else if ($system == "Sun4Solaris") then
  set f77libs     = "-L/opt/SUNWspro/SC2.0.1 -lF77 -lV77 -lM77 -lm"
  set XLIBPATH    = "-L/usr/openwin/lib"
  set libextra    = "/usr/ucblib/libucb.a"
else if ($system == "Sun3") then
  set f77libs     = "-L/usr/lang/SC1.0 -lF77 -lV77 /usr/lib/fswitch/libm.a"
else if ($system == "AIX_RS6000") then
  set f77libs     = "-lm -lxlf"
else if ($system == "DECRISC") then
  set f77libs     = "-lots -lfor -lF77 -lI77 -lU77 -lutil -li -lm -lUfor"
else if ($system == "HPUX_snake") then
  set f77libs     = "-lf -lm"
  set XLIBPATH    = "-L/usr/lib/X11R5"
else if ($system == "SGI4D") then
  set f77libs     = "-lF77 -lI77 -lU77 -lisam -lm -lc"
else if ($system == "AlphaOSF1") then
  set f77libs     = "-lm -lots -lfor"
else
  set f77libs     = "-lF77 -lI77 -lU77 -lm"
endif

set smooth = "$ro/libdashsmth.o"
set quick  = "$ro/libdashline.o $ro/libconrcqck.o $ro/libconraq.o"
set super  = "$ro/libdashsupr.o $ro/libconrcspr.o $ro/libconras.o"

#
# set up default libraries
#
set libncarg    =       "$libdir/libncarg.a"
set libgks      = "$libdir/libncarg_gksC.a $libdir/libncarg_gks.a"
set liblocal    = "$libdir/libncarg_loc.a"
set libncarg_c  = "$libdir/libncarg_c.a"
set libcbind    = "$libdir/libncargC.a"
set libX11      = "$XLIBPATH -lX11"

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

	case "-ictrans"
		echo "Output to ictrans"
		set ctrans_libs = `ctlib`
		breaksw

    case "-noX11"
        set stub_file = $ro/ggkwdr_stub.o
        set libX11 = ""
        breaksw

	default:
		set newargv = "$newargv $arg"

	endsw

end

set newargv = "$newargv $stub_file $ctrans_libs $libs $libcbind $libncarg $libgks $libncarg_c $liblocal $f77libs $libX11 $libextra"

echo $newargv
eval $newargv

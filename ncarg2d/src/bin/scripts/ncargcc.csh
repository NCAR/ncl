#!/bin/csh -f
#
#	$Id: ncargcc.csh,v 1.27 1994-03-24 15:05:06 haley Exp $
#

#*********************************************#
#                                             #
# Make sure NCARG_ROOT is set for this script #
#                                             #
#*********************************************#
setenv NCARG_ROOT  `ncargpath root`


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

#
# set up default libraries
#
set libncarg    =       "$libdir/libncarg.a"
set libgks      = "$libdir/libncarg_gksC.a $libdir/libncarg_gks.a"
set liblocal    = "$libdir/libncarg_loc.a"
set libncarg_c  = "$libdir/libncarg_c.a"
set libcbind    = "$libdir/libncargC.a"
set libX11      = "$XLIBPATH -lX11"

if ($system == "Cray2" || $system == "Cray") then
  set f77libs     =       "-L/lib -lf -lio -lm -lp -lsci -lu -lc"
else if ($system == "Sun4") then
  set f77libs     =       "-L/usr/lang/SC1.0 -Bstatic -lF77 -Bdynamic -lV77 -lm -lc"
else if ($system == "Sun4Solaris") then
  set f77libs     =       "-L/opt/SUNWspro/SC2.0.1 -lF77 -lV77 -lM77 -lm"
else if ($system == "Sun3") then
  set f77libs     =       "-L/usr/lang/SC1.0 -lF77 -lV77 /usr/lib/fswitch/libm.a"
else if ($system == "AIX_RS6000") then
  set f77libs     =       "-lm -lxlf"
else if ($system == "DECRISC") then
  set f77libs     =       "-lots -lfor -lF77 -lI77 -lU77 -lutil -li -lm -lUfor"
else if ($system == "HPUX_snake") then
  set f77libs     =       "-lf -lm"
else if ($system == "SGI4D") then
  set f77libs     =       "-lF77 -lI77 -lU77 -lisam -lm -lc"
else if ($system == "AlphaOSF1") then
  set f77libs     =       "-lm -lots -lfor"
else
  set f77libs     =       "-lF77 -lI77 -lU77 -lm"
endif

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

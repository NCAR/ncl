#!/bin/csh -f
#
#	$Id: ncargcc.csh,v 1.54 2010-04-02 17:49:55 haley Exp $
#                                                                      
#                Copyright (C)  2000
#        University Corporation for Atmospheric Research
#                All Rights Reserved
#
# The use of this Software is governed by a License Agreement.
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

set syslibdir = "SED_LIBSEARCHUSER"
set xlib     = "SED_XLIB"
set cairolib = "SED_CAIROLIBUSER"
set cc       = "SED_CC"
set defines  = "SED_STDDEF SED_PROJDEF"
set loadflags = "SED_LDCFLAGS"
set libdir   = `ncargpath SED_LIBDIR`
set incdir   = `ncargpath SED_INCDIR`
set ro       = "$libdir/SED_NCARGDIR/SED_ROBJDIR"
set f77libs  = "SED_CTOFLIBSUSER"
set libpath = "-L$libdir $syslibdir"
set incpath = "-I$incdir"

set libextra = ""

set newargv = "$cc $defines $loadflags"

set ctrans_libs = ""
set stub_file   = ""

set ncarbd   = "$ro/libncarbd.o"
set ngmathbd = "$ro/libngmathbd.o"
set smooth   = "$ro/libdashsmth.o"
set quick    = "$ro/libdashline.o $ro/libconrcqck.o $ro/libconraq.o"
set super    = "$ro/libdashsupr.o $ro/libconrcspr.o $ro/libconras.o"

#
# set up default libraries
#
set libncarg    = "-lncarg"
set libgks     = "-lSED_LIBNCARG_GKS"
set libmath     = ""
set libncarg_c  = "-lncarg_c"

set robjs
unset NGMATH_LD
unset NGMATH_BLOCKD_LD

foreach arg ($argv)

	switch ($arg)

	case "-ngmath":
		set libmath     = "-lngmath"
		set NGMATH_LD
	breaksw

	case "-sungks":
		echo "Using Sun GKS"
		set libgks="-lgks77 -lgks -lsuntool -lsunwindow -lpixrect -lm"
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

	case "-ictrans"
		echo "Output to ictrans"
		set ctrans_libs = `ctlib`
		breaksw

    case "-noX11"
    case "-nox11"
        set stub_file = $ro/ggkwdr_stub.o
        set xlib = ""
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

set ncarg_libs  = "$libncarg $libgks $libncarg_c $libmath"
set newargv = "$newargv $stub_file $libpath $incpath $ctrans_libs $robjs $ncarg_libs $f77libs $xlib $cairolib $libextra"


echo $newargv
eval $newargv

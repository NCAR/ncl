#!/bin/csh -f
#
#	$Id: nhlcc.csh,v 1.1 1994-12-05 17:23:31 haley Exp $
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

set xlibs = "SED_XMOTIFLIB SED_XTOOLLIB SED_XLIB"
set system   = "SED_SYSTEM_INCLUDE"
set cc       = "SED_CC"
set defines  = "SED_STDDEF SED_PROJDEF"
set loadflags  = "SED_LDCFLAGS"
set libdir   = `ncargpath SED_LIBDIR`
set incdir   = `ncargpath SED_INCDIR`
set syslibdir = "SED_LIBSEARCH"
set sysincdir = "SED_INCSEARCH"
set ro       = "$libdir/SED_NCARGDIR/SED_ROBJDIR"
set f77libs  = "SED_CTOFLIBS"
set newargv = "$cc $defines $loadflags"
set libpath = "-L$libdir $syslibdir"
set incpath = "-I$incdir $sysincdir"

#
# set up default libraries
#
set libncarg    = "-lncarg"
set libgks      = "-lncarg_gksC -lncarg_gks"
set libncarg_c  = "-lncarg_c"
set libcbind    = "-lncargC"
set libhlu      = "-lhlu"
set ncarg_libs = "$libhlu $libcbind $libncarg $libgks $libncarg_c"

foreach arg ($argv)
  set newargv = "$newargv $arg"
end

set newargv = "$newargv $libpath $incpath $ncarg_libs $xlibs $f77libs"

echo $newargv
eval $newargv

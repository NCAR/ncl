#!/bin/csh -f
#
#   $Id: MakeNcl.csh,v 1.5 2008-07-27 04:12:48 haley Exp $
#                                                                      
#                Copyright (C)  2004
#        University Corporation for Atmospheric Research
#                All Rights Reserved
#
# The use of this Software is governed by a License Agreement.
#
# Use this script to make an ncl executable from installed libraries.
#

set cc_ld         = "SED_CC_LD"
set cc_opts       = "SED_CC_OPT"
set ld_libs       = "SED_LDLIBS"
set extra_ld_libs = "SED_EXTRA_LDLIBS"
set libpath       = "SED_LIBPATH"
set incpath       = "SED_INCPATH"
set libdir        = "-L$libpath"
set incdir        = "-I$incpath"
set libncl        = "-lncl"
set libnfp        = "-lnfp -lnfpfort"
set libhlu        = "-lhlu"
set libncarg      = "-lncarg"
set libgks        = "-lncarg_gks"
set libncarg_c    = "-lncarg_c"
set libmath       = "-lngmath"
set ncarg_libs    = "$libncl $libnfp $libhlu $libncarg $libgks $libncarg_c $libmath"

if (! -d "$libpath") then
  echo "Library directory <$libpath> does not exist."
  exit 1
endif

set files      = ""
set extra_opts = ""

foreach arg ($argv)

    switch ($arg)

    case "-*":
        set extra_opts = "$extra_opts $arg"
        breaksw

    default:
        set files = "$files $arg"
        breaksw
    endsw
end

if ("$files" == "") then
  echo "MakeNcl error: You must input one or more *.o files"
  exit 1
endif

set newargv = "$cc_ld $cc_opts $extra_opts -o ncl $files $incdir $libdir $ncarg_libs $ld_libs $extra_ld_libs"

echo $newargv
eval $newargv

#!/bin/csh -f
#
#   $Id: MakeNcl.csh,v 1.3 2005-03-24 00:08:36 haley Exp $
#                                                                      
#                Copyright (C)  2004
#        University Corporation for Atmospheric Research
#                All Rights Reserved
#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2 of the License,
# or (at your option) any later version.
#
# This software is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this software; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
# USA.
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
set libnfp        = "-lnfpfort -lnfp"
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

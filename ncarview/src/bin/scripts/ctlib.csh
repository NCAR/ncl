#!/bin/csh -f
#
#	$Id: ctlib.csh,v 1.9 2008-07-27 03:58:55 haley Exp $
#
#                Copyright (C)  2000
#        University Corporation for Atmospheric Research
#                All Rights Reserved
#
# The use of this Software is governed by a License Agreement.
#
#	This script simply reports the libraries required by ctrans.
#	The library list will vary depending on machine configuration
#

set libdir = `ncargpath LIBDIR`
if ($status != 0) then
	exit 1
endif
if (! -d "$libdir") then
  echo "Library directory <$libdir> does not exist."
  exit 1
endif

set robjdir = $libdir/NCARGDIR/ROBJDIR

set locals = ""

foreach file (CTRANS_ROBJS)
  set locals = "$locals $robjdir/$file"
end

foreach file (CTRANS_LIBS_A)
  set locals = "$locals $libdir/$file"
end

echo $locals CTRANS_LIBS_B

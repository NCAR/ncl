#!/bin/csh -f
#
#	$Id: ctlib.csh,v 1.3 1991-08-16 16:45:14 clyne Exp $
#
#
#	This script simply reports the libraries required by ctrans.
#	The library list will vary depending on machine configuration
#

set libdir = `ncargpar LIBDIR`

if ($status != 0) then
	exit 1
endif

if (! -d "$libdir") then
  echo "Library directory <$libdir> does not exist."
  exit 1
endif

set locals = ""

foreach file (CTRANS_LIBS_A)
  set locals = "$locals $libdir/$file"
end

echo $locals CTRANS_LIBS_B

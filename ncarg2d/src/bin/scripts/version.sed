#!/bin/csh -f
#
#	$Id: version.sed,v 1.3 1994-10-05 00:12:09 haley Exp $
#

set year = "19`date +%y`"

if ($#argv > 0) then
  if ($1 == "-v") echo VERSION
else

echo ""
cat <<EOF
NCAR Graphics - UNIX Version VERSION
Copyright (C) $year - All Rights Reserved
University Corporation for Atmospheric Research
EOF
echo ""
endif

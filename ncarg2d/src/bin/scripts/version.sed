#!/bin/csh -f
#
#	$Id: version.sed,v 1.4 1995-08-21 19:28:40 haley Exp $
#

set year = "19`date +%y`"

if ($#argv > 0) then
  if ($1 == "-v") echo VERSION
else

echo ""
cat <<EOF
NCAR Graphics - UNIX Version VERSION
Copyright (C) 1987-$year
University Corporation for Atmospheric Research
The use of this Software is governed by a License Agreement.
EOF
echo ""
endif

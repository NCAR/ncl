#!/bin/csh -f
#
#	$Id: version.sed,v 1.9 1995-08-21 19:29:31 haley Exp $
#

set year = 19`date +%y`

echo ""
cat <<EOF
NCAR View - UNIX Version VERSION
Copyright (C) 1987-$year
University Corporation for Atmospheric Research
The use of this Software is governed by a License Agreement.
EOF


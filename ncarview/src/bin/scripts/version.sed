#!/bin/csh -f
#
#	$Id: version.sed,v 1.8 1993-02-19 02:35:41 clyne Exp $
#

set year = 19`date +%y`

echo ""
cat <<EOF
NCAR View - UNIX Version VERSION
Copyright (C) $year - All Rights Reserved
University Corporation for Atmospheric Research
EOF


#!/bin/csh -f
#
#	$Id: version.sed,v 1.7 1992-06-24 20:51:46 clyne Exp $
#

set date = `date`
set year = $date[6]

echo ""
cat <<EOF
NCAR View - UNIX Version VERSION
Copyright (C) $year - All Rights Reserved
University Corporation for Atmospheric Research
EOF


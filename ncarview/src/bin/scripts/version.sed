#!/bin/csh -f
#
#	$Id: version.sed,v 1.6 1992-02-14 12:56:17 clyne Exp $
#

set date = `date`
set year = $date[6]

echo ""
cat <<EOF
NCAR View - UNIX Version VERSION
Copyright (C) $year - All Rights Reserved
University Corporation for Atmospheric Research
EOF


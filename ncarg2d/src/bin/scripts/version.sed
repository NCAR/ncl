#!/bin/csh -f
#
#	$Id: version.sed,v 1.2 1993-02-18 23:18:31 haley Exp $
#

set year = "19`date +%y`"

echo ""
cat <<EOF
NCAR Graphics - UNIX Version VERSION
Copyright (C) $year - All Rights Reserved
University Corporation for Atmospheric Research
EOF
echo ""

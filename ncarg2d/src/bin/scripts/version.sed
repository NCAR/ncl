#!/bin/csh -f
#
#	$Id: version.sed,v 1.1 1992-11-02 14:38:59 haley Exp $
#

set date = `date`
set year = $date[6]

echo ""
cat <<EOF
NCAR Graphics - UNIX Version VERSION
Copyright (C) $year - All Rights Reserved
University Corporation for Atmospheric Research
EOF
echo ""

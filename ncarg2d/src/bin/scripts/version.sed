#!/bin/csh -f
#
#	$Id: version.sed,v 1.26 2008-12-22 23:49:51 haley Exp $
#

set year = "2009"
set sitefile = `ncargpath NCARGDIR`/NCARGSITEFILE
if (-f $sitefile) then
  set sitenum = `head -1 $sitefile`
else
  set sitenum = "Site ID Number undefined for support."
endif  

if ($#argv > 0) then
  if ("$1" == "-v") echo VERSION
else

echo ""
cat <<EOF
NCAR Graphics Software Version VERSION
Copyright (C) 1987-$year, University Corporation for Atmospheric Research

NCAR Graphics is a registered trademark of the University Corporation
for Atmospheric Research.

The use of this Software is governed by a License Agreement.
EOF
echo ""
endif

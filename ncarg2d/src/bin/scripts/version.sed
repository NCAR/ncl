#!/bin/csh -f
#
#	$Id: version.sed,v 1.6 1996-04-04 23:52:35 haley Exp $
#

set year = "19`date +%y`"
set sitefile = `ncargpath NCARGDIR`/NCARGSITEFILE
if (-f $sitefile) then
  set sitenum = "Site ID Number: `head -1 $sitefile`"
else
  set sitenum = "Site ID Number undefined for support -- please contact your Site Installer."
endif  

if ($#argv > 0) then
  if ("$1" == "-v") echo VERSION
else

echo ""
cat <<EOF
NCAR Graphics - UNIX Version VERSION
$sitenum

Copyright (C) 1987-$year
University Corporation for Atmospheric Research

The use of NCAR Graphics software and documentation is governed
by a License Agreement.
---------------------------------------------------------------
NCAR Graphics is a registered trademark of the University
Corporation for Atmospheric Research.
EOF
echo ""
endif

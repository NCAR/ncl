#!/bin/csh -f
#
#	$Id: fcaps.csh,v 1.7 2008-07-27 03:58:55 haley Exp $
#
#                Copyright (C)  2000
#        University Corporation for Atmospheric Research
#                All Rights Reserved
#
# The use of this Software is governed by a License Agreement.
#
#	Name		fcaps
#
#	Author		John Clyne
#
#	Date		Wed Jun 27 13:23:39 MDT 1990
#
#	Desc.		Report all the fontcaps installed in the fontcap 
#			directory.
# 

set version = "VERSION"
set dir = `ncargpath FONTCAPDIR`
if ($status != 0) then
	exit 1
endif

foreach arg ($argv)
	switch ($arg)
	case "-V"
		echo "${0}: Version $version"
		exit 0
	default:
		echo "Usage: $0 [-V]"
		exit 1
	endsw
end


if (! -d "$dir") then
  echo "Fontcap directory <$dir> does not exist."
  exit 1
endif

cd $dir
echo "The following fontcaps are installed to ${dir}:"
foreach fcap ("" *)
	echo "	$fcap"
end

#!/bin/csh -f
#
#	$Id: fcaps.csh,v 1.3 1992-09-09 15:07:49 clyne Exp $
#
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

set version = VERSION
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

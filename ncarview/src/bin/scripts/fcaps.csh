#!/bin/csh -f
#
#	$Id: fcaps.csh,v 1.5 2000-07-12 18:14:17 haley Exp $
#
#                Copyright (C)  2000
#        University Corporation for Atmospheric Research
#                All Rights Reserved
#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 2.1 of the
# License, or (at your option) any later version.
#
# This software is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this software; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
# USA.
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

#!/bin/csh -f
#
#	$Id: ctlib.csh,v 1.7 2000-07-12 18:14:17 haley Exp $
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
#	This script simply reports the libraries required by ctrans.
#	The library list will vary depending on machine configuration
#

set libdir = `ncargpath LIBDIR`
if ($status != 0) then
	exit 1
endif
if (! -d "$libdir") then
  echo "Library directory <$libdir> does not exist."
  exit 1
endif

set robjdir = $libdir/NCARGDIR/ROBJDIR

set locals = ""

foreach file (CTRANS_ROBJS)
  set locals = "$locals $robjdir/$file"
end

foreach file (CTRANS_LIBS_A)
  set locals = "$locals $libdir/$file"
end

echo $locals CTRANS_LIBS_B

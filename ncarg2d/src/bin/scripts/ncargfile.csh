#!/bin/csh -f
#
#   $Id: ncargfile.csh,v 1.3 2000-07-12 17:04:42 haley Exp $
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

set file_dir=`ncargpath SED_DBDIR`
if ($status != 0) then
        exit 1
endif

if (! -d "$file_dir") then
  echo "Example directory <$file_dir> does not exist."
  exit 1
endif

if ($#argv < 1) then
echo "usage: ncargfile [-all] file(s)"
echo ""
echo "See <man ncargfile>"
exit
endif

set file_list=( ezmap_area_ids )

set files

while ($#argv > 0)
    
    switch ($1)

        case "-all":
            shift
            set files=($file_list)
            breaksw

        case "-*":
            echo "$0 : Unknown option <$1>"
            exit 1
            breaksw

        default:
            set files=($files $1)
            shift
            breaksw
    endsw
end

foreach file ($files)

################################################################
#
# Code for handling various files
#
################################################################

  echo ""
  echo "Copying NCAR Graphics File <$file>..."
  echo ""
  cp $file_dir/$file .

end

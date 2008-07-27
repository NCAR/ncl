#!/bin/csh -f
#
#   $Id: ncargfile.csh,v 1.5 2008-07-27 00:59:06 haley Exp $
#                                                                      
#                Copyright (C)  2000
#        University Corporation for Atmospheric Research
#                All Rights Reserved
#
# The use of this Software is governed by a License Agreement.
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

#!/bin/csh -f
#
#	$Id: version.sed,v 1.14 2000-07-12 17:04:42 haley Exp $
#

set year = "2000"
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
------------------------------------------------------------------------------
NCAR Graphics Software and Documentation
------------------------------------------------------------------------------
  Version        : VERSION
  Site ID number : $sitenum
  Copyright (C)  : 1987-$year, University Corporation for Atmospheric Research
  Trademark      : NCAR Graphics is a registered trademark of the University
                   Corporation for Atmospheric Research.

  License        : This software is distributed in the hope that it will
                   be useful, but WITHOUT ANY WARRANTY; without even the
                   implied warranty of MERCHANTABILITY or FITNESS FOR A 
                   PARTICULAR PURPOSE.  See the GNU Lesser General Public
                   License for more details.

                   You should have received a copy of the GNU Lesser
                   General Public License along with this software; if
                   not, write to the Free Software Foundation, Inc.,
                   59 Temple Place, Suite 330, Boston, MA 02111-1307
                   USA.
------------------------------------------------------------------------------
EOF
echo ""
endif

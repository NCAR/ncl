.TH NGCKOP 3NCARG "October 1996" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGCKOP - a function to check if a specified GKS workstation is open.
.SH SYNOPSIS
INTEGER FUNCTION NGCKOP(WKID)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
int c_ngckop(int wkid)
.SH DESCRIPTION 
.IP WKID 12
(an input variable of type INTEGER) specifying a  GKS workstation
identifier.
.SH C-BINDING DESCRIPTION
The C binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
The function returns a "1" if the workstation identified by
WKID is open, otherwise it returns a "0".
.SH EXAMPLES
.nf

        IOPEN = NGCKOP(WKID)

.fi
sets IOPEN to "1" if WKID is open and sets IOPEN to "0" otherwise.
.SH ACCESS
To use NGCKOP or c_ngckop, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online URL: http://ngwww.ucar.edu/ngdoc/ng/gks/gkshome.html
.SH COPYRIGHT
Copyright (C) 1987-2005
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.

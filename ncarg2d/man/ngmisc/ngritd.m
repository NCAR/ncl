.TH NGRITD 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGRITD - Given the coordinates of a point, this routine performs a rotation
of that point about a specified axis by a specified angle.
.SH SYNOPSIS
CALL NGRITD (IAXS,ANGL,UCRD,VCRD,WCRD)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ngritd(int iaxs, float angl, float *ucrd, float *ycrd,
.br
float *wcrd)
.SH DESCRIPTION 
.IP IAXS 12
(an input expression of type INTEGER) specifies the axis about which rotation
is to be done (1 for the U axis, 2 for the V axis, and 3 for the W axis).
.IP ANGL 12
(an input expression of type REAL) specifies the magnitude, in degrees, of the
rotation angle.
.IP UCRD 12
(an input/output variable of type REAL) specifies the U coordinate of the
point being rotated.
.IP VCRD 12
(an input/output variable of type REAL) specifies the V coordinate of the
point being rotated.
.IP WCRD 12
(an input/output variable of type REAL) specifies the W coordinate of the
point being rotated.
.SH C-BINDING DESCRIPTION
The C binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
This routine is used by NGGCOG and NGGSOG to effect the rotations that are
used to generate an object of a specified shape at a specified point on the
surface of the globe.
.sp
NGRITD assumes that the UVW coordinate system is right-handed.  Positive
values of ANGL give counter-clockwise rotations and negative values of ANGL
give clockwise rotations.  (When IAXS = 1, ANGL = 90 carries the positive V
axis into the positive W axis; when IAXS = 2, ANGL = 90 carries the positive
W axis into the positive U axis; when IAXS = 3, ANGL = 90 carries the positive
U axis into the positive V axis.)
.SH EXAMPLES
.sp
Use the ncargex command to see the following relevant
example: 
cpex10.
.SH ACCESS
To use NGRITD or c_ngritd, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
None.
.SH SEE ALSO
Online:
nggcog(3NCARG),
nggsog(3NCARG).
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

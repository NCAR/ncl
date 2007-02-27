.TH LINE3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
LINE3 - Draws the straight-line segment joining the projections of two points
in 3-space.
.SH SYNOPSIS
CALL LINE3 (UA,VA,WA,UB,VB,WB)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_line3 (float ua, float va, float wa, float ub, 
.br
float vb, float wb)
.SH DESCRIPTION 
.IP "UA,VA,WA" 12
(input expressions of type REAL) are the coordinates of the first point
in 3-space.
.IP "UB,VB,WB" 12
(input expressions of type REAL) are the coordinates of the second point
in 3-space.
.PP
The statement "CALL LINE3 (UA,VA,WA,UB,VB,WB)" is
equivalent to the three statements "CALL FRST3 (UA,VA,WA)",
"CALL VECT3 (UB,VB,,WB)", and "CALL PLOTIF (0.,0.,2), but is
slightly more efficient.  To approximate a curve defined by three or more
points, though, it is not efficient to use LINE3, because the first point
of each line segment after the first will be a repeat of the second point
of the previous line segment and will therefore be repeated in the metafile.
Thus, to approximate a curve, you should use FRST3 and VECT3 or CURVE3.
.PP
Straight-line segments drawn by LINE3 are drawn in the current polyline
color, as determined by the last call to the GKS routine GSPLCI; by default,
color index 1 is used.  Line width is determined by the last call to the GKS
routine GSLWSC; by default, the line width scale factor is 1.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant example:
tpwrzt,
fthex02.
.SH ACCESS
To use LINE3 or c_line3, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
threed,
curve3,
fence3,
frst3,
perim3,
point3,
psym3,
pwrz,
pwrzt,
set3,
threed,
tick3,
tick43,
vect3,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2007
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

.TH DPLINE 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DPLINE - Draws a straight-line segment between two points in the user
coordinate system.
.SH SYNOPSIS
CALL DPLINE (XCP1,YCP1,XCP2,YCP2)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_dpline (float xcp1, float ycp1, float xcp2, float ycp2);
.SH DESCRIPTION 
.IP XCP1 12
(an input expression of type REAL) specifies the X coordinate of a point at
the beginning of the straight-line segment.
.IP YCP1 12
(an input expression of type REAL) specifies the Y coordinate of a point at
the beginning of the straight-line segment.
.IP XCP2 12
(an input expression of type REAL) specifies the X coordinate of a point at
the end of the straight-line segment.
.IP YCP2 12
(an input expression of type REAL) specifies the Y coordinate of a point at
the end of the straight-line segment.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
DPLINE maps the input coordinates to the fractional system and then passes
them on to DPDRAW.  Even is smoothing is turned on, no smoothing is done by
a call to DPLINE.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tdshpk.
.SH ACCESS
To use DPLINE or c_dpline, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
dashpack,
dashpack_params,
dpcurv,
dpdraw,
dpfrst,
dpgetc,
dpgeti,
dpgetr,
dplast,
dpsetc,
dpseti,
dpsetr,
dpsmth,
dpvect,
ncarg_cbind.
.sp
Hardcopy:
None.
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

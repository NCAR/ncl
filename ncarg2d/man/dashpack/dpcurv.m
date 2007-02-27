.TH DPCURV 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DPCURV - Used to draw a complete curve defined by a sequence of points in the
user coordinate system.
.SH SYNOPSIS
CALL DPCURV (XCPU,YCPU,NPTS)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_dpcurv (float *xcpu, float *ycpu, int npts);
.SH DESCRIPTION 
.IP XCPU 12
(an input array of type REAL) specifies the X coordinates of points in
the user coordinate system defining a curve to be drawn.
.IP YCPU 12
(an input array of type REAL) specifies the Y coordinates of points in
the user coordinate system defining a curve to be drawn.
.IP NPTS 12
(an input expression of type INTEGER) specifies the number of points
defining the curve.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
The FORTRAN statement "CALL DPCURV(XCPU,YCPU,NPTS)" draws a complete curve
by connecting the point (XCPU(1),YCPU(1)) to the point (XCPU(2),YCPU(2)),
connecting that point to the point (XCPU(3),YCPU(3)), and so on, through
the last point, (XCPU(NPTS),YCPU(NPTS)), in a manner described by the
current values of the internal parameters of DASHPACK.
.sp
DPCURV maps the input coordinates to the fractional system and then passes
them on to lower-level routines.  If smoothing is turned off, DPCURV calls
DPDRAW; if smoothing is turned on, it calls DPSMTH.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tdshpk.
.SH ACCESS
To use DPCURV or c_dpcurv, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
dashpack,
dashpack_params,
dpdraw,
dpfrst,
dpgetc,
dpgeti,
dpgetr,
dplast,
dpline,
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

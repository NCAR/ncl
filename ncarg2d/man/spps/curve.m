.TH CURVE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CURVE - draws the curve defined by a specified series of points
in the user coordinate system.  The "pen" (for subsequent calls to
FRSTPT, VECTOR, PLOTIF, and PLOTIT) is left at the location of the
last point in the curve.
.SH SYNOPSIS
CALL CURVE (PX,PY,NP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_curve (float *px, float *py, int np)
.SH DESCRIPTION 
.IP PX 12
(an input array of type REAL) defines the X user coordinates of
the curve.  Array PX is of length NP.
.IP PY 12
(an input array of type REAL) defines the Y user coordinates of
the curve.  Array PY is of length NP.
.IP NP 12
(an input expression of type INTEGER) specifies the number
of points in the curve.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
If the routine SET was not called, or if it was called with a linear
mapping in both the X and Y coordinates, then the GKS routine GPL is
called to output a polyline of length NP points.  If the mapping in
either axis is logarithmic, the input coordinates are transformed and
drawn ten points at a time. (The last segment may be less than ten points.)
.sp
Polyline type, line width, and color can also be set by calling the
GKS routines GSLN, GSLWSC, GSCR, and GSPLCI, before calling CURVE.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
sfex01, sfex02, and tsoftf.
.SH ACCESS
To use CURVE or c_curve, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
gpl, gsln, gslwsc, gscr, gsplci,
frstpt, vector, plotif, line, spps, spps_params, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
User's Guide for NCAR GKS-0A Graphics;
"The Use of X/Y Coordinates in NCAR Graphics" SCD User Document
.SH COPYRIGHT
Copyright (C) 1987-2000
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

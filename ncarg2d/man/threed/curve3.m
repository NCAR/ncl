.TH CURVE3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CURVE3 - Draws the projection of a curve defined by a sequence of points
in 3-space.
.SH SYNOPSIS
CALL CURVE3 (U,V,W,N)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_curve3 (float *u, float *v, float *w, int n)
.SH DESCRIPTION 
.IP "U,V,W" 12
(input arrays, dimensioned at least N, of type REAL) contain the U, V,
and W coordinates, respectively, of the points defining the curve whose
projection is to be drawn.
.IP N 12
(an input expression of type INTEGER) is the number of points defining the
curve.
.PP
Curves drawn by CURVE3 are drawn in the current polyline color, as determined
by the last call to the GKS routine GSPLCI; by default, color index 1 is used.
Line width is determined by the last call to the GKS routine GSLWSC; by
default, the line width scale factor is 1.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tthree,
fthex03.
.SH ACCESS
To use CURVE3 or c_curve3, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
threed,
fence3,
frst3,
line3,
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

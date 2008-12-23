.TH FENCE3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
FENCE3 - Draws the projection of a curve, just as CURVE3
does, but with a "fence" of straight line segments
that are parallel to one of the three axes and
extend from the curve to a specified plane perpendicular
to that axis.
.SH SYNOPSIS
CALL FENCE3 (U,V,W,N,IOREN,BOT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_fence3 (float *u, float *v, float *w, int n, 
.br
int ioren, float bot)
.SH DESCRIPTION 
.IP "U,V,W" 12
(input arrays, dimensioned at least N, of type REAL) contain the U, V,
and W coordinates, respectively, of the points defining the curve whose
projection is to be drawn.
.IP N 12
(an input expression of type INTEGER) is the number of points defining the
curve.
.IP IOREN 12
(an input expression of type INTEGER)
is the direction in which fence lines are to be drawn, as
follows:
.RS
.IP 1 3
Parallel to the U axis.
.IP 2 3
Parallel to the V axis.
.IP 3 3
Parallel to the W axis.
.RE
.IP BOT 12
(an input expression of type REAL)
specifies where the bottom of the fence is to be drawn.
For example, if the fence lines are to be drawn parallel to the
W-axis, and BOT=2., then the bottom of the fence would
be the plane W=2.
.sp
The word "bottom" may be misleading here; the entire curve might be below
the plane of the "bottom" and the fence might be horizontal, rather than
vertical, in the three-dimensional space.
.PP
The curve and the fence are drawn in the colors implied by the values of the
variables ITHRMJ and ITHRMN, respectively, in the Threed COMMON block
.PP
.RS
.IP " " 6
COMMON /THRINT/ ITHRMJ,ITHRMN,ITHRTX
.RE
.PP
The default value of both ITHRMJ and ITHRMN is 1; user code may change
these values.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example:
fthex04.
.SH ACCESS
To use FENCE3 or c_fence3, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
threed,
curve3,
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

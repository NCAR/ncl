.TH CURVED 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CURVED -
Draws a curve through a sequence of points.
.SH SYNOPSIS
CALL CURVED (X,Y,N)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_curved (float *x, float *y, int n)
.SH DESCRIPTION 
.IP X 12
(an input array of type REAL) defines the X user coordinates of
the curve.  Array X is of length N.
.IP Y 12
(an input array of type REAL) defines the Y user coordinates of
the curve.  Array Y is of length N.
.IP N 12
(an input expression of type INTEGER) specifies the number
of points in the curve.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument description.
.SH USAGE
The statement
.IP " " 6
CALL CURVED (X,Y,N)
.PP
is equivalent to the statements
.IP " " 6
CALL FRSTD (X(1),Y(1))
.br
CALL VECTD (X(2),Y(2))
.br
CALL VECTD (X(3),Y(3))
.br
\&.\&.\&.(et cetera)\&.\&.\&.
.br
CALL VECTD (X(N),Y(N))
.br
CALL LASTD
.PP
The basic difference is that CURVED requires you to have all of the
coordinates defining the curve available at once; sometimes this is
convenient and sometimes it is not.
.sp
If three or more distinct points are given, and if one of the smoothing
versions of Dashline is being used, and if the internal parameter that
suppresses smoothing is not set, then splines under tension are used to
generate a smooth curve; the number of points actually used to draw the
curve will depend on its length.  In all other cases, the "curve" will be
approximated by just connecting the given points in the specified order.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
carline, colcon, cmpfil, cmpgrp, cmpitm, cmplab, cmpmsk, cmptit,
cpex01, cpex02, cpex03, cpex04, cpex06, vvex01,
tdashc, tdashl, tdashp, tdashs, carline,
fcover, ffex03, ffex05,
fdlcurvd, fdldashd, fdlsmth, fpcloqu.
.SH ACCESS
To use CURVED or c_curved, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.sp
See the man page for dashline to see how to access the quick,
normal, smooth, and super line draw options.
.SH SEE ALSO
Online:
dashline, dashline_params,
dashdb, dashdc, frstd, lastd, lined, reset, vectd, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
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

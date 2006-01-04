.TH VECTD 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
VECTD -
Defines the second and following of a sequence of points through which a
curve is to be drawn.
.SH SYNOPSIS
CALL VECTD (X,Y)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_vectd (float x, float y)
.SH DESCRIPTION 
.IP X 12
(an input expression of type REAL) defines the X user coordinate of
the next point in a sequence of points defining a curve.
.IP Y 12
(an input expression of type REAL) defines the Y user coordinate of
the next point in a sequence of points defining a curve.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
One way to draw a curve with Dashline is to call FRSTD once to define the
first point of the curve and then to call VECTD repeatedly to define the
second and all following points of the curve.
.sp
If three or more distinct points are given, and if one of
the smoothing versions of Dashline is being used, and if the internal
parameter that suppresses smoothing is not set, then splines under tension
are used to generate a smooth curve; the number of points actually used to
draw the curve will depend on its length.  In all other cases, the "curve"
will be approximated by just connecting the user-given points in the specified
order.
.sp
After the call to VECTD defining the last point of the curve, you may call
LASTD, which flushes any portions of smoothed curves that are defined by
coordinates saved in internal buffers of FRSTD and VECTD and that have not
yet been drawn.  Calls to LASTD are not always required - for example, when
a non-smoothing version of Dashline is used (no buffering) or when the next
call to an NCAR Graphics routine will be to FRSTD (which flushes the
buffers) - but unnecessary calls do no harm.  If you judge that one of the
smoothing versions of Dashline may be used, it is best to put in the calls
to LASTD.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
tdashc, tdashl, tdashp, tdashs,
fdldashc, fdldashd.
.SH ACCESS
To use VECTD or c_vectd load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
dashline, dashline_params, curved,
dashdb, dashdc, frstd, lastd, lined, reset, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2006
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

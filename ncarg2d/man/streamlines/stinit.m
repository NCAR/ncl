.TH STINIT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
STINIT - 
Performs initialization tasks required before STREAM
may be called to create a streamline field flow plot,
including copying array size information into internal
common block variables and establishing the basic mapping
from grid coordinates to data coordinates and from user
coordinates to NDC space.
.SH SYNOPSIS
CALL STINIT (U,LU,V,LV,P,LP,M,N,WRK,LW) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_stinit( float *u, int lu, float *v, int lv, float *p, int lp, int m, int n, float *wrk, int lw)
.SH DESCRIPTION 
.IP U 
(REAL 2-dimensional array, dimensioned LU x n: n >= N,
input): By default, assumed to contain the first
dimensional Cartesian components of the vector field.
However, if PLR is non-zero, it is treated as containing
the vector magnitudes.
.IP LU 12
(INTEGER, input): Actual value of the first dimension of
array U.
.IP V 12 
(REAL 2-dimensional array, dimensioned LV x n: n >= N,
input): By default, assumed to contain the second
dimensional Cartesian components of the vector field.
However, if PLR is non-zero, it is treated as containing
the vector angles.
.IP LV 12 
(INTEGER, input): Actual value of the first dimension of
array V.
.IP P 12
(REAL, input, DUMMY): This argument is currently ignored.
Should be assigned a dummy value.
.IP LP 12
(INTEGER, input, DUMMY): This argument is currently
ignored. Should be assigned a dummy value.
.IP M 12
(INTEGER, input): Number of contiguous elements along the
first dimensional axis containing data to be processed in
each of the arrays, U and V.
.IP N 12
(INTEGER, input): Number of contiguous elements along the
second dimensional axis containing data to be processed in
each of the arrays, U and V.
.IP WRK 12
(REAL, array dimensioned n: n >= LW, input/output):
Work array used to store normalized values of the U and V
vector components while the streamlines are calculated. It
also holds bit flags indicating whether a grid location is
eligible for starting a streamline or placing a directional
arrowhead.
.IP LW 12
(INTEGER, input): Assumed size of the array WRK.
Currently it must greater or equal to 2*M*N.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions, with the following exceptions:
.sp
.IP lu 12
The second dimension of u in the calling program.
.IP lv 12
The second dimension of v in the calling program.
.IP lp 12
The second dimension of p in the calling program.
.IP m 12
The number of data values to be plotted in the y
direction (the second subscript direction).
.IP n 12
The number of data values to be plotted in the x
direction (the first subscript direction).
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
stex01,
stex03.
.SH ACCESS
To use STINIT, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_stinit, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH MESSAGES
See the streamlines man page for a description of all Streamlines error
messages and/or informational messages.
.SH SEE ALSO
Online:
streamlines,
streamlines_params,
ezstrm,
fx,
fy,
stgeti,
stgetr,
stream,
strmln,
strset,
stseti,
stsetr,
stuixy,
stumsl,
stumta,
stumxy,
ncarg_cbind.
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

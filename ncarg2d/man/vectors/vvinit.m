.TH VVINIT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
VVINIT - 
Performs initialization tasks required before VVECTR
may be called to plot a vector field, including copying
array size information into internal common block
variables, establishing the basic mapping from grid
coordinates to data coordinates and from user coordinates
to NDC space, determining the maximum and minimum vector
magnitudes and scalar array values, and, if required,
setting up the color threshold value array.
.SH SYNOPSIS
CALL VVINIT (U,LU,V,LV,P,LP,M,N,WRK,LW) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_vvinit(float *u, int lu, float *v, int lv, \\
.br
float *p, int lp, int m, int n,
float *wrk, int lw)
.SH DESCRIPTION 
.IP U 12
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
array V
.IP P 12
(REAL 2-dimensional array, dimensioned LP x n: n >= N,
input): Array of scalar data that may be used to color the
vectors. The grid points are assumed to coincide with the
grid points of the U and V arrays. Required only if CTV has
an absolute value of 2; otherwise this argument is ignored
and may be assigned a dummy value.
.IP LP 12
(INTEGER, input): Actual value of the first dimension of
array P
.IP M 12
(INTEGER, input): Number of contiguous elements along the
first dimensional axis containing data to be processed in
each of the arrays, U, V, and P (if used).
.IP N 12
(INTEGER, input): Number of contiguous elements along the
second dimensional axis containing data to be processed in
each of the arrays, U, V, and P (if used).
.IP WRK 12
(REAL, array dimensioned n: n >= LW, input/output):
Array intended for future enhancement of the Vectors
utility. It is currently ignored and may always be assigned
a dummy value.
.IP LW 12
(INTEGER, input): Assumed size of the array WRK. Since
the WRK array is not currently used, this argument may be
assigned a dummy value.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions with the following exceptions:
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
Use the ncargex command to see the following relevant examples: 
bnchmk,
stex03,
vvex01,
vvex02.
.SH ACCESS
To use VVINIT, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_vvinit, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
.SH MESSAGES
See the vectors man page for a description of all Vectors error
messages and/or informational messages.
.SH SEE ALSO
Online:
vectors,
ezvec,
fx,
fy,
vvectr,
vvgetc,
vvgeti,
vvgetr,
vvrset,
vvsetc,
vvseti,
vvsetr,
vvudmv,
vvumxy,
ncarg_cbind.
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

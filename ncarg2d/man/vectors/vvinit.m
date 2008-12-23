.TH VVINIT 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
VVINIT - Performs initialization tasks required before VVECTR may be
called to plot a vector field, including copying array size
information into internal common block variables, establishing coordinate
system mappings and boundaries, determining the maximum and minimum vector
magnitudes and scalar array values, and, if required, setting up the
color threshold value array.
.SH SYNOPSIS
CALL VVINIT (U,LU,V,LV,P,LP,M,N,WRK,LW) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_vvinit(float *u, int lu, float *v, int lv,
.br
              float *p, int lp, int m, int n,
.br
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
Work array required only if the parameter VMD is set to a value
greater than 0.0. If required must be dimensioned greater or equal to
2 * M * N. Otherwise may be set to a dummy value.
.IP LW 12
(INTEGER, input): Assumed size of the array WRK. If the parameter VMD
is set to a value greater than 0.0, must be set to a value less than
or equal to the dimension of the WRK array, but greater or equal to 
2 * M * N. Otherwise, this argument should be assigned the integer value 0.
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
Number of contiguous elements along the
second dimensional axis containing data to be processed in
each of the arrays, u, v, and p (if used).
.IP n 12
Number of contiguous elements along the
first dimensional axis containing data to be processed in
each of the arrays, u, v, and p (if used).
.SH USAGE
Call VVINIT before the first invocation of VVECTR and again anytime
you modify the contents of the input data arrays. You may precede a
VVINIT call with any number of calls to the Vectors parameter setting
routines (VVSETC, VVSETI, or VVSETR). After the VVINIT call, you may
still change certain parameters before calling VVECTR. (Consult the
vectors_params man page for further information on this point.)
.sp
Set up the two vector component arrays prior to calling VVINIT.  To
permit multiple purpose use of the array space, the VVINIT argument
list includes both the actual size and an assumed size for the first
dimension of each input array. Due to FORTRAN array ordering
conventions, only the assumed size needs to be specified for the
second dimension.  (Note: when using the C bindings, mentally exchange
all references to first and second dimensions in this discussion.) The
arguments LU, LV, and LP contain the actual size of the first
dimensions of arrays U, V, and P respectively. Since the grid
locations for each of the data arrays are assumed to coincide, a
single argument, M, represents the assumed size of the first dimension
for all the arrays.  Similarly, the argument, N, is the assumed size
of the second dimension. The only requirement for the actual second
dimension size is that it be greater than or equal to N for each
array.
.sp
The array specified by the WRK argument and its associated size
specifier, LW, are used only when the parameter VMD (Vector Minimum
Distance) is given a value greater than 0.0. In this case, Vectors
uses the array to keep track of the location of each vector in NDC
space so that the distances between vectors can be compared. Based on
these comparisons, Vectors eliminates some vectors such that the
remaining vectors are separated by at least the specified distance. If
VMD is less than or equal to 0.0, you may assign an arbitrary dummy
value to WRK, but you should set LW to the integer value 0.
.SH C-BINDING USAGE
C-Binding usage is the same as FORTRAN usage discussed above if
the references to "first dimension" and "second dimension" are exchanged.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
bnchmk,
fcover,
ffex00,
ffex01,
ffex02,
ffex05,
stex02,
stex03,
vvex01,
vvex02.
.SH ACCESS
To use VVINIT or c_vvinit, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the vectors man page for a description of all Vectors error
messages and/or informational messages.
.SH SEE ALSO
Online:
vectors,
vectors_params,
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
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

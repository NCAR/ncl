.TH STINIT 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
STINIT - Performs initialization tasks required before STREAM may be
called to create a streamline field flow plot.  Information about the
input data arrays is copied into internal common block variables and
the coordinate system mappings and boundaries are established.
.SH SYNOPSIS
CALL STINIT (U,LU,V,LV,P,LP,M,N,WRK,LW) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_stinit(float *u, int lu, float *v, int lv, 
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
array V.
.IP P 12
(REAL, input, DUMMY): This argument is currently ignored
and may be assigned a dummy value.
.IP LP 12
(INTEGER, input, DUMMY): The first dimension of array P. Not currently
used, this argument should be assigned the integer value 0.
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
Number of contiguous elements along the
second dimension containing data to be processed in
each of the arrays, u and v.
.IP n 12
Number of contiguous elements along the first dimension containing
data to be processed in each of the arrays, u and v.
.SH USAGE
Call STINIT before the first invocation of STREAM, and again anytime
you modify the contents of the input data arrays. You may precede an
STINIT call with any number of calls to the parameter setting
routines, STSETR and STSETI. Although it is possible to call STSETI or
STSETR between calls to STINIT and STREAM, changing the setting of
many of Streamline's parameters at this point will result in incorrect
behavior. When in doubt, set parameters before the STINIT call; see
the streamlines_params man page for more information.
.sp
Set up the two vector component arrays prior to calling STINIT.  To
permit multiple purpose use of the array space, the STINIT argument
list includes both the actual size and an assumed size for the first
dimension of each input array. Due to FORTRAN array ordering
conventions, only the assumed size needs to be specified for the
second dimension.  (Note: when using the C bindings, mentally exchange
all references to first and second dimensions in this discussion.) The
arguments LU and LV contain the actual size of the first dimensions of
U and V, respectively. Since the grid locations for each of the data
arrays are assumed to coincide, a single argument, M, represents the
assumed size of the first dimension for both arrays. Similarly, the
argument, N, is the assumed size of the second dimension. The only
requirement for the actual second dimension size is that it be
greater than or equal to N for each array.
.sp
The P and LP arguments are intended for future enhancement of the
Streamlines utility. They are not currently used or examined in any
way. For maximum assurance of compatibility with future changes to the
code, give the LP argument the INTEGER value 0.  
.sp
WRK is an array used to store normalized values of the U and V vector
components while the streamlines are calculated. It also holds bit
flags indicating whether a grid location is eligible for starting a
streamline or placing a directional arrowhead. Its assumed size is
specified by the argument LW. LW must have a value greater or equal to
2*M*N.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
ffex00,
ffex01,
ffex03,
ffex04,
fstream,
stex01,
stex02,
stex03.
.SH ACCESS
To use STINIT or c_stinit, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the streamlines man page for a description of all Streamlines error
messages and/or informational messages.
.SH SEE ALSO
Online:
stgetc,
stgeti,
stgetr,
stream,
streamlines,
streamlines_params,
strset,
stsetc,
stseti,
stsetr,
stuixy,
stumsl,
stumta,
stumxy,
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

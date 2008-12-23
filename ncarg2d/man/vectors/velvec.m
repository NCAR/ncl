.TH VELVEC 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
VELVEC - A older version of the Vectors utility, now a front-end to
VELVCT. It is identical to the VELVCT call except that there is no
LENGTH parameter to allow adjustment of the realized length of the
maximum vector magnitude.
.SH SYNOPSIS
VELVEC (U,LU,V,LV,M,N,FLO,HI,NSET,ISPV,SPV)
.SH STATUS
VELVEC is obsolete, and is supported only to provide compatibility
with old NCAR Graphics codes. However, the compatibility mode
parameter, CPM, offers a number of options to help ease the the
transition to the new version of the utility. When writing new code
you are encouraged not to use this entry point, since it provides less
capability than the standard Vectors interface, and may eventually
be phased out.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_velvec(float *u, int lu, float *v, int lv, int m,
.br
              int n, float flo, float hi, int nset,
.br
              int ispv, float *spv)
.SH DESCRIPTION
.IP U 12
(REAL 2-dimensional array, dimensioned LU x n: n >= N, input): By
default, assumed to contain the first dimensional Cartesian
components of the vector field. However, if PLR is non-zero, it is
treated as containing the vector magnitudes.
.IP LU 12
(INTEGER, input): Actual value of the first dimension of array U.
.IP V 12
(REAL 2-dimensional array, dimensioned LV x n: n >= N, input): By
default, assumed to contain the second dimensional Cartesian
components of the vector field. However, if PLR is non-zero, it is
treated as containing the vector angles.
.IP LV 12
LV (INTEGER, input): Actual value of the first dimension of array V.
.IP M 12
(INTEGER, input): Number of contiguous elements along the first
dimensional axis containing data to be processed in each of the
arrays, U and V.
.IP N 12
(INTEGER, input): Number of contiguous elements along the second
dimensional axis containing data to be processed in each of the
arrays, U and V.
.IP FLO 12
(REAL, input): Minimum vector magnitude allowed to be displayed in the
plot.
.IP HI 12
(REAL, input): Maximum vector magnitude allowed to be displayed in the
plot. If set to 0.0 there is no upper limit imposed.
.IP NSET 12
(INTEGER, input): Flag that controls how and when the SET call is
invoked. If NSET is 0, VELVEC makes a SET call to establish a standard
viewport and window boundaries coincident with the array coordinate
boundaries. PERIM is called to draw a border. If NSET is greater than
zero, VELVEC does not call SET or PERIM. If NSET is less than zero,
VELVEC calls SET to establish window boundaries coincident with the
array grid coordinate boundaries but does not modify the viewport or
call PERIM. Unlike the VVINIT/VVECTR interface, when VELVEC does a SET
call, it always restores the original coordinate system state before
returning.
.IP ISPV 12
(INTEGER, input): Flag to control the special value feature. 0 means
that the feature is not in use. 1 means that if the value of
U(I,J)=SPV(1) the vector will not be plotted.  2 means that if the
value of V(I,J)=SPV(2) the vector will not be plotted. 3 means that
if either U(I,J)=SPV(1) or V(I,J)=SPV(2) then the vector will not be
plotted. 4 means that if U(I,J)=SPV(1) and V(I,J)=SPV(2), the vector
will not be plotted.
.IP SPV 12
(REAL array, dimensioned 2, input): An array of length 2 which gives
the value in the U array and the value in the V array which denote
special values. This argument is ignored if ISPV=0. The default values
are 1.0E12.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions with the following exceptions:
.sp
.IP lu 12
The second dimension of u in the calling program.
.IP lv 12
The second dimension of v in the calling program.
.IP m 12
Number of contiguous elements along the
second dimensional axis containing data to be processed in
each of the arrays, u and v.
.IP n 12
Number of contiguous elements along the
first dimensional axis containing data to be processed in
each of the arrays, u and v.
.SH USAGE
VELVEC is used identically to VELVCT except that there is no provision
for adjusting the realized length of the maximum vector magnitude. See the
velvct man page for more information.
.SH ACCESS
To use VELVEC or c_velvec, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
vectors,
vectors_params,
fx,
vvectr,
vvgetc,
vvgeti,
vvgetr,
vvinit,
vvrset,
vvsetc,
vvseti,
vvsetr,
vvudmv,
vvumxy,
ncarg_cbind.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.

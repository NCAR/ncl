.TH CPSPS2 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPSPS2 - 
Interpolates from an array of data
on a "sparse" rectangular grid which is irregularly spaced
in X and Y to an array of data on a "dense" rectangular
grid and initializes contouring from the array on the
dense grid. (By a "sparse" grid is meant one whose
dimensions are smaller than one would like, so that contour
lines constructed directly on it are composed of long
straight segments.) CPSPS2 may be viewed as a data
smoothing routine.
.SH SYNOPSIS
 CALL CPSPS2 (XSPS, YSPS, ZSPS, KSPS, MSPS, NSPS, RWRK, 
.br
+ LRWK, IWRK, LIWK, ZDAT,
LZDT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cpsps2 (float *xsps, float *ysps, float *zsps, 
.br
int ksps, int msps, int nsps, float *rwrk, int lrwk, 
.br
int *iwrk, int liwk, float *zdat, int lzdt) 
.SH DESCRIPTION 
.IP XSPS 12
(REAL array, dimensioned MSPS) is the array of X 
coordinates of the irregular rectangular grid. These must 
be in strictly increasing numerical order.
.IP YSPS 12
(REAL array, dimensioned NSPS) is the array of Y 
coordinates of the irregular rectangular grid. These must 
be in strictly increasing numerical order.
.IP ZSPS 12
(REAL array, dimensioned KSPS x n, where "n" is 
greater than or equal to NSPS, input) is the "sparse" array 
of data, from which the "dense" array is to be generated.
.IP KSPS 12
(INTEGER, input) is the first dimension of the array 
ZSPS.
.IP MSPS 12
(INTEGER, input) is the first dimension of the 
"sparse" array of data in ZSPS. MSPS must be less than or 
equal to KSPS.
.IP NSPS 12
(INTEGER, input) is the second dimension of the 
"sparse" array of data in ZSPS. NSPS must be less than or 
equal to the declared second dimension of the array ZSPS.
.IP RWRK 12
(REAL array, dimensioned LRWK, input/output) is the 
real work array.
.IP LRWK 12
(INTEGER, input) is the length of RWRK.
.IP IWRK 12
(INTEGER array, dimensioned LIWK, input/output) is the 
integer work array.
.IP LIWK 12
(INTEGER, input) is the length of IWRK.
.IP ZDAT 12
(REAL array, dimensioned LZDT, output) is the array in 
which the interpolated "dense" array of data is to be 
returned. The dimensions of the interpolated array may be 
supplied by the user or determined by Conpack, depending on 
the value of the parameter 'ZDS'. Note that, if Conpack 
determines the dimensions of the "dense" array, it will 
attempt to use an aspect ratio which is close to that 
implied by the value of the ratio
.RS 17
.sp
(XSPS(MSPS)-XSPS(1))/(YSPS(NSPS)-YSPS(1))
.RE
.IP LZDT 12
(INTEGER, input) is the length of ZDAT.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions, with the following exceptions:
.sp
.IP "zsps(l,ksps)" 12
Dimensioned l by ksps, where l \(<= nsps.
.IP "ksps" 12
The second dimension of the array zsps.
.IP "msps" 12
The second dimension of the sparse array of data in zsps. msps \(<= ksps.
.IP "nsps" 12
The first dimension of the sparse array of data in zsps. 
nsps \(<= l, the declared first dimension of the array zsps.
.SH USAGE
CPSPS2 performs the same functions as CPRECT, but, in addition,
it interpolates from a sparse array of data to a dense array of
data.  CPSPS2 does this by using the routines SURF1 and
SURF2, from the package Fitpack, by Alan K. Cline, to fit
bicubic splines under tension to the sparse array of data and
to compute the dense grid of data that is returned to you.  The
tension on the spline surfaces is specified by the parameter
\&'T3D'.  By default, CPSPS2 selects the dimensions of the dense
array of data; if desired, you can specify these dimensions by
setting the parameter 'ZDS' non-zero and the parameters 'ZD1',
\&'ZDM', and 'ZDN' to the desired values. In either case, once
\&'ZD1', 'ZDM', and 'ZDN' are set, they should not be reset by
you until the contour plot is complete and a different contour
plot is to be drawn.
.sp
Because the routines SURF1 and SURF2 do not have a built-in
special value feature, if the special value parameter 'SPV' is
set non-zero and the sparse array contains occurrences of that
value, special action must be taken.  The indices of the
special values in the sparse array are saved in a part of the
integer workspace array; the special values are then replaced
by values interpolated from adjacent grid points and the
resulting array is used to obtain the dense array; then, the
special values in the sparse array are restored and the
corresponding elements of the dense array are also given the
special value.  
.SH EXAMPLES
Use the ncargex command to see the following relevant
example:
ccpsps2.
.SH ACCESS
To use CPSPS2 or c_cpsps2, load the NCAR Graphics libraries ncarg,
ncarg_gks, and ncarg_c, preferably in that order.  
.SH MESSAGES
See the conpack man page for a description of all Conpack error
messages and/or informational messages.
.SH SEE ALSO
Online:
conpack,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cpmviw, cpmvrw, cppkcl, cppklb, cprect, cprset, cpscae,
cpsetc, cpseti, cpsetr, cpsps1, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.

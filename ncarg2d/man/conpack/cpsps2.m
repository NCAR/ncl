.TH CPSPS2 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPSPS2 - 
Interpolates from an array of data
on a "sparse" rectangular grid which is irregularly spaced
in X and Y to an array of data on a "dense" rectangular
grid and to initialize contouring from the array on the
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
void c_cpsps2 (float *xsps, float *ysps, float *zsps, \\
.br
int ksps, int msps, int nsps, float *rwrk, int lrwk, \\
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
(XSPS(MSPS)- XSPS(1))/(YSPS(NSPS)-YSPS(1))
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
.SH USAGE@@@
CPSPS2 initializes the internal pointers that are used to
manage workspace use and decides what the ranges of X and Y
coordinates used to draw contour lines and position labels
ought to be.  CPSPS1 also interpolates from an irregularly
spaced rectangular grid of data to a dense array of data, and
can be used as a smoother.
.SH ACCESS
To use CPSPS2, load the NCAR Graphics libraries ncarg,
ncarg_gks, and ncarg_loc, preferably in that order.  To use 
c_cpsps2, load the NCAR Graphics libraries ncargC, ncarg_gksC, 
ncarg, ncarg_gks, and ncarg_loc, preferably in that order.
.SH MESSAGES
See the conpack man page for a description of all Conpack error
messages and/or informational messages.
.SH SEE ALSO
Online:
conpack,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cppkcl, cppklb, cprect, cprset, cpscae, cpsetc, cpseti,
cpsetr, cpsps1, ncarg_cbind
.sp
Hardcopy:
Tutorial: A Step-by-Step Guide to Contouring and Mapping
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved


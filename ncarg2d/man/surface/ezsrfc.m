.\"
.\"	$Id: ezsrfc.m,v 1.1 1993-03-11 16:34:13 haley Exp $
.\"
.TH EZSRFC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
EZSRFC - draws a perspective picture of a function of two
variables with hidden lines removed. The function is
approximated by a two-dimensional array of heights. Use EZSRFC
only if the entire array is to be drawn, the data points are
equally spaced in the X-Y plane, there are no stereo pairs, and
scaling is chosen internally.
.SH SYNOPSIS
CALL EZSRFC (Z,M,ANGH,ANGV,WORK)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ezsrfc (float *z, int m, float angh, float angv, float *work)
.SH DESCRIPTION 
.IP Z 12
The M by N array to be drawn.
.IP M 12
The first dimension of Z.
.IP N 12
The second dimension of Z.
.IP ANGH 12
Angle in degrees in the X-Y plane to the line of sight
(counterclockwise from the plus-X axis).
.IP ANGV 12
Angle in degrees from the X-Y plane to the line of
sight (positive angles are above the middle Z, negative
below).
.IP WORK 12
A scratch storage dimensioned at least 2*M*N+M+N.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions, with the following exceptions:
.IP z 12
The n by m array to be drawn.
.IP m 12
The second dimension of z.
.IP n 12
The first dimension of z.
.SH ACCESS
To use EZSRFC load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_ezsrfc load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
srface, ezsrfc, pwrzs, trn32s, ncarg_cbind
.sp
Hardcopy:  "NCAR Graphics User's Guide, Version 2.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved


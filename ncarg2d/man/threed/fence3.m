.TH FENCE3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
FENCE3 - Draws a line in 3-space as well as a "fence"
between the line and the plane normal to one of the coordinate
axes.
.SH SYNOPSIS
CALL FENCE3 (U,V,W,N,IOREN,BOT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_fence3 (float *u, float *v, float *w, int n, 
.br
int ioren, float bot)
.SH DESCRIPTION 
.IP "U, V, W" 12
Specify real arrays containing locations of points.
.IP N 12
Specifies the number of points to be plotted.  U, V, and W must be
dimensioned N or greater.
.IP IOREN 12
Specifies the direction in which the fence lines are to
be drawn (1 indicates parallel to the U-axis, 2
indicates parallel to the V-axis, and 3 indicates
parallel to to the W-axis.)
.IP BOT 12
Specifies where the bottom of the fence is to be drawn.
If the fence lines are to be drawn parallel to the
W-axis, and BOT=2., then the bottom of the fence would
be the plane W=2.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use FENCE3, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_fence3, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
threed,
curve3,
frst3,
line3,
perim3,
point3,
psym3,
pwrz,
pwrzt,
set3,
threed,
tick3,
tick43,
vect3,
ncarg_cbind.
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

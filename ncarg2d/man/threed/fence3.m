.\"
.\"	$Id: fence3.m,v 1.1 1993-03-11 16:34:30 haley Exp $
.\"
.TH FENCE3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
FENCE3 - used to draw a line in 3-space as well as a "fence"
between the line and the plane normal to one of the coordinate
axes.
.SH SYNOPSIS
CALL FENCE3 (U,V,W,N,IOREN,BOT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_fence3 (float *u, float *v, float *w, int n, int ioren, float bot)
.SH DESCRIPTION 
.IP "U, V, W" 12
real arrays containing locations of points
.IP N 12
the number of points to be plotted.  U, V, and W must be
dimensioned N or greater.
.IP IOREN 12
specifies the direction in which the fence lines are to
be drawn (1 indicates parallel to the U-axis, 2
indicates parallel to the V-axis, and 3 indicates
parallel to to the W-axis.)
.IP BOT 12
specifies where the bottom of the fence is to be drawn.
If the fence lines are to be drawn parallel to the
W-axis, and BOT=2., then the bottom of the fence would
be the plane W=2.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use FENCE3 load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_fence3 load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.sp
.SH SEE ALSO
Online:
curve3 fence3 frst3 line3 perim3 point3 set3 threed
tick43 vect3 ncarg_cbind
.sp
Hardcopy:  "NCAR Graphics User's Guide, Version 2.00"
.sp
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

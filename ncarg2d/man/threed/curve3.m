.\"
.\"	$Id: curve3.m,v 1.1 1993-03-11 16:34:27 haley Exp $
.\"
.TH CURVE3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
CURVE3 - Draws a curve through points.
.SH SYNOPSIS
CALL CURVE3 (U,V,W,N)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_curve3 (float *u, float *v, float *w, int n)
.SH DESCRIPTION 
.IP "U, V, W" 12
real arrays containing locations of points
.IP N 12
the number of points to be plotted.  U, V, and W must be
dimensioned N or greater.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use CURVE3 load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_curve3 load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
curve3 fence3 frst3 line3 perim3 point3 set3 threed
tick43 vect3 ncarg_cbind
.sp
Hardcopy:  "NCAR Graphics User's Guide, Version 2.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

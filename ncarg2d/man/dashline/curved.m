.\"
.\"	$Id: curved.m,v 1.1 1993-04-06 19:38:27 haley Exp $
.\"
.TH CURVED 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
CURVED - Draws a curve through points.
.SH SYNOPSIS
CALL CURVED (X, Y, N)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_curved (float *x, float *y, int n)
.SH DESCRIPTION 
.IP X 12
Array of X values in user coordinates, of length N.
.IP Y 12
Array of Y values in user coordinates, of length N.
.IP N 12 
Number of points to be connected.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument description.
.SH ACCESS
To use CURVED, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_curved, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.sp
To get the smooth version of CURVED, load libdashsmth.o, or add
the -dashsmooth option to your ncargf77 command.  To get the
super version of CURVED, load libdashsupr.o, or add the
-dashsuper option to your ncargf77 command.
.SH SEE ALSO
Online:
curved, dashline, dashdb, dashdc, frstd, lined, vectd, ncarg_cbind
.sp
Hardcopy:
"NCAR Graphics User's Guide," Version 2.00
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

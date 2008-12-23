.TH LINED 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
LINED -
Draws a line segment between two points whose user
coordinates are given.
.SH SYNOPSIS
CALL LINED (XA,XB,YA,YB)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_lined (float xa, float xb, float ya, float yb)
.SH DESCRIPTION 
.IP XA 12
(an input expression of type REAL) defines the X user coordinate of
the starting point of a line segment.
.IP YA 12
(an input expression of type REAL) defines the Y user coordinate of
the starting point of a line segment.
.IP XB 12
(an input expression of type REAL) defines the X user coordinate of
the ending point of a line segment.
.IP YB 12
(an input expression of type REAL) defines the Y user coordinate of
the ending point of a line segment.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
tdashc, tdashl, tdashp, tdashs, 
fcoord1, fcoord2,
fdldashc, fdldashd, fgklnclr, fgklnwth
dashdb, dashdc, frstd, lined, reset, vectd, ncarg_cbind
.SH ACCESS
To use LINED or c_lined, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
dashline, dashline_params, curved,
dashdb, dashdc, frstd, lastd, reset, vectd, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

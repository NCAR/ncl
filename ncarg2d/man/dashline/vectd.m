.TH VECTD 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
VECTD - draw a line segment between the pen position and the
current point in user coordinates
.SH SYNOPSIS
CALL VECTD (X, Y)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_vectd (float x, float y)
.SH DESCRIPTION 
.IP X 12
(an input coordinate of type REAL) defining the X user coordinate of
the current point in a line segment.
.IP Y 12
(an input coordinate of type REAL) defining the Y user coordinate of
the current point in a line segment.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
tdashc.f, tdashl.f, tdashp.f, tdashs.f,
fdldashc.f, fdldashd.f
.SH ACCESS
To use VECTD load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_vectd load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, 
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
dashline, dashline_params, curved,
dashdb, dashdc, frstd, lastd, lined, reset, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

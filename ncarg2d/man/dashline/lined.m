.TH LINED 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
LINED - Draws a line segment between two points in user
coordinates
.SH SYNOPSIS
CALL LINED (XA, XB, YA, YB)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_lined (float xa, float xb, float ya, float yb)
.SH DESCRIPTION 
.IP XA 12
(an input coordinate of type REAL) defining the X user coordinate of
the starting point of a line segment.
.IP YA 12
(an input coordinate of type REAL) defining the Y user coordinate of
the starting point of a line segment.
.IP XB 12
(an input coordinate of type REAL) defining the X user coordinate of
the ending point of a line segment.
.IP YB 12
(an input coordinate of type REAL) defining the Y user coordinate of
the ending point of a line segment.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
tdashc.f, tdashl.f, tdashp.f, tdashs.f, spset1.f, spset2.f
fdldashc.f, fdldashd.f, fgk.nclr.f, fgklnwth.f
dashdb, dashdc, frstd, lined, reset, vectd, ncarg_cbind
To use LINED load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_lined load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
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
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

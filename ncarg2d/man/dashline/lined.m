.\"
.\"	$Id: lined.m,v 1.1 1993-04-06 19:38:52 haley Exp $
.\"
.TH LINED 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
LINED - draw a line segment between two points in user
coordinates
.SH SYNOPSIS
CALL LINED (XA, XB, YA, YB)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_lined (float xa, float xb, float ya, float yb)
.SH DESCRIPTION 
.IP XA 12
the X position of the first point in user coordinates
.IP YA 12
the Y position of the first point in user coordinates
.IP XB 12
the X position of the second point in user coordinates
.IP YB 12
the Y position of the second point in user coordinates
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use LINED load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_lined load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
curved, dashline, dashdb, dashdc, frstd, lined, vectd, ncarg_cbind
.sp
Hardcopy:
"NCAR Graphics User's Guide, Version 2.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved


.\"
.\"	$Id: point3.m,v 1.1 1993-03-11 16:34:42 haley Exp $
.\"
.TH POINT3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
POINT3 - draws a point in 3-space.
.SH SYNOPSIS
CALL POINT3 (U,V,W)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_point3 (float u, float v, float w)
.SH DESCRIPTION 
.IP "U, V, W" 12
coordinates for the point in 3-space.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use POINT3 load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_point3 load 
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

.TH POINT3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
POINT3 - Draws a point in 3-space.
.SH SYNOPSIS
CALL POINT3 (U,V,W)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_point3 (float u, float v, float w)
.SH DESCRIPTION 
.IP "U, V, W" 12
Coordinates for the point in 3-space.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use POINT3, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_point3, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
threed,
curve3,
fence3,
frst3,
line3,
perim3,
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

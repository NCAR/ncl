.TH FRST3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
FRST3 - Positions the pen on a point in 3-space.
.SH SYNOPSIS
CALL FRST3 (UA,VA,WA)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_FRST3 (float ua, float va, float wa)
.SH DESCRIPTION 
.IP "UA, VA, WA" 12
Coordinates for the point in 3-space.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use FRST3, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_frst3, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
threed,
curve3,
fence3,
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

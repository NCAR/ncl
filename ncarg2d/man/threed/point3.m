.TH POINT3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
POINT3 - Draws the projection of a point in 3-space.
.SH SYNOPSIS
CALL POINT3 (U,V,W)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_point3 (float u, float v, float w)
.SH DESCRIPTION 
.IP "U,V,W" 12
(input expressions of type REAL) are the coordinates of a point in
3-space.
.PP
Each point is drawn by calling the GKS routine GPM to draw a polymarker
of type 1.  Points are therefore drawn in the current polymarker color, as
determined by the last call to the GKS routine GSPMCI; by default, color
index 1 is the one used.
.PP
Calling the GKS routine GSMKSC to change the polymarker size does not
normally affect polymarkers of type 1, so there is no good way to make
the points bigger; if you want to do this, you should use LINE3 (or perhaps
FRST3 and VECT3) instead of POINT3 to draw an object of the desired size.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples:
fthex01,
fthex02,
fthex03,
fthex04,
fthex05.
.SH ACCESS
To use POINT3 or c_point3, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
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
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

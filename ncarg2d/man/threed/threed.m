.TH THREED 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
THREED - 3-d line drawing package.
.SH SYNOPSIS
CURVE3 - Draws a curve through points.
.sp
FENCE3 - Draws a line in 3-space as well as a "fence"
between the line and the plane normal to one of the coordinate
axes.
.sp
FRST3 - Positions the pen on a point in 3-space.
.sp
LINE3 - Draws a line between two points in 3-space.
.sp
PERIM3 - Draws a perimeter with tick marks.
.sp
POINT3 - Draws a point in 3-space.
.sp
PWRZT - A character-plotting routine for plotting characters in
three-space when using Threed.
.sp
SET3 - Set up 3D equivalents of viewport and window, as well as
perspective angle.
.sp
TICK43 - Allows control of tick mark length.
.sp
VECT3 - Draws a line between the current pen position and the
point (UA,VA,WA).  The current pen position becomes (UA,VA,WA).
Note that a curve can be drawn by using a FRST3 call followed by
a sequence of VECT3 calls.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_curve3
.br
c_fence3
.br
c_frst3
.br
c_line3
.br
c_perim3
.br
c_point3
.br
c_pwrzt
.br
c_set3
.br
c_threed
.br
c_tick43
.br
c_vect3
.SH ACCESS 
To use THREED routines, load the NCAR Graphics libraries ncarg,
ncarg_gks, ncarg_c, and ncarg_loc, preferably in that order.  To use the
THREED C-binding routines, load the NCAR Graphics libraries ncargC,
ncarg_gksC, ncarg, ncarg_gks, ncarg_c, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
threed,
curve3,
fence3,
frst3,
line3,
perim3,
point3,
psym3,
pwrz,
pwrzt,
set3,
tick3,
tick43,
vect3,
ncarg_cbind.
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

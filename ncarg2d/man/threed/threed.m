.TH THREED 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
THREED - 3-d line drawing package.
.SH SYNOPSIS
CURVE3 -
Draws the projection of a curve defined by a
sequence of points in 3-space.
.sp
FENCE3 -
Draws the projection of a curve, just as CURVE3
does, but with a "fence" of straight line segments
that are parallel to one of the three axes and
that extend to a specified plane perpendicular
to that axis.
.sp
FRST3 -
Specifies the first in a sequence of points connected
by straight-line segments, the projections of which
are to be drawn. (FRST3 is called for the first point
in the sequence and VECT3 is called for the rest.)
.sp
LINE3 -
Draws the projection of a straight-line segment.
.sp
PERIM3 -
Draws the projection of a rectangular perimeter in
a plane parallel to one of the three coordinate
planes, with inward-pointing tick marks at specified
intervals.
.sp
POINT3 -
Draws the projection of a point.
.sp
PWRZT -
Draws the projection of character strings that are
positioned in a plane parallel to one of the three
coordinate planes.
.sp
SET3 -
Defines the transformation from three dimensions to
two dimensions.
.sp
TICK43 -
Gives user control of tick mark length in PERIM3.
.sp
VECT3 -
Defines the second and all subsequent points in a sequence of
points connected by straight-line segments, the
projections of which are to be drawn. (FRST3 is
called for the first point in the sequence and
VECT3 is called for the rest.)
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
ncarg_gks, and ncarg_c, preferably in that order.  
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
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

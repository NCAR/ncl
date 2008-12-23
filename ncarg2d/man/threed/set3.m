.TH SET3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SET3 - Defines the transformation from three dimensions to
two dimensions.
.SH SYNOPSIS
CALL SET3 (XMIN,XMAX,YMIN,YMAX,UMIN,UMAX,VMIN,VMAX,WMIN,WMAX,PEYE)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_set3 (float xmin, float xmax, float ymin, float ymax,
.br
float umin, float umax, float vmin, float vmax, float wmin,
.br
float wmax, float peye[3])
.SH DESCRIPTION 
.IP "XMIN,XMAX,YMIN,YMAX" 12
(input expressions of type REAL) are the fractional coordinates (in NDCs)
defining the portion of the plotter frame (the "viewport") into which 3-D
objects are to be projected.  Each has a value between 0. and 1., inclusive.
Examples:  To use the whole plotter frame, set XMIN=0., XMAX=1., YMIN=0.,
and YMAX=1.; to use the upper left quarter of the plotter frame, set XMIN=0.,
XMAX=.5, YMIN=.5, and YMAX=1.
.IP "UMIN,UMAX,VMIN,VMAX,WMIN,WMAX" 12
(input expressions of type REAL) are the limiting coordinates, in a 3-D
coordinate space of the user's choosing, of the rectangular box that is
to be projected.  This box normally contains all of the objects whose
projections are to be drawn.
.IP PEYE 12
(an input array, dimensioned 3, of type REAL) is an array specifying the
coordinates, in the user's 3-D coordinate system, of the eye position.  The
point
.RS
.IP " " 6
(PEYE(1),PEYE(2),PEYE(3))
.RE
.IP " " 12
must be outside the box defined by UMIN, UMAX, VMIN, VMAX, WMIN, and WMAX.
Objects will be drawn as they appear from this point.
.PP
The call to SET3 says what portion of the 3-D coordinate system is to be
projected, from what point it is to be projected, and where the resulting
picture is to be placed on the plotter frame.
.PP
The array PEYE specifies the position of the eye, which may be referred to
as the "viewpoint" or "center" of the projection.
.PP
The "line of sight" runs from the point
.IP " " 6
(PEYE(1),PEYE(2),PEYE(3))
.PP
(the eye position) to the point
.IP " " 6
(.5*(UMIN+UMAX),.5*(VMIN+VMAX),.5*(WMIN+WMAX))
.PP
(the point at the center of the projected box).
.PP
The "image plane" is perpendicular to the line of sight and may be thought
of as passing through the center of the box.  The projection of an arbitrary
point P in 3-space is the point where the straight line from the eye position
to P passes through the image plane.  The projection of a more complicated
object is just the union of the projections of its constituent points.
.PP
The projection in the image plane of the box specified by the arguments UMIN,
UMAX, VMIN, VMAX, WMIN, and WMAX will be scaled to fit in the viewport
specified by the arguments XMIN, XMAX, YMIN, and YMAX.  The projected box
will retain its natural aspect ratio; typically, it will just touch the left
and right edges of the viewport, or the top and bottom of the viewport, but
not both.
.PP
The image of the box will be rotated so that the projection of a unit vector
based at the center of the box and pointing in the direction of the positive
W axis will be made to point upward in the viewport.  However, if the line of
sight is very nearly parallel to the W axis, so that the projection of this
unit vector shrinks almost to a point, then a unit vector based at the center
of the box and pointing in the direction of the positive V axis will be made
to point upward in the viewport instead.  One implication of this is that,
if you are making a movie by letting the eye "fly around" the objects being
projected and it gets too close to being directly above or below the center
of the box, there may be a sudden disconcerting rotation of the projected
image.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
SET3 should be called before calling other THREED routines.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tpwrzt,
tthree.
.SH ACCESS
To use SET3 or c_set3, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
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

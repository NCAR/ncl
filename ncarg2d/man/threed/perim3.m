.TH PERIM3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PERIM3 -
Draws the projection of a rectangular perimeter in
a plane parallel to one of the three coordinate
planes, with inward-pointing tick marks at specified
intervals.
.SH SYNOPSIS
CALL PERIM3 (MAJOR1,MINOR1,MAJOR2,MINOR2,IAXIS,VALUE)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_perim3 (int major1, int minor1, int major2, int minor2,
.br
int iaxis, float value)
.SH DESCRIPTION 
.IP "MAJOR1" 12
(an input expression of type INTEGER) specifies the number of major
divisions of those sides of the perimeter parallel to direction 1.
(Directions 1 and 2 are defined by the value of IAXIS.)  Major divisions
are indicated by tick marks.
.IP "MINOR1" 12
(an input expression of type INTEGER) specifies the number of minor divisions
per major division along those sides of the perimeter parallel to direction
1.  (Directions 1 and 2 are defined by the value of IAXIS.)  Minor divisions
are indicated by tick marks that, by default, are smaller than the tick marks
indicating major divisions.
.IP "MAJOR2" 12
(an input expression of type INTEGER) specifies the number of major
divisions of those sides of the perimeter parallel to direction 2.
(Directions 1 and 2 are defined by the value of IAXIS.)  Major divisions
are indicated by tick marks.
.IP "MINOR2" 12
(an input expression of type INTEGER) specifies the number of minor divisions
per major division along those sides of the perimeter parallel to direction
2.  (Directions 1 and 2 are defined by the value of IAXIS.)  Minor divisions
are indicated by tick marks that, by default, are smaller than the tick marks
indicating major divisions.
.IP IAXIS 12
(an input expression of type INTEGER) specifies which of the three axes the
perimeter is to be perpendicular to and identifies directions 1 and 2:
.RS
.IP 1 3
The perimeter is to be perpendicular to the U axis.  Direction 1
is parallel to the V axis and direction 2 is parallel to the W axis.
.IP 2 3
The perimeter is to be perpendicular to the V axis.  Direction 1
is parallel to the U axis and direction 2 is parallel to the W axis.
.IP 3 3
The perimeter is to be perpendicular to the W axis.  Direction 1
is parallel to the U axis and direction 2 is parallel to the V axis.
.RE
.IP VALUE 12
(an input expression of type REAL)  is the value at which the plane of the
perimeter is to intersect the axis specified by IAXIS.  If IAXIS is a 1,
VALUE is a U coordinate; if IAXIS is a 2, VALUE is a V coordinate; and, if
IAXIS is a 3, VALUE is a W coordinate.
.PP
Each call to PERIM3 draws a perimeter: a rectangle representing the
intersection of a plane perpendicular to one of the three axes with
the box specified by the arguments UMIN, UMAX, VMIN, VMAX, WMIN, and WMAX
in the last call to SET3.  The perimeter can be placed at any desired
position along the axis to which it is perpendicular; it can have
inward-pointing major and minor tick marks along its edges, spaced as
specified by the user.  Tick marks along two parallel edges of the
perimeter are controlled by one pair of arguments, and tick marks along
the other two parallel edges are controlled by the other pair of arguments.
.PP
Each of the arguments MAJOR1 and MAJOR2 refers to the number of major
divisions (spaces between ticks) rather than to the number of major ticks;
counting those at the ends, there will be one more major tick than there
are major divisions.
.PP
Each of the arguments MINOR1 and MINOR2 refers to the number of minor
divisions (spaces between ticks) rather than to the number of minor ticks;
in a given major interval, there will be one fewer minor tick than there are
minor divisions.
.PP
The rectangle itself is drawn in the current polyline color, as determined
by the last call to the GKS routine GSPLCI; by default, color index 1 is
used.  Major ticks and minor ticks are drawn in the colors implied by the
values of the variables ITHRMJ and ITHRMN in the Threed COMMON block
.IP " " 6
COMMON /THRINT/ ITHRMJ,ITHRMN,ITHRTX
.PP
The default value of both ITHRMJ and ITHRMN is 1; user code may change
these values.
.PP
All lines are drawn using the current GKS line width scale factor, as set
by the last call to the GKS routine GSLWSC.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tthree,
fthex01,
fthex02,
fthex03,
fthex04,
fthex05.
.SH ACCESS
To use PERIM3 or c_perim3, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
threed,
curve3,
fence3,
frst3,
line3,
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
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

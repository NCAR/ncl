.\"
.\"	$Id: threed.m,v 1.1.1.1 1992-04-17 22:30:31 ncargd Exp $
.\"
.TH THREED 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsNA " THREED - 3-d line drawing package
.dsS1 " CALL SET3 (XA,XB,YA,YB,UC,UD,VC,VD,WC,WD,EYE)
.dsS2 " CALL CURVE3 (U,V,W,N)
.dsS3 " CALL LINE3 (UA,VA,WA,UB,VB,WB)
.dsS4 " CALL FRST3 (U,V,W)
.dsS5 " CALL VECT3 (U,V,W)
.dsS6 " CALL POINT3 (U,V,W)
.dsS7 " CALL PERIM3 (MAGR1,MINR2,MAGR2,MINR2,IWHICH,VAR)
.dsS8 " CALL TICK43 (MAGU,MINU,MAGV,MINV,MAGW,MINW)
.dsS9 " CALL FENCE3 (U,V,W,N,IOREN,BOT)
.nr sN 9
.\" print with tsi
.tr ~
.pn 387
.PH ""
.PF ""
.SK 1
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9THREED\s11@"
.EF "@\s\fB11%\s9\fR@@August 1987\s11@"
.OF "@\s9August 1987@@\s11\fB%\fR@"
.de hD          \" Heading macro level one
.br
.ne 5
.sp 2
.ps +3
.ft B           \" boldface, 14 pt.
\\$1
.ft R
.ps -3
.sp 
..
.de >>          \" display for indented lines
.in +.25i       \" usage: .>>
.sp
.nf
..              
.de <<          \" end of display for indented lines
.fi
.in -.25i       \" usage: .<<
.sp
..              
.de sf          \"start fortran (constant spacing)
.ps 10
.vs 12
.nf
.ft L
..
.de ef          \"end fortran (resume variable spacing & prev. size & font)
.ft
.fi
.ps
.vs
..
.br
.S 14
.S 11
.R
.H 3 "Latest Revision"
August 1987
.H 3 "Purpose"
THREED is a package of subroutines that
provides line drawing capabilities in
three-space.
.H 3 "Usage"
Each entry point in this package is
described below.

SET3 (XA,XB,YA,YB,UC,UD,VC,VD,WC,WD,EYE)
.in +.25i

XA, XB, YA, YB  define the portion of the
plotting surface into which the user's
plot will be placed.  These values should
be in the range 0. to 1.  For example, if
one wants the plot to occupy the maximum
plotting surface, set XA=0., YA=0., XB=1.,
YB=1.; if one wants the plot to appear in
the lower left corner of the plotting
surface, set XA=0., YA=0., XB=.5, YB=.5 .

UC, UD, VC, VD, WC, and WD define a
volume in user-coordinate space which
will be transformed onto the plotting
surface defined by XA, XB, YA, YB.

EYE is an array, 3 words long, containing the
U, V, and W coordinates of the EYE position.
All lines in the plot are drawn as viewed
from the EYE.  EYE is specified in user
coordinates and should be outside the box
defined by UC, UD, VC, VD, WC, and WD.
.sp
.ti -.25i
CURVE3 (U,V,W,N)

Draws a curve through N points.  The
points are defined by the linear arrays
U, V, and W, which are dimensioned N or
greater.

.ti -.25i
LINE3 (UA,VA,WA,UB,VB,WB)

Draws a line connecting the coordinates
(UA,VA,WA)  and  (UB,VB,WB).

.ti -.25i
FRST3 (U,V,W)

Positions the pen to (U,V,W).
.ti -.25i

VECT3 (U,V,W)

Draws a line between the current pen
position and the point (U,V,W).  The
current pen position becomes (U,V,W).
Note that a curve can be drawn by using
a FRST3 call followed by a sequence of
VECT3 calls.

.ti -.25i
.br
.ne 3
POINT3 (U,V,W)

Plots a point at (U,V,W).

.ti -.25i
PERIM3 (MAGR1,MINR1,MAGR2,MINR2,IWHICH,VAR)

Draws a perimeter with tick marks.

IWHICH designates the normal vector to the
perimeter drawn (1=U, 2=V, 3=W).

VAR is the value on the axis specified by
IWHICH where the perimeter is to be drawn.

MAGR1  and  MAGR2  specify the
number of major tick marks to be drawn in
the two coordinate directions.

MINR1  and  MINR2  specify the number
of minor ticks between each major tick.

MAGR1, MAGR2, MINR1 and MINR2
are specified by the number
of divisions (holes), not the number of
ticks.  So if MAGR1=1, there would be no
major divisions.

.ti -.25i
TICK43 (MAGU,MINU,MAGV,MINV,MAGW,MINW)

TICK43 allows program control of tick
mark length in subroutine PERIM3.
MAGU, MAGV, MAGW specify the length,
in plotter address units of major
division tick marks on the U, V, and W
axes.  MINU, MINV, MINW specify the length,
in plotter address units of minor
division tick marks on the U, V, and
W axes.
.ti -.25i

FENCE3 (U,V,W,N,IOREN,BOT)

This entry is used to draw a line in three-space 
as well as a "fence" between the
line and a plane normal to one of the
coordinate axes.

The arguments U, V, W, and N
are the same as for CURVE3, described above.

IOREN specifies the direction in which the
fence lines are to be drawn (1 indicates
parallel to the U-axis, 2 indicates parallel
to the V-axis, and 3 indicates parallel to
to the W-axis.)

BOT specifies where the bottom of the fence
is to be drawn.
If the fence lines are to be drawn parallel
to the W-axis, and  BOT=2., then the bottom
of the fence would be the plane  W=2.
.in -.25i
.H 3 "Note"
For drawing characters in conjunction
with THREED, use the companion routine
PWRZT.
.H 3 "Entry Points"
FENCE3, TRN32T, FRST3, VECT3, LIN3,
POINT3, CURVE3, PSYM3, PERIM3, LINE3W,
DRAWT, TICK43, TICK3, THREBD
.H 3 "Common Blocks"
TEMPR, SET31, PWRZ1T, TCK31, PRM31, THRINT
.H 3 "Required Library Routines"
PWRZT and the SPPS
.H 3 "Required GKS Level"
0A
.H 3 "History"
Written and standardized in November 1973.
.H 3 "I/O"
Plots lines.
.H 3 "Precision"
Single
.H 3 "Language"
FORTRAN 77
.H 3 "Accuracy"
+ or -.5 plotter address units per call.
There is no cumulative error.
.H 3 "Portability"
ANSI FORTRAN 77


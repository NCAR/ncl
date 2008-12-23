.TH NGDOTS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGDOTS - Draws filled circular dots at coordinate positions 
(X(I),Y(I),I=1,NUM) at size SIZE with color given by the color index ICOLOR.
.SH SYNOPSIS
CALL NGDOTS(X,Y,NUM,SIZE,ICOLOR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ngdots(float *x, float *y, int num, float size, 
.br 
int icolor)
.SH DESCRIPTION 
.IP X 12
(an input array of type REAL) defining the X world coordinates where a 
sequence of circular filled dots is to be drawn.
.IP Y 12
(an input array of type REAL) defining the Y world coordinates where a 
sequence of circular filled dots is to be drawn.
.IP NUM 12
(an input parameter of type INTEGER) the value of which is the number 
of dots to be drawn.
.IP SIZE 12
(an input parameter of type REAL) the value of which is the size, in 
world coordinate Y-axis units, of the diameter of each dot to be drawn.
.IP ICOLOR 12
(an input parameter of type INTEGER) the value of which is the GKS 
color index specifying what color the dots will be.
.SH C-BINDING DESCRIPTION
The C binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
The coordinates must be world coordinates and not
user coordinates.  NGDOTS does not respect the log scaling or
axis reversal options of the SET call and will report a warning
if these are not set to their default values.
.sp
The dots are scaled appropriately so that they will be circular
when the normalization transformation does not preserve aspect
ratio.
.sp
The algorithm is to construct a single circle and then translate
it to the various coordinate positions.  The original circle is
computed by using trig functions to get points in the first
octant, and then using symmetries to get the rest of the points
on the circle.
.sp
The number of points used for the circle is adjusted depending
on the relative size of the circle.  The maximum number of points
is 512 and the minimum is 8.
.SH EXAMPLES
Assuming that normalization transformation 0 is in effect and
that color index 2 has been defined as red in a call to GSCR, then
.nf

        CALL NGDOTS(.5,.25,1,.1,2)

.fi
would draw a single red dot at world coordinate position (.5,.25)
with a diameter of \&.1 .
.sp
Use the ncargex command to see the following relevant
example: 
fngngdts.
.SH ACCESS
To use NGDOTS or c_ngdots, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
Warning messages will be issued if one calls NGDOTS after having
called SET with any final argument other than "1".
.SH SEE ALSO
Online:
gpm(3NCARG),
point(3NCARG),
points(3NCARG),
set(3NCARG),
ncarg_cbind(3NCARG)
.sp
Online URL: http://ngwww.ucar.edu/ngdoc/ng/gks/gkshome.html
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version;
"The Use of X/Y Coordinates in NCAR Graphics" SCD User Document
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

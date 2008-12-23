.TH VECT3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
VECT3 -
Defines the second and all subsequent points in a sequence of
points connected by straight-line segments, the
projections of which are to be drawn. (FRST3 is
called for the first point in the sequence and
VECT3 is called for the rest.)
.SH SYNOPSIS
CALL VECT3 (U,V,W)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_vect3 (float u, float v, float w)
.SH DESCRIPTION 
.IP "U,V,W" 12
(input expressions of type REAL) are the coordinates of a point in 3-space
to the projection of which a line is to be drawn.
.PP
Each call to FRST3 moves a conceptual "pen" to the new starting point for a
sequence of line draws.  Each call to VECT3 draws a line from the current pen
position to a new pen position and then makes that the current pen position.
.PP
Line segments are drawn in the current polyline color, as determined
by the last call to the GKS routine GSPLCI; by default, color index 1 is
used.  Line width is determined by the last call to the GKS routine GSLWSC;
by default, the value of the line width scale factor is 1.
.PP
Note that the routines FRST3 and VECT3 do not flush the SPPS pen-move buffer.
In fact, to have them do so would entirely defeat the purpose of that buffer.
Therefore, if your last call was to the routine VECT3 and you are about to
call GKS routines to change color or line width, you must first call the
SPPS routine PLOTIF to flush the buffer; otherwise, the color and line
width changes will affect line draws flushed from the buffer later.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples:
fthex01,
fthex02,
fthex03.
.SH ACCESS
To use VECT3 or c_vect3, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
set3,
threed,
tick3,
tick43,
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

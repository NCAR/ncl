.TH TICK43 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TICK43 - Gives user control of tick mark length in PERIM3.
.SH SYNOPSIS
CALL TICK43 (MAJORU,MINORU,MAJORV,MINORV,MAJORW,MINORW)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tick43 (int majoru, int minoru, int majorv, int minorv,
.br
int majorw, int minorw)
.SH DESCRIPTION 
.IP "MAJORU" 12
(an input expression of type INTEGER) specifies the length of major ticks
to be used on perimeter sides that are parallel to the U axis.
.IP "MINORU" 12
(an input expression of type INTEGER) specifies the length of minor ticks
to be used on perimeter sides that are parallel to the U axis.
.IP "MAJORV,MINORV" 12
(input expressions of type INTEGER) specify the lengths of major and minor
ticks for perimeter sides parallel to the V axis.
.IP "MAJORW,MINORW" 12
(input expressions of type INTEGER) specify the lengths of major and minor
ticks for perimeter sides parallel to the W axis.
.PP
All tick lengths are specified as integers between 0 and 1024.  The value
"n" produces a tick that is n/1024ths as long as the longest side of the
box defined by the arguments UMIN, UMAX, VMIN, VMAX, WMIN, and WMAX in
the last call to SET3.  The default values are 12 for a major tick and 8
for a minor tick.
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
To use TICK43 or c_tick43, load the NCAR Graphics libraries ncarg, ncarg_gks,
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

.TH PERIM3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PERIM3 - Draws a perimeter with tick marks.
.SH SYNOPSIS
CALL PERIM3 (MAGR1,MINR2,MAGR2,MINR2,IWHICH,VAR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_perim3 (int magr1, int minr2, int magr2, int minr2, 
.br
int iwhich, float var)
.SH DESCRIPTION 
.IP "MAGR1, MAGR2" 12
Specify the number of major tick marks to be
drawn in the two coordinate directions.
.IP "MINR1, MINR2" 12 
Specify the number of minor ticks between each
major tick.
.IP "MAGR1, MAGR2, MINR1, MINR2" 12
Are specified by the number of
divisions (holes), not the number of ticks. So if
MAGR1=1, there would be no major divisions.
.IP IWHICH 12
Designates the normal vector to the perimeter drawn
(1=U, 2=V, 3=W).
.IP VAR 12
Is the value on the axis specified by IWHICH where
the perimeter is to be drawn.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: 
tthree.
.SH ACCESS
To use PERIM3, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_perim3, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
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
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

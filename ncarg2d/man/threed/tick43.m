.TH TICK43 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TICK43 - Allows control of tick mark length.
.SH SYNOPSIS
CALL TICK43 (MAGU,MINU,MAGV,MINV,MAGW,MINW)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tick43 (int magu, int minu, int magv, int minv, \\
.br
int magw, int minw)
.SH DESCRIPTION 
.IP "MAGU, MAGV, MAGW" 12
Specify the length, in plotter address units of
major division tick marks on  the U, V, and W axes.
.IP "MINU, MINV, MINW" 12
Specify the length, in plotter address units
of minor division tick marks on  the U, V, and W axes.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TICK43, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_tick43, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
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
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

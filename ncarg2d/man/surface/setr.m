.TH SETR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SETR - 
Establishes certain constants so that SRFACE
produces a picture whose size changes with respect to the
viewer's distance from the object.  It can also be used
when making a movie of an object evolving in time to keep
it positioned properly on the screen, saving computer time
in the bargin.  Call it with r0 negative to turn off this
feature.
.SH SYNOPSIS
SUBROUTINE SETR (XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,R0)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_setr (float xmin, float xmax, float ymin, float ymax,
.br
float zmin, float zmax, float r0)
.SH DESCRIPTION 
.IP "XMIN,XMAX" 12
Specifies the range of X array that will be passed to SRFACE.
.IP "YMIN,YMAX" 12
Specifies the range of Y array that will be passed to SRFACE.
.IP "ZMIN,ZMAX" 12
Specifies the range of Z array that will be passed to SRFACE.
If a movie is being
made of an evolving Z array, ZMIN and ZMAX
should contain range of the union of all the Z
arrays.  They need not be exact.
.IP R0 12
Distance between observer and point looked at
when the picture is to fill the screen when
viewed from the direction which makes the picture
biggest.  If R0 is not positive, then the
relative size feature is turned off, and subsequent
pictures will fill the screen.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use SETR, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_setr, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
surface,
surface_params,
ezsrfc,
pwrzs,
srface.
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

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
To use SETR or c_setr, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
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
Copyright (C) 1987-2005
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.

'\" t
.TH SFNORM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SFNORM - The subroutines SFWRLD and SFNORM are used to fill
that portion of the plotter frame inside the area defined by a
given polygonal boundary.
.SH SYNOPSIS
CALL SFNORM (XRA,YRA,NRA,DST,NST,IND,NND)
.br
CALL SFWRLD (XRA,YRA,NRA,DST,NST,IND,NND)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_sfnorm (float *xra, float *yra, int nra, 
.br
float *dst, 
int nst, int *ind, int nnd)
.sp
void c_sfwrld (float *xra, float *yra, int nra, 
.br
float *dst, 
int nst, int *ind, int nnd)
.SH DESCRIPTION 
.IP XRA 12 
(an input/output array of type REAL, dimensioned NRA) contains 
the X coordinates of the points defining the area to be filled, 
in the user coordinate system (if SFWRLD is called) or in the
fractional coordinate system (if SFNORM is called). Upon 
return from SFWRLD, the contents of XRA will have been 
converted to the fractional coordinate system.
.IP YRA 12
(an input/output array of type REAL, dimensioned NRA) contains 
the Y coordinates of the points defining the area to be filled, 
in the user coordinate system (if SFWRLD is called) or in the
fractional coordinate system (if SFNORM is called). Upon 
return from SFWRLD, the contents of YRA will have been 
converted to the fractional coordinate system.
.IP NRA 12
(an input expression of type INTEGER) is the number of points
defining the area to be filled. NRA must be greater than two.
.IP DST(NST) 12
(a scratch array of type REAL, dimensioned NST or greater) is 
for use by the fill algorithm.
.IP NST 12
(an input expression of type INTEGER) is the length of the 
array DST. It must be greater than or equal to NRA + NIM, where 
NIM is the largest number of intersection points of any fill 
line with the boundary lines. To be sure DST is large enough, 
use NIM = NRA; in practice, NIM rarely needs to be that large. 
For a convex polygon, for example, NIM = 2 suffices.
.IP IND
(a scratch array of type INTEGER, dimensioned NND or greater) 
is for use by the fill algorithm.
.IP NND 12
(an input expression of type INTEGER) is the length of the 
array IND. It must be greater than or equal to NRA + 2 * NIM, 
where NIM is as defined above.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the 
FORTRAN argument descriptions.
.SH USAGE
Both SFWRLD and SFNORM fill the area defined by the points
(XRA(I),YRA(I)), for I from 1 to NRA. The lines connecting
point 1 to point 2, point 2 to point 3, ..., point NRA-1 to
point NRA, and point NRA to point 1 bound the area to be
filled. The default values of SOFTFILL's internal parameters
cause fill to be done with solid, horizontal lines .00125
normalized-device-coordinate units apart.
.sp
Use SFWRLD if the arrays XRA and YRA contain world coordinates.
Use SFNORM if XRA and YRA contain normalized device
coordinates. Since SFWRLD transforms XRA and YRA from world
coordinates to normalized device coordinates, any subsequent
calls with those arrays should be to SFNORM (for example, to
create a cross-hatched effect).
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
ccpscam,
cpex01,
cpex02,
cpex03,
sfex01,
tsoftf.
.SH ACCESS
To use SFWRLD, SFNORM, c_sfwrld, or c_sfnorm, load the NCAR Graphics libraries 
ncarg, ncarg_gks, and ncarg_c, preferably in that order.
.SH MESSAGES
See the softfill man page for a description of all Softfill
error messages and/or informational messages.
.SH SEE ALSO
Online: 
softfill, softfill_params, sfgetc, sfgeti, sfgetp, sfgetr,
sfsetc, sfseti, sfsetp, sfsetr, sfsgfa, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

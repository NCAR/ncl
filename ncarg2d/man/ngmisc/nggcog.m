.TH NGGCOG 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGGCOG - Returns the latitudes and longitudes of a set of points approximating
a circle at a given point on the surface of the globe.
.SH SYNOPSIS
CALL NGGCOG (CLAT,CLON,CRAD,ALAT,ALON,NPTS)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_nggcog(float clat, float clon, float crad, float *alat, 
.br
float *alon, int npts)
.SH DESCRIPTION 
.IP CLAT 12
(an input expression of type REAL) is the latitude, in degrees, of a point
on the globe defining the center of the circle.
.IP CLON 12
(an input expression of type REAL) is the longitude, in degrees, of a point
on the globe defining the center of the circle.
.IP CRAD 12
(an input expression of type REAL) specifies the radius of the circle.  This
is given as a great-circle distance, in degrees.
.IP ALAT 12
(an output array, of type REAL, dimensioned NPTS) is an array in which the
latitudes of points on the circle are to be returned.
.IP ALON 12
(an output array, of type REAL, dimensioned NPTS) is an array in which the
longitudes of points on the circle are to be returned.
.IP NPTS 12
(an input expression, of type INTEGER) is the desired number of points to be
used to represent the circle.  Its value determines how accurately the circle
will be represented.
.SH C-BINDING DESCRIPTION
The C binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
Let C represent (CLAT,CLON) and let O represent the center of the globe.
The circle is the set of all points P on the globe such that the angle POC
is of the size specified by CRAD.
.sp
SIN and COS are used to generate points representing a circle having the
desired radius and centered at the North Pole.  These points are then
subjected to two rotations - one that brings the circle down to the desired
latitude, and another that carries it to the desired longitude.
.SH EXAMPLES
.sp
Use the ncargex command to see the following relevant
example: 
cpex10.
.SH ACCESS
To use NGGCOG or c_nggcog, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
None.
.SH SEE ALSO
Online:
nggsog(3NCARG),
ngritd(3NCARG).
.SH COPYRIGHT
Copyright (C) 1987-2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.

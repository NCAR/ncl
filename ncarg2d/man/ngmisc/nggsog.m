.TH NGGSOG 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGGSOG - Returns the latitudes and longitudes of six points defining a
five-pointed star at a given point on the surface of the globe.
.SH SYNOPSIS
CALL NGGSOG (SLAT,SLON,SRAD,ALAT,ALON)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_nggsog(float slat, float slon, float srad, float *alat,
.br
float *alon)
.SH DESCRIPTION 
.IP SLAT 12
(an input expression of type REAL) is the latitude, in degrees, of a point
on the globe defining the center of the star.
.IP SLON 12
(an input expression of type REAL) is the longitude, in degrees, of a point
on the globe defining the center of the star.
.IP SRAD 12
(an input expression of type REAL) specifies the great-circle distance, in
degrees, from the center of the star to the end of one of its points.
.IP ALAT 12
(an output array, of type REAL, dimensioned 6) is an array in which the
latitudes of points on the star are to be returned.
.IP ALON 12
(an output array, of type REAL, dimensioned 6) is an array in which the
longitudes of points on the star are to be returned.
.SH C-BINDING DESCRIPTION
The C binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
Let C represent (CLAT,CLON), let O represent the center of the globe, and
let P represent the end of one point of the star.  Then, the angle POC has
the magnitude specified by SRAD.
.sp
SIN, COS, and TAN are used to generate points representing a star having the
desired radius and centered at the the point with latitude 0 and longitude 0.
These points are then subjected to two rotations - one that brings the
star up to the desired latitude, and another that carries it to the desired
longitude.
.sp
NGGSOG is intended to be used for relatively small stars marking points of
interest on the surface of the globe; using SRAD = 90 degrees is guaranteed
to get you into trouble and SRAD < 10 degrees is recommended.
.SH EXAMPLES
.sp
Use the ncargex command to see the following relevant
example: 
cpex10.
.SH ACCESS
To use NGGSOG or c_nggsog, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
None.
.SH SEE ALSO
Online:
nggcog(3NCARG),
ngritd(3NCARG).
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

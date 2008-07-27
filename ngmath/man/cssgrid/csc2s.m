.\"
.\"	$Id: csc2s.m,v 1.4 2008-07-27 03:35:35 haley Exp $
.\"
.TH CSC2S 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.SH NAME
CSC2S - convert from  Cartesian to  lat/lon coordinates
.SH SYNOPSIS
CALL CSC2S (N, X, Y, Z, RLAT, RLON) 
.SH DESCRIPTION
.IP N 12
(integer, input) The number of coordinates to convert.
.IP X,Y,Z 12
(real, input) The Cartesian coordinates of the input points. 
.IP RLAT 12
(real, output) The latitudes of the output coordinates, in degrees.
.IP RLON 12
(real, output) The longitudes of the output coordinates, in degrees.
.SH USAGE
CSC2S is called to find equivalent latitude and 
longitude coordinates to specified Cartesian coordinates
on the unit sphere. 
The coordinate (1.,0.,0.) is mapped to the
latitude/longitude coordinate (0.,0.). The latitude/longitude 
coordinates are returned in degrees. 
.SH ACCESS
To use CSC2S, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
cssgrid,
css2c.
.sp
Complete documentation for Cssgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/cssgrid/csshome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.

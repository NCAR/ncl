.\"
.\"	$Id: csc2sd.m,v 1.4 2008-07-27 03:35:35 haley Exp $
.\"
.TH CSC2SD 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.SH NAME
CSC2SD - convert from  Cartesian to  lat/lon coordinates
.SH SYNOPSIS
CALL CSC2SD (N, X, Y, Z, RLAT, RLON) 
.SH DESCRIPTION
.IP N 12
(integer, input) The number of coordinates to convert.
.IP X,Y,Z 12
(double precision, input) The Cartesian coordinates of the input points. 
.IP RLAT 12
(double precision, output) The latitudes of the output coordinates, in
degrees.
.IP RLON 12
(double precision, output) The longitudes of the output coordinates, in
degrees.
.SH USAGE
CSC2SD is called to find equivalent latitude and 
longitude coordinates to specified Cartesian coordinates
on the unit sphere. 
The coordinate (1.,0.,0.) is mapped to the
latitude/longitude coordinate (0.,0.). The latitude/longitude 
coordinates are returned in degrees. 
CSC2SD is a double precision version of CSC2S.
.SH ACCESS
To use CSC2SD, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
cssgrid,
css2cd,
csc2s.
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

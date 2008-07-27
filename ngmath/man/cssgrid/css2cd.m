.\"
.\"	$Id: css2cd.m,v 1.4 2008-07-27 03:35:35 haley Exp $
.\"
.TH CSS2CD 3NCARG "MAY 2000" UNIX "NCAR GRAPHICS"
.SH NAME
CSS2CD - convert from lat/lon coordinates to Cartesian coordinates.
.SH SYNOPSIS
CALL CSS2CD (N, RLAT, RLON, X, Y, Z)
.SH DESCRIPTION
.IP N 12
(integer, input) The number of input lat/lon coordinates.
.IP RLAT 12
(double precision, input) An array containing the latitudes of the input coordinates. 
.IP RLON 12
(double precision, input) An array containing the longitudes of the input coordinates. 
.IP X 12
(double precision, output) An array containing the X component of the Cartesian 
coordinates of the output data. (X(I),Y(I),Z(I)) is
the Cartesian coordinate corresponding to the lat/lon 
coordinate (RLAT(I),RLON(I)) for I=1 to N. 
.IP Y 12
(double precision, output) An array containing the Y component of the Cartesian 
coordinates of the output data. (X(I),Y(I),Z(I)) is
the Cartesian coordinate corresponding to the lat/lon 
coordinate (RLAT(I),RLON(I)) for I=1 to N. 
.IP Z 12
(double precision, output) An array containing the Z component of the Cartesian 
coordinates of the output data. (X(I),Y(I),Z(I)) is
the Cartesian coordinate corresponding to the lat/lon 
coordinate (RLAT(I),RLON(I)) for I=1 to N. 
.SH USAGE
CSS2CD is called to find the equivalent Cartesian coordinates on a
unit sphere to specified latitude and longitude coordinates.
The coordinate of 0. latitude and 0. longitude is
converted to Cartesian coordinate (1.,0.,0.). Latitudes and 
longitudes are assumed to be in degrees. 
CSS2CD is a double precision verson of CSS2C.
.SH ACCESS
To use CSS2CD, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
cssgrid,
csc2sd,
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

.\"
.\"	$Id: css2c.m,v 1.2 2000-07-13 03:17:50 haley Exp $
.\"
.TH CSS2C 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.SH NAME
CSS2C - convert from  Cartesian to  lat/lon coordinates
.SH SYNOPSIS
CALL CSS2C (N, RLAT, RLON, X, Y, Z) 
.SH DESCRIPTION
.IP N 12
(integer, input) The number of coordinates to convert.
.IP RLAT 12
(real, input) The latitudes of the input coordinates, in degrees.
.IP RLON 12
(real, input) The longitudes of the input coordinates, in degrees.
.IP X,Y,Z 12
(real, output) The Cartesian coordinates of the output points. 
.SH USAGE
CSS2C is called to find equivalent Cartesian coordinates
on the unit sphere to latitude and 
longitude coordinates.
The coordinate 0. degrees latitude and 0. degrees longitude 
is mapped to the Cartesian coordinate (1., 0.,0.). 
.SH ACCESS
To use CSS2C, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
cssgrid,
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


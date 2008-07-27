.\"
.\"	$Id: cssgrid.m,v 1.5 2008-07-27 03:35:35 haley Exp $
.\"
.TH CSSGRID 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.SH NAME
CSSGRID - tension spline interpolation on a sphere.
.SH SYNOPSIS
CALL CSSGRID (N, RLAT, RLON, F, NI, NJ, PLAT, PLON, FF, IWK, RWK, IER)
.SH DESCRIPTION
.IP N 12
(integer,input) The number of input data points (N > 2). 
.IP RLAT 12
(real, input) An array containing the latitudes
of the input data, expressed in degrees.
The first three points must not be collinear
(lie on a common great circle).
.IP RLON 12
(real, input) An array containing the longitudes of the input data,
expressed in degrees.
.IP F 12
(real, input) An array containing data values. F(I) is a functional 
value at (RLAT(I),RLON(I)) for I = 1 to N. 
.IP NI 12
(integer, input) The number of rows in the uniform output grid. NI can be 1. 
.IP NJ 12
(integer, input) The number of columns in the uniform output grid. NJ can be 1. 
.IP PLAT 12
(real, intput) An array of length NI 
containing the latitudes the output grid lines. 
The values in PLAT should be in degrees.
.IP PLON 12
(real, intput) An array of length NJ 
containing the longitudes the output grid lines. 
The value in PLON should be in degrees.
.IP FF 12
(real, output) An NI by NJ array containing the desired 
interpolated values. FF(I,J) is the interpolated value at 
the coordinate specified by PLAT(I) and PLON(J) for I = 1 
to NI and J = 1 to NJ. 
.IP IWK 12 
(integer, input) An integer workspace of length 27*N. 
.IP RWK 12
(double precision, input) A work array dimensioned for 13*N.  Note
that RWK must be typed DOUBLE PRECISION.
.IP IER 12
(integer, output) An error return value.  If IER is returned as 0, then
no errors were detected. If IER is non-zero, then refer to the man
page for cssgrid_errors for details.
.SH USAGE
CSSGRID is called to find an interpolating tension
spline for data randomly positioned on a sphere.
.SH ACCESS
To use CSSGRID, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
csstri,
cssgrid_errors
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

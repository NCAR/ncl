.\"
.\"	$Id: surf2.m,v 1.2 2000-07-13 03:18:03 haley Exp $
.\"
.TH SURF2 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
SURF2 - 2D interpolation for gridded data
.SH SYNOPSIS
FUNCTION SURF2 (XX, YY, M, N, X, Y, Z, IZ, ZP, SIGMA) 
.sp
This function interpolates a surface value at a specified coordinate
using bi-splines under tension. SURF1 must be called before
invoking SURF2. The desired interpolated value is returned as the
value of the function. 
.SH DESCRIPTION
.IP XX 12
(real, input) Contains the X coordinate of a point to be mapped onto the
interpolated surface. 
.IP YY 12
(real, input) Contains the Y coordinate of a point to be mapped onto the
interpolated surface. 
.IP M 12
(integer, input) The number of grid lines in the X direction. (M > 1) 
.IP N 12
(integer, input) The number of grid lines in the Y direction. (N > 1) 
.IP X 12
(real, input) An array containing M X coordinates for grid lines in the X
direction. These values must be strictly increasing. 
.IP Y 12
(real, input) An array containing N Y coordinates for grid lines in the Y
direction. These values must be strictly increasing. 
.IP Z 12
(real, input) An array containing M x N functional values at the grid points;
Z(I,J) contains the functional value at (X(I),Y(J)) for I=1,M and
J=1,N. 
.IP IZ 12
(integer, input) The row dimension of the matrix Z (IZ is greater than or equal
to M). 
.IP ZP 12
(real, input) An array of size M x N x 3. 
.IP SIGMA 12
(real, input) Tension factor. Values near zero result in a cubic spline; large
values (e.g. 50) result in nearly a polygonal line. A typical value
is 1. 
.SH RETURN VALUE
SURF2 returns the interpolated value at the specified point T.
.SH ACCESS
To use SURF2, load the NCAR Graphics library ngmath.
.SH SEE ALSO
surf1,
fitgrid_params.
.sp
Complete documentation for Fitgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/fitgrid/fithome.html
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


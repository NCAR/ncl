.\"
.\"	$Id: curvp2.m,v 1.2 2000-07-13 03:18:01 haley Exp $
.\"
.TH CURVP2 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
CURVP2 - interpolate a periodic function at a specified point
.SH SYNOPSIS
FUNCTION CURVP2 (T, N, X, Y, P, YP, SIGMA)
.sp
This function interpolates a value at a specified point using a spline 
under tension.  CURVP1 must be called prior to calling CURVP2, and the 
values thus obtained used as input to CURVP2. CURVP2 returns the interpolated 
function value at the X-coordinate specified. 
.SH DESCRIPTION
.IP T 12
(real, input) The abscissa for which an interpolated function value is desired. 
.IP N 12
(integer, input) The number of input data values. (N > 1) 
.IP X 12
(real, input) An array containing the abscissae for the input function. 
.IP Y 12
(real, input) An array containing the functional values of the input 
data -- Y(K) is the functional value at X(K) for K=1,N. 
.IP X 12
(real, input) The period of the function. 
.IP YP 12
(real, input) Contains values for the second derivative 
(as calculated by CURV1). 
.IP SIGMA 12
(real, input) Tension factor. Values near zero result in a cubic spline; 
large values (e.g. 50) result in nearly a polygonal line. A typical value is 1. 
.SH RETURN VALUE
CURVP2 returns the interpolated value at the specified point T.
.SH ACCESS
To use CURVP2, load the NCAR Graphics library ngmath.
.SH SEE ALSO
curvp1,
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


.\"
.\"	$Id: curvpi.m,v 1.3 2000-08-22 15:14:52 haley Exp $
.\"
.TH CURVPI 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
CURVPI - integrate a periodic spline curve
.SH SYNOPSIS
FUNCTION CURVPI (XL, XU, N, X, Y, P, YP, SIGMA)
.sp
This function calculates an integral between two specified limits. 
CURVP1 must be called prior to calling CURVPI, and the values thus 
obtained used as input to CURVPI.  The value of the integral is returned. 
.SH DESCRIPTION
.IP XL 12
(real, input) The lower limit of the integration. 
.IP XR 12
(real, input) The upper limit of the integration. 
.IP N 12
(real, input) The number of input data values. (N > 1) 
.IP X 12
(real, input) An array containing the abscissae for the input function. 
.IP Y 12
(real, input) An array containing the functional values of the 
input function -- Y(K) is the (real, input) functional value at X(K) for K=1,N. 
.IP P 12
(real, input) The period of the function. 
.IP YP 12
(real, input) Contains values for the second derivative 
(as calculated by CURV1). 
.IP SIGMA 12
(real, input) Tension factor. Values near zero result in a cubic spline; 
large values (e.g. 50) result in nearly a polygonal line. A typical value is 1. 
.SH RETURN VALUE
CURVPI returns the integral of the interpolated curve between the specified
limits.
.SH ACCESS
To use CURVPI, load the NCAR Graphics library ngmath.
.SH SEE ALSO
curv1,
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
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.


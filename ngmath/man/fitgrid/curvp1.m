.\"
.\"	$Id: curvp1.m,v 1.3 2000-08-22 15:14:51 haley Exp $
.\"
.TH CURVP1 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
CURVP1 - calculate values for CURVP2 usage.
.SH SYNOPSIS
CALL CURVP1 (N, X, Y, P, YP, TEMP, SIGMA, IER)
.sp
This subroutine calculates certain values that are used by CURVP2 in 
order to compute an interpolatory spline under tension through a sequence 
of functional values for a periodic function. The actual computation of 
the interpolated values must be done using CURVP2 . 
.SH DESCRIPTION
.IP N 12
(integer, input) The number of input data values. (N > 1) 
.IP X 12
(real, input) An array containing the abscissae for the input function. 
.IP Y 12
(real, input) An array containing the functional values of the 
input function -- Y(K) is the functional value at X(K) for K=1,N. 
.IP P 12
(real, input) The period of the function. P must be greater than X(N)-X(1). 
.IP YP 12
(real, output) Contains values for the second derivative (these are 
calculated by CURVP1). 
.IP TEMP 12
(real, input) Scratch space. 
.IP SIGMA 12
(integer, input) Tension factor. Values near zero result in a cubic spline; 
large values (e.g. 50) result in nearly a polygonal line. A typical value is 1. 
.IP IER 12
(integer, output) An error return value. If IER is returned as 0, then 
no errors were detected. 
.sp
.br
= 1 if N is less than 2. 
.br
= 2 if P is less than or equal to X(N)-X(1). 
.br
= 3 if the X values are not strictly increasing. 
.SH ACCESS
To use CURVP1, load the NCAR Graphics library ngmath.
.SH SEE ALSO
curvp2,
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


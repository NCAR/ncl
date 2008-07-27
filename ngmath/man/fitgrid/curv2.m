.\"
.\"	$Id: curv2.m,v 1.4 2008-07-27 03:35:38 haley Exp $
.\"
.TH CURV2 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
CURV2 - 1D interpolation for non-periodic functions.
.SH SYNOPSIS
FUNCTION CURV2 (T, N, X, Y, YP, SIGMA)
.sp
This function interpolates a value at a specified point using a spline 
under tension.  CURV1 must be called prior to calling CURV2, and the 
values thus obtained used as input to CURV2. CURV2 returns the interpolated 
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
.IP YP 12
(real, input) Contains values for the second derivative 
(as calculated by CURV1). 
.IP SIGMA 12
(real, input) Tension factor. Values near zero result in a cubic spline; 
large values (e.g. 50) result in nearly a polygonal line. A typical value is 1. 
.SH RETURN VALUE
CURV2 returns the interpolated value at the specified point T.
.SH ACCESS
To use CURV2, load the NCAR Graphics library ngmath.
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

The use of this Software is governed by a License Agreement.

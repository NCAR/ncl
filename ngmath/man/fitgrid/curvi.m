.\"
.\"	$Id: curvi.m,v 1.4 2008-07-27 03:35:38 haley Exp $
.\"
.TH CURVI 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
CURVI - integrate a spline curve
.SH SYNOPSIS
FUNCTION CURVI (XL, XU, N, X, Y, YP, SIGMA)
.sp
This function calculates an integral between two specified limits. 
CURV1 must be called prior to calling CURVI, and the values thus 
obtained used as input to CURVI.  The value of the integral is returned. 
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
.IP YP 12
(real, input) Contains values for the second derivative 
(as calculated by CURV1). 
.IP SIGMA 12
(real, input) Tension factor. Values near zero result in a cubic spline; 
large values (e.g. 50) result in nearly a polygonal line. A typical value is 1. 
.SH RETURN VALUE
CURVI returns the integral of the interpolated curve between the specified
limits.
.SH ACCESS
To use CURVI, load the NCAR Graphics library ngmath.
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

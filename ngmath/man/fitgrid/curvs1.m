.\"
.\"	$Id: curvs1.m,v 1.2 2008-07-27 03:35:39 haley Exp $
.\"
.TH CURVS1 3NCARG "August 2002" UNIX "NCAR GRAPHICS"
.SH NAME
CURVS1 - calculate values for a smoothing spline for data in the plane.
.SH SYNOPSIS
CALL CURVS1 (N,X,Y,D,ISW,S,EPS,PARAM,XS,YS,XSP,YSP,
             SIGMA,TEMP,IERR)
.sp
This subroutine calculates certain values that are used by CURVS2 in order 
to compute an interpolatory smoothing spline under tension through a 
sequence of data values in the plane.  
In general this curve will not pass through the original data points.
The actual computation of the interpolated values must be done using CURVS2. 
.sp
Three parameters are used to control the degree of smoothness -- D, S, and EPS. 
.sp
The parameter D is a value indicating the degree of confidence in the 
accuracy of the input function values -- it should be an approximation of 
the standard deviation of error. Effectively the value of D controls how 
close the smoothed curve comes to the input data points. If D is small 
then the interpolated curve will pass close to the input data. The larger 
the value of D, the more freedom the smooth curve has in how close it
comes to the input data values. 
.sp
S is a more subtle global smoothing parameter. S must be non-negative. 
For small values of S, the interpolated curve approximates the tension 
spline and for larger values of S, the curve is smoother. A reasonable 
value for S is REAL(N). 
.sp
EPS controls the precision to which S is interpreted; EPS must be 
between 0. and 1. inclusive. A reasonable value for EPS is SQRT(2./REAL(N)). 
.SH DESCRIPTION
.IP N 12
(integer, input) The number of input data values. (N > 1) 
.IP X 12
(integer, input) An array containing the X-coordinates for the input data. 
These need not be increasing.
.IP Y 12
(integer, input) An array containing the Y-coordinates for the input data.
.IP D 12
(integer, input) A user-specified value containing the observed weights. D may
be either an array or a scalar, depending on the value of ISW (as
described below). 
.IP ISW 12
(integer, input) A switch for interpreting the value of D. If ISW=0, then D 
is an array of length N (D contains an individual error estimate for
each input data value); if ISW=1, then D is a scalar that serves
as an error estimate for every single data item. 
.IP S 12
(integer, input) Contains the value for smoothing. S must be non-negative.
Larger values for S yield greater smoothing. A reasonable value is REAL(N). 
.IP EPS 12
(integer, input) Contains a tolerance value for the relative precision to 
which S should be interpreted. EPS must be between 0. and 1. inclusive.
A reasonable value is SQRT(2./REAL(N)). 
.IP PARAM 12
(integer, output) PARAM(I) is the arc length of the curve up through
point (X(I),Y(I)), divided by the total arc length.
.IP XS 12
(integer, output) An array of length N.  Contains the smoothed values.
.IP XSP 12
(integer, output) An array of length N.  Contains second derivative
information for the X-coordinate values.
.IP YS 12
(integer, output) An array of length N.  Contains the smoothed values.
.IP YSP 12
(integer, output) An array of length N.  Contains second derivative
information for the X-coordinate values.
.IP SIGMA 12
(integer, input) Tension factor. Values near zero result in a cubic spline; 
large values (e.g. 50) result in nearly a polygonal line. A typical value
is 1. 
.IP TEMP 12
(integer, input) Scratch space of length at least 19*N.
.IP IER 12
(integer, output) An error return value. If IER is returned as 0, then no errors
were detected. 
.sp
= 1 if N is less than 2. 
.br
= 2 if S is negative. 
.br
= 3 if EPS is negative or greater than 1. 
.br
= 5 if D is negative. 
.SH ACCESS
To use CURVS1, load the NCAR Graphics library ngmath.
.SH SEE ALSO
curvs2,
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

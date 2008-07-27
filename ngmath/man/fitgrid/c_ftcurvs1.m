.\"
.\"	$Id: c_ftcurvs1.m,v 1.2 2008-07-27 03:35:38 haley Exp $
.\"
.TH c_ftcurvs1 3NCARG "August 2002" UNIX "NCAR GRAPHICS"
.SH NAME
c_ftcurvs1 - calculate values for a smoothing spline for data in the plane.
.SH FUNCTION PROTOTYPE
int c_ftcurvs1(int, float [], float [], int, float [],
               int, float, float, float [], float []);
.SH SYNOPSIS
int c_ftcurvs1(int n, float xi[], float yi[], int dflg, 
               float d[], int m, float xl, float xr, 
               float xo[], float yo[])
.SH DESCRIPTION
.IP n 12
The number of input data points. (n > 1) 
.IP xi 12
An array containing the X-coordinates for the input data.
These need not be increasing.
.IP yi 12
An array containing the Y-coordinates for the input data.
.IP dflg 12
A switch for interpreting the value of d (as described below). 
If dflg=0, then d is an array of length n (an error estimate for 
each input data value); if dflg=1, then d is a scalar that serves as an error
estimate for every single data item. 
.IP d 12
A user-specified value containing the observed weights. D may
be either an array or a scalar, depending on the value of dflg.
.IP m 12
The number of output values. 
.IP xl 12
A lower limit for an interval of interpolation.  The interval between
zero and one interpolates the entire curve, any value outside this
range results in extrapolation.  The interval [xl,xr] is divided
into m equal values to produce the interpolated curve.
.IP xr 12
An upper limit for an interval of interpolation.  The interval between
zero and one interpolates the entire curve, any value outside this
range results in extrapolation.  The interval [xl,xr] is divided
into m equal values to produce the interpolated curve.
.IP xo 12
The interpolated X values.
.IP yo 12
The interpolated Y values.
.SH RETURN VALUE
c_ftcurvs1 returns an error value as per: 
.br
.sp
= 1 if n is less than 2.
.br
= 2 if smt is negative.
.br
= 3 if eps is negative or greater than 1.
.br
= 5 if d is negative.
.br
.SH USAGE
This function computes an interpolatory smoothing spline under 
tension through a sequence of data values in the plane.
.sp
Two parameters and one function argument are used to control the 
degree of smoothness -- the parameters are smt, and eps and the 
function argument is d. 
.sp
The argument d is a value indicating the degree of confidence in 
the accuracy of the input function values -- it should be an 
approximation of the standard deviation of error. Effectively the value of d
controls how close the smoothed curve comes to the input data points. 
If d is small, then the interpolated curve will pass close to the 
input data. The larger the value of d, the more freedom the smooth curve has
in how close it comes to the input data values. 
.sp
The parameter smt is a more subtle global smoothing parameter; smt must 
be non-negative. For small values of smt, the curve approximates the 
tension spline and for larger values of smt, the curve is
smoother. A reasonable value for smt is (float) n. smt can be set
with the procedure c_ftsetr.
.sp
The parameter eps controls the precision to which smt is interpreted; 
eps must be between 0. and 1. inclusive. A reasonable value for 
eps sqrt( 2./(float) n ). eps can be set
with the procedure c_ftsetr.
.sp
c_ftcurvs1 is called after all of the desired values for control 
parameters have been set using the procedures c_ftseti, c_ftsetr, 
c_ftsetc. Control parameters that apply to c_ftcurvs are: sig, smt, eps, sf2. 
.sp
The value for the parameter sig specifies the tension factor. 
Values near zero result in a cubic spline; large values (e.g. 50) result 
in nearly a polygonal line. A typical value is 1. (the default). 
.SH ACCESS
To use c_ftcurvs1, load the NCAR Graphics library ngmath.
.SH SEE ALSO
fitgrid_params, c_ftseti, c_ftsetr, c_ftsetc.
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

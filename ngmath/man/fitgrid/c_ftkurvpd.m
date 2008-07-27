.\"
.\"	$Id: c_ftkurvpd.m,v 1.4 2008-07-27 03:35:38 haley Exp $
.\"
.TH c_ftkurvpd 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_ftkurvpd - interpolation and derivative calculation for closed parametric curves
.SH FUNCTION PROTOTYPE
int c_ftkurvpd (int, float [], float [], int, float [], 
                float [], float [], float [], float [], 
                float [], float []);
.SH SYNOPSIS
int c_ftkurvpd (n, xi, yi, m, t, xo, yo, xd, yd, xdd, ydd);
.SH DESCRIPTION
.IP n 12
The number of input data points. (n > 1) 
.IP xi 12
An array containing the abscissae for the input function. 
.IP yi 12
An array containing the functional values (y[k] is the functional value 
at x[k] for k=0,n). 
.IP m 12
The number of desired interpolated points. 
.IP t 12
Contains an array of values for the parameter mapping onto the interpolated curve. Any interval [tt,tt+1.] maps onto the entire curve. 
.IP xo 12
An array containing the X values for the interpolated points. 
t[k] maps to (xo[k],yo[k]) for k=0,n-1. 
.IP yo 12
An array containing the Y values for the interpolated points. 
.IP xd 12
Contains the first derivatives of the X component with respect to t. 
.IP yd 12
Contains the first derivatives of the Y component with respect to t. 
.IP xdd 12
Contains the second derivatives of the X component with respect to t. 
.IP ydd 12
Contains the second derivatives of the Y component with respect to t. 
.SH RETURN VALUE
c_ftkurvpd returns an error value as per: 
.br
.sp
= 0 -- no error.
.br
= 1 -- if n is less than 2.
.br
= 2 -- if adjacent coordinate pairs coincide.
.br
.SH USAGE
This procedure behaves like ftkurvp except that in addition it returns 
the first and second derivatives of
the component functions in the parameterization. 
.sp
Given a sequence of distinct input points 
( (x[0],y[0]), ... , (x[n-1],y[n-1]), the interpolated curve is
parameterized by mapping points in the interval [0.,1.] onto the 
interpolated curve. The resulting curve
has a parametric representation both of whose components are splines 
under tension and functions of the
polygonal arc length. The value 0. is mapped onto 
(x[0],y[0]) and the value 1. is mapped onto
(x[0],y[0]) as well (completing the closed curve). 
.sp
c_ftkurvpd is called after all of the desired values for control parameters 
have been set using the procedures c_ftseti, c_ftsetr, c_ftsetc. The 
only control parameter that applies to c_ftkurvpd is: sig. 
.sp
The value for the parameter sig specifies the tension factor. Values 
near zero result in a cubic spline;
large values (e.g. 50) result in nearly a polygonal line. A typical 
value is 1. (the default). 
.SH ACCESS
To use c_ftkurvpd, load the NCAR Graphics library ngmath.
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

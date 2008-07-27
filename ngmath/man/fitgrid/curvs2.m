.\"
.\"	$Id: curvs2.m,v 1.2 2008-07-27 03:35:39 haley Exp $
.\"
.TH CURVS2 3NCARG "August 2002" UNIX "NCAR GRAPHICS"
.SH NAME
CURVS2 - calculates an interpolated value for a smoothing spline under tension 
at a given point.
.SH SYNOPSIS
FUNCTION CURVS2(T,N,PARAM,X,Y,XP,YP,SIGMA,XO,YO)
.sp
This subroutine is a companion to CURV1S which must be
called before calling this subroutine.  CURV2S calculates
an interpolated value for a smoothing spline under tension 
at a given point.  
.SH DESCRIPTION
.IP T 12
(real, input) A real value to be mapped onto the interpolating curve.
Values of T between zero and one interpolate the original
data; any values of T outside this range result in extrapolation.
.IP N 12
(integer, input)  The number of points which were specified to
determine the curve in CURVS1.
.IP PARAM 12
(real, input) The arc lengths as computed by CURVS1.
.IP X 12
(real, input) An array containing the X-coordinate values 
of the specified points.
.IP Y 12
(real, input) An array containing the Y-coordinate values of 
the specified points.
.IP XP 12
(real, input) An array of second derivative values as calculated by CURVS1.
.IP YP 12
(real, input) An array of second derivative values as calculated by CURVS1.
.IP SIGMA 12
(real, input) Tension factor. Values near zero result in a cubic spline; 
large values (e.g. 50) result in nearly a polygonal line. A typical value is 1. 
.IP XO 12
(real, output) The interpolated X value.
.IP YO 12
(real, output) The interpolated Y value.
.SH ACCESS
To use CURV2, load the NCAR Graphics library ngmath.
.SH SEE ALSO
curvs1,
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

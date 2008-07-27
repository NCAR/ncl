.\"
.\"	$Id: kurv2.m,v 1.4 2008-07-27 03:35:39 haley Exp $
.\"
.TH KURV2 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
KURV2 - does interpolation for parametric curves
.SH SYNOPSIS
CALL KURV2 (T, XS, YS, N, X, Y, XP, YP, S, SIGMA) 
.sp
The interpolated curve is parameterized by mapping points in the
interval [0.,1.] onto the interpolated curve. The resulting curve has a
parametric representation both of whose components are splines under
tension and functions of the polygonal arc length. The value 0. is
mapped onto (X[1],Y[1]) and the value 1. is mapped onto
(X[N],Y[N]). KURV1 must be called before calling KURV2. 
.SH DESCRIPTION
.IP T 12
(real, input) A value to be mapped to a point on the curve. The interval
[0.,1.] is mapped onto the curve such that 0. is mapped to
(X(1),Y(1)) and 1. is mapped to (X(N),Y(N)). Values outside of
[0.,1.] result in extrapolation. 
.IP XS 12
(real, oputput) Contains the X coordinate of the interpolated point that T maps
to. 
.IP YS 12
(real, oputput) Contains the Y coordinate of the interpolated point that T maps
to. 
.IP N 12
(integer, input) The number of input data points. (N > 1) 
.IP X 12
(real, input) An array containing the X values of the input points. 
.IP Y 12
(real, input) An array containing the Y values of the input points. Adjacent
pairs of points must be distinct. 
.IP XP 12
(real, input) An array of length N as computed by KURV1. 
.IP YP 12
(real, input) An array of length N as computed by KURV1. 
.IP S 12
(real, input) An array of length N containing the polygonal arc lengths of the
curve. 
.IP SIGMA 12
(real, input) Tension factor. Values near zero result in a cubic spline; large
values (e.g. 50) result in nearly a polygonal line. A typical value is 1. 
.SH ACCESS
To use KURV2, load the NCAR Graphics library ngmath.
.SH SEE ALSO
kurv1,
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

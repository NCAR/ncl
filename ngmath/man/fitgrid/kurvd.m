.\"
.\"	$Id: kurvd.m,v 1.4 2008-07-27 03:35:39 haley Exp $
.\"
.TH KURVD 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
KURVD - interpolation and derivatives for parametric curves
.SH SYNOPSIS
CALL KURVD (T, XS, YS, XST, YST, XSTT, YSTT, N, X, Y, XP, YP, S, SIGMA)
.sp
This subroutine behaves like KURV2 except that in addition it returns
the first and second derivatives of the component functions in the
parameterization. The interval [0.,1.] maps onto a curve in the plane.
The resulting curve has a parametric representation both of whose
components are splines under tension and functions of the polygonal
arc length. KURV1 must be called before calling KURVD. 
.SH DESCRIPTION
.IP T 12
(real,input) A value to be mapped to a point on the curve. The interval
[0.,1.] is mapped onto the curve such that 0. is mapped to
(X(1),Y(1)) and 1. is mapped to (X(N),Y(N)). Values outside of
[0.,1.] result in extrapolation. 
.IP XS 12
(real,output) Contains the X coordinate of the point that T maps to. 
.IP YS 12
(real,output) Contains the Y coordinate of the point that T maps to. 
.IP XST 12
(real,output) Contains the first derivatives of the X component with 
respect to T. 
.IP YST 12
(real,output) Contains the first derivatives of the Y component with 
respect to T. 
.IP XSTT 12
(real,output) Contains the second derivatives of the X component with 
respect to T. 
.IP YSTT 12
(real,output) Contains the second derivatives of the X component with 
respect to T. 
.IP N 12
(integer,input) The number of input data points. (N > 1) 
.IP X 12
(real,input) An array containing the X values of the input points. 
.IP Y 12
(real,input) An array containing the Y values of the input points. Adjacent
pairs of points must be distinct. 
.IP XP 12
(real,input) An array of length N as computed by KURV1. 
.IP YP 12
(real,input) An array of length N as computed by KURV1. 
.IP S 12
(real,input) An array of length N containing the polygonal arc 
lengths of the curve, as computed by KURV1. 
.IP SIGMA 12
(real,input) Tension factor. Values near zero result in a cubic spline; 
large values (e.g. 50) result in nearly a polygonal line. A typical value
is 1. 
.SH ACCESS
To use KURVD, load the NCAR Graphics library ngmath.
.SH SEE ALSO
kurv1,kurv2,
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

.\"
.\"	$Id: kurvpd.m,v 1.2 2000-07-13 03:18:03 haley Exp $
.\"
.TH KURVPD 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
KURVPD - nterpolation and derivatives for closed  parametric curves
.SH SYNOPSIS
CALL KURVPD (T, XS, YS, XST, YST, XSTT, YSTT, N, X, Y, XP, YP, S, SIGMA)
.sp
This subroutine behaves like KURVP2 except that in addition it
returns the first and second derivatives of the component functions in
the parameterization. The interval [0.,1.] maps onto a closed curve in
the plane. The resulting curve has a parametric representation both of
whose components are splines under tension and functions of the
polygonal arc length. KURVP1 must be called before calling
KURVPD. 
.SH DESCRIPTION
.IP T 12
(real,input) 
A value to be mapped to a point on the curve. The interval
[0.,1.] is mapped onto the curve such that 0. is mapped to
(X(1),Y(1)) and 1. is mapped to (X(1),Y(1)). Any interval
[TT,TT+1.] maps onto the entire closed curve. 
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
(real,input) An array of length N as computed by KURVP1. 
.IP YP 12
(real,input) An array of length N as computed by KURVP1. 
.IP S 12
(real,input) An array of length N containing the polygonal arc 
lengths of the curve, as computed by KURVP1. 
.IP SIGMA 12
(real,input) Tension factor. Values near zero result in a cubic spline; 
large values (e.g. 50) result in nearly a polygonal line. A typical value
is 1. 
.SH ACCESS
To use KURVPD, load the NCAR Graphics library ngmath.
.SH SEE ALSO
kurvp1,kurvp2,
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
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.


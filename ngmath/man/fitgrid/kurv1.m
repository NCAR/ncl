.\"
.\"	$Id: kurv1.m,v 1.3 2000-08-22 15:14:53 haley Exp $
.\"
.TH KURV1 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
KURV1 - calculate values for KURV2 usage
.SH SYNOPSIS
CALL KURV1  (N, X, Y, SLP1, SLPN, ISLPSW, XP, YP, TEMP, S, SIGMA, IER) 
.sp
This subroutine calculates certain values that are used by KURV2 in
order to compute an interpolatory spline under tension through a
sequence of points in the plane. The actual computation of the
interpolated values must be done using KURV2.  
.SH DESCRIPTION
.IP N 12
(integer, input) The number of input data values. (N > 1) 
.IP X 12
(real, input) An array containing the X values of the points. 
.IP Y 12
(real, input) An array containing the Y values of the points. 
Adjacent pairs of points must be distinct. 
.IP SLP1 12
(real, input) A value (in radians) containing the slope at (X(1),Y(1)). 
The angles are measured counter-clockwise from the X axis and the
positive sense of the curve is assumed to be that moving from
point 1 to point N. A value for SLP1 may be omitted as
indicated by the switch ISLPSW. 
.IP SLPN 12
(real, input) A value (in radians) containing the slope at (X(N),Y(N)). The
angles are measured counter-clockwise from the X axis and the
positive sense of the curve is assumed to be that moving from
point 1 to point N. A value for SLP2 may be omitted as
indicated by the switch ISLPSW. 
.IP ISLPSW 12
(integer, input) A switch to indicate whether the slopes at the end points 
should be calculated internally. 
.sp
= 0 if SLP1 and SLPN are user-specified. 
.br
= 1 if SLP1 is user-specified, but SLPN calculated internally. 
.br
= 2 if SLPN is user-specified, but SLP1 calculated internally. 
.br
= 3 if SLP1 and SLPN are internally calculated. 
.IP XP 12
(real, output) An array of length N. 
.IP YP 12
(real, output) An array of length N. 
.IP TEMP 12
(real, input) Scratch space. 
.IP S 12
(real, input) An array of length N. On output, S contains the polygonal
arclengths of the curve. 
.IP SIGMA 12
(real, input) Tension factor. Values near zero result in a cubic spline; 
large values (e.g. 50) result in nearly a polygonal line. A typical value
is 1. 
.IP IER 12
(integer, output) An error return value. If IER is returned as 0, then no 
errors were detected. 
.sp
= 1 if N is less than 2. 
.br
= 2 if a pair of adjacent points coincide. 
.SH ACCESS
To use KURV1, load the NCAR Graphics library ngmath.
.SH SEE ALSO
kurv2,
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


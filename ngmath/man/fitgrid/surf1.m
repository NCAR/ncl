.\"
.\"	$Id: surf1.m,v 1.4 2008-07-27 03:35:39 haley Exp $
.\"
.TH SURF1 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
SURF1 - calculate values for SURF2 usage.
.SH SYNOPSIS
CALL SURF1 (M, N, X, Y, Z, IZ, ZX1, ZXM, ZY1, ZYN, ZXY11, ZXYM1, ZXY1N, ZXYMN, ISLPSW, ZP, TEMP, SIGMA, IER)
.sp
This subroutine calculates certain values that are used by SURF2 in
order to compute an interpolatory surface passing through a
rectangular grid of function values. The surface computed is a tensor
product of splines under tension. To calculate actual interpolated
values, SURF2 must be called. 
.SH DESCRIPTION
.IP M 12
(integer, input) The number of grid lines in the X direction. (M > 1) 
.IP N 12
(integer, input) The number of grid lines in the Y direction. (N > 1) 
.IP X 12
(real, input) An array containing M X coordinates for grid lines in the X
direction. These values must be strictly increasing. 
.IP Y 12
(real, input) An array containing N Y coordinates for grid lines in the Y
direction. These values must be strictly increasing. 
.IP Z 12
(real, input) An array containing M x N functional values at the grid points;
Z(I,J) contains the functional value at (X(I),Y(J)) for I=1,M and
J=1,N. 
.IP IZ 12
(integer, input) The row dimension of the matrix Z (IZ is greater than or equal
to M). 
.IP ZX1 12
(real, input) An array containing N X-partial derivatives of the function
along the line X(1), that is ZX1(J) is the X-partial derivative at
point (X(1),Y(J)) for J=1,N. This parameter may be defaulted by
setting ISLPSW appropriately. 
.IP ZXM 12
(real, input) An array containing N X-partial derivatives of the function
along the line X(M), that is ZXM(J) is the X-partial derivative
at point (X(M),Y(J)) for J=1,N. This parameter may be defaulted
by setting ISLPSW appropriately. 
.IP ZY1 12
(real, input) An array containing M Y-partial derivatives of the function
along the line Y(1), that is ZY1(I) is the Y-partial derivative at
point (X(I),Y(1)) for I=1,M. This parameter may be defaulted by
setting ISLPSW appropriately. 
.IP ZYN 12
(real, input) An array containing M Y-partial derivatives of the function
along the line Y(N), that is ZY1(I) is the Y-partial derivative at
point (X(I),Y(N)) for I=1,M. This parameter may be defaulted
by setting ISLPSW appropriately. 
.IP ZXY11 12
(real, input) The X-Y-partial derivative at (X(1),Y(1). This parameter may
be defaulted by setting ISLPSW appropriately. 
.IP ZXYM1 12
(real, input) The X-Y-partial derivative at (X(M),Y(1). This parameter may
be defaulted by setting ISLPSW appropriately. 
.IP ZXY1N 12
(real, input) The X-Y-partial derivative at (X(1),Y(N). This parameter may
be defaulted by setting ISLPSW appropriately. 
.IP ZXYMN 12
(real, input) The X-Y-partial derivative at (X(M),Y(N). This parameter may
be defaulted by setting ISLPSW appropriately. 
.IP ISLPSW 12
(integer, input) A switch to indicate which boundary derivatives are user
supplied and which should be estimated internally. Where 
.sp
I1 = 0 if ZX1 is user-supplied and 1 otherwise. 
.br
I2 = 0 if ZXM is user-supplied and 1 otherwise. 
.br
I3 = 0 if ZY1 is user-supplied and 1 otherwise. 
.br
I4 = 0 if ZYN is user-supplied and 1 otherwise. 
.br
I5 = 0 if ZXY11 is user-supplied and 1 otherwise. 
.br
I6 = 0 if ZXYM1 is user-supplied and 1 otherwise. 
.br
I7 = 0 if ZXY1N is user-supplied and 1 otherwise. 
.br
I8 = 0 if ZXYMN is user-supplied and 1 otherwise. 
.sp
set ISLPSW = I1 + 2*I2 + 4*I3 +8*I4 +16*I5 +32*I6 + 64*I7 +
128*I8. Then, for example, if ISLPSW=0, then all derivative
information is user-supplied and if ISLPSW=255, then all
derivative information is to be internally estimated. 
.IP ZP 12
(real, output) An array of size M x N x 3. On output this contains partial
derivatives of the surface at the given nodes. ZP is used by
SURF2. 
.IP TEMP 12
(real, input) Scratch space. 
.IP SIGMA 12
(real, input) Tension factor. Values near zero result in a cubic spline; large
values (e.g. 50) result in nearly a polygonal line. A typical value
is 1. 
.IP IER 12
(integer, output) An error return value. If IER is returned as 0, then no errors
were detected. 
.sp
= 1 if N is less than 2. 
.br
= 2 if X or Y values are not strictly increasing. 
.SH ACCESS
To use SURF1, load the NCAR Graphics library ngmath.
.SH SEE ALSO
surf2,
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

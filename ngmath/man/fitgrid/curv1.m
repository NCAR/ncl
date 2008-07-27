.\"
.\"	$Id: curv1.m,v 1.4 2008-07-27 03:35:38 haley Exp $
.\"
.TH CURV1 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
CURV1 - does set-up for CURV2.
.SH SYNOPSIS
CALL CURV1 (N, X, Y, SLP1, SLPN, ISLPSW, YP, TEMP, SIGMA, IER)
.SH DESCRIPTION
.IP N 12
(integer,input) The number of input data values. (N > 1) 
.IP X 12
(real, input) An array containing the abscissae for the input function. 
.IP Y 12
(real, input) An array containing the functional values of the 
input data -- (Y(K) is the functional value at X(K) for K=1,N). 
.IP SLP1 12
(real, input) A user-specified value for the desired slope at X(1). 
See ISLPSW below if you want to have a value calculated internally. 
.IP SLPN 12
(real, input) A user-specified value for the desired slope at X(N). 
See ISLPSW below if you want to have a value calculated internally. 
.IP ISLPSW 12
(integer, input) A switch to indicate whether the slopes at the 
end points should be calculated internally. 
.br
.sp
= 0 if SLP1 and SLPN are user-specified. 
.br
= 1 if SLP1 is user-specified, but SLPN is internally calculated. 
.br
= 2 if SLPN is user-specified, but SLP1 is internally calculated. 
.br
= 3 if SLP1 and SLPN are internally calculated. 
.br
.sp
.IP YP 12
(real, output) Contains values for the second derivative 
(CURV1 computes these). 
.IP TEMP 12
(real, input) Scratch space. 
.IP SIGMA 12
(real, input) Tension factor. Values near zero result in a cubic 
spline; large values (e.g. 50) result in nearly a polygonal line. 
A typical value is 1. 
.IP IER 12
(integer, output) An error return value. If IER is returned as 0, 
then no errors were detected. 
.br
.sp
= 1 if N is less than 2. 
.br
= 2 if X values are not strictly increasing. 
.SH ACCESS
To use CURV1, load the NCAR Graphics library ngmath.
.SH SEE ALSO
curv2,
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

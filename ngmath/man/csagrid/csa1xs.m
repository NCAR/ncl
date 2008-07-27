.\"
.\"	$Id: csa1xs.m,v 1.4 2008-07-27 03:35:33 haley Exp $
.\"
.TH CSA1XS 3NCARG "January 1999" UNIX "NCAR GRAPHICS"
.SH NAME
CSA1XS - cubic spline approximation, expanded entry for one-dimensional input
.SH SYNOPSIS
CALL CSA1XS (NI, XI, YI, WTS, KNOTS, SMTH, DERIV, NO, XO, YO, NWRK, WORK, IER)
.SH DESCRIPTION
.IP NI 12
(integer,input) The number of input data points. It must be that NI > 3 
and, depending on the size of KNOTS below, NI may have to be larger.
.IP XI 12
(real, input) An array dimensioned for NI containing the X 
coordinates of the input data points.
.IP YI 12
(real, input) An array dimensioned for NI 
containing function values at the input XI values, 
that is, YI(L) is the value of the input function at XI(L) for L=1,NI.
.IP WTS 12
(real, input) An array dimensioned for NI containing weights for the YI 
values at the input XI values, that is, WTS(L) is a weight for the 
value of YI(L) for L=1,NI.  If you do not desire to weight the input 
YI values, then set WTS(1) to -1.  The weights in the WTS array are 
relative and may be set to any non-negative value.  When CSA1XS is called,
the weights are summed and the individual weights are normalized
so that the weight sum is unity.
.IP KNOTS 12
(integer, input) The number of knots 
to be used in constructing the approximation
spline.  KNOTS must be at least 4.  The larger the value for
KNOTS, the closer the approximated curve will come to passing
through the input function values.
.IP SMTH 12
(real, input) A parameter that controls extrapolation into
data sparse regions.  If SMTH is zero, then nothing special 
is done in data sparse regions.  A good first choice for SMTH is 1.
.IP NDERIV 12
(integer, input) Specifies whether you want functional values (NDERIV=0),
first derivative values (NDERIV=1), or second derivative values (NDERIV=2).
.IP NO 12
(integer, input) The number of values to be calculated for the output curve.
.IP XO 12
(real, input) An array dimensioned for NO 
containing the X coordinates of the output curve.
.IP YO 12
(real, output) An array dimensioned for NO
containing the calculated function values for the output curve.
.IP NWRK 12 
(integer, input) The size of the WORK array.  NWRK must be at least
KNOTS*(KNOTS+3).
.IP WORK 12
(real, input) A work array dimensioned for NWRK.
.IP IER 12
(integer, output) An error return value.  If IER is returned as 0, then
no errors were detected. If IER is non-zero, then refer to the man
page for csagrid_errors for details.
.SH USAGE
CSA1XS is called to find an approximating cubic spline for one-dimensional
input data.  CSA1XS is called if you want to weight the input
data values, calculate derivatives, or handle data sparse areas specially.
If you do not want to do any of these three things, then use CSA1S.
.SH ACCESS
To use CSA1XS, load the NCAR Graphics library ngmath.
.SH SEE ALSO
csagrid,
csa1s,
csa1ls,
csa1lxs
.sp
Complete documentation for Csagrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/csagrid/csahome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.

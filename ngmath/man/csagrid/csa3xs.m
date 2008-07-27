.\"
.\"	$Id: csa3xs.m,v 1.4 2008-07-27 03:35:34 haley Exp $
.\"
.TH CSA3XS 3NCARG "January 1999" UNIX "NCAR GRAPHICS"
.SH NAME
CSA3XS - cubic spline approximation, expanded entry for three-dimensional input
.SH SYNOPSIS
CALL CSA3XS (NI, XI, UI, WTS, KNOTS, SMTH, NDERIV, 
.br
             NXO, ,NYO, NZO, XO, YO, ZO, UO, NWRK, 
.br
             WORK, IER)
.SH DESCRIPTION
.IP NI 12
(integer,input) The number of input data points. It must be that NI .gt. 3 
and, depending on the size of KNOTS below, NI may have to be larger.
.IP XI 12
(real, input) An array containing the X - Y - Z coordinates of the 
input data points.  XI is dimensioned for 3 x NI.  XI(1,L) is the X 
coordinate, XI(2,L) is the Y coordinate, and XI(2,L) is the Z coordinate 
for the input domain for L=1,NI.
.IP UI 12
(real, input) An array dimensioned for NI
containing function values at the input XI values, 
that is, UI(L) is the value of the input function at XI(L) for L=1,NI.
through the input function values.
.IP WTS 12
(real, input) An array dimensioned for NI containing weights for the UI 
values at the input XI values, that is, WTS(L) is a weight for the 
value of UI(L) for L=1,NI.  If you do not desire to weight the input 
UI values, then set WTS(1) to -1.  The weights in the WTS array are 
relative and may be set to any non-negative value.  When CSA3XS is called,
the weights are summed and the individual weights are normalized
so that the weight sum is unity.
.IP KNOTS 12
(integer, input) The number 
of knots to be used in constructing the approximation
spline.  KNOTS is dimensioned for 3 and provides the number of knots to be
used in the X, Y, and  Z directions.  Both KNOTS(I) must be at
least 4 for I=1,3.  The larger the values for KNOTS, the closer 
the approximated curve will come to passing through the input 
function values.
.IP SMTH 12
(real, input) A parameter that controls extrapolation into
data sparse regions.  If SMTH is zero, then nothing special 
is done in data sparse regions.  A good first choice for SMTH is 1.
.IP NDERIV 12
(integer, input) An array dimensioned for 3 that specifies, for each 
coordinate,  whether you want functional values (=0),
first derivative values (=1), or second derivative values (=2).
.IP NXO 12
(integer, input) The number of X coordinate values in the output grid.
.IP NYO 12
(integer, input) The number of Y coordinate values in the output grid.
.IP NZO 12
(integer, input) The number of Z coordinate values in the output grid.
.IP XO 12
(real, input) An array dimensioned for NXO 
containing the X coordinates of the output spline.
.IP YO 12
(real, input) An array dimensioned for NYO
containing the Y coordinates of the output spline.
.IP ZO 12
(real, input) An array dimensioned for NZO
containing the Z coordinates of the output spline.
.IP UO 12
(real, output) An array dimensioned for NXO x NYO x NZO 
containing the calculated function values for the
output function.  UO(I,J,K) is the calculated functional value
at (XO(I), YO(J), ZO(K)) for I=1,NXO and J=1,NYO and K=1,NZO.
.IP NWRK 12 
(integer, input) The size of the WORK array.  NWRK must be at least
NK * (NK+3) where NK = KNOTS(1) * KNOTS(2) * KNOTS(3).
.IP WORK 12
(real, input) A work array dimensioned for NWRK.
.IP IER 12
(integer, output) An error return value.  If IER is returned as 0, then
no errors were detected. If IER is non-zero, then refer to the man
page for csagrid_errors for details.
.SH USAGE
CSA3XS is called to find an approximating cubic spline for three-dimensional
input data.  CSA3XS is called if you want to weight the input
data values, calculate derivatives, or handle data sparse areas specially.
If you do not want to do any of these three things, then use CSA3S.
.SH ACCESS
To use CSA3XS, load the NCAR Graphics library ngmath.
.SH SEE ALSO
csagrid,
csa3s,
csa3ls,
csa3lxs
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

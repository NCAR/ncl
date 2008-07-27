.\"
.\"	$Id: csa2lxs.m,v 1.4 2008-07-27 03:35:33 haley Exp $
.\"
.TH CSA2LXS 3NCARG "January 1999" UNIX "NCAR GRAPHICS"
.SH NAME
CSA2LXS - cubic spline approximation, expanded entry for two-dimensional input,
list output
.SH SYNOPSIS
CALL CSA2LXS (NI, XI, UI, WTS, KNOTS, SMTH, NDERIV,
.br           
              NO, XO, YO, UO, NWRK, WORK, IER)
.SH DESCRIPTION
.IP NI 12
(integer,input) The number of input data points. It must be that NI .gt. 3 
and, depending on the size of KNOTS below, NI may have to be larger.
.IP XI 12
(real, input) An array containing the X - Y coordinates of the input data 
points. XI is dimensioned for 2 x NI.  XI(1,L) is the X coordinate and 
XI(2,L) is the Y coordinate for the input domain for L=1,NI.
.IP UI 12
(real, input) An array dimensioned for NI containing function values at 
the input XI values, 
that is UI(L) is the value of the input function at the coordinate
(XI(1,L),XI(2,L)) for L=1,NI.
.IP WTS 12
(real, input) An array dimensioned for NI containing weights for the UI 
values at the input XI values, that is, WTS(L) is a weight for the 
value of UI(L) for L=1,NI.  If you do not desire to weight the input 
UI values, then set WTS(1) to -1.  The weights in the WTS array are 
relative and may be set to any non-negative value.  When CSA2LXS is called,
the weights are summed and the individual weights are normalized
so that the weight sum is unity.
.IP KNOTS 12
(integer, input) The number of knots to be used 
in constructing the approximation
spline.  KNOTS is dimensioned for 2 and provides the number of knots to be
used in the X and the Y directions.  Both KNOTS(1) and KNOTS(2) must be at
least 4.  The larger the values for KNOTS, the closer the approximated curve
will come to passing through the input function values.
.IP SMTH 12
(real, input) A parameter that controls extrapolation into
data sparse regions.  If SMTH is zero, then nothing special 
is done in data sparse regions.  A good first choice for SMTH is 1.
.IP NDERIV 12
(integer, input) An array dimensioned for 2 that specifies, for each 
coordinate,  whether you want functional values (=0),
first derivative values (=1), or second derivative values (=2).
.IP NO 12
(integer, input) The number of coordinate values in the output list. 
NO can be any positive number. 
.IP XO 12
(real, input) An array dimensioned for NO containing the X coordinates 
of the output list.
.IP YO 12
(real, input) An array dimensioned for NO containing the Y coordinates 
of the output list.
.IP UO 12
(real, output) An array dimensioned for NO containing the calculated 
function values for the output spline.  UO(I) is the calculated functional 
value at (XO(I),YO(I)) for I=1,NO.
.IP NWRK 12 
(integer, input) The size of the WORK array.  NWRK must be at least
KNOTS(1)*KNOTS(2)*(KNOTS(1)*KNOTS(2)+3).
.IP WORK 12
(real, input) A work array dimensioned for NWRK.
.IP IER 12
(integer, output) An error return value.  If IER is returned as 0, then
no errors were detected. If IER is non-zero, then refer to the man
page for csagrid_errors for details.
.SH USAGE
CSA2LXS is called if you want to weight the input data values, 
calculate derivatives, or handle data sparse areas specially. 
If you do not want to do any of these three things, then use CSA2LS. 
.SH ACCESS
To use CSA2LXS, load the NCAR Graphics library ngmath.
.SH SEE ALSO
csagrid,
csa2s,
csa2xs,
csa2ls,
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

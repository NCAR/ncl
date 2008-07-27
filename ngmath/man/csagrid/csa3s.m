.\"
.\"	$Id: csa3s.m,v 1.4 2008-07-27 03:35:34 haley Exp $
.\"
.TH CSA3S 3NCARG "January 1999" UNIX "NCAR GRAPHICS"
.SH NAME
CSA3S - cubic spline approximation, simple entry for three-dimensional input
.SH SYNOPSIS
CALL CSA3S (NI, XI, UI, KNOTS, NXO, NYO, NZO, XO, YO, ZO, 
.br
            UO, NWRK, WORK, IER)
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
.IP KNOTS 12
(integer, input) An array dimensioned for 3 containing the number of 
knots to be used in each coordinate direction for constructing the 
approximation spline.  KNOTS(I) must be at least 4 for I=1,3.  The larger the
value for KNOTS, the closer the approximated curve will come to passing
through the input function values.
.IP NXO 12
(integer, input) The number of X coordinate values in the output grid.
.IP NYO 12
(integer, input) The number of Y coordinate values in the output grid.
.IP NZO 12
(integer, input) The number of Z coordinate values in the output grid.
.IP XO 12
(real, input) An array dimensioned for NXO 
containing the X coordinates of the output surface.
.IP YO 12
(real, input) An array dimensioned for NYO
containing the Y coordinates of the output surface.
.IP ZO 12
(real, input) An array dimensioned for NZO
containing the Y coordinates of the output surface.
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
CSA3S is called to find an approximating cubic spline for
three-dimensional input data.  If you want to weight the input data values,
calculate derivatives, or handle data sparse areas specially,
then you will need to use CSA3S.
.SH ACCESS
To use CSA3XS, load the NCAR Graphics library ngmath.
.SH SEE ALSO
csagrid,
csa3xs,
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

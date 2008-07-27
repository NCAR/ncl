.\"
.\"     $Id: natgridd.m,v 1.6 2008-07-27 03:35:40 haley Exp $
.\"
.TH NATGRIDD 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.SH NAME
NATGRIDD - primary double precision Fortran entry for natural neighbor gridding
.SH SYNOPSIS
CALL NATGRIDD (NPNTS, X, Y, Z, NUMXOUT, NUMYOUT, XI, YI, ZI, IER)
.SH DESCRIPTION
.IP NPNTS 12
(Integer, Input) - The number of input data points. (NPNTS > 3).
.IP X 12
(Double precision, Input) - An array containing the 
X coordinates of the input data points.
.IP Y 12
(Double precision, Input) - An array containing the Y 
coordinates of the input data points.
.IP Z 12
(Double precision, Input) - An array containing the functional values 
of the input data points. That is, Z(L) is the value of the input function at 
coordinate (X(L),Y(L)), for L=1,NPNTS. 
.IP NUMXOUT 12
(Integer, Input) - The number of X values in the output grid.
.IP NUMYOUT 12
(Integer, Input) - The number of Y values in the output grid.
.IP XI 12
(Double precision, Input) - An array of dimension NUMXOUT containing the X 
coordinates of the output data grid. The values in XI must be 
increasing, but need not be equally spaced. 
.IP YI 12
(Double precision, Input) - An array of dimension NUMYOUT containing the Y 
coordinates of the output data grid. The values in YI must be 
increasing, but need not be equally spaced. 
.IP ZI 12
(Double precision, Output) -  A two-dimensional array of
dimension NUMXOUT x NUMYOUT
containing the interpolated functional values. ZI(I,J) is the interpolated
value at grid point (XI(I),YI(J)).
.IP IER 12
(Integer, Output) - An error return value. If IER is returned as 0, then
no errors were detected. If IER is non-zero, then refer to the man
page for natgrid_errors for details.
.SH USAGE
NATGRIDD is the double precision gridding function in the Natgrid package.
The behavior of NATGRIDD is controlled by values set for the various
parameters described in natgrid_params.  Values for these parameters
can be set using the subroutines NNSETI, NNSETRD, and NNSETC.
.SH ACCESS
To use NATGRIDD, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params, 
nnseti, 
nngeti, 
nnsetrd, 
nngetrd, 
nnsetc, 
nngetc.
.sp
Complete documentation for Natgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/natgrid/nnhome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.

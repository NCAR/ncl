.\"
.\"	$Id: dsgrid3s.m,v 1.5 2008-07-27 03:35:37 haley Exp $
.\"
.TH DSGRID3S 3NCARG "September 1997-1998" UNIX "NCAR GRAPHICS"
.SH NAME
DSGRID3S - single precision Fortran entry for 3D gridding
.SH SYNOPSIS
CALL DSGRID3S (NPNTS, X, Y, Z, U, NUMXOUT, NUMYOUT, NUMZOUT, 
XO, YO, ZO, UO, IER)
.SH DESCRIPTION
.IP NPNTS 12
(Integer, Input) - The number of input data points.
.IP X 12
(Real, Input) - An array containing the X coordinates of the input data 
points.
.IP Y 12
(Real, Input) - An array containing the Y coordinates of the input data 
points.
.IP Z 12
(Real, Input) - An array containing the Z coordinates of the input data points. 
.IP U 12
(Real, Input) - An array containing the functional values 
of the input data points. 
That is, U(L) is the value of the input function at coordinate 
(X(L),Y(L),Z(L)), for L=1,NPNTS. 
.IP NUMXOUT 12
(Integer, Input) - The number of X values in the output grid.
.IP NUMYOUT 12
(Integer, Input) - The number of Y values in the output grid.
.IP NUMZOUT 12
(Integer, Input) - The number of Z values in the output grid.
.IP XO 12
(Real, Input) - An array of dimension NUMXOUT containing the X 
coordinates of the output data grid. The values in XO must be 
increasing, but need not be equally spaced. 
.IP YO 12
(Real, Input) - An array of dimension NUMYOUT containing the Y 
coordinates of the output data grid. The values in YO must be 
increasing, but need not be equally spaced. 
.IP ZO 12
(Real, Input) - An array of dimension NUMZOUT containing the Z 
coordinates of the output data grid. The values in ZO must be 
increasing, but need not be equally spaced. 
.IP UO 12
(Real, Output) - A three-dimensional array dimensioned as
UO(NUMXOUT,NUMYOUT,NUMZOUT)
containing the interpolated functional values. UO(I,J,K) is the 
interpolated value at grid point (XO(I),YO(J),ZO(K)). 
.IP IER 12
(Integer, Output) - An error return value. If IER is returned as 0, then
no errors were detected. If IER is non-zero, then refer to the man
page for dsgrid_errors for details.
.SH USAGE
DSGRID3S is the single precision gridding function in the Dsgrid package.
The behavior of DSGRID3S is controlled by values set for the various
parameters described in dsgrid_params.  Values for these parameters
can be set using the subroutines DSSETI, DSSETR, and DSSETC.
.SH ACCESS
To use DSGRID3S, load the NCAR Graphics library ngmath.
.SH SEE ALSO
dsgrid,
dsgrid_params, 
dsseti, 
dsgeti, 
dssetr, 
dsgetr, 
dssetc, 
dsgetc.
.sp
Complete documentation for Dsgrid is available at URL
.sp
http://ngwww.ucar.edu/ngdoc/ng/ngmath/dsgrid/dshome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.

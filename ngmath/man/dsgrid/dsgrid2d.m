.\"
.\"     $Id: dsgrid2d.m,v 1.5 2008-07-27 03:35:36 haley Exp $
.\"
.TH DSGRID2D 3NCARG "September 1997-1998" UNIX "NCAR GRAPHICS"
.SH NAME
DSGRID2D - double precision Fortran entry for iterpolation from 
2D random data to an output grid.
.SH SYNOPSIS
CALL DSGRID2D (NPNTS, X, Y, Z, NUMXOUT, NUMYOUT, XO, YO, ZO, IER)
.SH DESCRIPTION
.IP NPNTS 12
(Integer, Input) - The number of input data points.
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
.IP XO 12
(Double precision, Input) - An array of dimension NUMXOUT containing the X 
coordinates of the output data grid. The values in XO must be 
increasing, but need not be equally spaced. 
.IP YO 12
(Double precision, Input) - An array of dimension NUMYOUT containing the Y 
coordinates of the output data grid. The values in YO must be 
increasing, but need not be equally spaced. 
.IP ZO 12
(Double precision, Output) -  A two-dimensional array of
dimension NUMXOUT x NUMYOUT
containing the interpolated functional values. ZO(I,J) is the interpolated
value at grid point (XO(I),YO(J)).
.IP IER 12
(Integer, Output) - An error return value. If IER is returned as 0, then
no errors were detected. If IER is non-zero, then refer to the man
page for dsgrid_errors for details.
.SH USAGE
DSGRID2D is the double precision gridding function for 2D data 
in the Dsgrid package.
The behavior of DSGRID2D is controlled by values set for the various
parameters described in dsgrid_params.  Values for these parameters
can be set using the subroutines DSSETI, DSSETRD, and DSSETC.
.sp
In almost all cases the ngmath function NATGRIDD is superior to
DSGRID2D for 2D interpolation.  See the man page for NATGRIDD for details.
.SH ACCESS
To use DSGRID2D, load the NCAR Graphics library ngmath.
.SH SEE ALSO
dsgrid,
dsgrid_params, 
dsseti, 
dsgeti, 
dssetrd, 
dsgetrd, 
dssetc, 
dsgetc.
.sp
Complete documentation for Dsgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/dsgrid/dshome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.

.\"
.\"     $Id: dspnt2d.m,v 1.6 2008-07-27 03:35:37 haley Exp $
.\"
.TH DSPNT2D 3NCARG "September 1997-1998" UNIX "NCAR GRAPHICS"
.SH NAME
DSPNT2D- Interpolate at a single point (or points) in 2D in double precision
.SH SYNOPSIS
CALL DSPNT2D (N, X, Y, Z, M, XO, YO, ZO, IER)
.SH DESCRIPTION
.IP N 12
(Integer, Input) - The number of input data points. 
.IP X 12
(Double precision, Input) - An array containing the X coordinates of the 
input data points. 
.IP Y 12
(Double precision, Input) - An array containing the Y coordinates of the 
input data points.
.IP Z 12
(Double precision, Input) - An array containing the functional values of the 
input data points. That is, Z(L) is the value of the input
function at coordinate (X(L),Y(L)), for L=1,N. 
.IP M 12
(Integer, Input) - The number of output data points (this may be "1"). 
.IP XO 12
(Double precision, Input) - An array of dimension M containing 
the X coordinates of the output data. The values in XO may be in any order.
.IP YO 12
(Double precision, Input) - An array of dimension M containing 
the Y coordinates of the output data. The values in YO may be in any order. 
.IP ZO 12
(Double precision, Output) - An array of dimension M containing 
the interpolated values. ZO(I) is the interpolated value at 
point (XO(I),YO(I)) for I=1,M. 
.IP IER 12
(Integer, Output) - An error return value. If IER is returned as 0, then
no errors were detected. If IER is non-zero, then refer to the man
page for dsgrid_errors for details.
.SH USAGE
This subroutine is called when you want to interpolate at an individual
point or points. 
.SH ACCESS
To use DSPNT2D, load the NCAR Graphics library ngmath.
.SH SEE ALSO
dsgrid,
dsgrid_params.
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

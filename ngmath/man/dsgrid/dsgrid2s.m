.\"
.\"	$Id: dsgrid2s.m,v 1.4 2000-08-22 15:14:44 haley Exp $
.\"
.TH DSGRID2S 3NCARG "September 1997-1998" UNIX "NCAR GRAPHICS"
.SH NAME
DSGRID2S - primary single precision Fortran entry for 2D gridding
.SH SYNOPSIS
CALL DSGRID2S (NPNTS, X, Y, Z, NUMXOUT, NUMYOUT, XO, YO, ZO, IER)
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
(Real, Input) - An array containing the functional values of the 
input data points. That is, Z(L) is the value of the input function at 
coordinate (X(L),Y(L)), for L=1,NPNTS. 
.IP NUMXOUT 12
(Integer, Input) - The number of X values in the output grid.
.IP NUMYOUT 12
(Integer, Input) - The number of Y values in the output grid.
.IP XO 12
(Real, Input) - An array of dimension NUMXOUT containing the X 
coordinates of the output data grid. The values in XO must be 
increasing, but need not be equally spaced. 
.IP YO 12
(Real, Input) - An array of dimension NUMYOUT containing the Y 
coordinates of the output data grid. The values in YO must be 
increasing, but need not be equally spaced. 
.IP ZO 12
(Real, Output) -  A two-dimensional array of dimension NUMXOUT x NUMYOUT
containing the interpolated functional values. ZO(I,J) is the interpolated
value at grid point (XO(I),YO(J)).
.IP IER 12
(Integer, Output) - An error return value. If IER is returned as 0, then
no errors were detected. If IER is non-zero, then refer to the man
page for dsgrid_errors for details.
.SH USAGE
DSGRID2S is the single precision gridding function for 2D data 
in the Dsgrid package.
The behavior of DSGRID2S is controlled by values set for the various
parameters described in dsgrid_params.  Values for these parameters
can be set using the subroutines DSSETI, DSSETR, and DSSETC.
.sp
In almost all cases the ngmath function NATGRIDS is superior to
DSGRID2S for 2D interpolation.  See the man page for NATGRIDS for details.
.SH ACCESS
To use DSGRID2S, load the NCAR Graphics library ngmath.
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
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/dsgrid/dshome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.


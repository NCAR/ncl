.\"
.\"     $Id: c_nnpntinitd.m,v 1.4 2000-07-13 03:18:05 haley Exp $
.\"
.TH c_nnpntinitd 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
c_nnpntinitd - Enter single point mode
.SH FUNCTION PROTOTYPE
void c_nnpntinitd(int, double [], double [], double []);
.SH SYNOPSIS
void c_nnpntinitd (npnts, x, y, z);
.SH DESCRIPTION 
.IP npnts 12
The number of input data points. (npnts > 3) 
.IP x 12
An array of size npnts containing the X coordinates of the input data points.
.IP y 12
An array of size npnts containing the Y coordinates of the input data points.
.IP z 12
An array of size npnts containing the functional values of the input 
data points. That is, z[n] is the value of the input function at 
coordinate (x[n],y[n]), for 0 <= n < npnts. 
.SH USAGE
This function is invoked when you want to interpolate at individal
points.  It is an initialization routine that sets up some internal
variables and does this initial triangulation. To actually do the
interplation, use function c_nnpntd.  To terminate single point mode,
use the function c_nnpntendd.
.SH ACCESS
To use c_nnpntinitd, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params,
c_natgridd,
c_nnpntd,
c_nnpntendd.
.sp
Complete documentation for Natgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/natgrid/nnhome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.


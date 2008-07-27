.\"
.\"     $Id: c_nnpntinits.m,v 1.6 2008-07-27 03:35:40 haley Exp $
.\"
.TH c_nnpntinits 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
c_nnpntinits - Enter single point mode
.SH FUNCTION PROTOTYPE
void c_nnpntinits(int, float [], float [], float []);
.SH SYNOPSIS
void c_nnpntinits (npnts, x, y, z);
.SH DESCRIPTION 
.IP npnts 12
The number of input data points. (npnts > 2) 
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
interplation, use function c_nnpnts.  To terminate single point mode,
use the function c_nnpntend.
.SH ACCESS
To use c_nnpntinits, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params,
c_natgrids,
c_nnpnts,
c_nnpntend.
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

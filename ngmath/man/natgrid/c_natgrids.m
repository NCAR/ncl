.\"
.\"     $Id: c_natgrids.m,v 1.7 2000-08-22 15:14:58 haley Exp $
.\"
.TH c_natgrids 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_natgrids - primary C entry for natural neighbor gridding
.SH FUNCTION PROTOTYPE
float *c_natgrids(int, float [], float [], float [],
                  int, int, float [], float [], int *);
.SH SYNOPSIS
float *c_natgrids (npnts, x, y, z, numxout, numyout, xi, yi, ier);
.SH DESCRIPTION
.IP npnts 12
The number of input data points. (npnts > 3).
.IP x 12
An array containing the x coordinates of the input data 
points.
.IP y 12
An array containing the y coordinates of the input data 
points.
.IP z 12
An array containing the functional values of the 
input data points. That is, z[m] is the value of the input function at 
coordinate (x[m],y[m]), for 0 <= m < npnts. 
.IP numxout 12
The number of x values in the output grid.
.IP numyout 12
The number of y values in the output grid.
.IP xi 12
An array of dimension numxout containing the x 
coordinates of the output data grid. The values in xi must be 
increasing, but need not be equally spaced. 
.IP yi 12
An array of dimension numyout containing the y 
coordinates of the output data grid. The values in yi must be 
increasing, but need not be equally spaced. 
.IP ier 12
An error value. If *ier is returned as 0, then
no errors were detected. If *ier is non-zero, then refer to the man
page for natgrid_errors for details.
.SH USAGE
c_natgrids returns a pointer to a linear array of data that is the
interpolated grid stored in row-major order. That is, if out is declared as 
.sp
float *out;
.sp
and we set: 
.sp
  out = c_natgrids(numin, x, y, z, numxout, numyout, xo, yo, &ier);
.sp
then out[i*numyout+j] is the interpolated function value at coordinate
point (xo[i],yo[j]) for 0 <= i < numxout and 0 <= j < numyout. The
space for out is allocated internal to c_natgrids and is numxout*numyout
floats in size.
.SH ACCESS
To use c_natgrids, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params, 
c_nnseti, 
c_nngeti, 
c_nnsetr, 
c_nngetr, 
c_nnsetc, 
c_nngetc.
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


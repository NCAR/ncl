.\"
.\"     $Id: c_dspnt2s.m,v 1.4 2000-08-22 15:14:41 haley Exp $
.\"
.TH c_dspnt2s 3NCARG "September 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
c_dspnt2s - Interpolate at a single point (or points) in 2D in single precision
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
void c_dspnt2s(int, float [], float [], float [],
.br
               int, float [], float [], float *, int *);
.fi
.cs R
.SH SYNOPSIS
.nf
.cs R 24
void c_dspnt2s (int n, float x[], float y[], float z[], 
.br
                int m, float xo[], float yo[], float zo, int *ier);    
.fi
.cs R
.SH DESCRIPTION 
.IP n 12
The number of input data points. 
.IP x 12
An array containing the X coordinates of the input data points.
.IP y 12
An array containing the Y coordinates of the input data points.
.IP z 12
An array containing the functional values of the input 
data points. That is, z[l] is the value of the input
function at coordinate (x[l],y[l]), for 0 <= l < n . 
.IP m 12
The number of output data points (this may be "1"). 
.IP xo 12
An array of dimension m containing the X coordinates of the 
output data. The values in xo may be in any order.
.IP yo 12
An array of dimension m containing the Y coordinates of the 
output data. The values in xo may be in any order.
.IP zo 12
A pointer to a float. Space for m floats is allocated for zo by 
dspnt2s. zo[i] is the interpolated value at point (xo[i],yo[i]) 
for 0 <= i < n .
.IP ier 12
An error return value. If *ier is returned as 0,
then no errors were detected. If *ier is non-zero, then refer to
the man page for dsgrid_errors.
.SH USAGE
This function is called when you want to interpolate 2D data at an
individual point or points.
.SH ACCESS
To use c_dspnt2s, load the NCAR Graphics library ngmath.
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


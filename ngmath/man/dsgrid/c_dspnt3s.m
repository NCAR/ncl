.\"
.\"     $Id: c_dspnt3s.m,v 1.4 2000-08-22 15:14:42 haley Exp $
.\"
.TH c_dspnt3s 3NCARG "September 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
c_dspnt3s - interpolate single precision 3D data at specified points
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
void c_dspnt3s(int, float [], float [], float [], float[]
.br
int, float [], float [], float [], float *, int *);
.fi
.cs R
.SH SYNOPSIS
.nf
.cs R 24
void c_dspnt3d(int n, float xi[], float yi[], float zi[], float ui[],
.br
               int m, float xo[], float yo[], float zo[], float *uo,
.br
               int *ier)
.fi
.cs R
.SH DESCRIPTION 
.IP n 12
The number of input data points.
.IP xi 12
An array containing the X coordinates of the input data points.
.IP yi 12
An array containing the Y coordinates of the input data points.
.IP zi 12
An array containing the Z coordinates of the input data points.
.IP ui 12
An array containing the functional values of the input data 
points. That is, ui[l] is the value of the input function at 
coordinate (xi[l],yi[l],zi[l]), for 0 <= l < n .
.IP m 12
The number of output data points (this may be "1"). 
.IP xo 12
An array of dimension m containing the X coordinates of the 
output data. The values in xo may be in any order. 
.IP yo 12
An array of dimension m containing the Y coordinates of the 
output data. The values in xo may be in any order. 
.IP zo 12
An array of dimension m containing the Z coordinates of the 
output data. The values in xo may be in any order. 
.IP uo 12
A pointer to a float. Space for m floats is allocated for uo by 
dspnt3d. zo[i] is the interpolated value at point 
(xo[i],yo[i],zo[i]) for 0 <= i < n .
.IP ier 12
An error return value. If *ier is returned as 0, then no errors 
were detected. If *ier is non-zero, then refer to the man page
for dsgrid_errors.
.SH USAGE
This function is called to interpolate single precision 3D 
data at a specified point or points.
.SH ACCESS
To use c_dspnt3s, load the NCAR Graphics library ngmath.
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


.\"
.\"	$Id: c_cssgridd.m,v 1.3 2000-08-22 15:14:34 haley Exp $
.\"
.TH c_cssgridd 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.SH NAME
c_cssgridd - tension spline interpolation on a sphere
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
double *c_cssgridd(int, double [], double [], double [],
.br
                 int, int, double [], double [], int *);
.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
double *c_cssgridd (n, rlat, rlon, f, ni, nj, plat, plon, ier)
.fi
.cs R
.sp
.SH DESCRIPTION
.IP n 12
The number of input data points, n > 2. 
.IP rlat 12
An array containing the latitudes
of the input data, expressed in degrees.
The first three points must not be collinear
(lie on a common great circle).
.IP rlon 12
An array containing the longitudes of the input data,
expressed in degrees.
.IP f 12
Array containing data values. f[i] is the functional 
value at (rlat[i],rlon[i]) for i = 0 to n-1. 
.IP ni 12
The number of latitudes in the interpolated grid. 
.IP nj 12
The number of longitudes in the interpolated grid. 
ni and nj can both be 1, allowing for interpolation at a single point.
.IP plat 12
An array containing the latitudes of the points 
where interpolated values are to be computed.  
The values in plat should be in degrees.
.IP plon 12
An array of length nj
containing the longitudes the output grid lines.
The values in plon should be in degrees.
.IP ier 12
An error return value. If *ier is
returned as 0, then no errors were
detected. If *ier is non-zero, then refer to the error list in
cssgrid_errors for details.
.SH USAGE
c_cssgridd is called to find an interpolating tension spline for 
randomly positioned data on a unit sphere. c_cssgridd is a double
precision version of c_cssgrid.
.SH RETURN VALUE
c_cssgridd returns a pointer to a linear array of data that 
contains interpolated values at
user-specified lat/lon pairs. The returned array stores 
its values as if they were a
2-dimensional C array with latitude being the 
first dimension and longitude the second
dimension. That is, if out is declared as 
.sp
.nf
.cs R 24
  double *out;
.fi
.cs R
.sp
and we set: 
.sp
.nf
.cs R 24
  out = c_cssgridd(n, rlat, rlon, f, nlat, nlon, plat, plon, &ier);
.fi
.cs R
.sp
then out[i*nlon+j] is the interpolated function value at 
coordinate point (plat[i], plon[j])
for 0 <= i < nlat and 0 <= j < nlon. The space for out 
is allocated internal to c_cssgridd and
is nlat * nlon doubles in size. 
.SH ACCESS
To use c_cssgridd, load the NCAR Graphics library ngmath.
.SH SEE ALSO
c_cssgrid,
css_overview,
cssgrid_errors
.sp
Complete documentation for Cssgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/cssgrid/csshome.html
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


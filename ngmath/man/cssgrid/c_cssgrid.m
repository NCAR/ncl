.\"
.\"	$Id: c_cssgrid.m,v 1.5 2008-07-27 03:35:34 haley Exp $
.\"
.TH c_cssgrid 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.SH NAME
c_cssgrid - tension spline interpolation on a sphere
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
float *c_cssgrid(int, float [], float [], float [],
.br
                 int, int, float [], float [], int *);
.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
float *c_cssgrid (n, rlat, rlon, f, ni, nj, plat, plon, ier)
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
c_cssgrid is called to find an interpolating tension spline for 
randomly positioned data on a unit sphere.
.SH RETURN VALUE
c_cssgrid returns a pointer to a linear array of data that 
contains interpolated values at
user-specified lat/lon pairs. The returned array stores 
its values as if they were a
2-dimensional C array with latitude being the 
first dimension and longitude the second
dimension. That is, if out is declared as 
.sp
.nf
.cs R 24
  float *out;
.fi
.cs R
.sp
and we set: 
.sp
.nf
.cs R 24
  out = c_cssgrid(n, rlat, rlon, f, nlat, nlon, plat, plon, &ier);
.fi
.cs R
.sp
then out[i*nlon+j] is the interpolated function value at 
coordinate point (plat[i], plon[j])
for 0 <= i < nlat and 0 <= j < nlon. The space for out 
is allocated internal to c_cssgrid and
is nlat * nlon floats in size. 
.SH ACCESS
To use c_cssgrid, load the NCAR Graphics library ngmath.
.SH SEE ALSO
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

The use of this Software is governed by a License Agreement.

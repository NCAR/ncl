.\"
.\"     $Id: c_dsgrid2d.m,v 1.3 2000-07-13 03:17:53 haley Exp $
.\"
.TH c_dsgrid2d 3NCARG "September 1997-1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_dsgrid2d - primary double precision C entry for 2D gridding
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
double *c_dsgrid2d(int, double [], double [], double [],
.br
                  int, int, double [], double [], int *);
.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
double *c_dsgrid2d(int n, double x[], double y[], double z[],
.br
                   int nx, int ny, double xo[], double yo[], int *ier)
.fi
.cs R
.SH DESCRIPTION
.IP n 12
The number of input data points.
.IP x 12
An array containing the X coordinates of the 
input data points.
.IP y 12
An array containing the Y coordinates of the 
input data points.
.IP z 12
An array of size n containing the functional 
values of the input data points. That is, z[m] is the value of the
input function at coordinate (x[m],y[m]), for 0 <= m < n. 
.IP nx 12
The number of X values in the output grid.
.IP ny 12
The number of Y values in the output grid.
.IP xo 12
An array of size nx containing the X 
coordinates of the output data grid. The values in xo must be
increasing, but need not be equally spaced. 
.IP yo 12
An array of size ny containing the Y
coordinates of the output data grid. The values in yo must be 
increasing, but need not be equally spaced. 
.IP ier 12
An error return value. If *ier is returned as 0, 
then no errors were detected. If *ier is non-zero, then refer to
the man page for dsgrid_errors.
.SH USAGE
c_dsgrid2d returns a pointer to a linear array of data that is the
interpolated grid stored in row-major order. That is, if out is declared as 
.sp
double *out;
.sp
and we set: 
.sp
  out = c_dsgrid2d(n, x, y, z, nx, ny, xo, yo, ier);
.sp
then out[i*ny+j] is the interpolated function value at coordinate
point (xo[i],yo[j]) for 0 <= i < nx and 0 <= j < ny. The
space for out is allocated internal to c_dsgrid2d and is nx*ny
doubles in size.
.SH ACCESS
To use c_dsgrid2d, load the NCAR Graphics library ngmath.
.SH SEE ALSO
dsgrid,
dsgrid_params, 
c_dsseti, 
c_dsgeti, 
c_dssetrd, 
c_dsgetrd, 
c_dssetc, 
c_dsgetc.
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


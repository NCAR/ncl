.\"
.\"	$Id: c_csa3s.m,v 1.3 2000-07-13 03:17:43 haley Exp $
.\"
.TH c_csa3s 3NCARG "January 1999" UNIX "NCAR GRAPHICS"
.SH NAME
c_csa3s - cubic spline approximation, simple entry for three-dimensional input, gridded output
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
float *c_csa3s(int, float [], float [], float [], float [], int [], int, int,
               int, float [], float [], float [], int *);
.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
float *c_csa3s (int n, float xi[], float yi[], float zi[],
                float ui[], int knots[3], int nxo, int nyo,
                int nzo, float xo[], float yo[], float zo[], 
                int *ier);
.fi
.cs R
.sp
.SH DESCRIPTION
.IP n 12
(integer,input) The number of input data points. It must be 
that n is greater than 3 and, depending 
on the size of knots below, n may have to be larger.  
.IP xi 12
(real, input) An array dimensioned for n containing the X coordinate 
values for the input function.
.IP yi 12
(real, input) An array dimensioned for n containing the Y coordinate 
values for the input function. 
.IP zi 12
(real, input) An array dimensioned for n containing the Z coordinate 
values for the input function. 
.IP ui 12
(real, input) An array containing the functional values of 
the input function -- ui[k] is the
functional value at (xi[k], yi[k], zi[k]) for k=0,n-1. 
.IP knots 12
(integer, input) The number of knots to be used 
in constructing the approximation spline. knots[0],
knots[1] and knots[2] must each be at least 4. The 
larger the value for knots, the
closer the approximated curve will come 
to passing through the input function values.
.IP nxo 12
(integer, input) The number of X coordinate 
values to be calculated for the output surface. 
.IP nyo 12
(integer, input) The number of Y coordinate 
values to be calculated for the output surface. 
.IP nzo 12
The number of Z coordinate values to be calculated for the output grid. 
.IP xo 12
(real, input) An array dimensioned for nxo
containing the X coordinates of the output grid.
.IP yo 12
(real, output) An array dimensioned for nyo
containing the Y coordinates of the output grid.
.IP zo 12
(real, output) An array dimensioned for nzo
containing the Z coordinates of the output grid.
.IP ier 12
(pointer to integer, output) An error return value. 
If *ier is returned as 0, then no errors were 
detected. If *ier is non-zero, then refer to the error list in the error 
table for details. 
.SH USAGE
c_csa3s is called to find an approximating cubic spline for 
three-dimensional input data. If you want to weight the input 
data values, calculate derivatives, or handle data sparse areas
specially, then you will need to use c_csa3xs. 
.sp
c_csa3s returns a pointer to a linear array of data that is the approximation
spline stored in row-major order. That is, if out is declared as 

.nf
.cs R 24
  float *out;
.fi
.cs R
.sp
and we set: 

.nf
.cs R 24
  out = c_csa3s(n, x, y, z, u, nx, ny, nz, xo, yo, zo, &ier);
.fi
.cs R
.sp
then out[nz*ny*i + nz*j + k] is the approximation function value 
at coordinate point (xo[i],
yo[j], zo[k]) for 0 <= i < nx, 0 <= j < ny, and 0 <= k < nz. The space 
for out is allocated internal to c_csa3s and is nx*ny*nz floats in size. 
.SH ACCESS
To use c_csa3s, load the NCAR Graphics library ngmath.
.SH SEE ALSO
csagrid,
c_csa3xs,
c_csa3ls,
c_csa3lxs
.sp
Complete documentation for Csagrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/csagrid/csahome.html
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


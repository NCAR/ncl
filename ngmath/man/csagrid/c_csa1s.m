.\"
.\"	$Id: c_csa1s.m,v 1.4 2000-08-22 15:14:24 haley Exp $
.\"
.TH c_csa1s 3NCARG "January 1999" UNIX "NCAR GRAPHICS"
.SH NAME
c_csa1s - cubic spline approximation, simple entry for one-dimensional input
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
float *c_csa1s(int, float [], float [], int, int, float [], int *);
.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
float *c_csa1s(int n, float xi[], float yi[], int knots,
.br
            int m, float xo[], int *ier);
.fi
.cs R
.sp
.SH DESCRIPTION
.IP n 12
(integer,input) The number of input data points. It must be 
that n is greater than 3 and, depending 
on the size of knots below, n may have to be larger.  
.IP xi 12
(real, input) An array dimensioned for n containing the abscissae 
for the input function. 
.IP yi 12
(real, input) An array dimensioned for n containing the functional values 
of the input function -- yi[k] is the functional value at xi[k] for k=0,n-1. 
.IP knots 12
(integer, input) The number of knots to be used in constructing the 
approximation spline.  knots must be at least 4. The larger the value 
for knots, the closer the approximated curve will come to passing through 
the input function values. 
.IP m 12
(integer, input) The number of values to be calculated for the output curve. 
.IP xo 12
(real, input) An array dimensioned for m
containing the X coordinates of the output curve.
.IP ier 12
(pointer to integer, output) An error return value. If *ier is
returned as 0, then no errors were
detected. If *ier is non-zero, then refer to the error list in the
error table for details.
.SH USAGE
c_csa1s is called to find an approximating cubic spline for 
one-dimensional input data. If you want to weight the input 
data values, calculate derivatives, or handle data sparse areas
specially, then you will need to use c_csa1xs. 
.sp
c_csa1s returns a pointer to 
a linear array of data that is the approximated curve. 
.SH ACCESS
To use c_csa1s, load the NCAR Graphics library ngmath.
.SH SEE ALSO
csagrid,
c_csa1xs,
c_csa1ls,
c_csa1lxs
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


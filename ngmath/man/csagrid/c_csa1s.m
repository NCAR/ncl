.\"
.\"	$Id: c_csa1s.m,v 1.5 2008-07-27 03:35:33 haley Exp $
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

The use of this Software is governed by a License Agreement.

.\"
.\"	$Id: c_csa2s.m,v 1.4 2008-07-27 03:35:33 haley Exp $
.\"
.TH c_csa2s 3NCARG "January 1999" UNIX "NCAR GRAPHICS"
.SH NAME
c_csa2s - cubic spline approximation, simple entry for two-dimensional input, gridded output
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
float *c_csa2s(int, float [], float [], float [], int [],
               int, int, float [], float [], int *);
.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
int c_csa2s(int n, float xi[], float yi[], float zi[], int knots[2],
            int no, int mo, float xo[], float yo[], int *ier);
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
(real, input) An array containing the functional values of the input 
function -- zi[k] is the functional value at (xi[k], yi[k]) for k=0,n-1. 
.IP knots 12
(integer, input) The number of knots to be used in each coordinate
direction in constructing the 
approximation spline.  knots[0] and knots[1] 
must be at least 4. The larger the value 
for knots, the closer the approximated curve will come to passing through 
the input function values. 
.IP no 12
(integer, input) The number of X coordinate 
values to be calculated for the output surface. 
.IP mo 12
(integer, input) The number of Y coordinate 
values to be calculated for the output surface. 
.IP xo 12
(real, input) An array dimensioned for no
containing the X coordinates of the output grid.
.IP yo 12
(real, output) An array dimensioned for mo
containing the Y coordinates of the output grid.
.IP ier 12
(pointer to integer, output) An error return value. If *ier is 
returned as 0, then no errors were 
detected. If *ier is non-zero, then refer to the error list in the 
error table for details. 
.SH USAGE
c_csa2s is called to find an approximating cubic spline surface for 
two-dimensional input data. If you want to weight the input data 
values, calculate derivatives, or handle data sparse areas specially, 
then you will need to use c_csa2xs. 
.sp
c_csa2s returns a pointer to a linear array of data that is the approximated
grid stored in row-major order. That is, if out is declared as 

.nf
.cs R 24
  float *out;
.fi
.cs R
.sp
and we set: 

.nf
.cs R 24
  out = c_csa2s(n, x, y, z, knots, no, mo, xo, yo, &ier);
.fi
.cs R
.sp
then out[i*mo+j] is the approximated function value at coordinate point 
(xo[i], yo[j]) for 0 <= i < no and 0 <= j < mo. The space for out is 
allocated internal to c_csa2s and is no * mo floats in size. 
.SH ACCESS
To use c_csa2s, load the NCAR Graphics library ngmath.
.SH SEE ALSO
csagrid,
c_csa2xs,
c_csa2ls,
c_csa2lxs
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

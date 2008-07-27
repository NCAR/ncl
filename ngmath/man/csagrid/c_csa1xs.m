.\"
.\"	$Id: c_csa1xs.m,v 1.5 2008-07-27 03:35:33 haley Exp $
.\"
.TH c_csa1xs 3NCARG "January 1999" UNIX "NCAR GRAPHICS"
.SH NAME
c_csa1xs - cubic spline approximation, expanded entry for one-dimensional input
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
float *c_csa1xs(int, float [], float [], float [], int,
             float, int, int, float [], int *);
.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
float *c_csa1xs(int n, float xi[], float yi[], float wts[],
                int knots, float smth, int nderiv,
                int m, float xo[], int *ier);
.fi
.cs R
.sp
.SH DESCRIPTION
.IP n 12
(integer,input) The number of input data points. It must be 
that n is greater than 3 and, depending on the size 
of knots below, n may have to be larger.  
.IP xi 12
(real, input) An array dimensioned for n containing the abscissae 
for the input function. 
.IP yi 12
(real, input) An array dimensioned for n containing the functional values 
of the input function -- yi[k] is the functional value at xi[k] for k=0,n-1. 
.IP wts 12
(real, input) An array dimensioned for n containing weights 
for the yi values at the input xi values, 
that is, wts[l] is a weight for the value of yi[l] for l=0,n-1. If 
you do not desire to weight the input yi
values, then set wts[0] to -1. The 
weights in the wts array are relative and may be set
to any non-negative value. 
When c_csa1xs is called, the weights are summed and the
individual weights are normalized so that the weight sum is unity. 
.IP knots 12
(integer, input) The number of knots to be used in constructing the 
approximation spline.  knots must be at least 4. The larger the value 
for knots, the closer the approximated curve will come to passing through 
the input function values. 
.IP smth 12
(real, input) A parameter that controls extrapolation into data sparse regions. 
If smth is zero, then
nothing special is done in data sparse regions. 
A good first choice for smth is 1. 
.IP nderiv 12
(integer, input) Specifies whether you want functional values 
(nderiv=0), first derivative values (nderiv=1), or second 
derivative values (nderiv=2). 
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
c_csa1xs is called to find an approximating cubic spline 
for one-dimensional input data.
c_csa1xs is called if you want to weight the input data values, 
calculate derivatives, or handle data sparse areas specially. 
If you do not want to do any of these three things, then use c_csa1s. 
.sp
c_csa1s returns a pointer to
a linear array of data that is the approximated curve.
.SH ACCESS
To use c_csa1xs, load the NCAR Graphics library ngmath.
.SH SEE ALSO
csagrid,
c_csa1s,
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

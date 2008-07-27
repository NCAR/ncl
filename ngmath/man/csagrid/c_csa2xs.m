.\"
.\"	$Id: c_csa2xs.m,v 1.4 2008-07-27 03:35:33 haley Exp $
.\"
.TH c_csa2xs 3NCARG "January 1999" UNIX "NCAR GRAPHICS"
.SH NAME
c_csa2xs - cubic spline approximation, expanded entry for two-dimensional input, gridded output
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
float *c_csa2xs(int, float [], float [], float [], float [], int [], float,
                int [], int, int, float [], float [], int *);

.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
int c_csa2xs(int ni, float xi[], float yi[], float zi[], float wts[],
             int knots[2], float smth, int nderiv[2], int no, int mo, 
             float xo[], float yo[], int *ier);
.fi
.cs R
.sp
.SH DESCRIPTION
.IP ni 12
(integer,input) The number of input data points. It must be 
that ni is greater than 3 and, depending 
on the size of knots below, n may have to be larger.  
.IP xi 12
(real, input) An array dimensioned for ni containing the X coordinate 
values for the input function.
.IP yi 12
(real, input) An array dimensioned for ni containing the Y coordinate 
values for the input function. 
.IP zi 12
(real, input) An array containing the functional values of the input 
function -- zi[k] is the functional value at (xi[k], yi[k]) for k=0,ni-1. 
.IP wts 12
(real, input) An array containing weights for the zi values 
at the input xi values, that is, wts[l] is a
weight for the value of zi[l] for l=0,ni-1. If 
you do not desire to weight the input zi
values, then set wts[0] to -1. The weights in 
the wts array are relative and may be set
to any non-negative value. When c_csa2xs is 
called, the weights are summed and the
individual weights are normalized so that the weight sum is unity. 
.IP knots 12
(integer, input) The number of knots to be used in each coordinate
direction in constructing the 
approximation spline.  knots must be at least 4. The larger the value 
for knots, the closer the approximated curve will come to passing through 
the input function values. 
.IP smth 12
(real, input) A parameter that controls extrapolation into data sparse 
regions. If smth is zero, then nothing special is done in 
data sparse regions. A good first choice for smth is 1. 
.IP nderiv 12
(real, input) For each of the two coordinate direction, specifies whether you 
want functional values (nderiv=0), first derivative values (nderiv=1), 
or second derivative values (nderiv=2). For example, if 
nderiv[0]=1 and nderiv[1]=1, then the second order mixed partial would 
be computed. 
.IP no 12
(integer, input) The number of X coordinate values to be calculated 
for the output surface. 
.IP mo 12
(integer, input) The number of Y coordinate values to be calculated 
for the output surface. 
.IP xo 12
(real, input) An array dimensioned for no
containing the X coordinate values for the output grid. 
.IP yo 12
(real, output) An array dimensioned for mo
containing the Y coordinates of the output grid.
.IP ier 12
(pointer to integer, output) An error return value. If *ier 
is returned as 0, then no errors were 
detected. If *ier is non-zero, then refer to the error list in the 
error table for details. 
.SH USAGE
c_csa2xs is called to find an approximating cubic spline surface for 
two-dimensional input data. 
c_csa2xs is called if you want to weight the input data values, 
calculate derivatives, or handle data sparse areas specially. 
If you do not want to do any of these three things, then use c_csa2s. 
.sp
c_csa2xs returns a pointer to a linear array of data that is the approximated
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
out = c_csa2xs(ni, xi, yi, zi, wts, knots, smth, nderiv, 
               no, mo, xo, yo, &ier);
.fi
.cs R
.sp
then out[i*mo+j] is the approximated function value at coordinate point 
(xo[i], yo[j]) for 0 <= i < no and 0 <= j < mo. The space for out is 
allocated internal to c_csa2xs and is no * mo floats in size. 
.SH ACCESS
To use c_csa2xs, load the NCAR Graphics library ngmath.
.SH SEE ALSO
csagrid,
c_csa2s,
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

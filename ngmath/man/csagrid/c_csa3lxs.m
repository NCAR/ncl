.\"
.\"	$Id: c_csa3lxs.m,v 1.4 2008-07-27 03:35:33 haley Exp $
.\"
.TH c_csa3lxs 3NCARG "January 1999" UNIX "NCAR GRAPHICS"
.SH NAME
c_csa3lxs - cubic spline approximation, expanded entry for three-dimensional input, list output
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
float *c_csa3lxs(int, float [], float [], float [], float [],
                 float [], int [], float, int [],
                 int, float [], float [], float [], int *);
.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
float *c_csa3lxs(int n, float xi[], float yi[], float zi[], float ui[],
                 float wts[], int knots[3], float smth, int nderiv[3],
                 int no, float xo[], float yo[], float zo[], int *ier);
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
(real, input) An array containing the functional values of the input 
function -- ui[k] is the functional value at (xi[k], yi[k], zi[k]) 
for k=0,n-1. 
.IP wts 12
(real, input) An array containing weights for the ui values 
at the input values, that is, wts[l] is a
weight for the value of ui[l] for l=0,n-1. If 
you do not desire to weight the input ui
values, then set wts[0] to -1. The weights in 
the wts array are relative and may be set
to any non-negative value. When c_csa3lxs is 
called, the weights are summed and the
individual weights are normalized so that the weight sum is unity. 
.IP knots 12
(integer, input) The number of knots to be used in constructing the 
approximation spline.  knots[0], knots[1], and knots[2] 
must be at least 4. The larger the value 
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
nderiv[0]=1, nderiv[1]=1 and nderiv[2]=0, then 
the second order mixed partial with respect to X and Y would be computed. 
.IP no 12
(integer, input) The number of X - Y - Z coordinate values 
to be calculated for the output array. 
.IP xo 12
(real, input) An array dimensioned for no
containing the X coordinates of the output list.
.IP yo 12
(real, output) An array dimensioned for no
containing the Y coordinates of the output list.
.IP zo 12
(real, output) An array dimensioned for no
containing the Z coordinates of the output list.
.IP ier 12
(pointer to integer, output) An error return value. If *ier 
is returned as 0, then no errors were 
detected. If *ier is non-zero, then refer to the error list in the 
error table for details. 
.SH USAGE
c_csa3lxs is called to find values of an approximating cubic spline at
specified three-dimensional coordinates.  
c_csa3lxs is called if you want to weight the input data values, 
calculate derivatives, or handle data sparse areas specially.
If you do not want to do any of these three things, then use
c_csa3ls.
.sp
c_csa3lxs returns a pointer to a linear array of data that contains the
approximated values calculated at the input list of coordinate values. That is, if out is declared as 

.nf
.cs R 24
  float *out;
.fi
.cs R
.sp
and we set: 

.nf
.cs R 24
  out = c_csa3lxs(n, x, y, z, u, wts, knots, smth, nderiv, 
                  no, xo, yo, zo, &ier);
.fi
.cs R
.sp
then out[i] is the approximated function value at coordinate point 
(xo[i], yo[i], zo[i]) for 0 <= i < no. The space for out is allocated internal 
to c_csa3lxs and is no floats in size. 
.SH ACCESS
To use c_csa3lxs, load the NCAR Graphics library ngmath.
.SH SEE ALSO
csagrid,
c_csa3s,
c_csa3xs,
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

The use of this Software is governed by a License Agreement.

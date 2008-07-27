.\"
.\"	$Id: c_ftsurf.m,v 1.4 2008-07-27 03:35:38 haley Exp $
.\"
.TH c_ftsurf 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_ftsurf - 2D tension spline interpolation of rectangular data
.SH FUNCTION PROTOTYPE
float *c_ftsurf (int, int, float *, float *, float *, 
                 int, int, float *, float *, int *);
.SH SYNOPSIS
float *c_ftsurf (mi, ni, xi, yi, zi, mo, no, xo, yo, ier);
.SH DESCRIPTION
.IP mi 12
The number of grid lines in the X direction. (mi > 1) 
.IP ni 12
The number of grid lines in the Y direction. (ni > 1) 
.IP xi 12
Pointer to an array containing m X coordinates for grid 
lines in the X direction. 
.IP yi 12
Pointer to an array containing n Y coordinates for grid lines in 
the Y direction. 
.IP zi 12
Pointer to m x n floats which are the functional values at the 
grid points defined by xi and yi. 
.IP mo 12
The number of output values in the X direction. 
.IP no 12
The number of output values in the Y direction. 
.IP ier 12
The value *ier is an error flag as per: 
.sp
  = 0 -- no error. 
.br
  = 1 -- if n is less than 2 or m is less than 2. 
.br
  = 2 -- if X or Y values are not strictly increasing. 
.sp
.SH RETURN VALUE
c_ftsurf returns a pointer to an array containing mo x no floats 
which are the interpolated values on the grid specified by the arrays 
xo and yo. 
.SH USAGE
This procedure calculates an interpolatory surface passing
through a rectangular grid of function values. The surface
computed is a tensor product of splines under tension. 
.sp
c_ftsurf is called after all of the desired values for control
parameters have been set using the procedures c_ftseti, c_ftsetr,
c_ftsetc, c_ftsetfa. The control parameters that apply to c_ftsurf
are: sig, zx1, zxm, zy1, zyn, z11, zm1, z1n, zmn, df1, df2, df3,
df4, df5, df6, df7, df8 
.sp
The value for the parameter sig specifies the tension factor.
Values near zero result in a cubic spline; large values (e.g. 50)
result in nearly a polygonal line. A typical value is 1. (the
default). 
.sp
zx1 is an array containing n X-partial derivatives of the
function along the line xi[0], that is zx1[j] is the X-partial
derivative at point (x[0],y[j]) for j=0,n-1. This parameter may
be defaulted by setting the value for df1 appropriately. The
default is to compute zx1 internally. Values for zx1 can be set
using the procedure c_ftsetfa. 
.sp
zxm is an array containing n X-partial derivatives of the
function along the line xi[m-1], that is zxm[j] is the X-partial
derivative at point (xi[m-1],yi[j]) for j=0,n-1. This parameter
may be defaulted by setting the value for df2 appropriately. The
default is to compute zx2 internally. Values for zxm can be set
using the procedure c_ftsetfa. 
.sp
zy1 is an array containing m Y-partial derivatives of the
function along the line yi[0], that is zy1[j] is the Y-partial
derivative at point (x[i],y[0]) for i=0,m-1. This parameter may
be defaulted by setting the value for df3 appropriately. The
default is to compute zy1 internally. Values for zy1 can be set
using the procedure c_ftsetfa. 
.sp
zyn is an array containing m Y-partial derivatives of the
function along the line yi[n-1], that is zyn[j] is the Y-partial
derivative at point (x[i],y[n-1]) for i=0,m-1. This parameter
may be defaulted by setting the value for df4 appropriately. The
default is to compute zyn internally. Values for zyn can be set
using the procedure c_ftsetfa. 
.sp
z11, zm1, z1n, zmn specify X-Y-partial derivatives of the
function at the four corners (xi[0],yi[0]), (xi[m-1],yi[0]),
(xi[0],yi[n-1]), (xi[m-1],yi[n-1]), These parameters may be
defaulted by setting the values for df5, df6, df7, df8,
appropriately. The default is to compute z11, zm1, z1n, zmn
internally. 
.SH ACCESS
To use c_ftsurf, load the NCAR Graphics library ngmath.
.SH SEE ALSO
fitgrid_params, c_ftseti, c_ftsetr, c_ftsetc.
.sp
Complete documentation for Fitgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/fitgrid/fithome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.

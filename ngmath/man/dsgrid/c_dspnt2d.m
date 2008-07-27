.\"
.\"     $Id: c_dspnt2d.m,v 1.5 2008-07-27 03:35:36 haley Exp $
.\"
.TH c_dspnt2d 3NCARG "September 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
c_dspnt2d - Interpolate at a single point (or points) in 2D in single precision
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
void c_dspnt2d(int, double [], double [], double [],
.br
               int, double [], double [], double *, int *);
.fi
.cs R
.SH SYNOPSIS
.nf
.cs R 24
void c_dspnt2d (int n, double x[], double y[], double z[], 
.br
                int m, double xo[], double yo[], double zo, int *ier);    
.fi
.cs R
.SH DESCRIPTION 
.IP n 12
The number of input data points. 
.IP x 12
An array containing the X coordinates of the input data points.
.IP y 12
An array containing the Y coordinates of the input data points.
.IP z 12
An array containing the functional values of the input 
data points. That is, z[l] is the value of the input
function at coordinate (x[l],y[l]), for 0 <= l < n . 
.IP m 12
The number of output data points (this may be "1"). 
.IP xo 12
An array of dimension m containing the X coordinates of the 
output data. The values in xo may be in any order.
.IP yo 12
An array of dimension m containing the Y coordinates of the 
output data. The values in xo may be in any order.
.IP zo 12
A pointer to a double. Space for m doubles is allocated for zo by 
dspnt2d. zo[i] is the interpolated value at point (xo[i],yo[i]) 
for 0 <= i < n .
.IP ier 12
An error return value. If *ier is returned as 0,
then no errors were detected. If *ier is non-zero, then refer to
the man page for dsgrid_errors.
.SH USAGE
This function is called when you want to interpolate 2D data at an
individual point or points.
.SH ACCESS
To use c_dspnt2d, load the NCAR Graphics library ngmath.
.SH SEE ALSO
dsgrid,
dsgrid_params.
.sp
Complete documentation for Dsgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/dsgrid/dshome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.

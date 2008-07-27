.\"
.\"	$Id: c_shgetnp.m,v 1.4 2008-07-27 03:35:41 haley Exp $
.\"
.TH c_shgetnp 3NCARG "January 1999" UNIX "NCAR GRAPHICS"
.SH NAME
c_shgetnp - find the nearest points to a specified point in 3-space
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
int c_shgetnp(float, float, float, int, float *, float *, float *,
              int, int *);
.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
int c_shgetnp (px, py, pz, n, x[], y[], z[], iflag, ier );
.fi
.cs R
.sp
.SH DESCRIPTION
.IP px 12
(float) The X coordinate value for a point P 
whose nearest neighbor is to be found.
.IP py 12
(float) The Y coordinate value for a point P 
whose nearest neighbor is to be found.
.IP pz 12
(float) The Z coordinate value for a point P 
whose nearest neighbor is to be found.
.IP n 12
(int) The number of input data points, n > 1. 
.IP x 12
(float) An array of length n containing the X coordinate 
values for the input data points. 
.IP y 12
(float) An array of length n containing the Y coordinate 
values for the input data points. 
.IP z 12
(float) An array of length n containing the Z coordinate 
values for the input data points. 
.IP iflag 12
(int) A flag that equals 0 if this is the first call to 
this subroutine for the given dataset and equals 1 otherwise.
.IP ier 12
(pointer to int) An error return value. 
If *ier is returned as 0, then no errors were 
detected. If *ier is non-zero, then look at the man page for
shgrid_errors for details. 
.SH USAGE
c_shgetnp is called to find the nearest point to a specified point in 3-space. Successive
calls to c_shgetnp will determine the point nearest the specified point exclusive of the
points found in previous calls, i.e. successive calls can be used to find the N nearest points
for any N between one and the maximum number of points in the input dataset. 
.sp
c_shgetnp returns an integer, say np, such that 
(x[np],y[np],z[np]) is the nearest input data
point to P. np = -1 if *ier is not zero. On 
successive calls to this function after the first
(that is when iflag=1) you can find the Mth 
closest point to (px,py,pz) with the Mth call. 
.SH ACCESS
To use c_shgetnp, load the NCAR Graphics library ngmath.
.SH SEE ALSO
shgetnp,
c_shgrid,
c_shseti,
c_shgeti,
shgrid_params.
.sp
Complete documentation for Sh is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/shgrid/shhome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.

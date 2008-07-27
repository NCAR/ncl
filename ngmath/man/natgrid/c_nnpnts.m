.\"
.\"     $Id: c_nnpnts.m,v 1.6 2008-07-27 03:35:40 haley Exp $
.\"
.TH c_nnpnts 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
c_nnpnts - interpolate at a single point
.SH FUNCTION PROTOTYPE
void c_nnpnts(float, float, float *);
.SH SYNOPSIS
void c_nnpntinits (x, y, z);
.SH DESCRIPTION 
.IP x 12
The X coordinate of the point where interpolation is desired. 
.IP y 12
The Y coordinate of the point where interpolation is desired. 
.IP z 12
*z is the interpolated functional value at the point (X,Y).
.SH USAGE
This function is invoked when you want to interpolate at an individal
point.  c_nnpntinits must be called prior to calling c_nnpnts.
c_nnpntend must be called to terminate single point mode.
.SH ACCESS
To use c_nnpnts, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params,
c_natgrids,
c_nnpntinits,
c_nnpntend.
.sp
Complete documentation for Natgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/natgrid/nnhome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.

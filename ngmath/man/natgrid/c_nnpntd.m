.\"
.\"     $Id: c_nnpntd.m,v 1.5 2000-07-13 03:18:05 haley Exp $
.\"
.TH c_nnpntd 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
c_nnpntd - interpolate at a single point
.SH FUNCTION PROTOTYPE
void c_nnpntd(double, double, double *);
.SH SYNOPSIS
void c_nnpntd (x, y, z);
.SH DESCRIPTION 
.IP x 12
The X coordinate of the point where interpolation is desired. 
.IP y 12
The Y coordinate of the point where interpolation is desired. 
.IP z 12
*z is the interpolated functional value at the point (X,Y).
.SH USAGE
This functino is called when you want to interpolate at an individal
point.  c_nnpntinitd must be called prior to calling c_nnpntd.
c_nnpntendd must be called to terminate single point mode.
.SH ACCESS
To use c_nnpntd, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params,
c_natgrids,
c_nnpntinitd,
c_nnpntendd.
.sp
Complete documentation for Natgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/natgrid/nnhome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.


.\"
.\"     $Id: shgrid_errors.m,v 1.3 2000-08-22 15:15:17 haley Exp $
.\"
.TH shgrid_errors 3NCARG "January 1999" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
shgrid_errors - This document briefly describes all the
errors reported from Shgrid.
.SH DESCRIPTION 
Each entry below includes the error number and its meaning.
.IP "  0"
No error.
.IP "  1"
number of input data must be > 9 
.IP "  2"
NLS must be .GT. 9 
.IP "  3"
number of data points used in least squares fit must be > 9 
.IP "  4"
NFL (number of points used to calculate weights) must be at least 1 
.IP "  5"
number of points used in least squares fit too large
.IP "  6"
number of points used in calculating weights too large 
.IP "  7"
cell grid dimensions must be positive 
.IP "  8"
duplicate input points encountered 
.IP "  9"
collinear input, no unique solution 
.IP " 10"
at least two points must have different X coordinates 
.IP " 11"
at least two points must have different Y coordinates
.IP " 12"
at least two points must have different Z coordinates 
.IP " 13"
no cells contain a point within the radius of influence of the input point
.IP " 14"
negative search radius in calculating least squares fit 
.SH SEE ALSO
shgrid,
shseti,
shgeti,
shsetnp,
c_shgrid,
c_shseti,
c_shseti,
c_shgetnp.
.sp
Complete documentation for Shgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/shgrid/shhome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.


.\"
.\"     $Id: shgrid_errors.m,v 1.4 2008-07-27 03:35:42 haley Exp $
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

The use of this Software is governed by a License Agreement.

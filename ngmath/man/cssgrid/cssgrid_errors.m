.\"
.\"     $Id: cssgrid_errors.m,v 1.5 2008-07-27 03:35:35 haley Exp $
.\"
.TH cssgrid_errors 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
cssgrid_errors - This document briefly describes all the
errors reported from Cssgrid.
.SH DESCRIPTION 
Each entry below includes the error number and its meaning.
.IP " 1"
Invalid number of input points (must be greater than 3).
.IP " 2"
Invalid dimension for latitudes in the output grid. 
.IP " 3"
Invalid dimension for longitudes in the output grid. 
.IP " 4"
First three nodes of input array are collinear. 
.IP " 5"
Extrapolation failed due to the uniform grid extending too far beyond
the triangulation boundary. 
.IP " 6"
Internal algorithm error - please report this. 
.IP " 7"
Vertex of a triangle containing an interpolation point is outside its
valid range. 
.IP " 8"
The angular distance between an interpolated point and the nearest
point of the triangulation is at least 90 degrees. 
.IP " 9"
Not enough input points to calculate a gradient. 
.IP "10"
Insufficient space for the triangulation (must be >= number of
boundary nodes minus 2). 
.IP "11"
Degenerate triangle (two vertices lie on same geodesic). 
.IP "-L"
Coordinates L and M coincide for some M > L >= 1 (coordinate
numbering starting at 1). 
.SH SEE ALSO
css_overview,
cssgrid,
csstri,
csscoord,
cstrans,
csvoro,
c_cssgrid,
c_csstri,
c_csscoord,
c_cstrans,
c_csvoro 
.sp
Complete documentation for Cssgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/cssgrid/csshome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.

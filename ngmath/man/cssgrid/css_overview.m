.\"
.\"     $Id: css_overview.m,v 1.1 1999-06-29 00:23:02 fred Exp $
.\"
.TH Cssgrid 3NCARG "June 1999" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Cssgrid is a software package in the ngmath library that implements a 
cubic spline interpolation algorithm to fit a function to input data
randomly located on the surface of a sphere.
.sp
The output is a set of values at coordinates on a user-specified 
lat/lon grid (which can be a single point).
.sp
Functionally equivalent Fortran, C, and NCL interfaces are provided. 
.sp
Cssgrid is based on the work of Robert Renka and his packages
STRIPACK and SSRFPACK.
.SH SYNOPSIS
.sp
Fortran
.br
-----------------------------
.br
 CSSGRID  - interpolation on a sphere
.br
 CSSTRI   - calculates Delaunay triangulation
.br
 CSTRANS  - convert from lat/lon to Cartesian coordinates
.br
 CSSCOORD - convert from Cartesian to lat/lon coordinates
.br
 CSVORO   - calculate Voronoi polygons
.sp
C
.br
-----------------------------
.br
 c_cssgrid  - interpolation on a sphere
.br
 c_csstri   - calculates Delaunay triangulation
.br
 c_cstrans  - convert from lat/lon to Cartesian coordinates
.br
 c_csscoord - convert from Cartesian to lat/lon coordinates
.br
 c_csvoro   - calculate Voronoi polygons
.SH ACCESS 
To use Cssgrid entries, load the NCAR Graphics library ngmath.
.SH SEE ALSO
cssgrid,
csstri,
csscoord,
cstrans,
csvoro,
c_cssgrid,
c_csstri,
c_csscoord,
c_cstrans,
c_csvoro,
cssgrid_errors
.sp
Complete documentation for Cssgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/cssgrid/csshome.html
.SH COPYRIGHT
Copyright (C) 1999
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

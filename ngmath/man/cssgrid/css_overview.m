.\"
.\"     $Id: css_overview.m,v 1.5 2008-07-27 03:35:35 haley Exp $
.\"
.TH Cssgrid 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Cssgrid is a software package in the ngmath library that implements a 
tension spline interpolation algorithm to fit a function to input data
randomly located on the surface of a sphere.
.sp
There are user-settable parameters that control various aspects 
of the algorithm, the most important of which is the tension factor
that controls the level a tautness of the spline fit.  This varies
from pure cubic spline interpolation for a tension factor of 0. to near linear
interpolation for a large tension factor.  By default, the tension
factor is automatically computed to optimize certain fitting criteria.
.sp
The output is a set of values at coordinates on a user-specified 
lat/lon grid (which can be a single point).
Procedures also exist for finding a Dealunay triangulation and
Voronoi polygons.
.sp
Functionally equivalent Fortran, C, and NCL interfaces are provided. 
Fortran and C codes are available in both single and double precision.
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
 CSVORO   - calculates Voronoi polygons
.br
 CSS2C    - converts from lat/lon to Cartesian coordinates
.br
 CSC2S    - converts from Cartesian to lat/lon coordinates
.br
 CSSETI   - set values for integer-valued parameters
.br
 CSGETI   - retrieve values for integer-valued parameters
.br
 CSSETR   - set values for real valued parameters
.br
 CSGETR   - retrieve values for real valued parameters
.br
 CSSETD   - set values for double precision-valued parameters
.br
 CSGETD   - retrieve values for double precision-valued parameters
.br
 CSSGRIDD - interpolation on a sphere (double precision entry)
.br
 CSSTRID  - calculates Delaunay triangulation (double precision entry)
.br
 CSVOROD  - calculates Voronoi polygons (double precision entry)
 CSS2CD   - converts from lat/lon to Cartesian coordinates (double precision entry)
.br
 CSC2SD   - converts from Cartesian to lat/lon coordinates (double precision entry)
.br
-----------------------------
.br
 c_cssgrid  - interpolation on a sphere
.br
 c_csstri   - calculates Delaunay triangulation
.br
 c_csvoro   - calculates Voronoi polygons
.br
 c_css2c    - converts from lat/lon to Cartesian coordinates
.br
 c_csc2s    - converts from Cartesian to lat/lon coordinates
.br
 c_csseti   - set values for integer-valued parameters
.br
 c_csgeti   - retrieve values for integer-valued parameters
.br
 c_cssetr   - set values for real valued parameters
.br
 c_csgetr   - retrieve values for real valued parameters
.br
 c_cssetd   - set values for double precision-valued parameters
.br
 c_csgetd   - retrieve values for double precision-valued parameters
.br
 c_cssgridd - interpolation on a sphere (double precision entry)
.br
 c_csstrid  - calculates Delaunay triangulation (double precision entry)
.br
 c_csvorod  - calculates Voronoi polygons (double precision entry)
.br
 c_css2cd   - converts from lat/lon to Cartesian coordinates (double precision entry)
.br
 c_csc2sd   - converts from Cartesian to lat/lon coordinates (double precision entry)
.SH ACCESS 
To use Cssgrid entries, load the NCAR Graphics library ngmath.
.SH SEE ALSO
cssgrid_params
cssgrid_errors
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

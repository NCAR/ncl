.\"
.\"	$Id: csvoro.m,v 1.1 1999-06-29 00:23:03 fred Exp $
.\"
.TH CSVORO 3NCARG "JUNE 1999" UNIX "NCAR GRAPHICS"
.SH NAME
CSVORO - calculate Voronoi polygons for data on a sphere.
.SH SYNOPSIS
CALL CSVORO (NPTS, X, Y, Z, NI, NF, IWK, RWK, 
.br
             NC, XC, YC, ZC, RC, 
.br
             NCA, NUMV, NV, IER)
.SH DESCRIPTION
.IP NPTS 12
(integer,input) The number of input data points (NPTS > 3). 
.IP X 12
(real, input) An array, dimensioned for NPTS, containing the 
X Cartesian coordinates of the input data points. 
.IP Y 12
(real, input) An array, dimensioned for NPTS, containing 
the Y Cartesian coordinates of the input data points. 
.IP Z 12
(real, input) An array, dimensioned for NPTS, containing the 
Z Cartesian coordinates of the input data points.
.IP NI 12
(integer, input) The index of the input coordinate for which you 
want to determine the Voronoi polygon (1 .LE. NI .LE. NPTS). 
.IP NF 12
(integer, input) Flag indicating if this is the first call to 
CSVORO to retrieve Voronoi polygons for this dataset (1=yes, 0=no). 
Calls subsequent to the first call for a given dataset are
much faster than the first call. 
.IP IWK 12 
(integer, input) Integer work space dimensioned for 27*NPTS. 
.IP RWK 12
(real, input) Real work space dimensioned for NPTS. 
.IP NC 12
(integer, input) The maximum size of the output arrays 
XC, YC, ZC, and RC. NC should be 2*NPTS.
.IP XC 12
(real, output) The X Cartesian coordinates for the Voronoi indices. 
These are circumcenters of circles passing through the Delaunay 
triangles. If a coordinate is a boundary point, then the circle 
may pass through certain "pseudo points" that have been added to the
original dataset in order to complete the Voronoi polygon. 
.IP YC 12
(real, output) The Y Cartesian coordinates for the Voronoi indices. 
.IP ZC 12
(real, output) The Z Cartesian coordinates for the Voronoi indices. 
.IP RC 12
(real, output) Array containing circumradii (arc lengths in degrees 
of the angle between a circumcenter and its associated triangle vertices). 
.IP NCA 12
(integer, output) The actual number of circumcenters returned in 
XC, YC and ZC. This number may be larger than NPTS if the input 
dataset has boundary points since certain "pseudo points" may 
have been added to the original dataset in order to complete the Voronoi
polygon set. 
.IP NUMV 12
(integer, output) The number of vertices in the Voronoi polygon enclosing 
the coordinate (X(NI),Y(NI),Z(NI)). 
.IP NV 12
(integer, output) An array (dimensioned for NPTS) containing 
NUMV indices for the Voronoi polygon
enclosing the coordinate (X(NI),Y(NI),Z(NI). 
The indices returned in this array refer
to the coordinates returned in XC, YC, and ZC. 
For example, if the integer "J" is an
element of the NV array, then (XC(J),YC(J),ZC(J)) 
is a vertex of the Voronoi polygon enclosing (X(NI),Y(NI),Z(NI))). 
The indices in NV list out the vertices of
the Voronoi polygon in counter-clockwise order. 
.IP IER 12
(integer, output) An error return value.  If IER is returned as 0, then
no errors were detected. If IER is non-zero, then refer to the man
page for cssgrid_errors for details.
.SH USAGE
CSVORO is called if you want to determine the Voronoi polygons 
for data randomly positioned on a sphere. Each call to CSVORO 
calculates the vertices for the Voronoi polygon surrounding a 
specified input point. 
.SH ACCESS
To use CSSGRID, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
csstri,
csscoord,
cstrans,
cssgrid,
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
Copyright (C) 1997-1999
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

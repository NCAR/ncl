.\"
.\"	$Id: csvorod.m,v 1.4 2008-07-27 03:35:35 haley Exp $
.\"
.TH CSVOROD 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.SH NAME
CSVOROD - calculate Voronoi polygons for data on a sphere.
.SH SYNOPSIS
CALL CSVOROD (NPTS, RLATI, RLONI, NI, NF, IWK, RWK, 
.br
             NC, RLATO, RLONO, RC, 
.br
             NCA, NUMV, NV, IER)
.SH DESCRIPTION
.IP NPTS 12
(integer,input) The number of input data points (NPTS > 3). 
.IP RLATI 12
(double precision, input) An array containing the latitudes
of the input data, expressed in degrees.
The first three points must not be collinear
(lie on a common great circle).
.IP RLONI 12
(double precision, input) An array containing the longitudes of the input data,
expressed in degrees.
.IP NI 12
(integer, input) The index of the input coordinate for which you 
want to determine the Voronoi polygon (1 .LE. NI .LE. NPTS). 
.IP NF 12
(integer, input) Flag indicating if this is the first call to 
CSVOROD to retrieve Voronoi polygons for this dataset (1=yes, 0=no). 
Calls subsequent to the first call for a given dataset are
much faster than the first call. 
.IP IWK 12 
(integer, input) Integer work space dimensioned for 27*NPTS. 
.IP RWK 12
(double precision, input) A work space dimensioned 
for 9*NPTS.  Note that RWK must be typed DOUBLE PRECISION.
.IP NC 12
(integer, input) The maximum size of the output arrays 
RLATO, RLONO, and RC. NC should be 2*NPTS.
.IP RLATO 12
(double precision, output) The latitudes for the vertices of the Voronoi polygons.
These are circumcenters of circles passing through the Delaunay 
triangles. If a coordinate is a boundary point, then the circle 
may pass through certain "pseudo points" that have been added to the
original dataset in order to complete the Voronoi polygon. 
.IP RLONO 12
(double precision, output) The longitudes for the vertices of the Voronoi polygons.
.IP RC 12
(double precision, output) Array containing circumradii (arc lengths in degrees 
of the angle between a circumcenter and its associated triangle vertices). 
.IP NCA 12
(integer, output) The actual number of circumcenters returned in 
RLATO and RLONO. This number may be larger than NPTS if the input 
dataset has boundary points since certain "pseudo points" may 
have been added to the original dataset in order to complete the Voronoi
polygon set. 
.IP NUMV 12
(integer, output) The number of vertices in the Voronoi polygon enclosing 
the coordinate (RLATI(NI),RLONI(NI)).
.IP NV 12
(integer, output)
An array (dimensioned for NPTS) containing NUMV indices for the 
Voronoi polygon enclosing the
coordinate (RLATI(NI),RLONI(NI)). The indices returned in 
this array refer to the coordinates returned in
RLATO, RLONO, and RC. For example, if the integer "J" is an 
element of the NV array, then
(RLATO(J),RLONO(J)) is a vertex of the Voronoi polygon 
enclosing (RLATI(NI),RLONI(NI)). The indices
in NV list out the vertices of the Voronoi polygon in counter-clockwise order. 
.IP IER 12
(integer, output) An error return value.  If IER is returned as 0, then
no errors were detected. If IER is non-zero, then refer to the man
page for cssgrid_errors for details.
.SH USAGE
CSVOROD is called if you want to determine the Voronoi polygons 
for data randomly positioned on a sphere. Each call to CSVOROD
calculates the vertices for the Voronoi polygon surrounding a 
specified input point.  CSVOROD is a double precision version of
CSVORO.
.SH ACCESS
To use CSVOROD, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
csstrid,
cssgridd.
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

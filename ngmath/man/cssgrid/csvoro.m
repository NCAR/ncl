.\"
.\"	$Id: csvoro.m,v 1.3 2000-07-13 03:17:52 haley Exp $
.\"
.TH CSVORO 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.SH NAME
CSVORO - calculate Voronoi polygons for data on a sphere.
.SH SYNOPSIS
CALL CSVORO (NPTS, RLATI, RLONI, NI, NF, IWK, RWK, 
.br
             NC, RLATO, RLONO, RC, 
.br
             NCA, NUMV, NV, IER)
.SH DESCRIPTION
.IP NPTS 12
(integer,input) The number of input data points (NPTS > 3). 
.IP RLATI 12
(real, input) An array containing the latitudes
of the input data, expressed in degrees.
The first three points must not be collinear
(lie on a common great circle).
.IP RLONI 12
(real, input) An array containing the longitudes of the input data,
expressed in degrees.
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
(double precision, input) A work space dimensioned 
for 9*NPTS.  Note that RWK must be typed DOUBLE PRECISION.
.IP NC 12
(integer, input) The maximum size of the output arrays 
RLATO, RLONO, and RC. NC should be 2*NPTS.
.IP RLATO 12
(real, output) The latitudes for the vertices of the Voronoi polygons.
These are circumcenters of circles passing through the Delaunay 
triangles. If a coordinate is a boundary point, then the circle 
may pass through certain "pseudo points" that have been added to the
original dataset in order to complete the Voronoi polygon. 
.IP RLONO 12
(real, output) The longitudes for the vertices of the Voronoi polygons.
.IP RC 12
(real, output) Array containing circumradii (arc lengths in degrees 
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
CSVORO is called if you want to determine the Voronoi polygons 
for data randomly positioned on a sphere. Each call to CSVORO 
calculates the vertices for the Voronoi polygon surrounding a 
specified input point. 
.SH ACCESS
To use CSVORO, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
csstri,
cssgrid.
.sp
Complete documentation for Cssgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/cssgrid/csshome.html
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


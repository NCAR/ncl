.\"
.\"	$Id: c_csvorod.m,v 1.4 2008-07-27 03:35:35 haley Exp $
.\"
.TH c_csvorod 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.SH NAME
c_csvorod - calculate Voronoi polygons for points on a sphere.
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
void   c_csvorod(int, double [], double [], 
.br
                int, int, double [], double [], double [], 
.br
                int *, int *, int [], int *);
.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
void *c_csvorod (n, rlat, rlon, ni, nf, plat, plon, rc,
.br
                nca, numv, nv, ier);
.fi
.cs R
.sp
.SH DESCRIPTION
.IP n 12
The number of input data points, n > 2. 
.IP rlat 12
An array containing the latitudes
of the input data, expressed in degrees.
The first three points must not be collinear
(lie on a common great circle).
.IP rlon 12
An array containing the longitudes of the input data,
expressed in degrees.
.IP ni 12
The index of the input coordinate for which you 
want to determine the Voronoi polygon (0 <= ni < n).
.IP nf 12
Flag indicating if this is the first call to c_csvorod to 
retrieve Voronoi polygons for this
dataset (1=yes, 0=no). Calls subsequent to the 
first call for a given dataset are much
faster than the first call. 
.IP plat 12
The latitudes for the vertices of the Voronoi polygons.
These are circumcenters of circles passing through the Delaunay
triangles. If a coordinate is a boundary point, then the circle
may pass through certain "pseudo points" that have been added to the
original dataset in order to complete the Voronoi polygon.
.IP plon 12
The longitudes for the vertices of the Voronoi polygons.
.IP rc 12
Array containing circumradii (arc lengths in
degrees of the angle between a circumcenter and
its associated triangle vertices). 
.IP nca 12
*nca is the actual number of circumcenters
returned in plat and plon. This number may be
larger than n if the input dataset has boundary
points, since certain "pseudo points" may have
been added to the original dataset in order to
complete the Voronoi polygon set. 
.IP numv 12
*numv is the number of vertices in the Voronoi
polygon enclosing the coordinate
(rlat[ni],rlon[ni]). 
.IP nv 12
An array containing numv indices for the
Voronoi polygon enclosing the coordinate
(rlat[ni],rlon[ni]). The indices returned in this
array refer to the coordinates returned in plat and plon.
For example, if the integer "j" is an
element of the nv array, then (plat[j],plon[j]) is
a vertex of the Voronoi polygon enclosing
(rlat[ni],rlon[ni]). The indices in nv list out the
vertices of the Voronoi polygon in
counter-clockwise order. 
.IP ier 12
An error return value. If *ier is returned as 0,
then no errors were detected. If *ier is non-zero,
then refer to the error list in the error table for details. 
.SH USAGE
c_csvorod is called if you want to determine the
Voronoi polygons for data randomly positioned on a
sphere. Each call to c_csvorod calculates the vertices
for the Voronoi polygon surrounding a specified input
point.  c_csvorod is a double precision version of c_csvoro.
.SH ACCESS
To use c_csvorod, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
c_cssgrid,
c_csvoro,
cssgrid_errors.
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

.\"
.\"	$Id: c_csvoro.m,v 1.1 1999-06-29 00:23:02 fred Exp $
.\"
.TH c_csvoro 3NCARG "June 1999" UNIX "NCAR GRAPHICS"
.SH NAME
c_csvoro - calculate Voronoi polygons for points on a sphere.
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
void   c_csvoro(int, float [], float [], float [],
.br
                int, int, float [], float [], float [], 
.br
                float [], int *, int *, int [], int *);
.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
void *c_csvoro (n, x, y, z, ni, nf, xc, yc, zc, rc,
.br
                nca, numv, nv, ier);
.fi
.cs R
.sp
.SH DESCRIPTION
.IP n 12
The number of input data points, n > 2. 
.IP x 12
An array containing the X Cartesian coordinates of the input data points. 
.IP y 12
An array containing the Y Cartesian coordinates of the input data points. 
.IP z 12
An array containing the Z Cartesian coordinates of the input data points. 
.IP ni 12
The index of the input coordinate for which you 
want to determine the Voronoi polygon (0 <= ni < n).
.IP nf 12
Flag indicating if this is the first call to c_csvoro to 
retrieve Voronoi polygons for this
dataset (1=yes, 0=no). Calls subsequent to the 
first call for a given dataset are much
faster than the first call. 
.IP xc 12
The X Cartesian coordinates for the Voronoi
indices. These are circumcenters of circles
passing through the Delaunay triangles. If a
coordinate is a boundary point, then the circle
may pass through certain "pseudo points" that
have been added to the original dataset in order
to complete the Voronoi polygon. 
.IP yc 12
The Y Cartesian coordinates for the Voronoi indices. 
.IP zc 12
The Z Cartesian coordinates for the Voronoi indices. 
.IP rc 12
Array containing circumradii (arc lengths in
degrees of the angle between a circumcenter and
its associated triangle vertices). 
.IP nca 12
*nca is the actual number of circumcenters
returned in xc, yc and zc. This number may be
larger than n if the input dataset has boundary
points, since certain "pseudo points" may have
been added to the original dataset in order to
complete the Voronoi polygon set. 
.IP numv 12
*numv is the number of vertices in the Voronoi
polygon enclosing the coordinate
(x[ni],y[ni],z[ni]). 
.IP nv 12
An array containing numv indices for the
Voronoi polygon enclosing the coordinate
(x[ni],y[ni],z[ni]). The indices returned in this
array refer to the coordinates returned in xc, yc,
and zc. For example, if the integer "j" is an
element of the nv array, then (xc[j],yc[j],zc[j]) is
a vertex of the Voronoi polygon enclosing
(x[ni],y[ni],z[ni]). The indices in nv list out the
vertices of the Voronoi polygon in
counter-clockwise order. 
.IP ier 12
An error return value. If *ier is returned as 0,
then no errors were detected. If *ier is non-zero,
then refer to the error list in the error table for details. 
.SH USAGE
c_csvoro is called if you want to determine the
Voronoi polygons for data randomly positioned on a
sphere. Each call to c_csvoro calculates the vertices
for the Voronoi polygon surrounding a specified input
point. 
.SH ACCESS
To use c_csvoro, load the NCAR Graphics library ngmath.
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

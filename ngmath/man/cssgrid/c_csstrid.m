.\"
.\"	$Id: c_csstrid.m,v 1.4 2008-07-27 03:35:34 haley Exp $
.\"
.TH c_csstrid 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.SH NAME
c_csstrid - calculates a Delaunay triangulation for data on a sphere
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
    int *c_csstrid(int, double [], double [], int *, int *);
.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
    int *c_csstrid(n, rlat, rlon, nt, ier);
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
.IP nt 12
*nt is the number of triangles in 
the triangulation, unless *ier is non-zero, in which case *nt = 0.
Where nb is the number of boundary points on the
convex hull of the data, if nb is greater than 3, then
*nt = 2n-nb-2, otherwise *nt = 2n-4.
The input data are considered to be bounded if they all
lie in one hemisphere.
.IP ier 12
An error return value. If *ier is
returned as 0, then no errors were
detected. If *ier is non-zero, then refer to the error list in
cssgrid_errors for details.
.SH USAGE
c_csstrid is called to find a Delaunay triangulation of data 
randomly positioned on the surface of a sphere. c_csstrid is
a double precision version of c_csstri.
.SH RETURN VALUE
c_csstrid returns a pointer to a linear array that 
contains a sequence of integer triples. The
elements of a triple are indices of vertices of 
a triangle. Each index references an original
data point as it occurs in sequence in the input 
data set (numbering starts at 0). For example, if the 
triple <5,0,2> were in the list of triples, then (rlat[5],rlon[5]),
(rlat[0],rlon[0]), and (rlat[2],rlon[2]) would be vertices of 
a triangle in the Delaunay triangulation. 
.SH ACCESS
To use c_csstrid, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
c_cssgrid,
c_csstri,
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

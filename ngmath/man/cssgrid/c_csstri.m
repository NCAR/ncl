.\"
.\"	$Id: c_csstri.m,v 1.1 1999-06-29 00:23:02 fred Exp $
.\"
.TH c_csstri 3NCARG "June 1999" UNIX "NCAR GRAPHICS"
.SH NAME
c_csstri - calculates a Delaunay triangulation for data on a sphere
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
    int   *c_csstri(int, float [], float [], float [], int *, int *);
.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
    int *c_cssgrid (n, x, y, z, nt, ier)
.fi
.cs R
.sp
.SH DESCRIPTION
.IP n 12
The number of input data points, n > 2. 
.IP x 12
(float) An array containing the X component of the Cartesian
coordinates of the input data. pow(x[i],2) + pow(y[i],2) +
pow(z[i],2) = 1. for i = 0 to n-1. 
.IP y 12
An array containing the Y component of the Cartesian
coordinates of the input data. pow(x[i],2) + pow(y[i],2) +
pow(z[i],2) = 1. for i = 0 to n-1.
.IP z 12
An array containing the Z component of the Cartesian
coordinates of the input data. pow(x[i],2) + pow(y[i],2) +
pow(z[i],2) = 1. for i = 0 to n-1.
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
c_csstri is called to find a Delaunay triangulation of data 
randomly positioned on the surface of a sphere. 
.SH RETURN VALUE
c_csstri returns a pointer to a linear array that 
contains a sequence of integer triples. The
elements of a triple are indices of vertices of 
a triangle. Each index references an original
data point as it occurs in sequence in the input 
data set (numbering starts at 0). For example, if the 
triple <5,0,2> were in the list of triples, then (x[5],y[5],z[5]),
(x[0],y[0],z[0]), and (x[2],y[2],z[2]) would be vertices of 
a triangle in the Delaunay triangulation. 
.SH ACCESS
To use c_csstri, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
cssgrid,
csscoord,
cstrans,
csvoro,
c_cssgrid,
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

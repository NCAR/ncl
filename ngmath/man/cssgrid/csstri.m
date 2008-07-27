.\"
.\"	$Id: csstri.m,v 1.5 2008-07-27 03:35:35 haley Exp $
.\"
.TH CSSTRI 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.SH NAME
CSSTRI - calculates a Delaunay triangulation for data on a sphere
.SH SYNOPSIS
CALL CSSTRI (N, RLAT, RLON, NT, NTRI, IWK, RWK, IER)
.SH DESCRIPTION
.IP N 12
(integer,input) The number of input data points (N > 2). 
.IP RLAT 12
(real, input) An array containing the latitudes
of the input data, expressed in degrees.
The first three points must not be collinear 
(lie on a common great circle). 
.IP RLON 12
(real, input) An array containing the longitudes of the input data,
expressed in degrees.
.IP NT 12
(integer, output) The number of triangles in the triangulation, 
unless IER .NE. 0, in which case NT = 0. Where NB is the number 
of boundary points on the convex hull of the data, if NB .GE. 3, 
then NT = 2N-NB-2, otherwise NT=2N-4. The input data are considered to
be bounded if they all lie in one hemisphere.  Dimensioning NT for
2*N will always work.
.IP NTRI 12
(integer, output) A two-dimensional integer array dimensioned for 
3 x NT where NT is the number of triangles in the
triangulation (NT is at most 2*N). NTRI contains the 
triangulation data. The vertices of the Kth triangle are: 
(PLAT(NTRI((1,K)),PLON(NTRI(1,K)), (PLAT(NTRI((2,K)),PLON(NTRI(2,K)),
(PLAT(NTRI((3,K)),PLON(NTRI(3,K))
.IP IWK 12 
(integer, input) An integer workspace of length 27*N. 
.IP RWK 12
(double precision, input) A work array dimensioned for 13*N.  Note
that this work array must be typed DOUBLE PRECISION.
.IP IER 12
(integer, output) An error return value.  If IER is returned as 0, then
no errors were detected. If IER is non-zero, then refer to the man
page for cssgrid_errors for details.
.SH USAGE
CSSTRI is called to find a Delaunay triangulation of data randomly 
positioned on the surface of a sphere. 
.SH ACCESS
To use CSSTRI, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
cssgrid,
csvoro.
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

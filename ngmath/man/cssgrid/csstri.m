.\"
.\"	$Id: csstri.m,v 1.1 1999-06-29 00:23:03 fred Exp $
.\"
.TH CSSTRI 3NCARG "JUNE 1999" UNIX "NCAR GRAPHICS"
.SH NAME
CSSTRI - calculates a Delaunay triangulation for data on a sphere
.SH SYNOPSIS
CALL CSSTRI (N, X, Y, Z, NT, NTRI, IWK, WK, IER)
.SH DESCRIPTION
.IP N 12
(integer,input) The number of input data points (N > 2). 
.IP X 12
(real, input) An array containing the X component of the Cartesian 
coordinates of the input data. X(I)**2 + Y(I)**2 + Z(I)**2 = 1. 
for I = 1 to N. The first three points must not be collinear 
(lie on a common great circle). 
.IP Y 12
(real, input) An array containing the Y component of the Cartesian 
coordinates of the input data. X(I)**2 + Y(I)**2 + Z(I)**2 = 1. 
for I = 1 to N. The first three points must not be collinear 
(lie on a common great circle). 
.IP Z 12
(real, input) An array containing the Z component of the Cartesian 
coordinates of the input data. X(I)**2 + Y(I)**2 + Z(I)**2 = 1. 
for I = 1 to N. The first three points must not be collinear 
(lie on a common great circle). 
.IP NT 12
(integer, input) The number of triangles in the triangulation, 
unless IER .NE. 0, in which case NT = 0. Let NB be the number 
of boundary points on the convex hull of the data. If NB .GE. 3, 
then NT = 2N-NB-2, otherwise NT=2N-4. The input data are considered to
be bounded if they all lie in one hemisphere.  Dimensioning NT for
2*N will always work.
.IP NTRI 12
(integer, output) A two-dimensional integer array dimensioned 
for 3 x NT where NT is the number of triangles in the triangulation 
(NT is at most 2*N). NTRI contains the triangulation
data. The vertices of the Jth triangle are X(NTRI(1,J)), Y(NTRI(2,J)) and
Z(NTRI(3,J)). 
.IP IWK 12 
(integer, input) An integer workspace of length 27*N. 
.IP WK 12
(real, input) A work array dimensioned for N.
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
Copyright (C) 1997-1999
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

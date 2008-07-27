.\"
.\"	$Id: shgetnp.m,v 1.4 2008-07-27 03:35:42 haley Exp $
.\"
.TH SHGETNP 3NCARG "September 1999" UNIX "NCAR GRAPHICS"
.SH NAME
SHGETNP - find the nearest points to a specified point in 3-space.
.SH SYNOPSIS
CALL SHGETNP (PX, PY, PZ, N, X, Y, Z, IFLAG, IWK, RWK, NP, IER)
.SH DESCRIPTION
.IP PX 12
(real,input) The X coordinate value for a point P 
whose nearest neighbor is to be found. 
.IP PY 12
(real,input) The Y coordinate value for a point P 
whose nearest neighbor is to be found. 
.IP PZ 12
(real,input) The Z coordinate value for a point P 
whose nearest neighbor is to be found. 
.IP X 12
(real, input) An array containing the X coordinates of the input data points.
.IP Y 12
(real, input) An array containing the Y coordinates of the input data points.
.IP Z 12
(real, input) An array containing the Z coordinates of the input data points.
.IP IFLAG 12
(integer, input) A flag that equals 0 if this is the first call 
to this subroutine for the given dataset and equals 1 otherwise. 
.IP IRK 12 
(integer, input) An integer workspace dimensioned for at least 2*N. 
.IP RWK 12
(real, input) A real workspace dimensioned for at least 11*N+6. 
.IP NP 12
(integer, output) An index such that (X(NP),Y(NP),Z(NP)) is the nearest input 
data point to P. NP = 0 if IER .NE. 0. On successive calls to this 
subroutine after the first (that is when IFLAG=1) you can find the 
Mth closest point to (PX,PY,PZ) with the Mth call. IWK and RWK should 
not be modified between calls to SHGETNP if you are wanting to
find successive nearest neighbors. 
.IP IER 12
(integer, output) An error return value.  If IER is returned as 0, then
no errors were detected. If IER is non-zero, then refer to the man
page for shgrid_errors for details.
.SH USAGE
SHGETNP is called to find the nearest point to a specified point in 3-space. 
Successive calls to SHGETNP will determine the point nearest the 
specified point exclusive of the points found in previous calls, 
i.e. successive calls can be used to find the N nearest points
for any N between one and the maximum number of points in the input dataset. 
.SH ACCESS
To use SHGETNP, load the NCAR Graphics library ngmath.
.SH SEE ALSO
shgrid,
shseti,
shgeti,
shgrid_params.
.sp
Complete documentation for Shgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/shgrid/shhome.html
.SH COPYRIGHT
Copyright (C) 1997-2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.

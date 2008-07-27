.\"
.\"     $Id: nnpntinits.m,v 1.6 2008-07-27 03:35:41 haley Exp $
.\"
.TH NNPNTINITS 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.SH NAME
NNPNTINITS- Enter single point mode
.SH SYNOPSIS
CALL NNPNTINITS (NPNTS, X, Y, Z)
.SH DESCRIPTION
.IP NPNTS 12
(Integer, Input) - The number of input data points. (NPNTS > 3).
.IP X 12
(Real, Input) - An array of dimension NPNTS containing the X 
coordinates of the input data points.
.IP Y 12
(Real, Input) - An array of dimension NPNTS containing the Y 
coordinates of the input data points. 
.IP Z 12
(Real, Input) - An array of dimension NPNTS containing the functional 
values of the input data points. That is, Z(L) is the value of the 
input function at coordinate (X(L),Y(L)), for L=1,NPNTS. 
.SH USAGE
This subroutine is called when you want to interpolate at individal
points.  It is an initialization routine that sets up some internal
variables and does this initial triangulation.  To actually do the
interplation, use subroutine NNPNTS.  To terminate single point mode,
use the subroutine NNPNTEND.
.SH ACCESS
To use NNPNTINITS, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params,
natgrids,
nnpnts,
nnpntend.
.sp
Complete documentation for Natgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/natgrid/nnhome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.

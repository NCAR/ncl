.\"
.\"     $Id: nnpntinits.m,v 1.4 2000-07-13 03:18:09 haley Exp $
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


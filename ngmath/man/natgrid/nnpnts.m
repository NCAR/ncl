.\"
.\"     $Id: nnpnts.m,v 1.5 2000-08-22 15:15:11 haley Exp $
.\"
.TH NNPNTS 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.SH NAME
NNPNTS- Interpolate at a single point
.SH SYNOPSIS
CALL NNPNTS (X, Y, Z)
.SH DESCRIPTION
.IP X 12
(Real, Input) - The X coordinate of the point where interpolation is desired.
.IP Y 12
(Real, Input) - The Y coordinate of the point where interpolation is desired. 
.IP Z 12
(Real, Output) - The interpolated functional value at the point (X,Y).
.SH USAGE
This subroutine is called when you want to interpolate at an individal
point.  NNPNTINITS must be called prior to calling NNPNTS.  NNPNTEND
must be called to terminate single point mode.
.SH ACCESS
To use NNPNTS, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params,
natgrids,
nnpntinits,
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
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.


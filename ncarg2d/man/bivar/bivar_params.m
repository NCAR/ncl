.TH Bivar_params 3NCARG "November 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Bivar_params - This document briefly describes all Bivar parameters.
.SH DESCRIPTION
Bivar currently supports two parameters. The current values may be retrieved
using one of the routines IDGETI or IDGETR. Parameter values may be reset
using the routine IDSETI or IDSETR.
.sp
The Bivar parameter descriptions appear below in alphabetical
order. Each description begins with a line giving the parameter name 
and the intrinsic FORTRAN type of the parameter.
.IP "\&'ITY' - Integer"
Interpolation Type.  The default value, 0, says that quintic interpolation
should be done.  The value 1 says to use linear interpolation.
.IP "\&'TTY' - Integer"
Triangulation Type.  The default value, 0, says to use a triangulation due
to C. L. Lawson that maximizes the minimum angle occurring in the triangles.
The value 1 says to use the Delaunay triangulation, which ensures triangles
whose circumscribed circles have no data points in their interiors.
.SH SEE ALSO
Online:
bivar, idbvip, idsfft, idpltr, idgeti, idgetr, idseti, idsetr,
ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2005
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

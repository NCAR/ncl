.\"
.\"	$Id: gpl.m,v 1.13 2006-01-04 00:12:55 haley Exp $
.\"
.TH GPL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GPL (Polyline) - This output primitive draws
line segments connecting a sequence of user-specified coordinate pairs.
.SH SYNOPSIS
CALL GPL (N, X, Y)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gpolyline(const Gpoint_list *point_list);
.SH DESCRIPTION
.IP N 12
(Integer, Input) - The number of points in the line to 
be drawn. N must be larger than one.
.IP "X (N)" 12
(Real Array, Input) - The X coordinates (specified in world
coordinates) of the N points to be connected by line segments.  
.IP "Y (N)" 12
(Real Array, Input) - The Y coordinates (specified in world coordinates)
of the N points to be connected by line segments.
.SH USAGE
Note that the coordinate pairs must be in world coordinates and not
user coordinates.  Among other things, this means that the log scaling
and mirror-imaging features available via the SET call and the SPPS
functions for drawing lines are not applicable here.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
gsln, gslwsc, gscr, gsplci, gqln, gqlwsc, gqplci, dashline, set, gpolyline
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
"The Use of X/Y Coordinates in NCAR Graphics" SCD User Document"
.SH COPYRIGHT
Copyright (C) 1987-2006
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

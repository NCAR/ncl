.\"
.\"	$Id: gpm.m,v 1.10 2000-08-22 04:15:58 haley Exp $
.\"
.TH GPM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GPM (Polymarker) - the polymarker output primitive draws selected
symmetric symbols to mark user-specified coordinate positions.
.SH SYNOPSIS
CALL GPM (N, X, Y)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gpolymarker(const Gpoint_list *point_list);
.SH DESCRIPTION
.IP N 12
(Integer, Input) - The number of markers to be drawn.  N must be
greater than zero.
.IP "X (N)" 12
(Real Array, Input) - The X coordinates of the N markers to be 
drawn, in world coordinates.
.IP "Y (N)" 12
(Real Array, Input) - The Y coordinates of the N markers to be 
drawn, in world coordinates.
.SH USAGE
By default, the polymarker type is an asterisk. To 
select other polymarker types and attributes, see the man
page for gsmk.
.sp
Note that the coordinate pairs must be in world coordinates and not
user coordinates.  Among other things, this means that the log scaling
and mirror-imaging features available via the SET call and the SPPS
functions for drawing dots and markers are not applicable here.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gsmk, gsmksc, gscr, gspmci, gqmk, gqmksc, gqpmci, 
point, points, ngdots, gpolymarker
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2000
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

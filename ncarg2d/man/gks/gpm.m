.\"
.\"	$Id: gpm.m,v 1.16 2008-12-23 00:03:02 haley Exp $
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

.\"
.\"	$Id: gcell_array.m,v 1.11 2000-08-22 04:16:06 haley Exp $
.\"
.TH GCELL_ARRAY 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gcell_array (Cell array) - draws a cell array starting with a 
rectangle specified by two corner points; this initial 
rectangle is subdivided into subrectangles by 
specifying an arbitrary number of divisions in the X 
and Y directions; each subrectangle (or cell) is 
assigned a color and drawn with that color.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gcell_array(const Grect *rect, const Gpat_rep *colr_array);
.SH DESCRIPTION
.IP rect.p.x 12
(Gfloat Input) - X world coordinate of point 1.
.IP rect.p.y 12
(Gfloat Input) - Y world coordinate of point 1.
.IP rect.q.x 12
(Gfloat Input) - X world coordinate of point 2.
.IP rect.q.y 12
(Gfloat Input) - Y world coordinate of point 2.
.IP colr_array.dims.size_x 12
(Gint, Input) - X dimension of the color index array 
colr_array.colr_array.
.IP colr_array.dims.size_y 12
(Gint, Input) - Y dimension of the color index array 
colr_array.colr_array.
.IP colr_array.colr_array 12
(Gint *, Input) - An array of color indices 
dimensioned colr_array.dims.size_x by colr_array.dims.size_y which is 
used to determine the color of each cell drawn.
.SH USAGE
Points 1 and 2 must be diagonally opposite corner 
points of a rectangle to be divided into colr_array.dims.size_x cells in 
the X dimension and colr_array.dims.size_y cells in the Y dimension.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online:
.BR gset_colr_rep(3NCARG),
.BR gks(3NCARG),
.BR ncarg_gks_cbind(3NCARG)
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

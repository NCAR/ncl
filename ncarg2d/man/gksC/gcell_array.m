.\"
.\"	$Id: gcell_array.m,v 1.17 2008-12-23 00:03:04 haley Exp $
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

.\"
.\"	$Id: gcell_array.m,v 1.1 1993-03-21 01:29:05 haley Exp $
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
.IP rect 12
(Input) - X and Y world coordinates of point 1 and 2.
.IP colr_array.dims.size_x 12
(Input) - X dimension of the color index array 
colr_array.colr_array.
.IP colr_array.dims.size_y 12
(Input) - Y dimension of the color index array 
colr_array.colr_array.
.IP colr_array.colr_array 12
(Input) - An array of color indices 
dimensioned colr_array.dims.size_x x colr_array.dims.size_y which is 
used to determine the color of each 
cell drawn.
.SH USAGE
Points 1 and 2 must be diagonally opposite corner 
points of a rectangle to be divided into colr_array.dims.size_x cells in 
the X dimension and colr_array.dims_size_y cells in the Y dimension.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gksC, ncarg_gks, ncarg_c,
and ncarg_loc libraries.
.SH SEE ALSO
Online:
.BR gset_colr_rep(3NCARG),
.BR gks(3NCARG),
.BR ncarg_gks_cbind(3NCARG)
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

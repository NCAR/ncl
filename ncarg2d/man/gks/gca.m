.\"
.\"	$Id: gca.m,v 1.2 1993-04-02 16:43:55 haley Exp $
.\"
.TH GCA 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GCA (Cell array) - draws a cell array starting with a 
rectangle specified by two corner points; this initial 
rectangle is subdivided into subrectangles by 
specifying an arbitrary number of divisions in the X 
and Y directions; each subrectangle (or cell) is 
assigned a color and drawn with that color.
.SH SYNOPSIS
CALL GCA (XP1, YP1, XP2, YP2, NXCLR, NYCLR, IXCLR, IYCLR, NXDO, NYDO, CLRIA)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gcell_array(const Grect *rect, const Gpat_rep *colr_array);
.SH DESCRIPTION
.IP XP1 12
(Real, Input) - X world coordinate of point 1.
.IP YP1 12
(Real, Input) - Y world coordinate of point 1.
.IP XP2 12
(Real, Input) - X world coordinate of point 2.
.IP YP2 12
(Real, Input) - Y world coordinate of point 2.
.IP NXCLR 12
(Integer, Input) - X dimension of the color index array 
CLRIA.
.IP NYCLR 12
(Integer, Input) - Y dimension of the color index array 
CLRIA.
.IP IXCLR 12
(Integer, Input) - The index of the starting column in 
index array CLRIA.
.IP IYCLR 12
(Integer, Input) - The index of the starting row in 
index array CLRIA.
.IP NXDO 12
(Integer, Input) - Number of cell elements to draw in 
the X dimension.
.IP NYDO 12
(Integer, Input) - Number of cell elements to draw in
the Y dimension.
.IP CLRIA 12
(Integer array, Input) - An array of color indices 
dimensioned NXCLR x NYCLR which is 
used to determine the color of each 
cell drawn.
.SH USAGE
Points 1 and 2 must be diagonally opposite corner 
points of a rectangle to be divided into NXDO cells in 
the X dimension and NYDO cells in the Y dimension.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online:
gscr, gcell_array
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

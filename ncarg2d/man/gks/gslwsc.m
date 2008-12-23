.\"
.\"	$Id: gslwsc.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GSLWSC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSLWSC (Set linewidth scale factor) - sets the linewidth scale 
factor, or relative
thickness of a polyline.
.SH SYNOPSIS
CALL GSLWSC (LWIDTH)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_linewidth(Gdouble linewidth);
.SH DESCRIPTION
.IP LWIDTH 12
(Real, Input) - A scale factor to control the 
linewidth of the polyline to be 
drawn. It must be greater than or 
equal to 0. LWIDTH is applied to the 
nominal linewidth on a given device 
and the linewidths are mapped to the 
nearest available linewidth 
representable on that device. 
.sp
By default LWIDTH = 1.0, so setting LWIDTH = 2.0 
will double line width on most 
devices.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gpl, gsln, gscr, gsplci, gqln, gqlwsc, gqplci, 
dashline, gset_linewidth
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

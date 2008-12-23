.\"
.\"	$Id: gsmksc.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GSMKSC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSMKSC (Set marker size scale factor) - sets the polymarker 
size scale factor -- the relative size of the marker.
.SH SYNOPSIS
CALL GSMKSC (MSZSF)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_marker_size(Gdouble marker_size);
.SH DESCRIPTION
.IP MSZSF 12
(Real, Input) - A scale factor to control the size of the marker.  
It must be  greater than or equal to 0.  
Since MSZSF = 1.0 by default, setting MSZSF = 2.0 doubles polymarker size.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
gpm, gsmk, gscr, gspmci, gqmk, gqmksc, gqpmci, point, points, gset_marker_size
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

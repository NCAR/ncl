.\"
.\"	$Id: gsmksc.m,v 1.10 2000-08-22 04:16:03 haley Exp $
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

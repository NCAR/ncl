.\"
.\"	$Id: gsln.m,v 1.10 2000-08-22 04:16:03 haley Exp $
.\"
.TH GSLN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSLN (Set line type) - sets the line type to solid or various dashed patterns.
.SH SYNOPSIS
CALL GSLN (LTYPE)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_linetype(Gint linetype);
.SH DESCRIPTION
.IP LTYPE 12
(Integer, Input) - The type of polyline to be drawn.  
Options are:
.RS
.IP "< 0" 
Implementation dependent (not 
used in NCAR GKS-0A)
.IP "1" 
Solid line (default)
.IP "2" 
Dashed line
.IP "3" 
Dotted line
.IP "4" 
Dashed dotted line
.IP "\(>= 5" 
Reserved for registration or 
future standardization (not 
used in NCAR GKS-0A)
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gpl, gslwsc, gscr, gsplci, gqln, gqlwsc, gqplci, 
dashline, gset_linetype
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

.\"
.\"	$Id: gsfais.m,v 1.10 2000-08-22 04:16:02 haley Exp $
.\"
.TH GSFAIS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSFAIS (Set fill area interior style) - sets the fill style
of polygons drawn with GFA.
.SH SYNOPSIS
CALL GSFAIS (INTS)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_fill_int_style(Gfill_int_style fill_int_style);
.SH DESCRIPTION
.IP INTS 12
(Integer, Input) - Gives the style of fill to be used 
in subsequent calls to the GFA output primitive. Options are:
.RS
.IP 0 
Hollow fill (the default)
.IP 1 
Solid fill
.IP 2 
Pattern fill (not implemented in NCAR GKS).
.IP 3 
Hatch fill
.SH USAGE
For fill style "Hatch", GSFASI should be used to specify
the type of hatch pattern used (horizontal lines, vertical lines,
etc.)
.sp
Note: If you call GFA and generate an output graphic 
only to find that you have a closed curve with no 
fill, you probably have allowed the fill choice 
to default to hollow fill (no fill).
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gfa, gsfasi, gscr, gsfaci, gqfais, gqfasi, 
areas, gset_fill_int_style
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

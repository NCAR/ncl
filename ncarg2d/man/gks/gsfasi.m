.\"
.\"	$Id: gsfasi.m,v 1.11 2003-05-25 17:16:51 haley Exp $
.\"
.TH GSFASI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSFASI (Set fill are style index) - sets the fill area style index.
.SH SYNOPSIS
CALL GSFASI (STYLI)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_fill_style_ind(Gint fill_style_ind);
.SH DESCRIPTION
.IP STYLI 12
(Integer, Input) - 
Specifies workstation-dependent options for hatch or pattern fills. 
Pattern fill options are not supported in NCAR Graphics. Hatch fill 
options are:
.RS
.IP 1 
Horizontal lines (default)
.IP 2 
Vertical lines
.IP 3 
Lines of positive slope
.IP 4 
Lines of negative slope
.IP 5 
Horizontal and vertical lines
.IP 6 
Lines of positive and negative 
slope
.RS
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gfa, gsfais, gscr, gsfaci, gqfais, gqfasi, 
areas, gset_fill_style_ind
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright (C) 1987-2003
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


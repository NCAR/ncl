.\"
.\"	$Id: gset_linetype.m,v 1.9 2000-07-11 23:03:33 haley Exp $
.\"
.TH GSET_LINETYPE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_linetype (Set line type) - sets the line type to solid or various dashed patterns.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_linetype(Gint linetype);
.SH DESCRIPTION
.IP linetype 12
(Input) - The type of polyline to be drawn.  
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
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gpolyline(3NCARG),
.BR gset_linewidth(3NCARG),
.BR gset_colr_rep(3NCARG),
.BR gset_line_colr_ind(3NCARG),
.BR ginq_linetype(3NCARG),
.BR ginq_linewidth(3NCARG),
.BR ginq_line_colr_ind(3NCARG),
.BR dashline(3NCARG),
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
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.

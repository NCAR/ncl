.\"
.\"	$Id: gset_marker_size.m,v 1.10 2000-08-22 04:16:13 haley Exp $
.\"
.TH GSET_MARKER_SIZE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_marker_size (Set marker size scale factor) - sets the polymarker 
size scale factor -- the relative size of the marker.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_marker_size(Gdouble marker_size);
.SH DESCRIPTION
.IP marker_size 12
(Input) A scale factor to control the size of the marker.  
It must be  greater than or equal to 0.  
Since marker_size = 1.0 by default, setting marker_size = 2.0 doubles polymarker size.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gpolymarker(3NCARG),
.BR gset_marker_type(3NCARG),
.BR gset_colr_rep(3NCARG),
.BR gset_marker_colr_ind(3NCARG),
.BR ginq_marker_type(3NCARG),
.BR ginq_marker_size(3NCARG),
.BR ginq_marker_colr_ind(3NCARG),
.BR point(3NCARG),
.BR points(3NCARG),
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

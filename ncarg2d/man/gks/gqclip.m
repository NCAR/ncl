.\"
.\"	$Id: gqclip.m,v 1.13 2006-01-04 00:12:56 haley Exp $
.\"
.TH GQCLIP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQCLIP (Inquire clipping indicator) - retrieves the current value of the
clipping indicator as well as the current clipping rectangle.
.SH SYNOPSIS
CALL GQCLIP (ERRIND, ICLIP, CLRECT)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_clip(Gint *err_ind, Gclip *clip_ind_rect);
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired values cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP ICLIP 12
(Integer, Output) - Returns the current value of the clipping 
indicator as set by default or by a call to GSCLIP.
.RS
.IP 0 
Clipping is off, or deactivated.
.IP 1 
Clipping is on. Data outside of the world coordinate window will 
not be plotted.
.RE
.IP CLRECT 12
(Real, Array(4)) - Four normalized device coordinates providing the 
corner points of the current clipping rectangle in the order 
XMIN, XMAX, YMIN, YMAX.  
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
set, gsclip, gswn, gselnt, ginq_clip
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright (C) 1987-2006
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

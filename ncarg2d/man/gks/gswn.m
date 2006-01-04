.\"
.\"	$Id: gswn.m,v 1.13 2006-01-04 00:13:00 haley Exp $
.\"
.TH GSWN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSWN (Set window) - establishes a window, or rectangular subspace, 
of world coordinates to be plotted. Calls to GSWN are 
discouraged with NCAR Graphics. Instead, use the SET 
subroutine because SET also supports mirror imaging and log 
scaling of axes. 
.SH SYNOPSIS
CALL GSWN (TRNUM, XMIN, XMAX, YMIN, YMAX)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_win(Gint tran_num, const Glimit *win_limits);
.SH DESCRIPTION
.IP TRNUM 12
(Integer, Input) - A normalization transformation 
number. The number of available transformations is implementation 
specific. In the case of NCAR GKS-0A, two normalization 
transformations are provided:
.RS
.IP 0 
Selects the identity transformation in which both the 
window and viewport have the range of 0. to 1. in both 
coordinate directions. This is the default normalization transformation for 
GKS. It is also fixed within GKS; that is, it is illegal to call 
GSWN with TRNUM = 0.
.IP 1 
A normalization transformation in which the window is defined by XMIN 
to XMAX and YMIN to YMAX.
.RE
.IP XMIN 12
(Real, Input) - The left horizontal coordinate of the window.  XMIN < XMAX
.IP XMAX 12
(Real, Input) - The right horizontal coordinate of the window.
.IP YMIN 12
(Real, Input) - The bottom vertical coordinate of the window. YMIN < YMAX
.IP YMAX 12
(Real, Input) - The top vertical coordinate of the window.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
set, gsvp, gselnt, gqclip, gset_win
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
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


.\"
.\"	$Id: gset_win.m,v 1.12 2003-05-25 17:16:53 haley Exp $
.\"
.TH GSET_WIN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_win (Set window) - establishes a window, or rectangular subspace, 
of world coordinates to be plotted. Calls to gset_win are 
discouraged with NCAR Graphics. Instead, use the c_set 
routine because c_set also supports mirror imaging and log 
scaling of axes. 
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_win(Gint tran_num, const Glimit *win_limits);
.SH DESCRIPTION
.IP tran_num 12
(Input) A normalization transformation number. The number of available 
transformations is implementation specific. In the case of 
NCAR GKS-0A, two normalization transformations are provided:
.RS
.IP 0 
Selects the identity transformation in which both the 
window and viewport have the range of 0. to 1. in both 
coordinate directions. This is the default normalization transformation for 
GKS. It is also fixed within GKS; that is, it is illegal to call 
gset_win with tran_num = 0.
.IP 1 
A normalization transformation in which the window is defined by 
win_limits.x_min to win_limits.x_max and win_limits.y_min to win_limits.y_max.
.RE
.IP win_limits.x_min 12
(Gfloat, Input) - The left horizontal coordinate of the window.  x_min < x_max
.IP win_limits.x_max 12
(Gfloat, Input) - The right horizontal coordinate of the window.
.IP win_limits.y_min 12
(Gfloat, Input) - The bottom vertical coordinate of the window. y_min < y_max
.IP win_limits.y_max 12
(Gfloat, Input) - The top vertical coordinate of the window.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR set(3NCARG),
.BR gset_vp(3NCARG),
.BR gsel_norm_tran(3NCARG),
.BR ginq_clip(3NCARG),
.BR gks(3NCARG)
.BR ncarg_gks_cbind(3NCARG)
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
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


.\"
.\"	$Id: gset_win.m,v 1.1 1993-03-21 01:32:11 haley Exp $
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
win_limits.x_min to win_limits.xmax and win_limits.ymin to win_limits.ymax.
.RE
.IP win_limits 12
(Input) The coordinates of the window.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gksC, ncarg_gks, ncarg_c,
and ncarg_loc libraries.
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
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved


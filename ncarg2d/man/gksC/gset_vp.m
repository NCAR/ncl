.\"
.\"	$Id: gset_vp.m,v 1.1 1993-03-21 01:32:07 haley Exp $
.\"
.TH GSET_VP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_vp (Set viewport) - establishes a rectangular subspace of 
normalized device coordinates space. Calls to gset_vp are 
discouraged with NCAR Graphics. Instead, use the c_set 
routine because c_set also supports mirror imaging and log 
scaling of axes. 
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_vp(Gint tran_num, const Glimit *vp_limits);
.SH DESCRIPTION
.IP tran_num 12
(Input) A normalization transformation number. The number of available 
transformations is implementation specific. In the case of NCAR GKS-0A, 
two normalization transformations are provided:
.RS
.IP 0 
Selects the identity transformation in which both the 
window and viewport have the range of 0. to 1. in both 
coordinate directions.  This is the default normalization transformation for 
GKS. It is also fixed within GKS; that is, it is illegal to call gset_vp 
with tran_num = 0.
.IP 1 
A normalization transformation in which the viewport is defined
by vp_limits.x_min to vp_limits.x_max and vp_limits.y_min to 
vp_limits.y_max.
.RE
.IP vp_limits 12
(Input) The coordinates of the viewport.
0. \(<= vp_limits.x_min < vp_limits.x_max \(<= 1.
0. \(<= vp_limits.y_min < vp_limits.y_max \(<= 1.
.IP Defaults: 12
tran_num = 0, vp_limits.x_min = 0.0, vp_limits.x_max = 1.0,
vp_limits.y_min = 0.0, vp_limits.y_max = 1.0
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gksC and ncarg_gks
libraries.
.SH SEE ALSO
Online: 
.BR c_set(3NCARG),
.BR gset_win(3NCARG),
.BR gsel_norm_tran(3NCARG),
.BR ginq_clip(3NCARG),
.BR gks(3NCARG),
.BR ncarg_gks_cbind(3NCARG)
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

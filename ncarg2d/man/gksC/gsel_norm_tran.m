.\"
.\"	$Id: gsel_norm_tran.m,v 1.17 2008-12-23 00:03:04 haley Exp $
.\"
.TH GSEL_NORM_TRAN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gsel_norm_tran (Select normalization transformation) - selects a 
predefined or user-defined transformation that maps world coordinates 
to normalized device coordinates.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gsel_norm_tran(Gint tran_num);
.SH DESCRIPTION
.IP tran_num 12
(Input) - A normalization transformation number. The number of 
available transformations is implementation specific. In the case of 
NCAR GKS-0A, two normalization transformations are provided:
.sp
.RS
.IP 0 
Selects the identity transformation in which both the 
window and viewport have the range of 0. to 1. in both 
coordinate directions. This is the default.
.sp
.IP 1 
Selects a normalization transformation in which the 
window and viewport have been defined by calls to gset_win and 
gset_vp.
.RE
.SH USAGE
When a normalization transformation is selected, all world coordinate
arguments to GKS functions are transformed by it.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR set(3NCARG),
.BR gset_win(3NCARG),
.BR gset_vp(3NCARG),
.BR ginq_clip(3NCARG),
.BR gks(3NCARG),
.BR ncarg_gks_cbind(3NCARG)
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

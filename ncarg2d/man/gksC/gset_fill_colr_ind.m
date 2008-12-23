.\"
.\"	$Id: gset_fill_colr_ind.m,v 1.16 2008-12-23 00:03:04 haley Exp $
.\"
.TH GSET_FILL_COLR_IND 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_fill_colr_ind (Set fill area color index) - sets the fill area color index.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_fill_colr_ind(Gint fill_colr_ind);
.SH DESCRIPTION
.IP fill_colr_ind 12
(Input) - A color index.
.SH USAGE
All filled areas drawn with calls to the gfill_area output primitive
will be drawn with the color associated with index fill_colr_ind
until gset_fill_colr_ind is called again and a new index is assigned.
.sp
For all GKS output primitives, color is assigned using a color
index. The color indices run from 0 to 255, where 0 is the background
color index and 1 is the foreground color index.  Color values
are associated with indices by calls to the GKS routine gset_colr_rep.
If a color index is used that has no user-assigned color value
set in a gset_colr_rep call, then a device-dependent color value will
be assigned to that index.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gfill_area(3NCARG),
.BR gset_fill_int_style(3NCARG),
.BR gset_fill_style_ind(3NCARG),
.BR gset_colr_rep(3NCARG),
.BR ginq_fill_int_style(3NCARG),
.BR ginq_fill_style_ind(3NCARG),
.BR areas(3NCARG),
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

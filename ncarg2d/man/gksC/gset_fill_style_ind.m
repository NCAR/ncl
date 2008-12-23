.\"
.\"	$Id: gset_fill_style_ind.m,v 1.16 2008-12-23 00:03:04 haley Exp $
.\"
.TH GSET_FILL_STYLE_IND 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_fill_style_ind (Set fill are style index) - sets the fill area style index.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_fill_style_ind(Gint fill_style_ind);
.SH DESCRIPTION
.IP fill_style_ind 12
(Input) - 
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
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gfill_area(3NCARG),
.BR gset_fill_int_style(3NCARG),
.BR gset_colr_rep(3NCARG),
.BR gset_fill_colr_ind(3NCARG),
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

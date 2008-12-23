.\"
.\"	$Id: gset_fill_int_style.m,v 1.16 2008-12-23 00:03:04 haley Exp $
.\"
.TH GSET_FILL_INT_STYLE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_fill_int_style (Set fill area interior style) - sets the fill style
of polygons drawn with gfill_area.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_fill_int_style(Gfill_int_style fill_int_style);
.SH DESCRIPTION
.IP fill_int_style 12
(Input) - Gives the style of fill to be used 
in subsequent calls to the gfill_area output primitive. Options are:
.RS
.IP GSTYLE_HOLLOW 
Hollow fill (the default)
.IP GSTYLE_SOLID
Solid fill
.IP GSTYLE_PAT
Pattern fill (not implemented in NCAR GKS).
.IP GSTYLE_HATCH
Hatch fill
.SH USAGE
For fill style "Hatch", gset_fill_style_ind should be used to specify
the type of hatch pattern used (horizontal lines, vertical lines,
etc.)
.sp
Note: If you call gfill_area and generate an output graphic 
only to find that you have a closed curve with no 
fill, you probably have allowed the fill choice 
to default to hollow fill (no fill).
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gfill_area(3NCARG),
.BR gset_fill_style_ind(3NCARG),
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

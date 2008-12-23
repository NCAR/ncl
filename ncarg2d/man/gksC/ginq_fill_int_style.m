.\"
.\"	$Id: ginq_fill_int_style.m,v 1.17 2008-12-23 00:03:04 haley Exp $
.\"
.TH GINQ_FILL_INT_STYLE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ginq_fill_int_style (Inquire fill area interior style) - retrieves the current 
value for fill area interior style.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_fill_int_style(Gint *err_ind, Gfill_int_style *fill_int_style);
.SH DESCRIPTION
.IP err_ind 12
(Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in err_ind, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP fill_int_style 12
(Output) - Returns the current value for fill style as set by
default or by a call to gset_fill_int_style.  Possible return values
include:
.RS
.IP GSTYLE_HOLLOW 
Hollow fill (the default)
.IP GSTYLE_SOLID
Solid fill
.IP GSTYLE_PAT
Pattern fill (not implemented in NCAR GKS).
.IP GSTYLE_HATCH
Hatch fill
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gfill_area(3NCARG),
.BR gset_fill_int_style(3NCARG),
.BR gset_fill_style_ind(3NCARG),
.BR gset_colr_rep(3NCARG),
.BR gset_fill_colr_ind(3NCARG),
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

.\"
.\"	$Id: ginq_fill_style_ind.m,v 1.16 2008-12-23 00:03:04 haley Exp $
.\"
.TH GINQ_FILL_STYLE_IND 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ginq_fill_style_ind (Inquire fill area style index) - retrieves the current value of
the fill area style index.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_fill_style_ind(Gint *err_ind, Gint *fill_style_ind);
.SH DESCRIPTION
.IP err_ind 12
(Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in err_ind, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP fill_style_ind 12
(Output) - Returns the fill area style index as set by default
or by a call to gset_fill_style_ind.
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
.RE
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

.\"
.\"	$Id: ginq_asfs.m,v 1.17 2008-12-23 00:03:04 haley Exp $
.\"
.TH GINQ_ASFS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ginq_asfs - (Inquire aspect source flags) - Inquires the values for the 
aspect source flags that determine whether primitive attributes are 
to be selected from individual settings or from "bundle tables".
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_asfs(Gint *err_ind, Gasfs *list_asf);
.SH DESCRIPTION 
.IP err_ind 12
(Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in err_ind, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP list_asf 12
(Output) - A structure of settings for the thirteen attributes:
.RS
.IP list_asfs.linetype
Linetype
.IP list_asfs.linewidth
Linewidth scale factor
.IP list_asfs.line_colr_ind
Polyline color index
.IP list_asfs.marker_type
Marker type
.IP list_asfs.marker_size
Marker size scale factor
.IP list_asfs.marker_colr_ind
Polymarker color index
.IP list_asfs.text_font_prec
Text font and precision
.IP list_asfs.char_expan
Character expansion factor
.IP list_asfs.char_space
Character spacing
.IP list_asfs.text_colr_ind
Text color index
.IP list_asfs.fill_int_style
Fill area interior style
.IP list_asfs.fill_style_ind
Fill area style index
.IP list_asfs.fill_colr_ind
Fill area color index
.RE
.sp
Each value of list_asf is either GASF_BUNDLED(for "bundled") or GASF_INDIV 
(for "individual").
.SH USAGE
In NCAR Graphics all attribute aspect source flags are defaulted to
"individual" (all values for list_asf are GASF_INDIV).
It is advised that for dependable results when using NCAR Graphics
these values remain in their default settings.  Some GKS packages
default these values to "bundled".  ginq_asfs can be called to determine
the values of all aspect source flags.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online:
.BR gset_asfs(3NCARG),
.BR gset_linetype(3NCARG),
.BR gset_linewidth(3NCARG),
.BR gset_line_colr_ind(3NCARG),
.BR gset_marker_type(3NCARG),
.BR gset_marker_size(3NCARG),
.BR gset_marker_colr_ind(3NCARG),
.BR gset_text_font_prec(3NCARG),
.BR gset_char_expan(3NCARG),
.BR gset_text_font_prec(3NCARG),
.BR gset_text_colr_ind(3NCARG),
.BR gset_fill_int_style(3NCARG),
.BR gset_fill_style_ind(3NCARG),
.BR gset_fill_colr_ind(3NCARG),
.BR ginq_linetype(3NCARG),
.BR ginq_linewidth(3NCARG),
.BR ginq_line_colr_ind(3NCARG),
.BR ginq_marker_type(3NCARG),
.BR ginq_marker_size(3NCARG),
.BR ginq_marker_colr_ind(3NCARG),
.BR ginq_text_font_prec(3NCARG),
.BR ginq_char_expan(3NCARG),
.BR ginq_text_font_prec(3NCARG),
.BR ginq_text_colr_ind(3NCARG),
.BR ginq_fill_int_style(3NCARG),
.BR ginq_fill_style_ind(3NCARG),
.BR ginq_fill_colr_ind(3NCARG),
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

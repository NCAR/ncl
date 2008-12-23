.\"
.\"	$Id: gset_asfs.m,v 1.17 2008-12-23 00:03:04 haley Exp $
.\"
.TH GSET_ASFS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_asfs - (Set aspect source flags) - Sets the aspect source flags that
determine whether primitive attributes are to be selected from individual
settings or from "bundle tables".
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_asfs(const Gasfs *list_asf);
.SH DESCRIPTION 
.IP list_asf 12
(input) - A structure of settings for the thirteen attributes:
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
Each value of list_asf is of either GASF_BUNDLED (for "bundled") or GASF_INDIV 
(for "individual").
.SH USAGE
In NCAR Graphics all attribute aspect source flags are defaulted to 
GASF_INDIV (all values for list_asf are "1").  
It is advised that for dependable results when using NCAR Graphics 
these values remain in their default settings.  Some GKS packages
default these values to GASF_BUNDLED, so, if one expects to use such
a package, it would be precautionary to call gset_asfs to set all aspect
source flags to GASF_INDIV before calling other GKS functions.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online:
.BR ginq_asfs(3NCARG),
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

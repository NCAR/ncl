.\"
.\"	$Id: gset_marker_type.m,v 1.16 2008-12-23 00:03:04 haley Exp $
.\"
.TH GSET_MARKER_TYPE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_marker_type (Set marker type) - sets the type of polymarker to be used in
subsequent GPM calls.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_marker_type(Gint marker_type);
.SH DESCRIPTION
.IP marker_type 12
(Input) - Selects the type of marker to be drawn. 
Options are:
.RS
.IP "< 0"  
implementation dependent (not used in NCAR GKS-0A)
.IP "  1" 
 . (dot)
.IP "  2" 
 + (plus)
.IP "  3" 
 * (asterisk) This is the default
.IP "  4" 
 o (circle)
.IP "  5" 
 X (cross)
.IP "\(>= 6"
 reserved for registration or future standardization
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online:
.BR gpolymarker(3NCARG),
.BR gset_marker_size(3NCARG),
.BR gset_colr_rep(3NCARG),
.BR gset_marker_colr_ind(3NCARG),
.BR ginq_marker_type(3NCARG),
.BR ginq_marker_size(3NCARG),
.BR ginq_marker_colr_ind(3NCARG),
.BR point(3NCARG),
.BR points(3NCARG),
.BR ngdots(3NCARG),
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

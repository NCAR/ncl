.\"
.\"	$Id: gset_linetype.m,v 1.4 1994-08-11 17:53:24 haley Exp $
.\"
.TH GSET_LINETYPE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_linetype (Set line type) - sets the line type to solid or various dashed patterns.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_linetype(Gint linetype);
.SH DESCRIPTION
.IP linetype 12
(Input) - The type of polyline to be drawn.  
Options are:
.RS
.IP "< 0" 
Implementation dependent (not 
used in NCAR GKS-0A)
.IP "1" 
Solid line (default)
.IP "2" 
Dashed line
.IP "3" 
Dotted line
.IP "4" 
Dashed dotted line
.IP "\(>= 5" 
Reserved for registration or 
future standardization (not 
used in NCAR GKS-0A)
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gksC, ncarg_gks, and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gpolyline(3NCARG),
.BR gset_linewidth(3NCARG),
.BR gset_colr_rep(3NCARG),
.BR gset_line_colr_ind(3NCARG),
.BR ginq_linetype(3NCARG),
.BR ginq_linewidth(3NCARG),
.BR ginq_line_colr_ind(3NCARG),
.BR dashline(3NCARG),
.BR gks(3NCARG),
.BR ncarg_gks_cbind(3NCARG)
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

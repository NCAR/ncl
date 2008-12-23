.\"
.\"	$Id: ginq_marker_colr_ind.m,v 1.16 2008-12-23 00:03:04 haley Exp $
.\"
.TH GINQ_MARKER_COLR_IND 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ginq_marker_colr_ind (Inquire polymarker color index) - retrieves the polymarker color index.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_marker_colr_ind(Gint *err_ind, Gint *marker_colr_ind);
.SH DESCRIPTION
.IP err_ind 12
(Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in err_ind, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP marker_colr_ind 12
(Output) - Returns the polymarker color index as set by default
or by a call to gset_marker_colr_ind.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gpolymarker(3NCARG),
.BR gset_marker_type(3NCARG),
.BR gset_marker_size(3NCARG),
.BR gset_colr_rep(3NCARG),
.BR gset_marker_colr_ind(3NCARG),
.BR ginq_marker_type(3NCARG),
.BR ginq_marker_size(3NCARG),
.BR ginq_marker_colr_ind(3NCARG),
.BR point(3NCARG),
.BR points(3NCARG),
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

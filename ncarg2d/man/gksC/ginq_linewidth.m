.\"
.\"	$Id: ginq_linewidth.m,v 1.1 1993-03-21 01:30:14 haley Exp $
.\"
.TH GINQ_LINEWIDTH 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ginq_linewidth (Inquire linewidth scale factor) - retrieves the current setting of the
linewidth scale factor.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_linewidth(Gint *err_ind, Gdouble *linewidth);
.SH DESCRIPTION
.IP err_ind 12
(Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in err_ind, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP linewidth 12
(Output) - Returns the current value of the linewidth scale factor
as set by default or by a call to gset_linewidth.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gksC, ncarg_gks, ncarg_c,
and ncarg_loc libraries.
.SH SEE ALSO
Online: 
.BR gpolyline(3NCARG),
.BR gset_linetype(3NCARG),
.BR gset_linewidth(3NCARG),
.BR gset_colr_rep(3NCARG),
.BR gset_line_colr_ind(3NCARG),
.BR ginq_linetype(3NCARG),
.BR ginq_line_colr_ind(3NCARG),
.BR dashline(3NCARG),
.BR gks(3NCARG),
.BR ncarg_gks_cbind(3NCARG)
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

.\"
.\"	$Id: ginq_colr_rep.m,v 1.1 1993-03-21 01:29:55 haley Exp $
.\"
.TH GINQ_COLR_REP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ginq_colr_rep (Inquire color representation) - retrieves the color value associated
with a color index on a particular workstation.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_colr_rep(Gint ws_id, Gint colr_ind, Ginq_type type, Gint *err_ind, Gcolr_rep  *colr_rep);
.SH DESCRIPTION
.IP ws_id 12
(Input) - Gives a workstation identifier as was set in
a previous call to gopen_ws.
.IP colr_ind 12
(Input) - A color index.
.IP type 12
(Input) - Specifies whether the returned color values are the
ones that were specified by a call to gset_colr_rep or whether they are the
color values that are actually being used on the specified workstation
(frequently a workstation will not be able to represent a requested 
color value precisely).
.RS
.IP 0 
Returns the color value set for index CI either by default or by a call to gset_colr_rep.
.IP 1 
Returns the color value actually used by the workstation to represent the 
requested color.
.RE
.IP err_ind 12
(Output) - If the inquired values cannot be returned correctly,
a non-zero error indicator is returned in err_ind, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP colr_rep->rgb.red 12
(Output) - Returns an intensity value for red in the 
range 0. to 1. inclusive.
.IP colr_rep->rgb.green 12
(Output) - Returns an intensity value for green in the 
range 0. to 1. inclusive.
.IP colr_rep->rgb.blue 12
(Output) - Returns an intensity value for blue in the 
range 0. to 1. inclusive.
.SH USAGE
For details on setting and using color indices see the man page for gset_colr_rep.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gksC, ncarg_gks, ncarg_c,
and ncarg_loc libraries.
.SH SEE ALSO
Online: 
.BR gset_colr_rep(3NCARG),
.BR gset_line_colr_ind(3NCARG),
.BR gset_marker_size(3NCARG),
.BR gset_text_colr_ind(3NCARG),
.BR gset_fill_colr_ind(3NCARG),
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

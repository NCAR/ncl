.\"
.\"	$Id: gset_marker_size.m,v 1.1 1993-03-21 01:31:49 haley Exp $
.\"
.TH GSET_MARKER_SIZE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_marker_size (Set marker size scale factor) - sets the polymarker 
size scale factor -- the relative size of the marker.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_marker_size(Gdouble marker_size);
.SH DESCRIPTION
.IP marker_size 12
(Input) A scale factor to control the size of the marker.  
It must be  greater than or equal to 0.  
Since marker_size = 1.0 by default, setting marker_size = 2.0 doubles polymarker size.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gksC, ncarg_gks, ncarg_c,
and ncarg_loc libraries.
.SH SEE ALSO
Online: 
.BR gpolymarker(3NCARG),
.BR gset_marker_type(3NCARG),
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
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

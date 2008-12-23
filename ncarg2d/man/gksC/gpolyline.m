.\"
.\"	$Id: gpolyline.m,v 1.17 2008-12-23 00:03:04 haley Exp $
.\"
.TH GPOLYLINE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gpolyline (Polyline) - This output primitive draws
line segments connecting a sequence of user-specified coordinate pairs.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gpolyline(const Gpoint_list *point_list);
.SH DESCRIPTION
.IP point_list.num_points 12
(Gint, Input) - The number of points in the line to be drawn. Must be larger than one.
.IP point_list.points 12
(Gint *, Input) - The X and Y coordinates (specified in world coordinates) of 
the point_list.num_points points to be connected by line segments.  
.SH USAGE
Note that the coordinate pairs must be in world coordinates and not
user coordinates.  Among other things, this means that the log scaling
and mirror-imaging features available via the c_set call and the SPPS
functions for drawing lines are not applicable here.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gset_linetype(3NCARG),
.BR gset_linewidth(3NCARG),
.BR gset_colr_rep(3NCARG),
.BR gset_line_colr_ind(3NCARG),
.BR ginq_linetype(3NCARG),
.BR ginq_linewidth(3NCARG),
.BR ginq_line_colr_ind(3NCARG),
.BR dashline(3NCARG),
.BR set(3NCARG),
.BR gks(3NCARG),
.BR ncarg_gks_cbind(3NCARG)
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
"The Use of X/Y Coordinates in NCAR Graphics" SCD User Document"
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

.\"
.\"	$Id: gset_linewidth.m,v 1.4 1994-08-11 17:53:25 haley Exp $
.\"
.TH GSET_LINEWIDTH 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_linewidth (Set linewidth scale factor) - sets the linewidth scale 
factor, or relative
thickness of a polyline.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_linewidth(Gdouble linewidth);
.SH DESCRIPTION
.IP linewidth 12
(Input) - A scale factor to control the 
linewidth of the polyline to be 
drawn. It must be greater than or 
equal to 0. linewidth is applied to the 
nominal linewidth on a given device 
and the linewidths are mapped to the 
nearest available linewidth 
representable on that device. 
.sp
By default linewidth = 1.0, so setting linewidth = 2.0 
will double line width on most 
devices.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gksC, ncarg_gks, and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gpolyline(3NCARG),
.BR gset_linetype(3NCARG),
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

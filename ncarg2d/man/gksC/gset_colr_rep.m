.\"
.\"	$Id: gset_colr_rep.m,v 1.17 2008-12-23 00:03:04 haley Exp $
.\"
.TH GSET_COLR_REP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_colr_rep (Set color representation) - associates a color value with a
color index.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_colr_rep(Gint ws_id, Gint colr_ind, const Gcolr_rep *colr_rep);
.SH DESCRIPTION
.IP ws_id 12
(Input) - A workstation identifier.
ws_id must be the same as that used in some previous gopen_ws call.
.IP colr_ind 12
(Input) - A color index.
.IP colr_rep.rgb.red 12
(Gfloat, Input) - An intensity value for red between 0. and 1. inclusive.
.IP colr_rep.rgb.green 12
(Gfloat, Input) - An intensity value for green between 0. and 1. inclusive.
.IP colr_rep.rgb.blue 12
(Gfloat, Input) - An intensity value for blue between 0. and 1. inclusive.
.SH USAGE
Color in GKS is "indexed", i.e. color attributes are
assigned to primitives by using a color index.  The GKS
function gset_colr_rep is used to associate a color value with
color indices. 
.sp
It is recommended that all color indices used in 
a given job be defined prior to calling any output primitive.
Since the result of a dynamic color change can be ambiguous, it is also
recommended that gset_colr_rep not be called to change a color value
after the original definitions.
.sp
gset_colr_rep may be called after opening the workstation for which
the color indices are to be defined, and not before that
workstation is opened.  Different color tables can be maintained
for different workstations.  For example, color index 2 for a
CGM file may be associated with "red" while at the same time it
may be associated with "green" for some X workstation.
.sp
It is important to stress that color index 0 defines the background
color. If any color indices are defined, then you 
should define the background color index 0. Otherwise 
you run the risk of having a user-defined color match 
the default background color.
.sp
For all GKS output primitives, color is assigned by an 
indexing scheme. The indices run from 0 to 
255, where 0 is the background color index and 1 is 
the foreground color index.  
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR ginq_colr_rep(3NCARG),
.BR gset_line_colr_ind(3NCARG),
.BR gset_marker_colr_ind(3NCARG),
.BR gset_fill_colr_ind(3NCARG),
.BR gset_text_colr_ind(3NCARG),
.BR ginq_line_colr_ind(3NCARG),
.BR ginq_marker_colr_ind(3NCARG),
.BR ginq_fill_colr_ind(3NCARG),
.BR ginq_text_colr_ind(3NCARG),
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

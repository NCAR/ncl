.\"
.\"	$Id: gset_clip_ind.m,v 1.16 2008-12-23 00:03:04 haley Exp $
.\"
.TH GSET_CLIP_IND 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_clip_ind (Set clipping indicator) - controls whether data are
displayed outside the boundaries of the world coordinate window
of the current normalization transformation.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_clip_ind(Gclip_ind clip_ind);
.SH DESCRIPTION
.IP clip_ind 12
(Input) - A flag to turn clipping on or off.
.RS
.IP GIND_NO_CLIP
Clipping is off. Data outside of the window will be plotted.
.IP GIND_CLIP
Clipping is on. Data outside of the window will not be  plotted.
This is the default.
.RE
.SH USAGE
If the clipping indicator is off, 
and you make GKS output calls to plot world coordinate 
data outside your defined world coordinate window (and 
your viewport is smaller than the full plotting 
surface), those data will appear with your plot. If 
the clipping indicator is on, the data will be clipped 
to fit your window.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR set(3NCARG),
.BR gsup(3NCARG),
.BR gset_win(3NCARG),
.BR gsel_norm_tran(3NCARG),
.BR ginq_clip(3NCARG),
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

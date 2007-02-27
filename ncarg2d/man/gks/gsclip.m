.\"
.\"	$Id: gsclip.m,v 1.14 2007-02-27 18:20:20 haley Exp $
.\"
.TH GSCLIP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSCLIP (Set clipping indicator) - controls whether data are
displayed outside the boundaries of the world coordinate window
of the current normalization transformation.
.SH SYNOPSIS
CALL GSCLIP (ICLIP)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_clip_ind(Gclip_ind clip_ind);
.SH DESCRIPTION
.IP ICLIP 12
(Integer, Input) - A flag to turn clipping on or off.
.RS
.IP 0 
Clipping is off. Data outside of the window will be plotted.
.IP 1 
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
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
set, gsup, gswn, gselnt, gqclip, gset_clip_ind
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright (C) 1987-2007
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.

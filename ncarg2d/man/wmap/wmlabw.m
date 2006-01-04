.\"
.\"	$Id: wmlabw.m,v 1.10 2006-01-04 00:13:47 haley Exp $
.\"
.TH WMLABW 3NCARG "January 1995" UNIX "NCAR GRAPHICS"
.SH NAME
WMLABW - plots weather map regional weather labels (like "HOT", "BREEZY", "COLD", etc.
.SH SYNOPSIS
CALL WMLABW (X, Y, LABEL)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void c_wmlabw(float x, float y, char *label)
.SH DESCRIPTION
.IP X 12
(Real, Input) - An X coordinate (specified in world coordinates) of a
point that specifies the center of the label in LABEL.
.IP Y 12
(Real, Input) - A Y coordinate (specified in world coordinates) of a
point that specifies the center of the label in LABEL.
.IP LABEL 12
(Character, Input) - A character variable that contains the desired label.
.SH USAGE
Set the values for the appropriate internal parameters before calling
WMLABW to produce the desired label.  The internal parameters that control
the appearance of the labels are: RC1, RC2, RC3, RC4, RC5, and WHT.  
.SH ACCESS
To use WMLABW or c_wmlabw, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online: 
wmap, wmdflt, wmgetc, wmgeti, wmgetr, wmlabs, wmsetc, wmseti, wmsetr, wmap_params
.sp
Hardcopy: 
WMAP - A Package for Producing Daily Weather Maps and Plotting Station 
Model Data
.SH COPYRIGHT
Copyright (C) 1987-2006
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

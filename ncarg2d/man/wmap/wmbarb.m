.\"
.\"	$Id: wmbarb.m,v 1.11 2007-02-27 18:20:38 haley Exp $
.\"
.TH WMBARB 3NCARG "January 1995" UNIX "NCAR GRAPHICS"
.SH NAME
WMBARB - plots wind barbs.
.SH SYNOPSIS
CALL WMBARB (X,Y,U,V)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void c_wmbarb(float x, float y, float u, float v)
.SH DESCRIPTION
.IP X 12
(Real, Input) - An X coordinate (specified in world coordinates) of a
point that specifies the position of the base of a wind barb.
.IP Y 12
(Real, Input) - A Y coordinate (specified in world coordinates) of a
point that specifies the position of the base of a wind barb.
.IP U 12
(Real, Input) - The X component of a wind vector.
.IP V 12
(Real, Input) - The Y component of a wind vector.
.SH USAGE
The angle at which the wind barb is drawn is determined by the vector 
(U,V) and the magnitude of that vector denotes the wind speed in knots.
Set the values for the appropriate internal parameters before calling
WMBARB to produce the wind barb.  The internal parameters that control
the appearance of wind barbs are: COL, WBA, WBD, WBF, WBS, WBT.
.SH ACCESS
To use WMBARB or c_wmbarb, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online: 
wmap, wmdflt, wmgetc, wmgeti, wmgetr, wmlabs, wmsetc, wmseti, wmsetr, wmstnm, wmap_params
.sp
Hardcopy: 
WMAP - A Package for Producing Daily Weather Maps and Plotting Station 
Model Data
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

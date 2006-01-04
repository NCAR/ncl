.\"
.\"	$Id: wmlgnd.m,v 1.10 2006-01-04 00:13:47 haley Exp $
.\"
.TH WMLGND 3NCARG "January 1995" UNIX "NCAR GRAPHICS"
.SH NAME
WMLGND - plots weather map legends.
.SH SYNOPSIS
CALL WMLGND (X,Y,NTYPE,IROWS,ICOLS)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void c_wmlgnd(float x, float y, int ntype, int irows, int icols)
.SH DESCRIPTION
.IP X 12
(Real, Input) - An X coordinate (specified in world coordinates) of a
point that specifies a position for the legend to be plotted.
.IP Y 12
(Real, Input) - A Y coordinate (specified in world coordinates) of a
point that specifies a position for the legend to be plotted.
.IP NTYPE 12
(Integer, Input) - A character variable that indicates the desired kind of
legend.
.RS
.IP "Legal values for NTYPE are:"
.sp
1 - Plot the legend for the weather types. There are 
six weather types: showers; T-storms; rain; flurries; 
snow; ice. The coordinate (X,Y) for this 
type specifies the lower left corner of the legend.
.sp
2 - Plot the legend for front types. There are three 
front types labeled: cold; warm; stationary. The 
(X,Y) coordinate for this legend specifies the 
lower right corner of the legend.
.sp
3 - Plot the explanatory legend. The (X,Y) coordinate for 
this legend specifies the bottom center of the legend.
.RE
.IP IROWS 12
(Integer, Input) - An integer variable specifying how many rows there will
be in displaying the weather types.  IROWS and ICOLS are significant only when 
NTYPE=1. Choices for IROWS x ICOLS are: 1x6, 2x3, 3x2, and 6x1.
.IP ICOLS 12
(Integer, Input) - An integer variable specifying how many columns there will
be in displaying the weather types.  IROWS and ICOLS are significant only when 
NTYPE=1. Choices for IROWS x ICOLS are: 1x6, 2x3, 3x2, and 6x1.
.SH USAGE
The only internal parameter that controls the appearance of a legend is
COL, specifying its color.
.SH ACCESS
To use WMLGND or c_wmlgnd, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online: 
wmap, wmdflt, wmgetc, wmgeti, wmgetr, wmsetc, wmseti, wmsetr, wmap_params
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

.\"
.\"	$Id: wmlabs.m,v 1.9 2005-01-04 15:42:34 haley Exp $
.\"
.TH WMLABS 3NCARG "January 1995" UNIX "NCAR GRAPHICS"
.SH NAME
WMLABS - plots weather map special symbols and daily weather icons.
.SH SYNOPSIS
CALL WMLABS (X, Y, SYMTYP)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void c_wmlabs(float x, float y, char *symtyp)
.SH DESCRIPTION
.IP X 12
(Real, Input) - An X coordinate (specified in world coordinates) of a
point that specifies a position for the symbol type in SYMTYP.
.IP Y 12
(Real, Input) - A Y coordinate (specified in world coordinates) of a
point that specifies a position for the symbol type in SYMTYP.
.IP SYMTYP 12
(Character, Input) - A character variable that indicates the desired symbol.
.RS
.IP "Legal values for SYMTYP are:"
.sp
 'HI' for high pressure symbols.
.sp
 'LOW' for low pressure symbols.
.sp
 'ARROW' for arrows.
.sp
 'DOT' for dots.
.sp
 'CLOUD' for a cloud daily weather icon.
.sp
 'ICE' for an ice daily weather icon.
.sp
 'IS' for the intermittent shower daily weather icon.
.sp
 'IT' for the sunny, chance of T-storms daily weather icon.
.sp
 'MC' for the mostly cloudy daily weather icon.
.sp
 'RAIN' for the daily weather icon indicating rain.
.sp
 'RS' for the daily weather icon indicating rain and snow.
.sp
 'SNOWFLAKES' for the daily weather icon indicating snow.
.sp
 'SUN' for the sunny daily weather icon.
.sp
 'THUNDERSTROM' for the daily weather icon indicating thunderstorms.
.sp
 'WIND' for the windy daily weather icon.
.RE
.SH USAGE
Set the values for the appropriate internal parameters before calling
WMLABS to produce the desired symbol.  The internal parameters that control
the appearance of the symbols and icons are: SHT, HIB, HIF, HIS, HIC (for the
high symbols); SHT, LOF, LOB, LOS (for the low symbols); ARS, AWC, ARD, ASC,
AOC (for arrows); DBC, DTC, DTS (for dots); CC1, CC2, CC3 (for the cloud icon);
LC1, LC2, LC3 (for lightening bolts); SC1, SC2, SC3, SC4 (for the sun icon);
COL.
.SH ACCESS
To use WMLABS or c_wmlabs, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online: 
wmap, wmdflt, wmgetc, wmgeti, wmgetr, wmsetc, wmseti, wmsetr, wmap_params
.sp
Hardcopy: 
WMAP - A Package for Producing Daily Weather Maps and Plotting Station 
Model Data
.SH COPYRIGHT
Copyright (C) 1987-2005
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

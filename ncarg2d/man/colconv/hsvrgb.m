.TH HSVRGB 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
HSVRGB - Converts a color specification given in the
Hue, Saturation, and Value (HSV) color space to color values
in the Red, Green, Blue (RGB) color space.
.SH SYNOPSIS
CALL HSVRGB (H, S, V, R, G, B)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_hsvrgb (float h, float s, float v, float *r, 
.br
float *g, float *b)
.SH DESCRIPTION 
.IP H 12
(REAL, input, range [0.,360.) ) 
represents
the hue of the input color in HSV color space.
.IP S 12
(REAL, input, range [0.,1.]) 
represents
the saturation value of the input color in HSV color
space.  Saturation is a measure of how much white light
is mixed with the color. Saturation values of 0.
represent grays (with a gray value equal to the value
V). Saturation values of 1. are fully saturated colors.
The hue is undefined when S=0. The fully saturated pure
hues occur when S=1. and V=1.
.IP V 12
(REAL, input, range [0.,1.]) 
represents
the value of the input color in HSV color space.
.IP R 12
(REAL, output, range [0.,1.]) 
represents
the red intensity component of the output color in RGB
color space.
.IP G 12
(REAL, output, range [0.,1.]) 
represents
the green intensity component of the output color in
RGB color space.
.IP B 12
(REAL, output, range [0.,1.]) 
represents
the blue intensity component of the output color in RGB
color space.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
coex03,
tcolcv,
fcce02.
.SH ACCESS
To use HSVRGB or c_hsvrgb, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the colconv man page for a description of all Colconv error
messages and/or informational messages.
.SH SEE ALSO
Online:
colconv,
hlsrgb,
rgbhls,
rgbhsv,
rgbyiq,
yiqrgb,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2000
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

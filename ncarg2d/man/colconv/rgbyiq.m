.TH RGBYIQ 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
RGBYIQ - Converts a color specification given in the RGB
Red, Green, Blue, (RGB) color space to a color specification in the 
YIQ color space.
.SH SYNOPSIS
CALL RGBYIQ (R, G, B, Y, I, Q)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_rgbyiq (float r, float g, float b, float *y, 
.br
float *i, float *q)
.SH DESCRIPTION 
.IP R 12
(REAL, input, range [0.,1.]) 
represents
the red intensity component of the input point in RGB
color space.
.IP G 12
(REAL, input, range [0.,1.]) 
represents
the green intensity component of the input point in RGB
color space.
.IP B 12
(REAL, input, range [0.,1.]) 
represents
the blue intensity component of the input point in RGB
color space.
.IP Y 12
(REAL, output, range [0.,1.]) 
This is the color
component of a television signal that is shown on
black-and-white televisions; Y minimizes the effect of
two colors appearing different to the human eye but
mapping to similar monochrome intensities.
.IP I 12
(REAL, output, range [-.6,.6]) 
attains its
maximum when the input triple is (1.,0.,0.); I attains
its minimum when the input triple is (0.,1.,1.).
.IP Q 12
(REAL, output, range [-.52,.52]) 
attains its
maximum when the input triple is (1.,0.,1.); Q attains
its minimum when the input triple is (0.,1.,0.).
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example:  
tcolcv,
fcce02.
.SH ACCESS
To use RGBYIQ or c_rgbyiq, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
colconv,
hlsrgb,
hsvrgb,
rgbhls,
rgbhsv,
yiqrgb,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2003
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

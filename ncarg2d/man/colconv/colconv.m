.TH Colconv 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Colconv - 
Allows a user to convert among the color models RGB, HSV,
HLS, and YIQ.
.SH SYNOPSIS
HLSRGB - Converts a color specification given as Hue,
Lightness, and Saturation (HLS) values to Red, Green, and Blue
(RGB) intensity values in the RGB color space.
.sp
HSVRGB - Converts a color specification given in the
Hue, Saturation, and Value (HSV) color space to color values
in the Red, Green, Blue (RGB) color space.
.sp
RGBHLS - Converts a color specification given in the
Red, Green, Blue (RGB) color space to color values in the
Hue, Lightness, Saturation (HLS) color
space.
.sp
RGBHSV - Converts a color specification given in the
Red, Green, Blue (RGB) color space to color values in the
Hue, Saturation, and Value (HSV) color space.
.sp
RGBYIQ - Converts a color specification given in the RGB
Red, Green, Blue, (RGB) color space to a color specification in the
YIQ color space.
.sp
YIQRGB - Converts a color specification given in the YIQ
color space to the equivalent color specification in the
Red, Green, Blue (RGB) color space.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_hlsrgb
.br
c_hsvrgb
.br
c_rgbhls
.br
c_rgbhsv
.br
c_rgbyiq
.br
c_yiqrgb
.SH ACCESS 
To use Colconv or c_colconv routines, load the NCAR Graphics libraries
ncarg, ncarg_gks, and ncarg_c, preferably in that order.

.SH MESSAGES
When error conditions are detected, the support routine SETER
is called in such a way that it writes a message to the standard
error file (as defined by I1MACH(4)) and then terminates
execution. The possible error messages are as follows:
.IP "HLSRGB - L out of range"
Lightness is less than 0. or greater than 100.
.IP "HLSRGB - S out of range"
Saturation is less than 0. or greater than 100.
.IP "HSVRGB - S out of range"
Saturation is less than 0. or greater than 1.
.IP "HSVRGB - V out of range"
Value of input color is less than 0. or greater than 1.
.IP "RGBHLS - R out of range"
Value of red intensity component is less than 0. or greater than 1.
.IP "RGBHLS - G out of range"
Value of green intensity component is less than 0. or greater than 1.
.IP "RGBHLS - B out of range"
Value of blue intensity component is less than 0. or greater than 1.
.IP "RGBHSV - R out of range"
Value of red intensity component is less than 0. or greater than 1.
.IP "RGBHSV - G out of range"
Value of green intensity component is less than 0. or greater than 1.
.IP "RGBHSV - B out of range"
Value of blue intensity component is less than 0. or greater than 1.
.SH SEE ALSO
Online:
hlsrgb,
hsvrgb,
rgbhls,
rgbhsv,
rgbyiq,
yiqrgb,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

.\"
.\"	$Id: rgbhls.m,v 1.1 1993-03-11 16:15:30 haley Exp $
.\"
.TH RGBHLS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
RGBHLS - RGBHLS converts a color specification given in the
RGB color space to color values in the HLS color
space.
RGBHSV: RGBHSV converts a color specification given
in the
RGB color space to color values in the HSV color
space.
.SH SYNOPSIS
CALL RGBHLS (R, G, B, H, L, S)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_rgbhls (float r, float g, float b, float *h, float *l, float *s)
.SH DESCRIPTION 
.IP R 12
A real variable in the range [0.,1.] that represents
the red intensity component of the input point in RGB
color space.
.IP G 12
A real variable in the range [0.,1.] that represents
the green intensity component of the input point in RGB
color space.
.IP B 12
A real variable in the range [0.,1.] that represents
the blue intensity component of the input point in RGB
color space.
.IP H 12
A real variable in the range [0.,360.) that represents
the hue of the input point in HLS color space. A value
of (0.,0.,B) in the input space will result in a hue of
0. in the output space.
.IP L 12
A real variable in the range [0.,100.] that represents
the lightness value of the input point in HLS color
space.  Lightness is a measure of the quantity of light
- a lightness of 0. is black, and a lightness of 100.
gives white. The pure hues occur at lightness value 50.
The lightness should be thought of as a percentage.
.IP S 12
A real variable in the range [0.,100.] that represents
the saturation value of the input point in HLS color
space.  Saturation is a measure of how much white light
is mixed with the color. Saturation values of 0.
represent grays (with a gray value equal to the
lightness value L). Saturation values of 100. are fully
saturated colors. The hue is undefined when S=0.  The
fully saturated pure hues occur when S=100. and L=50.
The saturation value should be thought of as a
percentage.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use RGBHLS load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_rgbhls load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online: hlsrgb hsvrgb rgbhls rgbhsv rgbyiq yiqrgb ncarg_cbind
.sp
Hardcopy: "NCAR Graphics Autograph, A Graphing Utility, Version
2.00, August 1987", "NCAR Graphics User's Guide,
Version 2.00", and "NCAR Graphics Guide to New
Utilities, Version 3.00."
.sp
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

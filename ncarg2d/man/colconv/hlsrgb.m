.\"
.\"	$Id: hlsrgb.m,v 1.1 1993-03-11 16:15:24 haley Exp $
.\"
.TH HLSRGB 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
HLSRGB - converts a color specification given as the Hue,
Lightness, and Saturation (HLS) values to Red, Green, and Blue
(RGB) intensity values in the RGB color space.
.SH SYNOPSIS
CALL HLSRGB ( H, L, S, R, G, B )
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_hlsrgb (float h, float l, float s, float *r, float *g, float *b)
.SH DESCRIPTION 
.IP H 12
A real variable in the range [0.,360.) that represents
the hue of the input color in HLS color space. H=0.
corresponds to blue.
.IP L 12
A real variable in the range [0.,100.] that represents
the lightness value of the input color in HLS color
space.  Lightness is a measure of the quantity of light
- a lightness of 0. is black, and a lightness of 100.
gives white. The pure hues occur at lightness value
50.
.IP S 12
A real variable in the range [0.,100.] that represents
the saturation value of the input color in HLS color
space.  Saturation is a measure of how much white light
is mixed with the color. Colors having a saturation
value of 0. represent grays with a gray intensity value
equal to the lightness L.  Colors with a saturation
value of 100. are fully saturated colors. The hue is
undefined when S=0.  The fully saturated pure hues
occur when S=100.  and L=50.
.IP R 12
A real variable in the range [0.,1.] that represents
the red intensity component of the output color in RGB
color space.
.IP G 12
A real variable in the range [0.,1.] that represents
the green intensity component of the output color in
RGB color space.
.IP B 12
A real variable in the range [0.,1.] that represents
the blue intensity component of the output color in RGB
color space.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use HLSRGB load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_hlsrgb load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.sp
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

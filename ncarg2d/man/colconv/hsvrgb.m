.\"
.\"	$Id: hsvrgb.m,v 1.1 1993-03-11 16:15:26 haley Exp $
.\"
.TH HSVRGB 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
HSVRGB - HSVRGB converts a color specification given in the
Hue, Saturation, and Value (HSV) color space to color values
in the RGB color space.
.SH SYNOPSIS
CALL HSVRGB (H, S, V, R, G, B)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_hsvrgb (float h, float s, float v, float *r, float *g, float *b)
.SH DESCRIPTION 
.IP H 12
A real variable in the range [0.,360.) that represents
the hue of the input color in HSV color space.
.IP S 12
A real variable in the range [0.,1.] that represents
the saturation value of the input color in HSV color
space.  Saturation is a measure of how much white light
is mixed with the color. Saturation values of 0.
represent grays (with a gray value equal to the value
V). Saturation values of 1. are fully saturated colors.
The hue is undefined when S=0. The fully saturated pure
hues occur when S=1. and V=1.
.IP V 12
A real variable in the range [0.,1.] that represents
the value of the input color in HSV color space.
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
To use HSVRGB load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_hsvrgb load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online: hlsrgb hsvrgb rgbhls rgbhsv rgbyiq yiqrgb ncarg_cbind
.sp
Hardcopy: "NCAR Graphics Autograph, A Graphing Utility, Version
2.00, August 1987", "NCAR Graphics User's Guide,
Version 2.00", and "NCAR Graphics Guide to New
Utilities, Version 3.00."
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

.\"
.\"	$Id: rgbhsv.m,v 1.1 1993-03-11 16:15:32 haley Exp $
.\"
.TH RGBHSV 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
RGBHSV - converts a color specification given in the
RGB color space to color values in the HSV color space.
.SH SYNOPSIS
CALL RGBHSV (R, G, B, H, S, V)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_rgbhsv (float r, float g, float b, float *h, float *s, float *v)
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
the hue of the input point in HSV color space. A value
of (R,0.,0.) in the input space will result in a hue of
0. in the output space.
.IP S 12
A real variable in the range [0.,1.] that represents
the saturation value of the input point in HSV color
space.  Saturation is a measure of how much white light
is mixed with the color. Saturation values of 0.
represent grays (with a gray value equal to V).
Saturation values of 1. are fully saturated colors.
The hue is technically undefined when S=0.; the code
leaves H at its previous value when S=0. (0.
initially). The fully saturated pure hues occur when
S=1. and V=1.
.IP V 12
A real variable in the range [0.,1.] that represents
the value in HSV space.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use RGBHSV load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_rgbhsv load 
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

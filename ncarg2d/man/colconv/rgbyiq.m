.\"
.\"	$Id: rgbyiq.m,v 1.1 1993-03-11 16:15:35 haley Exp $
.\"
.TH RGBYIQ 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
RGBYIQ - RGBYIQ converts a color specification given in the RGB
color space to a color specification in the YIQ color space.
.SH SYNOPSIS
CALL RGBYIQ (R, G, B, Y, I, Q)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_rgbyiq (float r, float g, float b, float *y, float *i, float *q)
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
.IP Y 12
A real variable in the range [0.,1.]. Y is the color
component of a television signal that is shown on
black-and-white televisions; Y minimizes the effect of
two colors appearing different to the human eye but
mapping to similar monochrome intensities.
.IP I 12
A real variable in the range [-.6,.6]. I attains its
maximum when the input triple is (1.,0.,0.); I attains
its minimum when the input triple is (0.,1.,1.).
.IP Q 12
A real variable in the range [-.52,.52]. Q attains its
maximum when the input triple is (1.,0.,1.); Q attains
its minimum when the input triple is (0.,1.,0.).
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use RGBYIQ load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_rgbyiq load 
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

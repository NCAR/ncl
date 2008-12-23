.TH YIQRGB 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
YIQRGB - Converts a color specification given in the YIQ
color space to the equivalent color specification in the
Red, Green, Blue (RGB) color space.
.SH SYNOPSIS
CALL YIQRGB (Y, I, Q, R, G, B)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_yiqrgb (float y, float i, float q, float *r, 
.br
float *g, float *b)
.SH DESCRIPTION 
.IP Y 12
(REAL, input, range [0.,1.]) 
specifies the color
component of a television signal that is shown on
black-and-white televisions; Y minimizes the effect of
two colors appearing different to the human eye but
mapping to similar monochrome intensities.
.IP I 12
(REAL, input, range [-.6,.6])
.IP Q 12
(REAL, input, range [-.52,.52])
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
tcolcv,
fcce02.
.SH ACCESS
To use YIQRGB or c_yiqrgb, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
colconv,
hlsrgb,
hsvrgb,
rgbhls,
rgbhsv,
rgbyiq,
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

.\"
.\"	$Id: colconv.m,v 1.1 1993-03-11 16:15:20 haley Exp $
.\"
.TH COLCONV 3NCARG "12 June 1991" UNIX "NCAR GRAPHICS"
.SH NAME
COLCONV - Converts color values from one color space to another
.SH SYNOPSIS
CALL HLSRGB ( H, L, S, R, G, B ) Convert from HLS to RGB
.br
CALL HSVRGB ( H, S, V, R, G, B ) Convert from HSV to RGB
.br
CALL RGBHLS ( R, G, B, H, L, S ) Convert from RGB to HLS
.br
CALL RGBHSV ( R, G, B, H, S, V ) Convert from RGB to HSV
.br
CALL RGBYIQ ( R, G, B, Y, I, Q ) Convert from RGB to YIQ
.br
CALL YIQRGB ( Y, I, Q, R, G, B ) Convert from YIQ to RGB
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
To use COLCONV routines load the NCAR Graphics libraries
ncarg, ncarg_gks, and ncarg_loc, preferably in that order.
To use the COLCONV C-bindings load the NCAR Graphics libraries
ncargC, ncarg_gksC, ncarg, ncarg_gks, and ncarg_loc, preferably 
in that order.
.SH SEE ALSO
Online:
hlsrgb, hsvrgb, rgbhls, rgbhsv, rgbyiq, yiqrgb ncarg_cbind
.sp
Hardcopy:
"NCAR Graphics User's Guide, Version 2.00", and
"NCAR Graphics Guide to New Utilities, Version 3.00."
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

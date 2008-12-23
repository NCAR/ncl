.\"
.\"	$Id: gtx.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GTX 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GTX (Text) - The basic output primitive for drawing text.  A number 
of attribute setting routines are available for selecting the size, font, 
precision, orientation, color, character spacing, etc.
.SH SYNOPSIS
CALL GTX (X, Y, STRING)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gtext(const Gpoint *text_pos, const char *char_string);
.SH DESCRIPTION
.IP "X" 12
(Real, Input) - The X world coordinate of the text 
alignment of the first 
character in STRING.
.IP "Y" 12
(Real, Input) - The Y world coordinate of the text
alignment of the first 
character in STRING.
.IP STRING 12
(Character variable, Input) - The characters to be drawn.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gstxp, gstxal, gstxfp, gschh, gschsp, gschup, 
gschxp, gscr, gstxci, gqtxp, gqtxal, gqtxfp, gqchh, 
gqchsp, gqchup, gqchxp, plotchar, gtext
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

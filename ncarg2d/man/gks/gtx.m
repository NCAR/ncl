.\"
.\"	$Id: gtx.m,v 1.14 2007-02-27 18:20:21 haley Exp $
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
Copyright (C) 1987-2007
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

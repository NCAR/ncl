.\"
.\"	$Id: gstxal.m,v 1.9 2000-07-11 23:03:19 haley Exp $
.\"
.TH GSTXAL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSTXAL (Set text alignment) - sets the text alignment.
.SH SYNOPSIS
CALL GSTXAL (TXALH, TXALV)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_text_align(const Gtext_align *text_align);
.SH DESCRIPTION
.IP TXALH 12
(Integer, Input) - 
Determines the horizontal alignment of the text to be drawn. Options 
are:
.RS
.IP 0 
Normal (default)
.IP 1 
Left
.IP 2 
Center
.IP 3 
Right
.RE
.IP TXALV 12
(Integer, Input) - 
Determines the vertical alignment of the text to be drawn. Options are:
.RS
.IP 0 
Normal (default)
.IP 1 
Top
.IP 2 
Cap
.IP 3 
Half
.IP 4 
Base
.IP 5 
Bottom
.RE
.SH USAGE
Text alignment is used to position the text extent 
rectangle.  The text extent rectangle rotates about 
the point determined by the horizontal and vertical 
text alignment settings (TXALH, TXALV).
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gtx, gstxp, gstxfp, gschh, gschsp, gschup, 
gschxp, gscr, gstxci, gqtxp, gqtxal, gqtxfp, gqchh, 
gqchsp, gqchup, gqchxp, plotchar, gset_text_align
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.

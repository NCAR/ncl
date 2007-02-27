.\"
.\"	$Id: gstxp.m,v 1.14 2007-02-27 18:20:21 haley Exp $
.\"
.TH GSTXP 3NCARG "14 January 1992" UNIX "NCAR GRAPHICS"
.SH NAME
GSTXP (Set text path) - sets the text paths or directions
in which text is to be drawn.
.SH SYNOPSIS
CALL GSTXP (TXP)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_text_path(Gtext_path text_path);
.SH DESCRIPTION
.IP TXP 11
(Integer, Input) - 
Gives the direction in which a character string is to be drawn. 
Options are:
.RS
.IP 0 3
Draw successive characters in the string such that character 
n+l appears to the right of character n. This is text path
"right" and is the default.
.IP 1 3
Draw character n+1 to the left of character n.  This is text path
"left".
.IP 2 3
Draw character n+1 above character n.  This is text path "up".
.IP 3 3
Draw character n+1 below character n.  This is text path "down".
.PP
The right, left, up, and down directions are relative to the 
character up vector. The right text path direction is perpendicular 
to the up vector direction.  Thus, to draw a text string at a 45 
degree angle the character up vector would be (-1,1), and the text 
path would be right .
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gtx, gstxal, gstxfp, gschh, gschsp, gschup, 
gschxp, gscr, gstxci, gqtxp, gqtxal, gqtxfp, gqchh, 
gqchsp, gqchup, gqchxp, plotchar, gset_text_path
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

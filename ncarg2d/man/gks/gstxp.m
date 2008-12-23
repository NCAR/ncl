.\"
.\"	$Id: gstxp.m,v 1.16 2008-12-23 00:03:03 haley Exp $
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

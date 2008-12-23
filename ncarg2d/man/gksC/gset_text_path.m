.\"
.\"	$Id: gset_text_path.m,v 1.16 2008-12-23 00:03:05 haley Exp $
.\"
.TH GSET_TEXT_PATH 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_text_path (Set text path) - sets the text paths or directions
in which text is to be drawn.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_text_path(Gtext_path text_path);
.SH DESCRIPTION
.IP text_path 11
(Input) Gives the direction in which a character string is to be drawn. 
Options are:
.RS
.IP GPATH_RIGHT 3
Draw successive characters in the string such that character 
n+l appears to the right of character n. This is text path
"right" and is the default.
.IP GPATH_LEFT 3
Draw character n+1 to the left of character n.  This is text path
"left".
.IP GPATH_UP 3
Draw character n+1 above character n.  This is text path "up".
.IP GPATH_DOWN 3
Draw character n+1 below character n.  This is text path "down".
.PP
The right, left, up, and down directions are relative to the 
character up vector. The right text path direction is perpendicular 
to the up vector direction.  Thus, to draw a text string at a 45 
degree angle the character up vector would be (-1,1), and the text 
path would be right .
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gtext(3NCARG),
.BR gset_text_align(3NCARG),
.BR gset_text_font_prec(3NCARG),
.BR gset_char_ht(3NCARG),
.BR gset_char_space(3NCARG),
.BR gset_char_up_vec(3NCARG),
.BR gset_char_expan(3NCARG),
.BR gset_colr_rep(3NCARG),
.BR gset_text_colr_ind(3NCARG),
.BR ginq_text_path(3NCARG),
.BR ginq_text_align(3NCARG),
.BR ginq_text_font_prec(3NCARG),
.BR ginq_char_ht(3NCARG),
.BR ginq_char_space(3NCARG),
.BR ginq_char_up_vec(3NCARG),
.BR ginq_char_expan(3NCARG),
.BR plotchar(3NCARG),
.BR gks(3NCARG)
.BR ncarg_gks_cbind(3NCARG)
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

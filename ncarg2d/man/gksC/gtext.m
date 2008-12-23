.\"
.\"	$Id: gtext.m,v 1.17 2008-12-23 00:03:05 haley Exp $
.\"
.TH GTEXT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gtext (Text) - The basic output primitive for drawing text.  A number 
of attribute setting routines are available for selecting the size, font, 
precision, orientation, color, character spacing, etc.
.SH SYNOPSIS
void gtext(const Gpoint *text_pos, const char *char_string);
.SH DESCRIPTION
.IP text_pos.x 12
(Gfloat, Input) The x world coordinate of the text alignment of the first 
character in char_string.
.IP text_pos.y 12
(Gfloat, Input) The y world coordinate of the text alignment of the first 
character in char_string.
.IP char_string 12
(Input) The characters to be drawn.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gset_text_path(3NCARG),
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
.BR gks(3NCARG),
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

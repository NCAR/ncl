.\"
.\"	$Id: gset_text_align.m,v 1.16 2008-12-23 00:03:05 haley Exp $
.\"
.TH GSET_TEXT_ALIGN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_text_align (Set text alignment) - sets the text alignment.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_text_align(const Gtext_align *text_align);
.SH DESCRIPTION
.IP text_align.hor 12
(Input) Determines the horizontal alignment of the text to be drawn. Options 
are:
.RS
.IP GHOR_NORM
Normal (default)
.IP GHOR_LEFT
Left
.IP GHOR_CTR
Center
.IP GHOR_RIGHT
Right
.RE
.IP text_align.vert 12
(Input) Determines the vertical alignment of the text to be drawn. Options are:
.RS
.IP GVERT_NORM
Normal (default)
.IP GVERT_TOP
Top
.IP GVERT_CAP
Cap
.IP GVERT_HALF
Half
.IP GVERT_BASE
Base
.IP GVERT_BOTTOM
Bottom
.RE
.SH USAGE
Text alignment is used to position the text extent 
rectangle.  The text extent rectangle rotates about 
the point determined by the horizontal and vertical 
text alignment settings (text_align.hor, text_align.vert).
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gtext(3NCARG),
.BR gset_text_path(3NCARG),
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

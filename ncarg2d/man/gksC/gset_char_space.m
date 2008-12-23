.\"
.\"	$Id: gset_char_space.m,v 1.16 2008-12-23 00:03:04 haley Exp $
.\"
.TH GSET_CHAR_SPACE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_char_space (Set character spacing) - sets the character spacing for text.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_char_space(Gdouble char_space);
.SH DESCRIPTION
.IP char_space 12
(Input) - 
Specifies how much additional white 
space should be inserted between 
characters drawn using the gtext 
output primitive. char_space is specified 
as a fraction of the character 
height.
.sp
A character spacing of zero positions the character 
body of each successive character contiguously with enough white
space between them to produce pleasing normal spacing. A 
positive value inserts extra space between successive 
characters. A negative value causes successive characters to overlap.
By default char_space = 0.0.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gtext(3NCARG),
.BR gset_text_path(3NCARG),
.BR gset_text_align(3NCARG),
.BR gset_text_font_prec(3NCARG),
.BR gset_char_ht(3NCARG),
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

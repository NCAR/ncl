.\"
.\"	$Id: gset_char_ht.m,v 1.1 1993-03-21 01:31:17 haley Exp $
.\"
.TH GSET_CHAR_HT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_char_ht (Set character height) - sets the character height for text.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_char_ht(Gdouble char_ht);
.SH DESCRIPTION
.IP char_ht 12
(Input) - Gives the height in world coordinates at which a character is 
drawn using the GTX output 
primitive. char_ht > 0.
By default char_ht = 0.01.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gksC, ncarg_gks, ncarg_c,
and ncarg_loc libraries.
.SH SEE ALSO
Online: 
.BR gtext(3NCARG),
.BR gset_text_path(3NCARG),
.BR gset_text_align(3NCARG),
.BR gset_text_font_prec(3NCARG),
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
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

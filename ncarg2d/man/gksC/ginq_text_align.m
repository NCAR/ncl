.\"
.\"	$Id: ginq_text_align.m,v 1.1 1993-03-21 01:30:39 haley Exp $
.\"
.TH GINQ_TEXT_ALIGN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ginq_text_align (Inquire text alignment) - retrieves the horizontal and vertical
text alignments.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_text_align(Gint *err_ind, Gtext_align *text_align);
.SH DESCRIPTION
.IP err_ind 12
(Output) - If the inquired values cannot be returned correctly,
a non-zero error indicator is returned in err_ind, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP text_align->hor 12
(Output) - Returns the current horizontal text alignment:
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
.IP text_align->vert 12
(Output) - Returns the current vertical text alignment:
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
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gksC, ncarg_gks, ncarg_c,
and ncarg_loc libraries.
.SH SEE ALSO
Online: 
.BR gtext,
.BR gset_text_path,
.BR gset_text_align,
.BR gset_text_font_prec,
.BR gset_char_ht,
.BR gset_char_space,
.BR gset_char_up_vec,
.BR gset_char_expan,
.BR gset_colr_rep,
.BR gset_text_colr_ind,
.BR ginq_text_path,
.BR ginq_text_align,
.BR ginq_text_font_prec,
.BR ginq_char_ht,
.BR ginq_char_space,
.BR ginq_char_up_vec,
.BR ginq_char_expan,
.BR plotchar,
.BR ngdots,
.BR gks,
.BR ncarg_gks_cbind
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

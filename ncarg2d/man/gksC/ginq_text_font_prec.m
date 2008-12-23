.\"
.\"	$Id: ginq_text_font_prec.m,v 1.17 2008-12-23 00:03:04 haley Exp $
.\"
.TH GINQ_TEXT_FONT_PREC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ginq_text_font_prec (Inquire text font and precision) - retrieves the current setting
of the text font and precision values.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_text_font_prec(Gint *err_ind, Gtext_font_prec *text_font_prec);
.SH DESCRIPTION
.IP err_ind 12
(Output) - If the inquired values cannot be returned correctly,
a non-zero error indicator is returned in err_ind, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP text_font_prec.font 12
(Output) - 
Gives the font number as set by default or by a call to
gset_text_font_prec.  Font number one is a simple default ASCII font. 
Values for font between -2 and -20 (inclusive) are Hershey fonts.
.IP text_font_prec.prec 12
(Output) - Gives the text precision as set by default or by a call to
gset_text_font_prec.  Possible values include:
.RS
.IP GPREC_STRING
String precision (good)
.IP GPREC_CHAR
Character precision (better)
.IP GPREC_STROK
Stroke precision (best)
.RE
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

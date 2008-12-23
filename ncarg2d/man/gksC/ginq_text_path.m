.\"
.\"	$Id: ginq_text_path.m,v 1.16 2008-12-23 00:03:04 haley Exp $
.\"
.TH GINQ_TEXT_PATH 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ginq_text_path (Inquire text path) - retrieves the current text path.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_text_path(Gint *err_ind, Gtext_path *text_path);
.SH DESCRIPTION
.IP err_ind 12
(Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in err_ind, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP text_path 12
(Output) - Returns the current text path as set by default or
by a call to gset_text_path.  Legal values are:
.RS
.IP GPATH_RIGHT
Draw successive characters in the string such that character 
n+l appears to the right of character n. This is text path "right" and
is the default.
.IP GPATH_LEFT
Draw character n+1 to the left of character n.  This is text path "left".
.IP GPATH_UP
Draw character n+1 above character n.  This is text path "up".
.IP GPATH_DOWN
Draw character n+1 below character n.  This is text path "down".
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

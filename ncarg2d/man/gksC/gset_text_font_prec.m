.\"
.\"	$Id: gset_text_font_prec.m,v 1.17 2008-12-23 00:03:05 haley Exp $
.\"
.TH GSET_TEXT_FONT_PREC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_text_font_prec (Set text font and precision) - sets the text font 
and precision.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_text_font_prec(const Gtext_font_prec *text_font_prec);
.SH DESCRIPTION
.IP text_font_prec.font 12
(Gint, Input) Specifies the text font to be used in subsequent calls to 
the gtext output primitive. Options are:
.RS
.IP "  1"
ASCII font (default)
.IP " -2"
Hershey cartographic Roman
.IP " -3"
Hershey cartographic Greek
.IP " -4"
Hershey simplex Roman
.IP " -5"
Hershey simplex Greek
.IP " -6"
Hershey simplex script
.IP " -7"
Hershey complex Roman
.IP " -8"
Hershey complex Greek
.IP " -9"
Hershey complex script
.IP "-10"
Hershey complex italic
.IP "-11"
Hershey complex Cyrillic
.IP "-12"
Hershey duplex Roman
.IP "-13"
Hershey triplex Roman
.IP "-14"
Hershey triplex italic
.IP "-15"
Hershey Gothic German
.IP "-16"
Hershey Gothic English
.IP "-17"
Hershey Gothic Italian
.IP "-18"
Hershey math symbols
.IP "-19"
Hershey symbol set 1
.IP "-20"
Hershey symbol set 2
.sp
The Hershey fonts are not standardized by GKS but are 
locally implemented in NCAR GKS-0A. GKS requires that 
such locally implemented fonts be assigned negative 
font numbers. To view the Hershey fonts,
look at the plots produced from the PLOTCHAR example (execute
"ncargex epltch" to get a metafile) and examine the plot
titled "PLCHHQ - FONTCAP DATABASES ADDED 6/90".  The font
numbers there are the absolute values of the value for text_font_prec.font
described here (PLOTCHAR has no need to follow
the structures that GKS imposes on font names for gset_text_font_prec).
.RE
.IP text_font_prec.prec 12
(Input) Gives the precision used in subsequent calls to the gtext output 
primitive for font type text_font_prec.font. 
Options are:
.RS
.IP GPREC_STRING
String precision (good). This is the GKS default.
.IP GPREC_CHAR
Character precision (better). 
.IP GPREC_STROKE
Stroke precision (best). This is the default for NCAR GSK-0A.
.RE
.SH USAGE
If one accesses the Hershey fonts via gset_text_font_prec and gtext, then
the characters are not stroked until viewing time.  If
one accesses the Hershey fonts via PLOTCHAR, then the characters
are stroked by PLOTCHAR itself.  This can make a significant
difference in metafile sizes.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gtext(3NCARG),
.BR gset_text_path(3NCARG),
.BR gset_text_align(3NCARG),
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
.BR ncarg_gks_cbind(3NCARG),
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

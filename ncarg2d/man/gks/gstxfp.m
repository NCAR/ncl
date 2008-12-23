.\"
.\"	$Id: gstxfp.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GSTXFP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSTXFP (Set text font and precision) - sets the text font and precision.
.SH SYNOPSIS
CALL GSTXFP (FONT, PREC)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_text_font_prec(const Gtext_font_prec *text_font_prec);
.SH DESCRIPTION
.IP FONT 12
(Integer, Input) - Specifies the text font to be used in 
subsequent calls to the GTX output primitive. Options are:
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
.PP
The Hershey fonts are not standardized by GKS but are 
locally implemented in NCAR GKS-0A. GKS requires that 
such locally implemented fonts be assigned negative 
font numbers. To view the Hershey fonts,
look at the plots produced from the PLOTCHAR example (execute
"ncargex epltch" to get a metafile) and examine the plot
titled "PLCHHQ - FONTCAP DATABASES ADDED 6/90".  The font
numbers there are the absolute values of the value for FONT
described here (PLOTCHAR has no need to follow
the strictures that GKS imposes on font names for GSTXFP).
.RE
.IP PREC 12
(Integer, Input) - 
Gives the precision used in subsequent calls to the GTX output 
primitive for font type FONT. 
Options are:
.RS
.IP 0 
String precision (good). This is the GKS default.
.IP 1 
Character precision (better). 
.IP 2 
Stroke precision (best). This is the default for NCAR
GSK-0A.
.RE
.SH USAGE
If one accesses the Hershey fonts via GSTXFP and GTX, then
the characters are not stroked until viewing time.  If
one accesses the Hershey fonts via PLOTCHAR, then the characters
are stroked by PLOTCHAR itself.  This can make a significant
difference in metafile sizes.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gtx, gstxp, gstxal, gschh, gschsp, gschup, 
gschxp, gscr, gstxci, gqtxp, gqtxal, gqtxfp, gqchh, 
gqchsp, gqchup, gqchxp, plotchar, gset_text_font_prec
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

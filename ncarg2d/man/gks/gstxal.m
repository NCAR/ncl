.\"
.\"	$Id: gstxal.m,v 1.2 1993-04-02 16:50:49 haley Exp $
.\"
.TH GSTXAL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSTXAL (Set text alignment) - sets the text alignment.
.SH SYNOPSIS
CALL GSTXAL (TXALH, TXALV)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_text_align(const Gtext_align *text_align);
.SH DESCRIPTION
.IP TXALH 12
(Integer, Input) - 
Determines the horizontal alignment of the text to be drawn. Options 
are:
.RS
.IP 0 
Normal (default)
.IP 1 
Left
.IP 2 
Center
.IP 3 
Right
.RE
.IP TXALV 12
(Integer, Input) - 
Determines the vertical alignment of the text to be drawn. Options are:
.RS
.IP 0 
Normal (default)
.IP 1 
Top
.IP 2 
Cap
.IP 3 
Half
.IP 4 
Base
.IP 5 
Bottom
.RE
.SH USAGE
Text alignment is used to position the text extent 
rectangle.  The text extent rectangle rotates about 
the point determined by the horizontal and vertical 
text alignment settings (TXALH, TXALV).
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gtx, gstxp, gstxfp, gschh, gschsp, gschup, 
gschxp, gscr, gstxci, gqtxp, gqtxal, gqtxfp, gqchh, 
gqchsp, gqchup, gqchxp, plotchar, gset_text_align
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

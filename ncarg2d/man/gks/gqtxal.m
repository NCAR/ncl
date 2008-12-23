.\"
.\"	$Id: gqtxal.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GQTXAL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQTXAL (Inquire text alignment) - retrieves the horizontal and vertical
text alignments.
.SH SYNOPSIS
CALL GQTXAL (ERRIND, TXALH, TXALV)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_text_align(Gint *err_ind, Gtext_align *text_align);
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired values cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP TXALH 12
(Real, Output) - Returns the current horizontal text alignment:
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
(Real, Output) - Returns the current vertical text alignment:
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
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
gtx, gstxp, gstxal, gstxfp, gschh, gschsp, gschup, 
gschxp, gscr, gstxci, gqtxp, gqtxal, gqtxfp, gqchh, 
gqchsp, gqchup, gqchxp, plotchar, ngdots, ginq_text_align
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

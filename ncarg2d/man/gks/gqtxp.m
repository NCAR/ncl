.\"
.\"	$Id: gqtxp.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GQTXP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQTXP (Inquire text path) - retrieves the current text path.
.SH SYNOPSIS
CALL GQTXP (ERRIND, TXP)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_text_path(Gint *err_ind, Gtext_path *text_path);
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP TXP 12
(Integer, Output) - Returns the current text path as set by default or
by a call to GSTXP.  Legal values are:
.RS
.IP 0 
Draw successive characters in the string such that character 
n+l appears to the right of character n. This is text path "right" and
is the default.
.IP 1 
Draw character n+1 to the left of character n.  This is text path "left".
.IP 2 
Draw character n+1 above character n.  This is text path "up".
.IP 3 
Draw character n+1 below character n.  This is text path "down".
.RE
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
gtx, gstxp, gstxal, gstxfp, gschh, gschsp, gschup, 
gschxp, gscr, gstxci, gqtxp, gqtxal, gqtxfp, gqchh, 
gqchsp, gqchup, gqchxp, plotchar, ginq_text_path
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

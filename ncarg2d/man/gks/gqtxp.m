.\"
.\"	$Id: gqtxp.m,v 1.12 2005-01-04 15:42:07 haley Exp $
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
Copyright (C) 1987-2005
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.

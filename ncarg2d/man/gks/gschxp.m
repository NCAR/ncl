.\"
.\"	$Id: gschxp.m,v 1.11 2003-05-25 17:16:51 haley Exp $
.\"
.TH GSCHXP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSCHXP (Set character expansion factor) - sets 
the width to
height ratio of text.
.SH SYNOPSIS
CALL GSCHXP (CHXP)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_char_expan(Gdouble char_expan);
.SH DESCRIPTION
.IP CHXP 12
(Real, Input) - 
Specifies a deviation of the width 
to height ratio for characters drawn 
using the GTX output primitive. CHXP 
of 10.0 would specify a character 
that is 10 times as wide as normal. 
CHXP of 0.1 would request characters 
that are 1/10th as wide as normal. 
The character height remains 
unchanged. CHXP > 0.
By default CHXP = 1.0
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gtx, gstxp, gstxal, gstxfp, gschh, gschsp, gschup, 
gscr, gstxci, gqtxp, gqtxal, gqtxfp, gqchh, 
gqchsp, gqchup, gqchxp, plotchar, gset_char_expan
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2003
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

.\"
.\"	$Id: gschsp.m,v 1.9 2000-07-11 23:03:16 haley Exp $
.\"
.TH GSCHSP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSCHSP (Set character spacing) - sets the character spacing for text.
.SH SYNOPSIS
CALL GSCHSP (CHSP)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_char_space(Gdouble char_space);
.SH DESCRIPTION
.IP CHSP 12
(Real, Input) - 
Specifies how much additional white 
space should be inserted between 
characters drawn using the GTX 
output primitive. CHSP is specified 
as a fraction of the character 
height.
.sp
A character spacing of zero positions the character 
body of each successive character contiguously with enough white
space between them to produce pleasing normal spacing. A 
positive value inserts extra space between successive 
characters. A negative value causes successive characters to overlap.
By default CHSP = 0.0.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gtx, gstxp, gstxal, gstxfp, gschh, gschup, 
gschxp, gscr, gstxci, gqtxp, gqtxal, gqtxfp, gqchh, 
gqchsp, gqchup, gqchxp, plotchar, gset_char_space
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.

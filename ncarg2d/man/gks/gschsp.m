.\"
.\"	$Id: gschsp.m,v 1.16 2008-12-23 00:03:03 haley Exp $
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

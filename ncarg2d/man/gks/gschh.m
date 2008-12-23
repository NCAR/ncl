.\"
.\"	$Id: gschh.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GSCHH 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSCHH (Set character height) - sets the character height for text.
.SH SYNOPSIS
CALL GSCHH (CHH)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_char_ht(Gdouble char_ht);
.SH DESCRIPTION
.IP CHH 12
(Real, Input) - 
Gives the height in world 
coordinates at which a character is 
drawn using the GTX output 
primitive. CHH > 0.
By default CHH = 0.01.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gtx, gstxp, gstxal, gstxfp, gschsp, gschup, 
gschxp, gscr, gstxci, gqtxp, gqtxal, gqtxfp, gqchh, 
gqchsp, gqchup, gqchxp, plotchar, gset_char_ht
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

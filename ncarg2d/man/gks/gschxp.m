.\"
.\"	$Id: gschxp.m,v 1.1 1993-03-11 16:23:43 haley Exp $
.\"
.TH GSCHXP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSCHXP (Set character expansion factor) - sets 
the width to
height ratio of text.
.SH SYNOPSIS
CALL GSCHXP (CHXP)
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
gqchsp, gqchup, gqchxp, plotchar
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

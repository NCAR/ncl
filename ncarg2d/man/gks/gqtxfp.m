.\"
.\"	$Id: gqtxfp.m,v 1.1 1993-03-11 16:23:28 haley Exp $
.\"
.TH GQTXFP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQTXFP (Inquire text font and precision) - retrieves the current setting
of the text font and precision values.
.SH SYNOPSIS
CALL GQTXFP (ERRIND, FONT, PREC)
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired values cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP FONT 12
(Integer, Output) - 
Gives the font number as set by default or by a call to
GSTXFP.  FONT number one is a simple default ASCII font. Values for
FONT between -2 and -20 (inclusive) are Hershey fonts.
.IP PREC 12
(Integer, Output) - Gives the text precision as set by default or by a call to
GSTXFP.
.RS
.IP 0
String precision (good)
.IP 1
Character precision (better)
.IP 2
Stroke precision (best)
.RE
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
gtx, gstxp, gstxal, gstxfp, gschh, gschsp, gschup, 
gschxp, gscr, gstxci, gqtxp, gqtxal, gqtxfp, gqchh, 
gqchsp, gqchup, gqchxp, plotchar
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

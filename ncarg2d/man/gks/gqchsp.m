.\"
.\"	$Id: gqchsp.m,v 1.1 1993-03-11 16:22:32 haley Exp $
.\"
.TH GQCHSP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQCHSP (Inquire character spacing) - retrieves the current value for 
character spacing.
.SH SYNOPSIS
CALL GQCHSP (ERRIND, CHSP)
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP CHSP 12
(Real, Output) - 
Contains the current value for character spacing as set by default or
by a call to GSCHSP.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gtx, gstxp, gstxal, gstxfp, gschh, gschsp, gschup, 
gschxp, gscr, gstxci, gqtxp, gqtxal, gqtxfp, gqchh, 
gqchup, gqchxp, plotchar
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

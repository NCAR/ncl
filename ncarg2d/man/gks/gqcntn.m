.\"
.\"	$Id: gqcntn.m,v 1.1 1993-03-11 16:22:42 haley Exp $
.\"
.TH GQCNTN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQCNTN (Inquire current normalization transformation number) - retrieves
the number of the current normalization transformation.
.SH SYNOPSIS
CALL GQCNTN (ERRIND, CTNR)
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP CTNR 12
(Integer, Output) - 
Returns the current normalization transformation number as set by default
or by a call to GSELNT.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gselnt, gqmntn, gqnt, getset
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

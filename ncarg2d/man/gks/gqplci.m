.\"
.\"	$Id: gqplci.m,v 1.2 1993-04-02 16:47:53 haley Exp $
.\"
.TH GQPLCI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQPLCI (Inquire polyline color index) - gets the polyline color index.
.SH SYNOPSIS
CALL GQPLCI (ERRIND, COLI)
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP COLI 12
(Integer, Output) - Returns the polyline color index as set by default
of by a call to GSPLCI.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
gpl, gsln, gslwsc, gscr, gsplci, gqln, gqlwsc, gqplci, dashline
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

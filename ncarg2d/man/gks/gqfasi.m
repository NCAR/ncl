.\"
.\"	$Id: gqfasi.m,v 1.1 1993-03-11 16:22:53 haley Exp $
.\"
.TH GQFASI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQFASI (Inquire fill area style index) - retrieves the current value of
the fill area style index.
.SH SYNOPSIS
CALL GQFASI (ERRIND, STYLI)
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP STYLI 12
(Integer, Output) - Returns the fill area style index as set by default
or by a call to GSFASI.
.RS
.IP 1 
Horizontal lines (default)
.IP 2 
Vertical lines
.IP 3 
Lines of positive slope
.IP 4 
Lines of negative slope
.IP 5 
Horizontal and vertical lines
.IP 6 
Lines of positive and negative 
slope
.RE
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
gfa, gsfais, gscr, gsfaci, gqfais, gqfasi, 
areas
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

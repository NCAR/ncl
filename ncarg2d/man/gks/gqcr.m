.\"
.\"	$Id: gqcr.m,v 1.1 1993-03-11 16:22:45 haley Exp $
.\"
.TH GQCR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQCR (Inquire color representation) - retrieves the color value associated
with a color index on a particular workstation.
.SH SYNOPSIS
CALL GQCR (WKID, CI, TYPE, ERRIND, CR, CG, CB)
.SH DESCRIPTION
.IP WKID 12
(Integer, Input) - Gives a workstation identifier as was set in
a previous call to GOPWK.
.IP CI 12
(Integer, Input) - A color index.
.IP TYPE 12
(Integer, Input) - Specifies whether the returned color values are the
ones that were specified by a call to GSCR or whether they are the
color values that are actually being used on the specified workstation
(frequently a workstation will not be able to represent a requested 
color value precisely).
.RS
.IP 0 
Returns the color value set for index CI either by default or by a call to GSCR.
.IP 1 
Returns the color value actually used by the workstation to represent the 
requested color.
.RE
.IP ERRIND 12
(Integer, Output) - If the inquired values cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP CR 12
(Real, Output) - Returns an intensity value for red in the 
range 0. to 1. inclusive.
.IP CG 12
(Real, Output) - Returns an intensity value for green in the 
range 0. to 1. inclusive.
.IP CB 12
(Real, Output) - Returns an intensity value for blue in the 
range 0. to 1. inclusive.
.SH USAGE
For details on setting and using color indices see the man page for GSCR.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
gscr, gsplci, gsmkci, gstxci, gsfaci
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

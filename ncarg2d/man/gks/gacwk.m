.\"
.\"	$Id: gacwk.m,v 1.1 1993-03-11 16:21:42 haley Exp $
.\"
.TH GACWK 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GACWK (Activate workstation) - 
activates a GKS workstation.
.SH SYNOPSIS
CALL GACWK (WKID)
.SH DESCRIPTION
.IP WKID 12
(Integer, Input) - A number identifying the workstation to be activated.
WKID must be the same as that used in some previous GOPWK call.
.SH USAGE
Active workstations receive all GKS output primitives.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gopks, gopwk, gdawk, gclwk, gclks, opngks, clsgks
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

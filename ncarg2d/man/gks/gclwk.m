.\"
.\"	$Id: gclwk.m,v 1.1 1993-03-11 16:21:54 haley Exp $
.\"
.TH GCLWK 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GCLWK (Close workstation) - closes a workstation.
.SH SYNOPSIS
CALL GCLWK (WKID)
.SH DESCRIPTION
.IP WKID 12
(Input, Integer) - A number identifying the workstation to be
closed.  WKID must be the same as that used in some previous GOPWK call.
.SH USAGE
Close workstation updates the workstation and removes its identifier
from the set of open workstations.  The connection to the workstation
is released and the associated connection ID is available for re-use.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gopks, gopwk, gacwk, gdawk, gclks, opngks, 
clsgks
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

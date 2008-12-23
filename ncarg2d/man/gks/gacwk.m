.\"
.\"	$Id: gacwk.m,v 1.16 2008-12-23 00:03:02 haley Exp $
.\"
.TH GACWK 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GACWK (Activate workstation) - 
activates a GKS workstation.
.SH SYNOPSIS
CALL GACWK (WKID)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gactivate_ws(Gint ws_id);
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
gopks, gopwk, gdawk, gclwk, gclks, opngks, clsgks, gactivate_ws
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

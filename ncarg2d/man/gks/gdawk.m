.\"
.\"	$Id: gdawk.m,v 1.16 2008-12-23 00:03:02 haley Exp $
.\"
.TH GDAWK 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GDAWK (Deactivate workstation) - deactivates a workstation.
.SH SYNOPSIS
CALL GDAWK (WKID)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gdeactivate_ws(Gint ws_id);
.SH DESCRIPTION
.IP WKID 12
(Integer, Input) - A number identifying the workstation to be deactivated.
WKID must be the same as that used in some previous GACWK call.
.SH USAGE
The specified workstation is removed from the set of active
workstations.  GKS output primitives are not sent to workstations
that are not active.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gopks, gopwk, gacwk, gclwk, gclks, opngks, 
clsgks, gdeactivate_ws
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

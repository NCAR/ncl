.\"
.\"	$Id: gdawk.m,v 1.14 2007-02-27 18:20:19 haley Exp $
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
Copyright (C) 1987-2007
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.

.\"
.\"	$Id: gacwk.m,v 1.13 2006-01-04 00:12:53 haley Exp $
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
Copyright (C) 1987-2006
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

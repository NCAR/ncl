.\"
.\"	$Id: gqops.m,v 1.11 2003-05-25 17:16:51 haley Exp $
.\"
.TH GQOPS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQOPS (Inquire operating state value) - returns the operating state
of GKS. 
.SH SYNOPSIS
CALL GQOPS (OPSTA)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_op_st(Gop_st *op_st);
.SH DESCRIPTION
.IP OPSTA 12
(Integer, Output) - Returns the GKS operating state:
.RS
.IP 0 
GKS is closed
.IP 1 
GKS is open
.IP 2 
Additionally, a workstation is open
.IP 3 
Additionally, a workstation is active
.IP 4 
Additionally, a segment is open
.RE
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
gopks, gopwk, gacwk, gdawk, gclwk, gclks, opngks, clsgks, ginq_op_st
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright (C) 1987-2003
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

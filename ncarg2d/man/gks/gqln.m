.\"
.\"	$Id: gqln.m,v 1.13 2006-01-04 00:12:56 haley Exp $
.\"
.TH GQLN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQLN (Inquire line type) - retrieves the current setting for linetype.
.SH SYNOPSIS
CALL GQLN (ERRIND, LTYPE)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_linetype(Gint *err_ind, Gint *linetype);
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP LTYPE 12
(Integer, Output) - Returns the current value for the line type as set by
default or by a call to GSLN.
.RS
.IP 1 
Solid 
.IP 2
Dashed
.IP 3
Dotted
.IP 4
Dot-dash
.RE
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
gpl, gsln, gslwsc, gscr, gsplci, gqlwsc, gqplci, dashline, ginq_linetype
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

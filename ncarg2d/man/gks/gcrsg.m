.\"
.\"	$Id: gcrsg.m,v 1.10 2000-08-22 04:15:57 haley Exp $
.\"
.TH GCRSG 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GCRSG (Create segment) - creates a GKS segment.
.SH SYNOPSIS
CALL GCRSG(SGNA)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gcreate_seg(Gint seg_name);
.SH DESCRIPTION
.IP SGNA 12
(Integer, Input) - Specifies the segment identifier that will be used in
calls to subsequent segment functions applying to this segment.  There can be
a maximum of 100 segments in a given job step.  Legal values for SGNA are
integers between 0 and 99 inclusive.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gevtm, gactm, gclsg, gcsgwk, gdsg, gqopsg, gqsgus, gssgt., gcreate_seg
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright (C) 1987-2000
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

.\"
.\"	$Id: gcsgwk.m,v 1.9 2000-07-11 23:03:08 haley Exp $
.\"
.TH GCSGWK 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GCSGWK (Copy segment to workstation) - copies a GKS segment to a GKS 
workstation.
.SH SYNOPSIS
CALL GCSGWK(WKID,SGNA)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gcopy_seg_ws(Gint ws_id, Gint seg_name);
.SH DESCRIPTION
.IP WKID 12
(Integer, Input) - Specifies the workstation identifier (as defined in
a GOPWK call) that is to receive the segment copy.
.IP SGNA 12
(Integer, Input) - Identifies the segment 
that is to be copied to the specified workstation.
SGNA must have been used in a previous call to GCRSG.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gevtm, gactm, gclsg, gcrsg, gdsg, gqopsg, gqsgus, gssgt., gcopy_seg_ws
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright (C) 1987-2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
